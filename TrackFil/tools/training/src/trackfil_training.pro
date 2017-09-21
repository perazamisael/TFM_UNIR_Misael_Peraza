PRO trackfil_training,starttime,endtime, $
					  database=database, $
					  server=server,host=host, $
					  user=user,password=password, $
					  ds=ds, rmax=rmax, lmin=lmin, $
					  output_dir=output_dir, $
					  APPEND=APPEND


;+
; NAME:
;		trackfil_training
;
; PURPOSE:
; 		This routine performs the training of the trackfil software:
;		performing a manual filament tracking,
;		and using the corresponding results to get the 
;		best input parameters' values.		
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		TRACKFIL
;
; CALLING SEQUENCE:
;		IDL> trackfil_training,starttime,endtime
;
; INPUTS:
;		starttime  - First date and time of the time range to process (ISO 8601 date format).
;					 Default is endtime - 60 days.
;       endtime    - Last date and time of the time range to process (ISO 8601 date format). 	
;	
; OPTIONAL INPUTS:
;					 Default is current date and time.
;		database   - Name of the database to query.
;					 Default is 'hfc1'.
;		host	   - Name of the database host.
;					 Default is 'voparis-mysql5-paris.obspm.fr'.
;		server     - Name of the distant server used to reach the database.
;					 Default is 'voparis-helio.obspm.fr'.
;		user       - Name of the database user login.
;					 Default is 'guest'.
;		password   - Corresponding password.
;					 Default is 'guest'.
;		ds		   - Spatial resolution (in degrees) of the filament skeletons in 
;					 the Carrington reference frame.
;					 Default is 1 degree.
;		rmax	   - Maximal distance (in degrees) between two points of two 
;					 skeletons below which the curve matching is done on the
;					 Carrington frame.
;					 Default is 5 degrees.
;		lmin       - Minimum length (in degrees) of skeletons above which tracking
;					 is performing.
;					 Default is 0 degree.
;		output_dir - Scalar of string type specifying the path to the directory 
;					 where output files will be saved (use current one by default).
;
; KEYWORD PARAMETERS:
;		/APPEND - If set, then append new parameter sets to
;				  the existent output file.
;
; OUTPUTS:
;		None.		
;
; OPTIONAL OUTPUTS:
;		None.
;
; COMMON BLOCKS:		
;		None.	
;	
; SIDE EFFECTS:
;		None.
;
; RESTRICTIONS/COMMENTS:
;		An internet access must be available in order to query the HFC.
;		
; CALL:
;		trackfil_pix2mcar		
;		trackfil_set_trackid
;		trackfil_set_refid
;		anytim
;		anytim2jd
;		jd2str
;
; EXAMPLE:
;		None.	
;
; MODIFICATION HISTORY:
;		Written by:		X.Bonnin, 25-JUN-2010.
;	
;-	

if (n_params() lt 2) then begin
	message,/INFO,'Call is:'
	print,'trackfil_training,starttime,endtime, $'
	print,'                  database=database, $'
	print,'                  server=server,host=host, $'
	print,'                  user=user,password=password, $'
	print,'                  ds=ds,rmax=rmax,lmin=lmin, $'
	print,'                  output_dir=output_dir,/APPEND'	
	return
endif

APPEND = keyword_set(APPEND)

if (not keyword_set(ds)) then ds=1.0 ;deg
if (not keyword_set(rmax)) then rmax = 5.0 ;deg
if (not keyword_set(lmin)) then lmin = 0.0 ;deg
if (not keyword_set(output_dir)) then cd,current=output_dir

jstart = anytim2jd(starttime)
jstart = jstart.int + jstart.frac
jend = anytim2jd(endtime)
jend = jend.int + jend.frac

trackfil_load_sysvar
device,get_screen_size=screen
xsize=min([screen])*0.6
poffset = [-10,10]

;Loading HFC data
print,'Loading filament data from HFC...'
;trange = [jstart-!carr_period,jend+!carr_period]
trange = [jstart,jend]
trange = jd2str(trange)
hfc_stc = hfc_stilts_client('VIEW_FIL_HQI',starttime=trange[0],$
				  			endtime=trange[1],database=database,$
				  			server=server,hostname=host,user=user,$
				  			password=password,/STRUCT,/VERBOSE)
if (size(hfc_stc,/TNAME) ne 'STRUCT') then message,'Empty HFC content!'
cdelt2 = hfc_stc[0].cdelt2
naxis1 = hfc_stc[0].naxis1
naxis2 = hfc_stc[0].naxis2
center_x = hfc_stc[0].center_x
center_y = hfc_stc[1].center_y
rsun = hfc_stc[0].r_sun
Xsun = rsun*cos(2.*!pi*findgen(361)/360.) + center_x
Ysun = rsun*sin(2.*!pi*findgen(361)/360.) + center_y
print,'Loading filament data from HFC...done'

print,'Calculating filaments coordinates...'
trackfil_pix2mcar,hfc_stc,ske_stc,ds=ds,error=error,SILENT=SILENT
if (error) then message,'Calculating filaments coordinates...error'
print,'Calculating filaments coordinates...done'

print,'Computing tracking parameters...'
param = [5.,30.,4.]
track_id = trackfil_set_trackid(hfc_stc,ske_stc,$
					 			rmax=rmax,lmin=lmin, $
					 			param=param, threshold=0.5, $
					 			output_param=tracking_param,error=error,$
					 			SILENT=SILENT)
if (error) then message,'Computing tracking parameters...error'
print,'Computing tracking parameters...done'

print,'Classifying tracking results...'
ncomp = n_elements(tracking_param)
set = {MATCH:0,A:0.0d,THETA:0.0d,D:0.0d}
set = replicate(set,ncomp)
col_plot,ct=39
window,0,xsize=xsize,ysize=xsize
window,2,xsize=1.2*xsize,ysize=xsize
iter = 0l
for i=0l,ncomp-1l do begin
	print,strtrim(ncomp-i,2)+' classification(s) pending...'
	tpar = tracking_param[i]
	 
	if (tpar.a + tpar.theta + tpar.d eq 0.) then continue

	where_feat_i = (where(tpar.id_i eq hfc_stc.id_fil))[0]
	where_feat_j = (where(tpar.id_j eq hfc_stc.id_fil))[0]
	where_i = where(tpar.id_i eq ske_stc.index)
	where_j = where(tpar.id_j eq ske_stc.index)
	print,'length[i]= '+strtrim(tpar.len_i,2)+', length[j]= '+strtrim(tpar.len_j,2)

	if (tpar.len_i lt lmin) or (tpar.len_j lt lmin) then continue

	jdmin = min([hfc_stc[where_feat_i].jdint,hfc_stc[where_feat_j].jdint],max=jdmax)
	print,hfc_stc[where_feat_i].date_obs+' and '+hfc_stc[where_feat_j].date_obs
	print,'dt= '+strtrim(jdmax-jdmin,2)+' day(s)'
	where_tr = where(hfc_stc.jdint ge jdmin and $
					 hfc_stc.jdint le jdmax,ntr)

	wset,0
	xr = minmax([ske_stc[where_i].Xpix[0],ske_stc[where_j].Xpix[0]])+(poffset/cdelt2)
	yr = minmax([ske_stc[where_j].Xpix[1],ske_stc[where_j].Xpix[1]])+(poffset/cdelt2)
	plot,[0,naxis1],[0,naxis2],/NODATA,/XS,/YS
	oplot,Xsun,Ysun
	oplot,[0,naxis1],[center_y,center_y],line=2
	oplot,[center_x,center_x],[0,naxis2],line=2
	
	for j=0l,ntr-1l do begin
		where_id = where(hfc_stc[where_tr[j]].id_fil eq ske_stc.index)
		if (min(ske_stc[where_id].Xpix[1]) gt yr[1]) or $
		   (max(ske_stc[where_id].Xpix[1]) lt yr[0]) then continue
		oplot,ske_stc[where_id].Xpix[0],ske_stc[where_id].Xpix[1],thick=0.75
	endfor

	oplot,ske_stc[where_i].Xpix[0],ske_stc[where_i].Xpix[1],color=254,thick=3
	oplot,ske_stc[where_j].Xpix[0],ske_stc[where_j].Xpix[1],color=50,thick=3
	
	wset,2
	xr = minmax([ske_stc[where_i].Xcarr[0],ske_stc[where_j].Xcarr[0]])+poffset
	yr = minmax([ske_stc[where_j].Xcarr[1],ske_stc[where_j].Xcarr[1]])+poffset
	plot,xr,yr, $
		 /NODATA,/XS,/YS

	for j=0l,ntr-1l do begin
		where_id = where(hfc_stc[where_tr[j]].id_fil eq ske_stc.index)
		if (min(ske_stc[where_id].Xcarr[1]) gt yr[1]) or $
		   (max(ske_stc[where_id].Xcarr[1]) lt yr[0]) then continue
		oplot,ske_stc[where_id].Xcarr[0],ske_stc[where_id].Xcarr[1],thick=0.75
	endfor

	oplot,ske_stc[where_i].Xcarr[0],ske_stc[where_i].Xcarr[1],color=254,thick=3
	oplot,ske_stc[where_j].Xcarr[0],ske_stc[where_j].Xcarr[1],color=50,thick=3	

	status=button(xoffset=xsize,yoffset=xsize*1.1)
	if (status eq 3) or (status eq 4) then break

	set[iter].a = tpar.a
	set[iter].theta = tpar.theta
	set[iter].d = tpar.d
	case status of
		-1:begin
			set[iter].match = 0
			iter++
		end
		1:begin
			set[iter].match = 1
			iter++
		end
		else:print,'skip current classificaton'
	endcase
	print,strtrim(iter,2)+' set of parameters stored.'
endfor
print,'Classifying tracking results...done'

if (iter gt 0l) then set = set[0:iter-1l]
if (status ne 4) then begin
	print,'Writing output file...'
	outpath = output_dir + path_sep() + 'trackfil_param.csv'
	hfc_write_csv,set,outpath,APPEND=APPEND
	print,'Writing output file...done'
endif

print,'Exit program'
END