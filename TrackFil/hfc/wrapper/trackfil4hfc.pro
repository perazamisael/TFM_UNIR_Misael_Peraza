PRO trackfil4hfc,config_file,$
                 starttime=starttime,endtime=endtime, $
                 database=database,host=host,server=server,$
                 user=user,password=password, $
                 config_dir=config_dir,$
                 output_dir=output_dir,$
                 hfc_struc=hfc_struct, $
                 NEW_TRACKING=NEW_TRACKING,$
                 WRITE_CSV=WRITE_CSV,WRITE_PNG=WRITE_PNG, $
                 WRITE_LOG=WRITE_LOG,VERBOSE=VERBOSE,$
                 DEBUG=DEBUG,NULL_PLOT=NULL_PLOT

;+
; NAME:
;		trackfil4hfc
;
; PURPOSE:
; 		This routine performs the tracking of solar filaments
;		stored in the Heliophysics Feature Catalogue (HFC) using
;		a curve matching algorithm on the skeletons.
;		More information about the method can be found in:
;		Bonnin et al., Solar Physics, 2012.
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		TRACKFIL
;
; CALLING SEQUENCE:
;		IDL> trackfil4hfc,config_file,starttime=starttime,endtime=endtime
;
; INPUTS:
;		config_file - Scalar of string type providing the full path to the
;					  configuration file. This file contains inputs for the code.
;
; OPTIONAL INPUTS:
;		starttime  - First date and time of the time range to process (ISO 8601 date format).
;					 Default is endtime - 60 days.
;               endtime    - Last date and time of the time range to process (ISO 8601 date format).
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
;		config_dir - Directory where the configuration file is stored.
;		output_dir - Scalar of string type specifying the path to the directory
;					 where output files will be saved (use current one by default).
;
; KEYWORD PARAMETERS:
;		/NEW_TRACKING	     - Run program without take account of previous tracking data
;					 	       in the HFC database.
;							   If /ALL is set, all the filaments are used.
;        /WRITE_CSV           - Write output csv format file containg tracking results.
;                              Parameters returned are the same than those in the FILAMENTS_TRACKING HFC table.
;       /WRITE_PNG           - Write PNG images containing Carrington map with tracking results.
;		/WRITE_LOG			 - Write log file.
;		/VERBOSE			 - Talkative mode.
;       /DEBUG               - Debug mode.
;       /NULL_PLOT           - If set then do set_plot,'null' (avoid plot).
;
; OUTPUTS:
;		None.
;
; OPTIONAL OUTPUTS:
;		hfc_struct - Structure containing the tracking information.
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
;		trackfil_read_config
;		track_hfc_struct
;		trackfil_query_hfc
;		trackfil_pix2mcar
;		trackfil_set_trackid
;		trackfil_set_refid
;		trackfil_set_phenom
;		anytim
;		anytim2jd
;		jd2str
;
; EXAMPLE:
;		;Perform tracking for the filaments detected between
;		;January 1st, 2001 at 08:00:00, and December 31th, 2002 at 22:00:00:
;		trackfil4hfc,'../config/trackfil_mh_config.txt',starttime='2001-01-01T08:00:00',$
;                                 endtime='2002-12-31T22:00:00'
;
; MODIFICATION HISTORY:
;		Written by:		X.Bonnin, 25-MAY-2010.
;
; Version 1.00
;		01-MAR-2011, X.Bonnin:	First release.
;
; Version 1.1
;		31-MAR-2011, X.Bonnin:	Added tracking_set_refid routine.
;								Renamed tracking_pix2car into tracking_pix2mcar,
;								which only calculates the modified carrington
;								coordinates of the filament skeletons.
; Version 1.2
;		05-MAY-2011, X.Bonnin:	trackfil_set_trackid routine can now track smaller
;								filaments (i.e. ske_len_deg < !LMIN), which are
;								not taken account by the
;								curve matching algorithm.
;								Added /NEW_TRACKING keyword.
; Version 2.0
;		06-JUN-2011, X.Bonnin:  The HFC database login information are now provided
;								in the input parameter hfc_login.
;
; Version 2.1
;		20-JAN-2012, X.Bonnin:	Added config_file input.
;								Call trackfil_read_config.
;
; Version 2.2
;		30-JAN-2012, X.Bonnin:	Added command line execution capability.
;					Loops on files removed.
;
version = '2.21'
;           21-DEC-2015, X.Bonnin: Renamed to trackfil4hfc
;-

;[1]:Initializing software
;[1]:=============================
;Starting Time
syst0 = systime(/SEC)
qte = string(39b)

args = strlowcase(strtrim(command_line_args(),2))
nargs = n_elements(args)
if (args[0] ne '') then begin
   config_file = args[0]
   inputpar = ['starttime','endtime','database',$
              'host','user','password','config_dir',$
              'output_dir']
   inputkey = ['/new_tracking','/write_log','/write_csv',$
              '/write_png','/verbose','/debug','/null_plot']
   for i=0l,n_elements(args)-1 do begin
      where_key = (where(args[i] eq inputkey))[0]
      if (where_key ne -1) then begin
         flag = execute(strmid(inputkey[where_key],1)+'=1')
         continue
      endif
      value = strsplit(args[i],'=',/EXTRACT)
      if (n_elements(value) eq 2) then begin
         where_par = (where(value[0] eq inputpar))[0]
         if (where_par ne -1) then begin
            flag = execute(value[0]+'='+qte+value[1]+qte)
         endif
      endif
   endfor
endif else begin
   if (n_params() lt 1) then begin
      message,/INFO,'Call is: trackfil4hfc,config_file,$'
      print,'                              starttime=starttime, $'
      print,'                              endtime=endtime, $'
      print,'                              database=database, $'
      print,'                              host=host,server=server, $'
      print,'                              user=user,password=password, $'
      print,'                              config_dir=config_dir,$'
      print,'                              output_dir=output_dir,$'
      print,'                              hfc_struct=hfc_struct, $'
      print,'                              /NEW_TRACKING,$'
      print,'                              /WRITE_LOG, $'
      print,'                              /WRITE_CSV,/WRITE_PNG, $'
      print,'                              /VERBOSE,/DEBUG,/NULL_PLOT'
      return
   endif
endelse

;hostname
spawn,'hostname',hostname

;Current date
run_date = (strsplit(anytim(!stime,/CCSDS),'.',/EXTRACT))[0]
print,'Starting trackfil4hfc on '+hostname+' ('+run_date+')'

print,'Initializing inputs...'
;Setting keywords
NEW_TRACKING = keyword_set(NEW_TRACKING)
WRITE_LOG = keyword_set(WRITE_LOG)
WRITE_CSV = keyword_set(WRITE_CSV)
WRITE_PNG = keyword_set(WRITE_PNG)
VERBOSE = keyword_set(VERBOSE)
DEBUG = keyword_set(DEBUG)
if (DEBUG) then VERBOSE = 1
if not (VERBOSE) then !QUIET = 1
SILENT = 1 - VERBOSE
NULL = keyword_set(NULL_PLOT)
if (NULL) then set_plot,'NULL'

if (VERBOSE) then begin
   if (NEW_TRACKING) then print,'/NEW_TRACKING option is set.'
   if (WRITE_CSV) then print,'/WRITE_CSV option is set.'
   if (WRITE_LOG) then print,'/WRITE_LOG option is set.'
   if (WRITE_PNG) then print,'/WRITE_PNG option is set.'
   if (VERBOSE) then print,'/VERBOSE option is set.'
   if (DEBUG) then print,'/DEBUG option is set.'
   if (NULL) then print,'/NULL_PLOT option is set.'
endif

cfile = strtrim(file_basename(config_file[0]),2)
if (keyword_set(config_dir)) then cdir = strtrim(config_dir[0],2) $
else cdir = strtrim(file_dirname(config_file[0]),2)
cpath = cdir + path_sep() + cfile
if (not file_test(cpath,/REG)) then message,'ERROR: '+cpath+' does not exist!'

if not (keyword_set(output_dir)) then cd,current=outdir else outdir = strtrim(output_dir[0],2)

if (VERBOSE) then print,'Output directory --> '+outdir

;Time range
if (not keyword_set(endtime)) then etime = run_date else etime = strtrim(endtime[0],2)
jend = anytim2jd(endtime) & jend = jend.int + jend.frac
if (not keyword_set(starttime)) then begin
	jstart = jend - 60.0d
	stime=jd2str(jstart,format=1)
endif else begin
	stime = strtrim(starttime[0],2)
	jstart = anytim2jd(stime) & jstart = jstart.int + jstart.frac
endelse

;Remove 'T' between date and time (mysql date format)
stime = strjoin(strsplit(stime,'T',/EXTRACT),' ')
etime = strjoin(strsplit(etime,'T',/EXTRACT),' ')

if (not keyword_set(database)) then database = 'hfc1'
if (not keyword_set(server)) then server = 'voparis-helio.obspm.fr'
if (not keyword_set(host)) then host = 'voparis-mysql5-paris.obspm.fr'
if (not keyword_set(user)) then user = 'guest'
if (not keyword_set(password)) then password = 'guest'

;load system variables
trackfil_load_sysvar

;Define HFC output structures
frc_info_stc = {hfc_frc_info}

;Loading input parameters
print,'Reading configuration file '+cpath+'...'
inp_stc = trackfil_read_config(cpath)
if (size(inp_stc,/TNAME) ne 'STRUCT') then message,'ERROR: Cannot read configuration file!'
print,'Reading configuration file '+cpath+'...done'

print,'Initializing inputs...done'
;[1]:=============================

;[2]:Loading HFC data
;[2]:=========================
print,'Loading filament data from HFC...'
;Loading HFC data
trange = [jstart-!carr_period,jend+!carr_period]
trange = jd2str(trange)
hfc_stc = hfc_stilts_client('VIEW_FIL_HQI',starttime=trange[0],$
                            endtime=trange[1],database=database,$
                            server=server,hostname=host,user=user,$
                            password=password,/STRUCT,VERBOSE=VERBOSE)
if (size(hfc_stc,/TNAME) ne 'STRUCT') then message,'ERROR: Cannot load data from HFC!'
nfeat = n_elements(hfc_stc)
if (DEBUG) then begin
   help,hfc_stc,/STR
   stop
endif
where_null = where(hfc_stc.track_id le 0)
if (where_null[0] ne -1) then begin
   hfc_stc[where_null].track_id=hfc_stc[where_null].id_fil
   hfc_stc[where_null].ref_feat=-1
   hfc_stc[where_null].phenom=-1
   hfc_stc[where_null].track_lvl_trust=0.
endif

hfc_stc.phenom = -1
hfc_stc.track_lvl_trust = 0
if (NEW_TRACKING) then begin
   hfc_stc.track_id = hfc_stc.id_fil
   hfc_stc.ref_feat = -1
endif

date_obs = hfc_stc.date_obs
jd_obs = hfc_stc.jdint + hfc_stc.jdfrac
filename = hfc_stc.filename
where_inside = where(jd_obs ge jstart and jd_obs le jend)
if (where_inside[0] eq -1) then message,'No data in the time range!'
jd_obs = jd_obs[where_inside]
date_obs = date_obs[where_inside]
filename = filename[where_inside]

iobs = uniq(jd_obs,sort(jd_obs))
date_obs = date_obs[iobs]
jd_obs = jd_obs[iobs]
filename = filename[iobs]
nfile = n_elements(filename)

if (VERBOSE) then print,strtrim(nfeat,2)+' filaments extracted to track on '+strtrim(nfile,2)+' images.'
print,'Loading filament data from HFC...done'
;[2]:=================================

;[3]:Compute the Modified Carrington coordinates of filaments
;[3]:========================================================
print,'Calculating filaments coordinates...'
trackfil_pix2mcar,hfc_stc,ske_stc,ds=inp_stc.ds,error=error,SILENT=SILENT,PROGRESS=DEBUG
if (error) then message,'Calculating filaments coordinates...error'
print,'Calculating filaments coordinates...done'
;[3]:========================================================

;[4]:Computing the filament tracking
;[4]:===============================

print,'Tracking over one solar disc crossing...'
inp_param = [inp_stc.a0,inp_stc.theta0,inp_stc.d0]
track_id = trackfil_set_trackid(hfc_stc,ske_stc,$
                                rmax=inp_stc.rmax, $
                                lmin=inp_stc.lmin, $
                                threshold=inp_stc.threshold, $
                                param=inp_param, $
                                lvl_trust=lvl_trust, $
                                error=error,$
                                SILENT=SILENT,PROGRESS=DEBUG)
where_le0 = (where(track_id le 0l))[0]
if (DEBUG) then stop
if (error) or (where_le0 ne -1) then message,'Over one solar disc crossing...error'
hfc_stc.track_id = track_id
hfc_stc.track_lvl_trust = lvl_trust
print,'Tracking over one solar disc crossing...done'

;Determine the filaments behaviour
print,'Determining behaviours...'
phenom = trackfil_set_phenom(hfc_stc, $
                             lmin=inp_stc.lmin, $
                             hg_long_lim=inp_stc.hg_long_lim, $
                             error=error,DEBUG=DEBUG,SILENT=SILENT,$
                             PROGRESS=DEBUG)
if (error) then message,'ERROR: Can not set phenom field!'
hfc_stc.phenom = phenom
print,'Determining behaviours...done'

print,'Identifying filaments from a rotation to the following...'
inp_param = [inp_stc.a0,inp_stc.theta0,inp_stc.d0,inp_stc.dt0]
ref_id = trackfil_set_refid(hfc_stc,ske_stc, $
                            rmax=inp_stc.rmax, $
                            lmin=inp_stc.lmin, $
                            threshold=inp_stc.threshold, $
                            param=inp_param, $
                            error=error,$
                            PROGRESS=DEBUG, $
                            SILENT=SILENT)
if (error) then message,'ERROR:Can not set ref_feat values!'
hfc_stc.ref_feat = ref_id
print,'Identifying filaments from a rotation to the following...done'
;[4]:===============================
hfc_struct = hfc_stc

;[5]:Write output files
;[5]:==================
if (WRITE_CSV) then begin
	print,'Write '+strtrim(nfile,2)+' csv files'
	ver = strjoin(strsplit(version,'.',/EXTRACT))
    for i=0l,nfile-1l do begin
        iw = where(date_obs[i] eq hfc_stc.date_obs,ni)
		compact_date = strjoin((strsplit(date_obs[i],' .',/EXTRACT))[0:1],'T')
		compact_date = strjoin(strsplit(compact_date,'-:',/EXTRACT))
        tr_locfname_i = 'trackfil_'+ver+'_'+compact_date+'_meu1_track.csv'
        track_stc_i = 0b
        track_stc_i = {hfc_filaments_tracking}
        track_stc_i = replicate(track_stc_i,ni)
        track_stc_i.id_fil_track = lindgen(ni) + 1l
        track_stc_i.track_id = hfc_stc(iw).track_id
        track_stc_i.fil_id = hfc_stc(iw).id_fil
        track_stc_i.phenom = hfc_stc(iw).phenom
        track_stc_i.ref_feat = hfc_stc(iw).ref_feat
        track_stc_i.lvl_trust = hfc_stc(iw).track_lvl_trust
        track_stc_i.track_filename = tr_locfname_i
        track_stc_i.run_date = run_date
        track_stc_i.feat_filename = hfc_stc(iw).feat_filename

        track_path = outdir + path_sep() + tr_locfname_i[0]
        hfc_write_csv,track_stc_i,track_path
        if (DEBUG) then print,strtrim(nfile-i,2)+': '+track_path+' saved.'
    endfor
endif

if (WRITE_PNG) then begin
    loadct,39,/SILENT
    tvlct,r,g,b,/GET
    mincar = min(ske_stc.Xcarr[0],max=maxcar,/NAN)
    ncarr = round((maxcar - mincar)/360.) + 1
    offset = 90.0d
    nX = 360 + 2*90 & nY = 180
    dX = 1.0d & dY = 1.0d

    if (VERBOSE) then print,'Write '+strtrim(ncarr,2)+' png files'

    carrot0 = fix(tim2carr(trange[0],/DC))

    for i=0l,ncarr-1l do begin
        cmin_i = double(i*360) - offset
        cmax_i = double((i+1l)*360) + offset

        iw = where(-ske_stc.Xcarr[0] ge cmin_i and -ske_stc.Xcarr[0] lt cmax_i,ni)
        if (iw[0] eq -1) then continue

        img = bytarr(nX,nY) + 255b
        for j=0l,ni-1l do begin
            lon_j = -ske_stc(iw[j]).Xcarr[0]
            lat_j = ske_stc(iw[j]).Xcarr[1]
            X_j = (round(lon_j - cmin_i)/dX)>(0)<(nX-1)
            Y_j = (round(lat_j + 90.)/dY)>(0)<(nY-1)
            col_track = (20l*hfc_stc((where(ske_stc(iw[j]).index eq hfc_stc.id_fil))[0]).track_id) + 24l
            img[X_j,Y_j] = byte(col_track)
        endfor
        img = rebin(img,nX*2,nY*2)
        png_file = 'trackfil4hfc_carr'+strtrim(carrot0 + i,2)+'_results.png'
        png_path = outdir + path_sep() + png_file
        write_png,png_path,img,r,g,b
        if (DEBUG) then print,strtrim(ncarr-i,2)+': '+png_path+' saved.'
    endfor
endif
if (DEBUG) then stop
;[5]:==================

;[6]:End the program
;[6]:=================================
print,'Program ended on '+systime()+'.'
print,'Total processing time : '+$
			 string((systime(1) - syst0)/60.,format='(f8.2)')+' min.'
;[6]:=================================
END
