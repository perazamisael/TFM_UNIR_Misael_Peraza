PRO hfc_plot_trackfil,starttime,endtime,$			
				      ref_png=ref_png, $	
				      output_dir=output_dir,$
                      temp_dir=temp_dir,$ 
				      color=color,ctable=ctable,$
				      CARR=CARR,WRITE_PNG=WRITE_PNG,$
				      WRITE_PS=WRITE_PS,GRID=GRID,$
				      DISPLAY_INDEX=DISPLAY_INDEX,$
				      TEST=TEST,NO_IMAGE=NO_IMAGE,$
                      NO_CONTOUR=NO_CONTOUR, FILL=FILL, $
                      SKELETON=SKELETON, $
                      SILENT=SILENT

CARR = keyword_set(CARR)
WRITE_PNG=keyword_set(WRITE_PNG)
WRITE_PS = keyword_set(WRITE_PS)
DISPLAY_INDEX = keyword_set(DISPLAY_INDEX)
GRID = keyword_set(GRID)
TEST = keyword_set(TEST)
FILL = keyword_set(FILL)
NO_CONTOUR=keyword_set(NO_CONTOUR)
SKELETON = keyword_set(SKELETON)
NO_IMAGE = keyword_set(NO_IMAGE)
SILENT = keyword_set(SILENT)
if not (keyword_set(output_dir)) then cd,current=outdir else outdir = strtrim(output_dir[0],2)
if not (keyword_set(temp_dir)) then tmpdir = outdir else tmpdir = strtrim(temp_dir[0],2) 
if not (keyword_set(ctable)) then ctable = 39

loadct,0,/SILENT
tvlct,r0,g0,b0,/GET

if not (WRITE_PS) then begin
	if (CARR) or (NO_IMAGE) then col_plot,ct=ctable							 
	!p.charthick=2.5
	!p.charsize=2.5
	!p.font=1
	!x.thick=2.5
	!y.thick=2.5
    device,get_screen_size=screen
    window,/FREE,xsize=.8*screen[1],ysize=.8*screen[1]
endif


tbl = 'VIEW_FIL_HQI'
stime = strjoin(strsplit(starttime[0],'T',/EXTRACT),' ')
etime = strjoin(strsplit(endtime[0],'T',/EXTRACT),' ')
tr = [stime,etime]

if (CARR) then begin
    dc0 = fix(tim2carr(tr[0],/DC))
    dc = tim2carr(tr,/DC)
endif

limit = 1000000l
query = hio_form_hfc_query(from='VIEW_FIL_HQI',starttime=tr[0],endtime=tr[1],$
						   limit=limit,TEST=TEST,/VERB)
hfc_data = decode_votable(ssw_hio_query(query,VERBOSE=VERBOSE), /QUIET)
if (datatype(hfc_data) ne 'STC') then return
nfeat = n_elements(hfc_data.id_fil)
if not (SILENT) then print,'Number of filaments found in the catalogue: '+strtrim(nfeat,2)		
if (~keyword_set(colors)) then col = (20.*hfc_data.id_fil mod 230) + 24	$
else col = colors[0] + intarr(nfeat)

jd_obs = hfc_data.jdint + hfc_data.jdfrac

tracklist = hfc_data.track_id
tracklist = tracklist[uniq(tracklist,sort(tracklist))]
ntrack = n_elements(tracklist)

for i=0l,ntrack-1l do begin

    iw = where(hfc_data.track_id eq tracklist[i],ni)
    jdmin = min(hfc_data(iw).jdint + hfc_data(iw).jdfrac,max=jdmax)    

    where_in = where(jd_obs ge jdmin and jd_obs le jdmax,ni)
    hfc_data_i = hfc_data(where_in)
    date_obs_i = hfc_data_i.date_obs
    jd_obs_i = hfc_data_i.jdint + hfc_data_i.jdfrac
    qclk_i = hfc_data_i.qclk_url + '/' + hfc_data_i.qclk_fname
    datelist = date_obs_i[uniq(jd_obs_i,sort(jd_obs_i))]
    qclklist = qclk_i[uniq(jd_obs_i,sort(jd_obs_i))]
    ndate_i = n_elements(datelist)
    if not (SILENT) then print,strtrim(ndate_i,2)+' observation(s) retrieved for filament '+strtrim(tracklist[i],2)  

    if (WRITE_PS) then openplot,outdir + path_sep() + 'trackfil_feat'+strtrim(tracklist[i],2)+'_frames.ps',$
							    /ENCAPSULATED,/COLORS
    
    nx = (ndate_i/2)>1 & ny = ((ndate_i/nx) + (ndate_i mod nx))>1
    !p.multi=[0,nx,ny]
    ttcharsize = !p.charsize/float(nx)
    for j=0l,ndate_i-1l do begin
        jw = where(datelist[j] eq date_obs_i,nj)
        hfc_data_j = hfc_data_i(jw)
        naxis1 = hfc_data_j(0).naxis1
        naxis2 = hfc_data_j(0).naxis2
        cdelt1 = hfc_data_j(0).cdelt1   
        cdelt2 = hfc_data_j(0).cdelt2
        center_x = hfc_data_j(0).center_x
        center_y = hfc_data_j(0).center_y
        r_sun = hfc_data_j(0).r_sun

        x = cdelt1*(findgen(naxis1) - center_x)
        y = cdelt2*(findgen(naxis2) - center_y)

        theta = 2.*!pi*findgen(361)/360
        xsun = r_sun*cos(theta) & ysun = r_sun*sin(theta)
        xsun = cdelt1*xsun & ysun = cdelt2*ysun     
        
        ytitle = 'Arcsec'
        xtitle = 'Arcsec'
        title = hfc_data_j(0).observat+' '+hfc_data_j(0).instrume+' ('+hfc_data_j(0).wavename+') '+datelist[j] 

        if not (NO_IMAGE) then begin
            if (keyword_set(ref_png)) then begin
                where_png = (where(file_basename(qclklist[j]) eq file_basename(ref_png)))[0]
                if (where_png ne -1) then qlk_path = ref_png[where_png]
            endif else begin
                Popt = ' -P '+tmpdir
                url_i = 'wget -nc '+qclklist[j]+Popt
                spawn,url_i
                qlk_path = tmpdir + path_sep() + file_basename(qclklist[j])               
            endelse
            image = read_png(qlk_path,r0,g0,b0)

            display2d,image,x,y, $   
                      xtitle=xtitle,ytitle=ytitle, $
                      title=title, ttcharsize=ttcharsize, $
                      color=0
        endif else begin
            plot,x,y,/NODATA, $
                 xtitle=xtitle,ytitle=ytitle, $
                 title=title
            oplot,xsun,ysun
        endelse

        where_inside = where(hfc_data_j.track_id eq tracklist[i],nk)
        if (where_inside[0] eq -1) then continue
        hfc_data_k = hfc_data_j(where_inside)

        loadct,ctable,/SILENT
        for k=0l,nk-1l do begin
            feat_col = byte(20*hfc_data_k(k).track_id) + 24b
            if not (NO_CONTOUR) then begin
                feat_k = feat_cc_extract(hfc_data_k(k).cc,[hfc_data_k(k).cc_x_pix,hfc_data_k(k).cc_y_pix]) 
                feat_k[0,*] = cdelt1*(feat_k[0,*] - center_x)
                feat_k[1,*] = cdelt1*(feat_k[1,*] - center_y)
                oplot,feat_k[0,*],feat_k[1,*],color=feat_col
                if (FILL) then polyfill,feat_k[0,*],feat_k[1,*],color=feat_col
            endif
            if (SKELETON) then begin
                feat_k = feat_cc_extract(hfc_data_k(k).ske_cc,[hfc_data_k(k).ske_cc_x_pix,hfc_data_k(k).ske_cc_y_pix]) 
                feat_k[0,*] = cdelt1*(feat_k[0,*] - center_x)
                feat_k[1,*] = cdelt1*(feat_k[1,*] - center_y)
                oplot,feat_k[0,*],feat_k[1,*],color=feat_col
            endif
            xyouts,-1000,1000,strtrim(hfc_data_k(k).track_id,2),charsize=1.2*ttcharsize
        endfor
    endfor
    !p.multi=0
    if not (WRITE_PS) and not (WRITE_PNG) then begin
        ans=""
        read,ans,prompt='Press enter to continue'
    endif
    
    if (WRITE_PS) then closeplot

endfor

END