FUNCTION trackfil_set_phenom,feat_data, $
                             lmin=lmin,hg_long_lim=hg_long_lim, $
                             error=error, $
                             DEBUG=DEBUG,SILENT=SILENT,$
                             PROGRESS=PROGRESS

;+
; NAME:
;		trackfil_set_phenom
;
; PURPOSE:
; 		Determine the behaviour of the filaments over a rotation,
;       then set the phenom field of track_stc structure in consequence.
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		TRACKFIL
;
; CALLING SEQUENCE:
;		IDL> phenom = trackfil_set_phenom(feat_data)
;
; INPUTS:
;		feat_data  - Structure containing the feature data.
;
; OPTIONAL INPUTS:
;       lmin        - Filaments for which the skeleton length is lesser then length_min are not processed.
;                     Default is 0 degree.
;       hg_long_lim - Heliographic longitude (in degrees) over which the feature is assumed to be behind the limb.
;					  Default is 90 degrees.
;
; KEYWORD PARAMETERS:
;       /DEBUG    - Debug Mode.
;	/SILENT	  - Quiet mode.
;       /PROGRESS - Print the progress of the computation in the terminal.
;
; OUTPUTS:
;		phenom	-	Number referring to the behaviour of the filament :
;						0 = Normal behaviour (ie follow the rotation,
;							even if it bends or move at a special rate)
;						1 = Appearance (of the feature, nothing to do
;							with arriving on the visible side of the Sun!)
;						2 = Disappearance (of the feature, nothing to do
;							with leaving the visible side of the Sun)
;						3 = Splitting (1 features becomes 2 or more features)
;						4 = Merging (2 or more features become 1 or more feature)
;						5 = Disappearance followed by appearance of the same feature
;							(e.g. “disparition brusque”)
;						6 = Appearance following a disappearance, as above
;						7 = Abnormal behaviour
;
;
; OPTIONAL OUTPUTS:
;		error -	Equal to 1 if an error occures during the computation, 0 else.
;
; COMMON BLOCKS:
;		None.
;
; SIDE EFFECTS:
;		None.
;
; RESTRICTIONS/COMMENTS:
;		None.
;
; CALL:
;		hfc_pix2hel
;
;
; EXAMPLE:
;		None.
;
; MODIFICATION HISTORY:
;		Written by:		X.Bonnin, 23-JUL-2010.
;
;		02-MAR-2011, X.Bonnin:	Renamed input parameter data to feat_data,
;							    and added filename and ske_coord input parameters.
;		30-MAR-2011, X.Bonnin:  Major update (take account of the new PHENOM definition).
;
;-


;On_error,2


;[1]:Initialize the input parameters
;[1]:===============================

error = 1
if (n_params() lt 1) then begin
    message,/INFO,'Usage:'
    print,'trackfil_set_phenom, feat_data, $'
    print,'                                      lmin=lmin,hg_long_lim=hg_long_lim, $'
    print,'                                      error=error, $'
    print,'                                      /DEBUG,/SILENT, $'
    print,'                                      /PROGRESS'
    return, 1
endif

if (size(feat_data,/TNAME) ne 'STRUCT') then message,'FEAT_DATA input parameter must be an IDL structure!'

if not (keyword_set(lmin)) then lmin = 0. else lmin = float(lmin[0])

;Limb limits (beyond these values the filaments are supposed to be not detectable on the solar disc)
if not (keyword_set(hg_long_lim)) then limb = 90. else limb = float(abs(hg_long_lim[0]))

data = feat_data
nfeat = n_elements(data)
jd = data.jdint + data.jdfrac
datelist = data.date_obs
datelist = datelist[uniq(jd,sort(jd))]
jdlist = jd[uniq(jd,sort(jd))]
jdmin = min(jdlist,max=jdmax)
phenom = intarr(nfeat)

DEBUG = keyword_set(DEBUG)
SILENT = keyword_set(SILENT)
if (DEBUG) then SILENT = 1
PROGRESS = keyword_set(PROGRESS)
;[1]:===============================



;[2]:Determine the filament behaviour
;[2]:================================
for i=0L,nfeat-1L do begin

   if (PROGRESS) then printl,'Computation completed: '+$
                             string(100.*(i+1L)/float(nfeat),format='(f6.2)')+'%'

    if (data(i).ske_length_deg lt lmin) then continue

	date_i = data(i).date_obs
	jd_i = jd[i]
	track_i = data(i).track_id
	feat_gc_i = [(data(i).feat_x_arcsec/data(i).cdelt1) + data(i).center_x,$
                 (data(i).feat_y_arcsec/data(i).cdelt2) + data(i).center_y]
	feat_gc_i = hfc_pix2hel(feat_gc_i,data(i).cdelt1,data(i).cdelt2,$
                            data(i).center_x,data(i).center_y,data(i).r_sun, $
                            date_i)
    lon_i = feat_gc_i[0] & lat_i = feat_gc_i[1]

    ieq = (where(date_i eq datelist))[0]
	ilt = where(jd lt jd_i and track_i eq data.track_id,nlt)
    igt = where(jd gt jd_i and track_i eq data.track_id,ngt)

    lon0 = -999. & lon1 = -999.
    if (ilt[0] eq -1) and (igt[0] eq -1) then continue

    ;If filament is not observed on previous observations...
    if (ilt[0] eq -1) then begin
			;If current observation is not the first then...
			if (jd_i gt jdmin) then begin
				;Compute predictable filament heliographic longitude
				;on the previous observation
				lon0 = (jdlist[ieq-1l] - jd_i)*(14.48 - 2.16*(sin(lat_i/!radeg))^2) + lon_i
                ;If filament should be seen on the preceding image according to its predictable longitude,
                ;then appareance after East limb.
				if (abs(lon0) lt limb) then phenom[i] = 1
			endif
    endif else begin
    ;If filament is observed previously but not on the preceding observation, then appearance following a disappearance (6)
        if (max(jd[ilt],/NAN) lt jdlist[ieq-1l]) then phenom[i] = 6
    endelse

    ;If filament is not observed on next observations...
    if (igt[0] eq -1) then begin
			;If current observation is not the last then...
			if (jd_i lt jdmax) then begin
				;Compute predictable filament heliographic longitude
				;on the next observation
				lon1 = (jdlist[ieq+1l] - jd_i)*(14.48 - 2.16*(sin(lat_i/!radeg))^2) + lon_i
                ;If filament should be seen on the following image according to its predictable longitude,
                ;then disappareance before West limb
				if (abs(lon1) lt limb) then begin
                    if (phenom[i] eq 0) then phenom[i] = 2
                    if (phenom[i] eq 6) then phenom[i] = 7
                endif
			endif
    endif else begin
    ;If filament is observed nextly but not on the following observation, then appearance after a disappearance (6)
        if (min(jd[igt],/NAN) gt jdlist[ieq+1l]) then begin
            if (phenom[i] eq 0) then phenom[i] = 5
            if (phenom[i] eq 1) then phenom[i] = 7
        endif
    endelse

    ;if (DEBUG) then print,data(i).id_fil,data(i).track_id,lon_i,nlt,ngt,lon0,lon1,phenom[i]
endfor
;[2]:=====================

if (DEBUG) then stop

error = 0
return,phenom
END
