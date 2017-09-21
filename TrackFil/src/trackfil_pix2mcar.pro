PRO trackfil_pix2mcar, fil_data, ske_pos, ds=ds,$
                       error=error, DC=DC, SILENT=SILENT,$
                       PROGRESS=PROGRESS

;+
; NAME:
;		trackfil_pix2mcar
;
; PURPOSE:
;		 Compute the filament skeletons
;		 modified carrington coordinates from chain codes.
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		TRACKFIL
;
; CALLING SEQUENCE:
;		IDL> trackfil_pix2mcar,fil_data,ske_pos
;
; INPUTS:
;		fil_data - IDL structure containing the filaments data.
;
; OPTIONAL INPUTS:
;		ds - Scalar of double type containing the curvilinear distance between 2 points along
;			 the filaments skeletons.
;			 If ds is not set, then do not perform interpolation.
;
; KEYWORD PARAMETERS:
;		/DC       - Return Decimal Carrington coordinates
;		/SILENT   - Quiet mode.
;               /PROGRESS - Print the progress of computation in the prompt.
;
; OUTPUTS:
;		ske_pos - Structure containing extracted coordinates of filament skeletons.
;
; OPTIONAL OUTPUTS:
;		error	- Returns 0 if the calculation succeeds, 0 else.
;
; COMMON BLOCKS:
;		None.
;
; SIDE EFFECTS:
;		None.
;
; RESTRICTIONS:
;		None.
;
; CALL:
;		feat_cc_extract
;		curv_interpol
;		hfc_pix2car
;		printl
;		tim2carr
;		tim2jd
;
; EXAMPLE:
;		None.
;
; MODIFICATION HISTORY:
;		Written by: X.Bonnin,	23-JUL-2010.
;		31-MAR-2011, X.Bonnin:	Rename function to trackfil_pix2mcar.
;-

;[1]:Initialize the input parameters
;[1]:===============================
error = 1

if (n_params() lt 1) then begin
	message,/INFO,'Usage:'
	print,'trackfil_pix2mcar, fil_data, ske_pos, ds=ds, $'
	print,'                                error=error, /DC, /SILENT, $'
	print,'                                /PROGRESS'
	return
endif
if (size(fil_data,/TNAME) ne 'STRUCT') then message,'DATA input parameter must be a structure!'

DC = keyword_set(DC)

data = fil_data
nfil = n_elements(data.id_fil)

date_obs0 = data(0).date_obs
dc0 = (tim2carr(date_obs0,/DC))[0]

if (~keyword_set(ds)) then ds = 0

PROGRESS = keyword_set(PROGRESS)
SILENT = keyword_set(SILENT)
;[1]:===============================


;[2]:Compute the coordinates
;[2]:=======================

;Initialize output parameters
npt0 = 1000000l
ske_pos = {index:0l,jd:0.0d,Xpix:dblarr(2),Xcarr:dblarr(2)}
ske_pos = replicate(ske_pos,npt0)

;---- Start loops on filaments ----
i=0l & scount = 0L
while (i lt nfil) do begin
	if (PROGRESS) then printl,'Calculating Carrington coordinates: '+string(100.*(i+1L)/float(nfil),format='(f6.2)')+'% completed.'


	;Generate the filament skeleton coordinates (in pixel)
	;on the processed image using chain code
	skeleton= feat_cc_extract(data[i].ske_cc,$
                              [data[i].ske_cc_x_pix,data[i].ske_cc_y_pix])

	;If empty skeleton then skip
	if (n_elements(skeleton) eq 1) then continue

	npt_ske=n_elements(skeleton[0,*]) ;get skeleton point number
	ske_carr_i=dblarr(2,npt_ske) ; initialize the skeleton Carrington coordinates array

	;----------------- conversion into carrington coordinates -------------------
	k=0l
    while (k lt npt_ske) do begin
		;Call the conversion fonction
		ske_carr_i[*,k]=hfc_pix2car(skeleton[*,k],$
						data[i].cdelt1,data[i].cdelt2,$
						ceil(data[i].center_x),ceil(data[i].center_y),$
						data[i].r_sun,data[i].date_obs,dec_car0=dc0,DC=DC,/DRC)
	   k++
    endwhile
	;-----------------------------------------------------------------------------

	;If required, perform interpolation
	ske_pix_i = skeleton
	if (ds gt 0) then begin
		ske_carr_i = curv_interpol(ske_carr_i[0,*],ske_carr_i[1,*],ds,error=error_i)
		if (error_i) then begin
			message,/CONT,'Error with the interpolation!'
			return
		endif
		npt_ske = n_elements(ske_carr_i[0,*])
		ske_pix_i = lonarr(2,npt_ske)
		ske_pix_i[0,*] = interpol(skeleton[0,*],npt_ske)
		ske_pix_i[1,*] = interpol(skeleton[1,*],npt_ske)
	endif

    iske = lindgen(npt_ske) + scount
	;Pixel Coordinates of filament skeleton
	ske_pos(iske).Xpix = ske_pix_i
	;Carrington Coordinates of filament skeleton
	ske_pos(iske).Xcarr = ske_carr_i

	;Corresponding primary index
	ske_pos(iske).index = data(i).id_fil

	;... And Julian days
	ske_pos(iske).jd = data(i).jdint + data(i).jdfrac

	scount = scount + npt_ske
    i++
endwhile
if (scount eq 0L) then begin
	print,'Empty data!'
	return
endif
ske_pos = ske_pos(0l:scount-1l)
nske = n_elements(ske_pos)

where_nan = where(finite(ske_pos.Xcarr[0]) eq 0,nnan)
if (nnan gt nske/2) then message,'Too many bad coordinates, something is probably wrong!'
;[2]:=========================

error = 0
END
