FUNCTION hfc_pix2hel, pixel,cdelt1,cdelt2,center_x,center_y,r_sun,date_obs

;+
; NAME:
;		hfc_pix2hel
;
; PURPOSE:
; 		Converts the pixel position to heliographic coordinates.
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		None.
;
; CALLING SEQUENCE:
;		IDL> hel_coord = hfc_pix2hel(pixel,cdelt1,cdelt2,center_x,center_y,r_sun,date_obs)
;
; INPUTS:
;		indices 	- pixel subscripts
;       cdelt1     	- pixel to arcs ratio X (CDELT1 fits keyword)
;       cdelt2     	- pixel to arcs ratio Y (CDELT2 fits keyword)
;       center_x  	- sun center X          (CENTER_X fits keyword)
;       center_y   	- sun center Y          (CENTER_Y fits keyword)
;       r_sun    	- sun radius            (R_SUN or SOLAR_R fits keyword)
;		date_obs	- observation date      (DATE_OBS fits keyword)
;
; OPTIONAL INPUTS:
;		None.
;
; KEYWORD PARAMETERS:
;		None.
;			  
; OUTPUTS:
;		hel_coord - a 2 elements vector containing the heliographic coordinates: [long,lat] in degrees.
;
; OPTIONAL OUTPUTS:
;		None.
;
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; CALL:
;		ARCMIN2HEL
;
; EXAMPLE:
;		None.		
;
; MODIFICATION HISTORY:
;		Written by:		X.Bonnin (Adapted from PIX2CARR() by Nicolas Fuller)
;
;-

if (n_params() lt 7) then begin
	message,/INFO,'Call is:'
	print,'hel_coord = hfc_pix2hel(pixel,cdelt1,cdelt2,center_x,center_y,r_sun,date_obs)'
	return,0
endif

;  Use own Solar radius instead of using pb0r()

;  Calculate R0 value from Sun radius for arcmin2hel()

Robs    = 1.0/tan((cdelt1[0]*r_sun[0])/3600/!radeg)
   
   
;  Get pixel coord. from input indices
arrx=pixel(0)
arry=pixel(1)

;  Convert pixel to arcseconds
ix = DOUBLE(cdelt1[0]*(arrx - center_x[0]))
iy = DOUBLE(cdelt2[0]*(arry - center_y[0]))

;  Calculate the heliographic coords in degrees
hel_coord = ARCMIN2HEL(ix/60., iy/60., date=date_obs, $
           	off_limb=off_limb, Sphere=sphere, r0=Robs)
hel_coord = double(reverse(reform(hel_coord))) ; [lat,lon] -> [lon,lat]	
  
return,hel_coord
;-------------------------------------------------------------------------------
END
