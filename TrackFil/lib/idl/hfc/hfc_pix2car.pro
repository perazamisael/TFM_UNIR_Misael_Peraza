FUNCTION hfc_pix2car, pixel,cdelt1,cdelt2,center_x,center_y,r_sun,date_obs, $
				      hel_coord=hel_coord, $
				      dec_car0=dec_car0,DC=DC,DRC=DRC

;+
; NAME:
;		hfc_pix2car
;
; PURPOSE:
; 		Converts from pixel position (on the Sun surface) to carrington coordinates.
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		None.
;
; CALLING SEQUENCE:
;		IDL> car_coords = hfc_pix2car(pixel ,cdelt1,cdelt2,center_x,center_y,r_sun,date_obs)
;
; INPUTS:
;		pixel    - pixel coordinates [X,Y]
;       cdelt1   - pixel to arcs ratio X (CDELT1 fits keyword)
;       cdelt2   - pixel to arcs ratio Y (CDELT2 fits keyword)
;       center_x - sun center X          (CENTER_X fits keyword)
;       center_y - sun center Y          (CENTER_Y fits keyword)
;       r_sun    - sun radius            (R_SUN or SOLAR_R fits keyword)
;       date_obs - observation date      (DATE_OBS fits keyword)	
;
; OPTIONAL INPUTS:
;		dec_car0 - fixes the decimal carrington number to dec_car0.
;		l0 		 - fixes the central meridian Carrington long. value to l0
;                  (L0 = 0. will produce heliographic coordinates.) OBSOLETE
;
; KEYWORD PARAMETERS:
;		DC	- Returns longitude in decimal Carrington number 
;			  instead of degrees.
;		DRC	- Computes the Differential Rotation Corrected 
;			  Carrington coordinates using the 
;			  Ulrich and Boyden, Solar Physics, 2006 model.
;			  
; OUTPUTS:
;		car_coord - a 2 elements vector [long, lat] containing the Carrington longitude and latitude 
;				   (in degrees by default).
;
;
; OPTIONAL OUTPUTS:
;		hel_coord	- a 2 elements vector [long, lat] containing the heliographic coordinates in degrees.
;		DC			- return the decimal carrington rotation number	at DATE_OBS.
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
;		hfc_pix2hel
;		hfc_hel2car
;
; EXAMPLE:
;		None.		
;
; MODIFICATION HISTORY:
;		Written by:		M. Fouesneau, M. Galametz, F. Gonon, and A. Maire, 01-JUN-2006.
;						(Adapted from PIX2CARR() by Nicolas Fuller)
;
;		25-MAY-2010, X.Bonnin:	Renamed from pix2carr_light to filtrack_pix2car.
;		18-JUN-2010, X.Bonnin:	Added DC keyword.
;								Added hel_coord optional output parameter.
;								Renamed fixL0 to L0.
;		26-JAN-2010, X.Bonnin:  Replaced l0 by the dec_car0 optional input, 
;								and added the DCR keyword.
;		08-AUG-2011, X.Bonnin:	Call hfc_pix2hel and hfc_hel2car functions.
;
;-

if (n_params() lt 7) then begin
	message,/INFO,'Call is:'
	print,'car_coord = hfc_pix2car(pixel,cdelt1,cdelt2,center_x,center_y,r_sun,date_obs, $'
	print,'                        hel_coord=hel_coord,dec_car0=dec_car0,/DC,/DRC)'
	return,0
endif

hel_coord = hfc_pix2hel(pixel,cdelt1,cdelt2,center_x,center_y,r_sun,date_obs)
car_coord = hfc_hel2car(hel_coord,date_obs,dec0=dec_car0,DC=DC,DRC=DRC)

return,car_coord
END
