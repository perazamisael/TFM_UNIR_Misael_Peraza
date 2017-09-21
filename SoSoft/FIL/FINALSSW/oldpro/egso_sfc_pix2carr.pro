;+
; Project     : EGSO
;
; Name        : EGSO_SFC_PIX2CARR()
;
; Purpose     : Convert pixel coordinate to Carrington or heliographic
;
; Use         : IDL> res = EGSO_SFC_PIXCARR(indices,nax1,nax2,cd1,cd2,
;                          cenx,ceny,rsun,dateo[,fixl0=fixl0])             
;
; Inputs      : indices : pixel subscripts
;               nax1    : image size X          (NAXIS1 fits keyword)
;               nax2    : image size Y          (NAXIS2 fits keyword)
;               cd1     : pixel to arcs ratio X (CDELT1 fits keyword)
;               cd2     : pixel to arcs ratio Y (CDELT2 fits keyword)
;               cenx    : sun center X          (CENTER_X fits keyword)
;               ceny    : sun center Y          (CENTER_Y fits keyword)
;               rsun    : sun radius            (R_SUN or SOLAR_R fits keyword)
;               dateo   : observation date      (DATE_OBS fits keyword)
;
;               
;
; Keywords    : FIXL0 - fixes the Carrington long. L0 value to fixl0
;               FIXL0 = 0. will produce heliographic coordinates
;
;
; Outputs     : Function returns a 2xN element vector: [lat, long] in
;               degrees, where N is number of subscripts
;
; Calls       : ARCMIN2HEL,TIM2CARR
; 
; NB          : - P0 angle is not taken into account by Arcmin2hel
;                 assume that image is corrected from it. 
;               - Arcmin2hel() corrects coords from B0 angle calculated
;                 with pb0r function
;
; Written     : N. Fuller, Meudon Observatory, jan. 2003
;               copyright (C) 2003 Nicolas Fuller, Observatoire de Paris
;               This program is free software; you can redistribute it and/or modify it under the terms of the
;               GNU General Public License as published by the Free Software Foundation;
;
; Modified    : NF Mar 2005 last rev.
;
;-


FUNCTION EGSO_SFC_PIX2CARR,tab,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=fixl0


   tab_out=[[tab*0D],[tab*0D]]


;----------------------------------------------------- 
;  Use own Solar radius instead of using pb0r()
;-----------------------------------------------------
;  Calculate R0 value from Sun radius for arcmin2hel()
;-----------------------------------------------------
   Robs    = 1.0/tan((cd1*rsun)/3600/!radeg)


;-----------------------------------------------------
;  Calculate Carrington long. if necessary
;-----------------------------------------------------
   IF NOT ISVALID(FIXL0) THEN L0 = TIM2CARR(dateo) $
   ELSE L0=fixl0


;-----------------------------------------------------
;  Get pixel coord. from input indices
;-----------------------------------------------------
   arrx = tab MOD nax1
   arry = tab / nax1


;-----------------------------------------------------   
;  Convert pixel to arcseconds
;-----------------------------------------------------
   ix = DOUBLE(cd1*(arrx - cenx))
   iy = DOUBLE(cd2*(arry - ceny))


;-----------------------------------------------------
;  Calculate the heliographic coords
;-----------------------------------------------------
   tt = ARCMIN2HEL(ix/60., iy/60., date=dateo, $
            off_limb=off_limb, Sphere=sphere, r0=Robs)

   coord = TRANSPOSE(tt) 


;----------------------------------------------------- 
;  Calculate the Carrington coords
;-----------------------------------------------------
   tab_out[*,0]  = tt[0,*]
   tab_out[*,1]  = ((tt[1,*] + 360.) MOD 360. + L0[0]) MOD 360.


RETURN,tab_out


END


