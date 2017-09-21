;+
; Project     : EGSO
;
; Name        : PIX2CARR()
;
; Purpose     : Convert pixel coordinate to Carrington or heliographic
;
; Explanation :
;  
; Use         : IDL> res = PIXCARR(indices,h)             
;
; Inputs      : pixel indices and fits header info (structure h)
;
;
; Keywords    : FIXL0 - fixes the Carrington long. L0 value to fixl0
;
;            ;SoP=centroid indice
;            ;SoP for Set of Points, calculate the lat. and long. of the points
;            ;so that the values are continuous. ie: if the centroid belongs to
;            ;the rotation N and a part of the set belongs to
;            ;rotation N-1 or N+1 the long. values would not be like
;            ;[357,358,359,0,1,2,3,4...] but like [-3,-2,-1,0,1,2,3,4...]
;
; Outputs     : Function returns a 2xN element vector: [lat, long] in
;               degrees, where N is number of indices
;
; Calls       : ARCMIN2HEL,TIM2CARR
; 
; NB          : - P0 angle is not taken into account by Arcmin2hel
;                 assume that image is corrected from it. 
;               - Arcmin2hel() corrects coords from B0 angle calculated
;                 with pb0r function
;
; Written     : N. Fuller, Meudon Observatory, jan. 2003
;
; Modified    : 
;
;-


FUNCTION PIX2CARR,tab,h,FIXL0=fixl0;,SoP=sop


   tab_out=[[tab*0D],[tab*0D]]

;----------------------------------------------------- 
;  Get image size from h
;-----------------------------------------------------

   xsize = h.NAXIS1
   ysize = h.NAXIS2


;----------------------------------------------------- 
;  Get Solar radius from h instead of using pb0r()
;-----------------------------------------------------
   SUNradi  = h.SOLAR_R


;-----------------------------------------------------
;  Calculate R0 value from Sun radius for arcmin2hel()
;-----------------------------------------------------
   Robs    = 1.0/tan((h.CDELT1*SUNradi)/3600/!radeg)


;-----------------------------------------------------
;  Calculate Carrington long. if necessary
;-----------------------------------------------------
   IF NOT ISVALID(FIXL0) THEN L0 = TIM2CARR(h.DATE_OBS_CSI) $
   ELSE L0=fixl0

;-----------------------------------------------------
;  Get pixel coord. from input indices
;-----------------------------------------------------
   arrx = tab MOD xsize
   arry = tab / xsize


;-----------------------------------------------------   
;  Convert pixel to arcseconds
;-----------------------------------------------------
   ix = DOUBLE(h.CDELT1*(arrx - h.CENTER_X) + h.CRVAL1)
   iy = DOUBLE(h.CDELT2*(arry - h.CENTER_Y) + h.CRVAL2)


;-----------------------------------------------------
;  Calculate the heliographic coords
;-----------------------------------------------------
   tt = ARCMIN2HEL(ix/60., iy/60., date=h.DATE_OBS_CSI, $
            off_limb=off_limb, Sphere=sphere, r0=Robs)


   coord = TRANSPOSE(tt) 


;----------------------------------------------------- 
;  Calculate the Carrington coords
;-----------------------------------------------------
   tab_out[*,0]  = tt[0,*]
   tab_out[*,1]  = ((tt[1,*] + 360.) MOD 360. + L0[0]) MOD 360.


RETURN,tab_out


END


