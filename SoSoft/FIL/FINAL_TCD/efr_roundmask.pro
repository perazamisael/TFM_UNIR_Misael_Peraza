
;+
; NAME:
;       efr_roundmask
;
; PURPOSE:
;
;       Get the subscripts of pixels in a ring (btw minr and maxr)
;       in an image of size (xsize,ysize) and centered at 
;       [(xsize-1)/2.,(ysize-1/)2.] (or set xcen / ycen keyword) 
;
; AUTHOR:
;
;       Fuller Nicolas
;       LESIA / POLE SOLAIRE
;       Observatoire de MEUDON
;       5 place Jules Janssen
;       92190 MEUDON
;       E-mail: nicolas.fuller@obspm.fr
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Cleaning software
;
; CALLING SEQUENCE:
;
;        res = efr_roundmask(xsize,ysize,minr,maxr[,xcen=xcen,ycen=ycen,comp=comp])
;
; INPUTS
;
;        xsize : X Size of image where the subscripts are to be computed 
;        xsize : Y Size ""              ""             ""             ""
;        minr  : inner radius of the ring 
;        maxr  : outer radius
;
; OUTPUTS:
;
;       subscripts of the ring
;
;
; INPUT KEYWORDS:
;
;       Xcen   : X center coordinates in pixels if Sun not centred 
;       Ycen   : Y center coordinates in pixels if Sun not centred
;
; OUTPUT KEYWORDS
;
;       comp   : complement, subscripts which are not returned in result 
;
; MODIFICATION HISTORY:
;
;-  NF. March 2004 : add Xcen/Ycen keywords
;-  NF. May 2004   : comp keyword replacing inv

;###########################################################################

FUNCTION EFR_ROUNDMASK,xsize,ysize,minr,maxr,XCEN=xcen,YCEN=ycen,COMP=comp

;###########################################################################


  ;#### Sun centre coordinates
  IF NOT KEYWORD_SET(XCEN) THEN  xc = (xsize-1)/2d ELSE xc = xcen
  IF NOT KEYWORD_SET(YCEN) THEN  yc = (ysize-1)/2d ELSE yc = ycen

  IF minr GE maxr THEN RETALL
  IF maxr GT ((xc < (xsize-xc)) < (yc < (ysize-yc))) THEN BEGIN
     PRINT,'EFR_ROUNDMASK:  Warning, subscripts outside the image!'
  ENDIF


  mask_SUBS   = LINDGEN(xsize,ysize)
  mask_SUBS_Y = mask_SUBS /   xsize
  mask_SUBS_X = mask_SUBS MOD xsize
  mask_DIST   = SQRT((mask_SUBS_X - xc)^2 + $
                     (mask_SUBS_Y - yc)^2)
  mask        = WHERE(mask_DIST GE minr AND mask_DIST LT maxr,nmsk, $
                      COMP=comp,NCOMP=nmask_comp)


RETURN,mask

END


