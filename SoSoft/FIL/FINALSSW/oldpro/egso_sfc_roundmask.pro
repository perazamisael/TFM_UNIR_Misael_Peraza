;+
; NAME:
;       egso_sfc_roundmask
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
;       copyright (C) 2005 Nicolas Fuller, Observatoire de Paris
;       This program is free software; you can redistribute it and/or modify it under the terms of the
;       GNU General Public License as published by the Free Software Foundation;
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Solar Feature Catalog
;
; CALLING SEQUENCE:
;
;        res = egso_sfc_roundmask(xsize,ysize,minr,maxr[,XCEN=xcen,YCEN=ycen,COMP=comp])
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
;  NF Feb 2005 : Last revision
;-


;###########################################################################

FUNCTION EGSO_SFC_ROUNDMASK,xsize,ysize,minr,maxr,XCEN=xcen,YCEN=ycen,COMP=comp

;###########################################################################


  ;#### Function parameters check
   IF N_PARAMS() NE 4 THEN BEGIN
      PRINT,'EGSO_SFC_ROUNDMASk calling example: res=EGSO_SFC_ROUNDMASK(1024,1024,0,419,COMP=comp)'
      RETALL
   ENDIF


  ;#### Sun centre coordinates
   IF NOT KEYWORD_SET(XCEN) THEN  xc = (xsize-1)/2d ELSE xc = xcen
   IF NOT KEYWORD_SET(YCEN) THEN  yc = (ysize-1)/2d ELSE yc = ycen


  ;#### Check parameters values
   IF minr GE maxr THEN BEGIN
      PRINT,'EGSO_SFC_ROUNDMASK: min radius must be smaller than max radius!'
      RETALL
   ENDIF
   IF maxr GT ((xc < (xsize-xc)) < (yc < (ysize-yc))) THEN BEGIN
      PRINT,'EGSO_SFC_ROUNDMASK:  Warning, subscripts outside the image!'
   ENDIF


  ;#### Compute the subscripts
   mask_SUBS   = LINDGEN(xsize,ysize)
   mask_SUBS_Y = mask_SUBS / FIX(xsize)
   mask_SUBS_X = mask_SUBS MOD FIX(xsize)
   mask_DIST   = SQRT((mask_SUBS_X - xc)^2 + $
                     (mask_SUBS_Y - yc)^2)
   mask        = WHERE(mask_DIST GE minr AND mask_DIST LT maxr,nmsk, $
                      COMP=comp,NCOMP=nmask_comp)


RETURN,mask

END
