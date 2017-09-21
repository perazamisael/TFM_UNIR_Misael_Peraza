
;+
; NAME:
;       egso_sfc_dust_clean
;
; PURPOSE:
;
;       This function intend to remove dust points on 
;       solar images using median filtering (replace
;       dust pixel values according to their neighboors 
;       values). Mainly for photographic plate observations
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
;        res = egso_sfc_dust_clean(imin[,HIGHVAL2=highval2,DISPLAY=display])
;
; INPUTS
;
;        imin  : input n x m image 
;
;
; OUTPUTS:
;
;       cleaned image - n x m array   
;
;
; INPUT KEYWORDS:
;
;       Display :  display image before and after cleaning
;       highval2:  take into account high values too 
;
; MODIFICATION HISTORY:
;
;   NF Feb 2005: last revision
;-


;###########################################################################

FUNCTION EGSO_SFC_DUST_CLEAN,imin,HIGHVAL2=highval2,DISPLAY=display

;###########################################################################

  ;##### Replace the high gradient values by a median value,
  ;##### thus eliminating black dust points
  ;##### High gradient values are determined by subtracting the
  ;##### original image from the median image and then treshold.
  ;##### Process need to be iterative for large points 


  ;#### Image size
   xsize = (SIZE(imin))[1]
   ysize = (SIZE(imin))[2]


  ;#### Optional display
   IF KEYWORD_SET(display) THEN BEGIN
      WINDOW,/FREE,xs=xsize,ys=ysize
      TVSCL,imin
   ENDIF


  ;#### Define thresholds from image stats 
   mom    = MOMENT(imin[WHERE(imin)],SDEV=sdev)   
   tresh  = mom[0]/10.
   lowval = mom[0] - 3.0*sdev
   lowval2 = mom[0] + 3.0*sdev


  ;#### Main loop 
   ntc = 1
   nn  = 0
   imres = imin
   WHILE ntc GE 1 AND nn LT 10 DO BEGIN

      nn = nn + 1
      medim = MEDIAN(imres,3)
      subim = medim - imres
      tochange = WHERE(subim GT tresh AND medim GT lowval,ntc)
      IF KEYWORD_SET(highval2) THEN tochange = WHERE(ABS(subim) GT tresh $
                            AND medim GT lowval AND medim LT lowval2, ntc)
      IF ntc GT 0 THEN imres[tochange] = medim[tochange]

  ENDWHILE
  medim = 0
  subim = 0


  ;#### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,imres


RETURN,imres

END



