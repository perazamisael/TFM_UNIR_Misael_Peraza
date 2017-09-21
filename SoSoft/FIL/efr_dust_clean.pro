
;+
; NAME:
;       efr_dust_clean
;
; PURPOSE:
;
;       This function intend to remove dust points on 
;       solar images using median filtering (replace
;       dust pixel values according to their neighboors 
;       values)
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
;        res = efr_dust_clean(imin)
;
; INPUTS
;
;        imin  : input n x n image 
;
;
; OUTPUTS:
;
;       cleaned image - n x n array   
;
;
; INPUT KEYWORDS:
;
;       Display :  display image before and after cleaning
;       highval2:  take into account high values too 
;
; MODIFICATION HISTORY:
;
;       NF Apr2004 Bug correction (imin -> imres in loop)
;       NF sep2004 add highval2 keyword
;-


;###########################################################################

FUNCTION EFR_DUST_CLEAN,imin,HIGHVAL2=highval2,DISPLAY=display

;###########################################################################

  ;##### Replace the high gradient values by a median value,
  ;##### thus eliminating dust points
  ;##### High gradient values are determined by subtracting the
  ;##### original image from the median image and then treshold.
  ;##### Process need to be iterative for large points 

  imres  = imin
  mom    = MOMENT(imin[WHERE(imin)],SDEV=sdev)   
  ntc    = 1  &  nn = 1
  tresh  = mom[0]/10.
  lowval = mom[0] - 3.0*sdev
  lowval2 = mom[0] + 3.0*sdev

  ;#### Optional display
  IF KEYWORD_SET(display) THEN BEGIN
     WINDOW,/FREE,xs=(SIZE(imin))[1],ys=(SIZE(imin))[2]
     TVSCL,imin
  ENDIF


  WHILE ntc GT 0 AND nn LE 10 DO BEGIN

     nn = nn + 1
     medim = MEDIAN(imres,3)
     subim = medim - imres

     tochange = WHERE(subim GT tresh AND medim GT lowval , ntc)
     IF KEYWORD_SET(highval2) THEN tochange = WHERE(ABS(subim) GT tresh AND medim GT lowval AND medim LT lowval2, ntc)

     IF ntc GT 0 THEN imres[tochange] = medim[tochange]

  ENDWHILE

  ;#### Optional display
  IF KEYWORD_SET(display) THEN TVSCL,imres

RETURN,imres

END


