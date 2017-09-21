
;+
; NAME:
;       egso_sfc_sharpen
;
; PURPOSE:
;
;       Sharpen the image using High-boost filtering
;       (subtract the second derivative of image: laplacian)
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
;        res = egso_sfc_sharpen(imin,coeff)
;
; INPUTS
;
;        imin  : input n x m array 
;        coeff : A coefficient to level the importance of second
;                derivative subtraction ie: the higher coeff is, the
;                less the image will modified
;
;
; OUTPUTS:
;
;       sharpen image - n x m array   
;
;
; REFERENCE: 
; 
;       Digital Image Processing (second edition), R.C. Gonzalez,
;       R.E. Woods, Prentice Hall, p. 132
;
; MODIFICATION HISTORY:
;
;   NF FEB 2005 : Last Revision
;-


;###########################################################################

FUNCTION EGSO_SFC_SHARPEN,imin,coeff

;###########################################################################

  ;#### Function parameters check
   IF N_PARAMS() NE 2 THEN BEGIN
      PRINT,'EGSO_SFC_SHARPEN calling example: res=EGSO_SFC_SHARPEN(image,1.1)'
      RETALL
   ENDIF

  ;#### Make sure im and coeff are float
   im       = FLOAT(imin)
   coeff    = FLOAT(coeff)


  ;#### Laplacian kernel
   str      = REPLICATE(1,3,3)
   str[1,1] = -8


  ;#### Convolution of image with kernel
   conv     = CONVOL(imin,str)


  ;#### High boost filtering
   imr      = coeff*imin - conv


RETURN,imr
END


