
;+
; NAME:
;       efr_sharpen
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
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Cleaning software
;
; CALLING SEQUENCE:
;
;        res = efr_sharpen(imin,coeff)
;
; INPUTS
;
;        imin  : input n x n array 
;        coeff : A coefficient to level the importance of second
;                derivative subtraction ie: the higher coeff is, the
;                less the image will modified
;
;
; OUTPUTS:
;
;       sharpen image - n x n array   
;
;
; REFERENCE: 
; 
;       Digital Image Processing (second edition), R.C. Gonzalez,
;       R.E. Woods, Prentice Hall, p. 132
;
; MODIFICATION HISTORY:
;
;-


;###########################################################################

FUNCTION EFR_SHARPEN,imin,coeff

;###########################################################################

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


