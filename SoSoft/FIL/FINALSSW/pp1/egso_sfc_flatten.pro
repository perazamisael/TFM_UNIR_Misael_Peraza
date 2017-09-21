;+
; NAME:
;       egso_sfc_flatten
;
; PURPOSE:
;
;       From standardized solar images (round and centered 
;       solar disk), this program normalize the intensity
;       over the solar disk using median filtering.
;
;       NOTE1: standardized images from EGSO/cleaning code are
;       1024*1024 images with Sun centered at (511.5,511.5) with a
;       diameter of 840 pixels       
;
;       NOTE2: The program was initially written to flatten Solar Full Disk
;       Halpha images, but it may work with other wavelength.
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
;        res = egso_sfc_flatten(image,diam[,DISPLAY=display])
;
; INPUTS
;
;        image : 1024 x 1024 array from a standardized image of the Sun
;        diam  : Sun diameter (pixels)
;
; OUTPUTS:
;
;       flatten image - 1024 x 1024 array  (float!)
;
; INPUT KEYWORDS:
;
;       Display :  display initial and resulting image
;
;
; OUTPUT KEYWORDS:
;
;       None
;
; MODIFICATION HISTORY:
;
;  NF Feb 2005 : last revision
;-


;###########################################################################

FUNCTION EGSO_SFC_FLATTEN,image,diam,DISPLAY=display

;###########################################################################


  ;#### Function parameters check
   IF N_PARAMS() NE 2 THEN BEGIN
      PRINT,'EGSO_SFC_FLATTEN calling example: res=EGSO_SFC_FLATTEN(image,840,/DISPLAY)'
      RETALL
   ENDIF


  ;#### Check image size
   xsize = (SIZE(image))[1]
   ysize = (SIZE(image))[2]
   IF xsize NE 1024 AND ysize NE 1024 THEN BEGIN
     PRINT,'EGSO_SFC_FLATTEN: input should be a 1024*1024 standardized image'
     RETALL
   ENDIF


  ;####OPTIONAL DISPLAY
   IF KEYWORD_SET(display) THEN BEGIN
      WINDOW,/FREE,xs=xsize,ys=ysize
      TVSCL,image
   ENDIF


;###################################################################
; NF 2003
; Normalization of the background intensity in 3 steps:
; - first normalization with a median filter
; - normalized image is thresholded and highest/lowest values
;   are replaced by the median filtered image values
; - second normalization with the median filter of the image
;   calculated above to reduce the effect of bright regions 
;   and dark filaments.
;###################################################################


   ;#### Resize original image to a smaller scale
    resiz = xsize/4
    smallim = REBIN(image,resiz,resiz)


   ;#### Get nul and non nul pixels subscripts
    nnulimage = WHERE(image GT 0.,COMP=nulimage)


   ;#### Get resized image nul and non nul pixels subscripts
    nnulsmallim = WHERE(smallim GT 0.,COMP=nulsmallim)


   ;##### Extend the border of the Sun before computing the median
    rsunsmall = (resiz*1./xsize)*(diam/2.)
    smallimb  = EGSO_SFC_LIMBSYM(smallim,rsunsmall-1,0.2*rsunsmall,/INV)


   ;##### Large median filter on the new image
    smallimb  = MEDIAN(smallimb,40)


   ;##### Subtract the median image from the original
   ;##### and adjust global intensity level
    diffim = smallim - smallimb + MEAN(smallimb[nnulsmallim])
    diffim[nulsmallim] = 0.


   ;##### Define the thresholds to discard high and low values
    hist = HISTOGRAM(diffim[nnulsmallim],MIN=0,BIN=1)
    hist = SMOOTH(hist,10)
    maxh = MAX(hist)
    wmax = (WHERE(hist GE maxh/2.,nm))[nm-1]
    wmin = (WHERE(hist GE maxh/10.))[0]


   ;##### Threshold
    extr = WHERE(diffim GT wmax OR (diffim LT wmin AND diffim GT 0.),nhi)
    diffim = 0


   ;##### Optional Display
    IF KEYWORD_SET(display) THEN BEGIN
      tmpim = smallim
      tmpim[extr] = 0
      TVSCL,tmpim
      tmpim = 0    
    ENDIF


   ;##### Replace high/low values by the median values
    smallim[extr] = 1.*smallimb[extr]


   ;##### Extend the border of the sun before computing the median
    smallimb  = EGSO_SFC_LIMBSYM(smallim,rsunsmall-1,0.2*rsunsmall,/INV)
    smallim = 0


   ;##### Median filter on the new image
    smallimb  = MEDIAN(smallimb,20)


   ;##### Back to original size
    smallimb = REBIN(smallimb,xsize,xsize) 


   ;##### Subtract the median image from the original
   ;##### and adjust global intensity level
    imflt = image - smallimb + MEAN(smallimb[nnulimage])
    imflt[nulimage] = 0.
    smallimb = 0


   ;#####OPTIONAL DISPLAY
    IF KEYWORD_SET(display) THEN TVSCL,imflt


RETURN,imflt

END

