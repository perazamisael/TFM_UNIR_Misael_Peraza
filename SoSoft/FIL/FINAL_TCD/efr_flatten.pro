;+
; NAME:
;       efr_flatten
;
; PURPOSE:
;
;       From standardized solar images (round and centered 
;       solar disk), this program normalize the intensity
;       over the solar disk using median filtering.
;       (computing is done with a resized 256*256 image)
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
;        res = efr_flatten(image,diam)
;
; INPUTS
;
;        image : n x n array from a standardized image of the Sun
;        diam  : Sun diameter (pixels)
;
; OUTPUTS:
;
;       flatten image - n x n array  
;
; INPUT KEYWORDS:
;
;       Display :  display initial and resulting image
;
; CALLS:
;
;       EFR_LIMBSYM
;
; OUTPUT KEYWORDS:
;
;       None
;
; MODIFICATION HISTORY:
;
;  NF sept 2004 : last revision
;-


;###########################################################################

FUNCTION EFR_FLATTEN,image,diam,DISPLAY=display

;###########################################################################


  ;#####CHECK IMAGE SIZE

     ;#### Image must have 2 dim.
     dim   = (SIZE(image))[0]
     IF dim NE 2 THEN RETALL

     ;#### Image must be square
     xsize = (SIZE(image))[1] 
     ysize = (SIZE(image))[2]
     IF xsize NE ysize THEN RETALL

    ;#### size of the resized image
    resiz = 256

    ;#### Size must be greater than resiz
    IF xsize LT resiz THEN RETALL

    ;#### Size must be a multiple of resiz (for rebin)
    ;#### (or you may use congrid instead)
    IF FLOAT(xsize/resiz) NE (xsize/resiz) THEN RETALL

    ;#### DISPLAY size (square)
    dis_xs = 1024
    
  ;####OPTIONAL DISPLAY
   IF KEYWORD_SET(display) THEN BEGIN
      WINDOW,/free,xs=dis_xs,ys=dis_xs
      TVSCL,REBIN(image,dis_xs,dis_xs)
   ENDIF


;###################################################################
; NF 2003
; Normalization of the background intensity in 3 steps:
; - first normalization with a median filter
; - normalised image is thresholded and highest/lowest values
;   are replaced by the median filtered image values
; - second normalization with the median filter of the image
;   calculated above to reduce the effect of bright regions 
;   and filaments.
;###################################################################


    ;#### Resize original image to a smaller scale
    smallim = REBIN(image,resiz,resiz)


    ;#### get nul and non nul pixels from image
    nnulimage = WHERE(image GT 0.,COMP=nulimage)


    ;#### get nul and non nul pixels from smallim
    nnulsmallim = WHERE(smallim GT 0.,COMP=nulsmallim)


    ;##### Extend the border of the sun before computing the median
    rsunsmall = (resiz*1./xsize)*(diam/2.)
    under  = EFR_LIMBSYM(smallim,rsunsmall-1,0.2*rsunsmall,/INV)


    ;##### Median filter on the new image
    flat1  = MEDIAN(under,40)


    ;##### Subtract the median image from the original
    ;##### and adjust intensity value
    diffim = smallim - flat1 + MEAN(flat1[nnulsmallim])
    diffim[nulsmallim] = 0.


    ;##### Define the threshold to discard high and low values
    hist = HISTOGRAM(diffim[nnulsmallim],min=0.)
    hist = SMOOTH(hist,10)
    maxh = MAX(hist)
    wmax = (WHERE(hist GE maxh/2.,nm))[nm-1]
    wmin = (WHERE(hist GE maxh/10.))[0]
    extr = WHERE(diffim GT wmax OR (diffim LT wmin AND diffim GT 0.),nhi)


    ;##### Replace high/low values by the median values
    smallim[extr]=1.*flat1[extr]


    ;##### Extend the border of the sun before computing the median
    under2  = EFR_LIMBSYM(smallim,rsunsmall-1,0.2*rsunsmall,/INV)


    ;##### Median filter on the new image
    flat2  = MEDIAN(under2,20)


    ;##### Back to original size
    flat2rebin = REBIN(flat2,xsize,xsize) 


    ;##### Subtract the median image from the original
    ;##### and adjust by adding the mean of image
    imflt = image - flat2rebin + MEAN(flat2rebin[nnulimage])
    imflt[nulimage] = 0.


    ;#####OPTIONAL DISPLAY
     IF KEYWORD_SET(display) THEN TVSCL,REBIN(imflt,dis_xs,dis_xs)


RETURN,imflt

END
