;###################################################################

FUNCTION flatbckg,input

;###################################################################
; NF 2003
; Normalisation of the background intensity in 3 steps:
; - first normalisation with a median filter
; - normalised image is thresholded and highest/lowest values
;   are replaced by the median filtered image values
; - second normalisation with the median filter of the image
;   calculated above to reduce the effect of bright regions 
;   and filaments.
;###################################################################




;window,/free,xsize=1024,ysize=1024
;tvscl,input

xsiz  = (SIZE(input))[1]
usiz  = FIX(xsiz*1.2)
resiz = 256
IF xsiz/(resiz*1.) EQ xsiz/resiz THEN $
intfac = 1 ELSE intfac = 0

nnulinput = WHERE(input GT 0.)
nulinput  = WHERE(input LE 0.)
meaninput = MEAN(input[nnulinput])


copy=input
copy2=input

;##### Extend the border of the sun before computing the median
under  = CONGRID(input, usiz, usiz)
under  = under[(usiz-xsiz)/2:(usiz-xsiz)/2+xsiz-1, $
               (usiz-xsiz)/2:(usiz-xsiz)/2+xsiz-1] 
under[nnulinput] = input[nnulinput]


;##### Median filter on a resized image
IF intfac THEN undreb = REBIN(under,resiz,resiz) ELSE $
               undreb = CONGRID(under,resiz,resiz)
flat1  = MEDIAN(undreb,(resiz/256)*40)


;##### Subtract the median image from the original
;##### and adjust by adding the mean of input
IF intfac THEN flat1reb = REBIN(flat1,xsiz,xsiz) ELSE $
               flat1reb = CONGRID(flat1,xsiz,xsiz)
interm = under - flat1reb
interm[nulinput] = 0.
meantmp = MEAN(interm[WHERE(interm)])
interm = interm - meantmp + meaninput
interm[nulinput] = 0.

;tvscl,interm


;##### Define the threshold to discard high and low values
hist = HISTOGRAM(interm[WHERE(interm)],min=0.)
maxh = MAX(hist)
wmax = (WHERE(hist GE maxh/2.,nm))[nm-1]
wmin = (WHERE(hist GE maxh/10.))[0]
high = WHERE(interm GT wmax OR (interm LT wmin AND interm GT 0.),nhi)

copy[high] = 0.


;##### Replace high/low values by the median values
copy2[high]=1.*flat1reb[high]


;##### Extend the border of the sun before computing the median
under2  = CONGRID(copy2, usiz, usiz)
under2  = under2[(usiz-xsiz)/2:(usiz-xsiz)/2+xsiz-1, $
                 (usiz-xsiz)/2:(usiz-xsiz)/2+xsiz-1] 
under2[nnulinput] = copy2[nnulinput]


;##### Median filter on a resized image
IF intfac THEN undreb2 = REBIN(under2,resiz,resiz) ELSE $
               undreb2 = CONGRID(under2,resiz,resiz)
flat2  = MEDIAN(undreb2,(resiz/256)*20)

;##### Subtract the median image from the original
;##### and adjust by adding the mean of input
IF intfac THEN flat2reb = REBIN(flat2,xsiz,xsiz) ELSE $
               flat2reb = CONGRID(flat2,xsiz,xsiz)

final = input - flat2reb
final[nulinput] = 0.
meantmp = MEAN(final[WHERE(final)])
final = final - meantmp + meaninput
final[nulinput] = 0.

;##### Display
;tvscl,CONGRID(input,512,512)
;tvscl,CONGRID(final,512,512),0,512
;tvscl,CONGRID(copy2,512,512),512,512
;tvscl,CONGRID(input-final,512,512),512,0

RETURN,final

END
