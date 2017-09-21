Function flatbckgV2,input=input

IF NOT KEYWORD_SET(input) THEN $
input=readfits('/home/fuller/poub/FITS/PROCESSED/mh020104.142800_subtract_processed.fits')

tvscl,input

xsiz = (SIZE(input))[1]
usiz = FIX(xsiz*1.2)
IF xsiz/256. EQ xsiz/256 THEN $
intfac = 1 ELSE intfac = 0

nnulinput = WHERE(input GT 0.)
nulinput  = WHERE(input LE 0.)
meaninput = MEAN(input[nnulinput])


;copy=input
;copy2=input

;##### Extend the border of the sun before computing the median
under  = CONGRID(input, usiz, usiz)
under  = under[(usiz-xsiz)/2:(usiz-xsiz)/2+xsiz-1, $
               (usiz-xsiz)/2:(usiz-xsiz)/2+xsiz-1] 
under[nnulinput] = input[nnulinput]


;##### Median filter on a resized image
IF intfac THEN undreb = REBIN(under,256,256) ELSE $
               undreb = CONGRID(under,256,256)
flat1  = MEDIAN(undreb,40)


;##### Subtract the median image from the original
;##### and adjust by adding the mean of input
IF intfac THEN flat1reb = REBIN(flat1,xsiz,xsiz) ELSE $
               flat1reb = CONGRID(flat1,xsiz,xsiz)
interm = under - flat1reb
interm[nulinput] = 0.
meantmp = MEAN(interm[WHERE(interm)])
interm = interm - meantmp + meaninput
interm[nulinput] = 0.

tvscl,interm


;##### Define the threshold to discard high and low values
hist = HISTOGRAM(interm[WHERE(interm)],min=0.)
maxh = MAX(hist)
wmax = (WHERE(hist GE maxh/2.,nm))[nm-1]
wmin = (WHERE(hist GE maxh/10.))[0]
high = WHERE(interm GT wmax OR (interm LT wmin AND interm GT 0.),nhi)

;copy[high] = 0.
copy2=interm
copy3=interm
interm[high]=0.

tvscl,interm

;##### Fill the holes with a mean value of the neighbor pixels
;##### (at least 50 values)
FOR ii=0l,nhi-1 DO BEGIN
     xx = high[ii] MOD xsiz
     yy = high[ii]/xsiz
     nbpts = 0
     siz2 = 20
     WHILE nbpts LT 50 AND (xx-siz2) GE 0 AND (xx+siz2-1) LT xsiz AND $
                           (yy-siz2) GE 0 AND (yy+siz2-1) LT xsiz DO BEGIN
;        box = copy[xx-siz2:xx+siz2-1,yy-siz2:yy+siz2-1]
        box = interm[xx-siz2:xx+siz2-1,yy-siz2:yy+siz2-1]         
        nnul = WHERE(box,nbpts)
        siz2 = siz2*1.5
     ENDWHILE
     copy2[high[ii]]=MEAN(box[WHERE(box)])   
ENDFOR


;##### Extend the border of the sun before computing the median
under2  = CONGRID(copy2, usiz, usiz)
under2  = under2[(usiz-xsiz)/2:(usiz-xsiz)/2+xsiz-1, $
                 (usiz-xsiz)/2:(usiz-xsiz)/2+xsiz-1] 
under2[nnulinput] = copy2[nnulinput]


;##### Median filter on a resized image
IF intfac THEN undreb2 = REBIN(under2,256,256) ELSE $
               undreb2 = CONGRID(under2,256,256)
flat2  = MEDIAN(undreb2,20)


;##### Subtract the median image from the original
;##### and adjust by adding the mean of input
IF intfac THEN flat2reb = REBIN(flat2,xsiz,xsiz) ELSE $
               flat2reb = CONGRID(flat2,xsiz,xsiz)
final = copy3 - flat2reb
final[nulinput] = 0.
meantmp = MEAN(final[WHERE(final)])
final = final - meantmp + meaninput
final[nulinput] = 0.


tvscl,final
;tvscl,flat2
tvscl,congrid(input,400,400)
tvscl,rebin(input-final,256,256),768,0

RETURN,final
END
