;+
; NAME:
;       clean_image
;
; PURPOSE:
;
;       From standardized solar images, this program :
;         - Normalize the intensity over the disk
;         - Remove the darkest dust lines using the Hough transform.
;         - Remove dark points
;       in order to enhance the detection of filaments
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
;        cleanimage = clean_image(image,rsun)
;
; INPUTS
;
;        image : n x n array from a FITS standardized image of the Sun
;
;        rsun : Sun radius (pixels)
;
; OUTPUTS:
;
;       cleanimage - n x n array  
;
; INPUT KEYWORDS:
;
;       None
;
; OUTPUT KEYWORDS:
;
;       None
;
; MODIFICATION HISTORY:
;
;-


;###########################################################################

FUNCTION CLEAN_IMAGE,image,rsun,HOUGH=hough,MEUDON1=meudon1,MEUDON2=meudon2,BBSO=bbso

;###########################################################################

;window,/free,xs=1024,ys=1024
tvscl,image


  ;##### Check image size


     ;#### Image must have 2 dim.
     dim   = (SIZE(image))[0]
     IF dim NE 2 THEN RETURN,0

     ;#### Image must be square
     xsize = (SIZE(image))[1] 
     ysize = (SIZE(image))[2]
     IF xsize NE ysize THEN RETURN,0

     ;#### Size must be a multiple of 4
     IF FLOAT(xsize/4) NE (xsize/4) THEN RETURN,0


;#################################################################
;                      FIRST PART
;#################################################################


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



    ;window,/free,xsize=1024,ysize=1024
    ;tvscl,image

    resiz = 256 ;size of the resized image
    usiz = FIX(resiz*1.2) ;size used to extend the limb

    ;#### Resize original image to a smaller scale
    smallim = REBIN(image,resiz,resiz,/sample)


    ;#### get nul and non nul pixels from image
    nnulimage = WHERE(image GT 0.)
    nulimage  = WHERE(image LE 0.)


    ;#### get nul and non nul pixels from smallim
    nnulsmallim = WHERE(smallim GT 0.)
    nulsmallim  = WHERE(smallim LE 0.)


    ;##### Extend the border of the sun before computing the median
    under  = CONGRID(smallim, usiz, usiz)
    under  = under[(usiz-resiz)/2:(usiz-resiz)/2+resiz-1, $
                   (usiz-resiz)/2:(usiz-resiz)/2+resiz-1] 
    under[nnulsmallim] = smallim[nnulsmallim]


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
    under2  = CONGRID(smallim, usiz, usiz)
    under2  = under2[(usiz-resiz)/2:(usiz-resiz)/2+resiz-1, $
                     (usiz-resiz)/2:(usiz-resiz)/2+resiz-1] 
    under2[nnulsmallim] = smallim[nnulsmallim]


    ;##### Median filter on the new image
    flat2  = MEDIAN(under2,20)


    ;##### Resize to original scale
    flat2rebin = REBIN(flat2,xsize,xsize) 


    ;##### Subtract the median image from the original
    ;##### and adjust by adding the mean of image
    imflt = image - flat2rebin + MEDIAN(flat2rebin[nnulimage])
    imflt[nulimage] = 0.


    ;##### Display
    ;tv,bytscl(CONGRID(image,512,512),0.,MAX(image))
    ;tv,bytscl(CONGRID(imflt,512,512),0.,MAX(image)),512,0
    ;tvscl,CONGRID(copy2,512,512),512,512
    ;tvscl,CONGRID(image-imflt,512,512),0,512


tvscl,imflt


;#################################################################
;                      SECOND PART
;#################################################################


  ;##### Get a first treshold value

     mom = MOMENT(imflt[WHERE(imflt)],SDEV=sdev)
     meanv = mom[0]

IF KEYWORD_SET(HOUGH) THEN BEGIN

  ;##### DEFINE THRESHOLD DEPENDING ON OBSERVATORY
  lambda = 0.5
  IF KEYWORD_SET(meudon1) THEN lambda = 0.5
  IF KEYWORD_SET(meudon2) THEN lambda = 1.5
  IF KEYWORD_SET(bbso)    THEN lambda = 0.5   


  ;##### Get a first treshold value
  ;##### Define the number of points necessary to identify a line
  ;##### proportionnal to rsun

     nbpts = FIX(rsun / 4.2)
     nbpts_old = 2.*rsun
     seuilpts = nbpts
     npass = 0

WHILE nbpts GE seuilpts DO BEGIN

;goto,testconv
  ;##### Get a binary image

     minval = FIX(meanv- lambda*sdev)
     binim = imflt LT minval
     binim[nulimage] = 0b

tvscl,binim


  ;##### Get the skeleton of all regions
  ;##### in order to get straight lines for
  ;##### dust lines and irregular lines for filaments

     binim=THIN(TEMPORARY(binim))

;tvscl,binim

;testconv:
;goto,notestconv

   ;#####An other method using the laplacian to reveal the small
   ;#####details like dust lines. The treshold to compute the binary
   ;#####image is found from the convolved image histogram.
   ;#####better results than the previous if the seeing doesn't
   ;#####vary too much. 
;   str=replicate(1,3,3)
;   str[1,1] = -8
;   conv = CONVOL(imflt,str)
;   mini = ABS(MIN(conv))
;   hist = MEDIAN(histogram(conv[where(conv)]),100)
;   maxi = MAX(hist)
;   tresh = maxi/7.
;   ww  = WHERE(hist LT tresh)
;   ww3 = ww[1:N_ELEMENTS(ww)-1] - ww[0:N_ELEMENTS(ww)-2]
;   pt  = ww[where(ww3 EQ MAX(ww3))+1]
;   res = pt - mini
;   binim = conv GT res[0]

;notestconv:


  ;##### First HOUGH to determine if there
  ;##### are any lines (at least nbpts points)

    im = HOUGH(TEMPORARY(binim),RHO=rho,THETA=theta)
    nbpts = MAX(im)
;print,nbpts
    npass = npass + 1
    print,'Hough pass '+STRTRIM(npass,2)

;tvscl,im

    ;#### Test if the process is not stuck
    IF nbpts GE nbpts_old THEN nbpts = 0
    nbpts_old = nbpts


    IF nbpts GE seuilpts THEN BEGIN


     ;##### Hough transform / treshold / backprojection


       ;### Taking seuilpts (ie min) instead of nbpts (ie max)
       ;### here would give a much larger line in the backproject.
       ;### i.e. only lines with at least 0.9nbpts are removed at
       ;### a time (while condition)

       imt = (TEMPORARY(im)-nbpts*0.9) > 0

       imb = HOUGH(TEMPORARY(imt),/backproject,RHO=rho,THETA=theta)



     ;##### Get the points that should be changed


       imb[nulimage] = 0
       tochange = WHERE(imb)

     ;##### Enlarge the first set of points to be changed
     ;##### to insure all the line points will be removed


       tochange = [tochange,tochange+xsize,tochange-xsize, $
                  tochange+1,tochange-1];,tochange-xsize-1 , $
                  ;tochange-xsize+1,tochange+xsize-1,tochange+xsize+1]
       tochange = tochange[UNIQ(tochange,SORT(tochange))]



     ;##### Change only points with a low value


       tochange = tochange[WHERE(imflt[tochange] LT meanv - 0.5*sdev $
                             AND imflt[tochange] GT 0)]



     ;##### Replace the values of the points


       imflt[tochange] = 0 
       imtmp = imflt

;tvscl,imtmp

       FOR ii=0L,N_ELEMENTS(tochange)-1 DO BEGIN
 
          xx = tochange[ii] MOD xsize
          yy = tochange[ii] / xsize

  
       ;##### Compute an average value in a box centered
       ;##### on the pixel to be replaced
       ;##### Try to find a value from the close neighborhood
       ;##### at least 8 non zero values to get a mean value

         nbox = 0
         a    = 1
         WHILE nbox LT 8 DO BEGIN
          box   = imtmp(xx-a:xx+a,yy-a:yy+a)
          wbox  = WHERE(box,nbox)
          a=a+1
         ENDWHILE
         imflt[tochange[ii]] = MEAN(box[wbox])

      ENDFOR


   ENDIF

tvscl,imflt

;##### Continue until there are no more lines

ENDWHILE

tvscl,imflt

ENDIF
;#################################################################
;                      THIRD PART
;#################################################################


  ;##### Replace the high gradient values by a median value,
  ;##### thus eliminating dust points
  ;##### High gradient values are determined by subtracting the
  ;##### original image to the median image and then treshold.
  ;##### Process need to be iterative for large points 
   
  ntc    = 1  &  nn = 1
  tresh  = meanv/10.
  lowval = meanv - 3.0*sdev

  WHILE ntc GT 0 AND nn LE 10 DO BEGIN

     nn = nn + 1

     medim = MEDIAN(imflt,3)

     subim = medim - imflt

     tochange = WHERE(subim GT tresh AND medim GT lowval , ntc)

     IF ntc GT 0 THEN imflt[tochange] = medim[tochange]

  ENDWHILE




tvscl,imflt

;#######################################################################


RETURN,imflt
endi:
END

;#######################################################################
;#######################################################################





