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
;        rsun : Sun radius
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

FUNCTION CLEAN_IMAGE,image,rsun

;###########################################################################

;window,/free,xs=1024,ys=1024
;tvscl,image


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

    usiz  = FIX(xsize*1.2)
    resiz = 256
    IF xsize/(resiz*1.) EQ xsize/resiz THEN $
    intfac = 1 ELSE intfac = 0

    nnulimage = WHERE(image GT 0.)
    nulimage  = WHERE(image LE 0.)
    meanimage = MEAN(image[nnulimage])


    copy=image 
    copy2=image

    ;##### Extend the border of the sun before computing the median
    under  = CONGRID(image, usiz, usiz)
    under  = under[(usiz-xsize)/2:(usiz-xsize)/2+xsize-1, $
                   (usiz-xsize)/2:(usiz-xsize)/2+xsize-1] 
    under[nnulimage] = image[nnulimage]


    ;##### Median filter on a resized image
    IF intfac THEN undreb = REBIN(under,resiz,resiz) ELSE $
                   undreb = CONGRID(under,resiz,resiz)
    flat1  = MEDIAN(undreb,(resiz/256)*40)


    ;##### Subtract the median image from the original
    ;##### and adjust by adding the mean of image
    IF intfac THEN flat1reb = REBIN(flat1,xsize,xsize) ELSE $
                   flat1reb = CONGRID(flat1,xsize,xsize)
    interm = under - flat1reb
    interm[nulimage] = 0.
    meantmp = MEAN(interm[WHERE(interm)])
    interm = interm - meantmp + meanimage
    interm[nulimage] = 0.

;tvscl,interm


    ;##### Define the threshold to discard high and low values
    hist = HISTOGRAM(interm[WHERE(interm)],min=0.)
    maxh = MAX(hist)
    wmax = (WHERE(hist GE maxh/2.,nm))[nm-1]
    wmin = (WHERE(hist GE maxh/10.))[0]
    extr = WHERE(interm GT wmax OR (interm LT wmin AND interm GT 0.),nhi)

    copy[extr] = 0.


    ;##### Replace high/low values by the median values
    copy2[extr]=1.*flat1reb[extr]


    ;##### Extend the border of the sun before computing the median
    under2  = CONGRID(copy2, usiz, usiz)
    under2  = under2[(usiz-xsize)/2:(usiz-xsize)/2+xsize-1, $
                     (usiz-xsize)/2:(usiz-xsize)/2+xsize-1] 
    under2[nnulimage] = copy2[nnulimage]


    ;##### Median filter on a resized image
    IF intfac THEN undreb2 = REBIN(under2,resiz,resiz) ELSE $
                   undreb2 = CONGRID(under2,resiz,resiz)
    flat2  = MEDIAN(undreb2,(resiz/256)*20)

    ;##### Subtract the median image from the original
    ;##### and adjust by adding the mean of image
    IF intfac THEN flat2reb = REBIN(flat2,xsize,xsize) ELSE $
                   flat2reb = CONGRID(flat2,xsize,xsize)

    imflt = image - flat2reb
    imflt[nulimage] = 0.
    meantmp = MEAN(imflt[WHERE(imflt)])
    imflt = imflt - meantmp + meanimage
    imflt[nulimage] = 0.

    ;##### Display
    ;tvscl,CONGRID(image,512,512)
    ;tvscl,CONGRID(imflt,512,512),0,512
    ;tvscl,CONGRID(copy2,512,512),512,512
    ;tvscl,CONGRID(image-imflt,512,512),512,0

tvscl,imflt

;#################################################################
;                      SECOND PART (laplacian version)
;#################################################################

 
  ;##### Get a first treshold value


     mom = MOMENT(imflt[WHERE(imflt)],SDEV=sdev)
     meanv = mom[0]

print,meanv,sdev

  ;##### Define the number of points necessary to identify a line
  ;##### proportionnal to rsun

     nbpts = FIX(rsun / 4.2)
     nbpts_old = 2.*rsun
     seuilpts = nbpts
     filter=REPLICATE(1.,3,3)
     filter[1,1]= -8.
     
WHILE nbpts GE seuilpts DO BEGIN

  ;##### High pass filtering

     binim = CONVOL(imflt,filter)

     momb = MOMENT(binim[WHERE(binim)],SDEV=sdevb,MDEV=mdevb)

print,momb,sdevb,mdevb


  ;##### Get a binary image

;#######################################################
; difficulte a trouver un seuil fiable: fonction de 
; sdev,kurtosis,ou mdev ? a cause des variations du seeing
;#######################################################

     binim = BYTE(binim GT mdevb*2.)
     binim[nulimage] = 0b

tvscl,binim

  ;##### First HOUGH to determine if there
  ;##### are any lines (at least nbpts points)

    im = HOUGH(binim,RHO=rho,THETA=theta)
    nbpts = MAX(im)

print,nbpts
    ;#### Test if the process is not stuck
    IF nbpts GE nbpts_old THEN nbpts = 0;GOTO,dust_line_end
    nbpts_old = nbpts

    IF nbpts GE seuilpts THEN BEGIN


     ;##### Hough transform / treshold / backprojection


       ;### Taking seuilpts (ie min) instead of nbpts (ie max)
       ;### here would give a much larger line in the backproject.
       ;### i.e. only lines with at least 0.9nbpts are removed at
       ;### a time (while condition)

       imt = (im-nbpts*0.9) > 0

       imb = HOUGH(imt,/backproject,RHO=rho,THETA=theta)

tvscl,imb
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


;tvscl,imflt

;##### Continue until there are no more lines

ENDWHILE
;dust_line_end:

tvscl,imflt

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





