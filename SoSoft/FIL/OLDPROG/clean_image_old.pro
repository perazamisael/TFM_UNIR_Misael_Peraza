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
;       in order to enhance the detection of filaments.
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
;                n must be a multiple of 4 
;                (for standardization see ) 
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

window,/free,xs=1024,ys=1024
tvscl,image


  mask = WHERE(image LE 0.)


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



  ;##### First normalize the intensity by subtracting 
  ;##### a median image

     factor = 4
     usiz   = FIX(xsize*1.1)
     nnul   = WHERE(image)

     ;##### QuietSun intensity
     hist   = HISTOGRAM(image[nnul])
     mhist  = MEDIAN(hist,5)
     QS     = FLOAT((WHERE(mhist eq max(mhist)))[0])   

     ;##### Extend the border of the sun before computing the median
     under  = CONGRID(image, usiz, usiz)
     under  = under[(usiz-xsize)/2:(usiz-xsize)/2+xsize-1, $
                    (usiz-xsize)/2:(usiz-xsize)/2+xsize-1] 
     under[nnul] = image[nnul]

     ;##### Resample to a smaller scale to compute the median faster
     imreb  = REBIN(under, xsize/factor, xsize/factor,/sample)

     ;##### We don't want the median to take in account filaments and plages

     boxsiz = 60/factor
     immed = imreb*0.


     ;##### Median image computed with a large window 
     ;##### which is used to discriminate QS values from sun features
     ;##### in the next part
     medim = REBIN(MEDIAN(REBIN(imreb,xsize/(2*factor),xsize/(2*factor)), $
              100/factor),xsize/factor,xsize/factor)
  ;   medim  = REBIN(medim, xsize, xsize,/sample)

     FOR ii = boxsiz, xsize/factor-boxsiz-1 DO BEGIN
        FOR jj = boxsiz, xsize/factor-boxsiz-1 DO BEGIN
            IF imreb[ii,jj] GT 0. AND medim[ii,jj] GT 0. THEN BEGIN
              box = imreb[ii-boxsiz/2:ii+boxsiz/2-1,jj-boxsiz/2:jj+boxsiz/2-1]
              medimv = medim[ii,jj]
              extrem = WHERE( (box LT medimv*0.9 AND box GT 0.) OR box GT medimv*1.)
              IF extrem[0] NE -1 THEN box[extrem] = 0.;medimv
              IF (where(box))[0] NE -1 THEN immed[ii,jj] = MEAN(box[WHERE(box)]) ELSE $
              immed[ii,jj]=QS
            ENDIF ELSE immed[ii,jj]=0.
        ENDFOR
     ENDFOR

     
     ;##### Resample to the original size
     immed  = REBIN(immed, xsize, xsize,/sample)

     ;##### Subtract the median and adjust by adding the QSun intensity 
     imflt  = image - immed + QS 

     ;##### Cut the temporary edges
     imflt[mask]= 0.


;tvscl,CONGRID(image,600,600)
tvscl,rebin(image,512,512),0,512
tvscl,rebin(imflt,512,512)
immed[mask]=0.
tvscl,rebin(immed,512,512),512,0
medim=rebin(medim,1024,1024)
medim[mask]=0.
tvscl,rebin(medim,512,512),512,512
goto,endi
;#################################################################
;                      SECOND PART
;#################################################################

 
  ;##### Get a first treshold value


     mom = MOMENT(imflt[WHERE(imflt)],SDEV=sdev)
     meanv = mom[0]



  ;##### Define the number of points necessary to identify a line
  ;##### proportionnal to rsun

     nbpts = FIX(rsun / 4.2)
     seuilpts = nbpts



WHILE nbpts GE seuilpts DO BEGIN


  ;##### Get a binary image


     minval = FIX(meanv - 0.5*sdev)
     binim = imflt LT minval
     binim[mask]=0b

tvscl,binim

  ;##### Get the skeleton of all regions
  ;##### in order to get straight lines for
  ;##### dust lines and irregular lines for filaments


     binim=THIN(binim)

tvscl,binim

  ;##### First HOUGH to determine if there
  ;##### are any lines (at least 100 points)


    nbpts=MAX(HOUGH(binim))

    IF nbpts GE seuilpts THEN BEGIN


     ;##### Hough transform / treshold / backprojection


       im = HOUGH(binim,RHO=rho,THETA=theta)
;tvscl,im
       ;### Taking seuilpts (ie min) instead of nbpts (ie max)
       ;### here would give a much larger line in the backproject.
       ;### i.e. one line is removed at a time (while condition)
       imt = (im-nbpts*0.9) > 0
;tvscl,imt
       imb = HOUGH(imt,/backproject,RHO=rho,THETA=theta)
;tvscl,imb

     ;##### Get the points that should be changed


       imb[mask] = 0
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

     print,ntc

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





