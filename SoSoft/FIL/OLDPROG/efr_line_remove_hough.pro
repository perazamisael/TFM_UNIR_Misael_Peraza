
;+
; NAME:
;       efr_line_remove_hough
;
; PURPOSE:
;
;       This function intend to remove dust lines on 
;       solar images using the Hough transform (replace
;       line pixel values according to their neighboors 
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
;        res = efr_line_remove_hough(imin,diam)
;
; INPUTS
;
;        imin  : input n x n image 
;        diam  : Sun diameter in pixels
;
;
; OUTPUTS:
;
;       cleaned image - n x n array   
;
;
; INPUT KEYWORDS:
;
;       Display :  display different stages of processing image
;       flatok  :  set this key. if Sun disk as already been flatten
;       noflatres : set this key. if you don't want the res. to be flatten
;       (NB) Setting both flatok and noflatres returns an error
;
; MODIFICATION HISTORY:
;
;-


;###########################################################################

FUNCTION EFR_LINE_REMOVE_hough,imin,diam,FLATOK=flatok,NOFLATRES=noflatres,DISPLAY=display

;###########################################################################


  ;##### Check the number of input parameters
  IF N_PARAMS() LT 2 THEN BEGIN
     PRINT,'EFR_LINE_REMOVE_HOUGH - Ex: res=EFR_LINE_REMOVE_HOUGH(image,840,/DISPLAY)'
     RETALL
  ENDIF


  ;##### Image size
   xsize = (SIZE(imin))[1]
   ysize = (SIZE(imin))[2]


  ;##### Keywords flatok and noflatres are incompatible
  IF KEYWORD_SET(flatok) AND KEYWORD_SET(noflatres) THEN BEGIN
    PRINT,'EFR_LINE_REMOVE: You cannot set both FLATOK and NOFLATRES'
    RETALL
  ENDIF


  ;##### Optional display
   IF KEYWORD_SET(display) THEN BEGIN
     WINDOW,/FREE,xs=xsize,ys=ysize
     TVSCL,imin
   ENDIF


  ;##### If input image is not flatten then flatten it
  IF NOT KEYWORD_SET(flatok) THEN imflt = EFR_FLATTEN(imin,diam) ELSE $
  imflt = imin


  ;##### If you don't want the result to be flatten
  IF KEYWORD_SET(noflatres) THEN imo = imin
   

  ;##### nul points
   nulimage = WHERE(imflt LE 0.)


  ;##### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,imflt


  ;##### Get stats of the input
   mom = MOMENT(imflt[WHERE(imflt)],SDEV=sdev)
   meanv = mom[0]


  ;##### Compute the threshold to get the binary image
  ;##### which will feed the Hough transform
   sunsurf = LONG(diam/2.)^2*!pi
   npt     = sunsurf
   minval  = meanv
   ;##### Step by step thresh. diminishes 
   ;##### until the proportion of pixels under tresh.
   ;##### is lower or equal to 20% 
   WHILE (npt/sunsurf) GT 1/5. DO BEGIN
      tt = WHERE(imflt LT minval AND imflt GT 0,npt)
      minval = minval - sdev/30.
   ENDWHILE


  ;##### Define the number of points necessary to identify a line
  ;##### (proportionnal to diam)

     nbpts = FIX(diam / 8.4)
     nbpts_test = diam
     seuilpts = nbpts
     npass = 0


WHILE nbpts GE seuilpts DO BEGIN


  ;##### Get a binary image

   binim = imflt LT minval
   binim[nulimage] = 0b


  ;#### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,binim


  ;##### Get the skeleton of all regions
  ;##### in order to get straight lines for
  ;##### dust lines and irregular lines for filaments

   binim=THIN(TEMPORARY(binim))


  ;#### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,binim


  ;##### First HOUGH to determine if there
  ;##### are any lines (at least nbpts points)

    imh = HOUGH(TEMPORARY(binim),RHO=rho,THETA=theta)
    nbpts = MAX(imh)
    npass = npass + 1
    print,'Hough pass '+STRTRIM(npass,2)


    ;#### Test if the process is not stuck
    IF nbpts GE nbpts_test THEN nbpts = 0
    nbpts_test = nbpts


    IF nbpts GE seuilpts THEN BEGIN


     ;##### Hough transform / treshold / backprojection


       ;### Taking seuilpts (ie min) instead of nbpts (ie max)
       ;### here would give a much larger line in the backproject.
       ;### i.e. only lines with at least 0.9*nbpts are removed at
       ;### a time (while condition)

       imt = (TEMPORARY(imh)-nbpts*0.9) > 0
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

         IF KEYWORD_SET(noflatres) THEN BEGIN
            box = imin(xx-a:xx+a,yy-a:yy+a)
            imo[tochange[ii]]=MEAN(box[wbox])
         ENDIF

      ENDFOR


  ENDIF

;#### Optional display
IF KEYWORD_SET(display) THEN TVSCL,imflt


;#### Continue until there are no more lines
ENDWHILE


;#### Optional display
IF KEYWORD_SET(display) THEN TVSCL,imflt
IF KEYWORD_SET(display) AND KEYWORD_SET(noflatres) THEN TVSCL,imo


;####  
IF KEYWORD_SET(noflatres) THEN imres=TEMPORARY(imo) ELSE imres=TEMPORARY(imflt)

RETURN,imres

END
