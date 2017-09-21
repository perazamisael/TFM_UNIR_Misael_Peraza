
;+
; NAME:
;       efr_line_remove
;
; PURPOSE:
;
;       This function intend to remove dust lines on 
;       solar images using threshold/thinning/preferential
;       line orientation query
;       (replace line pixel values according to
;       their neighbors values)
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
;        res = efr_line_remove(imin,diam[,flatok=flatok,noflatres=noflatres,display=display])
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
;       Display :  display different steps of processing
;       flatok  :  set this key. if input image as already been
;                  flatten (efr_flatten)
;       noflatres : The default is to return a flatten result, set
;                   this key. if you don't want the res. to be
;                   flatten (but an intermediate image will be flatten
;                   in order to remove lines)
;
;       (NB) Setting both flatok and noflatres returns an error
;
; CALLS:
;
;      EFR_FLATTEN (optional)
;      EFR_LINE_DIRECTION
;
; MODIFICATION HISTORY:
;
;      NF feb 2005: last revision
;-


;###########################################################################

FUNCTION EFR_LINE_REMOVE,imin,diam,FLATOK=flatok,NOFLATRES=noflatres,DISPLAY=display,MAIND=maind

;###########################################################################


  ;##### Check the number of input parameters
  IF N_PARAMS() LT 2 THEN BEGIN
     PRINT,'EFR_LINE_REMOVE - Ex: res=EFR_LINE_REMOVE(image,840,/DISPLAY)'
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
     dis_xs = !d.X_SIZE
     ;WINDOW,/FREE,xs=xsize,ys=ysize
     TVSCL,REBIN(imin,dis_xs,dis_xs)
   ENDIF


  ;##### If input image is not flatten then flatten it
  IF NOT KEYWORD_SET(flatok) THEN imflt = EFR_FLATTEN(imin,diam) ELSE $
  imflt = imin


  ;##### nul points
   nulimage = WHERE(imflt LE 0.)


  ;##### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,REBIN(imflt,dis_xs,dis_xs)


  ;##### Get stats of the input
   mom = MOMENT(imflt[WHERE(imflt)],SDEV=sdev)
   meanv = mom[0]


  ;##### Compute the threshold from the cumulative histogram
  ;##### to get the binary image
   sunsurf = LONG(diam/2.)^2*!pi
   npts = sunsurf/4.
   c_hist = HIST_EQUAL(imflt[WHERE(imflt)],BINSIZE=1,/HISTOGRAM_ONLY,MINV=0)
   minval = (WHERE(c_hist GE npts,nsubs))[0]


  ;##### Get a binary image
   binim = imflt LT minval
   binim[nulimage] = 0b


  ;#### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,REBIN(binim,dis_xs,dis_xs)


  ;##### Get the skeleton of all regions
  ;##### in order to get straight lines for
  ;##### dust lines and irregular lines for filaments
  ;##### NB: SKELETAL values = 2 !
   binim = THIN(TEMPORARY(binim))


  ;#### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,REBIN(binim,dis_xs,dis_xs)


  ;##### Find the lines preferential orientation
  ;##### by summing at different angles
   IF NOT KEYWORD_SET(display) THEN imb = EFR_LINE_DIRECTION(binim,diam,MAIND=maind) ELSE $
   imb = EFR_LINE_DIRECTION(binim,diam,/dis,MAIND=maind)


  ;#### If lines have been found
  IF imb[0] NE -1 THEN BEGIN

    ;#### Optional display
     IF KEYWORD_SET(display) THEN TVSCL,REBIN(imb,dis_xs,dis_xs)

    ;##### Get the points corresponding to lines
     imb[nulimage] = 0
     tochange = WHERE(imb)
     

    ;##### Enlarge this set of points to insure
    ;##### all the line points will be removed
     tochange = [tochange,tochange-xsize,tochange+xsize,$
                 tochange-2*xsize,tochange+2*xsize]
     tochange = tochange[UNIQ(tochange,SORT(tochange))]


    ;##### Optional Display
    IF KEYWORD_SET(display) THEN BEGIN
       imin2 = imin
       imin2[tochange] = imin2[tochange]+500
       TVSCL,REBIN(imin2,dis_xs,dis_xs)
    ENDIF

    ;##### Change only points with a low value (dark lines should
    ;##### correspond to low values)
     tochange = tochange[WHERE(imflt[tochange] LT meanv - 0.5*sdev $
                             AND imflt[tochange] GT 0)]

    ;##### Replace the pixel values
     IF KEYWORD_SET(noflatres) THEN imflt = imin
     imtmp = imflt
     imtmp[tochange] = 0

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

 ENDIF ELSE BEGIN
   IF KEYWORD_SET(noflatres) THEN imflt = imin
   maind = '\N'
 ENDELSE
;#### Optional display
IF KEYWORD_SET(display) THEN TVSCL,REBIN(imflt,dis_xs,dis_xs)

;####  
RETURN,imflt

END






















