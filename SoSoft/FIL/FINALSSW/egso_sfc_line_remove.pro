
;+
; NAME:
;       egso_sfc_line_remove
;
; PURPOSE:
;
;       This function intend to remove dust lines on 
;       standardized solar images using threshold/thinning/preferential
;       line orientation query. It then replace possible line pixel
;       values according to their neighbors values.
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
;        res = egso_sfc_line_remove(imin,diam[,FLATOK=flatok,NOFLATRES=noflatres, 
;              DISPLAY=display,MAIND=maind])
;
; INPUTS
;
;        imin  : input 1024 x 1024 array 
;        diam  : Sun diameter in pixels (840 for standardized images)
;
;
; OUTPUTS:
;
;       cleaned image - 1024 x 1024 array   
;
;
; INPUT KEYWORDS:
;
;       DISPLAY   : Display different steps of processing
;
;       FLATOK    : Set this keyword if input image as already been
;                   flatten (egso_sfc_flatten)
;
;       NOFLATRES : The default is to return a flatten background result.
;                   Set this keyword if you don't want the result to be
;                   flatten (but an intermediate image still need to 
;                   be flatten in order to remove lines)
;       
;       MAIND     : Set this keyword to a named variable that will
;                   contain the main direction of the lines if at
;                   least one is found
;
;       (NB) Setting both flatok and noflatres returns an error
;
;
; MODIFICATION HISTORY:
;
;      NF Feb 2005: last revision
;-


;###########################################################################

FUNCTION EGSO_SFC_LINE_REMOVE,imin,diam,FLATOK=flatok,NOFLATRES=noflatres,DISPLAY=display,MAIND=maind

;###########################################################################


  ;##### Check the number of input parameters
  IF N_PARAMS() NE 2 THEN BEGIN
     PRINT,'EGSO_SFC_LINE_REMOVE - Ex: res=EGSO_SFC_LINE_REMOVE(image,840,/DISPLAY)'
     RETALL
  ENDIF


  ;##### Check Image size
   xsize = (SIZE(imin))[1]
   ysize = (SIZE(imin))[2]
   IF xsize NE 1024 AND ysize NE 1024 THEN BEGIN
      PRINT,'EGSO_SFC_LINE_REMOVE - Input images should be standardized 1024 x 1024 images'
      RETALL
   ENDIF


  ;##### Keywords flatok and noflatres are incompatible
  IF KEYWORD_SET(flatok) AND KEYWORD_SET(noflatres) THEN BEGIN
    PRINT,'EGSO_SFC_LINE_REMOVE - You cannot set both FLATOK and NOFLATRES'
    RETALL
  ENDIF


  ;##### Optional display
   IF KEYWORD_SET(display) THEN BEGIN
     WINDOW,/FREE,xs=xsize,ys=ysize
     TVSCL,imin
   ENDIF


  ;##### If input image is not flatten then flatten it
  IF NOT KEYWORD_SET(flatok) THEN imflt = EGSO_SFC_FLATTEN(imin,diam) ELSE $
  imflt = imin


  ;##### nul and non nul pixels
   nulimage = WHERE(imflt LE 0.,COMP=nnulimage)


  ;##### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,imflt


  ;##### Get stats of the input
   mom = MOMENT(imflt[nnulimage],SDEV=sdev)
   meanv = mom[0]


  ;##### Compute a threshold from the cumulative histogram
  ;##### to get a binary image of dark regions
   sunsurf = LONG(diam/2.)^2*!pi
   npts = sunsurf/4.
   c_hist = HIST_EQUAL(imflt[nnulimage],BINSIZE=1,/HISTOGRAM_ONLY,MINV=0)
   minval = (WHERE(c_hist GE npts,nsubs))[0]


  ;##### Get the binary image
   binim = imflt LT minval


  ;##### set background to 0
   binim[nulimage] = 0b


  ;#### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,binim


  ;##### Get the skeleton of all regions
  ;##### in order to get straight lines for
  ;##### dust lines and irregular lines for filaments
  ;##### NB: SKELETAL values = 2 ! (see THIN function)
   binim = THIN(binim)


  ;#### Optional display
   IF KEYWORD_SET(display) THEN TVSCL,binim


  ;##### Find the lines preferential orientation
  ;##### by summing at different angles
   subsline = EGSO_SFC_LINE_DIRECTION(WHERE(binim),diam,xsize,ysize,MAIND=maind)


  ;#### If lines have been found
   IF subsline[0] NE -1 THEN BEGIN


      binim = binim*0b
      binim[subsline] = 1b


     ;#### Optional display
      IF KEYWORD_SET(display) THEN TVSCL,binim


     ;##### Get the points corresponding to lines
      binim[nulimage] = 0b
      tochange = WHERE(binim)
      binim = 0

     ;##### Enlarge this set of points to insure
     ;##### all the line points will be removed
      tochange = [tochange,tochange-xsize,tochange+xsize,$
                  tochange-2*xsize,tochange+2*xsize, $
                  tochange-1,tochange+1,tochange-2,tochange+2]
      tochange = tochange[UNIQ(tochange,SORT(tochange))]


     ;##### Optional Display
      IF KEYWORD_SET(display) THEN BEGIN
         imin2 = imin
         imin2[tochange] = imin2[tochange]+500
         TVSCL,imin2
         imin2 = 0
      ENDIF

     ;##### Change only points with a low value (dark lines should
     ;##### correspond to low values)
      tochange = tochange[WHERE(imflt[tochange] LT meanv - 0.5*sdev $
                             AND imflt[tochange] GT 0)]


     ;##### Change pixels values on original image
     ;##### if keyword noflatres is set
      IF KEYWORD_SET(noflatres) THEN imflt = imin
      imtmp = imflt
      imtmp[tochange] = 0


     ;##### Pixel coordinates 
      xx = tochange MOD FIX(xsize)
      yy = tochange / FIX(xsize)


     ;##### Loop on each pixel value to replace      
      FOR ii=0L,N_ELEMENTS(tochange)-1 DO BEGIN

  
         ;##### Compute an average value in a box centered
         ;##### on the pixel to be replaced
         ;##### Try to find a value from the close neighborhood
         ;##### at least 8 non zero values to get a mean value
          nbox = 0
          a    = 1
          WHILE nbox LT 8 DO BEGIN
              box  = imtmp(xx[ii]-a:xx[ii]+a,yy[ii]-a:yy[ii]+a)
              wbox = WHERE(box,nbox)
              a = a + 1
          ENDWHILE
          imflt[tochange[ii]] = MEAN(box[wbox])

      ENDFOR

      imtmp = 0

 ENDIF ELSE BEGIN
     binim = 0
     maind = '\N'
     IF KEYWORD_SET(noflatres) THEN imflt = imin
 ENDELSE


 ;#### Optional display
 IF KEYWORD_SET(display) THEN TVSCL,imflt

  
RETURN,imflt

END






















