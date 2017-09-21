
;+
; NAME:
;       efr_line_remove
;
; PURPOSE:
;
;       This function intend to remove dust lines on 
;       solar images using threshold/thinning/preferential
;       line orientation query
;       (replace found line pixel values according to
;       their neighboors values)
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
;        res = efr_line_remove(imin,diam)
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

FUNCTION EFR_LINE_REMOVE,imin,diam,FLATOK=flatok,NOFLATRES=noflatres,DISPLAY=display

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
     ;WINDOW,/FREE,xs=xsize,ys=ysize
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
  ; sunsurf = LONG(diam/2.)^2*!pi
  ; npt     = sunsurf
  ; minval  = meanv
  ; ;##### Step by step thresh. diminishes 
  ; ;##### until the proportion of pixels under tresh.
  ; ;##### is lower or equal to 20% 
  ; WHILE (npt/sunsurf) GT 1/5. DO BEGIN
  ;    tt = WHERE(imflt LT minval AND imflt GT 0,npt)
  ;    minval = minval - sdev/30.
  ; ENDWHILE


  ;##### Compute the threshold to get the binary image   
  ;##### with the cumulative histogram
   sunsurf = LONG(diam/2.)^2*!pi
   npts = sunsurf/4.
   c_hist = HIST_EQUAL(imflt[WHERE(imflt)],BINSIZE=1,/HISTOGRAM_ONLY,MINV=0)
   minval = (WHERE(c_hist GE npts,nsubs))[0]


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


  ;##### Find the lines depending on a preferential
  ;##### Orientation by summing at different angles
   IF NOT KEYWORD_SET(display) THEN imb = EFR_LINE_DIRECTION(binim,diam) ELSE $
   imb = EFR_LINE_DIRECTION(binim,diam,/dis)

;il faut faire retourner les locs et la main direction
;puis tourner l'image originale en fonction
;et: si la locs est 592 par example 
tt1=smooth(imrot[594,*],5) 
tt2=smooth(imrot[592,*],5) 
diff=tt1-tt2
imrot2[592,*]=imrot[592,*]+diff 


  ;#### If lines as been found
  IF imb[0] NE -1 THEN BEGIN

    ;#### Optional display
     IF KEYWORD_SET(display) THEN TVSCL,imb

    ;##### Get the points that should be changed
     imb[nulimage] = 0
     tochange = WHERE(imb)
     

    ;##### Enlarge the first set of points to be changed
    ;##### to insure all the line points will be removed
     tochange = [tochange,tochange-xsize,tochange+xsize,$
                 tochange-2*xsize,tochange+2*xsize]
     tochange = tochange[UNIQ(tochange,SORT(tochange))]


    ;##### Optional Display
    IF KEYWORD_SET(display) THEN BEGIN
       imin2 = imin
       imin2[tochange] = imin2[tochange]+500
       TVSCL,imin2
    ENDIF


    ;##### Change only points with a low value
     tochange = tochange[WHERE(imflt[tochange] LT meanv - 0.2*sdev $
                             AND imflt[tochange] GT 0)]

tochangetab=LONARR(N_ELEMENTS(tochange),41)
FOR ii=-20,20 DO BEGIN
tochangetab[*,ii+20]=tochange+ii*xsize
ENDFOR

    ;##### Replace the pixel values
     imtmp = imflt
   ;  imtmp[tochange] = 0

     FOR ii=0L,N_ELEMENTS(tochange)-1 DO BEGIN

subs=tochangetab[ii,*]
good=0
bad=0
for jj=0,40 DO BEGIN
toto=where(tochange EQ subs[jj])
IF toto EQ -1 THEN good=[good,subs[jj]] ELSE bad=[bad,subs[jj]]
endfor
baseg=MEDIAN(imflt[good[1:N_ELEMENTS(good)-1]])
baseb=MEDIAN(imflt[bad[1:N_ELEMENTS(bad)-1]])

;base=imtmp[tochangetab[ii,*]]
;titi=WHERE(base LE 0,COMP=toto,nt)
; if nt GT 0 THEN base[titi]=MEAN(base[toto])
;plot,base
;smt=smooth(base,15)
;imflt[tochange[ii]]=smt[20]+30
;imflt[tochange[ii]]=median(base[toto])
imflt[tochange[ii]]=imflt[tochange[ii]]+(baseg-baseb)
goto,suite
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
suite:
     ENDFOR

   ENDIF

;#### Optional display
IF KEYWORD_SET(display) THEN TVSCL,imflt
IF KEYWORD_SET(display) AND KEYWORD_SET(noflatres) THEN TVSCL,imo


;####  
IF KEYWORD_SET(noflatres) THEN imres=TEMPORARY(imo) ELSE imres=TEMPORARY(imflt)

RETURN,imres

END
