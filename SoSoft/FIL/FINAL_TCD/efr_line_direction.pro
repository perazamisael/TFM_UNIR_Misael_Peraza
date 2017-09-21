
;+
; NAME:
;       efr_line_direction
;
; PURPOSE:
;
;       Rotate a binary Sun disk and sum on Ox after
;       each rotation (0-180) -> dust lines on the disk give
;       high values in a particular direction
;       The binary image returned equal 1 for line points and 0
;       elsewhere.
;       Nb: summations are done both on Ox and Oy after each rotation
;       (0-90) to go faster (90° is added to orientation value if
;       the summation is done on Oy
;       Nb2: A normalization profile is computed because lines near
;       the limb of the Sun a shorter than at the equator!
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
;        res = efr_line_direction(imin,diam[,display=display])
;
; INPUTS
;
;        imin  : input n x n image 
;        diam  : Sun diameter in pixels
;
;
; OUTPUTS:
;
;        n x n binary array if a main orientation is found, -1 if not   
;        
; INPUT KEYWORDS:
;
;       Display :  display different stages of processing
;
; CALLS:
;
;      EFR_ROUNDMASK
;
; MODIFICATION HISTORY:
;
;  NF nov 2004 last revision
;-


;###########################################################################

FUNCTION EFR_LINE_DIRECTION,im,diam,DISPLAY=display,MAIND=maind

  im = BYTE(im)

  xsize = (SIZE(im))[1]
  ysize = (SIZE(im))[2]

  xsizs = 512
  ysizs = 512

  ;#### The following variables can be modified to fit better other 
  ;#### image sets than Meudon
  ;#### VAR1 is used to find the main orientation (see code below)
  ;#### VAR2 is used to threshold lines points (see code below)
  VAR1 = 1.6
  VAR2 = 3.8

  ;#### Build a smaller image to get the approximate
  ;#### main direction faster
  IF xsize/xsizs EQ xsize/FLOAT(xsizs) THEN BEGIN
    small = REBIN(im,xsizs,ysizs)
  ENDIF ELSE BEGIN
    small = CONGRID(im,xsizs,ysizs)
  ENDELSE
    small = small GE 1

  ;#### NORMALIZATION PROFILE CALCULATION
  ;#### (a bit larger than disk to avoid
  ;#### pbs near limb)
  mask  = EFR_ROUNDMASK(xsizs,ysizs,0,((diam/(xsize/xsizs))/2.)*1.2)
  cmask = INTARR(xsizs)
  imask = BYTARR(xsizs,ysizs)
  imask[mask] = 1b
  cmask = TOTAL(imask,2)
  cmask = DOUBLE(cmask)/MAX(cmask)
  cmask[WHERE(cmask LE 0)] = 1

  newmax = 0
  maxsum = 0


  ;####TRY TO GET THE MAIN ORIENTATION
  FOR jj = 0,89 DO BEGIN

      imj = ROT(small,-1*jj) ;!ROT is clockwise!
      maxvX = MAX(TOTAL(imj,2)/cmask)
      maxvY = MAX(TOTAL(imj,1)/cmask)
      maxsum = maxsum + maxvX + maxvY
      IF maxvX GT newmax THEN BEGIN
         porien = 90-jj
         newmax = maxvX
      ENDIF
      IF maxvY GT newmax THEN BEGIN
         porien = 180-jj
         newmax = maxvY
     ENDIF
  ENDFOR


  ;#### Check if the main direction correspond to
  ;#### at least a value greater than x times the mean
  IF newmax LT VAR1*(maxsum/180.) THEN RETURN,-1


  ;####Get a more precise orientation value +/- 0.2° with the
  ;####full size image 


  ;####FULL NORMALIZATION PROFILE CALCULATION
  mask  = EFR_ROUNDMASK(xsize,ysize,0,(diam/2.)*1.2)
  cmask = INTARR(xsize)
  imask = BYTARR(xsize,ysize)
  imask[mask] = 1b
  cmask = TOTAL(imask,2)
  cmask = DOUBLE(cmask)/MAX(cmask)
  cmask[WHERE(cmask LE 0)] = 1


  ;####rotation +/- 0.2°
  newmax = 0
  FOR jj = porien-0.6,porien+0.6,0.2 DO BEGIN
      imj = ROT(im,jj)
      maxv = MAX(TOTAL(imj,1)/cmask)
      IF maxv GT newmax THEN BEGIN
         neworien = jj
         newmax = maxv
      ENDIF
  ENDFOR


  ;####Get points x times higher than mean of normalized profile 
  cumul = INTARR(xsize)
  im2   = ROT(im,neworien)
  cumul = TOTAL(im2,1)/cmask
  locs = WHERE(cumul GT MEAN(cumul)*VAR2)
  IF locs[0] EQ -1 THEN RETURN,-1
  PRINT,'Main line orientation, basic and refined:',porien,neworien
  maind = neworien

  ;####make the binary result
  imres = TEMPORARY(im2)*0b
  imres[*,locs] = 1b
  imres = ROT(imres,-1.*neworien[0])

RETURN,imres

END
