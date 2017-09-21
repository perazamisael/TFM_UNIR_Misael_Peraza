;+
; NAME:
;       egso_sfc_line_direction
;
; PURPOSE:
;
;       Given the subscripts of pixels corresponding to
;       thinned shapes on the Sun disk, find the possible
;       lines, and return these lines pixels subscripts and eventually
;       their main direction.(see egso_sfc_line_remove for input) 
;        
;       The main orientation is computed by projecting pixels
;       positions onto a rotating referential and getting the maximum
;       value of the projection histogram at each angle
;
;       Nb1: projections are done both onto Ox and Oy for each angle
;       (0-90) to go faster.
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
;       copyright (C) 2005 Nicolas Fuller, Observatoire de Paris
;       This program is free software; you can redistribute it and/or modify it under the terms of the
;       GNU General Public License as published by the Free Software Foundation;
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Solar Feature Catalogue
;
; CALLING SEQUENCE:
;
;        subsout = egso_sfc_line_direction(subsin,diam,xs,ys[,MAIND=maind])
;
; INPUTS
;
;        subsin  : subscripts of thinned shapes 
;        diam    : Sun diameter in pixels
;        xs      : Original image size (Ox)
;        ys      : Original image size (Oy)
;
;
; OUTPUTS:
;
;        Subscripts of lines pixels if a main orientation is found, -1
;        if not   
;        
;
; OUTPUT KEYWORDS:
;
;       MAIND    : Main orientation of lines (degrees)
;
;
; MODIFICATION HISTORY:
;
;  NF Feb 2005 last revision
;-


;###########################################################################

FUNCTION EGSO_SFC_LINE_DIRECTION,subsin,diam,xs,ys,MAIND=maind

 
  ;##### Check the number of input parameters
  IF N_PARAMS() NE 4 THEN BEGIN
     PRINT,'EGSO_SFC_LINE_DIRECTION - Ex: res=EGSO_SFC_LINE_DIRECTION(subs,840,1024,1024,MAIND=dir)'
     RETALL
  ENDIF


  ;#### The following variables may be modified to fit better other 
  ;#### image sets than Meudon
  ;#### VAR1 is used to find the main orientation (see code below)
  ;#### VAR2 is used to threshold lines points (see code below)
  VAR1 = 1.6
  VAR2 = 3.8


  ;#### Get (x,y) coordinates in Sun centered referential
  xcoo = subsin MOD FIX(xs) - xs/2 + 0.5
  ycoo = subsin / FIX(xs) - ys/2 + 0.5


  ;#### Get a normalisation profile
  ;#### (a bit larger than disk to avoid
  ;#### problems near limb: radius*1.2)
  diam = LONG(diam)
  tabn1 = INDGEN(diam)-diam/2+0.5
  tabn2 = 2*SQRT((1.2*diam/2)^2-tabn1^2)


  ;#### Get distance to Sun center and
  ;#### and angle to Ox
  dis = SQRT(xcoo^2+ycoo^2)
  angOx = ATAN(ycoo,xcoo)


  ;#### Set variables
  newmax = 0
  maxsum = 0


  ;#### Compute main orientation by cumulating
  ;#### projected points on rotating referential
  FOR ii = 0,89 DO BEGIN
    ang = ii/!radeg
    histc = HISTOGRAM(FIX(dis*COS(angOx-ang)+diam/2),min=0,binsize=1,nbins=840)
    hists = HISTOGRAM(FIX(dis*SIN(angOx-ang)+diam/2),min=0,binsize=1,nbins=840)
    maxvX = MAX(histc/tabn2)
    maxvY = MAX(hists/tabn2)
    maxsum = maxsum + maxvX + maxvY
    IF maxvX GT newmax THEN BEGIN
       porien = ii+90
       newmax = maxvX
    ENDIF
    IF maxvY GT newmax THEN BEGIN
       porien = ii
       newmax = maxvY
    ENDIF
   ENDFOR


  ;#### Check if the main direction correspond to
  ;#### at least a value greater than x times the mean
  IF newmax LT VAR1*(maxsum/180.) THEN RETURN,-1


  ;####Get a more precise orientation value +/- 0.1
  newmax = 0
  porien = FLOAT(porien)
  FOR ii = porien-1,porien+1,0.1 DO BEGIN
    ang = ii/!radeg
    hists = HISTOGRAM(FIX(dis*SIN(angOx-ang)+diam/2),min=0,binsize=1,nbins=840)
    maxvY = MAX(hists/tabn2)
    IF maxvY GT newmax THEN BEGIN
       neworien = ii
       newmax = maxvY
   ENDIF
  ENDFOR


  ;#### Get points x times higher than mean
  ;#### of normalized profile locations
  cumul = HISTOGRAM(FIX(dis*SIN(angOx-neworien/!radeg)+diam/2),min=0,binsize=1,nbins=840)/tabn2
  locs = WHERE(cumul GT MEAN(cumul)*VAR2)
  IF locs[0] EQ -1 THEN RETURN,-1 ELSE locs = locs+(xs/2-diam/2)


  ;#### Set MAIND keyword
  PRINT,'EGSO_SFC_LINE_DIRECTION: Main line orientation:',neworien
  maind = neworien[0]


  ;#### Get subscript of line points
  imres = BYTARR(xs,ys)
  imres[*,locs] = 1b
  imres = ROT(imres,-1.*neworien[0])
  subres = WHERE(TEMPORARY(imres))

RETURN,subres

END


