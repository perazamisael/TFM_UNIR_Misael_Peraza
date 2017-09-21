;+
; NAME:
;         EGSO_SFC_PRUNING
;
; PURPOSE:
;         From the pixel skeleton defined by indices, computes
;         the fully pruned skeleton by calculating distance
;         btw node points and end points, and removing pixels
;         on this criterion.
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
;
; CALLING SEQUENCE:
;
;         res = egso_sfc_pruning(indices,xsize,ysize[,CITY=city])
; 
;
; INPUT
;         indices      subscripts of non-null pixels
;         xsize        1st dim of the array
;         ysize        2nd dim of the array
;
; KEYWORD
;         city         Use city block distance
;                      (the default is euclidian dist)
;
; OUTPUTS
;         Output is the pruned skeleton subscripts
; 
; HISTORY:
;
;    NF Mar 2005 Last Rev.
;-

FUNCTION EGSO_SFC_PRUNING,indices,xsize,ysize,CITY=city

   xsize = FIX(xsize)

  ;#### Search for node points and end points
   imske = EGSO_SFC_PIXCUMUL(indices,xsize,ysize)

  ;#### Node points
   wnp4 = WHERE(imske GE 4,NP4)

  ;#### While there are node points the skeleton
  ;#### is not fully pruned
   WHILE NP4 GT 0 DO BEGIN

    ;#### End points
     wnp2  = WHERE(imske EQ 2,NP2)

     xwnp2 = wnp2 MOD xsize
     ywnp2 = wnp2 / xsize
     xwnp4 = wnp4 MOD xsize
     ywnp4 = wnp4 / xsize

     FOR ii = 0, NP4-1 DO BEGIN
      
       IF NOT KEYWORD_SET(city) THEN BEGIN
         dis = SQRT((xwnp2-xwnp4[ii])^2+(ywnp2-ywnp4[ii])^2) ;euclid
       ENDIF ELSE BEGIN
         dis = ABS(xwnp2-xwnp4[ii]) + ABS(ywnp2-ywnp4[ii]) ;city block
       ENDELSE
 
      ;#### Points to be removed:
       mini = WHERE(dis EQ MIN(dis),nmin)
      ;#### In case of same size branches, do something ?... 
      ;IF nmin GT 1...
       xpt = wnp2[mini[0]] MOD xsize
       ypt = wnp2[mini[0]] / xsize
       imske[xpt,ypt]=0b
    ENDFOR

    ske_ind = EGSO_SFC_SKELETON(WHERE(imske),xsize,ysize)
    imske = EGSO_SFC_PIXCUMUL(ske_ind,xsize,ysize)
    wnp4 = WHERE(imske GE 4,NP4)

  ENDWHILE

  ske=WHERE(imske)

RETURN,ske
END

