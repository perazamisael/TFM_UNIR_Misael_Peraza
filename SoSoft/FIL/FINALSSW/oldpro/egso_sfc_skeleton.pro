;+
; NAME:
;         EGSO_SFC_SKELETON
;
; PURPOSE:
;         Erode successively the shape corresponding 
;         to the indices until the shape is reduced to
;         its one pixel thick skeleton.
;         The structures used to erode the shape are
;         the following (cf. Hit-or-Miss transform):
;          0 0 0     x 0 0
;          x 1 x     1 1 0
;          1 1 1     x 1 x
;         and the four 90� rotations of them
;         x stands for "don't care" values
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
;         res = egso_sfc_skeleton(indices,xsize,ysize[,EC=ec])
;
; INPUT
;         indices      subscripts of non-null pixels
;         xsize        1st dim of the array
;         ysize        2nd dim of the array
;
; OUTPUT KEYWORDS
;         EC           Set to a named variable that contains
;                      the number of erosions
;                      needed to fully erode the shape
;
; OUTPUTS
;         Output is the skeleton subscripts array
;
; HISTORY:
;
;    NF Mar 2005 last rev.
;-

FUNCTION EGSO_SFC_SKELETON,indices,xsize,ysize,EC=ec
 
  mask = BYTARR(xsize,ysize)
  mask[indices] = 1b
  ec = 0


  ;#################### EROSION STRUCTURES
  h0 = [[-1,-1,-1],[0,1,0],[1,1,1]] 
  m0 = [[0,-1,-1],[1,1,-1],[0,1,0]]  
  h1 = ROTATE(h0,1)
  m1 = ROTATE(m0,1) 
  h2 = ROTATE(h0,2)
  m2 = ROTATE(m0,2) 
  h3 = ROTATE(h0,3) 
  m3 = ROTATE(m0,3)

  numb = 1

  ;############### EROSION ITERATIONS
  WHILE numb GT 0 DO BEGIN

   wim = WHERE(mask,num1)

   wimC = WHERE(CONVOL(mask,h0) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m0) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h1) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m1) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h2) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m2) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h3) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m3) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wim  = WHERE(mask,num2)
   numb = num1 - num2
   ec   = ec + 1

  ENDWHILE 

  ec = ec - 1

RETURN,WHERE(mask)
END
