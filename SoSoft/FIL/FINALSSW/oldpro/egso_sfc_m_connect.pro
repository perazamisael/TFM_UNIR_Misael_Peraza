;+
; NAME:
;         EGSO_SFC_M_CONNECT
;
; PURPOSE:
;         Delete pixels of a one pixel thick chain
;         (from indices) to make the chain m_connected
;         ex:
;
;         x              x
;          xxx   x        xx0   x
;            xx x    -->    x0 x 
;             xx             xx
;         --------------------------
;              xx             xx
;             x              x
;          xxxx      -->  xxx0
;             x              x
;              xx             xx            
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
;         res = egso_sfc_m_connect(indices,xsize,ysize[,DISP=disp])
;
; INPUT
;         indices      subscripts of non-null pixels
;         xsize        1st dim of the array
;         ysize        2nd dim of the array
;
; KEYWORD
;         DISP       display      
;
; OUTPUTS
;         Output is the m_connected subscripts array
;
; HISTORY:
;
;     NF mar 2005 last rev.
;-

FUNCTION EGSO_SFC_M_CONNECT,indices,xsize,ysize,DISP=disp

  mask = BYTARR(xsize,ysize)
  mask[indices] = 1b

  ;####################### THINNING STRUCTURES
  h0 = [[-1,1,0],[1,1,-1],[0,-1,-1]]   
  h1 = ROTATE(h0,1)
  h2 = ROTATE(h0,2)
  h3 = ROTATE(h0,3)

  m0 = [[0,1,0],[1,1,1],[-1,-1,-1]]   
  m1 = ROTATE(m0,1)
  m2 = ROTATE(m0,2)
  m3 = ROTATE(m0,3)

  numb = 1

  ;############### THINNING ITERATIONS
  WHILE numb GT 0 DO BEGIN

   wim = WHERE(mask,num1)

   wimC = WHERE(CONVOL(mask,h0) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h1) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h2) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h3) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m0) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m1) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m2) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m3) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wim = WHERE(mask,num2)
   numb = num1 - num2

   IF KEYWORD_SET(disp) THEN tvscl,mask 

  ENDWHILE 

RETURN,WHERE(mask)

END

