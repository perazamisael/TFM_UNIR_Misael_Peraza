;+
; NAME:
;         EGSO_SFC_PIXCUMUL
;
; PURPOSE:
;         Assign a value to every pixel of a chain
;         depending on the count of its non-null
;         M-connected neighbors
;
;         Eg: 000  000  010  001  011  101  111
;             010  010  010  010  010  010  111
;             000  100  001  101  101  101  111
;             ->1  ->2  ->3  ->4  ->4  ->5  ->5
;
;         In a 1 pixel thick skeleton, end points will
;         have value 1, node points value 4 or 5, and
;         other points value 3.
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
;         res = egso_sfc_pixcumul(indices,xsize,ysize)
;
; INPUT
;         indices      subscripts of non-null pixels
;         xsize        1st dim of the array
;         ysize        2nd dim of the array
;
; OUTPUTS
;         Output is an array of size (xsize,ysize)
;
; HISTORY:
;
;     NF Mar 2005 last rev.
;-

FUNCTION EGSO_SFC_PIXCUMUL,indices,xsize,ysize

  AA       = indices
  nAA      = N_ELEMENTS(AA)
  mask     = BYTARR(xsize,ysize)
  output   = mask
  mask[AA] = 1b
  pos      = [-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,xsize+1]


  FOR k = 0, nAA-1 DO BEGIN
      N4P = TOTAL(mask[AA[k]+pos[[1,3,4,6]]])
      tot = N4P+1
      IF mask[AA[k]+pos[0]] EQ 1 AND TOTAL(mask[AA[k]+pos[[1,3]]]) EQ 0 THEN $
         tot = tot+1
      IF mask[AA[k]+pos[2]] EQ 1 AND TOTAL(mask[AA[k]+pos[[1,4]]]) EQ 0 THEN $
         tot = tot+1
      IF mask[AA[k]+pos[5]] EQ 1 AND TOTAL(mask[AA[k]+pos[[3,6]]]) EQ 0 THEN $
         tot = tot+1
      IF mask[AA[k]+pos[7]] EQ 1 AND TOTAL(mask[AA[k]+pos[[4,6]]]) EQ 0 THEN $
         tot = tot+1
      output[AA[k]] = tot
  ENDFOR

RETURN,output

END

