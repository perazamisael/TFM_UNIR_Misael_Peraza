;+
; NAME:
;         EGSO_SFC_LENGTH_PIX
;
; PURPOSE:
;         From the pixel chain defined by indices, computes
;         the length in pixels of the chain. The following
;         rules are used:
;         - distance btw 2 aligned  pixels = 1
;         - distance btw 2 diagonal pixels = SQRT(2)
;         The chain can be a boundary or a pruned skeleton
;         Subscripts MUST be ordered (cf EGSO_SFC_ORDER_IND)
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
;        res = egso_sfc_length_pix(indices,xsize,ysize[,SMP=smp])
;
;
; INPUT
;         indices      subscripts of non-null pixels
;         xsize        1st dim of the array
;         ysize        2nd dim of the array
;
; OUTPUT
;
;         Output is the length of the chain in pixels
;
; OUTPUT KEYWORD 
;
;         SMP  is the subscript of the chain middle point
;
; HISTORY:
;
;    NF mar 2005 Last rev.
;-

FUNCTION EGSO_SFC_LENGTH_PIX,indices,xsize,ysize,SMP=smp

   nele  = N_ELEMENTS(indices)

   ;#### We compute the coordinates difference corresponding
   ;#### to the original subscripts and the shifted subscripts, ie
   ;#### btw 1 pixel and its neigbor

   tab1  = SHIFT(indices,-1)
   tab2  = indices
    
   difX  = tab1 MOD xsize - tab2 MOD xsize
   difY  = tab1 / xsize   - tab2 / xsize

   diag  = WHERE(ABS(difX) + ABS(difY) EQ 2,ndiag)
   line  = WHERE(ABS(difX) + ABS(difY) EQ 1,nline)

   length = ndiag*SQRT(2d) + nline + (nele-ndiag-nline < 1)


   ;#### Chain middle point calculation (if not a boundary)

   IF (nele-ndiag-nline) EQ 1 THEN BEGIN

      smp = 0
      lt2 = 0
      ;#### We compute length until it reaches
      ;#### total length / 2 
      WHILE lt2 LT length/2.0 DO BEGIN
        diffX = difX[0:smp]
        diffY = difY[0:smp]
        diag  = WHERE(ABS(diffX) + ABS(diffY) EQ 2,ndiag)
        line  = WHERE(ABS(diffX) + ABS(diffY) EQ 1,nline)          
        lt2   = 0.5 + ndiag*SQRT(2d) + nline   
        smp   = smp + 1
      ENDWHILE

      smp = indices[smp]

   ENDIF ELSE smp = -1


RETURN,length


END
