;+
; NAME:
;         EGSO_SFC_OUTER_BOUNDARY
;
; PURPOSE:
;         Find the external boundary of the object defined by
;         indices using subscripts shifting in 4 or 8 directions
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
;         res = egso_sfc_ext_boundary(indices,xsize,ysize[,ALLDIR=alldir])
;
; INPUT
;         indices      subscripts of non-null pixels
;         xsize        1st dim of the array
;         ysize        2nd dim of the array
;
; KEYWORD
;         ALLDIR         Shift in 8 directions instead of 4
;        
; OUTPUTS
;         Output is the boundary subscripts array
;
; HISTORY:
;
;    NF Mar 2005 last rev. 
;-

FUNCTION EGSO_SFC_OUTER_BOUNDARY,indices,xsize,ysize,ALLDIR=alldir
 
   ;#### Work within a larger array to avoid border problems
    xsiz = xsize + 2
    ysiz = ysize + 2
    indx = indices MOD xsize + 1 
    indy = indices / xsize + 1
    indi = indx + xsiz*indy  
    mask = BYTARR(xsiz,ysiz)


   ;#### Define the shift directions
    set4 = [-xsiz,xsiz,-1,1]
    set8 = [-xsiz-1,-xsiz,-xsiz+1,-1,1,xsiz-1,xsiz,xsiz+1]

    IF NOT KEYWORD_SET(ALLDIR) THEN BEGIN

      ;#### Shift in 4 directions
       FOR oo=0,3 DO mask[indi+set4[oo]] = 1b

    ENDIF ELSE BEGIN

      ;#### Shift in 8 directions
       FOR oo=0,7 DO mask[indi+set8[oo]] = 1b

    ENDELSE

   ;#### Set the inner pixels to 0
    mask[indi] = 0b
 
   ;#### Return to original size
    mask = mask[1:xsiz-2,1:ysiz-2]
    
   ;#### Get the boundary subscripts
    bound = WHERE(mask)

RETURN,bound
END



