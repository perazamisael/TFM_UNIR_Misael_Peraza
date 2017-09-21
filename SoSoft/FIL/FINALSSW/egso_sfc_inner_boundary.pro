;+
; NAME:
;         EGSO_SFC_INNER_BOUNDARY
;
; PURPOSE:
;         Find the inner boundary of the object defined by
;         indices using erosion with a structure wich is
;         either:
;
;         010      111
;         111  or  111
;         010      111
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
;         res = egso_sfc_inner_boundary(indices,xsize,ysize
;               [,ALLDIR=alldir])   
;
; INPUT
;         indices      subscripts of non-null pixels
;         xsize        1st dim of the array
;         ysize        2nd dim of the array
;
; KEYWORDS
;         ALLDIR         use 8 adjacency structure
;
; OUTPUTS
;         Output is the boundary subscripts array
;
; HISTORY:
;
;         NF last rev. Mar 2005
;-

FUNCTION EGSO_SFC_INNER_BOUNDARY,indices,xsize,ysize,ALLDIR=alldir

    mask          = BYTARR(xsize,ysize)
    mask[indices] = 1b
    
    IF NOT KEYWORD_SET(ALLDIR) THEN BEGIN

        erod = ERODE(mask,[[0,1,0],[1,1,1],[0,1,0]]) ;(4connect)

    ENDIF ELSE BEGIN

        erod = ERODE(mask,REPLICATE(1,3,3)) ;(8connect)

    ENDELSE

    wero = WHERE(erod,nero)
    IF nero NE 0 THEN mask[wero] = 0b
    bound = WHERE(mask)

RETURN,bound
END
