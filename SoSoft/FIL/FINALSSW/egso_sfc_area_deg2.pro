;+
; NAME:
;         EGSO_SFC_AREA_DEG2
;
; PURPOSE:
;         From the boundary pixels defined by indices, computes
;         the area in heliographic square degrees:indices must 
;         draw a unique filled shape! 
;         (cf EGSO_SFC_OUTER_BOUNDARY or EGSO_SFC_INNER_BOUNDARY)
;         area = ABS(SUM[(LATi - LATi-1)(LONi + LONi-1)/2])
;         (trapezoid approximation)
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
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Solar Feature Catalog
;
; CALLING SEQUENCE:
;
;         res = egso_sfc_area_deg2(indices,nax1,nax2,cd1,cd2,
;                                        cenx,ceny,rsun,dateo)
;
; INPUT
;         indices      subscripts of boundary pixels
;         nax1         1st dim of the array (pixels)    ex: 1024
;         nax2         2nd dim of the array (pixels)    ex: 1024
;         cd1          cdelt1 keyword (arcsecs/pixels)  ex: 2.28
;         cd2          cdelt2 keyword (arcsecs/pixels)  ex: 2.28
;         cenx         Sun center X coordinate (pixels) ex: 511.5
;         ceny         Sun center Y coordinate (pixels) ex: 511.5
;         rsun         Sun radius (pixels)              ex: 420
;         dateo        Observation date  ex: 2002-04-01T08:00:00.000
;
; OUTPUTS
;         Output is the area in heliographic square deg.
;
; HISTORY:
;
;         NF Last revision Mar 2005
;-

FUNCTION EGSO_SFC_AREA_DEG2,indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo


   nele    = N_ELEMENTS(indices)
   degarea = 0.
 
  ;#### Get the heliographics coordinates
   coord = EGSO_SFC_PIX2CARR(indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)

  ;#### Lat [270,360] & [0,90] -> [-90,90]
   coord = (coord + 90.) MOD 360. - 90.
   lati = coord[*,0] 
   loni = coord[*,1]  

  ;#### Shift coordinates to draw the trapezoids  
   latis = SHIFT(lati,-1)
   lonis = SHIFT(loni,-1)

  ;#### Sum the trapezoid areas
   degarea = ABS(TOTAL((loni+lonis)*(lati-latis)))/2.


RETURN,degarea
END
