;+
; NAME:
;         EGSO_SFC_LENGTH_DEG
; PURPOSE:
;         From the pixel chain defined by indices, computes
;         the length in heliographic degrees of the chain. 
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
;
; CALLING SEQUENCE:
;
;         res = egso_sfc_length_deg(indices,nax1,nax2,cd1,cd2,
;                                   cenx,ceny,rsun,dateo[,BND=bnd])
;
; INPUT
;         indices      subscripts of non-null pixels
;         nax1         1st dim of the array (pixels)    ex: 1024
;         nax2         2nd dim of the array (pixels)    ex: 1024
;         cd1          cdelt1 keyword (arcsecs/pixels)  ex: 2.28
;         cd2          cdelt2 keyword (arcsecs/pixels)  ex: 2.28
;         cenx         Sun center X coordinate (pixels) ex: 511.5
;         ceny         Sun center Y coordinate (pixels) ex: 511.5
;         rsun         Sun radius (pixels)              ex: 420
;         dateo        Observation date  ex: 2002-04-01T08:00:00.000
;
; INPUT KEYWORD
;         BND          Set if the chain is a boundary instead of
;                      a skeleton
;
;
; OUTPUTS
;         Output is the length of the chain in heliographic deg.
;
; HISTORY:
;
;    NF mar 2005 Last rev.
;-

FUNCTION EGSO_SFC_LENGTH_DEG,indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,BND=bnd


    nele  = N_ELEMENTS(indices)

   ;#### Get the heliographics coordinates
    hel  = EGSO_SFC_PIX2CARR(indices,nax1,nax2,cd1,cd2,cenx,ceny,$
           rsun,dateo,FIXL0=0.)


   ;#### Lat [270,360]&[0,90] -> [-90,90]
    hel  = (hel + 90.) MOD 360 - 90.
    lat = hel[*,0]
    lon = hel[*,1]

   ;#### Shift coordinates
    lats = SHIFT(lat,-1)
    lons = SHIFT(lon,-1)
   
   ;#### Don't take dist btw first and last point into 
   ;#### account in case of a skeleton
    IF KEYWORD_SET(BND) THEN lp = 1 ELSE lp = 2
    lat  = lat[0:nele-lp]  & lon  = lon[0:nele-lp]
    lats = lats[0:nele-lp] & lons = lons[0:nele-lp]
    
   ;#### Sum the distances btw each successive couple of pixels
    deglnth = TOTAL(SQRT((lat-lats)^2+(lon-lons)^2))

  
RETURN,deglnth
 
END
