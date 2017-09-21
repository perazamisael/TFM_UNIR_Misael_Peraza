;+
; NAME:
;         EGSO_SFC_CURL_IND
; PURPOSE:
;         Gives an index of how much the skeleton
;         is curled up based on the ratio between its length and
;         the distance between end points
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
;         res = egso_sfc_curl_ind(indices,xsize,ysize)
;
; INPUT
;         indices      subscripts of non-null skeleton pixels
;         xsize        1st dim of the array
;         ysize        2nd dim of the array
;
; OUTPUTS
;         Output is the curl index
;
; HISTORY:
;
;       NF last rev. Mar 2005
;       Should take into account features are on a sphere
;-

FUNCTION EGSO_SFC_CURL_IND,indices,xsize,ysize

   nele = N_ELEMENTS(indices)
   xsize = FIX(xsize)
   ysize = FIX(ysize)

  ;#### Distance between extrema
   x1   = indices[nele-1] MOD xsize & y1 = indices[nele-1]/xsize
   x0   = indices[0]      MOD xsize & y0 = indices[0]/xsize
   dist_extr = SQRT( (x1-x0)^2 + (y1-y0)^2 )

  ;#### Length
   X0 = indices[0:nele-2] MOD xsize & Y0 = indices[0:nele-2]/xsize
   X1 = indices[1:nele-1] MOD xsize & Y1 = indices[1:nele-1]/xsize
   dist = TOTAL(SQRT( (X1-X0)^2 + (Y1-Y0)^2 ))

  ;#### Index
   curlind =  10.* (1.- dist_extr/dist) 


RETURN,curlind

END
