;+
; NAME:
;       egso_sfc_limbsym
;
; PURPOSE:
;
;        Symmetry in relation to the limb:
;        copy a ring (limb to limb + delta)
;        inside the solar disk (-> e.g. for prominences)
;        If keyword 'inverse' is set, copy a
;        ring (limb - delta to limb) outside the disk
;        (->e.g. to avoid border effects)
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
;        res = egso_sfc_limbsym(imin,limb,delta[,INVERSE=inverse,XCEN=xcen,YCEN=ycen])
;
; INPUTS
;
;        imin  : input n x m image
;        limb  : radius corresponding to Sun limb (pixels)
;        delta : width of the ring (pixels) to copy
;
; OUTPUTS:
;
;       n x m image with limb symmetry
;
;
; INPUT KEYWORDS:
;
;       INVERSE : replace points in a ring outside the disk with points
;                 of a ring inside the disk
;       XCEN    : X center coordinates in pixels if Sun not centred 
;       YCEN    : Y center coordinates in pixels if Sun not centred
;
;
; MODIFICATION HISTORY:
;
;   NF Feb 2005: Last revision 
;-

;###########################################################################

FUNCTION EGSO_SFC_LIMBSYM,imin,limb,delta,INVERSE=inverse,XCEN=xcen,YCEN=ycen

;###########################################################################


  ;#### Function parameters check
   IF N_PARAMS() NE 3 THEN BEGIN
      PRINT,'EGSO_SFC_LIMBSYM calling example: res=EGSO_SFC_LIMBSYM(image,419,40,/INV)'
      RETALL
   ENDIF


  ;#### Image size
   siz = SIZE(imin)
   Xsiz = siz[1]
   Ysiz = siz[2]


  ;#### Sun centre coordinates
  IF NOT KEYWORD_SET(XCEN) THEN  xc = (Xsiz-1)/2d ELSE xc = xcen
  IF NOT KEYWORD_SET(YCEN) THEN  yc = (Ysiz-1)/2d ELSE yc = ycen


  ;#### Check if parameters are correct
   IF (limb+delta) GT ((xc < (Xsiz-xc)) < (yc < (Ysiz-yc))) OR $
     delta GT limb THEN BEGIN
     PRINT,'EGSO_SFC_LIMBSYM: Check limb and delta values!'
     RETALL
   ENDIF


  ;#### Find the subscripts of points to replace
   lim1 = limb
   lim2 = limb + delta
   lim3 = limb - delta
   IF KEYWORD_SET(inverse) THEN BEGIN 
     mask = EGSO_SFC_ROUNDMASK(Xsiz,Ysiz,lim1,lim2,XCEN=xc,YCEN=yc)
   ENDIF ELSE BEGIN
     mask = EGSO_SFC_ROUNDMASK(Xsiz,Ysiz,lim3,lim1,XCEN=xc,YCEN=yc)
   ENDELSE


  ;##### Apply thales to find the pixels from which get the values
   xi = mask MOD FIX(Xsiz)
   yi = mask / FIX(Xsiz)
   AM = SQRT((xi - xc)^2 + (yi - yc)^2)
   AB = 2d*lim1 - AM
   MN = yi - yc
   AN = xi - xc
   BC = (AB*MN)/AM
   AC = (AB*AN)/AM
   ptx = FIX(AC + xc + 0.5d)
   pty = FIX(BC + yc + 0.5d)


  ;##### replace values
   res = imin
   res[mask] = imin[ptx,pty]


RETURN,res
END


