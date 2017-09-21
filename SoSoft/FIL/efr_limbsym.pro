
;+
; NAME:
;       efr_limbsym
;
; PURPOSE:
;
;        copy a ring (limb to limb + delta)
;        inside the solar disk, symetrically with the limb
;        (-> e.g. for prominences)
;        If keyword 'inverse' is set, copy a
;        ring (limb - delta to limb) outside the disk
;        ,symetrically too (->e.g. for border effects)
;
; AUTHOR:
;
;       Fuller Nicolas
;       LESIA / POLE SOLAIRE
;       Observatoire de MEUDON
;       5 place Jules Janssen
;       92190 MEUDON
;       E-mail: nicolas.fuller@obspm.fr
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Cleaning software
;
; CALLING SEQUENCE:
;
;        res = efr_limbsym(imin,limb,delta[,inverse=inverse,xcen=xcen,ycen=ycen])
;
; INPUTS
;
;        imin  : input n x m image
;        limb  : radius corresponding to Sun limb (pixels)
;        delta : width of the ring (pixels) to copy
;
; OUTPUTS:
;
;       n x m image 
;
;
; INPUT KEYWORDS:
;
;       inv    : replace points in a ring outside the disk with points
;                in a ring inside the disk
;       Xcen   : X center coordinates in pixels if Sun not centred 
;       Ycen   : Y center coordinates in pixels if Sun not centred
;
; CALLS:
;
;       EFR_ROUNDMASK
;
; MODIFICATION HISTORY:
;
;   NF March 2004: add XCEN/YCEN keywords / use EFR_ROUNDMASK
;   NF may 2004: enhance precision (xsiz*1.)
;   NF may 2004: remove square condition
;-
;###########################################################################

FUNCTION EFR_LIMBSYM,imin,limb,delta,INVERSE=inverse,XCEN=xcen,YCEN=ycen

;###########################################################################

 
  ;#### Check the number of parameters
  IF N_PARAMS() LT 3 THEN BEGIN
     PRINT,'EFR_LIMBSYM: Ex. result = EFR_LIMBSYM(image,420,50[,/INVERSE])'    
     RETALL
  ENDIF

  ;#### Image size
  Xsiz = (SIZE(imin))(1)
  Ysiz = (SIZE(imin))(2)


  ;#### Sun centre coordinates
  IF NOT KEYWORD_SET(XCEN) THEN  xc = (Xsiz-1)/2d ELSE xc = xcen
  IF NOT KEYWORD_SET(YCEN) THEN  yc = (Ysiz-1)/2d ELSE yc = ycen


  ;#### Check if parameters are correct
  IF (limb+delta) GT ((xc < (Xsiz-xc)) < (yc < (Ysiz-yc))) OR $
     delta GT limb THEN BEGIN
     PRINT,'EFR_LIMBSYM: Check limb and delta values!'
     RETALL
  ENDIF


  res = imin

  ;#### Find the subscripts of points to replace
  lim1 = double(limb)
  lim2 = double(limb) + double(delta)
  lim3 = double(limb) - double(delta)

  IF KEYWORD_SET(inverse) THEN BEGIN 
    mask = EFR_ROUNDMASK(Xsiz,Ysiz,lim1,lim2,XCEN=xc,YCEN=yc)
  ENDIF ELSE BEGIN
    mask = EFR_ROUNDMASK(Xsiz,Ysiz,lim3,lim1,XCEN=xc,YCEN=yc)
  ENDELSE

  ;##### Apply thales to find the pixels from which get the values
  xi = mask MOD (Xsiz*1.)
  yi = mask / (Xsiz*1.)
  AM = SQRT((xi - xc)^2 + (yi - yc)^2)
  AB = 2d*lim1 - AM
  MN = yi - yc
  AN = xi - xc
  BC = (AB*MN)/AM
  AC = (AB*AN)/AM
  ptx = FIX(AC + xc + 0.5d)
  pty = FIX(BC + yc + 0.5d)
  res[mask] = imin[ptx,pty]

RETURN,res
END


