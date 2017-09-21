FUNCTION EFR_ROT_NNUL,subs,angle,xs,ys,xcen=xcen,ycen=ycen
;a priori plus lent que rot() donc sans reel interet

  IF NOT KEYWORD_SET(xcen) THEN xcen = (xs-1)/2.
  IF NOT KEYWORD_SET(ycen) THEN ycen = (ys-1)/2.

  subsx = subs MOD xs
  subsy = subs / ys

  distab = SQRT((subsx - xcen)^2 + (subsy - ycen)^2)
  angtab = (!radeg*ATAN(subsy-ycen,subsx-xcen) + 360) MOD 360

  angtab = (angtab + angle) MOD 360

  subsx = LONG(distab*COS(angtab/!radeg) + xcen + 1)
  subsy = LONG(distab*SIN(angtab/!radeg) + ycen + 1)
  
  nogood = WHERE((subsx LT 0 OR subsx GT xs-1) OR (subsy LT 0 OR subsy GT ys-1),COMP=comp) 

  IF comp[0] NE -1 THEN BEGIN
   subsx = subsx[comp]
   subsy = subsy[comp]
   newsubs = subsx + xs*subsy
  ENDIF ELSE RETURN,-1 


RETURN,newsubs

END
