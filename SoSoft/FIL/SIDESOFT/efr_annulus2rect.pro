FUNCTION EFR_ANNULUS2RECT,im,limb,delta
;NFuller 2004 /EGSO project

  imsize   = SIZE(im)
  xs       = imsize[1]
  ys       = imsize[2]
  xcen     = (xs-1)/2d
  ycen     = (ys-1)/2d
  rext     = limb + delta
  xsrect   = FIX(2d*!pi*rext)
  ysrect   = delta
  rect     = FLTARR(xsrect,ysrect)
  quar     = FIX(xsrect/4.+0.5)

  tangle  = (DINDGEN(quar)+1)/rext
  ctangle = COS(tangle)
  stangle = SIN(tangle)
  deltab  = INDGEN(ysrect)


  FOR ii = 0, quar-1 DO BEGIN
        xi1 = FIX(xcen+(deltab+limb)*ctangle[ii]+0.5)
        yi1 = FIX(ycen+(deltab+limb)*stangle[ii]+0.5)
        xi2 = FIX(xcen-(deltab+limb)*ctangle[ii]+0.5)
        yi2 = FIX(ycen-(deltab+limb)*stangle[ii]+0.5)       
        rect[xsrect-1-ii,deltab] = im[xi1,yi1]
        rect[3*quar-1-ii,deltab] = im[yi2,xi1]
        rect[2*quar-1-ii,deltab] = im[xi2,yi2]
        rect[1*quar-1-ii,deltab] = im[yi1,xi2]
    ENDFOR

;avant optimisation
;  FOR ii = 1, quar DO BEGIN
;    FOR jj = 0, ysrect-1 DO BEGIN
;        angle  = (ii*1d/(rext))
;        xi1    = FIX(xcen+(jj+limb)*COS(angle)+0.5)
;        yi1    = FIX(ycen+(jj+limb)*SIN(angle)+0.5)
;        xi2    = FIX(xcen-(jj+limb)*COS(angle)+0.5)
;        yi2    = FIX(ycen-(jj+limb)*SIN(angle)+0.5)       
;        rect[xsrect-ii,jj]=im[xi1,yi1]
;        rect[3*quar-ii,jj]=im[yi2,xi1]
;        rect[2*quar-ii,jj]=im[xi2,yi2]
;        rect[1*quar-ii,jj]=im[yi1,xi2]
;    ENDFOR
;  ENDFOR   


RETURN,rect
END  


