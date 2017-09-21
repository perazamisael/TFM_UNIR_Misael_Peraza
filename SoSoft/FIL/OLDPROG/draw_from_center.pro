
FUNCTION DRAW_FROM_CENTER,input

  diam = 840

  Xsiz = (SIZE(input))(1)
  Ysiz = (SIZE(input))(2)

  WINDOW,0,XSIZE=Xsiz,YSIZE=Ysiz
  TVSCL,input

  res = input*0b
  restemp = input*0b
  X0 = diam/2
  Y0 = X0
  base   = WHERE(input*0+1)
  base_Y = base /   Xsiz
  base_X = base MOD Xsiz
  base_DIST = SQRT((base_X-Xsiz/2)^2 + (base_Y-Ysiz/2)^2)

  FOR ii = 2,diam/2-1 DO BEGIN
    ray1 = ii
    circ1 = WHERE(base_DIST GT ray1-0.5 AND base_DIST LE ray1+0.5)
    data1 = input(circ1)
    goodpix = WHERE(data1 EQ 1b,nn)
    ;print,nn
    IF nn GT 0 THEN BEGIN
      FOR jj=0,nn-1 DO BEGIN
         nxy = circ1(goodpix(jj))
         Yn = FLOAT(nxy / Xsiz   - 1024/2)
         Xn = FLOAT(nxy MOD Xsiz - 1024/2)
         angle = ATAN(Yn,Xn)*180./!pi+180.

         IF angle GE  337.5 AND angle LT  360.0 THEN dd = [input(nxy+1)];,input(nxy+1024),input(nxy-1024),input(nxy-1)]
         IF angle GE    0.0 AND angle LT   22.5 THEN dd = [input(nxy+1)];,input(nxy+1024),input(nxy-1024),input(nxy-1)]
         IF angle GE  157.5 AND angle LT  202.5 THEN dd = [input(nxy-1)];,input(nxy+1024),input(nxy-1024),input(nxy+1)]
         IF angle GE  112.5 AND angle LT  157.5 THEN dd = [input(nxy+1024-1)];,input(nxy+1024),input(nxy-1),input(nxy-1024+1)]
         IF angle GE   67.5 AND angle LT  112.5 THEN dd = [input(nxy+1024)];,input(nxy-1),input(nxy+1),input(nxy-1024)]
         IF angle GE   22.5 AND angle LT   67.5 THEN dd = [input(nxy+1024+1)];,input(nxy+1024),input(nxy+1),input(nxy-1024-1)]
         IF angle GE  202.5 AND angle LT  247.5 THEN dd = [input(nxy-1024-1)];,input(nxy-1),input(nxy-1024),input(nxy+1024+1)]
         IF angle GE  247.5 AND angle LT  292.5 THEN dd = [input(nxy-1024)];,input(nxy-1),input(nxy+1),input(nxy+1024)]
         IF angle GE  292.5 AND angle LT  337.5 THEN dd = [input(nxy-1024+1)];,input(nxy+1),input(nxy-1024),input(nxy+1024-1)]
         ;IF TOTAL(dd(0:2)) NE 3 AND dd(3) EQ 1b THEN res(nxy)=1b
          IF dd(0) EQ 0b THEN res(nxy)=1b
      ENDFOR
    ENDIF
    tvscl,res

  ENDFOR

  
RETURN,res

end_filament: 
END


;####################################################








