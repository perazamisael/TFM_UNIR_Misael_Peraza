FUNCTION TWO_SIDES,input

  diam = 840

  Xsiz = (SIZE(input))(1)
  Ysiz = (SIZE(input))(2)

  ;contours
  ;bound = input*0b
  ;nozero = WHERE(input NE 0,nn)
  ;FOR ii=0,nn-1 DO BEGIN
  ;    num = nozero(ii)
  ;    neighb = [num-1,num+1,num+Xsiz,num-Xsiz]
  ;;      neighb = [num-1,num+1,num+Xsiz,num-Xsiz,num-1-Xsiz,num+1-Xsiz,num-1+Xsiz,num+1+Xsiz]
  ;    IF TOTAL(input(neighb)) NE 4 THEN bound(num)=1b
  ;ENDFOR
  
  WINDOW,0,XSIZE=Xsiz,YSIZE=Ysiz
  TVSCL,input

  ;WINDOW,1,XSIZE=Xsiz,YSIZE=Ysiz
  ;TVSCL,bound

  res = input*0b
  
  b = LABEL_REGION(input)
  h = HISTOGRAM(b, REVERSE_INDICES=r)

  FOR i=1, N_ELEMENTS(h)-1 DO BEGIN

     p = r(r[i]:r[i+1]-1)

     npoints = 120
     ell = Fit_Ellipse(p,XSIZE=1024,YSIZE=1024,CENTER=center,ORIENTATION=beta,AXES=axes,NPOINTS=npoints)
     IF beta LT -90. THEN beta = beta + 180.
     IF beta LT 0.   THEN beta = beta + 180.

     xc = center(0) - 1024/2
     yc = center(1) - 1024/2
     alpha = ATAN(yc,xc)*!radeg 
     alpha = alpha + 180
     IF alpha LT 0 THEN alpha = alpha + 360
     IF alpha GT beta AND alpha LT (beta+180) THEN delta = (beta + 270) MOD 360  $
     ELSE delta = beta + 90

     temp = input*0b
     restemp = input*0b
     restemp2 = input*0b
     pprime = p - LONG(center(0)-512) -1024l*LONG(center(1)-512)
     temp(pprime) = 1b
     
     ;XYOUTS,center(0),center(1),STRING(alpha)+STRING(beta)+STRING(delta),/device,color=90
     
     rotated = ROT(temp,delta,1.0)
     rotemp = WHERE(rotated eq 1b)
     X_rotemp = rotemp MOD 1024l
     Y_rotemp = FIX((rotemp - X_rotemp)/1024l)
     ymaxrot = MAX(Y_rotemp,MIN=yminrot)

     ;contour region
     ;boundr = rotated*0b
     ;nozero = WHERE(rotated NE 0b,nn)
     ;FOR ll=0,nn-1 DO BEGIN
     ;  num = nozero(ll)
     ;  neighb = [num-1,num+1,num+Xsiz,num-Xsiz]
     ;  IF TOTAL(rotated(neighb)) NE 4 THEN boundr(num)=1b
     ;ENDFOR
      

     ;1ere methode:proche du resultat d'un thinning
     wyy0 = WHERE(Y_rotemp EQ yminrot)
     wxx0 = X_rotemp(wyy0)
     xx_prev = wxx0(FIX(N_ELEMENTS(wxx0)/2.))
     restemp(xx_prev,yminrot)=1b
     FOR jj=1,(ymaxrot-yminrot) DO BEGIN
         yy = yminrot + jj
         wyy = WHERE(Y_rotemp EQ yy)
         wxx = X_rotemp(wyy)
         IF MEAN(wxx) LE xx_prev-1 THEN xx = xx_prev-1
         IF MEAN(wxx) GE xx_prev+1 THEN xx = xx_prev+1
         IF MEAN(wxx) LT xx_prev+1 AND MEAN(wxx) GT xx_prev-1 THEN xx = xx_prev
         xx_prev = xx
         restemp(xx,yy)=1b
         ;restemp(xx-1:xx+1,yy)=1b
         ;;pour obtenir le bord
         ;wxxb = WHERE(boundr(*,yy) EQ 1b)
         ;wxxb = wxxb MOD 1024
         ;toto = WHERE(wxxb LE xx)
         ;IF toto(0) NE -1 THEN restemp2(wxxb(toto),yy)=1b
     ENDFOR
     

     invrot = ROT(restemp,-delta,1.0)
     p2 = WHERE(invrot EQ 1b,nb)
     p2prime = p2 + LONG(center(0)-512) +1024l*LONG(center(1)-512)
     ;res(p2prime) = 1b

     ;on prend 1 point sur 5
     wfil2 = LONARR(nb/5+1)
     wfil2(0) = p2prime(0) 
     FOR pp=5,nb-1,5 DO BEGIN
         wfil2(pp/5)=p2prime(pp)
     ENDFOR
     wfil2(nb/5)=p2prime(nb-1)
     res(wfil2)=1b

  ENDFOR

TVSCL,res
RETURN,res

END
     ;    IF delta GE  337.5 AND delta LT  360.0 THEN ind = 0
     ;    IF delta GE    0.0 AND delta LT   22.5 THEN ind = 0
     ;    IF delta GE   22.5 AND delta LT   67.5 THEN ind = 1
     ;    IF delta GE   67.5 AND delta LT  112.5 THEN ind = 2
     ;    IF delta GE  112.5 AND delta LT  157.5 THEN ind = 3
     ;    IF delta GE  157.5 AND delta LT  202.5 THEN ind = 4
     ;    IF delta GE  202.5 AND delta LT  247.5 THEN ind = 5
     ;    IF delta GE  247.5 AND delta LT  292.5 THEN ind = 6
     ;    IF delta GE  292.5 AND delta LT  337.5 THEN ind = 7