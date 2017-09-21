FUNCTION TWO_SIDES,input

  mid  = 512 ;511.5
  res  = input 
  Xsiz = (SIZE(input))[1]
  Ysiz = (SIZE(input))[2]

  ;WINDOW,0,XSIZE=Xsiz,YSIZE=Ysiz
  ;TVSCL,input
  
  ;To connect close blobs
  input = MORPH_CLOSE(input,REPLICATE(1,10,10))

  b = LABEL_REGION(input)
  h = HISTOGRAM(b, REVERSE_INDICES=r)
  num_REG = N_ELEMENTS(h)
  final_tab = INTARR(1,3)-1
  cmpt=0

  FOR i=1, num_REG-1 DO BEGIN

     p = r(r[i]:r[i+1]-1)

     npo = 120
     ell = Fit_Ellipse(p,XSIZE=1024,YSIZE=1024,CENTER=center, $
           ORIENTATION=beta,AXES=axes,NPOINTS=npo)

     IF MAX(axes) LT 15 THEN GOTO,endf
     IF MAX(axes) LT 30 AND ABS(FLOAT(MAX(axes))/MIN(axes)-1.) LT 0.7 THEN GOTO,endf

     beta = (beta + 180) MOD 180

     xc = center[0] - mid
     yc = center[1] - mid

     ;angle entre le centre et l'objet
     alpha = (ATAN(yc,xc)+!pi)*!radeg 

     IF alpha GT beta AND alpha LE (beta+180) THEN $ 
     delta = (270 + beta) MOD 360 ELSE delta = (90 + beta) MOD 360

     temp = input*0b
     restemp = input*0b
     restemp2 = input*0b

     ;objet au centre
     pprime = p - LONG(center[0]-mid) -1024l*LONG(center[1]-mid)
     temp[pprime] = 1b
     
     ;rotation de l'objet
     rotated = ROT(temp,delta,1.0)
     rotemp = WHERE(rotated eq 1b)
     X_rotemp = rotemp MOD 1024l
     Y_rotemp = rotemp/1024
     ;extremites de l'objet
     ymaxrot = MAX(Y_rotemp,MIN=yminrot)

     ;1ere methode:proche du resultat d'un thinning
     tab = INTARR(ymaxrot-yminrot+1,3)
     wyy0 = WHERE(Y_rotemp EQ yminrot)
     wxx0 = X_rotemp[wyy0]
     xx_prev = wxx0[N_ELEMENTS(wxx0)/2]
     tab[0,0:1] = [xx_prev,yminrot]  
     FOR jj=1,(ymaxrot-yminrot) DO BEGIN
         yy = yminrot + jj
         wyy = WHERE(Y_rotemp EQ yy)
         wxx = X_rotemp[wyy]
         ;check if the x coordinate is continuous
         IF N_ELEMENTS(wxx) GE 2 THEN BEGIN
         diff_wxx = INTARR(N_ELEMENTS(wxx)-1)
         FOR yy=0,N_ELEMENTS(wxx)-2 DO diff_wxx[yy]=wxx[yy+1]-wxx[yy]-1
         IF TOTAL(diff_wxx) NE 0 THEN BEGIN
            print,wxx
            print,diff_wxx
            print,'*****************'
         ENDIF
         ENDIF
         ;IF MEAN(wxx) LE xx_prev-1 THEN xx = xx_prev-1
         ;IF MEAN(wxx) GE xx_prev+1 THEN xx = xx_prev+1
         ;IF MEAN(wxx) LT xx_prev+1 AND MEAN(wxx) GT xx_prev-1 THEN xx = xx_prev
         ;xx_prev = xx
         xx=wxx[0]
         tab[jj,0:1]=[xx,yy]
     ENDFOR

      toto=tab[*,0]
      titi=SMOOTH(toto,3)
      tab[*,0]=titi

     ;revenir au coordonnees initiales
     npoints = (SIZE(tab))[1]
     FOR uu=0,npoints-1 DO BEGIN
         coox = tab[uu,0]-mid
         cooy = tab[uu,1]-mid
         alpha= ATAN(cooy,coox)*!radeg
         beta = (alpha + delta)/!radeg
         coox2 = FIX(SQRT(coox^2+cooy^2)*COS(beta)+0.5)
         cooy2 = FIX(SQRT(coox^2+cooy^2)*SIN(beta)+0.5)
         tab[uu,0:1]=[coox2+center[0],cooy2+center[1]]
         tab[uu,2]  = cmpt 
         res[coox2+center[0],cooy2+center[1]]=0b
     ENDFOR

     final_tab=[final_tab,tab]
     cmpt = cmpt+1

  endf:
  ENDFOR

;save,res,filename='/home/fuller/poub/SAV/lineim5b.sav'
;tvscl,res
;wait,2
RETURN,TRANSPOSE(final_tab)

END

