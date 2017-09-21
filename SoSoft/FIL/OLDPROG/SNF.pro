;////SNF ALGORYTHM (Symmetric Neighborhood Filter)

FUNCTION SNF,image,DELTA=delta

  snfim = image*0.
  xsiz  = (SIZE(image))(1)
  ysiz  = (SIZE(image))(2)
  IF KEYWORD_SET(DELTA) THEN delta = delta ELSE delta = 0

  FOR ii = 1, xsiz-2 DO BEGIN

      FOR jj = 1, ysiz-2 DO BEGIN

          AB = image(ii,jj)
          IF AB EQ 0 THEN GOTO,zap
          AA = FLTARR(4)
          BB = FLTARR(4) 
          avbox = FLTARR(4)

          AA(0) = image(ii+1,jj)   & BB(0) = image(ii-1,jj) 
          AA(1) = image(ii+1,jj+1) & BB(1) = image(ii-1,jj-1) 
          AA(2) = image(ii,jj+1)   & BB(2) = image(ii,jj-1)
          AA(3) = image(ii-1,jj+1) & BB(3) = image(ii+1,jj-1)

          FOR kk = 0,3 DO BEGIN

            ;IF AA(kk) LT BB(kk) THEN BEGIN
            ;   CC0000 = BB(kk)
            ;   BB(kk) = AA(kk)
            ;   AA(KK) = CC0000
            ;ENDIF
            
            A  = MAX([AA(kk),BB(kk)])
            B  = MIN([AA(kk),BB(kk)])
            moy = (A+B)/2.
            min = (B-delta)
            max = (A+delta)
            IF AB LE max AND AB GT moy THEN avbox(kk) = A
            IF AB GE min AND AB LT moy THEN avbox(kk) = B
            IF AB GT max OR AB LT min THEN avbox(kk)  = AB
            IF AB EQ moy THEN avbox(kk) = AB

          ENDFOR
          newval = TOTAL(avbox)/4.
          snfim(ii,jj) = (newval+AB)/2.
          zap:
 
      ENDFOR
  ENDFOR
  
 RETURN,snfim

END

