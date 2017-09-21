@/home/fuller/IDL/Fit_Ellipse.pro

FUNCTION SUNSPOT,image

;binary image: region 1, bckgd 0
;suppress round regions which could be sunspot

  im_cp = image
  im_labl = LABEL_REGION(im_cp,/ALL_NEIGHBORS)
  hist_labl = HISTOGRAM(im_labl,REVERSE_INDICES=rev)
  nb_reg = N_ELEMENTS(hist_labl)
  good = WHERE(im_cp,cc)

  ;suppress isolated round regions which could be sunspot
 
  FOR ii=0,nb_reg-1 DO BEGIN

    IF hist_labl(ii) LT 120 THEN BEGIN

      region = rev(rev[ii]:rev[ii+1]-1)
      loc = Fit_Ellipse(region,xsize=1024,ysize=1024, $
                        NPOINTS=30,CENTER=center,AXES=axes)
      Xc = center(0)
      Yc = center(1)
       ;critere de distance / aux autres regions
      dist = FLTARR(cc)

      FOR jj=0L,cc-1 DO BEGIN
        Yg = FIX(good(jj)/1024L)
        Xg = good(jj) - Yg*1024L 
        dist(jj) = SQRT((Xg-Xc)^2+(Yg-Yc)^2)        
      ENDFOR

      closepix = WHERE(dist GT (MAX(axes)/2.+1) AND dist LE 10,ccc)

      IF ccc EQ 0 AND MAX(axes)*1./MIN(axes) LE 1.5 THEN BEGIN
         im_cp(region) = 0
      ENDIF

    ENDIF

  ENDFOR

;  AGLO
;  im_cp_2 = im_cp
;  mat = INTARR(10,10)+1
;  dil_im = DILATE(im_cp,mat)
;  dil_labl = LABEL_REGION(dil_im,/ALL_NEIGHBORS)
;  hist_labl = HISTOGRAM(dil_labl,REVERSE_INDICES=rev)       
;  nb_reg = N_ELEMENTS(hist_labl)

;  FOR kk=0,nb_reg-1 DO BEGIN
;      region_kk = rev(rev[kk]:rev[kk+1]-1)
;      im_reg_kk = im_cp*0
;      im_reg_kk(region_kk) = 1
;      good = WHERE(im_cp EQ im_reg_kk)
;      im_cp_2(good) = kk+1
;  ENDFOR

RETURN,im_cp
END


