;////SNF ALGORYTHM (Symmetric Neighborhood Filter)

FUNCTION SNF,image,DELTA=delta

  snfim = image*0b
  xsiz  = (SIZE(image))(1)
  ysiz  = (SIZE(image))(2)
  IF NOT KEYWORD_SET(DELTA) THEN delta = 0

  nozero = WHERE(image,cc)
  AA = FLTARR(4)
  BB = AA
  avbox = AA

;  FOR ii = 1, xsiz-2 DO BEGIN

;      FOR jj = 1, ysiz-2 DO BEGIN

;          AB = image(ii,jj)
;          IF AB GT 0 THEN BEGIN

;            AA(1:3) = image(ii-1:ii+1,jj+1)
;            AA(0)   = image(ii-1,jj)
;            BB(1:3) = REVERSE(image(ii-1:ii+1,jj-1))
;            BB(0)   = image(ii+1,jj)

;            FOR kk = 0,3 DO BEGIN

;              A   = MAX([AA(kk),BB(kk)],MIN=B)
;              moy = (A+B)/2.
;              IF (AB LE (A+delta) AND AB GT moy) THEN avbox(kk) = A  ELSE $
;              IF (AB GE (B-delta) AND AB LT moy) THEN avbox(kk) = B  ELSE avbox(kk) = AB

;            ENDFOR

;            snfim(ii,jj) = (TOTAL(avbox)/4.+AB)/2.

;          ENDIF
 
;      ENDFOR

;  ENDFOR

   FOR ll=0L,cc-1 DO BEGIN

            pix = nozero(ll)
            AB  = image(pix)

            AA(1:3) = image( (pix+xsiz-1):(pix+xsiz+1) )
            AA(0)   = image(pix-1)
            BB(1:3) = REVERSE(image( (pix-xsiz-1):(pix-xsiz+1) ))
            BB(0)   = image(pix+1)

            FOR kk = 0,3 DO BEGIN

              A   = MAX([AA(kk),BB(kk)],MIN=B)
              moy = (A+B)/2.
              IF (AB LE (A+delta) AND AB GT moy) THEN avbox(kk) = A  ELSE $
              IF (AB GE (B-delta) AND AB LT moy) THEN avbox(kk) = B  ELSE avbox(kk) = AB

            ENDFOR

            snfim(pix) = ( TOTAL(avbox)/4. + AB )/2.

   ENDFOR
  
   RETURN,snfim

END

;######################################################

FUNCTION STDDEVCALC,image

  xs  = (SIZE(image))(1)
  ys  = (SIZE(image))(2)
  image2 = image(1:xs-2,1:ys-2)  
  nzero = WHERE(image2,cc)
  tab = FLTARR(cc/4+1) ;take 1pix/4 to go faster
 
  FOR kk=0L,cc-1,4 DO BEGIN
     jj = FIX(nzero(kk)/(xs-2))
     ii = nzero(kk) - jj*(xs-2) 
     ii = ii + 1 & jj = jj +1 
     tab(kk/4) = STDDEV(image(ii-1:ii+1,jj-1:jj+1))
  ENDFOR
 
RETURN,MEDIAN(tab)
END

;#######################################################

FUNCTION APPLY_SNF,image,std

  xsiz = (SIZE(image))(1)
  ysiz = (SIZE(image))(2)
 
  ;/// SNF /////////////////////   
 
    eps = 2.*std

    ;/// 1.DEBLUR //////////////

    PRINT,'1.Deblur'
    TVSCL,image
    PRINT,'pass       1'
    snf_image = SNF(image)
    TVSCL,snf_image
    FOR uu=0,2 DO BEGIN
       PRINT,'pass',uu+2
       snf_image = SNF(snf_image)   
       TVSCL,snf_image
    ENDFOR

    ;/// 2.FLATTEN /////////////

    PRINT,'2.Flatten'

    Tperc = [0,1,2]
    BBB = 0
    snf_image_a = snf_image
    nulpix = WHERE(image EQ 0,nn)

    WHILE [Tperc(2) NE Tperc(1) OR Tperc(1) NE Tperc(0)] DO BEGIN

      snf_image_b = SNF(snf_image_a,DELTA=eps)
      TVSCL,snf_image_b
      diff = WHERE(FIX(snf_image_a) EQ FIX(snf_image_b),cc)
      perc = FIX( (100.*(cc-nn))/(LONG(xsiz)*LONG(ysiz)-nn) )
      Tperc = SHIFT(Tperc,-1)
      Tperc(2) = perc
      PRINT,'Tperc=',Tperc
      snf_image_a = snf_image_b
      BBB = BBB +1
      PRINT,'BBB=',BBB

    ENDWHILE 

    snf_image = snf_image_a

    ;/// 3.SHARPEN /////////////

     PRINT,'3.Sharpen'

       Tperc = [0,1,2]
       BBB = 0
       snf_image_a = snf_image

       WHILE [Tperc(2) NE Tperc(1) OR Tperc(1) NE Tperc(0)] DO BEGIN

         snf_image_b = SNF(snf_image_a)
         TVSCL,snf_image_b
         diff = WHERE(FIX(snf_image_a) EQ FIX(snf_image_b),cc)
         perc = FIX( (100.*(cc-nn))/(LONG(xsiz)*LONG(ysiz)-nn) )
         Tperc = SHIFT(Tperc,-1)
         Tperc(2) = perc
         PRINT,'Tperc=',Tperc
         snf_image_a = snf_image_b
         BBB = BBB +1
         PRINT,'BBB=',BBB

       ENDWHILE 

       snf_image = snf_image_a

RETURN,snf_image
END

;#######################################################

PRO FILAMENT,image

  image = HIST_EQUAL(image,percent=1)
  std = STDDEVCALC(image)
  PRINT,std 
  snf_image = APPLY_SNF(image,std)
  SAVE,snf_image,filename='/home/fuller/poub/snf_image.sav'
;  RESTORE,'/home/fuller/poub/snf_210701.sav'
  res = FILAMENT_GR(snf_image,std)
  window,2,xsize=1024,ysize=1024
  TVSCL,res

END

;######################################################

FUNCTION ADJ,image,orig,mi,ma,xs

;Compute the subscript of pixels within the range
;[mi,ma] around origine (one-adjacency) 

          ;///subscript of points in the one-adjacency

          pm = orig-xs
          pp = orig+xs
          one_adj = [pm-1,pm,pm+1,orig-1,orig+1,pp-1,pp,pp+1]

          ;///find the points with value between min and max 

          goodpix = WHERE(image(one_adj) LE ma AND image(one_adj) GE $
          mi,cc)

          IF cc GT 0 THEN pix_subs = one_adj(goodpix) $
          ELSE pix_subs = -1 

RETURN,pix_subs
END

;############################################################################

FUNCTION GROWING,image,subs0,std
;///region growing function

  coe  = 100.
  xsiz = (SIZE(image))(1)
  yy   = FIX(subs0/xsiz)
  xx   = subs0 - yy*xsiz 
  bsize = 100000

  WHILE bsize GT 500 AND coe GT 50. DO BEGIN

       mini = image(subs0)
       maxi = image(subs0) + coe*(6.25/std)

       blob = SEARCH2D(image,xx,yy,mini,maxi,/DIAGONAL)

       bsize = (SIZE(blob))(1)

       CASE 1 OF
          bsize GE 500000:coe = coe - 10.
          bsize GE 100000:coe = coe - 5.
          bsize GE  10000:coe = coe - 1.
                    ELSE: coe = coe - 0.5
       ENDCASE
       PRINT,coe,(SIZE(blob))(1)

  ENDWHILE

RETURN,blob
END

;  cp_im  = image      ;///image copy
;  xsiz   = (SIZE(image))(1)
;  subs   = subs0      ;///initial subscript
;  blob   = subs0      ;///initial region
;  count  = 1          ;///size of initial subscript
;  cp_im(subs0) = 0    ;///each subscript found: put 0 

;///while the algorithm still finds points, do ...
   
;WHILE count NE 0 DO BEGIN

;  subt = 0
;  ii   = 0L
  
  ;///loop on each point found
  
;  WHILE ii LT count DO BEGIN 

     ;///call the adjacency function

;       mini = image(subs(ii)) - coe*std
;       maxi = image(subs(ii)) + coe*std

;     subsi = ADJ(cp_im,subs(ii),mini,maxi,xsiz)

;     IF subsi(0) EQ -1 THEN BEGIN
;        subt = subt
;     ENDIF ELSE BEGIN 
;        cp_im(subsi) = 0
;        subt = [subt,subsi]
;     ENDELSE
;     ii=ii+1

;  ENDWHILE

;  nsubt = N_ELEMENTS(subt)

;  IF nsubt GT 1 THEN BEGIN
;     subt  = subt(1:nsubt-1) ;///supress the first zero
;     blob  = [blob,subt]     ;///add the new points to the blob
;     count = (SIZE(subt))(1)
;     subs  = subt
;  ENDIF ELSE count = 0


;ENDWHILE

;RETURN,blob
;END


;############################################################################

FUNCTION filament_gr,image,std

;///region growing applied to filaments

  xsiz = (SIZE(image))(1)  ;///assume square image
  diam = 840.               ;///disk diameter

;///result image////////////////////////////////
  
  im_seed = image*0.
  ;or; im_seed = HIST_EQUAL(im,percent=1)

;///create a mask...////////////////////////////

  im_mask     = image*0.+1.
  subs_mask   = WHERE(im_mask)
  subs_mask_y = FIX(subs_mask/xsiz)
  subs_mask_x = subs_mask - subs_mask_y*xsiz
  dist_mask   = SQRT((subs_mask_x-xsiz/2)^2 + (subs_mask_y-xsiz/2)^2) 

  ;///to eliminate the seeds too far from disk center

  mask = WHERE(dist_mask GE (diam/2)*0.98)

  ;///to eliminate the few pixels of the limb to avoid false detection

  mask2 = WHERE(dist_mask GE (diam/2)*0.98)
  im_cp = image + 1.;pour hist_equal
  im_cp(mask2) = 0.
  base = WHERE(im_cp EQ 1.)

;///Define a treshold with a maximum amount of seeds

;   nb_seeds = 100
;   tresh = MEDIAN(im_cp(WHERE(im_cp)))

tresh = 1.1 ;std

;  WHILE nb_seeds GE 100 DO BEGIN

    ;///make a first segmentation before labelling////

    im_seg = image
    im_seg(WHERE(im_cp LT tresh AND im_cp NE 0.,COMPLEMENT=bigv,cc)) = 1.
    im_seg(bigv) = 0.
    im_seg(mask) = 0.

    ;///first labelling///////////////////////////////
 
    im_labl = LABEL_REGION(im_seg)
    hist_labl = HISTOGRAM(im_labl,REVERSE_INDICES=rev)

    nb_seeds = N_ELEMENTS(hist_labl)
    PRINT,'nbre de seeds',nb_seeds

;    tresh = tresh/1.01
;    print,tresh

;  ENDWHILE

;///loop on each seed/////////////////////////////////


FOR ii=0, N_ELEMENTS(hist_labl)-1 DO BEGIN

   IF hist_labl(ii) GT 1 AND hist_labl(ii) LT 100000 THEN BEGIN

     region_subs = rev(rev[ii]:rev[ii+1]-1)  ;///subscripts of labelled region 
     region_val  = im_cp(region_subs)        ;///values of labelled region
     minpoints   = WHERE(region_val EQ MIN(region_val))
     seed = region_subs(minpoints(0))        ;///seed point subscript
     
     IF im_seed(seed) NE 1 THEN BEGIN

       ;///call the growing region function//////////// 

       region = GROWING(im_cp,seed,std)
       PRINT,'region points:',N_ELEMENTS(region)

       ;///eliminate the region if number of points to small
       IF N_ELEMENTS(region) GT 5 THEN im_seed(region) = 1

     ENDIF

   ENDIF
tvscl,im_seed
ENDFOR

 im_seed(base)=1

RETURN,im_seed

END




