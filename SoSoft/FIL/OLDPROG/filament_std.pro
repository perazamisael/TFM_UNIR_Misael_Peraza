
@/home/fuller/IDL/Fit_Ellipse.pro

FUNCTION STDDEVCALC,image

  xs     = (SIZE(image))(1)
  ys     = (SIZE(image))(2)
  image2 = image(1:xs-2,1:ys-2)  
  nzero  = WHERE(image2,cc)
  tab    = FLTARR(cc/4+1)   ;take 1pix/2 to go faster
 
  FOR kk=0L,cc-1,4 DO BEGIN
     jj = nzero(kk) /   (xs-2)
     ii = nzero(kk) MOD (xs-2) 
     ii = ii + 1 & jj = jj +1 
     tab(kk/4) = STDDEV(image(ii-1:ii+1,jj-1:jj+1))
  ENDFOR
 
RETURN,MEDIAN(tab)
END

;#######################################################

FUNCTION GROWING,image,subs,lim

;///region growing function

  coe   = lim
  bsize = 100000

  WHILE bsize GT 500 AND coe GT lim/2 DO BEGIN

       mini = MIN(image(subs))
       maxi = MAX(image(subs)) + coe


       blob = REGION_GROW(image,subs,/ALL_NEIGHBORS,threshold=[mini,maxi])

       bsize = (SIZE(blob))(1)

       CASE 1 OF
          bsize GE 500000:coe = coe - lim/10.
          bsize GE 100000:coe = coe - lim/20.
          bsize GE  10000:coe = coe - lim/50.
                    ELSE: coe = coe - lim/100.
       ENDCASE

       PRINT,coe,(SIZE(blob))(1)

  ENDWHILE

RETURN,blob
END

;#######################################################

FUNCTION FILAMENT_GR,im_HEP,im_SEG,lim

;APPLY REGION A REGION GROWING FUNCTION TO EACH SEED

  im_SEED = im_HEP*0

  ;####FIRST LABELING 
  im_LABL   = LABEL_REGION(im_SEG)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_SEED   = N_ELEMENTS(hist_LABL)

  ;####LOOP ON EACH SEED
  FOR ii=0, nb_SEED-1 DO BEGIN

    IF hist_LABL(ii) GT 1 AND hist_LABL(ii) LT 10000 THEN BEGIN

     ;####SUBSCRIPTS OF LABELLED REGIONS     
     region_SUBS = rev(rev[ii]:rev[ii+1]-1) 

     ;####CALCULATE AN INTENSITY LIMIT ON THE NEIGHBORHOOD 
     lim_REG = lim


     ;####TEST IF THE REGION HAS ALREADY BEEN DETECTED 
     seen_REG = WHERE(im_SEED(region_SUBS) EQ 1, s_r)
     IF s_r NE N_ELEMENTS(region_SUBS) THEN BEGIN

       region = GROWING(im_HEP,region_SUBS,lim_REG)

       PRINT,'region',FIX(ii),'/',FIX(nb_SEED)
       PRINT,'region points:',N_ELEMENTS(region)

       ;####TEST ON THE REGION SIZE (20 < size < 10000)
       IF N_ELEMENTS(region) GT 100 AND N_ELEMENTS(region) LT 400 THEN im_SEED(region) = 1

     ENDIF

    ENDIF

  ENDFOR

RETURN,im_SEED

END

;######################################################


FUNCTION FILAMENT_STD,INPUT=input

  diam = 840

  Xsiz = (SIZE(input))(1)
  Ysiz = (SIZE(input))(2)
  
  input=FIX(input)

  ;####TEST THE SIZE OF INPUT
  IF Xsiz NE Ysiz OR Xsiz NE 1024 THEN GOTO,end_filament

  ;####CREATE SOME MASKS
  mask_SUBS   = WHERE(input*0+1)
  mask_SUBS_Y = mask_SUBS /   Xsiz
  mask_SUBS_X = mask_SUBS MOD Xsiz
  mask_DIST   = SQRT((mask_SUBS_X-Xsiz/2)^2 + (mask_SUBS_Y-Ysiz/2)^2) 
  mask_0      = WHERE(mask_DIST GE (diam/2))
  mask_1      = WHERE(mask_DIST GE (diam/2)*0.99)

  ;####APPLY A MEDIAN FILTER AND DIVIDE INPUT BY THE RESULT
  im_REB = REBIN(input, Xsiz/2, Ysiz/2)
  im_MED = MEDIAN(im_REB, 30)
  im_MED = REBIN(im_MED, Xsiz, Ysiz)
  im_DIV = input/(im_MED+1)

  im_DIV(mask_1) = 0

  ;####ENHANCE THE CONSTRAST WITH HIST_EQUAL
  im_HEP  = HIST_EQUAL(im_DIV,PERCENT=1)

  ;####TRESHOLD TO FIND THE SEEDS
  tresh  = 2.
  im_SEG = (im_HEP LT tresh AND im_HEP GE 0.)
  im_SEG(mask_1) = 0
  
  ;####LIMIT CALCULATION
  ;std3x3_in = STDDEVCALC(input)
  std3x3 = STDDEVCALC(im_HEP)
  hist = HISTOGRAM(im_HEP)
  hist(0)=0 & hist(N_ELEMENTS(hist)-1)=0
  hist=SMOOTH(hist,10)
  ma=(WHERE(hist EQ MAX(hist)))(0)

  lim = ma & lim0 = ma
  jj  = 0
  res = FLTARR(6,3)
 
  WHILE lim GE lim0/2 DO BEGIN

    resu = FILAMENT_GR(im_HEP,im_SEG,lim)

    im_LABL   = LABEL_REGION(resu)
    hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
    nb_regi   = N_ELEMENTS(hist_LABL)
    res(jj,0) = lim 
    res(jj,1) = nb_regi
    res(jj,2) = std3x3
    lim = lim - 0.1*lim0
    jj = jj + 1
 
  ENDWHILE

RETURN,res

end_filament: 
END


;####################################################



