
@/home/fuller/IDL/Fit_Ellipse.pro

FUNCTION STDDEVCALC,image

  xs     = (SIZE(image))(1)
  ys     = (SIZE(image))(2)
  image2 = image(1:xs-2,1:ys-2)  
  nzero  = WHERE(image2,cc)
  tab    = FLTARR(cc/2+1)   ;take 1pix/2 to go faster
 
  FOR kk=0L,cc-1,2 DO BEGIN
     jj = nzero(kk) /   (xs-2)
     ii = nzero(kk) MOD (xs-2) 
     ii = ii + 1 & jj = jj +1 
     tab(kk/2) = STDDEV(image(ii-1:ii+1,jj-1:jj+1))
  ENDFOR
 
RETURN,MEDIAN(tab)
END

;#######################################################

FUNCTION GROWING,image,subs,lim

;///region growing function

  lim = lim*1.2
  coe = lim
  limsize = 80*ALOG(N_ELEMENTS(subs))+100 ;500
  bsize = limsize + 1

  WHILE bsize GT limsize AND coe GT lim/2 DO BEGIN

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


FUNCTION LIM_CALC,im_HEP,region_SUBS

;CALCULATE AN INTENSITY LIMIT ON THE NEIGHBORHOOD   
;IF NO LIMIT FOUND, LIMIT=-1

  ;####FIT AN ELLIPSE ON THE SEED 
  fitel = FIT_ELLIPSE(region_SUBS,CENTER=center,AXES=axes,NPOINTS=30,XSIZE=1024,YSIZE=1024)
  xx    = center(0)
  yy    = center(1)

  ;####DEFINE AN AERA WITH THE MAJOR AXIS LENGTH
  dist  = MAX(axes)
  dist  = dist*(50/(dist+10)+1)

  IF dist GT 400 THEN dist=400
  IF (xx-dist) GE 0 AND (xx+dist) LT 1024 AND (yy-dist) GE 0 $
                    AND (yy+dist) LT 1024 THEN BEGIN
  
     square1 = im_HEP(xx-dist:xx+dist,yy-dist:yy+dist)
 
     ;####COMPUTE THE INTENSITY LIMIT FROM THE HISTOGRAM MAX
     hist = HISTOGRAM(square1(WHERE(square1)),BINSIZE=1,NBINS=256)
     hist(0:1)=0 & hist(255)=0
     limit=(WHERE(hist EQ MAX(hist)))(0)    
     limit = limit * (0.5) ;0.55 - 0.002*std)

  ENDIF ELSE limit = -1

RETURN,limit

END

;#######################################################

FUNCTION FILAMENT_GR,im_HEP,im_SEG,lim

;APPLY REGION A REGION GROWING FUNCTION TO EACH SEED

  im_SEED = im_HEP*0

  ;####FIRST LABELING 
  im_LABL   = LABEL_REGION(im_SEG)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_SEED   = N_ELEMENTS(hist_LABL)

  PRINT,'nbre de seeds',nb_SEED

  ;####LOOP ON EACH SEED
  FOR ii=0, nb_SEED-1 DO BEGIN

    IF hist_LABL(ii) GT 1 AND hist_LABL(ii) LT 10000 THEN BEGIN

     ;####SUBSCRIPTS OF LABELLED REGIONS     
     region_SUBS = rev(rev[ii]:rev[ii+1]-1) 

     ;####CALCULATE AN INTENSITY LIMIT ON THE NEIGHBORHOOD 
     lim_REG = LIM_CALC(im_HEP, region_SUBS)
     IF lim_REG EQ -1 THEN lim_REG = lim

     ;PRINT,'lim_region',lim_REG

     ;####CHECK IF THE REGION HAS ALREADY BEEN DETECTED 
     seen_REG = WHERE(im_SEED(region_SUBS) EQ 1, s_r)
     IF s_r NE N_ELEMENTS(region_SUBS) THEN BEGIN

       region = GROWING(im_HEP,region_SUBS,lim_REG)

       PRINT,'region',FIX(ii),'/',FIX(nb_SEED)
       PRINT,'region points:',N_ELEMENTS(region)
       PRINT,'###########################'

       ;####TEST ON THE REGION SIZE (20 < size < 10000)
       IF N_ELEMENTS(region) GT 20 AND N_ELEMENTS(region) LT 10000 THEN im_SEED(region) = 1

     ENDIF

    ENDIF

  ENDFOR

RETURN,im_SEED

END

;######################################################


FUNCTION FILAMENT,INPUT=input

  diam = 840

  ;####GET THE INPUT ARRAY
  IF NOT KEYWORD_SET(INPUT) THEN BEGIN
    file = DIALOG_PICKFILE(PATH='/home/fuller/poub/SAV/',FILTER='*_p.sav')
    IF file NE '' THEN BEGIN
      RESTORE,file
      input = arr
    ENDIF ELSE GOTO,end_filament
  ENDIF

  Xsiz = (SIZE(input))(1)
  Ysiz = (SIZE(input))(2)
  
  input=FIX(input)

  ;####TEST THE SIZE OF INPUT
  IF Xsiz NE Ysiz OR Xsiz NE 1024 THEN BEGIN
     mess = DIALOG_MESSAGE('Not a 1024x1024 array, resize ?',/QUESTION)
     IF mess EQ 'Yes' THEN BEGIN
        Xsiz  = 1024
        Ysiz  = 1024
        input = CONGRID(input, Xsiz, Ysiz)
     ENDIF ELSE GOTO,end_filament
  ENDIF

  WINDOW,0,XSIZE=Xsiz,YSIZE=Ysiz
  SAVE,input,filename='/home/fuller/poub/SAV/image1.sav'
  TVSCL,input

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
  ;SAVE,im_DIV,filename='/home/fuller/poub/SAV/image2.sav'
  ;TVSCL,im_DIV

  ;####ENHANCE THE CONSTRAST WITH HIST_EQUAL
  im_HEP  = HIST_EQUAL(im_DIV,PERCENT=1) + 1
  im_HEP(mask_1) = 0
  ;SAVE,im_HEP,filename='/home/fuller/poub/SAV/image3.sav'
  ;TVSCL,im_HEP

  ;####TRESHOLD TO FIND THE SEEDS
  tresh  = 3.
  im_SEG = (im_HEP LT tresh AND im_HEP GE 1.)
  ;im_SEG(mask_1) = 0
  ;SAVE,im_SEG,filename='/home/fuller/poub/SAV/image4.sav'
  ;TVSCL,im_SEG

  
  ;####LIMIT CALCULATION
  ;std3x3 = STDDEVCALC(input)
  hist = HISTOGRAM(im_HEP)
  hist(0)=0 & hist(N_ELEMENTS(hist)-1)=0
  hist=SMOOTH(hist,10)
  ma=(WHERE(hist EQ MAX(hist)))(0)

  lim = ma*(0.5); 0.5 - 0.002*std3x3)
  PRINT,lim

  ;####REGION GROWING FUNCTION
   res = FILAMENT_GR(im_HEP,im_SEG,lim)
   ;SAVE,res,filename='/home/fuller/poub/SAV/image5.sav
   TVSCL,res

  ;####MIXED IMAGES
   inputb = input
   toto = WHERE(res EQ 1)
   inputb(toto) = 0
   ;TVSCL,inputb
   ;SAVE,inputb,filename='/home/fuller/poub/SAV/image6.sav'
  
RETURN,res

end_filament: 
END


;####################################################






