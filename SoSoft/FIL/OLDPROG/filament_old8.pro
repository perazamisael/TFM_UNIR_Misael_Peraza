
@/home/fuller/IDL/FIL/Fit_Ellipse.pro
;@/home/fuller/IDL/FIL/SNF.pro

;#######################################################

FUNCTION FIRST_SEG,im_IN,im_2TRESH,mask1,mask2,mask3


  im_mg=im_IN*0.

  ;Lower treshold to find the seeds
  ;hist = HISTOGRAM(im_2TRESH(WHERE(im_2TRESH GT 0.)),MIN=0.)
  ;gfit = GAUSSFIT(FINDGEN(N_ELEMENTS(hist)),hist,AA)
  ;lowtresh = AA(1)-4.5*AA(2)

  mom = MOMENT(im_2TRESH(WHERE(im_2TRESH GT 0.)),SDEV=sdev)
  lowtresh = mom(0) - 3.0*sdev

  stru=REPLICATE(1,3,3)
  im_GRAD=MORPH_GRADIENT(im_IN,stru)
  im_GRAD(mask2) = 0. ;eliminate the high values of the limb

  ;mediane sur subimage 80x80 du morph_gradient
  FOR ii=40,1024-41,5 DO BEGIN
   FOR jj=40,1024-41,5 DO BEGIN
      area = im_GRAD(ii-40:ii+40,jj-40:jj+40)
      toto=WHERE(area GT 0,ntoto)
      IF ntoto GT 0 THEN meadi=MEDIAN(area(toto)) ELSE meadi=0.
      warea = WHERE(area GT 0. AND area LT 1.1*meadi)
      IF im_IN(ii,jj) GT 0. THEN BEGIN
         area2 = area(warea) 
         mea = MEDIAN(area2)
         ;mea2 = MEAN(area2)
         im_mg(ii-2:ii+2,jj-2:jj+2)=mea
      ENDIF
   ENDFOR
  ENDFOR  

  ;reconstruction du bord solaire a partir d'une valeur moyenne
  FOR jj=0,N_ELEMENTS(mask3)-1 DO BEGIN
      pt=mask3(jj)
      ptx=pt MOD 1024
      pty=pt/1024
      aire=im_mg(ptx-10:ptx+10,pty-10:pty+10)
      im_mg(pt)=MEAN(aire(WHERE(aire GT 0.)))
  ENDFOR

;  window,4,xs=1024,ys=1024
;  TVSCL,im_mg

  ;hist = HISTOGRAM(im_mg(WHERE(im_mg GT 0.)),MIN=0.)
  ;gfit = GAUSSFIT(FINDGEN(N_ELEMENTS(hist)),hist,AA)
  ;window,5
  ;print,AA(1)
  ;shft=AA(1)
  ;maxi=MAX(im_mg(WHERE(im_mg GT 0.)),MIN=mini)

  shft = MEAN(im_mg(WHERE(im_mg GT 0.)))
  fact=2.2

  im_tresh = lowtresh+(im_IN*0.) - fact*(im_mg-shft)
  im_tresh(mask1)=0.

  im_2TRESH=FLOAT(im_2TRESH)+10.^3 ;to avoid any problem with 0.
  im_2TRESH(mask1)=0.
  im_DIFF = FLOAT(im_2TRESH) - im_tresh

  im_OUT = (im_DIFF LT 10.^3 AND im_DIFF GT 0.) 

;window,6,xs=1024,ys=1024
;tvscl,im_OUT

RETURN,im_OUT
END

;#######################################################

FUNCTION CLEAN_DUST,image,mask
;window,3,xs=1024,ys=1024
 
  im=image
  res=image

  struct = REPLICATE(1,3,3)
  struct(1,1) = 0
  cis  = CONVOL(FLOAT(image),struct)/8.

  dcis = (FLOAT(cis)+1.)/(FLOAT(image)+1.)
  dcis(mask)=1.

  ;medi=0.8*MEDIAN(image(WHERE(image)))

  mom = MOMENT(image(WHERE(image GT 0.)),SDEV=sdev)
  lowval = mom(0) - 3.0*sdev
 
  tochange = WHERE(dcis GT 1.125 OR dcis LT 0.875,ntc)

  IF ntc GT 0 THEN BEGIN
     FOR pp=0,ntc-1 DO BEGIN
         xxp  = tochange(pp) MOD 1024
         yyp  = tochange(pp)/1024
         mbox = image(xxp-2:xxp+2,yyp-2:yyp+2)
         mbox2= res(xxp-3:xxp+3,yyp-3:yyp+3)
         tc2  = WHERE(mbox GT lowval,ntc2)
         IF ntc2 GE 18 THEN BEGIN
            res(xxp-1:xxp+1,yyp-1:yyp+1)=MEDIAN(mbox2)
            ;im(xxp-1:xxp+1,yyp-1:yyp+1)=1.
         ENDIF
     ENDFOR
  ENDIF
  ;TVSCL,im
  ;window,4,xs=1024,ys=1024
  ;tvscl,res

RETURN,res
END

;#######################################################

FUNCTION LIM_CALC,im_HEP,region_SUBS

;CALCULATE AN INTENSITY LIMIT ON THE NEIGHBORHOOD   
;IF NO LIMIT FOUND, LIMIT=-1

  ;####FIT AN ELLIPSE ON THE SEED 
   fitel = FIT_ELLIPSE(region_SUBS,CENTER=center,AXES=axes,NPOINTS=30,XSIZE=1024,YSIZE=1024)
   xx    = center(0)
   yy    = center(1)

  ;####DEFINE AN AERA WITH THE MAJOR AXIS LENGTH
   dist  = MAX(axes)
   dist  = 20*SQRT(dist)        ;the area size should not be growing 
                                ;linearly with the major axis length

   dist = (dist < xx) < (1024-xx-1)
   dist = (dist < yy) < (1024-yy-1)
   dist = dist < 300
print,'dist',dist
;   IF (xx-dist) GE 0 AND (xx+dist) LT 1024 AND (yy-dist) GE 0 $
;                     AND (yy+dist) LT 1024 THEN BEGIN
  
      square1 = im_HEP(xx-dist:xx+dist,yy-dist:yy+dist)
      sunsurf = square1(WHERE(square1 GT 1))
      mom     = MOMENT(sunsurf,SDEV=sdev)
      limit   = mom(0) - 1.4*sdev

;   ENDIF ELSE limit = -1


RETURN,limit

END

;#######################################################

FUNCTION FILAMENT_GR,im_HEP,im_SEG,lim

;APPLY REGION A REGION GROWING FUNCTION TO EACH SEED


  im_SEED = im_HEP*0b


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

     ;####CHECK IF THE REGION HAS ALREADY BEEN DETECTED 
     seen_REG = WHERE(im_SEED(region_SUBS) EQ 1, s_r)
     IF s_r NE N_ELEMENTS(region_SUBS) THEN BEGIN

        mini = MIN(im_HEP(region_subs))
        maxi = lim_REG 

        region = REGION_GROW(im_HEP,region_subs,/ALL_NEIGHBORS,threshold=[mini,maxi]) 

        PRINT,'region',FIX(ii),'/',FIX(nb_SEED)
        PRINT,'region points:',N_ELEMENTS(region)
        PRINT,'###########################'

        ;####TEST ON THE REGION SIZE (20 < size < 10000)
        IF N_ELEMENTS(region) GT 20 AND N_ELEMENTS(region) LT 10000 THEN im_SEED(region) = 1b

     ENDIF

    ENDIF

  ENDFOR

RETURN,im_SEED

END

;######################################################


FUNCTION FILAMENT,INPUT=input

  diam = 840

  ;####IDL RELEASE CHECK
  version = FLOAT(!VERSION.RELEASE)
  IF version LT 5.5 THEN BEGIN
    PRINT,'Use IDL5.5 or greater'
    GOTO,end_filament
  ENDIF
 

  ;####GET THE INPUT ARRAY
  IF NOT KEYWORD_SET(INPUT) THEN BEGIN
    ;file = DIALOG_PICKFILE(PATH='/home/fuller/poub/SAV/',FILTER='*_p.sav')
    file = DIALOG_PICKFILE(PATH='/home/fuller/poub/FITS/PROCESSED/',FILTER='*fits')
    IF file NE '' THEN BEGIN
      ;RESTORE,file
      arr = READFITS(file,head1)   
      input = arr
    ENDIF ELSE GOTO,end_filament
  ENDIF


  ;#####SIZE AND TYPE
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


  ;#####DISPLAY
  WINDOW,0,XSIZE=Xsiz,YSIZE=Ysiz
  ;SAVE,input,filename='/home/fuller/poub/SAV/image1.sav'
  TVSCL,input


  ;####CREATE SOME MASKS
  mask_SUBS   = WHERE(input*0+1)
  mask_SUBS_Y = mask_SUBS /   Xsiz
  mask_SUBS_X = mask_SUBS MOD Xsiz
  mask_DIST   = SQRT((mask_SUBS_X-Xsiz/2)^2 + (mask_SUBS_Y-Ysiz/2)^2) 
  mask_0      = WHERE(mask_DIST GE (diam/2))
  mask_1      = WHERE(mask_DIST GE (diam/2)*0.99,nmsk1)
  mask_2      = WHERE(mask_DIST GE (diam/2)*0.98,nmsk2)
  mask_3      = WHERE(mask_DIST LE (diam/2)*0.99 AND mask_DIST GE (diam/2)*0.98,nmsk3)

  ;####APPLY A MEDIAN FILTER AND DIVIDE INPUT BY THE RESULT
  medv   = MEDIAN(input(WHERE(input)))
  im_REB = REBIN(input, Xsiz/2, Ysiz/2)
  im_MED = MEDIAN(im_REB, 30)
  im_MED = REBIN(im_MED, Xsiz, Ysiz)
  ;im_DIV = 1d/(im_MED+1.)*input*medv
  im_DIV = input-im_MED+medv
  im_DIV(mask_1) = 0.
  ;SAVE,im_DIV,filename='/home/fuller/poub/SAV/image2.sav'
  TVSCL,im_DIV

  ;####REMOVE THE DUST
  im_DIV = CLEAN_DUST(im_DIV,mask_2)
  TVSCL,im_DIV

  ;####ENHANCE THE CONSTRAST WITH HIST_EQUAL
  im_HEP  = HIST_EQUAL(im_DIV,PERCENT=1) + 1
  im_HEP(mask_1) = 0
  ;SAVE,im_HEP,filename='/home/fuller/poub/SAV/image3.sav'
;  TVSCL,im_HEP

  ;####TRESHOLD TO FIND THE SEEDS
  im_SEG = FIRST_SEG(im_HEP,im_DIV,mask_1,mask_2,mask_3)
  ;im_SEG(mask_1) = 0
  ;SAVE,im_SEG,filename='/home/fuller/poub/SAV/image4.sav'
  TVSCL,im_SEG
  
  ;####GLOBAL LIMIT CALCULATION
  ;nozero   = im_HEP(WHERE(im_HEP GT 1))
  ;mom      = MOMENT(nozero,SDEV=stddglob)
  ;meanglob = mom(0)
  ;lim      = meanglob - 1.4*stddglob

  ;####REGION GROWING FUNCTION
   res = FILAMENT_GR(im_HEP,im_SEG,lim)
   ;SAVE,res,filename='/home/fuller/poub/SAV/image5b.sav
;   WINDOW,1,XSIZE=xsiz,YSIZE=ysiz
   TVSCL,res

  ;####MORPH_CLOSE,THIN
   ;thinres = draw_from_center(res);THIN(MORPH_CLOSE(res,REPLICATE(1,10,10)))
   ;WINDOW,2,XSIZE=xsiz,YSIZE=ysiz
   ;TVSCL,thinres

  ;####MIXED IMAGES
   ;inputb = im_HEP
   ;toto = WHERE(thinres EQ 1b)
   ;inputb(toto) = MAX(im_HEP)-100
   ;TVSCL,inputb
   ;SAVE,inputb,filename='/home/fuller/poub/SAV/image6.sav'
  

RETURN,res

end_filament: 
END


;####################################################






