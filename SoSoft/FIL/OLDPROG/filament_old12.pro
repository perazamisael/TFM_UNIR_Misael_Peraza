@/home/fuller/IDL/FIL/clean_image.pro
@/home/fuller/IDL/FIL/efr_roundmask.pro
@/home/fuller/IDL/FIL/efr_sharpen.pro

FUNCTION EFR_FIL_SEEDS_OLD,im_IN,coeff
;overlapping square windows covering the whole image 
;to make a first segmentation


  im_OUT = im_IN*0b 
  xsize  = (SIZE(im_IN))[1]
  mom = MOMENT(im_IN[WHERE(im_IN)],SDEV=sdev,MDEV=mdev)


  ;##### distance between 2 successive windows
  tay = 80L 


  ;##### Loop over the windows
  FOR ii = tay,1024-tay-1,tay DO BEGIN

     FOR jj = tay,1024-tay-1,tay DO BEGIN
 
        ;#### define the window content 
        win  = im_IN[ii-tay:ii+tay-1,jj-tay:jj+tay-1]

        ;#### If the window is close to the limb make sure
        ;#### there are enough points to compute stats
        wwin = WHERE(win,nw)
        IF wwin[0] NE -1 AND nw GT (4*tay*tay)/2 THEN BEGIN

            ;#### Set values greater than (mean+stdev) to 0.
            ;#### Set values lower   than (mean-stdev) to 0.
            momi = MOMENT(win[WHERE(win)],SDEV=sdevi)
            wintmp = win
            wmore = WHERE(wintmp GT momi[0] + 2.*sdevi)
            IF wmore[0] NE -1 THEN wintmp[wmore] = 0.;momi[0]+1.*sdevi
            wless = WHERE(wintmp LT momi[0] - 2.*sdevi AND wintmp GT 0.)
            IF wless[0] NE -1 THEN wintmp[wless] = 0.;momi[0]-1.*sdevi  


            ;#### Then we can compute the std dev of the 'Quiet Sun'
            momi = MOMENT(wintmp[WHERE(wintmp)],SDEV=sdevi,MDEV=mdevi)

            ;#### And get a treshold to find the seeds in the window

            lowtreshi = momi[0] - coeff*sdevi 


            ;#### Treshold
            fil = WHERE(win le lowtreshi AND win GT 0.,nfil)


            ;#### Get back to the coordinates of the whole image
            IF fil[0] NE -1 THEN BEGIN
              xxf = FIX(fil MOD (2*tay)) + (ii-tay)
              yyf = FIX(fil/(2*tay)) + (jj-tay)
              ind = xxf + yyf*xsize
              im_OUT(ind)= 1b
            ENDIF

       ENDIF

     ENDFOR

  ENDFOR 


  ;##### Set limb points to 0 (limb values can be false)
  mask = EFR_ROUNDMASK(xsize,ysize,0.,420*0.99,/INV)
  im_OUT[mask] = 0b


;window,/free,xs=1024,ys=1024
;tvscl,im_OUT


RETURN,im_OUT
END

;########################################################

FUNCTION EFR_FIL_SEEDS,im_IN,coeff
;32 square windows covering the whole image 
;to make a find the seeds


  im_OUT = im_IN*0b 
  xsize  = (SIZE(im_IN))[1]
  mom = MOMENT(im_IN[WHERE(im_IN)],SDEV=sdev,MDEV=mdev)


  ;##### distance between 2 successive windows
  tay = 150L 


  ;##### Loop over the windows
  FOR ii = 62,961,tay DO BEGIN

     FOR jj = 62,961,tay DO BEGIN
 
        ;#### define the window content 
        win  = im_IN[ii:ii+tay-1,jj:jj+tay-1]

        ;#### If the window is close to the limb make sure
        ;#### there are enough points to compute stats
        wwin = WHERE(win,nw)
        IF wwin[0] NE -1 THEN BEGIN

           
            ;momi  = MOMENT(win[wwin],SDEV=sdevi)
            ;qsi   = momi[0]
            
            qsi   = MEDIAN(win[wwin])
            sdevi = SQRT(1./(N_ELEMENTS(win[wwin])-1)*TOTAL((win[wwin]-qsi)^2))

            ;#### Discard brigth and dark regions from mean
            ;#### calculation
            wvals = WHERE(win GT qsi - 2.*sdevi AND $
                          win LT qsi + 2.*sdevi,nvals)
            
            IF nvals GT (tay*tay)/10. THEN BEGIN 

              ;#### Then we can compute the std dev of the 'Quiet Sun'
              momi2 = MOMENT(win[wvals],SDEV=sdevi2,MDEV=mdevi2)

              ;#### And get a treshold to find the seeds in the window
              lowtreshi = momi2[0] - coeff*sdevi2 

            ENDIF ELSE BEGIN
print,'cas2'
              lowtreshi = qsi - coeff*sdevi 

            ENDELSE

            ;#### Treshold
            fil = WHERE(win le lowtreshi AND win GT 0.,nfil)


            ;#### Get back to the coordinates of the whole image
            IF fil[0] NE -1 THEN BEGIN
              xxf = FIX(fil MOD tay) + ii
              yyf = FIX(fil/tay) + jj
              ind = xxf + yyf*xsize
              im_OUT(ind)= 1b
            ENDIF



        ENDIF

     ENDFOR

  ENDFOR 


  ;##### Set limb points to 0 (limb values can be false)
  mask_SUBS   = WHERE(im_IN*0+1)
  mask_SUBS_Y = mask_SUBS /   xsize
  mask_SUBS_X = mask_SUBS MOD xsize
  mask_DIST   = SQRT((mask_SUBS_X-xsize/2)^2 + (mask_SUBS_Y-xsize/2)^2) 
  mask        = WHERE(mask_DIST GE 420*0.99)
  im_OUT[mask] = 0b


;window,/free,xs=1024,ys=1024
;tvscl,im_OUT


RETURN,im_OUT
END

;#######################################################
FUNCTION LIM_CALC,im_ORI,region_SUBS,coeff

;CALCULATE AN INTENSITY LIMIT ON THE NEIGHBORHOOD   
;IF NO LIMIT FOUND, LIMIT=-1


   ;####Bounding rectangle
   xsize = (SIZE(im_ORI))[1]
   max_xx = MAX(region_SUBS MOD xsize,MIN=min_xx)
   max_yy = MAX(region_SUBS / xsize,MIN=min_yy)

   xx   = min_xx + (max_xx - min_xx)/2
   yy   = min_yy + (max_yy - min_yy)/2
   dist  = 20*SQRT([max_xx-min_xx,max_yy-min_yy]) > 30


   distx = ( (dist[0] < xx) < (xsize-xx-1) ) < 300
   disty = ( (dist[1] < yy) < (xsize-yy-1) ) < 300


;essai (on supprime les valeurs hautes et basses avant le calcul
;du seuil)
square1  = im_ORI(xx-distx:xx+distx,yy-disty:yy+disty)
mom      = MOMENT(square1[WHERE(square1)],SDEV=sdev,MDEV=mdev)
medv     = MEDIAN(square1[WHERE(square1)])
sunsurf  = square1(WHERE(square1 GT medv-1.5*sdev AND square1 LT medv+1.5*sdev))
mom      = MOMENT(sunsurf,SDEV=sdev,MDEV=mdev)
limit    = mom[0] - coeff*sdev


RETURN,limit

END

;#######################################################

FUNCTION FILAMENT_GR,im_ORI,im_SEG,diam,coeff

;APPLY REGION A REGION GROWING FUNCTION TO EACH SEED

  mask = EFR_ROUNDMASK((SIZE(im_ORI))[1],(SIZE(im_ORI))[2],0.,0.99*diam/2.,/INV)
  im_ORI[mask]=0.
  im_SEG[mask]=0b
  im_SEED = im_SEG*0b


  ;####FIRST LABELING 
  im_LABL   = LABEL_REGION(im_SEG)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_SEED   = N_ELEMENTS(hist_LABL)

  PRINT,'nbre de seeds',nb_SEED


  ;####LOOP ON EACH SEED
  FOR ii=0, nb_SEED-1 DO BEGIN

    IF hist_LABL(ii) GT 10 AND hist_LABL(ii) LT 10000 THEN BEGIN

     ;####SUBSCRIPTS OF LABELLED REGIONS     
     region_SUBS = rev(rev[ii]:rev[ii+1]-1) 


     ;####CALCULATE AN INTENSITY LIMIT ON THE NEIGHBORHOOD 
     lim_REG = LIM_CALC(im_ORI, region_SUBS, coeff)

     ;####CHECK IF THE REGION HAS ALREADY BEEN DETECTED 
     seen_REG = WHERE(im_SEED(region_SUBS) EQ 1, s_r)

     IF s_r NE N_ELEMENTS(region_SUBS) THEN BEGIN

        mini = 0.00001;MIN(im_ORI(region_subs))
        maxi = lim_REG 


        ;##### Fix treshold range from the local statisics 

        region = REGION_GROW(im_ORI,region_SUBS,/ALL_NEIGHBORS,threshold=[mini,maxi]) 


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


FUNCTION FILAMENT,obskey,INPUT=input,CORRECTED=im_FLT,FULLCLEAN=fullclean,FLATCLEAN=flatclean,DISPLAY=display

  diam = 840
  pathdata = '/data2/fuller/FITS/Ha/2002/PROCESSED/'
  pathprog = '/home/fuller/IDL/FIL/'


  ;####IDL RELEASE CHECK
  version = FLOAT(!VERSION.RELEASE)
  IF version LT 5.5 THEN BEGIN
    PRINT,'Use IDL5.5 or greater'
    RETALL
  ENDIF
 

  ;####GET THE INPUT ARRAY
  IF NOT KEYWORD_SET(INPUT) THEN BEGIN
    file = DIALOG_PICKFILE(PATH=pathdata,FILTER='*fits')
    IF file NE '' THEN BEGIN
      input = LONG(READFITS(file,head1))   
    ENDIF ELSE RETALL
  ENDIF


  ;#####IMAGE SIZE
  Xsiz = (SIZE(input))[1]
  Ysiz = (SIZE(input))[2]


  ;#####DISPLAY
  IF KEYWORD_SET(display) THEN BEGIN
    WINDOW,0,XSIZE=Xsiz,YSIZE=Ysiz
    TVSCL,input
  ENDIF

  ;#####GET PARAMETERS FROM SAVE FILE
  RESTORE,pathprog + 'obs_param.sav'
  wobs = WHERE(obs_param.obs EQ STRING(obskey),nmatch)
  IF nmatch EQ 0 THEN RETALL
  coeffhough  = obs_param[wobs].hough
  coefflapl   = obs_param[wobs].lapl
  coeffseeds  = obs_param[wobs].seeds
  coeffregion = obs_param[wobs].regiongrow


  ;####CLEAN THE IMAGE (INTENSITY/LINES/DUST)
   im_FLT = input
   IF KEYWORD_SET(fullclean) THEN im_FLT = CLEAN_IMAGE(input,diam/2,/HOUGH) 
   IF KEYWORD_SET(flatclean) THEN im_FLT = CLEAN_IMAGE(input,diam/2)

  ;####DISPLAY
   IF KEYWORD_SET(display) THEN TVSCL,im_FLT

   im_FLT=LONG(im_FLT)

  ;####SUBTRACT THE LAPLACIAN OF IMAGE TO SHARPEN THE EDGES
   im_LAPL  = EFR_SHARPEN(im_FLT,coefflapl)


  ;####MEDIAN FILTER TO REDUCETHE NUMBER OF SEEDS
   im_LAPL = MEDIAN(im_LAPL,3)  
   

  ;####FIND THE SEEDS
   im_SEG = EFR_FIL_SEEDS(im_LAPL,coeffseeds)


  ;####DISPLAY
   IF KEYWORD_SET(display) THEN TVSCL,im_SEG


  ;####REGION GROWING FUNCTION
   res = FILAMENT_GR(im_LAPL,im_SEG,diam,coeffregion)


  ;####DISPLAY
   IF KEYWORD_SET(display) THEN TVSCL,res


  ;####DISPLAY
   IF KEYWORD_SET(display) THEN BEGIN

     toto=where(res)
     imi=im_FLT
     imi[toto]=0. 
     ;masktemp   = WHERE(mask_DIST LE diam/2.)
     masktemp = EFR_ROUNDMASK(1024,1024,0.,diam/2.)
     imi2=intarr(1024,1024)
     imi2[masktemp]=500.
     imi3=imi2
     imi3[where(im_SEG)]=1000.
     imi4=imi2
     imi4[where(res)]=MAX(imi)
     tvscl,imi
     window,10,xsize=1024,ysize=512
     tv,bytscl(rebin(imi4,512,512),0.,MAX(imi4))
     tvscl,rebin(im_FLT,512,512),512,0
   ENDIF


RETURN,res

END


;####################################################








