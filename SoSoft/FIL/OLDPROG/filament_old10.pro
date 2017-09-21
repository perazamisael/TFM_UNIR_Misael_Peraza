
@/home/fuller/IDL/FIL/clean_image.pro



FUNCTION FIRST_SEG,im_IN
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
        ;#### there are enough point to compute stats
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

            lowtreshi = momi[0] - 4.2*sdevi

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
  mask_SUBS   = WHERE(im_IN*0+1)
  mask_SUBS_Y = mask_SUBS /   xsize
  mask_SUBS_X = mask_SUBS MOD xsize
  mask_DIST   = SQRT((mask_SUBS_X-xsize/2)^2 + (mask_SUBS_Y-xsize/2)^2) 
  mask        = WHERE(mask_DIST GE 420*0.99)
  im_OUT[mask] = 0b


;window,/free,xs=1024,ys=1024
tvscl,im_OUT


RETURN,im_OUT
END

;#######################################################
FUNCTION LIM_CALC,im_ORI,region_SUBS

;CALCULATE AN INTENSITY LIMIT ON THE NEIGHBORHOOD   
;IF NO LIMIT FOUND, LIMIT=-1


   ;####Bounding rectangle
   xsize = (SIZE(im_ORI))[1]
   max_xx = MAX(region_SUBS MOD xsize,MIN=min_xx)
   max_yy = MAX(region_SUBS / xsize,MIN=min_yy)

   xx   = min_xx + (max_xx - min_xx)/2
   yy   = min_yy + (max_yy - min_yy)/2
   dist  = 20*SQRT([max_xx-min_xx,max_yy-min_yy]) > 20

   ;distx = ( (dist < xx) < (1024-xx-1) ) < 300
   ;disty = ( (dist < yy) < (1024-yy-1) ) < 300

   distx = ( (dist[0] < xx) < (xsize-xx-1) ) < 300
   disty = ( (dist[1] < yy) < (xsize-yy-1) ) < 300


   im_WNF  = im_ORI ;### do not include the region in moment computation
   im_WNF[region_subs]=0.
   square1 = im_WNF(xx-distx:xx+distx,yy-disty:yy+disty)


   sunsurf = square1(WHERE(square1 GT 1))
   mom     = MOMENT(sunsurf,SDEV=sdev,MDEV=mdev)
   limit   = mom(0) - 1.5*sdev


RETURN,limit

END

;#######################################################

FUNCTION FILAMENT_GR,im_ORI,im_SEG,mask

;APPLY REGION A REGION GROWING FUNCTION TO EACH SEED

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
     lim_REG = LIM_CALC(im_ORI, region_SUBS)


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


FUNCTION FILAMENT,INPUT=input,CORRECTED=im_FLT

  diam = 840

  ;####IDL RELEASE CHECK
  version = FLOAT(!VERSION.RELEASE)
  IF version LT 5.5 THEN BEGIN
    PRINT,'Use IDL5.5 or greater'
    GOTO,end_filament
  ENDIF
 

  ;####GET THE INPUT ARRAY
  IF NOT KEYWORD_SET(INPUT) THEN BEGIN
    file = DIALOG_PICKFILE(PATH='/home/fuller/poub/FITS/PROCESSED/',FILTER='*fits')
    IF file NE '' THEN BEGIN
      arr = READFITS(file,head1)   
      input = arr ;(temporary?)
    ENDIF ELSE GOTO,end_filament
    ;ENDIF ELSE RETURN,0
  ENDIF


  ;#####SIZE AND TYPE
  Xsiz = (SIZE(input))(1)
  Ysiz = (SIZE(input))(2)
  input=FIX(input)


  ;#####DISPLAY
  WINDOW,0,XSIZE=Xsiz,YSIZE=Ysiz
  TVSCL,input


  ;####CREATE SOME MASKS
  mask_SUBS   = WHERE(input*0+1)
  mask_SUBS_Y = mask_SUBS /   Xsiz
  mask_SUBS_X = mask_SUBS MOD Xsiz
  mask_DIST   = SQRT((mask_SUBS_X-(Xsiz-0.5)/2)^2 + (mask_SUBS_Y-(Ysiz-0.5)/2)^2) 
  ;mask_0      = WHERE(mask_DIST GE (diam/2),nmsk0)
  mask_1      = WHERE(mask_DIST GE (diam/2)*0.99,nmsk1)
  ;mask_2      = WHERE(mask_DIST GE (diam/2)*0.98,nmsk2)
  ;mask_3      = WHERE(mask_DIST LE (diam/2)*0.99 AND mask_DIST GE (diam/2)*0.98,nmsk3)

 
  ;####CLEAN THE IMAGE (INTENSITY/LINES/DUST)
   im_FLT = CLEAN_IMAGE(input,diam/2)
   tvscl,im_FLT


  ;####TRESHOLD TO FIND THE SEEDS
  im_SEG = FIRST_SEG(im_FLT)
  TVSCL,im_SEG

  
  ;####REGION GROWING FUNCTION
   res = FILAMENT_GR(im_FLT,im_SEG,mask_1)
;  WINDOW,1,XSIZE=xsiz,YSIZE=ysiz


   TVSCL,res

;#####affichage
toto=where(res)
imi=im_FLT
imi[toto]=0.
masktemp   = WHERE(mask_DIST LE diam/2.)
imi2=intarr(1024,1024)
imi2[masktemp]=500.
imi3=imi2
imi3[where(im_SEG)]=1800.
imi4=imi2
imi4[where(res)]=1800.
tvscl,imi
;window,/free,xsize=1024,ysize=512
;  tv,bytscl(rebin(im_FLT,512,512),0.,4000.)
;  tv,bytscl(rebin(imi3,512,512),0.,2000.),512,0
window,10,xsize=1024,ysize=512
  tv,bytscl(rebin(imi4,512,512),0.,2000.)
  tv,bytscl(rebin(im_FLT,512,512),0.,MAX(im_FLT)),512,0

RETURN,res

end_filament: 
END


;####################################################








