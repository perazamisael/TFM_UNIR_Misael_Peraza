
@/home/fuller/IDL/FIL/Fit_Ellipse.pro
@/home/fuller/IDL/FIL/clean_image.pro

;#######################################################

FUNCTION FIRST_SEG,im_IN,im_2TRESH,mask1,mask2,mask3


  im_mg=im_IN*0.

  ;Lower treshold to find the seeds
  mom = MOMENT(im_2TRESH(WHERE(im_2TRESH GT 0.)),SDEV=sdev)
  lowtresh = mom(0) - 3.0*sdev

  ;morphological gradient
  stru=REPLICATE(1,3,3)
  im_GRAD=MORPH_GRADIENT(im_IN,stru)
  im_GRAD(mask2) = 0. ;eliminate the high values of the limb


  ;median on 80x80 subimages
  FOR ii=40,1024-41,5 DO BEGIN
   FOR jj=40,1024-41,5 DO BEGIN

      area = im_GRAD(ii-40:ii+40,jj-40:jj+40)
      toto=WHERE(area GT 0,ntoto)
      IF ntoto GT 0 THEN meadi=MEDIAN(area(toto)) ELSE meadi=0.
      warea = WHERE(area GT 0. AND area LT 1.1*meadi) ;to eliminate highest values
      IF im_IN(ii,jj) GT 0. THEN BEGIN
         area2 = area(warea) 
         mea = MEDIAN(area2)
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

 ; window,4,xs=1024,ys=1024
 ; TVSCL,im_mg

  shft = MEAN(im_mg(WHERE(im_mg GT 0.)))
  fact=2.2
save,im_mg,filename='/home/fuller/IDL/FIL/adaptresh.sav'
  im_tresh = lowtresh+(im_IN*0.) - fact*(im_mg-shft)
  im_tresh(mask1)=0.

  im_TEMP=FLOAT(im_2TRESH)+10.^3 ;to avoid any problem with 0.
  im_TEMP(mask1)=0.
  im_DIFF = im_TEMP - im_tresh

  im_OUT = (im_DIFF LT 10.^3 AND im_DIFF GT 0.) 

window,6,xs=1024,ys=1024
tvscl,im_OUT

RETURN,im_OUT
END

;#######################################################

FUNCTION FIRST_SEG2,im_IN
;overlapping squares windows covering the whole image 

  
  im_OUT = im_IN*0b 
  xsize  = (SIZE(im_IN))[1]

  ;##### distance between 2 successive windows
  tay = 100L 

  ;##### Loop over the windows
  FOR ii = tay,1024-tay-1,tay DO BEGIN

     FOR jj = tay,1024-tay-1,tay DO BEGIN
 
        ;#### define the window content 
        win  = im_IN[ii-tay:ii+tay-1,jj-tay:jj+tay-1]

        ;#### If the window is close to the limb make sure
        ;#### there are enough point to compute stats
        wwin = WHERE(win,nw)
        IF wwin[0] NE -1 AND nw GT (4*tay*tay)/3 THEN BEGIN

            ;#### Set values greater than (mean+stdev) to (mean+stdev)
            ;#### Set values lower   than (mean-stdev) to (mean-stdev)
            momi = MOMENT(win[WHERE(win)],SDEV=sdevi)
            wintmp = win
            wmore = WHERE(wintmp GT momi[0] + sdevi)
            IF wmore[0] NE -1 THEN wintmp[wmore] = momi[0]+1.*sdevi
            wless = WHERE(wintmp LT momi[0] - sdevi AND wintmp GT 0.)
            IF wless[0] NE -1 THEN wintmp[wless] = momi[0]-1.*sdevi  
 
            ;#### Then we can compute the std dev of the background
            momi = MOMENT(wintmp[WHERE(wintmp)],SDEV=sdevi)

            ;#### And get a treshold to find the seeds in the window
            lowtreshi = momi[0] - 5.*sdevi

            ;#### Treshold
            fil = WHERE(area le lowtreshi AND area GT 0.,nfil)

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

window,/free,xs=1024,ys=1024
tvscl,im_OUT


RETURN,im_OUT
END
;#######################################################

FUNCTION FIRST_SEG3,im_IN
;rings

  im_OUT = im_IN*0b
  Xsiz = (SIZE(im_IN))[1]
  rsun = 420
  ;####
  mask_SUBS   = WHERE(im_IN*0+1)
  mask_SUBS_Y = mask_SUBS /   Xsiz
  mask_SUBS_X = mask_SUBS MOD Xsiz
  mask_DIST   = SQRT((mask_SUBS_X-Xsiz/2)^2 + (mask_SUBS_Y-Xsiz/2)^2)

  nba = 30

  FOR ii=nba,1,-1 DO BEGIN
 
   im_OUT_ii = im_OUT*0b
   reg_ii = WHERE(mask_DIST LE ii*(rsun/nba) AND mask_DIST GT (ii-1)*(rsun/nba))
   mom_ii = MOMENT(im_IN[reg_ii],SDEV=sdev_ii)
   lowtresh_ii = mom_ii[0] - 3.5*sdev_ii
   filt = WHERE(im_IN LE lowtresh_ii AND im_IN GT 0.)  
   IF filt[0] NE -1 THEN im_OUT_ii[filt] = 1b
   im_OUT_ii[reg_ii] = im_OUT_ii[reg_ii] + 1b
   fil = WHERE(im_OUT_ii EQ 2b)
   IF fil[0] NE -1 THEN im_OUT[fil]=1b
tvscl,im_OUT
  ENDFOR


RETURN,im_OUT
END

;#######################################################

FUNCTION FIRST_SEG4,im_IN
;adaptative treshold from sample points

  im = im_IN
  xsize=1024L
  xsm  = 84
  nsampx = 11

 ;Moment of original(cleaned) image
  mom=MOMENT(im_IN[WHERE(im_IN)],SDEV=sdev,MDEV=mdev)

  
 ;fill the high and low values not to influence
 ;the statistics
; w1 = WHERE(im_IN GT mom[0]+1.*sdev)
; IF w1[0] NE -1 THEN im[w1] = mom[0]+1.*sdev
 w2 = WHERE(im_IN LT mom[0]-1.*sdev AND im_IN GT 0.)
 IF w2[0] NE -1 THEN im[w2] = mom[0]-1.*sdev  


 ;sample points
  aa  = FINDGEN(nsampx)*xsm + 92
  bb  = INTARR(nsampx,nsampx)
  FOR ii=0,nsampx-1 DO bb[ii,*]=aa
  cc  = TRANSPOSE(bb)
  ind = LONARR(nsampx*nsampx)
  ind = bb + xsize*cc

 ;Dilate original image to avoid limb effect
  usiz   = FIX(xsize*1.1)
  nnul   = WHERE(im_IN)
  nul    = WHERE(im_IN LE 0.)
  under  = CONGRID(im, usiz, usiz)
  under  = under[(usiz-xsize)/2:(usiz-xsize)/2+xsize-1, $
                 (usiz-xsize)/2:(usiz-xsize)/2+xsize-1] 
  under[nnul] = im[nnul]

under[ind]=2000.
tvscl,under
  ;Calculate the stat over each point
  im_res = im_IN*0.
  nbp = N_ELEMENTS(ind) 
  FOR ii = 0,nbp-1 DO BEGIN
    xxi = ind[ii] MOD xsize
    yyi = ind[ii]/xsize
    box = im[xxi-xsm:xxi+xsm-1,yyi-xsm:yyi+xsm-1]
    wbox = WHERE(box,nbox)
    IF nbox GT (4*xsm*xsm)/3. THEN BEGIN 
      momii = MOMENT(box[wbox],SDEV=sdevii,MDEV=mdevii) 
      im_res[xxi,yyi]=sdevii
    ENDIF ELSE im_res[xxi,yyi]=0.
  ENDFOR
  
  wres = WHERE(im_res)
  xx = wres MOD xsize
  yy = wres / xsize
  zz = im_res[wres]
  TRIANGULATE,xx,yy,tri
  im_OUT = TRIGRID(xx,yy,zz,tri,NX=1024,NY=1024,/QUINTIC)
  im_OUT[nul] = 0.


RETURN,im_OUT

END

;#######################################################

FUNCTION GET_UNIFORMITY,im_IN

;imag=im_IN

  Xsiz = (SIZE(im_IN))[1]
  rsun = 420
  ;####
  mask_SUBS   = WHERE(im_IN*0+1)
  mask_SUBS_Y = mask_SUBS /   Xsiz
  mask_SUBS_X = mask_SUBS MOD Xsiz
  mask_DIST   = SQRT((mask_SUBS_X-Xsiz/2)^2 + (mask_SUBS_Y-Xsiz/2)^2)

  reg1 = WHERE(mask_DIST LE (rsun-10) AND mask_DIST GT rsun-100)
  reg2 = WHERE(mask_DIST LE 150 AND mask_DIST GT 0)

;imag[reg1]=0.
;imag[reg2]=0.
;tvscl,imag

 im1=im_IN[reg1]
 im2=im_IN[reg2]

 ;get the properties
 mom1 = MOMENT(im1,sdev=sdev1)
 mom2 = MOMENT(im2,sdev=sdev2)

 ;fill the high and low values
 w1a = WHERE(im1 GT mom1[0]+3.*sdev1)
 IF w1a[0] NE -1 THEN im1[w1a] = mom1[0]+3.*sdev1
 w2a = WHERE(im2 GT mom2[0]+3.*sdev2)
 IF w2a[0] NE -1 THEN im2[w2a] = mom2[0]+3.*sdev2
 w1b = WHERE(im1 LT mom1[0]-3.*sdev1)
 IF w1b[0] NE -1 THEN im1[w1b] = mom1[0]-3.*sdev1
 w2b = WHERE(im2 LT mom2[0]-3.*sdev2)
 IF w2b[0] NE -1 THEN im2[w2b] = mom2[0]-3.*sdev2

 ;get the properties again
 mom1 = MOMENT(im1,sdev=sdev1)
 mom2 = MOMENT(im2,sdev=sdev2)

 ;get the extremums
 index = sdev1/sdev2
 print,sdev1,sdev2

 RETURN,index
END

;#######################################################

;FUNCTION CLEAN_DUST,image,mask
;to remove dust points and strong lines
;
;  ;window,3,xs=1024,ys=1024
;  struct = REPLICATE(1,3,3)
;  struct(1,1) = 0
;
;FOR dd=0,3 DO BEGIN
;
;  im=image  ;(temporary?)
;  res=image ;(temporary?)
;
;  cis  = CONVOL(FLOAT(im),struct)/8.
;  dcis = (FLOAT(cis)+1.)/(FLOAT(im)+1.)
;  dcis(mask)=1.
;
;  mom = MOMENT(im(WHERE(im GT 0.)),SDEV=sdev)
;  lowval = mom(0) - 3.0*sdev
; 
;  tochange = WHERE(dcis GT 1.125 OR dcis LT 0.875,ntc)
;  ;tochange = WHERE(dcis GT 1.1 OR dcis LT 0.9,ntc)
;
;  IF ntc GT 0 THEN BEGIN
;     FOR pp=0,ntc-1 DO BEGIN
;         xxp  = tochange(pp) MOD 1024
;         yyp  = tochange(pp)/1024
;         mbox = im(xxp-2:xxp+2,yyp-2:yyp+2)
;         mbox2= res(xxp-3:xxp+3,yyp-3:yyp+3)
;         tc2  = WHERE(mbox GT lowval,ntc2)
;         IF ntc2 GE 18 THEN BEGIN
;            res(xxp-1:xxp+1,yyp-1:yyp+1)=MEDIAN(mbox2)
;         ENDIF
;     ENDFOR
;  ENDIF
;  image=res ;(temporary?)
; 
;ENDFOR
;
;
;  ;window,4,xs=1024,ys=1024
;  ;tvscl,res
;
;RETURN,res
;END

;#######################################################
;FUNCTION REPLACEVAL,image,indices,mean
;
;   imtmp=image
;   imtmp[indices]=0
;   FOR ii=0L,N_ELEMENTS(indices)-1 DO BEGIN
;      xx=indices[ii] MOD 1024
;      yy=indices[ii] / 1024
;      box=imtmp(xx-2:xx+2,yy-2:yy+2)
;      wbox=WHERE(box,nbox)
;      IF nbox GT 0 THEN image[indices[ii]]= MEAN(box[wbox]) ELSE $
;      image[indices[ii]] = mean
;   ENDFOR
;RETURN,image
;END

;#######################################################

;FUNCTION CLEAN_LINES,image,mask
;###########to remove dust lines
;
; 
;   ;#####Moment to get a treshold value
;   mom = MOMENT(image(WHERE(image)),SDEV=sdev)
;   mean = mom[0]
;   nbpts = 100 ;##higher value if get_boundary instead of thin
;   seuilpts = 100 ;''  ''  ''
;   ;#####nbpts is the number of pixel on the same line
;
;WHILE nbpts GE seuilpts DO BEGIN
;
;   ;#####get a binary image
;   minval = FIX(mean - 0.5*sdev)
;   binim = image LT minval
;   binim[mask]=0b
;   
;   ;#####get the skeleton of all regions
;   ;#####in order to get straight lines for
;   ;#####dust lines and irregular lines for filaments
;   binim=THIN(binim)
;
;   ;#####Other method:
;   ;#####get the boundary of all regions
;   ;#####in order to get right lines for
;   ;#####dust lines and irregular lines for filaments
;  ; binimind=GET_BOUNDARY(WHERE(binim),4,1024,1024)
;  ; binim=binim*0b
;  ; binim[binimind]=1b
;
;
;   ;#####First HOUGH to determine if there
;   ;#####are any lines (at least 100 points)
;   nbpts=MAX(HOUGH(binim))
;
;   IF nbpts GE seuilpts THEN BEGIN
;
;      ;#####Hough transform / treshold / backprojection
;      im = HOUGH(binim,RHO=rho,THETA=theta)
;      imt = (im-nbpts*0.9) > 0
;      imb = HOUGH(imt,/backproject,RHO=rho,THETA=theta)
;
;      ;#####Get the points that should be changed
;      imb[mask] = 0
;      tochange = WHERE(imb)
;      ;#####Enlarge the first set of points to be changed
;      tochange = [tochange,tochange+1024,tochange-1024, $
;                  tochange+1,tochange-1]
;      tochange = tochange[UNIQ(tochange,SORT(tochange))]
;      ;#####Change only points with a low value
;      tochange = tochange[WHERE(image[tochange] LT mean - 0.5*sdev)]
;
;      ;#####Replace the values of the points
;      image = REPLACEVAL(image,tochange,mean)
;
;   ENDIF
;
;;#####continue until there are no more lines
;ENDWHILE
;
;RETURN,image
;END

;#######################################################
;FUNCTION FLAT_IM,im2flt,mask
;
;  xsiz   = (SIZE(im2flt))(1)
;  ysiz   = (SIZE(im2flt))(2)
;  medv   = MEDIAN(im2flt(WHERE(im2flt)))
;  imreb  = REBIN(im2flt, Xsiz/2, Ysiz/2)
;  immed  = MEDIAN(imreb, 30)
;  immed  = REBIN(immed, Xsiz, Ysiz)
;  imflt  = im2flt-immed+1.01*medv
;  imflt(mask) = 0.   
;
;RETURN,imflt   
;END
;

;#######################################################
FUNCTION LIM_CALC,im_ORI,region_SUBS

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
                                ;should be modify because of small filaments
                                ;close to big ones

   distx = ( (dist < xx) < (1024-xx-1) ) < 300
   disty = ( (dist < yy) < (1024-yy-1) ) < 300

   im_WNF  = im_ORI ;### do not include the region in moment computation
   im_WNF[region_subs]=0.
   square1 = im_WNF(xx-distx:xx+distx,yy-disty:yy+disty)
   ;square1 = im_ORI(xx-distx:xx+distx,yy-disty:yy+disty)
   sunsurf = square1(WHERE(square1 GT 1))
   mom     = MOMENT(sunsurf,SDEV=sdev)
   limit   = mom(0) - 1.5*sdev

RETURN,limit

END

;#######################################################

FUNCTION FILAMENT_GR,im_ORI,im_SEG,mask

;APPLY REGION A REGION GROWING FUNCTION TO EACH SEED

  im_ORI[mask]=0.
  im_SEG[mask]=0b
  im_SEED = im_ORI*0b


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

;**********************************************
;##### I #######

;npoints=n_elements(region_subs)
;mask=bytarr(1024,1024)
;mask[region_subs]=1b
;xce= LONG(Total(Total(mask,2)*Indgen(1024))/FLOAT(npoints))
;yce= LONG(Total(Total(mask,1)*Indgen(1024))/FLOAT(npoints))
;region = REGION_GROW(im_ORI,xce+yce*1024L,/ALL_NEIGHBORS,threshold=[mini,maxi]) 

;***********************************************
;##### II #######
;mi=WHERE(im_ORI[region_subs] EQ MIN(IM_HEP[region_subs]))
;region = REGION_GROW(im_ORI,region_subs[mi[0]],/ALL_NEIGHBORS,stddev_multiplier=4.5)

;***********************************************
;##### III #######
;Fix treshold range from the local statisics (stddev_multiplier hazardous)

        region = REGION_GROW(im_ORI,region_subs,/ALL_NEIGHBORS,threshold=[mini,maxi]) 

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


  ;####TEST THE SIZE OF INPUT
;  IF Xsiz NE Ysiz OR Xsiz NE 1024 THEN BEGIN
;     mess = DIALOG_MESSAGE('Not a 1024x1024 array, resize ?',/QUESTION)
;     IF mess EQ 'Yes' THEN BEGIN
;       Xsiz  = 1024
;        Ysiz  = 1024
;        input = CONGRID(input, Xsiz, Ysiz)
;     ENDIF ELSE GOTO,end_filament
    ;ENDIF ELSE RETURN,0
;  ENDIF


  ;#####DISPLAY
  WINDOW,0,XSIZE=Xsiz,YSIZE=Ysiz
  ;SAVE,input,filename='/home/fuller/poub/SAV/image1.sav'
  TVSCL,input


  ;####CREATE SOME MASKS
  mask_SUBS   = WHERE(input*0+1)
  mask_SUBS_Y = mask_SUBS /   Xsiz
  mask_SUBS_X = mask_SUBS MOD Xsiz
  mask_DIST   = SQRT((mask_SUBS_X-Xsiz/2)^2 + (mask_SUBS_Y-Ysiz/2)^2) 
  mask_0      = WHERE(mask_DIST GE (diam/2),nmsk0)
  mask_1      = WHERE(mask_DIST GE (diam/2)*0.99,nmsk1)
  mask_2      = WHERE(mask_DIST GE (diam/2)*0.98,nmsk2)
  mask_3      = WHERE(mask_DIST LE (diam/2)*0.99 AND mask_DIST GE (diam/2)*0.98,nmsk3)



  ;####APPLY A MEDIAN FILTER AND SUBTRACT INPUT BY THE RESULT
;  im_FLT = FLAT_IM(input,mask_1)
;  TVSCL,im_FLT
  ;medv   = MEDIAN(input(WHERE(input)))
  ;im_REB = REBIN(input, Xsiz/2, Ysiz/2)
  ;im_MED = MEDIAN(im_REB, 30)
  ;im_MED = REBIN(im_MED, Xsiz, Ysiz)
  ;im_FLT = input-im_MED+1.01*medv
  ;im_FLT(mask_1) = 0.
  ;####REMOVE DUST LINES WITH THE HOUGH TRANSFORM
;  im_FLT = CLEAN_LINES(im_FLT,mask_2) 
;  TVSCL,im_FLT
  ;####REMOVE THE DUST POINTS
;  im_FLT = CLEAN_DUST(im_FLT,mask_2)
;  TVSCL,im_FLT


  ;####CLEAN THE IMAGE (INTENSITY/LINES/DUST)
   im_FLT = CLEAN_IMAGE(input,diam/2)

tvscl,im_FLT


  ;####ENHANCE THE CONSTRAST WITH HIST_EQUAL
;  im_HEP  = HIST_EQUAL(im_FLT,PERCENT=1) + 1
;  im_HEP(mask_1) = 0
; TVSCL,im_HEP

  ;####TRESHOLD TO FIND THE SEEDS0
;   mome=MOMENT(im_FLT[WHERE(im_FLT)],sdev=sdevflt)
;   tresh=mome[0]-2.5*sdevflt
;   im_SEG=im_FLT LT tresh
;   im_SEG[mask_0]=0b
;   TVSCL,im_SEG

  ;####TRESHOLD TO FIND THE SEEDS1
;  im_SEG = FIRST_SEG(im_HEP,im_FLT,mask_1,mask_2,mask_3)
;  TVSCL,im_SEG

  ;####TRESHOLD TO FIND THE SEEDS2
  im_SEG = FIRST_SEG2(im_FLT)
  TVSCL,im_SEG
  
  ;####REGION GROWING FUNCTION
   res = FILAMENT_GR(im_FLT,im_SEG,mask_2)
;   res = FILAMENT_GR(im_HEP,im_SEG)
;  WINDOW,1,XSIZE=xsiz,YSIZE=ysiz
   TVSCL,res


RETURN,res

end_filament: 
END


;####################################################








