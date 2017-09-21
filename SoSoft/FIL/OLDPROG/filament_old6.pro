
@/home/fuller/IDL/Fit_Ellipse.pro

FUNCTION STDDEVCALC,image

  xs     = (SIZE(image))(1)
  ys     = (SIZE(image))(2)
  image2 = image(1:xs-2,1:ys-2)  
  nzero  = WHERE(image2,cc)
  tab    = FLTARR(cc/2+1)   ;take 1pix/4 to go faster
 
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

FUNCTION LIM_CALC,imag,reg
  

  ftell = FIT_ELLIPSE(reg,CENTER=cent,AXES=axes,NPOINTS=30,XSIZE=1024,YSIZE=1024)
  xx=cent(0)
  yy=cent(1)
  dist=MAX(axes)
  dist=dist*(50/(dist+10)+1)
  IF dist GT 400 THEN dist=400
  IF (xx-dist) GE 0 AND (xx+dist) LT 1024 AND (yy-dist) GE 0 $
                    AND (yy+dist) LT 1024 THEN BEGIN
  
    square = imag(xx-dist:xx+dist,yy-dist:yy+dist)
    hist = HISTOGRAM(square(WHERE(square)),BINSIZE=1,NBINS=256)
    hist(0)=0 & hist(255)=0
    limit=(WHERE(hist EQ MAX(hist)))(0)    
    std = STDDEVCALC(square)
    IF std GT 5. THEN limit = limit*(5./std) ELSE limit=-1
  ENDIF ELSE limit=-1

;print,'limit=',limit

;tvscl,CONGRID(square,400,400)
;wait,1

RETURN,limit
END

;#######################################################

FUNCTION FIND_SEEDS,image

im_seg = image*0b
xsiz   = (SIZE(image))(1)
winsiz = 20
coe    = 4./5. 
ws=128

FOR ii=0,1024/ws-1 DO BEGIN
  FOR jj=0,1024/ws-1 DO BEGIN
      surr=image(ii*ws:(ii+1)*ws-1,jj*ws:(jj+1)*ws-1)
      IF N_ELEMENTS(WHERE(surr)) GT 10 THEN BEGIN
        ;surr=HIST_EQUAL(surr,percent=1)
        ;tresh = 10.
        med_surr = MEDIAN(surr(WHERE(surr)))
        im_seg(ii*ws:(ii+1)*ws-1,jj*ws:(jj+1)*ws-1) = (surr LT coe*med_surr AND surr GT 0.)      
      ENDIF ELSE im_seg(ii*ws:(ii+1)*ws-1,jj*ws:(jj+1)*ws-1)=0
  ENDFOR
ENDFOR

;  FOR ii=winsiz,xsiz-winsiz-1 DO BEGIN
;   FOR jj=winsiz,xsiz-winsiz-1 DO BEGIN
;       IF image(ii,jj) NE 0 THEN BEGIN
;         surr=image(ii-winsiz:ii+winsiz,jj-winsiz:jj+winsiz)
;         med_surr = TOTAL(surr(WHERE(surr)))/N_ELEMENTS(surr(WHERE(surr)))
;         IF surr(winsiz,winsiz) LT coe*med_surr THEN im_seg(ii,jj)=1
;       ENDIF     
;   ENDFOR
;  ENDFOR 

RETURN,im_seg
END

;#######################################################

FUNCTION FILAMENT_GR,orig,image,lim,std

;///region growing applied to filaments

  xsiz = (SIZE(image))(1)   ;///assume square image
  diam = 840.               ;///disk diameter

;///result image////////////////////////////////
  
  im_seed = image*0.

;///create a mask...////////////////////////////

  subs_mask   = WHERE(image*0.+1.)
  subs_mask_y = subs_mask /   xsiz
  subs_mask_x = subs_mask MOD xsiz
  dist_mask   = SQRT((subs_mask_x-xsiz/2)^2 + (subs_mask_y-xsiz/2)^2) 

  ;///to eliminate the few pixels of the limb to avoid false detection

  mask  = WHERE(dist_mask GE (diam/2)*0.98)
  im_cp = image + 1.
  im_cp(mask) = 0.

  ;///make a first segmentation before labelling////

;   im_seg = FIND_SEEDS(orig)
  orig_temp=REBIN(orig,512,512)
  im_med=MEDIAN(orig_temp,30)
  im_med=REBIN(im_med,1024,1024)
  div = orig/(im_med+1)
  div(mask)=0
  im_for_seed=HIST_EQUAL(div)+1 
  im_for_seed(mask)=0
tvscl,im_for_seed
wait,5
  tresh = 3.
  im_seg = (im_for_seed LT tresh AND im_for_seed GT 0.)
tvscl,im_seg

  ;///first labelling///////////////////////////////
 
  im_labl = LABEL_REGION(im_seg)
  hist_labl = HISTOGRAM(im_labl,REVERSE_INDICES=rev)

  nb_seeds = N_ELEMENTS(hist_labl)
  PRINT,'nbre de seeds',nb_seeds

;///loop on each seed/////////////////////////////////


FOR ii=0, nb_seeds-1 DO BEGIN

   IF hist_labl(ii) GT 1 AND hist_labl(ii) LT 100000 THEN BEGIN

     region_subs = rev(rev[ii]:rev[ii+1]-1)  ;///subscripts of labelled region 

     ;/// call a function to calculate the lim

      lim_region = LIM_CALC(im_cp,region_subs)
      IF lim_region NE -1 THEN BEGIN
        ;lim_region = lim_region*(5./std)  
        lim=lim_region
      ENDIF
      print,'lim_region',lim_region

     ;///call the growing region function//////////// 

     seen_reg=WHERE(im_seed(region_subs) EQ 1,s_r)
     IF s_r NE N_ELEMENTS(region_subs) THEN BEGIN

       region = GROWING(im_cp,region_subs,lim)

       PRINT,'region',FIX(ii),'/',FIX(nb_seeds)
       PRINT,'region points:',N_ELEMENTS(region)

       ;///eliminate the region if number of points too small( or too big)
       IF N_ELEMENTS(region) GT 20 AND N_ELEMENTS(region) LT 10000 THEN im_seed(region) = 1

     ENDIF

   ENDIF

ENDFOR

RETURN,im_seed

END

;######################################################


FUNCTION FILAMENT,INPUT=input

  IF NOT KEYWORD_SET(INPUT) THEN BEGIN
    file = DIALOG_PICKFILE(PATH='/home/fuller/poub',FILTER='*_p.sav')
    RESTORE,file
    input = arr
  ENDIF

  image  = HIST_EQUAL(input,BINSIZE=1,PERCENT=1)
  window,2,xsize=1024,ysize=1024
  TVSCL,image

  ;LIMIT CALCULATION
  ;std3x3 = STDDEVCALC(image)
  hist = HISTOGRAM(image,BINSIZE=1)
  hist(0)=0 & hist(255)=0
  hist=SMOOTH(hist,10)
  ma=(WHERE(hist EQ MAX(hist)))(0)
  ;half0=WHERE(hist LE MAX(hist)/2.)
  ;half1=WHERE(half0 LT ma)

  ;half = ma-MAX(half1) 
  ;lim = ma -(std3x3/6.)*half
  lim = ma/2.; *(4.5/std3x3)

  ;REGION GROWING FUNCTION
  res = FILAMENT_GR(input,image,lim,std3x3)
  ;TVSCL,res

  PRINT,'LIM=',lim
 ; PRINT,'STD=',std3x3

RETURN,res

END


;####################################################






