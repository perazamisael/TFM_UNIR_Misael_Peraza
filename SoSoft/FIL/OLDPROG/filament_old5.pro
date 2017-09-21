
FUNCTION STDDEVCALC,image

  xs  = (SIZE(image))(1)
  ys  = (SIZE(image))(2)
  image2 = image(1:xs-2,1:ys-2)  
  nzero = WHERE(image2,cc)
  tab = FLTARR(cc/4+1) ;take 1pix/4 to go faster
 
  FOR kk=0L,cc-1,4 DO BEGIN
     jj = nzero(kk) /  (xs-2)
     ii = nzero(kk) MOD (xs-2) 
     ii = ii + 1 & jj = jj +1 
     tab(kk/4) = STDDEV(image(ii-1:ii+1,jj-1:jj+1))
  ENDFOR
 
RETURN,MEDIAN(tab)
END

;#######################################################

FUNCTION GROWING,image,subs,std

;///region growing function

       mini = MIN(image(subs)) > 0.1
       maxi = MAX(image(subs)) + (1./std)*8000.

       blob = REGION_GROW(image,subs,threshold=[mini,maxi],/ALL_NEIGHBORS)

       PRINT,(SIZE(blob))(1)

RETURN,blob
END

;#######################################################

FUNCTION FILAMENT_GR,image,std

;///region growing applied to filaments

  xsiz = (SIZE(image))(1)   ;///assume square image
  diam = 840.               ;///disk diameter

;///result image////////////////////////////////
  
  im_seed = image*0.

;///create a mask...////////////////////////////

  subs_mask   = WHERE(image*0 + 1)
  subs_mask_y = subs_mask /   xsiz
  subs_mask_x = subs_mask MOD xsiz
  dist_mask   = SQRT((subs_mask_x-xsiz/2)^2 + (subs_mask_y-xsiz/2)^2) 
  mask = WHERE(dist_mask GE (diam/2)*0.99)

  im_cp = image
  im_cp(mask) = 0.

  ;///make a first segmentation before labelling////

  tresh = MEDIAN(im_cp(WHERE(im_cp)))/1.3
print,tresh
  im_seg = (im_cp LT tresh AND im_cp GT 0.)
tvscl,im_seg
  ;///first labelling///////////////////////////////
 
  im_labl = LABEL_REGION(im_seg)
  hist_labl = HISTOGRAM(im_labl,REVERSE_INDICES=rev)

  nb_seeds = N_ELEMENTS(hist_labl)
  PRINT,'nbre de seeds',nb_seeds

;///loop on each seed/////////////////////////////////


FOR ii=0, N_ELEMENTS(hist_labl)-1 DO BEGIN

   IF hist_labl(ii) GT 1 AND hist_labl(ii) LT 100000 THEN BEGIN

     region_subs = rev(rev[ii]:rev[ii+1]-1)  ;///subscripts of labelled region 

       ;///call the growing region function//////////// 

       region = GROWING(im_cp,region_subs,std)
       PRINT,'region points:',N_ELEMENTS(region)

       ;///eliminate the region if number of points to small
       IF N_ELEMENTS(region) GT 20 THEN im_seed(region) = 1

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

  image  = input;HIST_EQUAL(input,BINSIZE=1,PERCENT=1)
  window,2,xsize=1024,ysize=1024
  TVSCL,image

  ;LIMIT CALCULATION
  std3x3 = STDDEVCALC(image)

  ;REGION GROWING FUNCTION
  res = FILAMENT_GR(image,std3x3)
  TVSCL,res

RETURN,res

END


;####################################################






