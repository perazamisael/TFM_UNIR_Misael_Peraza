;\\\\\\\\\\\ MAIN /////////////
PRO filament,file

;RESTORE,'/home/fuller/poub/test_mh_p.sav'

DET_COEFF,file

res1=filament_gr(file,MORPH=2)
res2=filament_gr(file,MORPH=1)
res=res1+res2
toto=WHERE(res NE 0,COMPLEMENT=titi)
res(toto) = 1
res(titi) = 0
mat=[[1,1,1] ,$
     [1,1,1] ,$
     [1,1,1] ]
mat2=[1,1]
;res=MORPH_CLOSE(res,mat)
;res=ERODE(res,mat2)
;res=THIN(res)
window,2,xsize=1024,ysize=1024
tvscl,res

END

;############################################################################

PRO DET_COEFF,im
;///List of coeff used

COMMON EDGESTUFF,mi_edge
COMMON COEFFS,coeff1,coeff2,coeff3,coeff4,coeff5,coeff6
   
   coeff1 = 1.00       ;tresh to find the seed points
   coeff2 = 1.02       ;tresh for the region growing func
   coeff3 = 0.95       ;portion of the disk where seeds are searched
   coeff4 = 0.95       ;portion of the disk where pixels are searched
   coeff5 = 1.10       ;tresh for the region growing func associate with morpho
   coeff6 = 2.80       ;coeff for the morpho_gradient detection

END

;############################################################################

FUNCTION ADJ,image2,orig,mi,ma,xs,EDGEMAP=edgemap

COMMON EDGESTUFF,mi_edge
COMMON COEFFS,coeff1,coeff2,coeff3,coeff4,coeff5,coeff6

;Compute the subscript of pixels within the range
;[mi,ma] around origine (one-adjacency) OR within
;[mi,ma*coeff] AND with edge map range lower than treshold

          ;///subscript of points in the one-adjacency

          pm = orig-xs
          pp = orig+xs
          one_adj = [pm-1,pm,pm+1,orig-1,orig+1,pp-1,pp,pp+1]


          ;///find the points with value between min and max 

          IF NOT KEYWORD_SET(EDGEMAP) THEN BEGIN

             goodpix = WHERE(image2(one_adj) LE ma AND image2(one_adj) GE mi,cc)

          ENDIF ELSE BEGIN

          ;///find the points with value between min and max 
          ;///taking into account edge map values

             ma2 = ma*coeff5
             goodpix = WHERE ( (image2(one_adj) LE ma AND image2(one_adj) GE mi) $
                          OR ( (image2(one_adj) LE ma2 AND image2(one_adj) GE mi) $
                             AND (edgemap(one_adj) GE mi_edge)) ,cc )
          ENDELSE

          IF cc GT 0 THEN pix_subs = one_adj(goodpix) $
          ELSE pix_subs = -1 

RETURN,pix_subs
END

;############################################################################

FUNCTION GROWING,image,subs0,mini,maxi,EDGE=edge
;///region growing function

COMMON EDGESTUFF,mi_edge
COMMON COEFFS,coeff1,coeff2,coeff3,coeff4,coeff5,coeff6
COMMON MAPPY,mappy

  cp_im  = image      ;///image copy
  xsiz   = (SIZE(image))(1)
  subs   = subs0      ;///initial subscript
  blob   = subs0      ;///initial region
  count  = 1          ;///size of initial subscript
  cp_im(subs0) = 0    ;///each subscript found: put 0 

;///while the algorithm still finds points, do ...

WHILE count NE 0 DO BEGIN

  subt = 0
  ii   = 0L
  
  ;///loop on each point found
  
  WHILE ii LT count DO BEGIN 

     ;///call the adjacency function

     IF NOT KEYWORD_SET(EDGE) THEN BEGIN 
       subsi = ADJ(cp_im,subs(ii),mini,maxi,xsiz)
     ENDIF ELSE BEGIN
       subsi = ADJ(cp_im,subs(ii),mini,maxi,xsiz,EDGEMAP=edge)
     ENDELSE

     IF subsi(0) EQ -1 THEN BEGIN
        subt = subt
     ENDIF ELSE BEGIN 
        cp_im(subsi) = 0
        subt = [subt,subsi]
     ENDELSE
     ii=ii+1

  ENDWHILE

  nsubt = N_ELEMENTS(subt)

  IF nsubt GT 1 THEN BEGIN
     subt  = subt(1:nsubt-1) ;///supress the first zero
     blob  = [blob,subt]     ;///add the new points to the blob
     count = (SIZE(subt))(1)
     subs  = subt
  ENDIF ELSE count = 0

;mappy(blob)=mappy(blob)+1
;tvscl,mappy
ENDWHILE

RETURN,blob
END


;############################################################################

FUNCTION filament_gr,im,MORPH=morph

COMMON EDGESTUFF,mi_edge
COMMON COEFFS,coeff1,coeff2,coeff3,coeff4,coeff5,coeff6
COMMON MAPPY,mappy
;///region growing applied to filaments

  xsiz = (SIZE(im))(1)  ;///assume square image
  diam = 840            ;///disk diameter

;///result image
  
;  h_s = HISTO_STRETCH(im) 
;  im_seed = BYTSCL(im,h_s(0),h_s(1)) 
;or; im_seed = HIST_EQUAL(im,percent=1)
  im_seed = im*0.

;///create a mask...

  im_mask     = im*0.+1.
  subs_mask   = where(im_mask)
  subs_mask_y = FIX(subs_mask/xsiz)
  subs_mask_x = subs_mask - subs_mask_y*xsiz
  dist_mask   = SQRT((subs_mask_x-xsiz/2)^2 + (subs_mask_y-xsiz/2)^2) 

  ;///to eliminate the seeds too far from disk center

  mask = WHERE(dist_mask GE (diam/2)*coeff3)

  ;///to eliminate the few pixels of the limb to avoid false detection

  mask2 = WHERE(dist_mask GE (diam/2)*coeff4)

;///smoothed image
  ims = SMOOTH(im,3)
  im_smooth = im;SMOOTH(im,2)
  im_smooth(mask2) = 0.

;///Define a treshold with a maximum amount of seeds
nb_seeds = 100
first_tresh = MEDIAN(im_smooth(WHERE(im_smooth)))
tresh = first_tresh

WHILE nb_seeds GE 100 DO BEGIN

;  coeff1 = coeff1+0.01
;  print,'coeff1=',coeff1

;///first treshold to find the seeds

;  tresh = MEDIAN(im_smooth(WHERE(im_smooth)))/coeff1

;///make a first segmentation before labelling

  im_seg = im
  im_seg(WHERE(im_smooth LT tresh AND im_smooth NE 0.,COMPLEMENT=bigv,cc)) = 1.
  im_seg(bigv) = 0.
  im_seg(mask) = 0.


;///first labelling
 
  im_labl = LABEL_REGION(im_seg)
  hist_labl = HISTOGRAM(im_labl,REVERSE_INDICES=rev)

  nb_seeds = N_ELEMENTS(hist_labl)
  PRINT,'nbre de seeds',nb_seeds

  tresh = tresh/1.01
  print,tresh

ENDWHILE

;///edge map (morph_gradient)
  
  IF KEYWORD_SET(MORPH) THEN BEGIN
     IF morph EQ 1 THEN im_edge = MORPH_GRADIENT(ims,intarr(1,3)+1,/ULONG) ELSE $
                        im_edge = MORPH_GRADIENT(ims,intarr(3,1)+1,/ULONG)
window,1,xsize=1024,ysize=1024
tvscl,im_edge
print,max(im_edge)
print,MEDIAN(im_edge(where(im_edge)))
  ;   cce = cc
  ;   coe = 1.0
  ;   WHILE cce GE cc DO BEGIN
       mi_edge = MEDIAN(im_edge(WHERE(im_edge)))*coeff6

       toto = WHERE(im_edge GE mi_edge,cce)
       imaeur = im_edge*0.
       imaeur(toto) =1.
;window,1,xsize=1024,ysize=1024
;tvscl,imaeur
  ;     coe = coe + 0.01
       print,'cce=',cce,'cc=',cc;,'coe=',coe
  ;   ENDWHILE     

  ENDIF

;///loop on each seed


FOR ii=0, N_ELEMENTS(hist_labl)-1 DO BEGIN

   IF hist_labl(ii) GT 1 AND hist_labl(ii) LT 100000 THEN BEGIN

     region_subs = rev(rev[ii]:rev[ii+1]-1) ;///subscripts of labelled region 
     region_val = im_smooth(region_subs)    ;///values of labelled region
     treshmax = MAX(region_val)*coeff2      ;///tresholds for the search
     treshmin = MIN(region_val)
     mini = WHERE(region_val EQ MIN(region_val))
     seed = region_subs(mini(0))            ;///seed point subscript
     
     IF im_seed(seed) NE 1. THEN BEGIN

       ;///call the growing region function 
       IF KEYWORD_SET(MORPH) THEN BEGIN
         region = GROWING(im_smooth,seed,treshmin,treshmax,EDGE=im_edge)
       ENDIF ELSE BEGIN
         region = GROWING(im_smooth,seed,treshmin,treshmax)
       ENDELSE
       PRINT,'region points:',N_ELEMENTS(region)

       ;///eliminate the region if number of points lower or equal to 1
       IF N_ELEMENTS(region) GT 5 THEN im_seed(region) = 1.

     ENDIF

   ENDIF

ENDFOR

;window,1,XSIZE=xsiz,YSIZE=xsiz
;tvscl,im_seed

RETURN,im_seed

END





;############################################################################

FUNCTION ADJ4,image2,orig,mi,ma,xs

;Compute the subscript of pixels within the range
;[mi,ma] around origine (4-adjacency)

          ;///subscript of points in the 4-adjacency

          tab0 = INDGEN(8) -4 + orig - 4*xs
          four_adj = tab0
          FOR ii = 1,8 DO BEGIN
            tab = ii*xs + tab0 
            four_adj=[four_adj,tab]
          ENDFOR

          ;///find the points with value between min and max 
          goodpix = WHERE(image2(four_adj) LE ma AND image2(four_adj) GE mi,cc)
      
          IF cc GT 0 THEN pix_subs = four_adj(goodpix) $
          ELSE pix_subs = -1 

RETURN,pix_subs
END

;############################################################################

FUNCTION HISTO_STRETCH,image3
;///to increase the contrast

 nbins = 1000
 hist = HISTOGRAM(image3,NBINS=nbins)
 hist(0) = 0
 lim = MAX(hist)/100.
 tt = WHERE(hist EQ MAX(hist))
 tt0=tt & tt1=tt

 WHILE hist(tt0) GE lim AND tt0 GT 0 DO BEGIN
   tt0=tt0-1
 ENDWHILE  

 WHILE hist(tt1) GE lim AND tt1 LT nbins DO BEGIN
   tt1=tt1+1
 ENDWHILE  

 res_val = INTARR(2)
 res_val(0) = tt0*(MAX(image3)/nbins)
 res_val(1) = tt1*(MAX(image3)/nbins)

RETURN,res_val
END