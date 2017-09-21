
;\\\\\\\\\\\ MAIN /////////////

PRO filament

  folder = '/home/fuller/poub/'
  file = DIALOG_PICKFILE(path=folder,filter='*.sav')

  RESTORE,file
  DET_COEFF,arr
  
  res = FILAMENT_GR(arr,/SB_IM)

  window,2,xsize=1024,ysize=1024
  tvscl,res

END

;############################################################################

PRO DET_COEFF,im
;///List of coeff used

COMMON EDGESTUFF,mi_edge
COMMON COEFFS,coeff1,coeff2,coeff3,coeff4,coef4b,coeff5,coeff6
   
   coeff1 = 1.00       ;tresh to find the seed points
   coeff2 = 1.02      ;tresh for the region growing func
   coeff3 = 0.95       ;portion of the disk where seeds are searched
   coeff4 = 0.95       ;portion of the disk where pixels are searched
   coef4b = 0.99       ;portion of the disk where pixels are searched(sobel)
   coeff5 = 1.08       ;tresh for the region growing func associate with sobel
   coeff6 = 2.50       ;coeff for the sobel detection

END

;############################################################################

FUNCTION ADJ,image2,orig,mi,ma,xs,EDGEMAP=edgemap

COMMON EDGESTUFF,mi_edge
COMMON COEFFS,coeff1,coeff2,coeff3,coeff4,coef4b,coeff5,coeff6

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
COMMON COEFFS,coeff1,coeff2,coeff3,coeff4,coef4b,coeff5,coeff6

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

ENDWHILE

RETURN,blob
END


;############################################################################

FUNCTION filament_gr,im,SB_IM=sb_im

COMMON EDGESTUFF,mi_edge
COMMON COEFFS,coeff1,coeff2,coeff3,coeff4,coef4b,coeff5,coeff6

;///region growing applied to filaments

  xsiz = (SIZE(im))(1)  ;///assume square image
  diam = 840            ;///disk diameter

;///result image////////////////////////////////
  
  im_seed = im*0.
  ;or; im_seed = HIST_EQUAL(im,percent=1)

;///create a mask...////////////////////////////

  im_mask     = im*0.+1.
  subs_mask   = WHERE(im_mask)
  subs_mask_y = FIX(subs_mask/xsiz)
  subs_mask_x = subs_mask - subs_mask_y*xsiz
  dist_mask   = SQRT((subs_mask_x-xsiz/2)^2 + (subs_mask_y-xsiz/2)^2) 

  ;///to eliminate the seeds too far from disk center

  mask = WHERE(dist_mask GE (diam/2)*coeff3)

  ;///to eliminate the few pixels of the limb to avoid false detection

  mask2 = WHERE(dist_mask GE (diam/2)*coeff4)
  im_cp = im
  im_cp(mask2) = 0.

;///smoothed image//////////////////////////////////

  ims = SMOOTH(im,3)

;///Define a treshold with a maximum amount of seeds

  nb_seeds = 100
  tresh = MEDIAN(im_cp(WHERE(im_cp)))

  WHILE nb_seeds GE 100 DO BEGIN

    ;///make a first segmentation before labelling////

    im_seg = im
    im_seg(WHERE(im_cp LT tresh AND im_cp NE 0.,COMPLEMENT=bigv,cc)) = 1.
    im_seg(bigv) = 0.
    im_seg(mask) = 0.

    ;///first labelling///////////////////////////////
 
    im_labl = LABEL_REGION(im_seg)
    hist_labl = HISTOGRAM(im_labl,REVERSE_INDICES=rev)

    nb_seeds = N_ELEMENTS(hist_labl)
    PRINT,'nbre de seeds',nb_seeds

    tresh = tresh/1.01
    print,tresh

  ENDWHILE

;///edge map (sobel)//////////////////////////////////
  
  IF KEYWORD_SET(SB_IM) THEN BEGIN

     im_edge = SOBEL(ims)
     mask3 = WHERE(dist_mask GE (diam/2)*coef4b)
     im_edge(mask3) = 0.
     mi_edge = MEDIAN(im_edge(where(im_edge)))*coeff6

  ENDIF

;///loop on each seed/////////////////////////////////


FOR ii=0, N_ELEMENTS(hist_labl)-1 DO BEGIN

   IF hist_labl(ii) GT 1 AND hist_labl(ii) LT 100000 THEN BEGIN

     region_subs = rev(rev[ii]:rev[ii+1]-1) ;///subscripts of labelled region 
     region_val = im_cp(region_subs)        ;///values of labelled region
     treshmax = MAX(region_val)*coeff2      ;///tresholds for the search
     treshmin = MIN(region_val)/coeff2
     mini = WHERE(region_val EQ MIN(region_val))
     seed = region_subs(mini(0))            ;///seed point subscript
     
     IF im_seed(seed) NE 1 THEN BEGIN

       ;///call the growing region function//////////// 

       IF KEYWORD_SET(SB_IM) THEN BEGIN
         region = GROWING(im_cp,seed,treshmin,treshmax,EDGE=im_edge)
       ENDIF ELSE BEGIN
         region = GROWING(im_cp,seed,treshmin,treshmax)
       ENDELSE
       PRINT,'region points:',N_ELEMENTS(region)

       ;///eliminate the region if number of points to small
       IF N_ELEMENTS(region) GT 5 THEN im_seed(region) = 1

     ENDIF

   ENDIF

ENDFOR

RETURN,im_seed

END





