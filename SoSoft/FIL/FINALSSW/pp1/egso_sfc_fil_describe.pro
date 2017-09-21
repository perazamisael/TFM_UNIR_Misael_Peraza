;+
; NAME:
;       egso_sfc_fil_describe
;
; PURPOSE:
;
;       From the subscripts of a segmented standardized solar image,
;       describes the
;       features by the mean of their pruned skeleton and boundary
;       (and  other parameters, see below) More explainations could
;       be found in 'Filament Recognition And Image Cleaning On Meudon
;       Halpha Spectroheliograms, Sol. Phys. 2005, Fuller N., Aboudarham J.
;       and Bentley R.B.'
;
;       NOTE1: standardized images from EGSO/cleaning code are
;       1024*1024 images with Sun centered at (511.5,511.5) with a
;       diameter of 840 pixels
; 
;       NOTE2: Segmented image: 1024*1024 binary array with feature pixels
;       set to 1 and all other to 0
;
;       NOTE3: list of extracted parameters for each segmented feature 
;
;       IND:            Indice in the structure
;       GRAV_C_ARCX:    Gravity center X position (arcsecs) (*)
;       GRAV_C_ARCY:    Gravity center Y position (arcsecs) (*)
;       GRAV_C_CAR_LAT: Gravity center Carrington latitude  (*)
;       GRAV_C_CAR_LON: Gravity center longitude            (*)
;       SAMPLECOUNT:    Count of pixels
;       AREA:           Area in square degrees
;       MEAN_INT_RATIO: Mean intensity to Quiet Sun Intensity ratio
;       BRARC_X_LL:     Bounding rectangle lower left corner X pos.(arcsec)
;       BRARC_Y_LL:     Bounding rectangle lower left corner Y pos.(arcsec)
;       BRARC_X_UR:     Bounding rectangle upper right corner X pos.(arcsec)
;       BRARC_Y_UR:     Bounding rectangle upper right corner Y pos.(arcsec)
;       BRPIX_X_LL:     Bounding rectangle lower left corner X pos.(pixels)
;       BRPIX_Y_LL:     Bounding rectangle lower left corner Y pos.(pixels)
;       BRPIX_X_UR:     Bounding rectangle upper right corner X pos.(pixels)   
;       BRPIX_Y_UR:     Bounding rectangle upper right corner Y pos.(pixels)   
;       FEAT_MAX_INT:   Maximum intensity (**)   
;       FEAT_MIN_INT:   Minimum intensity (**)   
;       FEAT_MEAN_INT:  Mean intensity (**)   
;       COD_PIX_X:      Boundary chain code 1st pixel X pos.(pixels)    
;       COD_PIX_Y:      Boundary chain code 1st pixel Y pos.(pixels)     
;       COD_ARC_X:      Boundary chain code 1st pixel X pos.(arcsec)     
;       COD_ARC_Y:      Boundary chain code 1st pixel Y pos.(arcsec)     
;       SKE_LEN_DEG:    Skeleton length in degrees   
;       THICKNESS_PIX:  Thickness in pixels   
;       CURVATURE:      Curvature index 
;       ELONG:          Elongation factor   
;       ORIENTATION:    Orientation (in degrees counterclockwise)   
;       COD_SKE_PIX_X:  Skeleton chain code 1st pixel X pos.(pixels)   
;       COD_SKE_PIX_Y:  Skeleton chain code 1st pixel Y pos.(pixels)    
;       COD_SKE_ARC_X:  Skeleton chain code 1st pixel X pos.(arcsec)    
;       COD_SKE_ARC_Y:  Skeleton chain code 1st pixel Y pos.(arcsec)    
;       CHAIN_CODE:     Boundary chain code   
;       CCODE_LNTH:     Boundary chain code length  
;       CHAIN_CODE_SKE: Skeleton chain code  
;       CCODE_SKE_LNTH: Skeleton chain code length  
;
;       (*)  For filaments the gravity center is replaced by the
;            skeleton center
;       (**) Only available with the image input keyword 
;    
; AUTHOR:
;
;       Fuller Nicolas
;       LESIA / POLE SOLAIRE
;       Observatoire de MEUDON
;       5 place Jules Janssen
;       92190 MEUDON
;       E-mail: nicolas.fuller@obspm.fr
;       copyright (C) 2005 Nicolas Fuller, Observatoire de Paris
;       This program is free software; you can redistribute it and/or modify it under the terms of the
;       GNU General Public License as published by the Free Software Foundation;
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Solar Feature Catalog 
;
; CALLING SEQUENCE:
;
;       str = EGSO_SFC_FIL_DESCRIBE(indices,dateobs[,IMAGE=image
;             CDELT=cdelt,NOCLOSING=noclosing])
;
; INPUT
; 
;       input : - A 1D vector of pixel indices that describe the feature.
;                 For example, the indices may be returned as a result
;                 of the WHERE function (in a 1024*1024 array)
;               - date of observation (i.e. 2002-03-01T08:12:00.000)  
;                 For example the DATE-OBS keyword of the FITS header
;
; OUTPUTS:
;
;       structure defined above
;
;
; INPUT KEYWORDS:
;
;       IMAGE : Original image leading to the segmentation in input 
;
;       CDELT  : Arcsec/pixel ratio, if not set the program computes
;                it with Pb0r function from date input 
;
;       NOCLOSING: Use only if a morphological closing was
;       previously applied to the segmented features. The closing
;       smoothes the features contours. If features contours are too
;       complex the program may return an error message     
;    
;
; CALLING EXAMPLES
;
;    str = EGSO_SFC_FIL_DESCRIBE(WHERE(seg_im),'2002-03-01T08:12:00',
;          IMAGE=cleanim,CDELT=2.30302)
;    str = EGSO_SFC_FIL_DESCRIBE(subs,'2004-05-21 09:15:00')
;
; MODIFICATION HISTORY:
;
;  NF Feb 2005: last revision
;              
;-


FUNCTION EGSO_SFC_FIL_DESCRIBE,indices,dateobs,IMAGE=image,CDELT=cdelt,NOCLOSING=noclosing


  ;#### Function parameters check
   IF N_PARAMS() NE 2 THEN BEGIN
      PRINT,'EGSO_SFC_FIL_DESCRIBE calling example: res=EGSO_SFC_FIL_DESCRIBE(indices,)'
      RETALL
   ENDIF


  ;#### The input subscripts are considered to come
  ;#### from a standardized image: (change if necessary)   
   rsun = 420
   nax1 = 1024
   nax2 = 1024
   cenx = 511.5
   ceny = 511.5

 
  ;#### Define the structure that will contain all processed data
   default = '\N'
   strfil    = {FEATUREPARAMETERS, $
                IND:               default, $
                GRAV_C_ARCX:       default, $
                GRAV_C_ARCY:       default, $
                GRAV_C_CAR_LAT:    default, $
                GRAV_C_CAR_LON:    default, $
                SAMPLECOUNT:       default, $
                AREA:              default, $ 
                MEAN_INT_RATIO:    default, $
                BRARC_X_LL:        default, $
                BRARC_Y_LL:        default, $
                BRARC_X_UR:        default, $
                BRARC_Y_UR:        default, $
                BRPIX_X_LL:        default, $
                BRPIX_Y_LL:        default, $
                BRPIX_X_UR:        default, $
                BRPIX_Y_UR:        default, $
                FEAT_MAX_INT:      default, $
                FEAT_MIN_INT:      default, $
                FEAT_MEAN_INT:     default, $
                COD_PIX_X:         default, $
                COD_PIX_Y:         default, $
                COD_ARC_X:         default, $
                COD_ARC_Y:         default, $
                SKE_LEN_DEG:       default, $
                THICKNESS_PIX:     default, $
                CURVATURE:         default, $
                ELONG:             default, $
                ORIENTATION:       default, $
                COD_SKE_PIX_X:     default, $
                COD_SKE_PIX_Y:     default, $
                COD_SKE_ARC_X:     default, $
                COD_SKE_ARC_Y:     default, $
                CHAIN_CODE:        default, $
                CCODE_LNTH:        default, $
                CHAIN_CODE_SKE:    default, $
                CCODE_SKE_LNTH:    default }
   B = strfil


  ;#### Compute cdelt1 and cdelt2 parameters if not in input
   IF NOT KEYWORD_SET(CDELT) THEN BEGIN
     angles = PB0R(dateobs,/ARCS)
     cd1 = angles[2]/FLOAT(rsun)
     cd2 = cd1
     PRINT,'EGSO_SFC_FIL_DESCRIBE: Computed cdelt (arcsec/pix):',STRTRIM(cd1,2)
   ENDIF ELSE BEGIN
     cd1 = cdelt
     cd2 = cd1
   ENDELSE 


  ;#### Compute Quiet Sun Intensity if required
   IF KEYWORD_SET(IMAGE) THEN BEGIN
     hist = MEDIAN(HISTOGRAM(image[WHERE(image)],MIN=0.,BIN=1),10)
     maxh = MAX(hist)
     qsi  = (WHERE(hist EQ maxh))[0]
   ENDIF


  ;#### Draw the filament binary image
   mask = BYTARR(nax1,nax2)
   mask[indices] = 1b


  ;#### Link close blobs and fill holes with a closing operator
   IF NOT KEYWORD_SET(NOCLOSING) THEN BEGIN
     rad = 5
     StrEl= SHIFT(DIST(2*rad+1),rad,rad) LE rad
     mask = MORPH_CLOSE(TEMPORARY(mask),StrEl)
   ENDIF ELSE BEGIN
     PRINT,'EGSO_SFC_FIL_DESCRIBE: use NOCLOSING keyword only if a morphological closing has already been applied to the segmented image'
   ENDELSE


  ;#### Label the blobs to get subscripts of each one
  ;#### (first one is background)
   r_l   = LABEL_REGION(mask,/all_neigh)
   h_r_l = HISTOGRAM(r_l,reverse_indices=r)


  ;#### Loop on each blob
   cmpt  = 0
   FOR i = 1, N_ELEMENTS(h_r_l)-1 DO BEGIN

       
      ;#### Get the blob subscripts
       indi  = r[r[i]:r[i+1]-1]


      ;#### Compute intensity stuff
       IF KEYWORD_SET(IMAGE) THEN BEGIN
         meanval  = MEAN(image[indi])
         minval   = MIN(image[indi],MAX=maxval)
         mir      = meanval / qsi
       ENDIF ELSE BEGIN
         meanval  = '/N'
         minval   = '/N'
         mir      = '/N'
         maxval   = '/N' 
       ENDELSE 


      ;#### Create a box that will contain the fil.
      ;#### to compute the parameters faster (+10 pixels on each side)
       coordX = indi MOD nax1
       coordY = indi / nax1
       minX   = MIN(coordX,MAX=maxX)
       minY   = MIN(coordY,MAX=maxY)
       xsizeb = maxX-minX+20
       ysizeb = maxY-minY+20
       boxind = (coordY - (minY-10))*xsizeb + coordX - (minX-10)
       box = BYTARR(xsizeb,ysizeb)
       box[boxind] = 1b


      ;#### Give a bounding rectangle in pixels
       brp = [minX,minY,maxX,maxY]


      ;#### Give a bounding rectangle in arcsecs
       mmX = DOUBLE(cd1*([minX,maxX]-cenx))
       mmY = DOUBLE(cd2*([minY,maxY]-ceny))          
       bra = [mmX[0],mmY[0],mmX[1],mmY[1]]


      ;#### Check if the blob inside box
      ;#### contains holes and fill them
       ;boxind = EFR_CHECK_FILLING(boxind,xsizeb,ysizeb)
       boxind = EGSO_SFC_LAKE2BAY(boxind,xsizeb,ysizeb)
       box[boxind] = 1b


      ;#### Compute the skeleton
      ;#### ec is the number of erosion needed to completly
      ;#### erode the structure 
       ske_ind = EGSO_SFC_SKELETON(boxind,xsizeb,ysizeb,EC=ec)
       ske_ind = EGSO_SFC_M_CONNECT(ske_ind,xsizeb,ysizeb)
       ske_ind = EGSO_SFC_PRUNING(ske_ind,xsizeb,ysizeb)   
       ske_ind = EGSO_SFC_M_CONNECT(ske_ind,xsizeb,ysizeb)
       ske_ind = EGSO_SFC_ORDER_IND(ske_ind,xsizeb,ysizeb)


     IF N_ELEMENTS(ske_ind) GT 3 THEN BEGIN

        cmpt = cmpt + 1

       ;#### Boundary: first test the minimum thickness of the blob
        r_lbox = LABEL_REGION(box) ;with 4 connected regions
        nregb  = N_ELEMENTS(HISTOGRAM(r_lbox))


       ;#### If there are more than 2 regions (+backgr) then
       ;#### there are 1 pixel thick places -> dilate the blob
       ;#### and get the inner boundary
       ;#### If not get the external boundary
        IF nregb EQ 2 THEN BEGIN
           bound_ind  = EGSO_SFC_OUTER_BOUNDARY(boxind,xsizeb,ysizeb)  
        ENDIF ELSE BEGIN
           boxdilateind = WHERE(DILATE(box,REPLICATE(1,3,3)))
          ;#### Make sure we didn't create a bay object with the dilation
           boxdilateind = EGSO_SFC_LAKE2BAY(boxdilateind,xsizeb,ysizeb)
           bound_ind    = EGSO_SFC_INNER_BOUNDARY(boxdilateind,xsizeb,ysizeb)
        ENDELSE
        bound_ind  = EGSO_SFC_M_CONNECT(bound_ind,xsizeb,ysizeb)        
        bound_ind  = EGSO_SFC_ORDER_IND(bound_ind,xsizeb,ysizeb)


       ;#### Get the first points for the chain codes       
        ske_chain_strt  = ske_ind[0]
        bnd_chain_strt  = bound_ind[0]


       ;#### Compute the chain codes
        ske_chain  = EGSO_SFC_CHAIN_CODE(ske_ind,xsizeb,ysizeb)
        bnd_chain  = EGSO_SFC_CHAIN_CODE(bound_ind,xsizeb,ysizeb)


       ;#### Get the blob orientationin deg counter-clockwise
       ;#### from the X axis
        orien = EGSO_SFC_DFORIENTATION(boxind,xsizeb,ysizeb)


       ;#### Give the first points of chain codes in pixel coordinates
        ske_chain_strt_pix = [ske_chain_strt MOD xsizeb + minX - 10 ,$
                              ske_chain_strt /   xsizeb + minY - 10]
        bnd_chain_strt_pix = [bnd_chain_strt MOD xsizeb + minX - 10 ,$
                              bnd_chain_strt /   xsizeb + minY - 10]


       ;#### In arcsecs
        ske_chain_strt_arcs = [DOUBLE(cd1*(ske_chain_strt_pix[0]-cenx)),$
                               DOUBLE(cd2*(ske_chain_strt_pix[1]-ceny))]
        bnd_chain_strt_arcs = [DOUBLE(cd1*(bnd_chain_strt_pix[0]-cenx)),$
                               DOUBLE(cd2*(bnd_chain_strt_pix[1]-ceny))]


       ;#### Get the skeleton indices for the full size image
        Xske_ind = ske_ind MOD xsizeb
        Yske_ind = ske_ind / xsizeb
        ske_ind_full = (Yske_ind + minY - 10)*nax1 + (Xske_ind + minX - 10)
        ske_ind_full = EGSO_SFC_ORDER_IND(ske_ind_full,nax1,nax2) 


       ;#### Get the boundary indices for the full size image
        Xbnd_ind = bound_ind MOD xsizeb
        Ybnd_ind = bound_ind / xsizeb
        bnd_ind_full = (Ybnd_ind + minY - 10)*nax1 + (Xbnd_ind + minX - 10)
        bnd_ind_full = EGSO_SFC_ORDER_IND(bnd_ind_full,nax1,nax2) 


       ;#### Get the filament area in deg2
        area_deg2 = EGSO_SFC_AREA_DEG2(bnd_ind_full,nax1,nax2,cd1,cd2, $
                                                   cenx,ceny,rsun,dateobs) 



       ;#### Get the skeleton length (pixel) and center
        ske_len_pix = EGSO_SFC_LENGTH_PIX(ske_ind,xsizeb,ysizeb,SMP=smp)
        ske_cen_pix = [smp MOD xsizeb+minX-10,smp/xsizeb+minY-10]


       ;#### Get the skeleton center in arcs
        ske_cen_arc = [DOUBLE(cd1*(ske_cen_pix[0]-cenx)),$
                       DOUBLE(cd2*(ske_cen_pix[1]-ceny))]


       ;#### Get the skeleton length in degrees and center in carrington
        ske_len_deg = EGSO_SFC_LENGTH_DEG(ske_ind_full,nax1,nax2,cd1,cd2,$
                      cenx,ceny,rsun,dateobs)
        ske_cen_car = EGSO_SFC_PIX2CARR([LONG(ske_cen_pix[0]) + $
                      LONG(ske_cen_pix[1])*nax1],nax1,nax2,cd1, $
                      cd2,cenx,ceny,rsun,dateobs)


       ;#### Blob area in pixels
        area_pix = N_ELEMENTS(indi)


       ;#### Thickness in pixels
        thick = ec


       ;#### Elongation
        elong = FLOAT(area_pix)/((2.*ec)^2)


       ;#### Compute an indice of the curvature (between 0 and 10)
       ;#### if elong is bigger than 1.
        IF elong GE 1. THEN BEGIN
          resampl_ske = EGSO_SFC_RESAMPLE_IND(ske_ind,5) ;(necessary?faster?)
          curv        = EGSO_SFC_CURL_IND(resampl_ske,xsizeb,ysizeb)
        ENDIF ELSE curv = -1


       ;#### Define the string format of chain codes
        nbndc = N_ELEMENTS(bnd_chain)
        nskec = N_ELEMENTS(ske_chain)
        bndform = '('+STRTRIM(nbndc,2)+'I1)'
        skeform = '('+STRTRIM(nskec,2)+'I1)'


       ;#### Create a structure / fill it's fields
        A = strfil

        A.IND                   = STRTRIM(cmpt,2)
        A.GRAV_C_ARCX           = STRTRIM(ske_cen_arc[0],2)
        A.GRAV_C_ARCY           = STRTRIM(ske_cen_arc[1],2)
        A.GRAV_C_CAR_LAT        = STRTRIM(ske_cen_car[0],2)
        A.GRAV_C_CAR_LON        = STRTRIM(ske_cen_car[1],2)
        A.SAMPLECOUNT           = STRTRIM(area_pix,2)
        A.AREA                  = STRTRIM(area_deg2,2)
        A.MEAN_INT_RATIO        = STRTRIM(mir,2)
        A.BRARC_X_LL            = STRTRIM(bra[0],2)
        A.BRARC_Y_LL            = STRTRIM(bra[1],2)
        A.BRARC_X_UR            = STRTRIM(bra[2],2)
        A.BRARC_Y_UR            = STRTRIM(bra[3],2)
        A.BRPIX_X_LL            = STRTRIM(brp[0],2)
        A.BRPIX_Y_LL            = STRTRIM(brp[1],2)
        A.BRPIX_X_UR            = STRTRIM(brp[2],2)
        A.BRPIX_Y_UR            = STRTRIM(brp[3],2)
        A.FEAT_MAX_INT          = STRTRIM(maxval,2)
        A.FEAT_MIN_INT          = STRTRIM(minval,2)
        A.FEAT_MEAN_INT         = STRTRIM(meanval,2)
        A.COD_PIX_X             = STRTRIM(bnd_chain_strt_pix[0],2)
        A.COD_PIX_Y             = STRTRIM(bnd_chain_strt_pix[1],2)
        A.COD_ARC_X             = STRTRIM(bnd_chain_strt_arcs[0],2)
        A.COD_ARC_Y             = STRTRIM(bnd_chain_strt_arcs[1],2)
        A.SKE_LEN_DEG           = STRTRIM(ske_len_deg,2)
        A.THICKNESS_PIX         = STRTRIM(thick,2)
        A.CURVATURE             = STRTRIM(curv,2)
        A.ELONG                 = STRTRIM(elong,2)
        A.ORIENTATION           = STRTRIM(orien,2)
        A.COD_SKE_PIX_X         = STRTRIM(ske_chain_strt_pix[0],2)
        A.COD_SKE_PIX_Y         = STRTRIM(ske_chain_strt_pix[1],2)
        A.COD_SKE_ARC_X         = STRTRIM(ske_chain_strt_arcs[0],2)
        A.COD_SKE_ARC_Y         = STRTRIM(ske_chain_strt_arcs[1],2)
        A.CHAIN_CODE            = STRING(bnd_chain,FORMAT=bndform)
        A.CCODE_LNTH            = STRTRIM(nbndc,2)
        A.CHAIN_CODE_SKE        = STRING(ske_chain,FORMAT=skeform)
        A.CCODE_SKE_LNTH        = STRTRIM(nskec,2)

        B=[B,A]

     ENDIF

   ENDFOR

   B=B[1:N_ELEMENTS(B)-1]

RETURN,B

END




