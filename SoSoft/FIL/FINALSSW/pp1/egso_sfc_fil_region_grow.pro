
;+
; NAME:
;       egso_sfc_fil_region_grow
;
; PURPOSE:
;
;       From the 2 input arrays (original image and seeds locations),
;       grow the seeds regions to filaments and return a byte array
;       with filament pixels set to 1 and other to 0. See also EGSO_SFC_FILAMENT,
;       EGSO_SFC_FIL_SEEDS... 
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
;        res = egso_sfc_fil_region_grow(imori,imseed,diam,coeff)
;
; INPUTS
;
;        imori : input n x n array (original image)
;        imseed: input n x n array (seeds = 1, other = 0) 
;        diam  : Sun diameter in pixels
;        coeff : A coefficient to determine the threshold to the
;                region growing function
;
;
; OUTPUTS:
;
;       segmented image - n x n array (filament pixels are set to 1
;                         and others to 0)  
;
;
; MODIFICATION HISTORY:
;
;  NF Feb 2005 : Last revision
;-

;###########################################################################

FUNCTION EGSO_SFC_FIL_REGION_GROW,imori,imseed,diam,coeff

;###########################################################################



  ;#### Function parameters check
   IF N_PARAMS() NE 4 THEN BEGIN
      PRINT,'EGSO_SFC_FIL_REGION_GROW calling example: res=EGSO_SFC_FIL_REGION_GROW(im_ori,im_seeds,840,2.1)'
      RETALL
  ENDIF


  ;#### Define the resulting byte array
   imgrow   = imseed*0b


  ;#### Get image size
   xsize    = (SIZE(imori))[1]
   ysize    = (SIZE(imori))[2]
 
 
  ;#### Define limit size of seeds and grown regions
   sunsurf   = !pi*(LONG(diam)/2)^2
   limsize1  = FIX(sunsurf/55E3)>1
   limsize1b = FIX(sunsurf/50)
   limsize2  = FIX(sunsurf/18E3)
   limsize2b = FIX(sunsurf/20)
 

  ;#### Labeling of seeds
   im_LABL   = LABEL_REGION(imseed,/all)
   hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
   nb_SEED   = N_ELEMENTS(hist_LABL)


  ;#### Limit the detection near the limb
   rndmask = EGSO_SFC_ROUNDMASK(xsize,ysize,0.,diam/2.-5,COMP=mask_comp)
   imori_noext = imori
   imori_noext[mask_comp] = 0.


  ;#### Loop on each seed
   FOR ii=0, nb_SEED-1 DO BEGIN


      ;#### Size range acceptable seeds
       IF hist_LABL[ii] GT limsize1 AND hist_LABL[ii] LT limsize1b THEN BEGIN


           ;#### Subscripts of labelled regions
            region_SUBS = rev[rev[ii]:rev[ii+1]-1] 

           ;#### Compute an intensity limit in a bounding rectangle
           ;#### 1-Define the bounding rectangle from seed size
            max_xx = MAX(region_SUBS MOD xsize,MIN=min_xx)
            max_yy = MAX(region_SUBS / xsize,MIN=min_yy)

           ;#### 2-B.R. center
            xxc   = min_xx + (max_xx - min_xx)/2
            yyc   = min_yy + (max_yy - min_yy)/2

           ;#### 3-Define the size of the BR (nonlinear with a min value)
           ;#### i.e. the BR for a small seed will be proportionnally
           ;#### larger than for a bigger seed
            dist  = 20*SQRT([max_xx-min_xx,max_yy-min_yy]) > diam/30

           ;#### 4-Make sure the B.R. is not too big 
            distx = ( (dist[0] < xxc) < (xsize-xxc-1) ) < diam/2
            disty = ( (dist[1] < yyc) < (xsize-yyc-1) ) < diam/2


           ;#### 5-Compute the threshold in this bounding rectangle
            square1  = imori[xxc-distx:xxc+distx,yyc-disty:yyc+disty]
            mom      = MOMENT(square1[WHERE(square1)],SDEV=sdev) ;(mean or median)
            goodvals  = square1[WHERE(square1 GT mom[0]-1.5*sdev AND square1 LT mom[0]+1.5*sdev)]
            mom      = MOMENT(goodvals,SDEV=sdev)
            lim_REG  = mom[0] - coeff*sdev


           ;#### Check if the region has already been grown 
            seen_REG = WHERE(imgrow[region_SUBS] EQ 1, s_r)
            IF s_r NE N_ELEMENTS(region_SUBS) THEN BEGIN


                  ;#### Define intensity limits for growing process
                   mini = 1./10E3
                   maxi = lim_REG 


                  ;##### Region growing function 
                   region = REGION_GROW(imori_noext,region_SUBS,/ALL_NEIGHBORS,THRESHOLD=[mini,maxi]) 


                  ;##### Display results
                   PRINT,'region',FIX(ii),'/',STRTRIM(nb_SEED,2)
                   PRINT,'region points:',N_ELEMENTS(region)
                   PRINT,'###########################'


                  ;#### Size range of acceptable grown regions
                   IF N_ELEMENTS(region) GT limsize2 AND N_ELEMENTS(region) LT limsize2b THEN imgrow[region] = 1b

 
             ENDIF

         ENDIF

    ENDFOR

RETURN,imgrow

END


