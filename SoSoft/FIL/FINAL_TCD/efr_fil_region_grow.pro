
;+
; NAME:
;       efr_fil_region_grow
;
; PURPOSE:
;
;       From the 2 input arrays (original image and seeds locations),
;       grow the seeds to filaments
;
; AUTHOR:
;
;       Fuller Nicolas
;       LESIA / POLE SOLAIRE
;       Observatoire de MEUDON
;       5 place Jules Janssen
;       92190 MEUDON
;       E-mail: nicolas.fuller@obspm.fr
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Cleaning software
;
; CALLING SEQUENCE:
;
;        res = efr_fil_region_grow(imori,imseed,diam,coeff)
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
;- NF Apr 2004 : Add EFR_LIMBSYM
;- NF may 2004 : remove EFR_LIMBSYM (incorporate it in efr_filament)
;                and add EFR_ROUNDMASK
; 

;###########################################################################

FUNCTION EFR_FIL_REGION_GROW,imori,imseed,diam,coeff

;###########################################################################


  imgrow   = imseed*0b
  xsize    = (SIZE(imori))[1]
  ysize    = (SIZE(imori))[2]
  sunsurf  = !pi*(LONG(diam)/2)^2
  limsize1 = FIX(sunsurf/55E3)>1
  limsize2 = FIX(sunsurf/18E3)

;test en ajoutant le THIN a imseed
;qs=MEAN(imori[WHERE(imori)])
;restore,'/data2/fuller/savebinim.sav'
;imseed = imseed + binim
;nnul = WHERE(imseed)
;imseed = imseed * 0b
;imseed[nnul] = 1b
 
  ;####FIRST LABELING 
  im_LABL   = LABEL_REGION(imseed,/all)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_SEED   = N_ELEMENTS(hist_LABL)


  ;####REGION_GROW SHOULD NOT BE RUN ON AN EXTENDED DISK
  rndmask = EFR_ROUNDMASK(xsize,ysize,0.,diam/2.-6,COMP=mask_comp)
  imori_noext = imori
  imori_noext[mask_comp] = 0.


  ;####LOOP ON EACH SEED REGION
  FOR ii=0, nb_SEED-1 DO BEGIN

    ;####SIZE RANGE OF ACCEPTABLE SEEDS
    IF hist_LABL[ii] GT limsize1 AND hist_LABL[ii] LT 10E3 THEN BEGIN

     ;####SUBSCRIPTS OF LABELLED REGIONS     
     region_SUBS = rev(rev[ii]:rev[ii+1]-1) 

     ;####CALCULATE AN INTENSITY LIMIT ON THE NEIGHBORHOOD


      ;####Define a bounding rectangle (with a max and min size)
      max_xx = MAX(region_SUBS MOD xsize,MIN=min_xx)
      max_yy = MAX(region_SUBS / xsize,MIN=min_yy)

      xx   = min_xx + (max_xx - min_xx)/2
      yy   = min_yy + (max_yy - min_yy)/2
      dist  = 20*SQRT([max_xx-min_xx,max_yy-min_yy]) > diam/30

      distx = ( (dist[0] < xx) < (xsize-xx-1) ) < diam/2
      disty = ( (dist[1] < yy) < (xsize-yy-1) ) < diam/2


      ;####Compute the threshold in this bounding rectangle
      square1  = imori[xx-distx:xx+distx,yy-disty:yy+disty]
      mom      = MOMENT(square1[WHERE(square1)],SDEV=sdev) ;mom[0] or median
      ;medv=MEDIAN(square1[WHERE(square1)])
      ;sunsurf  = square1[WHERE(square1 GT medv-1.5*sdev AND square1 LT medv+1.5*sdev)]
      sunsurf  = square1[WHERE(square1 GT mom[0]-1.5*sdev AND square1 LT mom[0]+1.5*sdev)]
      mom      = MOMENT(sunsurf,SDEV=sdev)
      lim_REG  = mom[0] - coeff*sdev

     ;####CHECK IF THE REGION HAS ALREADY BEEN DETECTED 
     seen_REG = WHERE(imgrow[region_SUBS] EQ 1, s_r)

     IF s_r NE N_ELEMENTS(region_SUBS) THEN BEGIN

        mini = 1./10E3
        maxi = lim_REG 

        ;##### APPLY REGION_GROW FUNCTION 

        region = REGION_GROW(imori_noext,region_SUBS,/ALL_NEIGHBORS,THRESHOLD=[mini,maxi]) 

        ;##### DISPLAY RESULTS
        PRINT,'region',FIX(ii),'/',STRTRIM(nb_SEED,2)
        PRINT,'region points:',N_ELEMENTS(region)
        PRINT,'###########################'


        ;####SIZE RANGE OF ACCEPTABLE REGIONS
        IF N_ELEMENTS(region) GT limsize2 AND N_ELEMENTS(region) LT 10E3 THEN imgrow[region] = 1b

        ;####SIZE RANGE OF ACCEPTABLE REGIONS
        ;(and intensity for small ones)
        ;IF N_ELEMENTS(region) GT limsize2 AND N_ELEMENTS(region) LT 10E3 THEN imgrow[region] = 1b
        ;IF N_ELEMENTS(region) LE limsize2 AND N_ELEMENTS(region) GT limsize2/2 AND MEAN(imori_noext[region]) LE 0.7*qs THEN imgrow[region] = 1b
     ENDIF

    ENDIF

  ENDFOR

;test en ajoutant le THIN a imseed
;  imgrow2=imgrow*0b
;  im_LABL   = LABEL_REGION(imgrow,/all)
;  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
;  nb_SEED   = N_ELEMENTS(hist_LABL)
;  FOR ii=0, nb_SEED-1 DO BEGIN
;       region_SUBS = rev(rev[ii]:rev[ii+1]-1) 
;    IF hist_LABL(ii) GE 50 AND hist_LABL(ii) LT 10E3 THEN BEGIN
;       imgrow2[region_SUBS]=1b
;    ENDIF
;    IF hist_LABL(ii) GT 25 AND hist_LABL(ii) LT 50 AND $
;       MEAN(imori_noext[region_SUBS]) LE 0.7*qs THEN BEGIN
;       imgrow2[region_SUBS]=1b
;    ENDIF
;  ENDFOR
;  imgrow=imgrow2

RETURN,imgrow

END


