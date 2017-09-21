;######dose not work well with 10 Jan 2002 from Meudon
;+
; NAME:
;       efr_fil_region_grow_hc
;
; PURPOSE:
;
;       From the 2 input arrays (orginal image and seeds locations),
;       grow the seeds to filaments
;       This version is simpler, it uses rhe histogram cumul of the
;       whole image
;    
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
;        res = efr_fil_region_grow_hc(imori,imseed,diam)
;
; INPUTS
;
;        imori : input n x n array (original image)
;        imseed: input n x n array (seeds = 1, other = 0) 
;        diam  : Sun diameter in pixels
;
;
; OUTPUTS:
;
;       segmented image - n x n array (filament pixels are set to 1
;                         and others to 0)  
;
;
;
; MODIFICATION HISTORY:
;
;-


;###########################################################################

FUNCTION EFR_FIL_REGION_GROW_HC,imori,imseed,diam

;###########################################################################


  imgrow   = imseed*0b
  xsize    = (SIZE(imori))[1]
  sunsurf  = !pi*(LONG(diam)/2)^2
  limsize1 = FIX(sunsurf/55E3)>1
  limsize2 = 3*limsize1

  ;####FIRST LABELING 
  im_LABL   = LABEL_REGION(imseed)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_SEED   = N_ELEMENTS(hist_LABL)

  ;####HISTOGRAM CUMUL
  cum = HIST_EQUAL(imori[WHERE(imori)],bin=1,minv=1,/hist)

  ;####THRESHOLD MAX VALUE
  ttr = (WHERE(cum GT 0.045*sunsurf))[0]


  ;####LOOP ON EACH SEED REGION
  FOR ii=0, nb_SEED-1 DO BEGIN

    ;####SIZE RANGE OF ACCEPTABLE SEEDS
    IF hist_LABL(ii) GT limsize1 AND hist_LABL(ii) LT 10E3 THEN BEGIN

     ;####SUBSCRIPTS OF LABELLED REGIONS     
     region_SUBS = rev(rev[ii]:rev[ii+1]-1) 

     ;####CHECK IF THE REGION HAS ALREADY BEEN DETECTED 
     seen_REG = WHERE(imgrow[region_SUBS] EQ 1, s_r)

     IF s_r NE N_ELEMENTS(region_SUBS) THEN BEGIN

        mini = 1./10E3
        maxi = ttr 

        ;##### APPLY REGION_GROW FUNCTION 

        region = REGION_GROW(imori,region_SUBS,/ALL_NEIGHBORS,THRESHOLD=[mini,maxi]) 
 

        ;##### DISPLAY RESULTS
        PRINT,'region',FIX(ii),'/',STRTRIM(nb_SEED,2)
        PRINT,'region points:',N_ELEMENTS(region)
        PRINT,'###########################'


        ;####SIZE RANGE OF ACCEPTABLE REGIONS
        IF N_ELEMENTS(region) GT limsize2 AND N_ELEMENTS(region) LT 10E3 THEN imgrow[region] = 1b

     ENDIF

    ENDIF

  ENDFOR

RETURN,imgrow

END


