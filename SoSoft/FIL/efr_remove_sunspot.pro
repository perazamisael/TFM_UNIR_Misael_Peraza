;##########################################################
; Remove potential sunspots
;##########################################################

;+
; NAME:
;       efr_remove_sunspot.pro
;
; PURPOSE:
;       remove potential sunspots, on shape, intensity, location criteria
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
;       HELIO / EGSO (European Grid of Solar Observations)
;
; CALLING SEQUENCE:
;
;       
;
; INPUTS:
;    
;       - output of EFR_FILAMENT.PRO i.e. segmented filament image (0 or 1)
;       - corresponding corrected grayscale image 
;       - cdelt1/cdelt2: arcsec/pixels
;       - cenx/ceny: sun center coordinates in pixels
;       - rsun: sun radius in pixels
;       - date (see efr_fil2ascii for format)
;
; OUTPUTS:
;
;         segmented image (0 or 1) without small faint and isolated
;         regions as defined in the program
;
; OPTIONS:
;
;         DISPLAY: Displays score details
;  
; MODIFICATION HISTORY:
;
;    WRITTEN NF 2010/2011
;    NF feb2011 Add latitude criteria
;-

@efr_shape_lib
FUNCTION EFR_REMOVE_SUNSPOT,segim,corrim,cd1,cd2,cenx,ceny,rsun,date,DISPLAY=display

  segim2 = segim
  limit_score_0 = 20.;this is a minimum, the limit score vary with size

  xsize = (SIZE(segim))[1]
  ysize = (SIZE(segim))[2]

  rad = 20*xsize/1024
  mc_str = SHIFT(DIST(2*rad+1),rad,rad) le rad

  wsegim = WHERE(segim,nsegim)
  wcorrim = WHERE(corrim,ncorrim,COMPLEMENT=zeros)
  corrim_quiet = corrim
  corrim_quiet[wsegim]=0

  ;STATS ON QUIET SUN
  mask = segim*0b+1b
  mask[zeros]=0b
  mask[wsegim]=0b
  IMAGE_STATISTICS,corrim,MEAN=mean_corrim,MASK=mask,STDDEV=sdev_corrim
  ;valscorrim = corrim[SORT(corrim)]
  ;max_corrim=MEAN(valscorrim[N_ELEMENTS(valscorrim)-500:N_ELEMENTS(valscorrim)-1])

  ;FRACTIONNAL YEAR AND SOLAR CYCLE DETERMINATION
  year = FLOAT(STRMID(date,0,4))
  month = FLOAT(STRMID(date,5,7))
  fracdate = year+ (month-0.5)/12.
  cycles_frac = [1954.29,1964.79,1976.45,1986.70,1996.37,2008.95,2020.,2031.]
  ww = WHERE(cycles_frac GT fracdate)
  fracdate0 = cycles_frac[ww[0]-1]

  ;SEGMENTED VALUES
   valsegim = corrim[wsegim]
   mean_segim = MEAN(valsegim)
   valsegim = valsegim[SORT(valsegim)]
   min_segim = MEAN(valsegim[0:(N_ELEMENTS(valsegim)*0.05)<(500*xsize/1024)])
   IMAGE_STATISTICS,valsegim,STDDEV=sdev_segim;remplacer par MAD?

  ;####FIRST LABELING 
  im_LABL   = LABEL_REGION(segim,/all)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_REG   = N_ELEMENTS(hist_LABL)

  ;#### HISTOGRAM OF THE SEGMENTED DATA (IF ENOUGH DATA)
IF N_ELEMENTS(wsegim) GT 5E-3*ncorrim AND nb_REG GT 5 THEN BEGIN
  hist_seg = HISTOGRAM(corrim[wsegim],binsize=1,min=0)
  sm_hist = SMOOTH(hist_seg,10)
  mean_segim = (WHERE(sm_hist EQ MAX(sm_hist)))[0]
ENDIF

IF KEYWORD_SET(DISPLAY) THEN BEGIN
    PRINT,'Scoring Algorithm for sunspot removal (needs at least '+STRTRIM(limit_score_0,2)+' points)'
ENDIF
 
  ;####LOOP ON EACH REGION
  FOR ii=0, nb_REG-1 DO BEGIN

    ;####SIZE RANGE OF POTENTIAL SUNSPOTS
    IF hist_LABL[ii] LT 1.E-3*ncorrim THEN BEGIN;-> ONLY RATHER SMALL REGIONS
    ;typically, ncorrim=555000pix -> size limit=830pix

       ;####SUBSCRIPTS OF LABELLED REGIONS     
       region_SUBS = rev(rev[ii]:rev[ii+1]-1)  
       
       ;####LIMIT SCORE FUNCTION OF SIZE
       IF hist_LABL[ii] GT 2.E-4*ncorrim THEN limit_score = limit_score_0 + 3. ELSE limit_score = limit_score_0
       IF hist_LABL[ii] GT 4.E-4*ncorrim THEN limit_score = limit_score + 3.

       ;####COMPUTE CENTRAL HELIOGRAPHIC COORDS. OF THE REGION
       cen_lat_ind = region_SUBS[hist_LABL[ii]/2]
       reg_cen_hel   = PIX2CARR(cen_lat_ind,xsize,ysize,cd1,cd2,cenx,ceny,rsun,date,FIXL0=90.)
       latitude = reg_cen_hel[0]
       longitude = reg_cen_hel[1] - 90.

       ;###INTENSITY OF 70% DARKEST PIXELS IN REGION
       ;not to account for potential penumbra
       valsreg = corrim[region_SUBS]
       valsreg = valsreg[SORT(valsreg)]
       lowmean = MEAN(valsreg[0:N_ELEMENTS(valsreg)*0.70])
       ;val80p = valsreg[N_ELEMENTS(valsreg)*0.70]

       ;###INTENSITY OF 5% DARKEST PIXELS IN REGION
       minIreg = MEAN(valsreg[0:N_ELEMENTS(valsreg)*0.05])

       ;###DILATE region to get neighborhood index
       tmpim = segim*0b
       tmpim[region_SUBS]=1b
       comp = WHERE(tmpim EQ 0b)
       dilim = DILATE(tmpim,mc_str)
       dilim[region_SUBS]=0b
       wdilim = WHERE(dilim)
       
       ;###NEIGHBORHOOD MAX AND MEAN (plages)
       valszone = corrim_quiet[wdilim]
       valszone = valszone[WHERE(valszone)]
       sdev_zone = STDDEV(valszone)
       valszone = valszone[SORT(valszone)] 
      ; Izone_max = MAX(corrim[wdilim],/NAN)
       Izone_high = MEAN(valszone[N_ELEMENTS(valszone)*0.5:N_ELEMENTS(valszone)-1])

       ;###ELONGATION
       all_x = region_SUBS MOD xsize
       all_y = region_SUBS / xsize
       max_x = MAX(all_x,MIN=min_x)
       max_y = MAX(all_y,MIN=min_y)
       xsize2 = (max_x+1)-(min_x-1)+1
       ysize2 = (max_y+1)-(min_y-1)+1
       region_SUBS2 = xsize2*(all_y - (min_y-1)) + (all_x - (min_x-1))
       ;ske = EFR_SKELETON(region_SUBS2,xsize2,ysize2,EC=ec)
       ;elong = FLOAT(N_ELEMENTS(region_SUBS))/((2.*(ec+1))^2)
  
       ;###Compacity Index (gravelius) (replacement of elongation)
       ;perimeter computation
       bound = EFR_INNER_BOUNDARY(region_SUBS2,xsize2,ysize2,ALLDIR=alldir)
       perim = N_ELEMENTS(bound)
       compac = perim/(2*SQRT(!pi*N_ELEMENTS(region_SUBS)))

       ;### X cross section intensity curve
        mean_y = (max_y+min_y)/2
        w_cross_y = WHERE(all_y EQ mean_y)
        crossx_x = all_x[w_cross_y]
        crossx_y = all_y[w_cross_y]
        ind_cross_x = crossx_x+xsize*crossx_y
        val_cross_x = corrim[ind_cross_x[SORT(ind_cross_x)]]
        nval_cross_x = N_ELEMENTS(val_cross_x)

      ;### Y cross section intensity curve
        mean_x = (max_x+min_x)/2
        w_cross_x = WHERE(all_x EQ mean_x)
        crossy_x = all_x[w_cross_x]
        crossy_y = all_y[w_cross_x]
        ind_cross_y = crossy_x+xsize*crossy_y
        val_cross_y = corrim[ind_cross_y[SORT(ind_cross_y)]]
        nval_cross_y = N_ELEMENTS(val_cross_y)

  ;#### COMPUTE SCORE

      ;### Mean intensity
      param1 = 10.*(mean_segim - lowmean)/(mean_segim-min_segim) < 10
      a1 = 0.6

      ;### Elongation 
      ;param2 = 10 - FIX(10.*(elong-0.3)/(2.-0.3))
      ;a2 =1.

      ;### Compacity
      param2b = 10.*(1.5-compac)/(1.6-0.9) <10
      a2 = 1.2

      ;### Low values
      param3 = 10.*(mean_segim-0.5*sdev_segim-minIreg)/(mean_segim - 0.5*sdev_segim - min_segim) <10
      a3 = 0.6

      ;### Plages
      param4 = 10 - 10.*(mean_corrim+4.*sdev_corrim - Izone_high)/(4.*sdev_corrim) < 10
      a4 = 1.1

      ;### region variations
      ;print,sdev_zone,sdev_corrim
      param5 = 10.*(sdev_zone-sdev_corrim)/(sdev_corrim) <10  
      ;variation are usually greater near the limb (projection effect):
      IF ABS(longitude) GT 60 THEN param5=param5/2.
      a5 = 0.5

      ;### If latitude is greater than a limit which depends on the
      ;### solar cycle phase, give a malus 
      IF fracdate LT fracdate0+3 THEN lim_lat1 = 35 ELSE lim_lat1 = 35-(20./8.)*(fracdate-(fracdate0+3))
      IF ABS(latitude) GT lim_lat1 THEN param6 = -1*(ABS(latitude)-lim_lat1) ELSE param6 = 0
      IF ABS(latitude) GT (lim_lat1+15) THEN param6 = -20

      ;### If longitude is greater than 60 and size is quite big ->malus
      ;### near the limb sunspots which are in the photosphere should bethin
       param7 = 0
       IF ABS(longitude) GT 60 AND hist_LABL[ii] GT 4.E-4*ncorrim THEN param7 = -10
       IF ABS(longitude) GT 70 AND hist_LABL[ii] GT 2.E-4*ncorrim THEN param7 = -10
            
      ;### If central values are not lower than border values it is not
      ;### a sunspot
        param8 = 0
        IF nval_cross_x GE 6 THEN BEGIN
            nfourth = nval_cross_x/4
            border_mean_left = MEAN(val_cross_x[0:nfourth-1])
            border_mean_right = MEAN(val_cross_x[nval_cross_x-nfourth:nval_cross_x-1])
            center_mean = MEAN(val_cross_x[nfourth:(nval_cross_x-nfourth-1)])
            IF center_mean GE border_mean_left OR center_mean GE border_mean_right THEN BEGIN
                param8 = -4
            ENDIF
        ENDIF
        IF nval_cross_y GE 6 THEN BEGIN
            nfourth = nval_cross_y/4
            border_mean_left = MEAN(val_cross_y[0:nfourth-1])
            border_mean_right = MEAN(val_cross_y[nval_cross_y-nfourth:nval_cross_y-1])
            center_mean = MEAN(val_cross_y[nfourth:(nval_cross_y-nfourth-1)])
            IF center_mean GE border_mean_left OR center_mean GE border_mean_right THEN BEGIN
                param8 = param8 - 4
            ENDIF
         ENDIF


      score = a1*param1 + a2*param2b + a3*param3 + a4*param4 + a5*param5 + param6 + param7 + param8

      IF KEYWORD_SET(DISPLAY) THEN BEGIN
         PRINT,"Mean Intensity:",STRTRIM(param1*a1,2)," Elongation:",STRTRIM(param2b*a2,2)," low values:",STRTRIM(param3*a3,2)," Plages:",STRTRIM(param4*a4,2)," Local stddev:",STRTRIM(param5*a5,2)," Latitude malus:",STRTRIM(param6,2)," Longitude&size malus:",STRTRIM(param7,2)," Cross section malus:",STRTRIM(param8,2)," score ",STRTRIM(score,2),"/",STRTRIM(limit_score,2)
         IF score GE limit_score THEN PRINT, "Potential sunspot detected"
      ENDIF

      IF score GE limit_score THEN segim2[region_SUBS]=0b

 IF score GE limit_score THEN print,hist_LABL[ii]

      dilim = 0
   ENDIF

 ENDFOR
 tmpim=0
 
     
RETURN,segim2
END
