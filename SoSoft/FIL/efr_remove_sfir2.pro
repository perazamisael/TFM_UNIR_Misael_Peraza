;##########################################################
; Remove Small and Faint Isolated Regions (and sunspots)
;##########################################################

;+
; NAME:
;       efr_remove_SFIR2.pro
;
; PURPOSE:
;       remove small and faint isolated regions and potential sunspots
;       from the segmented image upon various criteria: size, mean and min intensity,
;       neighboors,elongation. Each parameter gives a score and the 
;       removal of the region depends on the overall score
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
;       - Quiet Sun value
;
; OUTPUTS:
;
;         segmented image (0 or 1) without small faint and isolated
;         regions as defined in the program
;
;  
; MODIFICATION HISTORY:
;
;    WRITTEN NF 2010 
;    NF 01/2011 ADD A DISPLAY OPTION
;    NF 02/2011 Code cleaning and improvements on parameters 
;-

@efr_shape_lib
FUNCTION efr_remove_sfir2,segim,corrim,DISPLAY=display

  ;####Size of input image
      xsize = (SIZE(segim))[1]
      ysize = (SIZE(segim))[2]

  ;####Copy of input image -> output
      segim2 = segim

  ;####Parameters associated to the "isolated" criteria
      rad = 50*xsize/1024
      mc_str = SHIFT(DIST(2*rad+1),rad,rad) le rad
  
  ;####Index of segmented regions 
      wsegim = WHERE(segim,nsegim)

  ;####Index of Sun disk pixels, and index of out of disk pixels
      wcorrim = WHERE(corrim,ncorrim,COMPLEMENT=zeros)

  ;####Maximum size of the regions which could be discarded
  ;####typically, ncorrim=555000pix -> size limit=650pix
      max_region_size = 0.0012*ncorrim 

  ;####Overall min score to remove a region
      limit_score = 18

  ;####If the ratio of segmented pixels is low (<1%)
  ;####criteria like "isolated" have no meaning
      c5_option = 1
      IF (nsegim*1.)/ncorrim LT 0.01 THEN BEGIN
        limit_score = 14
        c5_option = 0
      ENDIF

  ;####Stats on Quiet Sun
     mask = segim*0b+1b
     mask[zeros]=0b
     mask[wsegim]=0b
     IMAGE_STATISTICS,corrim,MEAN=qsun,MASK=mask,STDDEV=sdev_qsun
     mask = 0

  ;####Stats on segmented values
      ;IMAGE_STATISTICS,corrim[wsegim],MEAN=mean_segim,MIN=min_segim,STDDEV=sdev_segim

  ;####Minimum value of segmented region (mean of mins)
      valsegim = corrim[wsegim]
      valsegim = valsegim[SORT(valsegim)]
      lowsegval = MEAN(valsegim[0:N_ELEMENTS(valsegim)/20.])

  ;####LABEL REGIONS 
     im_LABL   = LABEL_REGION(segim,/all)
     hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
     nb_REG   = N_ELEMENTS(hist_LABL)
  
  ;####LOOP ON EACH REGION
  FOR ii=0, nb_REG-1 DO BEGIN

    ;####SIZE RANGE OF SFIRs
    IF hist_LABL[ii] LT max_region_size THEN BEGIN

       ;####SUBSCRIPTS OF LABELLED REGIONS     
       region_SUBS = rev(rev[ii]:rev[ii+1]-1)  

       ;####Stats on the region:

       ;####Mean of 80% darkest pixels
       valsreg = corrim[region_SUBS]
       valsreg = valsreg[SORT(valsreg)]
       Ireg = MEAN(valsreg[0:N_ELEMENTS(valsreg)*0.8],/NAN)

       ;####Mean of 5% darkest pixels
       minIreg = MEAN(valsreg[0:N_ELEMENTS(valsreg)*0.05],/NAN)
       IF minIreg LE 0 THEN minIreg = 0.
       
       ;###Dilate the region & check if it overlaps with others
       tmpim = segim*0b
       tmpim[region_SUBS]=1b
       dilim = DILATE(tmpim,mc_str)
       wdilim = WHERE(dilim)
       tab = UNIQ([wsegim,wdilim],SORT([wsegim,wdilim]))
       nuniq = N_ELEMENTS(tab)
       nval = N_ELEMENTS([wsegim,wdilim])-hist_LABL[ii]
       IF nval EQ nuniq THEN isol = 1 ELSE isol = 0
       tmpim = 0

       ;####If it overlaps get the subscripts of the overlaped pixels
       IF isol EQ 0 THEN BEGIN
          dilim=dilim+segim
          dilim[region_SUBS]=0b
          wdilim2=WHERE(dilim EQ 2b,n3)
          IF n3 GT 0 THEN overReg = MEAN(corrim[wdilim2]) ELSE overReg = 0
          dilim = 0
       ENDIF ELSE overReg = 0

       ;####Form factor : privilegiate elongated shapes
       ;####Work on a cropped image to speed up the process
       all_x = region_SUBS MOD xsize
       all_y = region_SUBS / xsize
       max_x = MAX(all_x,MIN=min_x)
       max_y = MAX(all_y,MIN=min_y)
       xsize2 = (max_x+1)-(min_x-1)+1
       ysize2 = (max_y+1)-(min_y-1)+1
       region_SUBS2 = xsize2*(all_y - (min_y-1)) + (all_x - (min_x-1))
       ske_gray = EFR_SKELETON(region_SUBS2,xsize2,ysize2,EC=ec)
       elong = FLOAT(N_ELEMENTS(region_SUBS))/((2.*(ec+1))^2)
       
       
       ;####Size index (min index correspond to max size/3. defined above)
       param1 = 10.*(max_region_size/4. - hist_LABL[ii])/(max_region_size/4.) > 0.
       c1 = 1.2
       
       ;####Local mean intensity index (the darkest region get the minimum index)
       param2 = 10.*(Ireg - (qsun-5.*sdev_qsun))/(4.*sdev_qsun)
       c2 = 1.5

       ;####Elongation factor
       ;param3 = (10.*(1.5-elong)/(1.5-0.7) < 10 ) >0
       param3 = ((10.*(1.3-elong)/(2.-1.3) ) > (-10) ) < 0
       c3 = 0.6

      ;####Local min intensity index (the darkest region get the minimum index)
       param4 = 10.*(minIreg - (qsun-7.*sdev_qsun))/(6.*sdev_qsun)
       c4 = 1.2

      ;####Isolated parameter
      ;####if not isolated we consider the range of overlaped pixels
      ;####i.e. it's isolated even if there are neighboors but faint ones
      ;IF isol EQ 0 THEN param5 = 10.*(2*overReg - (lowsegval+qsun))/(qsun - lowsegval) 
      ;IF isol EQ 0 THEN param5 = 100.*(overReg - Ireg)/(Ireg)
      IF isol EQ 0 THEN BEGIN
          IF overReg LT (qsun - 2.*sdev_qsun) THEN param5 = 0 ELSE param5 = 5.   
      ENDIF
      IF isol EQ 1 THEN param5 = 10.
      c5 = 0.5
      IF c5_option EQ 0 THEN c5=0.      

      ;####FINAL SCORE
      ;#### c-parameter denotes the relative importance of the parameter
      ;#### Sum(Ci) = 5
      score = c1*param1 + c2*param2 + c3*param3 +c4*param4 + c5*param5
      IF score GE limit_score THEN segim2[region_SUBS]=0b

      ;####DISPLAY RESULTS OPTION
      IF KEYWORD_SET(DISPLAY) THEN BEGIN
         PRINT,"Size index:",STRTRIM(param1*c1,2)," Mean intensity:",STRTRIM(param2*c2,2)," Elongation:",STRTRIM(param3*c3,2)," Min intensity:",STRTRIM(param4*c4,2)," Isolated index:",STRTRIM(param5*c5,2)," score ",STRTRIM(score,2)
         IF score GE limit_score THEN PRINT, "Sfir removed"
      ENDIF

    ENDIF
    
  ENDFOR
 
RETURN,segim2

END
