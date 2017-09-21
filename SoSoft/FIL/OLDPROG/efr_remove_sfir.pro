;##########################################################
; Remove Small and Faint Isolated Regions
;##########################################################

;+
; NAME:
;       efr_remove_SFIR.pro
;
; PURPOSE:
;       remove small and faint isolated regions from the segmented image
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
;         output of EFR_FILAMENT.PRO
;        
;
; OUTPUTS:
;
;     
;
;  
; MODIFICATION HISTORY:
;
;    NF 2010 
;-

FUNCTION efr_remove_sfir,segim,corrim

  segim2 = segim
  rad = 25 ;parameter associated to the "isolated" criteria
  mc_str = SHIFT(DIST(2*rad+1),rad,rad) le rad
  
  wsegim = WHERE(segim)
  wcorrim = WHERE(corrim,ncorrim);,COMPLEMENT=zeros)
  
  ;STATS ON QUIET SUN
  ;mask = segim*0b+1b
  ;mask[zeros]=0b
  ;mask[wsegim]=0b
  ;IMAGE_STATISTICS,corrim,MEAN=mean_corrim,MASK=mask
  
  ;####FIRST LABELING 
  im_LABL   = LABEL_REGION(segim,/all)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_REG   = N_ELEMENTS(hist_LABL)
  
  ;####LOOP ON EACH REGION
  FOR ii=0, nb_REG-1 DO BEGIN

    ;####SIZE RANGE OF SFIRs
    IF hist_LABL[ii] LT 4*ncorrim/10E3 THEN BEGIN;-> SMALL

       ;####SUBSCRIPTS OF LABELLED REGIONS     
       region_SUBS = rev(rev[ii]:rev[ii+1]-1)  
       
       ;###DILATE one region and see if it overlaps with another one  
       tmpim = segim*0b
       tmpim[region_SUBS]=1b
       dilim = DILATE(tmpim,mc_str)
       wdilim = WHERE(dilim)
       tab = UNIQ([wsegim,wdilim],SORT([wsegim,wdilim]))
       nuniq = N_ELEMENTS(tab)
       nval = N_ELEMENTS([wsegim,wdilim])-hist_LABL[ii]
       
       ;###Level compared to local background
       dilim[wsegim]=0b
       wdilim2 = WHERE(dilim)
       momi = MOMENT(corrim[region_SUBS],SDEV=sdevi)
       Ireg = momi[0]-sdevi
       print,Ireg,MEAN(corrim[region_SUBS]),Ireg/MEAN(corrim[region_SUBS])
       Izone = MEAN(corrim[wdilim2])
       
       IF Ireg/Izone GT 0.75 THEN BEGIN;-> FAINT
          
	  IF nval EQ nuniq THEN BEGIN ;->ISOLATED
     
              ;####DISCARD FEATURE
              segim2[region_SUBS]=0b
     
          ENDIF ELSE BEGIN ;->not isolated but close region is also faint
	      dilim[wdilim]=1b
	      dilim=dilim+segim
	      dilim[region_SUBS]=0b
	      wdilim3=WHERE(dilim EQ 2b,n3)
	      IF n3 GT 0 AND MEAN(corrim[wdilim3])/Izone GT 0.8 THEN BEGIN
	      
	         ;####DISCARD FEATURE
                  segim2[region_SUBS]=0b
	        
	      ENDIF
	  ENDELSE
       
       ENDIF	
       dilim = 0 
    ENDIF
    
 ENDFOR
 tmpim=0
 
     
RETURN,segim2
END