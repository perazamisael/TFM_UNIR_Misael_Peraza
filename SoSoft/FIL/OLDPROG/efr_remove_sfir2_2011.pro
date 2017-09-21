;##########################################################
; Remove Small and Faint Isolated Regions
;##########################################################

;+
; NAME:
;       efr_remove_SFIR2.pro
;
; PURPOSE:
;       remove small and faint isolated regions from the segmented
;       image
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
;-

@efr_shape_lib
FUNCTION efr_remove_sfir2,segim,corrim,DISPLAY=display

  xsize = (SIZE(segim))[1]
  ysize = (SIZE(segim))[2]

  segim2 = segim
  rad = 25*xsize/1024 ;parameter associated to the "isolated" criteria
  mc_str = SHIFT(DIST(2*rad+1),rad,rad) le rad
  rad2 = 50*xsize/1024 ;parameter associated to the "isolated" criteria
  mc_str2 = SHIFT(DIST(2*rad2+1),rad2,rad2) le rad2
 
  
  wsegim = WHERE(segim,nsegim)
  wcorrim = WHERE(corrim,ncorrim,COMPLEMENT=zeros)
  
  ;STATS ON QUIET SUN
  mask = segim*0b+1b
  mask[zeros]=0b
  mask[wsegim]=0b
  IMAGE_STATISTICS,corrim,MEAN=mean_corrim,MASK=mask,STDDEV=sdev_corrim
  ;print,sdev_corrim,mean_corrim
  
  ;SEGMENTED VALUES
   IMAGE_STATISTICS,corrim[wsegim],MEAN=mean_segim,STDDEV=sdev_segim
;   ratio = mean_corrim/mean_segim
;   concentr_segim = mean_segim/N_ELEMENTS(wsegim)
;   print,ratio 

  ;####FIRST LABELING 
  im_LABL   = LABEL_REGION(segim,/all)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_REG   = N_ELEMENTS(hist_LABL)

  ;test histogram pour sunspots removal
;IF N_ELEMENTS(wsegim) GT 5E-3*ncorrim AND nb_REG GT 10 THEN BEGIN
;  remove_sunspots = 1
;  hist_seg = HISTOGRAM(corrim[wsegim],binsize=1,min=0)
 ; window,11
;  sm_hist = SMOOTH(hist_seg,10)
;  plot,sm_hist
;  mean_segim = (WHERE(sm_hist EQ MAX(sm_hist)))[0]
;  ratio=mean_corrim/mean_segim
;  wset,10
;ENDIF ELSE BEGIN
;  remove_sunspots = 0
;ENDELSE


IF KEYWORD_SET(DISPLAY) THEN BEGIN
    PRINT,'Conditional removal of small, faint and isolated regions (limit score:2.0):'
    PRINT,'area score, intensity score, neighboors int. score, isolated score, really isolated score, elongated bonus,  sum'
ENDIF
  
  ;####LOOP ON EACH REGION
  FOR ii=0, nb_REG-1 DO BEGIN

    ;####SIZE RANGE OF SFIRs
    IF hist_LABL[ii] LT 1.2E-3*ncorrim THEN BEGIN;-> ONLY RATHER SMALL REGIONS
    ;typically, ncorrim=555000pix -> size limit=830pix

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
       IF nval EQ nuniq THEN isol = 1 ELSE isol = 0
       
       ;###Level compared to local background
       dilim[wsegim]=0b
       dilim[zeros]=0b
       wdilim2 = WHERE(dilim)
       Ireg = MEAN(corrim[region_SUBS],/NAN)
       ;;minIreg = MIN(corrim[region_SUBS])
       ;IF minIreg LE 0 THEN minIreg = 1.
       ;valsreg = corrim[region_SUBS]
       ;valsreg = valsreg[SORT(valsreg)]
       ;lowmedian = MEDIAN(valsreg[0:N_ELEMENTS(valsreg)/1.33])
       ;minIreg = MEDIAN(valsreg[0:N_ELEMENTS(valsreg)/5.])
       ;concentr = lowmedian/(N_ELEMENTS(valsreg)/1.33)
       Izone = MEAN(corrim[wdilim2],/NAN)
       
       ;###DILATE the region moreover to see if it is really isolated
       dilim2 = DILATE(tmpim,mc_str2)
       wdilim2 = WHERE(dilim2)
       tab = UNIQ([wsegim,wdilim2],SORT([wsegim,wdilim2]))
       nuniq = N_ELEMENTS(tab)
       nval = N_ELEMENTS([wsegim,wdilim2])-hist_LABL[ii]
       IF nval EQ nuniq THEN isol2 = 1 ELSE isol2 = 0
       
       ;if it overlaps get the subscripts of the overlaped region
       dilim[wdilim]=1b
       dilim=dilim+segim
       dilim[region_SUBS]=0b
       wdilim3=WHERE(dilim EQ 2b,n3)
       limdark = (2.2*sdev_corrim+mean_corrim)/mean_corrim
       IF n3 GT 0 THEN overReg = MEAN(corrim[wdilim3])/Izone ELSE overReg = 1./limdark
       
       ;form factor : privilegiate elongated
       ;shapes (replca by compacity index? cf remove_sunspot
       all_x = region_SUBS MOD xsize
       all_y = region_SUBS / xsize
       max_x = MAX(all_x,MIN=min_x)
       min_y = MIN(all_y)
       xsize2 = (max_x+1)-(min_x-1)+1
       region_SUBS2 = xsize2*(all_y - (min_y-1)) + (all_x - (min_x-1))
       ske_gray = EFR_SKELETON(region_SUBS2,xsize2,xsize2,EC=ec)
       elong = FLOAT(N_ELEMENTS(region_SUBS))/((2.*(ec+1))^2)
       ;print,elong
       elong_bonus = 0.
       IF elong GT 1.5 AND ec GE 3 THEN elong_bonus = 2.
       IF elong GT 2.5 AND ec GE 3 THEN elong_bonus = 4.
       
       
       ;window,8,xs=1024,ys=1024
       ;tvscl,dilim
       ;wait,1
       ;#### Define conditional criteria
       a0=8000.
       a1=30.
       a2=10.
       a3=-0.7
       a4=-0.5
       ;a5=6.
       ;if there are very few filaments on the disk, ie. quiet sun period for example, the isolated criteria
       ;should be lower, because in this case all regions are more or less isolated -> /2.
       IF nb_REG LT 10 THEN BEGIN
         a3=a3/2.
	 a4=a4/2.
       ENDIF 

       
       ;print,izone,ireg
       cond = a0*hist_LABL[ii]/ncorrim + a1*(Izone/Ireg-limdark) + a2*(1./limdark-overReg) + a3*isol + a4*isol2 + elong_bonus
       IF KEYWORD_SET(DISPLAY) THEN print, a0*hist_LABL[ii]/ncorrim,' + ',a1*(Izone/Ireg-limdark),' + ',a2*(1./limdark-overReg),' + ',a3*isol,' + ',a4*isol2,' + ',elong_bonus,' = ',cond
       IF cond LT 3.5 THEN  segim2[region_SUBS]=0b
	      
       dilim = 0
       dilim2 = 0 
    ENDIF
    
 ENDFOR
 tmpim=0
 
     
RETURN,segim2
END
