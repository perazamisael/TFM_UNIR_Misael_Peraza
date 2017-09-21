@/home/fuller/IDL/FIL/pruned_ske.pro

PRO DETECT_SUNSPOTS,res,im,resu


  im_LABL   = LABEL_REGION(res)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_REG   = N_ELEMENTS(hist_LABL)

  lim_area = 100 
  lim_circ = 0.90
  latinf   = 512 - 420/2
  latsup   = 512 + 420/2
  limratio = 0.3
  resu     = res

  FOR ii=1, nb_REG-1 DO BEGIN

     region_SUBS = rev(rev[ii]:rev[ii+1]-1)
     nele = N_ELEMENTS(region_SUBS) 
     latpix = region_SUBS[nele/2] / 1024     

     IF latpix GT latinf AND latpix LT latsup THEN BEGIN

        area_pix = N_ELEMENTS(region_SUBS)

        IF area_pix LT lim_area AND area_pix GT 5 THEN BEGIN

          values = im[region_SUBS]     
          sval = SORT(values)
          sval2= REVERSE(sval)
          highmean = MEAN(values[sval2[0:4]])           
          npix = WHERE(values LT (highmean-highmean/5.),npixm)

          IF npixm GT 0 AND (npixm*1./area_pix) GT limratio THEN BEGIN 


            bound_ind  = GET_BOUNDARY(region_SUBS,4,1024,1024)     
            bound_ind  = M_CONNECT(bound_ind,1024,1024,/NOPRINT)    
            circ= (4.*!pi*area_pix)/(N_ELEMENTS(bound_ind))^2 ;circularity
 
            IF circ GT lim_circ THEN BEGIN

               resu[region_SUBS]=2

            ENDIF
          ENDIF
        ENDIF
     ENDIF
  ENDFOR

END




