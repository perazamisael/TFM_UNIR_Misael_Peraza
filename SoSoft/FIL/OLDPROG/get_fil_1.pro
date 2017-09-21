FUNCTION GET_FIL_1,arr
  
  csi=GET_CSI()

  im_LABL   = LABEL_REGION(arr)
  hist_LABL = HISTOGRAM(im_LABL, REVERSE_INDICES=rev)
  nb_REG    = N_ELEMENTS(hist_LABL)

  ;####LOOP ON EACH REGION
  FOR ii=1, nb_REG-1 DO BEGIN

     ;####SUBSCRIPTS OF LABELLED REGIONS     
     region_SUBS = rev(rev[ii]:rev[ii+1]-1)
     region_SUBS_Y = region_SUBS /   1024
     region_SUBS_X = region_SUBS MOD 1024
     ix = FLOAT(csi.cdelt1*(region_SUBS_X-csi.crpix1)+csi.crval1)
     iy = FLOAT(csi.cdelt2*(region_SUBS_Y-csi.crpix2)+csi.crval2)
     temp = ARCMIN2HEL(ix/60., iy/60.,  date=csi.date_obs, $
                           off_limb=off_limb)
     region_SUBS2 = TRANSPOSE(temp)
     PRINT,'REGION',ii,'*************************'
     PRINT,region_SUBS2
  ENDFOR
RETURN,region_SUBS2
END