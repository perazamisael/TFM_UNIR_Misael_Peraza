FUNCTION COORDTAB,arrx,arry,csi;,Lcarr

     ix = FLOAT(csi.cdelt1*(arrx-csi.crpix1)+csi.crval1)
     iy = FLOAT(csi.cdelt2*(arry-csi.crpix2)+csi.crval2)
     temp = ARCMIN2HEL(ix/60., iy/60.,  date=csi.date_obs, $
                           off_limb=off_limb);,l0=Lcarr(0))
     new_coord = TRANSPOSE(temp)

RETURN,new_coord
END