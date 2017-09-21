;TEST marche mais empeche de fixer une limite pour la taille des seeds
;dans efr_fil_region_grow: du coup il faut passer tous les seeds en
;revue avant d'eventuellement les eliminer parceque les regions resultantes
;sont trop petites: c'est beaucoup plus long


FUNCTION EFR_FIL_SEEDS_GEO,xs,diam

  bin = 12*(xs/1024)
  im_OUT = BYTARR(xs,xs)
  FOR ii=1,xs-2,bin DO BEGIN
     FOR jj=1,xs-2,bin DO BEGIN
        im_OUT[ii-1:ii,jj-1:jj] = 1b
     ENDFOR
  ENDFOR

  ;##### Set seeds out of the disk or very close to the limb to 0 
  mask         = EFR_ROUNDMASK(xs,xs,0.,diam/2.-6,COMP=mask_comp)
  im_OUT[mask_comp] = 0b
   
RETURN,im_OUT
END
