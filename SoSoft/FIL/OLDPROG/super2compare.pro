FUNCTION CHAIN2IND,first,chainc,xsize,ysize
  
   ind=first[1]*LONG(xsize)+first[0]
   length=STRLEN(chainc)
   ;set=[1,xsize+1,xsize,xsize-1,-1,-xsize-1,-xsize,-xsize+1];trigo debut droite
   set=[-1,-xsize-1,-xsize,-xsize+1,1,xsize+1,xsize,xsize-1];trigo debut gauche 
   newind=ind
   FOR jj=0,length-1 DO BEGIN
      next=FIX(STRMID(chainc,jj,1))
      newind=newind+set[next]
      ind=[ind,newind]
   ENDFOR      
   
RETURN,ind
END


FUNCTION SUPER2COMPARE,COORD=coord
  ;superimpose segmented filaments to the Sun to compare
  ;with bass2000
  ;The standard Sun : sunrad 420 , centered at (511.5,511.5)


  ;restore the structure(s) (daily stored)
   path= '/home/fuller/poub/SAV/FIL/STRUCT/'
   files = DIALOG_PICKFILE(PATH=path,FILTER='*9801*_fil.sav',/mult)
   nbjour = N_ELEMENTS(files)

  ;Build a sun shape
   rsun = 420
   Xsiz = 1024
   Ysiz = 1024
   input = Bytarr(Xsiz,Ysiz)
   mask_SUBS   = WHERE(input*0+1)
   mask_SUBS_Y = mask_SUBS /   Xsiz
   mask_SUBS_X = mask_SUBS MOD Xsiz
   mask_DIST   = SQRT((mask_SUBS_X-(Xsiz-1)/2)^2 + (mask_SUBS_Y-(Ysiz-1)/2)^2) 
   mask_0      = WHERE(mask_DIST LE rsun,nmsk0)
   input[mask_0]=1b

   loadct,3
   window,/free,xs=1024,ys=1024

   FOR ww=0,nbjour-1 DO BEGIN

     RESTORE,files[ww]

     ngd = N_ELEMENTS(strfil) 

     ;####recuperation des valeurs utilisees lors de l'encodage (chain code)
     cdelt1srce = strpro.cdelt1
     cdelt2srce = strpro.cdelt2
     naxis1srce = strpro.naxis1
     naxis2srce = strpro.naxis2


     FOR ii=0,ngd-1 DO BEGIN


      ;####calcul pour tous les points de la position en pixels
      ;####de l'origine puis de la chaine
      first_pt_arcsx = strfil[ii].cod_ske_arc_x
      first_pt_arcsy = strfil[ii].cod_ske_arc_y
      xpos = FIX(DOUBLE(511.5) + first_pt_arcsx/DOUBLE(cdelt1srce))
      ypos = FIX(DOUBLE(511.5) + first_pt_arcsy/DOUBLE(cdelt2srce))
      schain = strfil[ii].chain_code_ske
      indices = CHAIN2IND([xpos,ypos],schain,naxis1srce,naxis2srce)

      input[indices]=0b
      input[xpos,ypos]=0b
  
    ENDFOR

    TVSCL,input


    ;affichage optionel de la longueur en deg
    ;des longitudes et latitudes en carrington

    IF KEYWORD_SET(COORD) THEN BEGIN

       FOR jj=0,ngd-1 DO BEGIN

        first_pt_arcsx = strfil[jj].cod_ske_arc_x
        first_pt_arcsy = strfil[jj].cod_ske_arc_y
        xpos = FIX(DOUBLE(511.5) + first_pt_arcsx/DOUBLE(cdelt1srce))
        ypos = FIX(DOUBLE(511.5) + first_pt_arcsy/DOUBLE(cdelt2srce))
        schain = strfil[jj].chain_code_ske
        indices = CHAIN2IND([xpos,ypos],schain,naxis1srce,naxis2srce)
        tay = N_ELEMENTS(indices)
        pixcenx = indices[tay/2] MOD Xsiz
        pixceny = indices[tay/2] / Xsiz
        length = strfil[jj].ske_len_deg
        carlat = strfil[jj].grav_c_car_lat
        carlon = strfil[jj].grav_c_car_lon
        ;XYOUTS,xpos-5,ypos-5,STRTRIM(length,2),/device,COLOR=100
        ;XYOUTS,xpos+5,ypos-5,STRTRIM(FIX(carlat),2),/device,COLOR=200
        ;XYOUTS,xpos+5,ypos-15,STRTRIM(FIX(carlon),2),/device,COLOR=50
        XYOUTS,pixcenx,pixceny,STRMID(STRTRIM(length,2),0,4),/device,COLOR=100
        XYOUTS,pixcenx,pixceny-10,STRTRIM(FIX(carlon),2),/device,COLOR=200
        XYOUTS,pixcenx,pixceny-20,STRTRIM(FIX(carlat),2),/device,COLOR=50
    
        XYOUTS,300,50,strobs.date_obs,/device,color=200,charsize=3
      ENDFOR
 
    ENDIF  

ENDFOR

RETURN,input
 
END
