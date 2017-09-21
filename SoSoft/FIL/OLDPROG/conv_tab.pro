PRO CONV_TAB,tab,head,nr,n_unit

    nb = MAX(tab(2,*)) ;(SIZE(tab))(3)
    arr = tab*0.-1.

    ;get data from header
    csi = ITOOL_SET_CSI(head)

    ;get the julian date of obs.
    date  = csi.date_obs
    year  = STRMID(date,0,4)
    month = STRMID(date,5,2)
    day   = STRMID(date,8,2) 
    hour  = STRMID(date,11,2)
    min   = STRMID(date,14,2)
    sec   = STRMID(date,17,2)
    jday  = JULDAY(month,day,year,hour,min,sec)    
    sjday = STRTRIM(STRING(jday,format='(f14.6)'),2)

    ;get the julian date of nr and nr+1
    nr1  = CARR2EX(nr)
    nr2  = CARR2EX(nr+1)
    jnr1 = JULDAY(nr1(5),nr1(4),nr1(6),nr1(0),nr1(1),nr1(2))
    jnr2 = JULDAY(nr2(5),nr2(4),nr2(6),nr2(0),nr2(1),nr2(2))

    ;get carrington longitude L0
    L0 = TIM2CARR(csi.date_obs)

    ;convert coordinates for each filament
    FOR ii=0,nb-1 DO BEGIN

       wx  = WHERE(tab(2,*) EQ ii+1,nx)
       tbx = tab(0,wx)
       tby = tab(1,wx)
       coord = COORDTAB(tbx,tby,csi)

      ;region number 
       regn  = STRTRIM(STRING(ii+1),2)


      ;check if the filament belongs to the rotation (from the middle point coordinates)
       longb_check  =  ((coord(nx/2,1) + 360.) MOD 360. + L0) MOD 360.
       range = FLTARR(2)
       timlim = DOUBLE(jnr1+jnr2)/2.

       IF jday LT timlim THEN BEGIN
          range(0)=(L0+180.) MOD 360. 
          range(1)=360.
       ENDIF
       IF jday GE timlim THEN BEGIN
          range(0)= 0. 
          range(1)= (L0+180.) MOD 360.
       ENDIF

       IF longb_check GE range(0) AND longb_check LE range(1)  THEN BEGIN

         ;calculate the barycenter coord (middle point+L0)
          long_b  = ((coord(nx/2,1) + 360.) MOD 360. + L0) MOD 360.
          lat_b   = coord(nx/2,0)

         ;print the barycenter coordinates
          slong_b = STRTRIM( STRING(long_b, format='(f10.6)' ) ,2)
          slat_b  = STRTRIM( STRING(lat_b, format='(f10.6)' ) ,2)
          PRINTF,n_unit,FORMAT='(A,"/",A,"/",A,"/",A,"/")',sjday,regn,slong_b,slat_b

         ;each point of the filament:
          FOR jj=0,nx-1 DO BEGIN

            ;calculate the coordinates of fil (+L0)
             longi  = ((coord(jj,1) + 360.) MOD 360. + L0) MOD 360.
             lati  = coord(jj,0)

            ;check the long in case the filament would cross the L0 long
             IF ABS(longi-longb_check) GT 180. AND longi LT 180. THEN longi=longi+360.
             IF ABS(longi-longb_check) GT 180. AND longi GT 180. THEN longi=longi-360.              

            ;print the coordinates
             slongi = STRTRIM( STRING( longi,format='(f10.6)' ) ,2)
             slati  = STRTRIM( STRING( lati ,format='(f10.6)' ) ,2)
             PRINTF,n_unit,FORMAT='(A,"/",A,"/")',slongi,slati

          ENDFOR
        
          ;mark the end of filament
          PRINTF,n_unit,'-999.99/'

        ENDIF
    ENDFOR    
;RETURN,arr
END


