PRO CONVERTAB;,tab,header

    RESTORE,'/home/fuller/poub/SAV/ftim5b.sav
    tab2 = resu*0. -1.
    siz=SIZE(resu)
    nb = siz(3)
    csi=GET_CSI()
    date=csi.date_obs
    year  = STRMID(date,0,4)
    month = STRMID(date,5,2)
    day   = STRMID(date,8,2) 
    hour  = STRMID(date,11,2)
    min   = STRMID(date,14,2)
    sec   = STRMID(date,17,2)
    jday  = STRTRIM(STRING(JULDAY(month,day,year,hour,min,sec),format='(f14.6)'),2)
    L0 = TIM2CARR(csi.date_obs)
   
    OPENW,wunit,'/home/fuller/poub/SAV/txtim5b.txt',/GET_LUN
    FOR ii=0,nb-1 DO BEGIN
        tab = resu(*,*,ii)
        wx  = WHERE(tab(0,*) NE -1,nx)
        wy  = WHERE(tab(1,*) NE -1)
        tabx = INTARR(nx)
        taby = INTARR(nx)
        tabx(*) = tab(0,wx)
        taby(*) = tab(1,wy)
        coord = COORDTAB(tabx,taby,csi)
        regn  = STRTRIM(STRING(ii),2)
        long_b= STRTRIM( STRING( ( (coord(nx/2,1) +360 ) MOD 360 + L0) MOD 360, format='(f10.6)' ) ,2)
        lat_b = STRTRIM( STRING( coord(nx/2,0), format='(f10.6)' ) ,2)
        PRINTF,wunit,FORMAT='(A,"/",A,"/",A,"/",A,"/")',jday,regn,long_b,lat_b
        FOR jj=0,nx-1 DO BEGIN
          longi = STRTRIM( STRING( ( (coord(jj,1)+360) MOD 360 + L0) MOD 360,format='(f10.6)' ) ,2)
          lati  = STRTRIM( STRING( coord(jj,0), format='(f10.6)' ) ,2)
          PRINTF,wunit,FORMAT='(A,"/",A,"/")',longi,lati
          tab2(0,jj,ii) = ( (coord(jj,1)+360) MOD 360 + L0) MOD 360
          tab2(1,jj,ii) = coord(jj,0)
        ENDFOR
        PRINTF,wunit,'-999.99/'
    ENDFOR
    SAVE,tab2,filename='/home/fuller/poub/SAV/tab_c_im5b.sav'
    FREE_LUN,wunit
END
