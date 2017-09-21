PRO SHOW_SQUARE

RESTORE,'/home/fuller/poub/SAV/tab_c_im5b.sav'
    siz=SIZE(tab2)
    nb = siz(3)
    screen = FLTARR(1024,1024)
    L0 = 328.
    FOR ii=0,nb-1 DO BEGIN
        tab = tab2(*,*,ii)
        wx  = WHERE(tab(0,*) NE -1,nx)
        wy  = WHERE(tab(1,*) NE -1)
        tabx = FLTARR(nx)
        taby = FLTARR(nx)
        tabx(*) = (tab(0,wx)-(L0-90.)+360.) MOD 360.
        taby(*) = tab(1,wy)+90.
        screen(tabx*(1024./180.),taby*(1024./180.))=1. 
        ;FOR jj=0,nx-1 DO BEGIN
        ;    
        ;ENDFOR

    ENDFOR
    window,0,xs=1024,ys=1024
    tvscl,screen

END