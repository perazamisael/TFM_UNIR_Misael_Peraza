@/home/fuller/IDL/filament_std.pro

PRO STDRELA

command='ls -1 /home/fuller/poub/SAV/mh*_p.sav'
SPAWN,command,tab
restab = FLTARR(N_ELEMENTS(tab),6,3)

FOR ii=0,N_ELEMENTS(tab)-1 DO BEGIN
    print,'ii',ii
    RESTORE,tab(ii)
    restab(ii,*,*) = FILAMENT_STD(input=arr)
    print,restab(ii,*,*)
    print,'********************************************' 
    SAVE,restab,filename='/home/fuller/res_std_rela.sav'
ENDFOR

END