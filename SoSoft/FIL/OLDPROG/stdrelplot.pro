PRO STDRELPLOT
RESTORE,'/home/fuller/res_std_rela.sav'
RESTORE,'/home/fuller/res_std_calc.sav'
nn=(SIZE(restab))(1)
res=FLTARR(nn,2)
resu=FLTARR(nn)
FOR ii=0,nn-1 DO BEGIN
  maxseed=WHERE(restab(ii,*,1) EQ MAX(restab(ii,*,1)))
  maxseed=maxseed(0)
  goodlim=restab(ii,maxseed,0)
  maxlim =restab(ii,0,0)
  resu(ii,0)=goodlim*1./maxlim
ENDFOR
;plot,resu
;plot,restab2
;resu=resu(14:36)
;restab2=restab2(14:36)
toto=SORT(restab2)
print,max(restab2)
print,min(restab2)
print,max(resu)
print,min(resu)
plot,restab2(toto),resu(toto)


;resort=SORT(restab2)
;res(*,0)=restab2(resort)
;res(*,1)=resu(resort)
;plot,res(12:36,0),res(12:36,1)
;plot,res(*,0)
;oplot,res(*,1)
END