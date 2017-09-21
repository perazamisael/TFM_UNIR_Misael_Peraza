PRO TESTCDELT

raptx = 0
rapty = 0
difftx = 0
diffty = 0
diffcd = 0
diffcd1 = 0
diffcd2 = 0
diffrs = 0
diffrs1 = 0
diffrs2 = 0
diffc1 = 0
diffc2 = 0
count=0
diffdiam = 0

;files = DIALOG_PICKFILE(path='/home/fuller/poub/SAV/FIL/STRUCT/',/mult)
;nfile = N_ELEMENTS(files)
fil98 = FINDFILE('/home/fuller/poub/SAV/FIL/STRUCT/98*meu*_fil.sav',count=c98)
fil99 = FINDFILE('/home/fuller/poub/SAV/FIL/STRUCT/99*meu*_fil.sav',count=c99)
fil00 = FINDFILE('/home/fuller/poub/SAV/FIL/STRUCT/00*meu*_fil.sav',count=c00)
fil01 = FINDFILE('/home/fuller/poub/SAV/FIL/STRUCT/01*meu*_fil.sav',count=c01)

files=[fil98,fil99,fil00,fil01]
nfile =c98+c99+c00+c01
print,nfile
FOR ii = 0,nfile-1 DO BEGIN

;1 trouver la valeur du diametre solaire en arcsecs
;  pour le jour donne et les valeurs de cdelt1 et cdelt2

   RESTORE,files[ii]
   cdelt1 = FLOAT(strpro.cdelt1)
   cdelt2 = FLOAT(strpro.cdelt2)
   locfil = strpro.loc_file
   datepos = STRPOS(locfil,'mh')
   dye = STRTRIM(STRMID(locfil,datepos+2,2),2)
   dmo = STRTRIM(STRMID(locfil,datepos+4,2),2)
   dda = STRTRIM(STRMID(locfil,datepos+6,2),2)
   dho = STRTRIM(STRMID(locfil,datepos+9,2),2)
   dmi = STRTRIM(STRMID(locfil,datepos+11,2),2)
   res = GET_SUN(dye+'/'+dmo+'/'+dda+', '+dho+':'+dmi+':00.000',/SD)
   diam = res[1]
  
;2 calculer le cdelt theorique du fichier preprocessed

  cdelt = diam / 420d

;3 comparer au cdelt afficher

  difftx = [difftx,cdelt1 - cdelt]
  diffty = [diffty,cdelt2 - cdelt]

;4 calculer la difference entre le diametre du header et celui de
;get_sun

  rsunh = FLOAT(strobs.r_sun)
  cdelth = FLOAT(strobs.cdelt1)
IF ABS(rsunh*cdelth-diam) GT 5 then print,locfil
  diffdiam = [diffdiam,rsunh*cdelth-diam]
 
;5 difference entre cdelt header et cdelt calcule en fonction
;  des params de l'ellipse et de diam

  IF strpro.el_axis1 NE '\N' THEN BEGIN
   ellax1 = FLOAT(strpro.el_axis1)
   ellax2 = FLOAT(strpro.el_axis2)
   ellaxmean = (ellax1+ellax2) /2.
   cdeltell = diam/ellaxmean
   cdeltell1 = diam/ellax1
   cdeltell2 = diam/ellax2
   diffcd = [diffcd,cdelth-cdeltell]
   diffcd1 = [diffcd1,cdelth-cdeltell1]
   diffcd2 = [diffcd2,cdelth-cdeltell2]
  ENDIF ELSE PRINT,'no ellipse data' 


;6 difference entre rsun header et rsun ellipse (mean)


  diffrs = [diffrs,rsunh-ellaxmean]
  diffrs1 = [diffrs1,rsunh-ellax1]
  diffrs2 = [diffrs2,rsunh-ellax2]   

;7 decalage entre les positions du centre (ellipse et header)

  IF strpro.el_cen_x NE '\N' THEN BEGIN
   ellc1 = FLOAT(strpro.el_cen_x)
   ellc2 = FLOAT(strpro.el_cen_y)
   hc1   = FLOAT(strobs.center_x)
   hc2   = FLOAT(strobs.center_y)
IF ABS(hc1-ellc1) GE 5 OR ABS(hc2-ellc2) GE 5 THEN count=count+1
   diffc1 = [diffc1,hc1-ellc1+100]
   diffc2 = [diffc2,hc2-ellc2+100]
ENDIF ELSE PRINT,'no ellipse data' 

ENDFOR
;print,count
plot,difftx,yra=[-0.005,0.005]

;plot,diffdiam,YRA=[-5,+5]

;plot,diffcd
;window,/free
;plot,diffcd1
;window,/free
;plot,diffcd2

;plot,diffrs1
;window,/free
;plot,diffrs2

;   histc1 = HISTOGRAM(diffc1)
;   histc2 = HISTOGRAM(diffc2)
;   sizmax1 = n_elements(histc1)-100
;   histc1=histc1[100-sizmax1:100+sizmax1-1]
;   tab1 = INDGEN(2*sizmax1)-sizmax1
;   sizmax2 = n_elements(histc2)-100
;   histc2=histc2[100-sizmax2:100+sizmax2-1]
;   tab2 = INDGEN(2*sizmax2)-sizmax2

;plot,tab1,histc1
;window,/free
;plot,tab2,histc2


END
