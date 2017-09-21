PRO SYNIM
;utilise la librairie solarsoft
;Appel de MAP_CARRINGTON
window,xs=1024,ys=1024,/free
nn=1966
;essai pour creer une carte synoptique en cumulant
;des images halpha sur une rotation (nn)
coe = 4
res = FLTARR(360*coe,420)
res2 = res
cc=0
;----------------------------------------------------------
; Define path where are stored the processed images
; Define path to store the synoptique image
;----------------------------------------------------------

  ;spath = '/data2/fuller/FITS/Ha/2003/PROCESSED/'
  spath = '/home/fuller/poub/SAV/FIL/TEST/'
  apath = '/home/fuller/poub/SAV/FIL/TEST/'

;----------------------------------------------------------
; Compute the dates of interest for this Carrington rot. 
;----------------------------------------------------------

  dat1 = CARR2EX(FIX(nn),OFFSET=90)
  dat2 = CARR2EX(FIX(nn)+1,OFFSET=-90)

  jdat1 = JULDAY(dat1(5),dat1(4),dat1(6),dat1(0),dat1(1),dat1(2))
  jdat2 = JULDAY(dat2(5),dat2(4),dat2(6),dat2(0),dat2(1),dat2(2))
  timlim = (jdat1+jdat2)/2.
  nbj  = LONG(jdat2)-LONG(jdat1) + 1
  jdat = LONG(jdat1) + INDGEN(nbj)

  CALDAT, jdat, mo, da, ye
;print,mo,da,ye
;----------------------------------------------------------
; Compute the fits filenames and check if they exist
;----------------------------------------------------------

  tabstr = STRARR(nbj)
  FOR ii = 0 ,nbj-1 DO BEGIN

    IF mo[ii] LT 10 THEN moii = '0'+STRTRIM(mo[ii],2) $
    ELSE moii = STRTRIM(mo[ii],2)
    IF da[ii] LT 10 THEN daii = '0'+STRTRIM(da[ii],2) $
    ELSE daii = STRTRIM(da[ii],2)
    yeii = STRMID(STRTRIM(ye[ii],2),2)
    name = 'mh'+yeii+moii+daii+'*_subtract_processed.fits'
    ffile = FINDFILE(spath+name,COUNT=cc)
    IF cc GT 1 THEN ffile = ffile[n_ELEMENTS(ffile)-1]
    IF cc EQ 0 THEN BEGIN
       PRINT,name,' is missing, ignore [1] or cancel [0]?'
       READ,resp,FORMAT='(A1)'
       IF resp NE 1 THEN RETALL
       ffile = '\N'
    ENDIF
    tabstr[ii] = ffile
  ENDFOR

  mtab = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  FOR jj = 0,nbj-1 DO BEGIN

     file = tabstr[jj]
 
    IF file NE '\N' THEN BEGIN
cc= cc+1
     print,tabstr[jj]
    ;-------------------------------------------------------
    ; Extract date from filename
    ;-------------------------------------------------------

     splitf  = STRSPLIT(file,'/',/EXTRACT)
     pfile   = splitf[N_ELEMENTS(splitf)-1]
     splitf2 = STRSPLIT(pfile,'_subtract_processed',/EXTRACT,/REGEX)
     da1 = STRMID(splitf2[0],2,2)
     da2 = STRMID(splitf2[0],4,2)
     da3 = STRMID(splitf2[0],6,2)
     da4 = STRMID(splitf2[0],9,2)
     da5 = STRMID(splitf2[0],11,2)
     da6 = STRMID(splitf2[0],13,2)
     IF da1 LT 50 THEN da1='20'+STRTRIM(da1,2) ELSE da1='19'+STRTRIM(da1)
     jday = JULDAY(da2,da3,da1,da4,da5,da6)
     date = da1+'-'+da2+'-'+da3+' '+da4+':'+da5+':'+da6
     da2m = mtab[da2-1]
     dateCDS = da4+':'+da5+' '+da3+'-'+da2m+'-'+STRTRIM(STRMID(da1,2,2),0)
    ;--------------------------------------------------
    ; Get carringt. long corresp. to observation date 
    ;--------------------------------------------------
       L0 = TIM2CARR(date)
       im = READFITS(tabstr[jj])
       ;im = im[92:931,92:931]
       tvscl,im
;       rect = CONGRID(EFR_ANNULUS2RECT(im,0,420),coe*180,420)
       xycen = FLTARR(2)
       cd1 = 2.2539
       ;cd1=2.
       xycen[0]=0.
       xycen[1]=0.;100.
       print,dateCDS ;le format de la date n'est pas bon ! voir get_pb0r, puis anytim2ints et modifier la date en entree
       rect = MAP_CARRINGTON(im,xycen,dateCDS,cd1)
       rect = CONGRID(rect,coe*180,420)
       medrect = MEDIAN(rect[WHERE(rect)])
tvscl,rect
wait,2
goto,endpoly
;       rect[0:coe*45-1,*]=medrect
;       rect[coe*135:coe*180-1,*]=medrect
    ;--------------------------------------------------
    ; Projection using POLY2D
    ;--------------------------------------------------

      nnul = WHERE(im,nmb)
      helcoo1 = PIX2CARR(nnul,1024,1024,2.248191,2.248191,511.5,511.5,420,$
                STRTRIM(date,2),FIXL0=0.) 
      helcoo2 = helcoo1*0.
      helcoo2[*,0] = helcoo1[*,0]+90.
      helcoo2[*,1] = (helcoo1[*,1]+90.) MOD 360.
      tab = FLTARR(721,641)
      tab2 = tab
      FOR ii=0l,nmb-1 DO BEGIN
        lati =  helcoo2[ii,0]
        loni =  helcoo2[ii,1]
        IF lati GE 10 AND lati LT 170 THEN BEGIN
          tab[FIX(4.*loni+0.5),FIX(4.*lati+0.5)-4*10]=im[nnul[ii]]
        ENDIF
      ENDFOR
      nul = WHERE(tab EQ 0,nbn)
      FOR kk = 0l,nbn-1 DO BEGIN
        ddi = 1
        good = 0
        nulx = nul[kk] MOD 721
        nuly = nul[kk]/721
        WHILE ddi LE 10 AND good LT 3 DO BEGIN
                                ;print,(nulx-ddi)>0,(nulx+ddi)<720,(nuly-ddi)>0,(nuly+ddi)<640
          tabi = tab[(nulx-ddi)>0:(nulx+ddi)<720,(nuly-ddi)>0:(nuly+ddi)<640]
          wgood = WHERE(tabi NE 0,good)
          ddi = ddi+1
        ENDWHILE
        IF wgood[0] NE -1 THEN tab2[nul[kk]] = MEDIAN(tabi[wgood]) ELSE $
        tab2[nul[kk]] = 0.
      ENDFOR   
rect=CONGRID(tab+tab2,coe*180,640)
tab=0
tab2=0
nnul=0
helcoo1=0
helcoo2=0
tvscl,rect
wait,1
endpoly:
goto,endo
    ;--------------------------------------------------
    ;
    ;--------------------------------------------------

       medrect = MEDIAN(rect[WHERE(rect)])
       rect = rect/medrect
       rect2= rect GT 0;*0+1

       IF L0 LT 90 THEN BEGIN
          cc = 90 - FIX(L0)
          IF jday LT timlim THEN BEGIN
            u0 = 0      & u1 = coe*cc-1
            v0 = coe*360-coe*cc & v1 = coe*360-1
          ENDIF ELSE BEGIN
            u0 = coe*cc     & u1 = coe*180-1
            v0 = 0      & v1 = coe*180-coe*cc-1
          ENDELSE
       ENDIF

       IF L0 GT 270 THEN BEGIN
          cc =360-FIX(L0)
          IF jday LT timlim THEN BEGIN
            u0 = 0      & u1 = coe*90+coe*cc-1
            v0 = coe*270-coe*cc & v1 = coe*360-1        
          ENDIF ELSE BEGIN
            u0 = coe*90+coe*cc  & u1 = coe*180-1
            v0 = 0      & v1 = coe*90-coe*cc-1
          ENDELSE
       ENDIF

       IF L0 GE 90 AND L0 LE 270 THEN BEGIN
            cc = 270-FIX(L0)
            u0 = 0      & u1 = coe*180-1
            v0 = coe*180-coe*cc & v1 = coe*360-coe*cc-1  
       ENDIF
;print,u0,u1,u1-u0
;print,v0,v1,v1-v0
;print,(u1-u0)-(v1-v0)
;print,'""""""""""""""""""'
       res[v0:v1,*] = rect[u0:u1,*] + res[v0:v1,*]
       res2[v0:v1,*] = rect2[u0:u1,*] + res2[v0:v1,*]
rect=0
rect2=0
tvscl,res


     ENDIF
endo:
  ENDFOR

;tvscl,res/res2

END

