FUNCTION STDDEVCALC,image

  xs     = (SIZE(image))(1)
  ys     = (SIZE(image))(2)
  image2 = image(1:xs-2,1:ys-2)  
  nzero  = WHERE(image2,cc)
  tab    = FLTARR(cc/4+1)   ;take 1pix/2 to go faster
 
  FOR kk=0L,cc-1,4 DO BEGIN
     jj = nzero(kk) /   (xs-2)
     ii = nzero(kk) MOD (xs-2) 
     ii = ii + 1 & jj = jj +1 
     tab(kk/4) = STDDEV(image(ii-1:ii+1,jj-1:jj+1))
  ENDFOR
 
RETURN,MEDIAN(tab)
END


PRO STDCALC

command='ls -1 /home/fuller/poub/SAV/mh*_p.sav'
SPAWN,command,tab
restab2 = FLTARR(N_ELEMENTS(tab))

FOR ii=0,N_ELEMENTS(tab)-1 DO BEGIN
    print,'ii',ii
    RESTORE,tab(ii)
    input=arr

    diam = 840

    Xsiz = (SIZE(input))(1)
    Ysiz = (SIZE(input))(2)
  
    input=FIX(input)

    ;####TEST THE SIZE OF INPUT
;    IF Xsiz NE Ysiz OR Xsiz NE 1024 THEN GOTO,end_filament

    ;####CREATE SOME MASKS
;    mask_SUBS   = WHERE(input*0+1)
;    mask_SUBS_Y = mask_SUBS /   Xsiz
;    mask_SUBS_X = mask_SUBS MOD Xsiz
;    mask_DIST   = SQRT((mask_SUBS_X-Xsiz/2)^2 + (mask_SUBS_Y-Ysiz/2)^2) 
;    mask_0      = WHERE(mask_DIST GE (diam/2))
;    mask_1      = WHERE(mask_DIST GE (diam/2)*0.99)

    ;####APPLY A MEDIAN FILTER AND DIVIDE INPUT BY THE RESULT
;    im_REB = REBIN(input, Xsiz/2, Ysiz/2)
;    im_MED = MEDIAN(im_REB, 30)
;    im_MED = REBIN(im_MED, Xsiz, Ysiz)
;    im_DIV = input/(im_MED+1)

;    im_DIV(mask_1) = 0
    ;####ENHANCE THE CONSTRAST WITH HIST_EQUAL
;    im_HEP  = HIST_EQUAL(im_DIV,PERCENT=1)

    ;std = STDDEVCALC(im_HEP)
    std = STDDEVCALC(input)
    restab2(ii)=std
    print,'std',std
    print,'********************************************' 
    SAVE,restab2,filename='/home/fuller/res_std_calc.sav'

end_filament: 
ENDFOR

END