FUNCTION FREQUENCY,image
window,1,xs=1024,ys=1024
nul=where(image LE 0.)
nnul=where(image GT 0.)
im2=image
;im2[nul]=MEDIAN(image[nnul])
tvscl,im2
fftim=fft(im2)
sfftim=shift(fftim,512,512) 
tvscl,ALOG(ABS(sfftim)^2)


filter=bytarr(1024,1024)+1b
fsiz=30
wf=WHERE(filter)

  FILT_SUBS_Y = wf / 1024
  FILT_SUBS_X = wf MOD 1024
  FILT_DIST   = SQRT((FILT_SUBS_X-(1024-0.5)/2)^2 + (FILT_SUBS_Y-(1024-0.5)/2)^2) 
  FILT        = WHERE(FILT_DIST LE fsiz)

  FILT2       = WHERE(FILT_DIST GT 420.)

;tsfftim = sfftim
;tsfftim[FILT]=0.;MEDIAN(sfftim)
;tvscl,tsfftim

ssfftim=MEDIAN(ABS(sfftim),10)
diff=ABS(sfftim)-ssfftim
difft = diff gt 0.03

hdifft = HOUGH(difft,RHO=rho,THETA=theta)
hdifft = (hdifft - 400) > 0

bp=HOUGH(hdifft,/BACK,RHO=rho,THETA=theta)
tvscl,bp

toto = bp gt 1500
wtoto = WHERE(toto)

titi=sfftim
titi[wtoto]=0
titi[FILT]=sfftim[FILT]

;tvscl,ALOG(ABS(titi)^2)

;surface,diff,zra=[0,0.51]

infft=FFT(SHIFT(titi,-512,-512), /INVERSE)
infft[FILT2]=0
tvscl,infft
RETURN,infft
END

