FUNCTION EFR_FFTCORRECT,imin

  ;#### Image size
  siz = SIZE(imin)
  xsiz = siz[1]
  ysiz = siz[2]

  ;#### Fourier Transform and shift
  fftim = FFT(imin,-1)
  fftim = SHIFT(fftim,xsiz/2,ysiz/2)


;tutu=ABS(fftim)-transpose(ABS(fftim))
;mama=tutu*0.
;FOR ii=1,1019,3 DO BEGIN
;  FOR jj=3,1019,3 DO BEGIN
;      mama[ii-1:ii+1,jj-1:jj+1]=MAX(tutu[ii-1:ii+1,jj-2:jj+1])
;  ENDFOR
;ENDFOR 
;tv,bytscl(mama,0,0.2)
;tv,bytscl(mama-smooth(mama,20),0,0.2)
;GOTO,autre


  ;#### fourier threshold with cumulative histogram
   npts = (xsiz*ysiz)/1.15
   fft_c_hist = HIST_EQUAL(100*REAL_PART(ABS(fftim)),/HISTOGRAM_ONLY,BINSIZE=1,MINV=0)
   minval = (WHERE(fft_c_hist GE npts,nsubs))[0]
   minval = minval/100.

   TVSCL,real_part(ABS(fftim)) GT minval

   imreal = REAL_PART(ABS(fftim)) GT minval
   imreal_o = imreal
   mask   = EFR_ROUNDMASK(xsiz,ysiz,0,150)
   imreal[mask] = 0b

tvscl,imreal


   newmax = 0
   maxsum = 0
  ;####TRY TO GET THE MAIN ORIENTATION
   FOR jj = 0,179 DO BEGIN
      imj = ROT(imreal,jj)
      maxv = MAX(TOTAL(imj,2))
      maxsum = maxsum + maxv
      IF maxv GT newmax THEN BEGIN
         porien = jj
         newmax = maxv
      ENDIF
  ENDFOR

  print,porien

  ;####Get a more precise value +/- 0.2°
  newmax = 0
  FOR jj = porien-0.6,porien+0.6,0.2 DO BEGIN
      imj = ROT(imreal,jj)
      maxv = MAX(TOTAL(imj,2))
      IF maxv GT newmax THEN BEGIN
         neworien = jj
         newmax = maxv
      ENDIF
  ENDFOR

  cumul = INTARR(xsiz)
  im2   = ROT(imreal,neworien)
  cumul = TOTAL(im2,2)
  PRINT,'Main line orientation:',neworien
  locs = WHERE(cumul GT MEAN(cumul)*4.) 

  imres = TEMPORARY(im2)*0b
  imres[locs,*] = 1b
  imres = ROT(imres,-1.*neworien[0])

  locs2 = WHERE(imres)
  locs3 = WHERE(imreal_o[locs2] GE minval)

  imres2=imres*0b
  imres2[locs2[locs3]]=1b
  tvscl,imres2
  
  ww=WHERE(imres2,nbpts)
  limx=(SIZE(fftim))[1]
  limy=(SIZE(fftim))[2]
nbpts=n_elements(locs2)
  FOR ii=0,nbpts-1 DO BEGIN
     ptx = locs2[ii] MOD 1024
     pty = locs2[ii]/1024
     mini=fftim[(ptx-5)>0:(ptx+5)<limx-1,(pty-5)>0:(pty+5)<limy-1]
     fftim[locs2[ii]]=MEAN(mini[where(mini)])  
  ENDFOR
  ;fftim[ww]=minval
;   fftim[locs2]=0.

tv,bytscl(fftim,0,0.2)
  
  imres = FFT(SHIFT(fftim,-xsiz,-ysiz),1)
RETURN,imres
END
