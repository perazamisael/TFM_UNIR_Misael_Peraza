FUNCTION EFR_ORIENTED_CUMUL,im,diam,DISPLAY=display

  im = BYTE(im)
  xsize = (SIZE(im))[1]
  ysize = (SIZE(im))[2]

  xsizs = 512
  ysizs = 512

  small = REBIN(im,xsizs,ysizs,/sample)


  ;#### NORMALIZATION PROFILE CALCULATION
  ;#### (a bit larger than disk to avoid
  ;#### pbs near limb)
  mask  = EFR_ROUNDMASK(xsizs,ysizs,0,((diam/(xsize/xsizs))/2.)*1.1)
  cmask = INTARR(xsizs)
  imask = BYTARR(xsizs,ysizs)
  imask[mask] = 1b
  cmask = TOTAL(imask,2)
  cmask = DOUBLE(cmask)/MAX(cmask)
  cmask[WHERE(cmask LE 0)] = 1

  newmax = 0
  maxsum = 0
  ;####GET THE PREFERENTIAL ORIENTATION
  FOR jj = 0,179 DO BEGIN
      imj = ROT(small,jj)
      maxv = MAX(TOTAL(imj,2)/cmask)
      maxsum = maxsum + maxv
      IF maxv GT newmax THEN BEGIN
         porien = jj
         newmax = maxv
      ENDIF
  ENDFOR

  ;####CHECK IF IT'S ENOUGH PREFERENTIAL
  IF newmax LT 2.*(maxsum/180.) THEN RETURN,-1

  ;####FULL NORMALIZATION PROFILE CALCULATION
  mask  = EFR_ROUNDMASK(xsize,ysize,0,(diam/2.)*1.1)
  cmask = INTARR(xsize)
  imask = BYTARR(xsize,ysize)
  imask[mask] = 1b
  cmask = TOTAL(imask,2)
  cmask = DOUBLE(cmask)/MAX(cmask)
  cmask[WHERE(cmask LE 0)] = 1

  ;####GET THE MAX VALUES AT THIS ORIENTATION
;##changer pour un pas de 0.2 et une boucle
  cumul = INTARR(xsize,3)
  im2 = ROT(im,porien-0.3)
  cumul[*,0] = TOTAL(im2,2)/cmask
  im2 = ROT(im,porien)
  cumul[*,1] = TOTAL(im2,2)/cmask
  im2 = ROT(im,porien+0.3)
  cumul[*,2] = TOTAL(im2,2)/cmask
  mcumul = [MAX(cumul[*,0]),MAX(cumul[*,1]),MAX(cumul[*,2])]
  mporien = [porien-0.3,porien,porien+0.3]
  ind = WHERE(mcumul EQ MAX(mcumul))
  cumul = cumul[*,ind]
  porien = mporien[ind]
  PRINT,'Main line orientation:',porien
  locs = WHERE(cumul GT MEAN(cumul)*3.5)

  ;####
  imres = TEMPORARY(im2)*0b
  imres[locs,*] = 1b
  imres = ROT(imres,-1.*porien[0])


RETURN,imres

END
