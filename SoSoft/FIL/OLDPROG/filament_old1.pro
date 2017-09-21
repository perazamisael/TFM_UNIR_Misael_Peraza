FUNCTION growing_filament,im

ims=SMOOTH(hist_equal(im),3)
im_res = im
mask = WHERE(im EQ 0.)

;first tresh to find basic seeds
tresh = MEDIAN(ims(WHERE(ims)))/1.05 ;+ le coeff est eleve moins il y a de seeds
im_res(WHERE(ims LT tresh AND ims NE 0.,COMPLEMENT=bigv)) = 1.
im_res(bigv) = 0.
im_res(mask) = 0.

im_labl = LABEL_REGION(im_res)
im_seed = im
im_seed(*,*) = 0.  
hist_labl = HISTOGRAM(im_labl,REVERSE_INDICES=r)
mo_im1 = MORPH_GRADIENT(im,intarr(1,3)+1)
mo_im2 = MORPH_GRADIENT(im,intarr(3,1)+1)
sb_im = (mo_im1+mo_im2)/2.
im_seed2 = im_seed
;goto,next
FOR i=0, N_ELEMENTS(hist_labl)-1 DO BEGIN
   IF hist_labl(i) GE 2 AND hist_labl(i) LT 10000 THEN BEGIN
     ;PRINT, 'Region ',i, ', Population = ', hist_labl(i)
     p = r(r[i]:r[i+1]-1)
     q = ims(p)
     treshmax = MAX(q);*1.2
     treshmin = MIN(q)
     mini = WHERE(q EQ MIN(q))
     seed = p(mini(0))
     ypos = FIX(seed/1024.)
     xpos = seed - long(1024)*ypos
     dist = SQRT(LONG(512-xpos)^2+LONG(512-ypos)^2)
     IF dist LE 0.95*420 then $
        region = GROWING(ims,xpos,ypos,treshmin,treshmax)$;/1.2) $
        else region = 0
     print,'trouve:',N_ELEMENTS(region)
     loc = Fit_Ellipse(region,xsize=1024,ysize=1024,NPOINTS=30,CENTER=center)
     dist = SQRT(LONG(512-center(0))^2+LONG(512-center(1))^2)
     IF dist LE 0.95*420 and N_ELEMENTS(region) GT 1 THEN im_seed(region)=1.
   ENDIF
ENDFOR
next:
print,'________________________'
FOR i=0, N_ELEMENTS(hist_labl)-1 DO BEGIN
   IF hist_labl(i) GE 2 AND hist_labl(i) LT 10000 THEN BEGIN
     ;PRINT, 'Region ',i, ', Population = ', hist_labl(i)
     p = r(r[i]:r[i+1]-1)
     q = im(p)
     treshmax = MAX(q)*2.
     treshmin = MIN(q)
     mini = WHERE(q EQ MIN(q))
     seed = p(mini(0))
     ypos = FIX(seed/1024.)
     xpos = seed - long(1024)*ypos
     dist = SQRT(LONG(512-xpos)^2+LONG(512-ypos)^2)
     IF dist LE 0.95*420 then $
        region = GROWING(im,xpos,ypos,treshmin,treshmax,SOBELIM=sb_im) $
        else region = 0
     print,'trouve:',N_ELEMENTS(region)
     loc = Fit_Ellipse(region,xsize=1024,ysize=1024,NPOINTS=30,CENTER=center)
     dist = SQRT(LONG(512-center(0))^2+LONG(512-center(1))^2)
     IF dist LE 0.95*420 and N_ELEMENTS(region) GT 1 THEN im_seed2(region)=1.
   ENDIF
ENDFOR

im_res=im_seed2+im_seed
window,1,xsize=1024,ysize=1024
tvscl,median(im_res,3)
;window,2,xsize=1024,ysize=1024
;tvscl,im
goto,ende
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ra = 420
;xc = 512
;yc = 512

im_res = im
mask = WHERE(im EQ 0.)

;HISTO STRETCH
 GOTO,endhisto

 nbins = 1000
 hist = HISTOGRAM(im,NBINS=nbins,MAX=MAX(im),MIN=0)
 hist(0) = 0
 ntot = long(1024)*long(1024)
 lim = ntot/(10*nbins)
 tt = WHERE(hist EQ MAX(hist))
 tt0=tt & tt1=tt

 WHILE hist(tt0) GE lim AND tt0 GT 0 DO BEGIN
   tt0=tt0-1
 ENDWHILE  

 WHILE hist(tt1) GE lim AND tt1 LT nbins DO BEGIN
   tt1=tt1+1
 ENDWHILE  

 hist0=hist(tt0:tt1-1)

 mini = tt0*(MAX(im)/nbins)
 maxi = tt1*(MAX(im)/nbins)
 im_new=BYTSCL(im,mini,maxi)
endhisto:

;first treshold
tresh = MEDIAN(im(WHERE(im)))/1.5
im_res(WHERE(im LT tresh AND im NE 0.,COMPLEMENT=bigv)) = 1.
im_res(bigv) = 0.
im_res(mask) = 0.

im_labl = LABEL_REGION(im_res)
im_seed = im
im_seed(*,*) = 0.  
hist_labl = HISTOGRAM(im_labl,REVERSE_INDICES=r)
FOR i=0, N_ELEMENTS(hist_labl)-1 DO BEGIN
   IF hist_labl(i) GE 10 AND hist_labl(i) LT 10000 THEN BEGIN
     PRINT, 'Region ',i, ', Population = ', hist_labl(i)
     p = r(r[i]:r[i+1]-1)
     q = im(p)
     treshmax = MAX(q)*1.05
     treshmin = MIN(q)
     mini = WHERE(q EQ MIN(q))
     seed = p(mini(0))
     ypos = FIX(seed/1024.)
     xpos = seed - long(1024)*ypos
     IF SQRT((512-xpos)^2+(512-ypos)^2) LE 0.95*420 then $
;IF im_new(seed) EQ 0. THEN BEGIN 
     region  = SEARCH2D(im,xpos,ypos,treshmin,treshmax) $ 
     else region=im(0,0)
;     morepix = 1
;     region = [seed]
;     WHILE morepix NE 0 DO BEGIN
;       nbg = im(xpos-2:xpos+2,ypos-2:ypos+2)
;     ENDWHILE
     print,'trouve:',N_ELEMENTS(region)
     loc = Fit_Ellipse(region,xsize=1024,ysize=1024,NPOINTS=30,CENTER=center)
     dist = SQRT((512-center(0))^2+(512-center(1))^2)
     IF dist LE 0.95*420 THEN im_seed(region)=1.
     im_seed(region)=1.
     ;IF N_ELEMENTS(region) LT 500 THEN im_seed(region)=1.
;ENDIF
   ENDIF
ENDFOR

window,1,xsize=1024,ysize=1024
tvscl,im_seed
;window,2,xsize=1024,ysize=1024
;tvscl,im



;pix_dist = 4
;FOR ii=0+pix_dist,1024-pix_dist-1 DO BEGIN
;  FOR jj=0+pix_dist,1024-pix_dist-1 DO BEGIN
;     IF im_res(ii,jj) EQ 1 THEN BEGIN
;       
;     ENDIF
;  ENDFOR
;ENDFOR

ende:
return,im_res
END


FUNCTION GROWING,image,xi,yi,mini,maxi,SOBELIM=sobelim

  cp_im  = image
  xsize  = (SIZE(image))(1)
  subs0  = xi+yi*xsize
  subs   = subs0
  blob   = subs0
  count  = 1
  cp_im(subs0) = 0

WHILE count NE 0 DO BEGIN

  subt = 0
  ii   = 0L

  WHILE ii LT count DO BEGIN 

      IF NOT KEYWORD_SET(SOBELIM) THEN BEGIN
          subsi = ADJ(cp_im,subs(ii),mini,maxi,xsize)
      ENDIF ELSE BEGIN
          subsi = ADJ(cp_im,subs(ii),mini,maxi,xsize,SOBELI=sobelim)
      ENDELSE
      IF subsi(0) EQ -1 THEN BEGIN
        subt = subt
      ENDIF ELSE BEGIN 
        cp_im(subsi) = 0
        subt = [subt,subsi]
      ENDELSE
      ii=ii+1

  ENDWHILE

      nsubt = N_ELEMENTS(subt)
      IF nsubt GT 1 THEN BEGIN
         subt  = subt(1:nsubt-1)
         blob  = [blob,subt]
         count = (SIZE(subt))(1)
         subs  = subt
      ENDIF ELSE count = 0

ENDWHILE

RETURN,blob
END

;Compute the subscript of pixels within the range
;[mi,ma] around orig (one-adjacency)

FUNCTION ADJ,image2,orig,mi,ma,xs,SOBELI=sobeli

          pm = orig-xs
          pp = orig+xs
          one_adj = [pm-1,pm,pm+1,orig-1,orig+1,pp-1,pp,pp+1]

          IF KEYWORD_SET(SOBELI) THEN BEGIN
            tresh_sbmi = 15;MAX(sobelim)/2.
            tresh_sbma = 200
            goodpix = WHERE((image2(one_adj) LE ma AND image2(one_adj) GE $
            mi) AND (sobeli(one_adj) GE tresh_sbmi AND sobeli(one_adj) LE tresh_sbma),cc)
          ENDIF ELSE BEGIN
            goodpix = WHERE(image2(one_adj) LE ma AND image2(one_adj) GE mi,cc)
          ENDELSE
      
          IF cc GT 0 THEN pix_subs = one_adj(goodpix) $
          ELSE pix_subs = -1 

RETURN,pix_subs
END