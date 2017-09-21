
FUNCTION EFR_FIL_SEEDS_BIS,im_IN,coeff
;overlapping square windows covering the whole image 
;to make a first segmentation


  im_OUT = im_IN*0b 
  xsize  = (SIZE(im_IN))[1]
  mom = MOMENT(im_IN[WHERE(im_IN)],SDEV=sdev,MDEV=mdev)


  ;##### distance between 2 successive windows
  tay = 80L 


  ;##### Loop over the windows
  FOR ii = tay,1024-tay-1,tay DO BEGIN

     FOR jj = tay,1024-tay-1,tay DO BEGIN
 
        ;#### define the window content 
        win  = im_IN[ii-tay:ii+tay-1,jj-tay:jj+tay-1]

        ;#### If the window is close to the limb make sure
        ;#### there are enough points to compute stats
        wwin = WHERE(win,nw)
        IF wwin[0] NE -1 AND nw GT (4*tay*tay)/2 THEN BEGIN

            ;#### Set values greater than (mean+stdev) to 0.
            ;#### Set values lower   than (mean-stdev) to 0.
            momi = MOMENT(win[WHERE(win)],SDEV=sdevi)
            wintmp = win
            wmore = WHERE(wintmp GT momi[0] + 2.*sdevi)
            IF wmore[0] NE -1 THEN wintmp[wmore] = 0.;momi[0]+1.*sdevi
            wless = WHERE(wintmp LT momi[0] - 2.*sdevi AND wintmp GT 0.)
            IF wless[0] NE -1 THEN wintmp[wless] = 0.;momi[0]-1.*sdevi  


            ;#### Then we can compute the std dev of the 'Quiet Sun'
            momi = MOMENT(wintmp[WHERE(wintmp)],SDEV=sdevi,MDEV=mdevi)

            ;#### And get a treshold to find the seeds in the window

            lowtreshi = momi[0] - coeff*sdevi 


            ;#### Treshold
            fil = WHERE(win le lowtreshi AND win GT 0.,nfil)


            ;#### Get back to the coordinates of the whole image
            IF fil[0] NE -1 THEN BEGIN
              xxf = FIX(fil MOD (2*tay)) + (ii-tay)
              yyf = FIX(fil/(2*tay)) + (jj-tay)
              ind = xxf + yyf*xsize
              im_OUT(ind)= 1b
            ENDIF

       ENDIF

     ENDFOR

  ENDFOR 


  ;##### Set limb points to 0 (limb values can be false)
  mask = EFR_ROUNDMASK(xsize,ysize,0.,420*0.99,/INV)
  im_OUT[mask] = 0b


;window,/free,xs=1024,ys=1024
;tvscl,im_OUT


RETURN,im_OUT
END



