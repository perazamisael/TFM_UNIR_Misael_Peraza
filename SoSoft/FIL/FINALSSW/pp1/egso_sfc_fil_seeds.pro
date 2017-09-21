
;+
; NAME:
;       egso_sfc_fil_seeds
;
; PURPOSE:
;
;       From a standardized Halpha Sun image, find seed points to be
;       used by the region growing function to detect filaments
;
;       NOTE1: standardized images from EGSO/cleaning code are
;       1024*1024 images with Sun centered at (511.5,511.5) with a
;       diameter of 840 pixels
; 
;       NOTE2: This program should be called by EGSO_SFC_FILAMENT. If
;       not, the input image might be first flatten
;       (egso_sfc_flatten), and enlarged (egso_sfc_limbsym)
;
; AUTHOR:
;
;       Fuller Nicolas
;       LESIA / POLE SOLAIRE
;       Observatoire de MEUDON
;       5 place Jules Janssen
;       92190 MEUDON
;       E-mail: nicolas.fuller@obspm.fr
;       copyright (C) 2005 Nicolas Fuller, Observatoire de Paris
;       This program is free software; you can redistribute it and/or modify it under the terms of the
;       GNU General Public License as published by the Free Software Foundation;
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Solar Feature Catalog
;
; CALLING SEQUENCE:
;
;        res = egso_sfc_fil_seeds(im_IN,diam,coeff)
;
; INPUTS
;
;        im_IN : input 1024 x 1024 array 
;        diam  : Sun diameter in pixels
;        coeff : A coefficient to determine the threshold level within
;                the square windows covering the Sun disk 
;
;
; OUTPUTS:
;
;       segmented image - 1024 x 1024 byte array (seeds pixels are set to 1
;                         and others to 0)  
;
;
;
; MODIFICATION HISTORY:
;
;  NF Feb 2005 : last revision
;-


;###########################################################################

FUNCTION EGSO_SFC_FIL_SEEDS,im_IN,diam,coeff

;###########################################################################

  ;#### Function parameters check
   IF N_PARAMS() NE 3 THEN BEGIN
      PRINT,'EGSO_SFC_FIL_SEEDS calling example: res=EGSO_SFC_FIL_SEEDS(image,840,2.7)'
      RETALL
   ENDIF


  ;#### Check image size
   xsize = (SIZE(im_IN))[1]
   ysize = (SIZE(im_IN))[2]
   IF xsize NE 1024 AND ysize NE 1024 THEN BEGIN
     PRINT,'EGSO_SFC_FIL_SEEDS: input should be a 1024*1024 standardized image'
     RETALL
   ENDIF


  ;#### Make the result byte array
   im_OUT = im_IN*0b


  ;##### Compute stats of input image
   mom = MOMENT(im_IN[WHERE(im_IN)],SDEV=sdev)


  ;##### Control the subwindows sizes
   div   = 7
   wdth  = LONG(xsize/div)


  ;##### Loop over the subwindows
  FOR jj = 0,wdth*div-1,wdth DO BEGIN

     FOR ii = 0,wdth*div-1,wdth DO BEGIN
 

        ;#### define the window content 
        win  = im_IN[ii:ii+wdth-1,jj:jj+wdth-1]


        ;#### Need enough points to compute reliable stats
        wwin  = WHERE(win,nwwin)
        IF nwwin GT (wdth*wdth/10.) THEN BEGIN   


           ;#### Stats
            qsi = (MOMENT(win[wwin],SDEV=sdevi))[0]


           ;#### discard highest and lowest values from 
           ;#### threshold calculation (quiet sun)
            wvals = WHERE(win GT qsi - 2.*sdevi AND $
                        win LT qsi + 2.*sdevi,nvals)

            
           ;#### Mean and std dev of the 'Quiet Sun'
            momi = MOMENT(win[wvals],SDEV=sdevi2)


           ;#### Window Treshold
            threshi = momi[0] - coeff*sdevi2 > 1


           ;#### Apply Treshold
            fil = WHERE(win le threshi AND win GT 0.,nfil)


           ;#### Get back to the coordinates in the whole image
            IF fil[0] NE -1 THEN BEGIN
              xxf = FIX(fil MOD wdth) + ii
              yyf = FIX(fil/wdth) + jj
              ind = xxf + yyf*xsize
              im_OUT[ind]= 1b
            ENDIF

        ENDIF

     ENDFOR

  ENDFOR 


  ;##### Set seeds out of the disk or very close to the limb to 0 
  mask = EGSO_SFC_ROUNDMASK(xsize,ysize,0.,diam/2.-6,COMP=mask_comp)
  im_OUT[mask_comp] = 0b

RETURN,im_OUT
END






