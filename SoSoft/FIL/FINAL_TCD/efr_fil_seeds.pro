
;+
; NAME:
;       efr_fil_seeds
;
; PURPOSE:
;
;       From the input array, find seed points to be
;       used by the region growing function
;
; AUTHOR:
;
;       Fuller Nicolas
;       LESIA / POLE SOLAIRE
;       Observatoire de MEUDON
;       5 place Jules Janssen
;       92190 MEUDON
;       E-mail: nicolas.fuller@obspm.fr
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Cleaning software
;
; CALLING SEQUENCE:
;
;        res = efr_fil_seeds(im_IN,diam,coeff)
;
; INPUTS
;
;        im_IN : input n x n array 
;        diam  : Sun diameter in pixels
;        coeff : A coefficient to determine the threshold level within
;                the square windows covering the Sun disk 
;
;
; OUTPUTS:
;
;       segmented image - n x n array (seeds pixels are set to 1
;                         and others to 0)  
;
;
;
; MODIFICATION HISTORY:
;
;  NF may 2004: remove EFR_LIMBSYM (incorporate in efr_filament)
;-


;###########################################################################

FUNCTION EFR_FIL_SEEDS,im_IN,diam,coeff

;###########################################################################



  im_OUT = im_IN*0b
  xsize  = (SIZE(im_IN))[1]
  ysize  = (SIZE(im_IN))[2]


  ;##### Compute stats
  mom    = MOMENT(im_IN[WHERE(im_IN)],SDEV=sdev)


  ;##### windows size
  div   = 7
  wdth  = LONG(xsize/div)


  ;##### Loop over the windows
  FOR ii = 0,wdth*div-1,wdth DO BEGIN

     FOR jj = 0,wdth*div-1,wdth DO BEGIN
 
        ;#### define the window content 
        win  = im_IN[ii:ii+wdth-1,jj:jj+wdth-1]

        ;#### We need enough points to compute stats (if square close to limb)
        wwin  = WHERE(win,nwwin)
        IF nwwin GT (wdth*wdth/10.) THEN BEGIN   

           ;qsi   = MEDIAN(win[wwin])
           ;sdevi = SQRT(1./(N_ELEMENTS(win[wwin])-1)*TOTAL((win[wwin]-qsi)^2))
           qsi = (MOMENT(win[wwin],SDEV=sdevi))[0]
;print,'#########',ii,jj
;print,qsi,sdevi
          ;#### discard highest and lowest values from 
          ;#### threshold calculation (quiet sun)
          wvals = WHERE(win GT qsi - 2.*sdevi AND $
                        win LT qsi + 2.*sdevi,nvals)
            
          ;#### Mean and std dev of the 'Quiet Sun'
          momi = MOMENT(win[wvals],SDEV=sdevi2)
;print,momi[0],sdevi2
          ;#### Window Treshold
          threshi = momi[0] - coeff*sdevi2 > 1
;print,threshi
          ;#### Apply Treshold
          fil = WHERE(win le threshi AND win GT 0.,nfil)


          ;#### Get back to the coordinates of the whole image
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
  mask         = EFR_ROUNDMASK(xsize,ysize,0.,diam/2.-6,COMP=mask_comp)
  im_OUT[mask_comp] = 0b

RETURN,im_OUT
END






