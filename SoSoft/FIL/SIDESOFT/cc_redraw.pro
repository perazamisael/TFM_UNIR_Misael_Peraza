PRO CC_REDRAW,tab,locs,pa,SKE=ske,BND=bnd,BOTH=both

; This procedure plots filaments on the original image
; and displays both original and redraw 
; It should be called by FIL_ASCII_read and it calls 
; CHAIN2IND.pro 
; NF 2004

;INPUT:
;       tab     : Array red by FIL_ASCII_read
;       pa      : User path (ex:/data2/smith/FITS)
;       locs    : The chosen filaments defined in FIL_ASCII_read 

;DISPLAYS:
;       Original image and redraw image resized to a 1024*512 window

;KEYWORDS
;       SKE     : display filaments skeleton 
;       BND     : display filaments boundary
;       BOTH    : display both skeleton and boundary


  nloc   = N_ELEMENTS(locs)
  pfullname = ''
  nn = 1
  pfile = tab[12,locs[0]]

  FOR ii = 1,nloc-1 DO BEGIN
      file = tab[12,locs[ii]]
      IF file EQ pfile THEN nn = nn ELSE nn = nn + 1  
      pfile = file
  ENDFOR

  beg = 0
  aa = 1


  FOR jj=0,nn-1 DO BEGIN

        file  = tab[12,locs[beg]]
        nfile = tab[12,locs[*]]
        wfil  = WHERE(nfile EQ file,nfil)

        fullname = pa+file
        ;fitsread_win,header,im,FILENAME=fullname
        im = READFITS(fullname,head)
        im_ori = im         
        date_obs = tab[1,locs[beg]]
        disp = MAX(im_ori)

    FOR kk = 0,nfil-1 DO BEGIN

       pos        = locs[wfil[kk]]
       ccstr      = tab[37,pos]
       ccstrske   = tab[46,pos]
       ccpix_x    = tab[33,pos]
       ccpix_y    = tab[34,pos]
       ccpixske_x = tab[42,pos]
       ccpixske_y = tab[43,pos]
       nax1       = tab[5 ,pos]
       nax2       = tab[6 ,pos]
  
       ind    = CHAIN2IND([ccpix_x,ccpix_y],ccstr,nax1,nax2)
       indske = CHAIN2IND([ccpixske_x,ccpixske_y],ccstrske,nax1,nax2)

       IF KEYWORD_SET(ske) OR KEYWORD_SET(both) THEN im[indske] = disp
       IF KEYWORD_SET(bnd) OR KEYWORD_SET(both) THEN im[ind]    = 0.


    ENDFOR

      WINDOW,aa,XS=1024,YS=512,TITLE='original and redraw '+STRTRIM(date_obs,2)
      TVSCL,REBIN(im,512,512)
      TVSCL,REBIN(im_ori,512,512),512,0

      aa  = aa + 2
      beg = beg + nfil

  ENDFOR

END



