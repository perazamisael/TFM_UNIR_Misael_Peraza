FUNCTION CHAIN2IND,first,chainc,xsize,ysize
  
   ind=first[1]*LONG(xsize)+first[0]
   length=STRLEN(chainc)
   ;set=[1,xsize+1,xsize,xsize-1,-1,-xsize-1,-xsize,-xsize+1];trigo start right
   set=[-1,-xsize-1,-xsize,-xsize+1,1,xsize+1,xsize,xsize-1];trigo start left 
   newind=ind
   FOR jj=0,length-1 DO BEGIN
      next=FIX(STRMID(chainc,jj,1))
      newind=newind+set[next]
      ind=[ind,newind]
   ENDFOR      
   
RETURN,ind
END


FUNCTION ASCII2FIL,FILE=file,SKE=ske,BOUND=bound,IM_2K=im_2k,THICK=thick,PP_DISP=pp_disp

  ;From ascii files written by efr_fil2ascii.pro, extract boundary and/or
  ;skeleton positions to superimpose them on a Sun shape defined below:
  ;The standard Sun : sunrad 420 , centered at (511.5,511.5)
  ;OR, for 2K*2K images : sunrad 840 , centered at (1023.5,1023.5)
  ;
  ;KEYWORDS:
  ;  FILE: name and path of the ascii file '*_feat.txt'
  ;  SKE: extract skeleton data
  ;  BOUND: extract boundary data
  ;  IM_2K: 2048*2048 images (BBSO for example) instead of 1024*1024
  ;  THICK: to draw skeleton or boundary with a larger pen 
  ;  PP_DISP: instead of a sun shape, display results on the
  ;                  preprocessed image


   ;### change the following path to your path (for the dialog pickfile):
   path='/home/fuller/poub/SAV/FIL/DAILY_ASCII/'


  ;### Select a file if none was specified with FILE keyword
   IF NOT KEYWORD_SET(file) THEN BEGIN
     file = DIALOG_PICKFILE(PATH=path,FILTER='*_feat.csv')
   ENDIF 

   ;### Read the ascii files
   tabfeat = RD_TFILE(file,/AUTOCOL,DELIM=';',/HSKIP,HEADER=headf)
   headf  = STRSPLIT(headf,';',/EXTRACT)

  ;### Build a sun shape
   rsun = 420
   Xsiz = 1024
   Ysiz = 1024
   IF KEYWORD_SET(IM_2K) THEN BEGIN
      rsun = 840
      Xsiz = 2048
      Ysiz = 2048
   ENDIF
   mask_0 = EFR_ROUNDMASK(Xsiz,Ysiz,0.,rsun,COMP=mask_comp)

  ;### Define the output array
   output = BYTARR(Xsiz,Ysiz) + 0b

  ;### Number of features
   sizt = SIZE(tabfeat)
   IF sizt[0] EQ 2 THEN ngd = sizt[2] ELSE ngd = 1

  ;### Get the parameters positions in the header
  ;### for the boundary chain code
   wwfpxb = WHERE(headf EQ 'CC_X_PIX')
   wwfpyb = WHERE(headf EQ 'CC_Y_PIX')
   wwchainb = WHERE(headf EQ 'CC')

  ;### Get the parameters positions in the header
  ;### for the skeleton chain code
   wwfpxs = WHERE(headf EQ 'SKE_CC_X_PIX')
   wwfpys = WHERE(headf EQ 'SKE_CC_Y_PIX')
   wwchains = WHERE(headf EQ 'SKE_CC')

  ;### Loop on each filament
  FOR ii=0,ngd-1 DO BEGIN

    IF KEYWORD_SET(BOUND) THEN BEGIN    
      ;#### Get the chain code and the first point
      ;#### coordinates in pixels from the ascii file
      schain_b = (tabfeat[wwchainb,ii])[0]
      xpos_b = FIX((tabfeat[wwfpxb,ii])[0])
      ypos_b = FIX((tabfeat[wwfpyb,ii])[0])
      indices_b = CHAIN2IND([xpos_b,ypos_b],schain_b,Xsiz,Xsiz)
      output[indices_b] = 1b
      output[xpos_b,ypos_b] = 1b  
    ENDIF 

    IF KEYWORD_SET(SKE) THEN BEGIN
     ;#### Get the chain code and the first point
     ;#### coordinates in pixels from the ascii file
     schain_s = (tabfeat[wwchains,ii])[0]
     xpos_s = FIX((tabfeat[wwfpxs,ii])[0])
     ypos_s = FIX((tabfeat[wwfpys,ii])[0])
     indices_s = CHAIN2IND([xpos_s,ypos_s],schain_s,Xsiz,Xsiz)
     output[indices_s] = 1b
     output[xpos_s,ypos_s] = 1b  
    ENDIF

  ENDFOR

  ;### Make lines thicker  
  IF KEYWORD_SET(THICK) THEN BEGIN
    IF KEYWORD_SET(IM_2K) THEN BEGIN
         output = DILATE(output,REPLICATE(1,5,5))
    ENDIF ELSE BEGIN
         output = DILATE(output,REPLICATE(1,3,3))
    ENDELSE
  ENDIF


  ;### Optionaly superimpose detection on preprocessed image
   IF KEYWORD_SET(PP_DISP) THEN BEGIN
       ;### find name of the preprocessed file
       fbase = FILE_BASENAME(file,'_feat.csv')
       fdir  = FILE_DIRNAME(file,/MARK_DIRECTORY)
       ppascii = fdir+fbase+'_pp.csv'
       fs = FILE_SEARCH(ppascii,count=ccnt)
       IF ccnt GT 0 THEN BEGIN
          tabpp = RD_TFILE(ppascii,/AUTOCOL,DELIM=';',/HSKIP,HEADER=headpp)
          headpp  = STRSPLIT(headpp,';',/EXTRACT)
          wwpploc = WHERE(headpp EQ 'LOC_FILE')
          pploc = (tabpp[wwpploc,0])[0]
          ;#### Read the preprocessed FITS file
          IF STRPOS(pploc,'processed') NE -1 THEN BEGIN
             ppim = READFITS(pploc)
          ENDIF ELSE BEGIN
             PRINT,'Sorry,cannot find the preprocessed FITS'
             PP_DISP=0
          ENDELSE
       ENDIF ELSE BEGIN
             PRINT,'Sorry,cannot find '+ppascii
             PRINT,'-> Cannot display preprocessed FITS fimage'
             PP_DISP=0
        ENDELSE
   ENDIF 


   IF NOT KEYWORD_SET(PP_DISP) THEN BEGIN
      ;### Add the sun shape
      wwtemp = WHERE(output) 
      output[mask_0] = 2b
      output[wwtemp] = 0b
      output[0,0] = 3b
   ENDIF ELSE BEGIN
     ;### Display the fits image
      wwtemp = WHERE(output) 
      output = ppim
      output[wwtemp] = 0b
   ENDELSE

RETURN,output
 
END
