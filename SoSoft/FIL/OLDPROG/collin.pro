
FUNCTION COLLIN,INPUT=input

  diam = 840

  ;####GET THE INPUT ARRAY
  IF NOT KEYWORD_SET(INPUT) THEN BEGIN
    file = DIALOG_PICKFILE(PATH='/home/fuller/poub/SAV/',FILTER='*_p.sav')
    IF file NE '' THEN BEGIN
      RESTORE,file
      input = arr
    ENDIF ELSE GOTO,end_filament
  ENDIF

  Xsiz = (SIZE(input))(1)
  Ysiz = (SIZE(input))(2)
  
  input=FIX(input)

  ;####TEST THE SIZE OF INPUT
  IF Xsiz NE Ysiz OR Xsiz NE 1024 THEN BEGIN
     mess = DIALOG_MESSAGE('Not a 1024x1024 array, resize ?',/QUESTION)
     IF mess EQ 'Yes' THEN BEGIN
        Xsiz  = 1024
        Ysiz  = 1024
        input = CONGRID(input, Xsiz, Ysiz)
     ENDIF ELSE GOTO,end_filament
  ENDIF

  WINDOW,0,XSIZE=Xsiz,YSIZE=Ysiz
  TVSCL,input

  ;####CREATE SOME MASKS
  mask_SUBS   = WHERE(input*0+1)
  mask_SUBS_Y = mask_SUBS /   Xsiz
  mask_SUBS_X = mask_SUBS MOD Xsiz
  mask_DIST   = SQRT((mask_SUBS_X-Xsiz/2)^2 + (mask_SUBS_Y-Ysiz/2)^2) 
  mask_0      = WHERE(mask_DIST GE (diam/2))
  mask_1      = WHERE(mask_DIST GE (diam/2)*0.99)

  ;####APPLY A MEDIAN FILTER AND DIVIDE INPUT BY THE RESULT
  im_REB = REBIN(input, Xsiz/2, Ysiz/2)
  im_MED = MEDIAN(im_REB, 30)
  im_MED = REBIN(im_MED, Xsiz, Ysiz)
  im_DIV = input/(im_MED+1)

  im_DIV(mask_1) = 0
  ;TVSCL,im_DIV

  ;####ENHANCE THE CONSTRAST WITH HIST_EQUAL
  im_HEP  = HIST_EQUAL(im_DIV,PERCENT=1) + 1
  im_HEP(mask_1) = 0
  ;TVSCL,im_HEP

  res = input*0b
  X0 = diam/2
  Y0 = X0
  base   = WHERE(input*0+1)
  base_Y = base /   Xsiz
  base_X = base MOD Xsiz
  base_DIST = SQRT((base_X-Xsiz/2)^2 + (base_Y-Ysiz/2)^2)

  FOR ii = 5,diam/2-6,5 DO BEGIN
    ray = ii
    base_CIRC = WHERE(base_DIST GT ray-5 AND base_DIST LE ray+5)
    data = im_HEP(base_CIRC)
    noseedata = data(WHERE(data GT 5))
    mean_data = TOTAL(noseedata)/N_ELEMENTS(noseedata)
    std_data = STDDEV(noseedata)
    good = WHERE(data LE mean_data - 2.0*std_data,nb)
    IF nb GE 1 THEN BEGIN
      res(base_CIRC(good))=1b
    ENDIF
    TVSCL,res
  ENDFOR

  
RETURN,res

end_filament: 
END


;####################################################






