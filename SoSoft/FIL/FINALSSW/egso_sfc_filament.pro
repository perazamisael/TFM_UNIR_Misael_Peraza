
;+
; NAME:
;       egso_sfc_filament
;
; PURPOSE:
;
;       From a standardized Ha solar image, (round and centered 
;       solar disk), this program automatically extract dark regions
;       (mainly filaments) using a hybrid region growing method. More
;       explainations couls be found in 'Filament Recognition And
;       Image Cleaning On Meudon Halpha Spectroheliograms,
;       Sol. Phys. 2005, Fuller N., Aboudarham J. and Bentley R.B.'
;
;       NOTE1: standardized images from EGSO/cleaning code are
;       1024*1024 images with Sun centered at (511.5,511.5) with a
;       diameter of 840 pixels
; 
;       NOTE2: For optimal results, one should apply
;       EGSO_SFC_FLATTEN.pro to he image before using this program,
;       especially if the image shows significant background variations
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
;       bin_image=EGSO_SFC_FILAMENT(std_image[,DISPLAY=display,OBSKEY=obskey
;       ,FLATCLEAN=flatclean,LINECLEAN=lineclean,DUSTCLEAN=dustclean])
;
; INPUT
; 
;       input : 1024 x 1024 image
;
; OUTPUTS:
;
;       segmented image - 1024 x 1024 binary (filaments pixels are set to 1
;                         and others to 0)  
;
; INPUT KEYWORDS:
;
;       DISPLAY :  display different stages of processing image
;
;       OBSKEY  :  Some coefficients can be changed to better fit a
;                  specific kind of observation. Sets of coefficients
;                  are available for:
;                  1. Meudon Photographic plate obs: OBSKEY='meu1'
;                  2. Meudon CCD camera obs.         OBSKEY='meu2' (def)
;                  3. BBSO CCD camera obs.           OBSKEY='bbso'
;                  4. YNAO CCD camera obs.           OBSKEY='ynao'
;
;       FLATCLEAN: Flatten background intensity, recommended
;                  for better results, mandatory on some images
;
;       LINECLEAN: Use only if the image shows dark lines across
;                  the Sun disk
;       
;       DUSTCLEAN: Use only if the image shows high gradient pixels
;                  values due to dust paricles for example       
;    
; OUTPUT KEYWORDS:
;
;       CLEANIM: a cleaned image, useful if flatclean, lineclean or
;                dustclean are set.
; 
; CALLING EXAMPLES
;
;    bin_image = EGSO_SFC_FILAMENT(std_image,OBSKEY='bbso',/FLAT,/DISP,
;                CLEANIM=cleanedimage)
;    bin_image = EGSO_SFC_FILAMENT(std_image,OBSKEY='meu1',/LINE,/DIS)
;
; MODIFICATION HISTORY:
;
;  NF Mar 2005: last revision
;              
;-


;###########################################################################

FUNCTION EGSO_SFC_FILAMENT,input,DISPLAY=display,OBSKEY=obskey,FLATCLEAN=flatclean,LINECLEAN=lineclean,DUSTCLEAN=dustclean,CLEANIM=cleanim

;###########################################################################


  ;####FUNCTION PARAMETERS CHECK
   IF N_PARAMS() NE 1 THEN BEGIN
      PRINT,'EGSO_SFC_FILAMENT calling example: res=EGSO_SFC_FILAMENT(image,/DISP,/FLAT,OBSK=''meu1'')'
      RETALL
   ENDIF


  ;####IDL VERSION RELEASE CHECK
  version = FLOAT(!VERSION.RELEASE)
  IF version LT 5.5 THEN BEGIN
    PRINT,'EGSO_SFC_FILAMENT: Use IDL5.5 or greater'
    RETALL
  ENDIF
 

  ;####CHECK IMAGE SIZE
  Xsiz = (SIZE(input))[1]
  Ysiz = (SIZE(input))[2]
  IF Xsiz NE 1024 AND Ysiz NE 1024 THEN BEGIN
    PRINT,'EGSO_SFC_FILAMENT: input should be a 1024*1024 standardized image'
    RETALL
  ENDIF


  ;####DEFAULT SUN DIAMETER
  diam = 840


  ;####OPTIONAL DISPLAY
  IF KEYWORD_SET(display) THEN BEGIN
    WINDOW,/FREE,XSIZE=Xsiz,YSIZE=Ysiz/2
    TVSCL,REBIN(input,Xsiz/2,Ysiz/2)
  ENDIF


  ;####DEFINE COEFFICIENT FOR: 
  ;## 1.Laplacian filtering
  ;## 2.Seeds Threshold
  ;## 3.Region Threshold 
  IF KEYWORD_SET(OBSKEY) THEN BEGIN

   RESTORE,'egso_sfc_obs_param.sav'
   wobs = WHERE(obs_param.obs EQ STRCOMPRESS(obskey,/REM),nmatch)
   IF nmatch EQ 0 THEN RETALL
   coefflapl   = obs_param[wobs].lapl
   coeffseeds  = obs_param[wobs].seeds
   coeffregion = obs_param[wobs].regiongrow

  ENDIF ELSE BEGIN 

   coefflapl   = 3.0 
   coeffseeds  = 2.0;3.0
   coeffregion = 2.0

  ENDELSE


  ;####COPY INPUT
  im_FLT = input


  ;####OPTIONALY CLEAN THE IMAGE (INTENSITY/LINES/DUST)
   IF KEYWORD_SET(lineclean) AND KEYWORD_SET(flatclean) THEN $
      im_FLT = EGSO_SFC_LINE_REMOVE(input,diam,MAIND=maind) 
   IF KEYWORD_SET(flatclean) AND NOT KEYWORD_SET(lineclean) THEN $
      im_FLT = EGSO_SFC_FLATTEN(input,diam)
   IF KEYWORD_SET(lineclean) AND NOT KEYWORD_SET(flatclean) THEN $
      im_FLT = EGSO_SFC_LINE_REMOVE(input,diam,/noflatres,MAIND=maind)  
   IF KEYWORD_SET(dustclean) THEN im_FLT = EGSO_SFC_DUST_CLEAN(im_FLT)


  ;####CLEANED IMAGE OUTPUT
   cleanim = im_FLT



  ;####OPTIONAL DISPLAY
  IF KEYWORD_SET(display) THEN BEGIN
     TVSCL,REBIN(im_FLT,Xsiz/2,Ysiz/2)
  ENDIF


  ;####EXTEND THE DISK WITH LIMB SYMETRY TO AVOID BORDER EFFECTS
  im_FLT  = EGSO_SFC_LIMBSYM(im_FLT,diam/2.-1,(Xsiz-diam)/2.,/INV)


  ;####SUBTRACT THE LAPLACIAN OF IMAGE TO SHARPEN THE EDGES
  IF coefflapl NE 0 THEN BEGIN


      ;####COMPUTE THE SHARPEN IMAGE
      im_LAPL = EGSO_SFC_SHARPEN(TEMPORARY(im_FLT),coefflapl)
   

      ;####KEEP THE SAME INTENSITY RANGE AND DISCARD NEG. VALUES
      neg      = WHERE(im_LAPL LT 0.,nneg)
      IF nneg GT 0 THEN im_LAPL[neg] = 1./10E3
      im_LAPL = im_LAPL/coefflapl


      ;####MEDIAN FILTER TO REDUCE THE NUMBER OF SEEDS
      im_LAPL = MEDIAN(im_LAPL,3)  


      ;####OPTIONAL DISPLAY
      IF KEYWORD_SET(display) THEN BEGIN
         TVSCL,REBIN(im_LAPL,Xsiz/2,Ysiz/2)
      ENDIF

   ENDIF ELSE im_LAPL = TEMPORARY(im_FLT)


  ;####FIND THE SEEDS
   im_SEED = EGSO_SFC_FIL_SEEDS(im_LAPL,diam,coeffseeds)


  ;####OPTIONAL DISPLAY
   IF KEYWORD_SET(display) THEN BEGIN
      TVSCL,REBIN(im_SEED,Xsiz/2,Ysiz/2)
   ENDIF


  ;####REGION GROWING FUNCTION
   res = EGSO_SFC_FIL_REGION_GROW(TEMPORARY(im_LAPL),TEMPORARY(im_SEED),diam,coeffregion)


  ;####OPTIONAL DISPLAY
   IF KEYWORD_SET(display) THEN BEGIN
     TVSCL,REBIN(input,Xsiz/2,Ysiz/2)
     sub = WHERE(res)
     imi = input
     imi[sub] = 0.
     TVSCL,REBIN(imi,Xsiz/2,Ysiz/2),Xsiz/2,0
     imi = 0
   ENDIF


RETURN,res

END


;####################################################








