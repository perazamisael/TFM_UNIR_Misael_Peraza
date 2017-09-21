;+
; NAME:
;         EGSO_SFC_LAKE2BAY
;
; PURPOSE:
;         Before computing the boundary of the shape
;         (indices) we must check the presence 
;         of lake-like filaments (which lead to 2 different
;         boundaries, outer and inner). The size of the gap
;         which defines a lake instead of a gap that should
;         be filled-in is set via ratio keyword. Other
;         lakes are changed in bays by removing pixels.         
; 
;  EX (only for illustration):
;  
;     xxx             xxx
;    xxxxxx          xxxxxx
;    xxx000x   -->   xxxxxxx   fill the gap
;     xx00x           xxxxx  
;      xxx             xxx
; 
;     xxxx              xxxx             
;    x0000x            x0000x        
;    x0000x    -->     x00000  lake to bay (quite rare!)
;     x000x             x000x          
;      xxx               xxx           
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
;
; CALLING SEQUENCE:
;
;         res = egso_sfc_lake2bay(indices,xsize,ysize[,RATIO=ratio])
;
; INPUT
;         indices      subscripts of non-null pixels
;         xsize        1st dim of the array
;         ysize        2nd dim of the array
;
; INPUT KEYWORDS
;         RATIO        ratio btw the gap size (GS) and the size of
;                      the feature (FS: number of subscripts)
;                      If GS/FS is bigger than 'ratio' then the
;                      gap is kept. (default is 1/3.)     
;
; OUTPUTS
;         Output is the filled-in or modified feature subscripts array
;
; HISTORY:
;
;      NF last rev. Mar 2005
;-

FUNCTION EGSO_SFC_LAKE2BAY,indices,xsize,ysize,RATIO=ratio


  IF NOT KEYWORD_SET(ratio) THEN ratio = 1/3.  

  mask = BYTARR(xsize,ysize)
  mask[indices] = 1b

  ;#### Image's negative (combined with label_region
  ;#### we can get the size of feature gaps)
   mask_neg = mask+255b


  ;#### Compute the number of regions within
  ;#### the image's negative (backgrnd = 1 region)
   reg = LABEL_REGION(mask_neg)
   h_reg = HISTOGRAM(reg,reverse_indices=rev)
   nbreg = N_ELEMENTS(h_reg) 


  ;#### Compute the number of region within
  ;#### the eroded image's negative to deal with
  ;#### limit cases (to avoid pbs with the external 
  ;#### boundary definition) 
   mask_neg_e = ERODE(mask_neg,REPLICATE(1,3,3))
   regb = LABEL_REGION(mask_neg_e)
   h_regb = HISTOGRAM(regb,reverse_indices=revb)
   nbregb = N_ELEMENTS(h_regb)

   flag = 0
   IF nbreg EQ 2 AND nbregb EQ 2 THEN RETURN,indices ELSE BEGIN
     FOR uu = 0, nbregb-1 DO BEGIN
         indregb = revb[revb[uu]:revb[uu+1]-1]

         ;#### Ratio condition and
         ;#### make sure indreg is not background
         IF N_ELEMENTS(indregb) LT N_ELEMENTS(indices)*ratio $
         AND WHERE(indregb EQ 0) EQ -1 THEN BEGIN
             flag = 1  
             mask[indregb]=1b
         ENDIF

     ENDFOR
     ;#### Fill the zeros btw the original (not eroded) 
     ;#### and the filled regions or make a bay from a lake
     IF flag EQ 1 THEN BEGIN
         mask = MORPH_CLOSE(mask,REPLICATE(1,3,3))
     ENDIF ELSE BEGIN
       indgap = 0
       FOR vv = 0, nbreg-1 DO BEGIN
         indreg = rev[rev[vv]:rev[vv+1]-1]
         IF WHERE(indreg EQ 0) EQ -1 AND mask_neg[indreg[0]] NE 0b $
         THEN indgap=indreg
       ENDFOR
         IF N_ELEMENTS(indgap) EQ 1 THEN RETURN,indices       
         xp = MEAN(indgap MOD xsize)
         yp = MEAN(indgap / FIX(xsize))
         mask[xp:xsize-1,yp-1:yp+1]=0b ;should be optimized
     ENDELSE

  ENDELSE

  indices = WHERE(mask)

RETURN,indices
END






