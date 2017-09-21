FUNCTION EFR_PIXCUMUL,indices,xsize,ysize

 ;+
 ; NAME:
 ;         EFR_PIXCUMUL
 ; PURPOSE:
 ;         Assign a value to every pixel of a chain
 ;         depending on the count of its non-null
 ;         M-connected neighbors
 ;
 ;         Eg: 000  000  010  001  011  101  111
 ;             010  010  010  010  010  010  111
 ;             000  100  001  101  101  101  111
 ;             ->1  ->2  ->3  ->4  ->4  ->5  ->5
 ;
 ;         In a 1 pixel thick skeleton, end points will
 ;         have value 1, node points value 4 or 5, and
 ;         other points value 3.
 ; 
 ; CALLING SEQUENCE:
 ;         res = efr_pixcumul(indices,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; OUTPUTS
 ;         Output is an array of size (xsize,ysize)
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-

  AA       = indices
  nAA      = N_ELEMENTS(AA)
  mask     = BYTARR(xsize,ysize)
  output   = mask
  mask[AA] = 1b
  pos      = [-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,xsize+1]


  FOR k = 0, nAA-1 DO BEGIN
      N4P = TOTAL(mask[AA[k]+pos[[1,3,4,6]]])
      tot = N4P+1
      IF mask[AA[k]+pos[0]] EQ 1 AND TOTAL(mask[AA[k]+pos[[1,3]]]) EQ 0 THEN $
         tot = tot+1
      IF mask[AA[k]+pos[2]] EQ 1 AND TOTAL(mask[AA[k]+pos[[1,4]]]) EQ 0 THEN $
         tot = tot+1
      IF mask[AA[k]+pos[5]] EQ 1 AND TOTAL(mask[AA[k]+pos[[3,6]]]) EQ 0 THEN $
         tot = tot+1
      IF mask[AA[k]+pos[7]] EQ 1 AND TOTAL(mask[AA[k]+pos[[4,6]]]) EQ 0 THEN $
         tot = tot+1
      output[AA[k]] = tot
  ENDFOR

RETURN,output

END


;###################################################################


FUNCTION EFR_ORDER_IND,indices,xsize,ysize

 ;+
 ; NAME:
 ;         EFR_ORDER_IND
 ; PURPOSE:
 ;         
 ;   Order subscripts of a 1 pixel thick chain (ex:pruned skeleton)
 ;   from a first one to the successive neighbors
 ;   In the case of a boundary (ie no end points), the first point
 ;   is the smallest indice
 ;   Pixels MUST be M-connected             
 ; 
 ; CALLING SEQUENCE:
 ;         res = efr_order_ind(indices,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; OUTPUTS
 ;         Output is the ordered input array
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-


  ind        = indices
  nele       = N_ELEMENTS(ind)
  order      = LONARR(nele,/NOZERO)
  valuesmap  = EFR_PIXCUMUL(ind,xsize,ysize) ;(value 2 or 3)

  IF MAX(valuesmap) GT 3 THEN BEGIN
     PRINT,'Not a pruned or m_connected pixel chain!'
     RETALL
  ENDIF

  where2     = WHERE(valuesmap EQ 2,nv) ;(end points)
  
  ;#### if boundary, change it to skeleton
  IF nv NE 2 THEN BEGIN 
    ind        = ind[1:nele-1]
    nele       = nele-1
    order      = LONARR(nele,/NOZERO)
    valuesmap  = EFR_PIXCUMUL(ind,xsize,ysize) 
    where2     = WHERE(valuesmap EQ 2,nv)
    lake       = 1
  ENDIF ELSE lake = 0

  set = [-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,1+xsize]
  order[nele-1] = where2[1] 
  ep = where2[0]

  FOR ii=0,nele-2 DO BEGIN
   order[ii]     = ep
   valuesmap[ep] = 0
   wnewpt        = WHERE(valuesmap[set+ep])
   ep            = ep + set[wnewpt[0]]
  ENDFOR

  IF lake EQ 1 THEN order=[indices[0],order]

RETURN,order
END

;###################################################################

FUNCTION EFR_RESAMPLE_IND,indices,factor

 ;+
 ; NAME:
 ;         EFR_RESAMPLE_IND
 ; PURPOSE:
 ;         Reduce the number of pixel subscripts by factor         
 ;         Subscripts are picked up at regular interval 
 ;         The new number of subscripts is:
 ;         FIX(N_ELEMENTS(indices)/factor)
 ;
 ; CALLING SEQUENCE:
 ;         res = efr_resample_ind(indices,factor)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         factor       resampling factor
 ;
 ; OUTPUTS
 ;         Output is the resample subscripts array
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-


  nele       = N_ELEMENTS(indices)
  nb_samples = FIX(nele/factor) > 4
  samples    = LONARR(nb_samples)
  samples[0] = indices[0]
  samples[nb_samples-1] = indices[nele-1]
  dist       = (nele-1)*1./(nb_samples - 1)
  pick       = FIX( (FINDGEN(nb_samples-2)+1. )*dist )
  samples[1:nb_samples-2] = indices[pick]

  RETURN,samples

END

;###################################################################
FUNCTION EFR_CURL_IND,indices,xsize,ysize

 ;+
 ; NAME:
 ;         EFR_CURL_IND
 ; PURPOSE:
 ;         Gives an index of how much the skeleton
 ;         is curled up based on the ratio between its length and
 ;         the distance between end points
 ;
 ; CALLING SEQUENCE:
 ;         res = efr_curl_ind(indices,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; OUTPUTS
 ;         Output is the curl index
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-

   nele = N_ELEMENTS(indices)

   x1   = indices[nele-1] MOD xsize & y1 = indices[nele-1]/xsize
   x0   = indices[0]      MOD xsize & y0 = indices[0]/xsize
   dist_extr = SQRT( (x1-x0)^2 + (y1-y0)^2 )

   X0 = indices[0:nele-2] MOD xsize & Y0 = indices[0:nele-2]/xsize
   X1 = indices[1:nele-1] MOD xsize & Y1 = indices[1:nele-1]/xsize
   dist = TOTAL(SQRT( (X1-X0)^2 + (Y1-Y0)^2 ))

   curlind =  10.* (1.- dist_extr/dist) 

RETURN,curlind

END

;###################################################################
FUNCTION EFR_INNER_BOUNDARY,indices,xsize,ysize,ALLDIR=alldir

 ;+
 ; NAME:
 ;         EFR_INNER_BOUNDARY
 ; PURPOSE:
 ;         Find the boundary of the object defined by
 ;         indices using erosion with a structure wich is
 ;         either:
 ;
 ;         010      111
 ;         111  or  111
 ;         010      111
 ; 
 ; CALLING SEQUENCE:
 ;         res = efr_inner_boundary(indices,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; KEYWORDS
 ;         ALLDIR         use 8 adjacency structure
 ;
 ; OUTPUTS
 ;         Output is the boundary subscripts array
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-


    mask          = BYTARR(xsize,ysize)
    mask[indices] = 1b
    
    IF NOT KEYWORD_SET(ALLDIR) THEN BEGIN

        erod=ERODE(mask,[[0,1,0],[1,1,1],[0,1,0]]) ;(4connect)

    ENDIF ELSE BEGIN

        erod=ERODE(mask,REPLICATE(1,3,3)) ;(8connect)

    ENDELSE

    wero = WHERE(erod,nero)
    IF nero NE 0 THEN mask[wero] = 0b
    bound = WHERE(mask)

RETURN,bound
END

;###################################################################

FUNCTION EFR_EXT_BOUNDARY,indices,xsize,ysize,ALLDIR=alldir

 ;+
 ; NAME:
 ;         EFR_EXT_BOUNDARY
 ; PURPOSE:
 ;         Find the external boundary of the object defined by
 ;         indices using subscripts shifting in 4 or 8 directions
 ; 
 ; CALLING SEQUENCE:
 ;         res = efr_ext_boundary(indices,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; KEYWORD
 ;         ALLDIR         Shift in 8 directions instead of 4
 ;        
 ; OUTPUTS
 ;         Output is the boundary subscripts array
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-


 
    ;####work within a larger array to avoid border problems
    xsiz = xsize + 2
    ysiz = ysize + 2
    indx = indices MOD xsize + 1 
    indy = indices / xsize + 1
    indi = indx + xsiz*indy  
    mask = BYTARR(xsiz,ysiz)

    ;####define the shift directions
    set4 = [-xsiz,xsiz,-1,1]
    set8 = [-xsiz-1,-xsiz,-xsiz+1,-1,1,xsiz-1,xsiz,xsiz+1]

    IF NOT KEYWORD_SET(ALLDIR) THEN BEGIN

       ;####shift in 4 directions
       FOR oo=0,3 DO mask[indi+set4[oo]] = 1b

    ENDIF ELSE BEGIN

       ;####shift in 8 directions
       FOR oo=0,7 DO mask[indi+set8[oo]] = 1b

    ENDELSE

    ;####Set the inner pixels to 0
    mask[indi] = 0b
 
    ;####return to original size
    mask       = mask[1:xsiz-2,1:ysiz-2]
    
    ;####get the boundary subscripts
    bound      = WHERE(mask)

RETURN,bound
END


;###################################################################

FUNCTION EFR_CHAIN_CODE,indices,xsize,ysize

 ;+
 ; NAME:
 ;         EFR_CHAIN_CODE
 ; PURPOSE:
 ;         Computes the chain code (freeman's code) of the 
 ;         chain pixels corresponding to indices. Could be
 ;         a boundary or a pruned skeleton
 ;         subscripts MUST be ordered (efr_order_ind)
 ; 
 ; CALLING SEQUENCE:
 ;         res = efr_chain_code(indices,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ;
 ; OUTPUTS
 ;         Output is the chain code array
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-

   
   nele  = N_ELEMENTS(indices)
   chain = BYTARR(nele)+255b

   ;####We compute the coordinates difference corresponding
   ;####to the original subscripts and the shifted subscripts, ie
   ;####btw 1 pixel and its neigbor

   tab1  = SHIFT(indices,-1)
   tab2  = indices
    
   difX  = tab1 MOD xsize - tab2 MOD xsize
   difY  = tab1 / xsize   - tab2 / xsize

   ;####We define numbers corresponding to each direction
   ;####in trigonometric way, starting on the rigth or left
   ;####
   ;####  right 321  left   765  
   ;####        4x0         0x4
   ;####        567         123
   ;####ex: direction 7 corresponds to
   ;####    dx=1  and dy=-1 (right)
   ;####    dx=-1 and dy=1  (left)

   ;trigox = [1, 1, 0,-1,-1,-1, 0, 1] ;trigo starts right
   ;trigoy = [0, 1, 1, 1, 0,-1,-1,-1]

   trigoX = [-1,-1, 0, 1, 1, 1, 0,-1] ;trigo starts left
   trigoY = [ 0,-1,-1,-1, 0, 1, 1, 1]

   FOR ii=0,7 DO BEGIN
    wt = WHERE(difX EQ trigoX[ii] AND difY EQ trigoY[ii],nt)
    IF nt GT 0 THEN chain[wt]=ii
   ENDFOR

   ;####Discard last difference for skeletons
   chain = chain[WHERE(chain NE 255b)]

RETURN,chain

END


;################################################
FUNCTION EFR_SKELETON,indices,xsize,ysize,EC=ec

 ;+
 ; NAME:
 ;         EFR_SKELETON
 ; PURPOSE:
 ;         Erode successively the shape corresponding 
 ;         to the indices until the shape is reduced to
 ;         its one pixel thick skeleton.
 ;         The structures used to erode the shape are
 ;         the following (cf. Hit-or-Miss transform):
 ;          0 0 0     x 0 0
 ;          x 1 x     1 1 0
 ;          1 1 1     x 1 x
 ;         and the four 90° rotations of them
 ;         x stands for "don't care" values
 ; 
 ; CALLING SEQUENCE:
 ;         res = efr_skeleton(indices,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; OUTPUT KEYWORDS
 ;         EC           Set to a named variable that contains
 ;                      the number of erosions
 ;                      needed to fully erode the shape
 ;
 ; OUTPUTS
 ;         Output is the skeleton subscripts array
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-
 
  mask = BYTARR(xsize,ysize)
  mask[indices] = 1b
  ec = 0

  tvscl,mask

  ;####################STRUCTURES
  h0 = [[-1,-1,-1],[0,1,0],[1,1,1]] 
  m0 = [[0,-1,-1],[1,1,-1],[0,1,0]]  
  h1 = ROTATE(h0,1)
  m1 = ROTATE(m0,1) 
  h2 = ROTATE(h0,2)
  m2 = ROTATE(m0,2) 
  h3 = ROTATE(h0,3) 
  m3 = ROTATE(m0,3)

  numb = 1

  ;###############THINNING
  WHILE numb GT 0 DO BEGIN

   wim = WHERE(mask,num1)

   wimC = WHERE(CONVOL(mask,h0) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m0) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h1) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m1) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h2) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m2) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h3) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m3) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wim  = WHERE(mask,num2)
   numb = num1 - num2
   ec   = ec + 1

   TVSCL,mask

  ENDWHILE 

  ec = ec - 1

RETURN,WHERE(mask)
END

;###################################################################
FUNCTION EFR_CHECK_FILLING,indices,xsize,ysize

  ;######avoid gap regions but keep big
  ;######ones in case of lake-like shapes

  mask = BYTARR(xsize,ysize)
  mask[indices] = 1b
  C_mask = ABS(mask-1b) ;complement de mask


  reg = LABEL_REGION(C_mask);,/all_neigh)
  h_reg = HISTOGRAM(reg,reverse_indices=rev)
  nbreg = N_ELEMENTS(h_reg) ;il y a au minimum 2 regions 

    ;####Avoid compatibility pb with external boundary
    ;####there should be no bay shape which are 2 pixels
    ;####far from a lake shape
  regb = LABEL_REGION(ERODE(C_mask,REPLICATE(1,3,3)))
  h_regb = HISTOGRAM(regb,reverse_indices=revb)
  nbregb = N_ELEMENTS(h_regb)


  IF nbreg EQ 2 AND nbregb EQ 2 THEN RETURN,indices ELSE BEGIN
     FOR uu=0,nbregb-1 DO BEGIN
         indreg=revb[revb[uu]:revb[uu+1]-1]
         IF N_ELEMENTS(indreg) LT N_ELEMENTS(indices)/3. $
            AND WHERE(indreg EQ 0) EQ -1 THEN BEGIN
            ;la 1ere cond. assure que la reg. a remplir est au plus
            ;le tiers de la region originale et la 2eme que ce n'est
            ;pas le bord de l'imagette (ne contientpas le point 0,0)
            mask[indreg]=1b
         ENDIF
     ENDFOR
     mask = MORPH_CLOSE(mask,REPLICATE(1,3,3))
  ENDELSE

  indices=WHERE(mask)
RETURN,indices
END


;###################################################################
FUNCTION EFR_M_CONNECT,indices,xsize,ysize,NODISP=nodisp

 ;+
 ; NAME:
 ;         EFR_M_CONNECT
 ; PURPOSE:
 ;         Delete pixels of a one pixel thick chain
 ;         (from indices) to make the chain m_connected
 ;         ex:
 ;
 ;         x              x
 ;          xxx   x        xx0   x
 ;            xx x    -->    x0 x 
 ;             xx             xx
 ;         --------------------------
 ;              xx             xx
 ;             x              x
 ;          xxxx      -->  xxx0
 ;             x              x
 ;              xx             xx            
 ; 
 ; CALLING SEQUENCE:
 ;         res = efr_m_connect(indices,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; KEYWORD
 ;         NODISP       No display      
 ;
 ; OUTPUTS
 ;         Output is the m_connected subscripts array
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-


  mask = BYTARR(xsize,ysize)
  mask[indices] = 1b

  ;#######################STRUCTURES
  h0 = [[-1,1,0],[1,1,-1],[0,-1,-1]]   
  h1 = ROTATE(h0,1)
  h2 = ROTATE(h0,2)
  h3 = ROTATE(h0,3)

  m0 = [[0,1,0],[1,1,1],[-1,-1,-1]]   
  m1 = ROTATE(m0,1)
  m2 = ROTATE(m0,2)
  m3 = ROTATE(m0,3)

  numb = 1

  ;###############THINNING
  WHILE numb GT 0 DO BEGIN

   wim = WHERE(mask,num1)

   wimC = WHERE(CONVOL(mask,h0) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h1) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h2) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,h3) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m0) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m1) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m2) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wimC = WHERE(CONVOL(mask,m3) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC] = 0b

   wim = WHERE(mask,num2)
   numb = num1 - num2

   IF NOT KEYWORD_SET(nodisp) THEN tvscl,mask 

  ENDWHILE 

RETURN,WHERE(mask)

END

;########################################################


FUNCTION EFR_LENGTH_PIX,indices,xsize,ysize,SMP=smp

 ;+
 ; NAME:
 ;         EFR_LENGTH_PIX
 ; PURPOSE:
 ;         From the pixel chain defined by indices, computes
 ;         the length in pixels of the chain. The following
 ;         rules are used:
 ;         - distance btw 2 aligned  pixels = 1
 ;         - distance btw 2 diagonal pixels = SQRT(2)
 ;         The chain can be a boundary or a pruned skeleton
 ;         Subscripts MUST be ordered (cf EFR_ORDER_IND)
 ;
 ; CALLING SEQUENCE:
 ;         res = efr_length_pix(indices,xsize,ysize)
 ;         res = efr_length_pix(indices,xsize,ysize,SMP=smp)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; OUTPUT
 ;
 ;         Output is the length of the chain in pixels
 ;
 ; KEYWORD OUTPUT
 ;
 ;         SMP  is the subscript of the chain middle point
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-

   nele  = N_ELEMENTS(indices)

   ;####We compute the coordinates difference corresponding
   ;####to the original subscripts and the shifted subscripts, ie
   ;####btw 1 pixel and its neigbor

   tab1  = SHIFT(indices,-1)
   tab2  = indices
    
   difX  = tab1 MOD xsize - tab2 MOD xsize
   difY  = tab1 / xsize   - tab2 / xsize

   diag  = WHERE(ABS(difX) + ABS(difY) EQ 2,ndiag)
   line  = WHERE(ABS(difX) + ABS(difY) EQ 1,nline)

   length = ndiag*SQRT(2d) + nline + (nele-ndiag-nline < 1)


   ;####Chain middle point calculation (if not a boundary)

   IF (nele-ndiag-nline) EQ 1 THEN BEGIN

      smp = 0
      lt2 = 0
      ;####We compute length until it reaches
      ;####total length / 2 
      WHILE lt2 LT length/2.0 DO BEGIN
        diffX = difX[0:smp]
        diffY = difY[0:smp]
        diag  = WHERE(ABS(diffX) + ABS(diffY) EQ 2,ndiag)
        line  = WHERE(ABS(diffX) + ABS(diffY) EQ 1,nline)          
        lt2   = 0.5 + ndiag*SQRT(2d) + nline   
        smp   = smp + 1
      ENDWHILE

      smp = indices[smp]

   ENDIF ELSE smp = -1


RETURN,length


END

;########################################################

FUNCTION EFR_LENGTH_DEG,indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,BND=bnd

 ;+
 ; NAME:
 ;         EFR_LENGTH_DEG
 ; PURPOSE:
 ;         From the pixel chain defined by indices, computes
 ;         the length in heliographic degrees of the chain. 
 ;         The chain can be a boundary or a pruned skeleton
 ;         Subscripts MUST be ordered (cf EFR_ORDER_IND)
 ;
 ; CALLING SEQUENCE:
 ;         res = efr_length_deg(indices,nax1,nax2,cd1,cd2,
 ;                                        cenx,ceny,rsun,dateo)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         nax1         1st dim of the array (pixels)    ex: 1024
 ;         nax2         2nd dim of the array (pixels)    ex: 1024
 ;         cd1          cdelt1 keyword (arcsecs/pixels)  ex: 2.28
 ;         cd2          cdelt2 keyword (arcsecs/pixels)  ex: 2.28
 ;         cenx         Sun center X coordinate (pixels) ex: 511.5
 ;         ceny         Sun center Y coordinate (pixels) ex: 511.5
 ;         rsun         Sun radius (pixels)              ex: 420
 ;         dateo        Observation date  ex: 2002-04-01T08:00:00.000
 ;
 ; INPUT KEYWORD
 ;         BND          if the chain is a boundary
 ;
 ;
 ; OUTPUTS
 ;         Output is the length of the chain in heliographic deg.
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-


   nele  = N_ELEMENTS(indices)

   ;####Get the heliographics coordinates
   hel  = PIX2CARR(indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)


   ;####[270,360]+[0,90] -> [-90,90]
   hel  = (hel + 90.) MOD 360 - 90.

   lat = hel[*,0]
   lon = hel[*,1]

   lats = SHIFT(lat,-1)
   lons = SHIFT(lon,-1)

   ;####Don't take dist btw first and last point into 
   ;####account in case of a skeleton
   IF KEYWORD_SET(BND) THEN lp = 1 ELSE lp = 2
   lat  = lat[0:nele-lp]  & lon  = lon[0:nele-lp]
   lats = lats[0:nele-lp] & lons = lons[0:nele-lp]
    
   ;####Sum the distances btw each successive couple of pixels
   deglnth = TOTAL(SQRT((lat-lats)^2+(lon-lons)^2))

RETURN,deglnth
 
END

;###################################################################

FUNCTION EFR_AREA_DEG2,indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo

 ;+
 ; NAME:
 ;         EFR_AREA_DEG2
 ; PURPOSE:
 ;         From the boundary pixels defined by indices, computes
 ;         the area in heliographic square degrees:indices must 
 ;         draw a unique filled shape! 
 ;         (cf EFR_EXT_BOUNDARY or EFR_INNER_BOUNDARY)
 ;         area = ABS(SUM[(LATi - LATi-1)(LONi + LONi-1)/2])
 ;         (trapezoid approximation)
 ;
 ; CALLING SEQUENCE:
 ;         res = efr_area_deg2(indices,nax1,nax2,cd1,cd2,
 ;                                        cenx,ceny,rsun,dateo)
 ;
 ; INPUT
 ;         indices      subscripts of non-null boundary pixels
 ;         nax1         1st dim of the array (pixels)    ex: 1024
 ;         nax2         2nd dim of the array (pixels)    ex: 1024
 ;         cd1          cdelt1 keyword (arcsecs/pixels)  ex: 2.28
 ;         cd2          cdelt2 keyword (arcsecs/pixels)  ex: 2.28
 ;         cenx         Sun center X coordinate (pixels) ex: 511.5
 ;         ceny         Sun center Y coordinate (pixels) ex: 511.5
 ;         rsun         Sun radius (pixels)              ex: 420
 ;         dateo        Observation date  ex: 2002-04-01T08:00:00.000
 ;
 ; OUTPUTS
 ;         Output is the area in heliographic square deg.
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-


  nele    = N_ELEMENTS(indices)
  degarea = 0.
 
  ;####Get the heliographics coordinates
  coord = PIX2CARR(indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)

  ;####[270,360]+[0,90] -> [-90,90]
  coord = (coord + 90.) MOD 360. - 90.

  lati = coord[0,*] & loni = coord[1,*]
  
  ;####Shift coordinates  
  latis = SHIFT(lati,-1)
  lonis = SHIFT(loni,-1)

  ;####Sum the trapezoid areas
  degarea = ABS(TOTAL((loni+lonis)*(lati-latis)))/2.


RETURN,degarea
END

;###################################################################

FUNCTION EFR_PRUNING,indices,xsize,ysize,CITY=city

 ;+
 ; NAME:
 ;         EFR_PRUNING
 ; PURPOSE:
 ;         From the pixel skeleton defined by indices, computes
 ;         the fully pruned skeleton by calculating distance
 ;         btw node points and end points, and removing pixels
 ;         on this criterion.
 ;
 ; CALLING SEQUENCE:
 ;         res = efr_pruning(indices,xsize,ysize)
 ; 
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; KEYWORD
 ;         city         Use city block distance
 ;                      (the default is euclidian)
 ;
 ; OUTPUTS
 ;         Output is the pruned skeleton subscripts
 ; 
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;-

  ;####Search for node points and end points
  imske = EFR_PIXCUMUL(indices,xsize,ysize)

  ;####Node points
  wnp4 = WHERE(imske GE 4,NP4)

  ;####While there are node points the skeleton
  ;####is not fully pruned
  WHILE NP4 GT 0 DO BEGIN

    ;####End points
    wnp2  = WHERE(imske EQ 2,NP2)

    xwnp2 = wnp2 MOD xsize
    ywnp2 = wnp2 / xsize
    xwnp4 = wnp4 MOD xsize
    ywnp4 = wnp4 / xsize

    FOR ii = 0, NP4-1 DO BEGIN
      
       IF NOT KEYWORD_SET(city) THEN BEGIN
         dis = SQRT((xwnp2-xwnp4[ii])^2+(ywnp2-ywnp4[ii])^2) ;euclid
       ENDIF ELSE BEGIN
         dis = ABS(xwnp2-xwnp4[ii]) + ABS(ywnp2-ywnp4[ii]) ;city block
       ENDELSE
 
       ;####point to be removed:
       mini = WHERE(dis EQ MIN(dis),nmin)
       ;####In case of same size branches, do something ?... 
       ;IF nmin GT 1...
       xpt = wnp2[mini[0]] MOD xsize
       ypt = wnp2[mini[0]] / xsize
       imske[xpt,ypt]=0b
    ENDFOR

    ske_ind=EFR_SKELETON(WHERE(imske),xsize,ysize);remplace M_CONNECT
    imske=EFR_PIXCUMUL(ske_ind,xsize,ysize)
    wnp4=WHERE(imske GE 4,NP4)

  ENDWHILE

  ske=WHERE(imske)

RETURN,ske
END



;###################################################################

FUNCTION EFR_PRUNED_SKE,indices,xsize,ysize,EC=ec



  ;window,/free,xs=xsize,ys=ysize

  im=bytarr(xsize,ysize)
  im[indices]=1b

  ;#########full skeleton

   ske_ind=EFR_SKELETON(indices,xsize,ysize);,EC=ec)
   ske_ind=EFR_M_CONNECT(ske_ind,xsize,ysize)

  ;#########pruning

  ske = EFR_PRUNING(ske_ind,xsize,ysize)   
  ske = EFR_M_CONNECT(ske,xsize,ysize)

RETURN,ske
END



;################################################
FUNCTION EFR_PRUNING2,indices,xsize,ysize
;###DO NOT WORK####
  set0 = [-xsize,-1,1,xsize]
  ;####Search for node points and end points
  imske = EFR_PIXCUMUL(indices,xsize,ysize)
  ;####End points
  wnp2 = WHERE(imske EQ 2,NP2)
  xs=1l*xsize & dxs=2l*xsize

  WHILE NP2 GT 2 DO BEGIN

  ;####Node points
  wnp4 = WHERE(imske GE 4,NP4)
  wnp4s = wnp4[0]
  FOR oo=0,N_ELEMENTS(wnp4)-1 DO wnp4s = [wnp4s,wnp4[oo]+set0]
  wnp4s = wnp4s[WHERE(imske[wnp4s] GT 0)]
  imske[wnp4s]=0
  imske[wnp4]=0 
 
  ske_LABL  = LABEL_REGION(imske,/ALL)
  set2 = [-dxs-2,-dxs-1,-dxs,-dxs+1,-dxs+2, $
          -xs-2,-xs+2,-2,2,xs-2,xs+2, $
          dxs-2,dxs-1,dxs,dxs+1,dxs+2]


  FOR ii=0,NP4-1 DO BEGIN
      nei  = ske_LABL[wnp4[ii]+set2]
      vals = nei[WHERE(nei GT 0)]
      vals = vals[UNIQ(vals,SORT(vals))]
      nbr  = N_ELEMENTS(vals)
      prevlnt = 0
      brsubc = -1L
      cnt    = 0

    FOR jj=0,nbr-1 DO BEGIN
  
      val = vals[jj];nei[wbr[jj]]
      brsub = WHERE(ske_LABL EQ val)
      IF MIN(imske[brsub]) EQ 2 THEN BEGIN

         brsubc = [brsubc,brsub]
         lnt    = N_ELEMENTS(brsub)
         cnt    = cnt + 1

         IF lnt GT prevlnt THEN BEGIN

           brsubm = brsub
           prevlnt = lnt

         ENDIF

      ENDIF
   
    ENDFOR

    IF cnt GE 2 THEN BEGIN

      imske[brsubc[1:N_ELEMENTS(brsubc)-1]] = 0b
      imske[brsubm] = 1b   

    ENDIF

  ENDFOR


  imske[wnp4s] = 1
  imske[wnp4] = 1
  indices = WHERE(imske)
  indices=EFR_M_CONNECT(indices,xsize,ysize) 
  imske = EFR_PIXCUMUL(indices,xsize,ysize)
  wnp2s = WHERE(imske[wnp4s] EQ 2,nwp2s)
  IF nwp2s GT 0 THEN BEGIN
     imske[wnp4s[wnp2s]]=0
     indices = WHERE(imske)
     imske=EFR_PIXCUMUL(indices,xsize,ysize)
  ENDIF

  wnp2 = WHERE(imske EQ 2,NP2)

 ENDWHILE

  ske=indices

RETURN,ske
END
;################################################
FUNCTION EFR_PRUNING3,indices,xsize,ysize
;###DO NOT WORK####

  ;####Search for node points and end points
  imske = EFR_PIXCUMUL(indices,xsize,ysize)
  ;####End points
  wnp2 = WHERE(imske EQ 2,NP2)

  set0 = [-xsize,-1,1,xsize]

  WHILE NP2 GT 2 DO BEGIN

  ;####Node points
  wnp4 = WHERE(imske GE 4,NP4)
  IF NP4 GE 2 THEN BEGIN

  FOR ii=0,NP4-1 DO BEGIN

      wnp4s = wnp4[ii]+set0
      wnp4s = wnp4s[WHERE(imske[wnp4s] GT 0)]
      imske[wnp4s] = 0
      imske[wnp4[ii]] = 0
      ske_LABL  = LABEL_REGION(imske,/ALL) 
      nbr  = MAX(ske_LABL)
      cnt    = 0      
      prevlnt = 0
      brsubc = -1L  

      IF nbr GE 3 THEN BEGIN
 
         FOR jj=0,nbr-1 DO BEGIN
 
           val = jj+1
           brsub = WHERE(ske_LABL EQ val)  

           IF MIN(imske[brsub]) EQ 2 AND MAX(imske[brsub]) LT 4 THEN BEGIN

              brsubc = [brsubc,brsub]
              cnt = cnt+1
              lnt = N_ELEMENTS(brsub)
  
              IF lnt GT prevlnt THEN BEGIN

                 brsubm = brsub
                 prevlnt = lnt

              ENDIF

           ENDIF
   
        ENDFOR

    ENDIF
print,cnt
    IF cnt GE 2 THEN BEGIN

      imske[brsubc[1:N_ELEMENTS(brsubc)-1]] = 0
tvscl,imske
wait,4
      imske[brsubm] = 1   
tvscl,imske
wait,4
      imske[wnp4s] = 1
      imske[wnp4] = 1
tvscl,imske
wait,4
      indices = WHERE(imske)
      indices = EFR_M_CONNECT(indices,xsize,ysize)
      imske = EFR_PIXCUMUL(indices,xsize,ysize)
      wnp2s = WHERE(imske[wnp4s] LE 2,nwp2s)
      IF nwp2s GT 0 THEN BEGIN
        imske[wnp4s[wnp2s]]=0
        indices = WHERE(imske)
        imske=EFR_PIXCUMUL(indices,xsize,ysize)
      ENDIF
tvscl,imske
wait,4
    ENDIF

   ENDFOR

ENDIF ELSE BEGIN

      wnp4s = wnp4[ii]+set0
      wnp4s = wnp4s[WHERE(imske[wnp4s] GT 0)]
      imske[wnp4s] = 0
      imske[wnp4[ii]] = 0
      ske_LABL  = LABEL_REGION(imske,/ALL) 
      brsub1 = WHERE(ske_LABL EQ 1,n1)
      brsub2 = WHERE(ske_LABL EQ 2,n2)
      brsub3 = WHERE(ske_LABL EQ 3,n3)
      brsub4 = WHERE(ske_LABL EQ 4,n4) 

      IF n4 EQ -1 THEN BEGIN
          tab = SORT([n1,n2,n3])+1
          val= tab[0]
          imske[WHERE(ske_LABL) EQ val]=0 
      ENDIF ELSE BEGIN
          tab = [n1,n2,n3,n4]       
          val1= tab[0]
          val2= tab[1]
          imske[WHERE(ske_LABL) EQ val1]=0 
          imske[WHERE(ske_LABL) EQ val2]=0 
      ENDELSE
      
      imske[wnp4s] = 1
      imske[wnp4] = 1
      indices = WHERE(imske)
      indices = EFR_M_CONNECT(indices,xsize,ysize)
      imske = EFR_PIXCUMUL(indices,xsize,ysize)
      wnp2s = WHERE(imske[wnp4s] LE 2,nwp2s)
      IF nwp2s GT 0 THEN BEGIN
        imske[wnp4s[wnp2s]]=0
        indices = WHERE(imske)
        imske=EFR_PIXCUMUL(indices,xsize,ysize)
      ENDIF

ENDELSE  

      wnp2 = WHERE(imske EQ 2,NP2)
 ENDWHILE

  ske=indices

RETURN,ske
END














