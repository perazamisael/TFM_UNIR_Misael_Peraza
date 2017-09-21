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
 ;         have value 2, node points value 4 or 5, and
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
 ;         Last modified Nov. 2004
 ;-

  AA       = indices
  nAA      = N_ELEMENTS(AA)
  mask     = BYTARR(xsize,ysize)
  output   = mask
  mask[AA] = 1b
  pos      = [-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,xsize+1]


  FOR k = 0, nAA-1 DO BEGIN
      N4P = TOTAL(mask[AA[k]+pos[[1,3,4,6]]])
      N8P = TOTAL(mask[AA[k]+pos[*]])
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

FUNCTION EFR_PIXCUMUL_8N,indices,xsize,ysize

 ;+
 ; NAME:
 ;         EFR_PIXCUMUL_8N
 ; PURPOSE:
 ;         Assign a value to every pixel of a chain
 ;         depending on the count of its non-null
 ;         8-connected neighbors
 ;
 ;         Eg: 000  000  010  001  011  101  111
 ;             010  010  010  010  010  010  111
 ;             000  100  001  101  101  101  111
 ;             ->1  ->2  ->3  ->4  ->5  ->5  ->9
 ;
 ; 
 ; CALLING SEQUENCE:
 ;         res = efr_pixcumul_8n(indices,xsize,ysize)
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
 ;         2010  Written by N. Fuller
 ;-

  AA       = indices
  nAA      = N_ELEMENTS(AA)
  mask     = BYTARR(xsize,ysize)
  output   = mask
  mask[AA] = 1b
  pos      = [-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,xsize+1]


  FOR k = 0, nAA-1 DO BEGIN
      output[AA[k]] = TOTAL(mask[AA[k]+pos[*]])+1
  ENDFOR

RETURN,output

END


;###################################################################


FUNCTION EFR_ORDER_IND,indices,xsize,ysize,SKE_IND=ske_ind

 ;+
 ; NAME:
 ;         EFR_ORDER_IND
 ; PURPOSE:
 ;         
 ;   Order subscripts of a 1 pixel thick chain (ex:pruned skeleton)
 ;   from a first one to the successive neighbors
 ;   in the case of a skeleton the order is from low to high indices
 ;   In the case of a boundary (ie no end points), the first point
 ;   is the smallest indice (unless SKE_IND is set) and the order is
 ;   counterclockwise
 ;   if SKE_IND is set, in the case of a boundary, the first point will
 ;   be the closest to this indice (for example the first point of the
 ;   corresponding skeleton)
 ;   Pixels MUST be M-connected             
 ; 
 ; KEYWORD
 ;        SKE_IND    indice of the first point of skeleton or indice
 ;                   of the point where the ordering should start
 ;                   NOT CODED YET!
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
 ;         modified Jul. 2010
 ;-


  ind        = indices
  nele       = N_ELEMENTS(ind)
  order      = LONARR(nele,/NOZERO)
  valuesmap  = EFR_PIXCUMUL(ind,xsize,ysize) ;(value 2 or 3)

  IF nele LT 2 THEN RETURN,-1

  IF MAX(valuesmap) GT 3 THEN BEGIN
     PRINT,'EFR_ORDER_IND: needs a pruned & m_connected pixel chain!'
     RETALL
  ENDIF

  where2     = WHERE(valuesmap EQ 2,nv) ;(end points)
  revers     = 0
  
  ;#### if boundary, remove first indice to open it
  IF nv NE 2 THEN BEGIN 
    ;revers: insure that the order is counterclockwise eb=ven in the special
    ;case corresponding to the condition below
    IF TOTAL(valuesmap[ind[0]+[-xsize-1,-xsize,-xsize+1,-1,1]]) EQ 0 THEN revers=1
    ind        = ind[1:nele-1]
    nele       = nele-1
    order      = LONARR(nele,/NOZERO)
    valuesmap  = EFR_PIXCUMUL(ind,xsize,ysize) 
    where2     = WHERE(valuesmap EQ 2,nv)
    lake       = 1
  ENDIF ELSE lake = 0

  set = [-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,1+xsize]
  IF revers EQ 0 THEN BEGIN
     order[nele-1] = where2[1] 
     ep = where2[0]
  ENDIF ELSE BEGIN
     order[nele-1] = where2[0] 
     ep = where2[1]
  ENDELSE

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
 ;         indices      subscripts of non-null skeleton pixels
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
 ;         indices using erosion with a structure which is
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

;###################################################################


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
 ;         and the four 90� rotations of them
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

  ;tvscl,mask

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

   ;TVSCL,mask

  ENDWHILE 

  ec = ec - 1

RETURN,WHERE(mask)
END


;###################################################################
;Function associated with EFR_SKELETON_GRAY

FUNCTION REMOVE_PIX,mask,wimC,nwimC,graybox,hi,n,xsize
       xx = wimC MOD xsize
       yy = wimC / xsize
       FOR ii=0,nwimC-1 DO BEGIN
            smallgbox = graybox[xx[ii]-n:xx[ii]+n,yy[ii]-n:yy[ii]+n]
            meanii = MEAN(smallgbox[WHERE(smallgbox)])
	    IF meanii gt hi THEN mask[wimC[ii]] = 0b
       ENDFOR
RETURN,mask
END

FUNCTION EFR_SKELETON_GRAY,indices,graybox,xsize,ysize,SMOOTHW=smoothw,EC=ec,HIST=hist,NOTLIN=notlin,NLEVEL=nlevel

 ;+
 ; NAME:
 ;         EFR_SKELETON_GRAY
 ; PURPOSE:
 ;         Erode successively the shape corresponding 
 ;         to the indices until the shape is reduced to
 ;         its one pixel thick skeleton.
 ;         The structures used to erode the shape are
 ;         the following (cf. Hit-or-Miss transform):
 ;          0 0 0     x 0 0
 ;          x 1 x     1 1 0
 ;          1 1 1     x 1 x
 ;         and the four 90� rotations of them
 ;         x stands for "don't care" values
 ;         
 ;         Intensity levels are defined and the erosion
 ;         first starts for high value pixels, then
 ;         the level condition decrease and erosion continue 
 ; 
 ; CALLING SEQUENCE:
 ;         res = efr_skeleton_gray(indices,graybox,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels (in a xsize * ysize array)
 ;         graybox      gray scale image of filament (xsize * ysize)
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; INPUT KEYWORDS
 ;         smoothw       size of the smoothing window
 ;         hist          determination of gray levels: from histogram cumulative distribution
 ;         notlin        determination of gray levels: not linear, closer levels at high values
 ;         nlevel        number of gray levels
 ;
 ; OUTPUT KEYWORDS
 ;         EC           Set to a named variable that contains
 ;                      the number of erosions
 ;                      needed to fully erode the shape
 ;
 ; CALLS
 ;          REMOVE_PIX
 ;
 ; OUTPUTS
 ;         Output is the skeleton subscripts array
 ;
 ; HISTORY:
 ;         2010  Written by N. Fuller from EFR_SKELETON
 ;         
 ;-
 
  mask = BYTARR(xsize,ysize)
  mask[indices] = 1b
  
  ;test
  ;jo = [[1,1,1],[1,1,1],[1,1,1]]
  ;wtest = WHERE(CONVOL(mask,jo) LT 5,ntest)
  ;graybox[wtest]=0
  ;fintest
  
  zeros = WHERE(mask EQ 0b)
  
  ;optionnal graybox smoothing: high means
  ;smoother skeleton but less accurate
  IF KEYWORD_SET(SMOOTHW) THEN BEGIN
      IF SIZE(smoothw,/TYPE) EQ 2 AND smoothw GT 0 AND smoothw LT 100 THEN BEGIN
         graybox  = SMOOTH(graybox,smoothw)
      ENDIF ELSE BEGIN
         graybox  = SMOOTH(graybox,5)
      ENDELSE 
  ENDIF 
  graybox[zeros]=0
  
  ;compute the gray levels: different choices
  gmin= MIN(graybox[WHERE(graybox)],MAX=gmax)
  ;Number of gray levels -> high "may" be more accurate but slower
  IF KEYWORD_SET(NLEVEL) THEN nlevels = nlevel ELSE nlevels = 10
  levels = INTARR(nlevels)
  IF KEYWORD_SET(HIST) THEN BEGIN   
      hist = HIST_EQUAL(graybox[WHERE(graybox)],/HISTOGRAM_ONLY,binsize=1,MINV=0,MAXV=gmax)
      hmax = MAX(hist)
      FOR oo=1,nlevels DO BEGIN
           levels[oo-1] = (WHERE(hist GT hmax-oo*(hmax/nlevels)))[0]
      ENDFOR
  ENDIF ELSE IF KEYWORD_SET(NOTLIN) THEN BEGIN
      FOR oo=1,nlevels DO BEGIN
           levels[oo-1] = gmax-((gmax-gmin)/(1.5^(nlevels-oo)))
      ENDFOR
  ENDIF ELSE BEGIN
      FOR oo=1,nlevels DO BEGIN ;default
           levels[oo-1] = gmax - oo*(gmax-gmin)/nlevels 
      ENDFOR
  ENDELSE
   
  n =  1;size of neighborhood
  
  ;####################STRUCTURES
  h0 = [[-1,-1,-1],[0,1,0],[1,1,1]] 
  m0 = [[0,-1,-1],[1,1,-1],[0,1,0]]  
  h1 = ROTATE(h0,1)
  m1 = ROTATE(m0,1) 
  h2 = ROTATE(h0,2)
  m2 = ROTATE(m0,2) 
  h3 = ROTATE(h0,3) 
  m3 = ROTATE(m0,3)

  ec = 0

FOR aa=1,nlevels DO BEGIN

  hi = levels[aa-1]
  numb = 1

  ;###############THINNING
  WHILE numb GT 0 DO BEGIN

   wim = WHERE(mask,num1)

   wimC = WHERE(CONVOL(mask,h0) EQ 4,nwimC)
   IF nwimC NE 0 THEN BEGIN
       mask = REMOVE_PIX(mask,wimC,nwimC,graybox,hi,n,xsize)
   ENDIF
   
   wimC = WHERE(CONVOL(mask,m0) EQ 3,nwimC)
   IF nwimC NE 0 THEN BEGIN
      mask = REMOVE_PIX(mask,wimC,nwimC,graybox,hi,n,xsize) 
   ENDIF
   
   wimC = WHERE(CONVOL(mask,h1) EQ 4,nwimC)
   IF nwimC NE 0 THEN BEGIN
      mask = REMOVE_PIX(mask,wimC,nwimC,graybox,hi,n,xsize) 
   ENDIF
   
   wimC = WHERE(CONVOL(mask,m1) EQ 3,nwimC)
   IF nwimC NE 0 THEN BEGIN
      mask = REMOVE_PIX(mask,wimC,nwimC,graybox,hi,n,xsize) 
   ENDIF   
   
   wimC = WHERE(CONVOL(mask,h2) EQ 4,nwimC)
   IF nwimC NE 0 THEN BEGIN
      mask = REMOVE_PIX(mask,wimC,nwimC,graybox,hi,n,xsize) 
   ENDIF   
   
   wimC = WHERE(CONVOL(mask,m2) EQ 3,nwimC)
   IF nwimC NE 0 THEN BEGIN
       mask = REMOVE_PIX(mask,wimC,nwimC,graybox,hi,n,xsize) 
   ENDIF   
   
   wimC = WHERE(CONVOL(mask,h3) EQ 4,nwimC)
   IF nwimC NE 0 THEN BEGIN
       mask = REMOVE_PIX(mask,wimC,nwimC,graybox,hi,n,xsize) 
   ENDIF   
   
   wimC = WHERE(CONVOL(mask,m3) EQ 3,nwimC)
   IF nwimC NE 0 THEN BEGIN
       mask = REMOVE_PIX(mask,wimC,nwimC,graybox,hi,n,xsize) 
   ENDIF

   wim  = WHERE(mask,num2)
   numb = num1 - num2
   IF num1 GT num2 THEN ec = ec + 1
   
  ENDWHILE 

ENDFOR

ec = ec - 1

RETURN,WHERE(mask)
END

;###################################################################
FUNCTION MIN_DIST,a,b,LOCA=loca,LOCB=locb
;#### minimum distance between the values of two arrays of different size
;#### (not a euclidian distance, min difference btwn values)
;#### LOCA keyword gives the corresponding location in array a, the same
;#### for locb. ARRAY 'a' MUST BE THE LONGEST ONE
  na = N_ELEMENTS(a)  
  nb = N_ELEMENTS(b)
  IF nb GT na THEN RETURN,0
  min = max(a)
  FOR ii = 0,na-1 DO BEGIN
    diff = ABS(b-(SHIFT(a,-1*ii))[0:nb-1])
    minii = MIN(diff)
    wwmin = (WHERE(diff EQ minii))[0]
    IF minii LT min THEN BEGIN 
      min = minii
      loca = wwmin + ii 
      locb = wwmin 
    ENDIF
  ENDFOR

RETURN, min
END

;###################################################################
FUNCTION EFR_LAKE2BAY,indices,xsize,ysize,RATIO=ratio

 ;+
 ; NAME:
 ;         EFR_LAKE2BAY
 ; PURPOSE:
 ;         Before computing the boundary of the shape
 ;         (indices) we must check the presence 
 ;         of lake-like filaments (which lead to 2 different
 ;         boundaries, outer and inner). The size of the gap
 ;         which define a lake instead of a gap that should
 ;         be filled-in is set via 'ratio' (see keyword).
 ;         Lakes are changed in bays by removing pixels.         
 ; 
 ;  EX:
 ;  
 ;     xxx                 xxx
 ;    xxxxxx            xxxxxx
 ;    xxx000    -->  xxxxxx
 ;     xx00x            xxxxx  
 ;      xxx                xxx
 ; 
 ;     xxxx                xxxx             
 ;    x0000x            x0000x        
 ;    x0000x    -->   x00000 
 ;     x000x             x000x          
 ;      xxx                 xxx           
 ; 
 ;
 ; CALLING SEQUENCE:
 ;         res = efr_lake2bay(indices,xsize,ysize)
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;
 ; INPUT KEYWORDS
 ;         RATIO    ratio btw the gap size (GS) and the size of
 ;                      the feature (FS: number of subscripts)
 ;                      If GS/FS is bigger than 'ratio' then the
 ;                      gap is kept. (default is 1/3.)     
 ;
 ; OUTPUTS
 ;         Output is the filled-in or modified feature subscripts array
 ;
 ; HISTORY:
 ;         2003  Written by N. Fuller
 ;         modified Fev. 2004
 ;         major upgrade Apr2012 N.F. (opening cavities process)
 ;-

  IF NOT KEYWORD_SET(ratio) THEN ratio = 1/3.  

  mask = BYTARR(xsize,ysize)
  mask[indices] = 1b

  ;####Image's negative
  C_mask = ABS(mask-1b)

  ;####Compute the number of regions within
  ;####the image's negative
  reg = LABEL_REGION(C_mask)
  h_reg = HISTOGRAM(reg,reverse_indices=rev)  
  nbreg = N_ELEMENTS(h_reg) 

  ;####Compute the number of region within
  ;####the eroded image's negative to deal with
  ;####limit cases (to avoid pbs with the external 
  ;####boundary definition) 
  regb = LABEL_REGION(ERODE(C_mask,REPLICATE(1,3,3)))
  h_regb = HISTOGRAM(regb,reverse_indices=revb)
  nbregb = N_ELEMENTS(h_regb)

 ;#### First case, 2 regions only (foreground + background) -> no lake
 IF nbreg EQ 2 AND nbregb EQ 2 THEN RETURN,indices ELSE BEGIN
     FOR uu = 0, nbregb-1 DO BEGIN
          filled = 0
          indreg = revb[revb[uu]:revb[uu+1]-1]
            ;####The region should not be the external foreground
            ;####of the negative image:if (1,1) is included, indreg is
            ;####the external foreground (not 0,0 because label_region
            ;####always considers border pixels as zeros). for
            ;####the same reasons the border should not be included
            ;####(ie border at 1 and then 0 and the object)->this correspond 
            ;####to the object itself (not the holes)
         IF (WHERE(indreg EQ (xsize+1)) EQ -1 AND WHERE(indreg EQ (0)) EQ -1) THEN BEGIN
             ;####Region smaller than ratio*size(object) -> fill it
             IF N_ELEMENTS(indreg) LT N_ELEMENTS(indices)*ratio THEN BEGIN
                mask[indreg]=1b
                filled = 1
             ;####If not keep the region indices
             ENDIF ELSE BEGIN
                filled = -1
                hole_ind = indreg
             ENDELSE
 
            IF filled EQ 1 THEN BEGIN 
               ;####Fill the zeros btw the original (not eroded) 
               ;####and the filled regions
               mask = MORPH_CLOSE(mask,REPLICATE(1,3,3))
            ENDIF

            IF filled EQ -1 THEN BEGIN
               ;#### in the case the hole is kept, we need to open it
               PRINT,"#################################################"
               PRINT,"EFR_LAKE2BAY.pro: Opening cavity in filament/prominence"
               masktmp = mask
               masktmp[hole_ind]=1b
               masktmp = MORPH_CLOSE(masktmp,REPLICATE(1,3,3))
               bnd_hole = EFR_INNER_BOUNDARY(hole_ind,xsize,ysize,ALLDIR=alldir)
               bnd_obj = EFR_EXT_BOUNDARY(WHERE(masktmp),xsize,ysize,ALLDIR=alldir)
               IF N_ELEMENTS(bnd_hole) GT N_ELEMENTS(bnd_obj) THEN BEGIN
                   mind = MIN_DIST(bnd_hole,bnd_obj,LOCA=loch,LOCB=loco)
                   IF bnd_hole[loch] LE bnd_obj[loco] THEN openbar = FINDGEN(mind+2)+bnd_hole[loch]-1 ELSE $
                    openbar = FINDGEN(mind+2)+bnd_obj[loco]-1
               ENDIF ELSE BEGIN
                   mind = MIN_DIST(bnd_obj,bnd_hole,LOCA=loco,LOCB=loch)
                   IF bnd_obj[loco] LE bnd_hole[loch] THEN openbar = FINDGEN(mind+2)+bnd_obj[loco]-1 ELSE $
                    openbar = FINDGEN(mind+2)+bnd_hole[loch]-1
                  openbar=[openbar,openbar-xsize,openbar+xsize]
               ENDELSE
               mask[openbar]=0b
            ENDIF
 
         ENDIF

      ENDFOR

  ENDELSE
  indices = WHERE(mask)

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

   ;SPHERICAL
   ;lat = lat/180.
   ;lon = lon/180.
   ;lats = lats/180.
   ;lons = lons/180.
   ;deglnth2 = TOTAL(180.*(ACOS(SIN(lat)*SIN(lats)+COS(lat)*COS(lats)*COS(lon-lons))))
   ;PRINT,deglnth,deglnth2,deglnth-deglnth2,100*(deglnth-deglnth2)/deglnth
  
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

  lati = coord[*,0] & loni = coord[*,1]  

  ;####Shift coordinates  
  latis = SHIFT(lati,-1)
  lonis = SHIFT(loni,-1)

  ;####Sum the trapezoid areas
  degarea = ABS(TOTAL((loni+lonis)*(lati-latis)))/2.

   ;SPHERICAL
   ;lati = lati/180.
   ;loni = loni/180.
   ;latis = latis/180.
   ;lonis = lonis/180.
   ;degarea2 = ABS(TOTAL((180.*(lati-latis))*180.*(ACOS(SIN((lati+latis)/2.)*SIN((lati+latis)/2.)+COS((lati+latis)/2.)*COS((lati+latis)/2.)*COS((loni+lonis)/2.)))))
   ;PRINT,degarea,degarea2,(degarea-degarea2),100.*(degarea-degarea2)/degarea

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

    ske_ind=EFR_SKELETON(WHERE(imske),xsize,ysize);replaces M_CONNECT
    imske=EFR_PIXCUMUL(ske_ind,xsize,ysize)
    wnp4=WHERE(imske GE 4,NP4)

  ENDWHILE

  ske=WHERE(imske)

RETURN,ske
END
;###################################################################

FUNCTION EFR_PRUNING_GRAY,indices,graybox,xsize,ysize

 ;+
 ; NAME:
 ;         EFR_PRUNING
 ; PURPOSE:
 ;         From the pixel skeleton defined by indices, computes
 ;         the fully pruned skeleton taking into account the grayscale
 ;         values of each branch. There is no successive distance calculation
 ;         like in efr_pruning, a given branch is entirely removed in one step 
 ;
 ; CALLING SEQUENCE:
 ;         res = efr_pruning_gray(indices,graybox,xsize,ysize)
 ; 
 ;
 ; INPUT
 ;         indices      subscripts of non-null pixels corresponding to skeleton
 ;         xsize        1st dim of the array
 ;         ysize        2nd dim of the array
 ;         graybox      grayscale image of filament
 ;
 ;
 ; OUTPUTS
 ;         Output is the pruned skeleton subscripts
 ; 
 ;
 ; HISTORY:
 ;         2010  Written by N. Fuller
 ;         from EFR_PRUNING
 ;-
 
 
  ;####Search for node points and end points
  imske = EFR_PIXCUMUL_8N(indices,xsize,ysize)
  wnp4 = WHERE(imske GE 4,NP4,comp=comp)
  
  
  ;From the indices values, create a map of the skeleton
  ;where each pixel is proportional to the neighborhood values
  ;-> grayscale skeleton
  mask = imske*0.
  ;size of the neighborhood
  n = 5
  ;max of original image
  gmax = MAX(graybox)+1
  ;Inverse the values (ie big values are the small ones)
  ;square to enhance the difference between big and small vals
  ;ie keep the dark branch instead of the light one even if it is longer 
  FOR ii=0,N_ELEMENTS(indices)-1 DO BEGIN
     xx = indices[ii] MOD xsize
     yy = indices[ii] / xsize
     smallgbox = graybox[(xx-n)>0:(xx+n)<(xsize-1),(yy-n)>0:(yy+n)<(ysize-1)]
     mask[indices[ii]]=((gmax-MEAN(smallgbox[WHERE(smallgbox)]))^2)/1000.
  ENDFOR
  
  ;####While there are node points the skeleton
  ;####is not fully pruned
  pass=0
  WHILE NP4 GT 0 AND pass LT 100 DO BEGIN
  
     ;search for node point clusters (ie node points which are connected)
     imtmp = imske
     imtmp[comp]=0
     reg0 = LABEL_REGION(imtmp,/all)
     h_reg0 = HISTOGRAM(reg0,reverse_indices=rev0)
     nnode = N_ELEMENTS(h_reg0)
  
     FOR ii = 1, nnode-1 DO BEGIN
     
        indnode = rev0[rev0[ii]:rev0[ii+1]-1]
        imske[indnode]=0
	;When the node point disapears, the branches can be labelled
        reg = LABEL_REGION(imske,/all)
        h_reg = HISTOGRAM(reg,reverse_indices=rev)
        nbranch = N_ELEMENTS(h_reg) 

	;there should be at least 3 branches to be able to remove one
        IF nbranch GT 2 THEN BEGIN
	   ;create an array to store the weight of the branches
           resume_val = 0
           FOR bb=1,nbranch-1 DO BEGIN ;bb=0 correspond to background
               indbranch = rev[rev[bb]:rev[bb+1]-1]
	       ;compute the weight of the branch
               value = TOTAL(mask[indbranch])
               resume_val=[resume_val,value]
           ENDFOR
	   ;look for the lowest weight and remove corresponding pixels
           minv = MIN(resume_val[1:N_ELEMENTS(resume_val)-1])
           indv = WHERE(resume_val EQ minv)
           remove_ind = rev[rev[indv[0]]:rev[indv[0]+1]-1]
           imske[remove_ind]=0
	   ;put node point cluster back on map
	   imske[indnode]=3
         ENDIF ELSE BEGIN
	  print,'not enough nbranches for that node point (<= 2, needs > 2)'
	 ENDELSE
         
  
    ENDFOR
   
    ;remove potential very small branches left (1 pixel)
    ske_ind=EFR_SKELETON(WHERE(imske),xsize,ysize);replaces M_CONNECT
    ;search for node points again
    imske=EFR_PIXCUMUL_8N(ske_ind,xsize,ysize)
    wnp4=WHERE(imske GE 4,NP4,comp=comp)
   
    ;to avoid endless "while" 
    pass += 1
  ENDWHILE
 
  ske=WHERE(imske)

RETURN,ske
END

;################################################
FUNCTION EFR_MAD,values,central,MEDIAN=median
;return the mean absolute deviation of the dataset from a central point
;(could be mean, median or mode). If Median keyword is chosen
;returns meadian absolute deviation instead

  IF KEYWORD_SET(MEDIAN) THEN BEGIN
   mad = MEDIAN(ABS(values - central))
  ENDIF ELSE BEGIN
    mad = MEAN(ABS(values - central))
  ENDELSE
RETURN,mad
END

;################################################
FUNCTION EFR_RASTER_LEVELS,imagette,nlev,Baselev
  ;If nlev=5 then levs = backgr + Baselev + lev1 + lev2 + lev3


  ;######morphological tools to smooth contours
   rad = 3
   mc_str = SHIFT(DIST(2*rad+1),rad,rad) le rad
   mo_str = [[0,1,0],[1,1,1],[0,1,0]]

  IF nlev GT 9 THEN RETALL

  ;######Define levels

   ;#####From maximum level
   ;mma = MAX(imagette)
   ;delt = FIX((mma-Baselev)/FLOAT(nlev-1)) 
   ;levels = (INDGEN(nlev-1))*delt + Baselev


   ;#####From cumul histogram
   h_e=HIST_EQUAL(imagette[WHERE(imagette GT Baselev)],bin=1,min=0,/histo)
   max_h_e = MAX(h_e)
   bins = max_h_e/(nlev-1)
   levels = INTARR(nlev-1)
   levels[0] = Baselev
   FOR ii=1,nlev-2 DO BEGIN
     levels[ii]=(WHERE(h_e GE ii*bins))[0]
   ENDFOR

  ;######create the n-byte tab (smooth each level contours)
   bytim = BYTE(imagette)*0b ;(backgr level)
   bytim[WHERE(imagette)]=1b
   tempim = imagette*0b
   FOR ii=1,nlev-2 DO BEGIN
     pos = WHERE(imagette GT levels[ii],npos)
     tempim[pos]=1b
     tempim = MORPH_OPEN(MORPH_CLOSE(tempim,mc_str),mo_str)
     pos = WHERE(tempim,npos)
     IF npos GT 0 THEN bytim[pos]=bytim[pos]+1b
     tempim = tempim*0b
   ENDFOR
   tempim = 0

RETURN,bytim
END

;################################################
FUNCTION EFR_RS_INT,origim,bytim,qslev,levsep,LMIN=lmin,LMAX=lmax,LMEAN=lmean,LM2QS=lm2qs

  nlev = MAX(bytim)

  lmin=''
  lmax=''
  lmean=''
  lm2qs=''
  FOR ii=1b,nlev DO BEGIN
    pos = WHERE(bytim EQ ii,npos)
    lmin = [lmin,STRTRIM(FIX(MIN(origim[pos])),2),levsep]
    lmax = [lmax,STRTRIM(FIX(MAX(origim[pos])),2),levsep]
    lmean = [lmean,STRTRIM(FIX(MEAN(origim[pos])),2),levsep]
    lm2qs = [lm2qs,STRMID(STRTRIM(MEAN(origim[pos])/qslev,2),0,5),levsep]
  ENDFOR 
  lmin = STRJOIN(lmin[1:N_ELEMENTS(lmin)-2])  
  lmax = STRJOIN(lmax[1:N_ELEMENTS(lmax)-2]) 
  lmean = STRJOIN(lmean[1:N_ELEMENTS(lmean)-2]) 
  lm2qs = STRJOIN(lm2qs[1:N_ELEMENTS(lm2qs)-2]) 

RETURN,bytim
END

;################################################
FUNCTION EFR_RLE,bytim

  ;;;;Image Run Length Encoding
  siz = SIZE(bytim)
  IF siz[0] EQ 2 THEN BEGIN
    nrow = siz[2]
    ncol = siz[1]
  ENDIF ELSE BEGIN ;if only 1 line
    nrow = 1
    ncol = siz[1]
  ENDELSE 
  rle = STRARR(nrow)
  FOR ii=0,nrow-1 DO BEGIN
   row0 = bytim[*,ii]
   row1 = [row0[0],row0[0:ncol-2]]
   diff = [ABS(row1-row0),1]
   loc0 = WHERE(diff NE 0,nloc)
   IF nloc GT 1 THEN BEGIN
     loc1 = [0,loc0[0:nloc-2]]
     dif2 = ABS(loc1-loc0)
     vals = FIX(row0[loc0-1])
     pts  = [STRARR(nloc-1)+'.',':']
     rle[ii] = STRJOIN(STRTRIM(vals,2)+STRTRIM(dif2,2)+pts)
   ENDIF ELSE BEGIN ;specific case of only 1 value
     rle[ii] = STRTRIM(FIX(row0[0]),2)+STRTRIM(ncol,2)+':'
   ENDELSE

  ENDFOR

RETURN,rle;or STRJOIN(rle)
END


;#############################################################
;#### determine the chirality with Rust/Bernasconi's method

FUNCTION EFR_CHIRALITY0,p_ske,bnd,xs,ys

  ;distances computation
  
  nbnd = N_ELEMENTS(bnd)
  dists = FLTARR(nbnd)
  x1 = p_ske MOD xs
  y1 = p_ske / xs
  FOR ii = 0, nbnd-1 DO BEGIN
     x0 = bnd[ii] MOD xs
     y0 = bnd[ii] / xs
     distii = SQRT((x0-x1)^2+(y0-y1)^2)
     wwii = WHERE(distii EQ MIN(distii))
     dists[ii] = distii[wwii]
  ENDFOR
  plot,SMOOTH(dists,10)
RETURN,0
END


;#############################################################
;#### determine the chirality depending on the orientation of
;#### branches / skeleton



FUNCTION EFR_CHIRALITY,ske,p_ske,xs,ys
;print,'chirality'

   imi=BYTARR(xs,ys)
   im_pske = imi
   im_pske[p_ske]=1b
   npske=N_ELEMENTS(p_ske)
   temp = DILATE(im_pske,REPLICATE(1,3,3))
   titi=temp
   titi[ske]=2b
   ;tvscl,titi
   d_p_ske = WHERE(temp)
   imi[ske]=1b
   imi[d_p_ske]=0b
   ;tvscl,imi,200,0
   ww=WHERE(imi,count)
   ims = imi*0b
   ims[ske] = 10
   fs_indices = EFR_ORDER_IND(p_ske,xs,ys)
   n_fs = N_ELEMENTS(fs_indices)
   Ndex = 0.
   Nsin = 0.
   ndex_dis = 0.
   nsin_dis = 0.
   cf = 0.
   cf2 = 0.
   IF count GT 0 THEN BEGIN
       
       r_l   = LABEL_REGION(imi,/all_neigh)
       h_r_l = HISTOGRAM(r_l,reverse_indices=r)
       
       FOR i = 1, N_ELEMENTS(h_r_l)-1 DO BEGIN
           
         b_indices = r[r[i]:r[i+1]-1]
	 nbind = N_ELEMENTS(b_indices)
 	 IF nbind GT 1 THEN BEGIN
	    imb = EFR_PIXCUMUL(b_indices,xs,ys)
	    wwend = WHERE(imb EQ 2,nend)
	    ims[b_indices]=1b
	    x0 = 0
	    ;find the root point of the branch (ie connected to the skeleton)
 	    FOR ee=0,nend-1 DO BEGIN
	       to=TOTAL(ims[wwend[ee]+[-xs-1,-xs,-xs+1,-1,1,xs-1,xs,xs+1]]);
	       ;print,'i=',i,'ee=',ee,'to=',to
	       IF to GT 10 THEN BEGIN
	          x0 = wwend[ee] MOD xs
	          y0 = wwend[ee] / xs
	       ENDIF
	    ENDFOR
	    IF x0 EQ 0 THEN BEGIN
	      ;In specific case of a "v" branch the test above does not work:
	      ;all points of the branch should be checked, not only end points
	       FOR vv=0,N_ELEMENTS(b_indices)-1 DO BEGIN
	          to=TOTAL(ims[b_indices[vv]+[-xs-1,-xs,-xs+1,-1,1,xs-1,xs,xs+1]])
	          ;print,"V case ",'i=',i,'vv=',vv,'to=',to
	          IF to GT 10 THEN BEGIN
	             x0 = b_indices[vv] MOD xs
	             y0 = b_indices[vv] / xs
	          ENDIF
	       ENDFOR
	    ENDIF
	    IF x0 EQ 0 THEN BEGIN
	      PRINT,"Chirality: didn't manage to find root point of branch number"+STRTRIM(i)
	      RETURN,0
	    ENDIF
	    ;find the opposite point (longest distance from root point)
	    disti = 0
	    FOR ff=0,nend-1 DO BEGIN
	      xi = wwend[ff] MOD xs
	      yi = wwend[ff] /xs
	      disti = [disti,SQRT((x0-xi)^2+(y0-yi)^2)]
	    ENDFOR
	    wwdi = WHERE(disti EQ MAX(disti))
	    ;print,"end=",wwdi[0]-1
	    xe = wwend[wwdi[0]-1] MOD xs
	    ye = wwend[wwdi[0]-1] / xs   
	    anb = ATAN(ye-y0,xe-x0)
	    IF abs(anb) GT !pi/2. THEN anb = anb - anb/(abs(anb))*!pi
	    ;print,i,180./!pi*anb
	    ims[ske]=10
	    
 	    ;get the link to the skeleton
	    
	    alldist = SQRT((p_ske MOD xs - x0)^2+(p_ske / xs - y0)^2)
	    wwp = WHERE(alldist EQ MIN(alldist))
	    x0s = p_ske[(wwp[0]-10)>0] MOD xs
	    y0s = p_ske[(wwp[0]-10)>0] / xs
	    xes = p_ske[(wwp[0]+10)<(npske-1)] MOD xs
	    yes = p_ske[(wwp[0]+10)<(npske-1)] / xs
	    ans = ATAN(yes-y0s,xes-x0s)
	    IF abs(ans) GT !pi/2. THEN ans = ans - ans/(abs(ans))*!pi
	    diff = anb - ans
	    IF abs(diff) GT !pi/2. THEN diff = diff - diff/(abs(diff))*!pi
	    ;print,'Final angles: anb',180./!pi*anb,'   ans',180./!pi*ans,'   diff',180./!pi*diff
	    IF diff LT 0 THEN BEGIN
	        Ndex = Ndex + 1.
		ndex_dis = ndex_dis + nbind
	    ENDIF
	    IF diff GT 0 THEN BEGIN
	        Nsin = Nsin + 1.
		nsin_dis = nsin_dis + nbind
	    ENDIF
	    ;print,'Ndexdis=',ndex_dis,'  nsin_dis=',nsin_dis
	 ENDIF  
	 
       ENDFOR    
       ;Compute chirality
	    
       ;fractional(Pevtsov)
	IF (Ndex + Nsin) NE 0 THEN cf = (Ndex - Nsin)/(Ndex + Nsin)
	;fractional 2 (cumulating branches sizes)
	IF (ndex_dis + nsin_dis) NE 0 THEN cf2 = (ndex_dis - nsin_dis)/(ndex_dis + nsin_dis)
    ENDIF 
RETURN,cf2
END

;################################################
FUNCTION EFR_CHIRALITY2,p_ske,bnd_ind,xsize,ysize
mask=bytarr(xsize,ysize)
;a effacer
     ;
     nbnd = N_ELEMENTS(bnd_ind)
     nske = N_ELEMENTS(p_ske)
     xbnd = bnd_ind MOD xsize
     ybnd = bnd_ind / xsize
     xske = p_ske MOD xsize
     yske = p_ske / xsize
     ata = 0
     atb = 0
     ;set the first point of bnd_ind as the closest to first ske indice
     xske0 = xske[0]
     yske0 = yske[0]
     dist = SQRT((xbnd - xske0)^2+(ybnd - yske0)^2)
     wwd = WHERE(dist EQ MIN(dist))
     bnd_ind = SHIFT(bnd_ind, nbnd - wwd[0]) 
 
     ;Same thing with end of skeleton
     xbnd = bnd_ind MOD xsize
     ybnd = bnd_ind / xsize
     xskee = xske[nske-1]
     yskee = yske[nske-1]
     dist = SQRT((xbnd - xskee)^2+(ybnd - yskee)^2)
     wwd = WHERE(dist EQ MIN(dist))
 
     ;separate into 2 tables (both sides of the skeleton)
     bnd_ind_a = bnd_ind[0:wwd[0]-1]
     bnd_ind_b = bnd_ind[wwd[0]:nbnd-1]
     
     IF SQRT((xske0 - bnd_ind_a[0] MOD xsize)^2+(yske0 - bnd_ind_a[0] / xsize)^2) GT 3 THEN BEGIN
         PRINT,'EFR_CHIRALITY: Error in trying to adjust skeleton start and boundary start'
	 RETURN,0
     ENDIF
     IF SQRT((xskee - bnd_ind_b[0] MOD xsize)^2+(yskee - bnd_ind_b[0] / xsize)^2) GT 3 THEN BEGIN
         PRINT,'EFR_CHIRALITY: Error in trying to adjust skeleton end and boundary midway'
	 RETURN,0
     ENDIF
     
     nbnda = N_ELEMENTS(bnd_ind_a)
     nbndb = N_ELEMENTS(bnd_ind_b)
     
     FOR ii= 1,nbnda-2 DO BEGIN
        x0 = xbnd[ii-1]
	y0 = ybnd[ii-1]
	xe = xbnd[ii+1]
	ye = ybnd[ii+1]
	anb = ATAN(ye-y0,xe-x0)
	IF abs(anb) GT !pi/2. THEN anb = anb - anb/(abs(anb))*!pi
	;corresponding ske orientation
	rb = ii*1./nbnda
	pos = FIX(rb*nske)
	pos1 = pos - 10
	pos2 = pos + 10
	IF pos1 LT 0 THEN BEGIN
	   pos1 = 0
	   pos2 = 20
	   IF pos2 GE nske THEN pos2 = nske-1
	ENDIF   
	IF pos2 GE nske THEN BEGIN
	   pos1 = nske - 21
	   pos2 = nske-1 
	   IF pos1 LT 0 THEN pos1 = 0
	ENDIF
	x0 = xske[pos1]
	y0 = yske[pos1]
	xe = xske[pos2]
	ye = yske[pos2]
	ans =  ATAN(ye-y0,xe-x0) 
	IF abs(ans) GT !pi/2. THEN ans = ans - ans/(abs(ans))*!pi
	diff = anb-ans
	IF abs(diff) GT !pi/2. THEN diff = diff - diff/(abs(diff))*!pi
	ata = [ata,diff]
	;mask[xe,ye]=1b
	;tvscl,mask
	;wait,1
     ENDFOR	
     
     ;inverse skeleton indices order
     p_ske = REVERSE(p_ske)
     xske = p_ske MOD xsize
     yske = p_ske / xsize
     
     FOR ii= 1,nbndb-2 DO BEGIN
        x0 = xbnd[nbnda+ii-1]
	y0 = ybnd[nbnda+ii-1]
	xe = xbnd[nbnda+ii+1]
	ye = ybnd[nbnda+ii+1]
	anb = ATAN(y0-ye,x0-xe)
	IF abs(anb) GT !pi/2. THEN anb = anb - anb/(abs(anb))*!pi
	;at = [at,ATAN(ye-y0,xe-x0)]
	;print,180/!pi*ATAN(ye-y0,xe-x0)
	
	;corresponding ske orientation
	rb = ii*1./nbndb
	pos = FIX(rb*nske)
	pos1 = pos - 10
	pos2 = pos + 10
	IF pos1 LT 0 THEN BEGIN
	   pos1 = 0
	   pos2 = 20
	   IF pos2 GE nske THEN pos2 = nske-1
	ENDIF   
	IF pos2 GE nske THEN BEGIN
	   pos1 = nske - 21
	   pos2 = nske-1 
	   IF pos1 LT 0 THEN pos1 = 0
	ENDIF
	x0 = xske[pos1]
	y0 = yske[pos1]
	xe = xske[pos2]
	ye = yske[pos2]
	ans =  ATAN(y0-ye,x0-xe)
	IF abs(ans) GT !pi/2. THEN ans = ans - ans/(abs(ans))*!pi
	diff = anb - ans
	IF abs(diff) GT !pi/2. THEN diff = diff - diff/(abs(diff))*!pi
	atb = [atb,diff]
	;mask[xe,ye]=1b
	;tvscl,mask
	;wait,1
     ENDFOR	
 ;print,180/!pi*at
 IF nske GT 100 THEN BEGIN
 plot,histogram((180./!pi)*[ata,atb] ,binsize=1)
 ;window,12
 ;plot,histogram(180/!pi*atb,binsize=1)
 wait,10
 ENDIF
 print,'----------------------------------'
RETURN,0
END

FUNCTION REDUCE_ANGLE,ang
   IF abs(ang) GT !pi/2. THEN ang = ang - ang/(abs(ang))*!pi
RETURN,ang
END
;################################################
FUNCTION EFR_CHIRALITY3,p_ske,ind,im,xsize,ysize

;a effacer

;test pour un filament droit, ie on n'utilise que le premier et le dernier
;point du ske pour touver l'orientation generale
mask=intarr(xsize,ysize)

	nske = N_ELEMENTS(p_ske)
	nseg = nske / 11
	rest = nske MOD 11
	ske_points = 0
	ske_ang = 0
	short = 0
	IF nseg GE 2 THEN BEGIN
	    FOR aa=0,nseg-1 DO BEGIN
	        ind0 = p_ske[aa*11]
	        ind1 = p_ske[(aa+1)*11-1]
	        indm = p_ske[aa*11+5]
	        ans = ATAN(ind1/xsize - ind0/xsize, ind1 MOD xsize - ind0 MOD xsize)
		IF abs(ans) GT !pi/2. THEN ans = ans - ans/(abs(ans))*!pi
	        ske_points = [ske_points,indm]
	        ske_ang = [ske_ang,ans]
	    ENDFOR
	    IF rest GT 5 THEN BEGIN
	        ind0 = p_ske[nseg*11]
	        ind1 = p_ske[nske-1]
	        indm = p_ske[nseg*11+rest/2]
	        ans = ATAN(ind1/xsize - ind0/xsize, ind1 MOD xsize - ind0 MOD xsize)
		IF abs(ans) GT !pi/2. THEN ans = ans - ans/(abs(ans))*!pi
	        ske_points = [ske_points,indm]
	        ske_ang = [ske_ang,ans]
	    ENDIF
	    ske_points = ske_points[1:N_ELEMENTS(ske_points)-1]
	    ske_points_x = ske_points MOD xsize
	    ske_points_y = ske_points / xsize
	    ske_ang = ske_ang[1:N_ELEMENTS(ske_ang)-1]
	ENDIF ELSE BEGIN
	    ind0 = p_ske[0]
	    ind1 = p_ske[nske-1]
	    ans = ATAN(ind1/xsize - ind0/xsize, ind1 MOD xsize - ind0 MOD xsize)
	    IF abs(ans) GT !pi/2. THEN ans = ans - ans/(abs(ans))*!pi
	    short = 1
	ENDELSE
	
	
	tab = 0
	magtab=0
	;gradient map
	FOR ii=0,N_ELEMENTS(ind)-1 DO BEGIN
	  xi = ind[ii] MOD xsize
	  yi = ind[ii] / xsize
	  dx = im[xi+1,yi] - im[xi-1,yi]
	  dy = im[xi,yi+1] - im[xi,yi-1]
	  ;magtab = [magtab,SQRT(dx^2+dy^2)]
	  mag = SQRT(dx^2+dy^2)
	  mask[xi,yi]=mag
	  ;ici il faut ajuster le trehshold en fonction des valeurs, ex seuil correspondant a 20% des valeurs
	  IF mag LT 500 THEN BEGIN
	  	an = ( ATAN(dy,dx) + !pi/2. ) MOD !pi
		IF abs(an) GT !pi/2. THEN an = an - an/(abs(an))*!pi
		IF short EQ 1 THEN BEGIN
	  	    diff = an - ans
		ENDIF ELSE BEGIN
		    dist = SQRT((xi - ske_points_x)^2+(yi - ske_points_y)^2)
		    wmin = WHERE(dist EQ MIN(dist),ndis)
		    ;print,nseg,wmin[0]
		    ;print,'--------------------------'
		    diff = an - ske_ang[wmin[0]]
		ENDELSE    
	  	IF abs(diff) GT !pi/2. THEN diff = diff - diff/(abs(diff))*!pi
	  	tab = [tab,diff]
	  ENDIF
        ENDFOR
	
	tvscl,mask
	wait,1
	IF nske GT 150 THEN BEGIN
	  
      	  ;plot,histogram((180./!pi)*tab ,binsize=1)
	  ;print,nske
	  ;wait,5
	  ;plot,histogram(magtab ,binsize=1)
	  ;wait,10
	  ;print,(180./!pi)*ske_ang
	  ;IF nske EQ 164 THEN retall
	ENDIF
RETURN,0
END
;################################################
FUNCTION EFR_BITPLAN_RLE,bytim,bit
;###NOT FINISHED
;###NOT USED
  ;;;;Bit Plan Run Length Encoding
  siz = SIZE(bytim)
  IF siz[0] NE 2 THEN RETALL
  ;control image type
  nrow = siz[2]
  ncol = siz[1]

  ;bit plans (8 bit tab)
  IF bit LT 1 OR bit GT 3 THEN RETALL
  bp3 = ISHFT(ISHFT(bytim,5),-7)
  bp2 = ISHFT(ISHFT(bytim,6),-7)
  bp1 = ISHFT(ISHFT(bytim,7),-7)
  IF bit EQ 3 THEN bpt=bp3
  IF bit EQ 2 THEN bpt=bp2
  IF bit EQ 1 THEN bpt=bp1
  TVSCL,CONGRID(bpt,300,300)

  rle = STRARR(nrow)
  FOR ii=0,nrow-1 DO BEGIN
   row0 = bpt[*,ii]
   ww = WHERE(row0 EQ 1,COMP=wwc,n1)
   IF n1 GT 0 THEN BEGIN
     IF n1 LT ncol THEN BEGIN
       strt = STRTRIM(FIX(row0[0]),2)
       ww1=WHERE(ABS(ww-shift(ww,-1)) GT 1,nww1)
       ww0=WHERE(ABS(wwc-shift(wwc,-1)) GT 1,nww0)
;ligne ci-dessous fausse
       IF nww0 GT 1 THEN pos=[ww[ww1]+1,wwc[ww0[0:(nww0-2)]]+1] ELSE pos=[ww[ww1]+1]
       npos = N_ELEMENTS(pos)
       IF npos GT 1 THEN pts=[STRARR(n_elements(pos)-1)+'.',';'] ELSE pts=';'
       rle[ii]=STRJOIN([strt,STRTRIM(pos,2)+pts])
     ENDIF ELSE BEGIN    
       rle[ii]='1;'
     ENDELSE
   ENDIF ELSE BEGIN
     rle[ii]='0;'
   ENDELSE

ENDFOR
rle=[STRTRIM(ncol,2)+';',rle]
print,STRJOIN(rle)
RETURN,rle;or STRJOIN(rle)
END

;################################################
FUNCTION EFR_PRUNING2,indices,xsize,ysize
;###DOES NOT WORK####
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
;###DOES NOT WORK####

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

