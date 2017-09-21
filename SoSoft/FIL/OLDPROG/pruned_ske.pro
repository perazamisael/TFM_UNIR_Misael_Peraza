;version ayant servi a la creation des premiers ascii
;pour la db de Sergei (janvier 2004)
;appeler par fill_struct.pro elle meme
;appelee par filelist2struct.pro

FUNCTION MIXVALUE,indices,xsize,ysize

  mask=bytarr(xsize,ysize)
  output=bytarr(xsize,ysize)
  mask[indices]=1b
  AA=WHERE(mask EQ 1b,nAA)

  FOR k=0,nAA-1 DO BEGIN
      N4P=TOTAL(mask[[AA[k]-1,AA[k]+1,AA[k]-xsize,AA[k]+xsize]])
      tot=N4P+1
      IF mask[AA[k]-xsize-1] EQ 1 AND TOTAL(mask[[AA[k]-1,AA[k]-xsize]]) $
         EQ 0 THEN tot=tot+1
      IF mask[AA[k]-xsize+1] EQ 1 AND TOTAL(mask[[AA[k]-xsize,AA[k]+1]]) $
         EQ 0 THEN tot=tot+1
      IF mask[AA[k]+xsize-1] EQ 1 AND TOTAL(mask[[AA[k]-1,AA[k]+xsize]]) $
         EQ 0 THEN tot=tot+1
      IF mask[AA[k]+xsize+1] EQ 1 AND TOTAL(mask[[AA[k]+xsize,AA[k]+1]]) $
         EQ 0 THEN tot=tot+1
      output[AA[k]]=tot
  ENDFOR

RETURN,output

END

;###################################################################

FUNCTION GET_SHAPE_TYPE,indices,xsize,ysize

;############
;NOT FINISHED
;NOT USED
;############

  shape=''
  maskv = MIXVALUE(indices,xsize,ysize)
  wendpoints = WHERE(maskv EQ 2b,endpoints)
  IF endpoints EQ 0 THEN shape_type='lake' ELSE BEGIN
     shape_type='line segment'
     ;#######add the test for bay-like objects
  ENDELSE  

RETURN,shape_type

END

;###################################################################

FUNCTION ORDER_IND_OLD,indices,xsize,ysize
;order indices of a skeleton from a first one to the successive
;neighbors
;if the indices draw a lake object, the first point is
;the smallest indice
;(slower than order_ind)

  ind        = indices
  nele       = N_ELEMENTS(indices)
  order      = LONARR(nele)
  valuesmap  = MIXVALUE(ind,xsize,ysize) ;val 2 or 3
  where2     = WHERE(valuesmap EQ 2,nv)

  IF nv NE 2 THEN BEGIN ;dans le cas lake, on supprime le 1er pix pour faire une chaine
    ind        = ind[1:nele-1]
    nele       = nele-1
    order      = LONARR(nele)
    valuesmap  = MIXVALUE(ind,xsize,ysize) 
    where2     = WHERE(valuesmap EQ 2,nv)
    lake       = 1
  ENDIF ELSE lake = 0

  order[0]  = where2[0]
  order[nele-1] = where2[1]
  valuesmap[where2[0]] = 0b
  ind    = WHERE(valuesmap,nval)

  FOR ii=1,nele-2 DO BEGIN
    valuesmap2 = MIXVALUE(ind,xsize,ysize)
    where2     = WHERE(valuesmap2 EQ 2 AND valuesmap EQ 3,nv)   
    order[ii]  = where2[0]
    valuesmap[where2[0]] = 0b
    ind    = WHERE(valuesmap,nval)   
  ENDFOR

  IF lake EQ 1 THEN order=[indices[0],order]

RETURN,order
END
;###################################################################

FUNCTION ORDER_IND,indices,xsize,ysize
;order indices of a skeleton from a first one to the successive
;neighbors
;if the indices draw a lake object, the first point is
;the smallest indice
;pixels must be m-connected

  ;ind        = M_CONNECT(indices,xsize,ysize,/NOPRINT)
  ind        = indices
  nele       = N_ELEMENTS(ind)
  order      = LONARR(nele,/NOZERO)
  valuesmap  = MIXVALUE(ind,xsize,ysize) ;val 2 or 3
  where2     = WHERE(valuesmap EQ 2,nv)

  IF nv NE 2 THEN BEGIN ;dans le cas lake, on supprime le 1er pix pour faire une chaine
    ind        = ind[1:nele-1]
    nele       = nele-1
    order      = LONARR(nele,/NOZERO)
    valuesmap  = MIXVALUE(ind,xsize,ysize) 
    where2     = WHERE(valuesmap EQ 2,nv)
    lake       = 1
  ENDIF ELSE lake = 0

  set=[-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,1+xsize]
  order[nele-1] = where2[1] 
  ep = where2[0]

  FOR ii=0,nele-2 DO BEGIN
   order[ii] = ep
   valuesmap[ep] = 0
   wnewpt = WHERE(valuesmap[set+ep])
   ep = ep + set[wnewpt[0]]
  ENDFOR

  IF lake EQ 1 THEN order=[indices[0],order]

RETURN,order
END

;###################################################################

FUNCTION RESAMPLE_IND,indices,factor
;indices must be ordered (order_ind())

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
FUNCTION GET_CURVATURE_IND,sampind,xsize,ysize

;   nele = N_ELEMENTS(sampind)
;   phi  = FLTARR(nele-2)
; 
;   FOR ii=0,nele-3 DO BEGIN
;     ind1  = sampind[ii]
;     ind2  = sampind[ii+1]
;     ind3  = sampind[ii+2]
;     cooy1 = ind1/xsize & coox1 = ind1 MOD xsize
;     cooy2 = ind2/xsize & coox2 = ind2 MOD xsize
;     cooy3 = ind3/xsize & coox3 = ind3 MOD xsize
;     U     = [coox1-coox2,cooy1-cooy2]
;     V     = [coox3-coox2,cooy3-cooy2]
;     phi[ii]= ACOS((U[0]*V[0]+U[1]*V[1])/(SQRT((U[0]*U[0]+U[1]* $
;             U[1])*(V[0]*V[0]+V[1]*V[1]))))
;   ENDFOR    
;
;   curvind = ABS(TOTAL(phi*!radeg-180.)/(nele-2))
;print,curvind

   nele = N_ELEMENTS(sampind)

   x1   = sampind[nele-1] MOD xsize & y1 = sampind[nele-1]/xsize
   x0   = sampind[0] MOD xsize      & y0 = sampind[0]/xsize
   dist_extr = SQRT( (x1-x0)^2 + (y1-y0)^2 )

   X0 = sampind[0:nele-2] MOD xsize & Y0 = sampind[0:nele-2]/xsize
   X1 = sampind[1:nele-1] MOD xsize & Y1 = sampind[1:nele-1]/xsize
   dist = TOTAL(SQRT( (X1-X0)^2 + (Y1-Y0)^2 ))

   curvind =  10.* (1.- dist_extr/dist) 

RETURN,curvind

END

;###################################################################
FUNCTION GET_BOUNDARY,indices,connect,xsize,ysize

  ;#############recherche du contour

    mask=bytarr(xsize,ysize)
    mask[indices]=1b
    IF connect EQ 8 THEN $
        erod=ERODE(mask,REPLICATE(1,3,3)) ;(8connect)
    IF connect EQ 4 THEN $
        erod=ERODE(mask,[[0,1,0],[1,1,1],[0,1,0]]) ;(4connect)
    wero=WHERE(erod,nero)
    IF nero NE 0 THEN mask[wero]=0b
    bound=WHERE(mask)

RETURN,bound
END

;###################################################################

FUNCTION GET_EXT_BOUNDARY,indices,connect,xsize,ysize


  ;############# recherche du contour EXTERIEUR
  ;############# afin d'eviter que le contour soit
  ;############# trop fin par endroits (1pix -> pb pour chain_code)
 
    IF connect NE 4 AND connect NE 8 THEN RETALL
    mask = BYTARR(xsize,ysize)
    set4 = [-xsize,xsize,-1,1]
    set8 = [-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,xsize+1]
    IF connect EQ 4 THEN $
       FOR oo=0,3 DO mask[indices+set4[oo]]=1b
    IF connect EQ 8 THEN $
       FOR oo=0,7 DO mask[indices+set8[oo]]=1b
    mask[indices]=0b
    bound=WHERE(mask)

RETURN,bound
END
;###################################################################

FUNCTION GET_INNER_BOUNDARY,indices,connect,xsize,ysize


  ;############# recherche du contour INTERIEUR
  ;############# a priori equivalent a get_boundary
    IF connect NE 4 AND connect NE 8 THEN RETALL
    mask = BYTARR(xsize,ysize)
    mask[indices]=1b
    bound = LONARR(1) 
    set4 = [-xsize,xsize,-1,1]
    set8 = [-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,xsize+1]
    IF connect EQ 4 THEN $
       FOR oo=0,3 DO BEGIN
           mask[indices+set4[oo]]=0b
           bound = [bound,WHERE(mask)]
           mask[indices]=1b
       ENDFOR
    IF connect EQ 8 THEN $
       FOR oo=0,7 DO BEGIN
           mask[indices+set8[oo]]=0b
           bound = [bound,WHERE(mask)]
           mask[indices]=1b
       ENDFOR
    bound = bound[1,N_ELEMENTS(bound)-1]
    bound = bound[UNIQ(bound,SORT(bound))]
  
RETURN,bound
END

;###################################################################
FUNCTION CHAIN_CODE_SKE_OLD,indices,firstpt,xsize,ysize


   test=WHERE(indices EQ firstpt,ntest)
   IF ntest EQ 0 THEN BEGIN
      PRINT,'chain_code:First point does not belong to the set,returning...'
      RETURN,0
   ENDIF

   mask = BytArr(xsize, ysize)
   mask[indices] = 1b

   nele = N_ELEMENTS(indices)
   set1=[1,1+xsize,xsize,xsize-1,-1,-xsize-1,-xsize,-xsize+1]
   set2=[-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,1+xsize]
   chain=BYTARR(nele-1)

   ;TEST IF FIRSTPT IS AN ENDPOINT OF THE SEGMENT
   mv=MIXVALUE(indices,xsize,ysize)
   wmv=WHERE(mv EQ 2,nwmv)
   IF firstpt NE wmv[0] AND firstpt NE wmv[1] THEN BEGIN
      PRINT,'chain_code:bad first point for a segment,choosing one'
      firstpt=wmv[0]
   ENDIF

   ep1=firstpt
   wset2=WHERE(mask[set2+ep1]) 
   mask[ep1]=0b  
   ep=ep1+set2[wset2[0]]
   chain[0]=(WHERE(set1 EQ set2[wset2[0]]))[0]
   
   FOR ii=1,nele-2 DO BEGIN

    wset2=WHERE(mask[set2+ep])
    mask[ep]=0b
    ep=ep+set2[wset2[0]]
    chain[ii]=(WHERE(set1 EQ set2[wset2[0]]))[0]
 
   ENDFOR

RETURN,chain

END

;###################################################################

FUNCTION CHAIN_CODE_BND_OLD,indices,firstpt,xsize,ysize,Close=close

;cf remarque chain_code_ske

   test=WHERE(indices EQ firstpt,ntest)
   IF ntest EQ 0 THEN BEGIN
      PRINT,'chain_code:First point does not belong to the set,returning...'
      RETURN,0
   ENDIF

   mask = BytArr(xsize, ysize)
   mask[indices] = 1b

   nele = N_ELEMENTS(indices)
   set1=[1,1+xsize,xsize,xsize-1,-1,-xsize-1,-xsize,-xsize+1]
   set2=[-xsize-1,-xsize,-xsize+1,-1,1,xsize-1,xsize,1+xsize]
   ;chain=BYTARR(nele-1)

   IF NOT KEYWORD_SET(Close) THEN chain=BYTARR(nele-1) ELSE chain=BYTARR(nele)

   ep1=firstpt
   wset2=WHERE(mask[set2+ep1]) 
   mask[ep1]=0b  
   ep=ep1+set2[wset2[0]]
   chain[0]=(WHERE(set1 EQ set2[wset2[0]]))[0]
   

   FOR ii=1,nele-2 DO BEGIN

    wset2=WHERE(mask[set2+ep])
    mask[ep]=0b
    ep=ep+set2[wset2[0]]
    chain[ii]=(WHERE(set1 EQ set2[wset2[0]]))[0]
 
   ENDFOR

   IF KEYWORD_SET(Close) THEN BEGIN
    mask[ep1]=1b
    wset2=WHERE(mask[set2+ep])
    ep=ep+set2[wset2[0]]
    chain[nele-1]=(WHERE(set1 EQ set2[wset2[0]]))[0]
   ENDIF

RETURN,chain

END
;###################################################################

FUNCTION CHAIN_CODE,indices,xsize,ysize,SKE=ske

;return chain code for boundary or skeleton(/ske)
;indices must be ordered before (see ORDER_IND)

   
   nele = N_ELEMENTS(indices)
   chain= BYTARR(nele)

   tab1 = [indices[1:nele-1],indices[0]]
   xtab1= tab1 MOD xsize
   ytab1= tab1 / xsize

   tab2 = [indices]
   xtab2= tab2 MOD xsize
   ytab2= tab2 / xsize
    
   difx = xtab1 - xtab2
   dify = ytab1 - ytab2

   ;transx = [1, 1, 0,-1,-1,-1, 0, 1] ;trigo starts right
   ;transy = [0, 1, 1, 1, 0,-1,-1,-1]

   transx = [-1,-1, 0, 1, 1, 1, 0,-1] ;trigo starts left
   transy = [ 0,-1,-1,-1, 0, 1, 1, 1]

   FOR ii=0,7 DO BEGIN
    wt = WHERE(difx EQ transx[ii] AND dify EQ transy[ii],nt)
    IF nt GT 0 THEN chain[wt]=ii
   ENDFOR

   IF KEYWORD_SET(ske) THEN chain=chain[0:nele-2]

RETURN,chain

END

;###################################################################

FUNCTION SKELETON2,indices,xsize,ysize

;##########
;NOT USED
;##########

  mask=bytarr(xsize,ysize)
  mask[indices]=1b
  temp=mask*0b
  tvscl,mask
  set =[-1-xsize,-xsize,-xsize+1,-1,1,-1+xsize,xsize,xsize+1]
  set1=[xsize,1+xsize,1,-xsize+1,-xsize,-1-xsize,-1,-1+xsize,xsize]
  set2=[1+xsize,1,-xsize+1,-xsize,-1-xsize,-1,-1+xsize,xsize]
  numb=1

  ;###############THINNING
  WHILE numb GT 0 DO BEGIN

   wim=WHERE(mask,num1)

   bound=GET_BOUNDARY(wim,8,xsize,ysize)

;STEP1

   FOR kk=0,N_ELEMENTS(bound)-1 DO BEGIN
     Np=TOTAL(mask[bound[kk]+set])
     t1=mask[bound[kk]+set1]
     t2=[mask[bound[kk]+set2],0]
     wt=WHERE(t1 EQ 0 AND t2 EQ 1,nwt)
     Mpa=mask[bound(kk)+xsize]*mask[bound(kk)+1]*mask[bound(kk)-xsize]
     Mpb=mask[bound(kk)-1]*mask[bound(kk)+1]*mask[bound(kk)-xsize]
     IF Np GE 2 AND Np LE 6 AND nwt EQ 1 AND Mpa EQ 0 AND Mpb EQ 0 THEN $
     temp[bound[kk]]=1b
   ENDFOR

;DELETION

   delete=WHERE(temp,ndel)  
   IF ndel NE 0 THEN mask[delete]=0b
   temp=temp*0b
;   wimt=WHERE(mask)
;   bound=GET_BOUNDARY(wimt,8,xsize,ysize)

;STEP2

  FOR kk=0,N_ELEMENTS(bound)-1 DO BEGIN
     Np=TOTAL(mask[bound[kk]+set])
     t1=mask[bound[kk]+set1]
     t2=[mask[bound[kk]+set2],0]
     wt=WHERE(t1 EQ 0 AND t2 EQ 1,nwt)
     Mpc=mask[bound(kk)+xsize]*mask[bound(kk)+1]*mask[bound(kk)-1]
     Mpd=mask[bound(kk)-1]*mask[bound(kk)+xsize]*mask[bound(kk)-xsize]
     IF Np GE 2 AND Np LE 6 AND nwt EQ 1 AND Mpc EQ 0 AND Mpd EQ 0 THEN $
     temp[bound[kk]]=1b
   ENDFOR

;DELETION

   delete=WHERE(temp,ndel)  
   IF ndel NE 0 THEN mask[delete]=0b
   temp=temp*0b


   wim=WHERE(mask,num2)
   numb=num1-num2

   tvscl,mask

  ENDWHILE 

RETURN,WHERE(mask)
END

;################################################
FUNCTION SKELETON,indices,xsize,ysize,EC=ec
 
  mask=bytarr(xsize,ysize)
  mask[indices]=1b
  erosion_count=0

  tvscl,mask

  ;####################STRUCTURES
  h0=[[-1,-1,-1],[0,1,0],[1,1,1]] 
  m0=[[0,-1,-1],[1,1,-1],[0,1,0]]  
  h1=ROTATE(h0,1)
  m1=ROTATE(m0,1) 
  h2=ROTATE(h0,2)
  m2=ROTATE(m0,2) 
  h3=ROTATE(h0,3) 
  m3=ROTATE(m0,3)

  numb=1

  ;###############THINNING
  WHILE numb GT 0 DO BEGIN

   wim=WHERE(mask,num1)

   wimC=WHERE(CONVOL(mask,h0) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,m0) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,h1) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,m1) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,h2) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,m2) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,h3) EQ 4,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,m3) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wim=WHERE(mask,num2)
   numb=num1-num2
   erosion_count=erosion_count+1

   tvscl,mask

  ENDWHILE 

  ec=erosion_count+1

RETURN,WHERE(mask)
END

;###################################################################
FUNCTION CHECK_FILLING,indices,xsize,ysize

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

FUNCTION CHECK_FILLING_OLD,indices,xsize,ysize

  ;######avoid gap regions but keep big
  ;######ones in case of lake-like shapes

  mask = BYTARR(xsize,ysize)
  mask[indices] = 1b
  C_mask = ABS(mask-1b) ;complement de mask


  reg = LABEL_REGION(C_mask);,/all_neigh)
  h_reg = HISTOGRAM(reg,reverse_indices=rev)
  nbreg = N_ELEMENTS(h_reg) ;il y a au minimum 2 regions 


  IF nbreg EQ 2 THEN RETURN,indices ELSE BEGIN
     FOR uu=0,nbreg-1 DO BEGIN
         indreg=rev[rev[uu]:rev[uu+1]-1]
         IF N_ELEMENTS(indreg) LT N_ELEMENTS(indices)/3. $
            AND WHERE(indreg EQ 0) EQ -1 THEN BEGIN
            ;la 1ere cond. assure que la reg. a remplir est au plus
            ;le tiers de la region originale et la 2eme que ce n'est
            ;pas le bord de l'imagette (ne contientpas le point 0,0)
            mask[indreg]=1b
         ENDIF
     ENDFOR
  ENDELSE

  indices=WHERE(mask)
RETURN,indices
END

;###################################################################
FUNCTION M_CONNECT,indices,xsize,ysize,NOPRINT=noprint

  ;cf gonzalez p67 and pb 9.3

  mask=bytarr(xsize,ysize)
  mask[indices]=1b

  ;#######################STRUCTURES
  str1=[[-1,1,0],[1,1,-1],[0,-1,-1]]   
  str2=[[0,1,-1],[-1,1,1],[-1,-1,0]]
  str3=[[-1,-1,0],[-1,1,1],[0,1,-1]]
  str4=[[0,-1,-1],[1,1,-1],[-1,1,0]]

  numb=1

  ;###############THINNING
  WHILE numb GT 0 DO BEGIN

   wim=WHERE(mask,num1)

   wimC=WHERE(CONVOL(mask,str1) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,str2) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,str3) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b

   wimC=WHERE(CONVOL(mask,str4) EQ 3,nwimC)
   IF nwimC NE 0 THEN mask[wimC]=0b
 
   wim=WHERE(mask,num2)
   numb=num1-num2

   IF NOT KEYWORD_SET(noprint) THEN tvscl,mask 

  ENDWHILE 

RETURN,WHERE(mask)

END

;########################################################

FUNCTION GET_SKE_PATH,pt1,indices,xsize,ysize

;##########
;NOT USED
;##########

;   mask = BytArr(xsize, ysize)
;   mask[indices] = 1b
   mask=MIXVALUE(indices,xsize,ysize)
   set1=[-1,1,xsize,-xsize]
   set2=[-1-xsize,-1+xsize,1-xsize,1+xsize]
   diag = SQRT(2.0D)
   length=0.0
   pt = pt1
   path=[pt]

   REPEAT BEGIN

    mask[pt]=0b
    wset1=WHERE(mask[set1+pt] NE 0b,nset1)
    wset2=WHERE(mask[set2+pt] NE 0b,nset2)
    IF nset1 EQ 1 AND nset2 EQ 1 THEN nset2=0
    IF nset2 EQ 2 THEN nset2=1
    IF nset1 EQ 2 THEN nset1=1
   
    IF nset1 EQ 1 THEN BEGIN
       length=length+1.0
       pt=(set1[wset1])[0]+pt
       path=[path,pt]
    ENDIF
    IF nset2 EQ 1 THEN BEGIN
       length=length+diag
       pt=(set2[wset2])[0]+pt
       path=[path,pt]
    ENDIF
    IF mask[pt] GT 3b THEN RETURN,0
print,path
   ENDREP UNTIL mask[pt] EQ 2b


RETURN,[length,path]
END
;########################################################

FUNCTION GET_SKELETON_PROPERTIES,indices,head,xsize,ysize

;##########
;NOT USED
;##########

   mask = BytArr(xsize, ysize)
   mask[indices] = 1b
   imvalue=MIXVALUE(indices,xsize,ysize)
   ep=(WHERE(imvalue EQ 2))[0]
   ep2=(WHERE(imvalue EQ 2))[1]
   set1=[-1,1,xsize,-xsize]
   set2=[-1-xsize,-1+xsize,1-xsize,1+xsize]
   diag = SQRT(2.0D)

 ;#######GET THE LENGTH OF THE SEGMENT#######

   length=0.0

   WHILE ep NE ep2 DO BEGIN

    mask[ep]=0b
    wset1=WHERE(mask[set1+ep] EQ 1b,nset1)
    wset2=WHERE(mask[set2+ep] EQ 1b,nset2)
    IF nset1 EQ 1 THEN BEGIN
       length=length+1.0
       ep=(set1[wset1])[0]+ep
    ENDIF
    IF nset2 EQ 1 THEN BEGIN
       length=length+diag
       ep=(set2[wset2])[0]+ep
    ENDIF

   ENDWHILE

 ;#######GET THE LENGTH OF THE SEGMENT (deg)#######

   mask[indices] = 1b
   ep=(WHERE(imvalue EQ 2))[0]
   deglength=0.0

   WHILE ep NE ep2 DO BEGIN

    mask[ep]=0b
    wset1=WHERE(mask[set1+ep] EQ 1b,nset1)
    wset2=WHERE(mask[set2+ep] EQ 1b,nset2)
    IF nset1 EQ 1 THEN BEGIN
       ep_carr0=PIX2CARR([ep],head,xsize,ysize,FIXL0=90.) 
       ep=(set1[wset1])[0]+ep
       ep_carr1=PIX2CARR([ep],head,xsize,ysize,FIXL0=90.)        
       deglength=deglength+SQRT((ep_carr1[0]-ep_carr0[0])^2+ $
                                (ep_carr1[1]-ep_carr0[1])^2)
    ENDIF
    IF nset2 EQ 1 THEN BEGIN
       ep_carr0=PIX2CARR([ep],head,xsize,ysize,FIXL0=90.) 
       ep=(set2[wset2])[0]+ep
       ep_carr1=PIX2CARR([ep],head,xsize,ysize,FIXL0=90.)        
       deglength=deglength+SQRT((ep_carr1[0]-ep_carr0[0])^2+ $
                                (ep_carr1[1]-ep_carr0[1])^2)
    ENDIF

   ENDWHILE

 ;#####GET THE CENTER OF THE SEGMENT######

   mask[indices] = 1b
   ep=(WHERE(imvalue EQ 2))[0]
   cumul=0.0

   WHILE cumul LT FIX(length/2.0) DO BEGIN

    mask[ep]=0b
    wset1=WHERE(mask[set1+ep] EQ 1b,nset1)
    wset2=WHERE(mask[set2+ep] EQ 1b,nset2)
    IF nset1 EQ 1 THEN BEGIN
       cumul=cumul+1.0
       ep=(set1[wset1])[0]+ep
    ENDIF
    IF nset2 EQ 1 THEN BEGIN
       cumul=cumul+diag
       ep=(set2[wset2])[0]+ep
    ENDIF

   ENDWHILE

   ske_center_ind = ep

 ;#####GET THE CENTER OF THE SEGMENT in CARRINGTON######

   ske_center_carr = PIX2CARR([ske_center_ind],head,xsize,ysize)
 
RETURN,[length,deglength,ske_center_ind,ske_center_carr[0],ske_center_carr[1]]

END

;########################################################

FUNCTION GET_SKE_PROPERTIES_PIX,indices,xsize,ysize

;cf remarque chain_code_ske

   mask = BytArr(xsize, ysize)
   mask[indices] = 1b
   imvalue=MIXVALUE(indices,xsize,ysize)
   ep=(WHERE(imvalue EQ 2))[0]
   ep2=(WHERE(imvalue EQ 2))[1]
   set1=[-1,1,xsize,-xsize]
   set2=[-1-xsize,-1+xsize,1-xsize,1+xsize]
   diag = SQRT(2.0D)

 ;#######GET THE LENGTH OF THE SEGMENT#######

   length=0.0

   WHILE ep NE ep2 DO BEGIN

    mask[ep]=0b
    wset1=WHERE(mask[set1+ep] EQ 1b,nset1)
    wset2=WHERE(mask[set2+ep] EQ 1b,nset2)
    IF nset1 EQ 1 THEN BEGIN
       length=length+1.0
       ep=(set1[wset1])[0]+ep
    ENDIF
    IF nset2 EQ 1 THEN BEGIN
       length=length+diag
       ep=(set2[wset2])[0]+ep
    ENDIF

   ENDWHILE

 ;#####GET THE CENTER OF THE SEGMENT######

   mask[indices] = 1b
   ep=(WHERE(imvalue EQ 2))[0]
   cumul=0.0

   WHILE cumul LT FIX(length/2.0) DO BEGIN

    mask[ep]=0b
    wset1=WHERE(mask[set1+ep] EQ 1b,nset1)
    wset2=WHERE(mask[set2+ep] EQ 1b,nset2)
    IF nset1 EQ 1 THEN BEGIN
       cumul=cumul+1.0
       ep=(set1[wset1])[0]+ep
    ENDIF
    IF nset2 EQ 1 THEN BEGIN
       cumul=cumul+diag
       ep=(set2[wset2])[0]+ep
    ENDIF

   ENDWHILE

   ske_center_ind = ep
 
RETURN,[length,ske_center_ind]

END

;########################################################

FUNCTION GET_SKE_LENGTH_DEG,indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo
;no need of order_ind but longer

   mask = BytArr(nax1, nax2)
   mask[indices] = 1b
   imvalue = MIXVALUE(indices,nax1,nax2)
   ep = (WHERE(imvalue EQ 2))[0]
   ep2 = (WHERE(imvalue EQ 2))[1]
   set1 = [-1,1,nax1,-nax1]
   set2 = [-1-nax1,-1+nax1,1-nax1,1+nax1]

 ;#######GET THE LENGTH OF THE SEGMENT (deg)#######

   deglength=0.0

   WHILE ep NE ep2 DO BEGIN

    mask[ep] = 0b
    wset1 = WHERE(mask[set1+ep] EQ 1b,nset1)
    wset2 = WHERE(mask[set2+ep] EQ 1b,nset2)
    IF nset1 EQ 1 THEN BEGIN
       ep_carr0 = PIX2CARR([ep],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)
       ep = (set1[wset1])[0]+ep
       ep_carr1 = PIX2CARR([ep],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)
       IF ABS(ep_carr1[1]-ep_carr0[1]) LT 180. THEN BEGIN
       deglength=deglength+SQRT((ep_carr1[0]-ep_carr0[0])^2+ $
                                (ep_carr1[1]-ep_carr0[1])^2)
       ENDIF ELSE BEGIN
       deglength=deglength+SQRT((ep_carr1[0]-ep_carr0[0])^2+ $
                      (ABS(ep_carr1[1]-ep_carr0[1])-360.)^2)      
       ENDELSE

    ENDIF
    IF nset2 EQ 1 THEN BEGIN
       ep_carr0 = PIX2CARR([ep],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)
       ep = (set2[wset2])[0]+ep
       ep_carr1 = PIX2CARR([ep],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.) 
       IF ABS(ep_carr1[1]-ep_carr0[1]) LT 180. THEN BEGIN
       deglength=deglength+SQRT((ep_carr1[0]-ep_carr0[0])^2+ $
                                (ep_carr1[1]-ep_carr0[1])^2)
       ENDIF ELSE BEGIN
       deglength=deglength+SQRT((ep_carr1[0]-ep_carr0[0])^2+ $
                      (ABS(ep_carr1[1]-ep_carr0[1])-360.)^2)       
       ENDELSE

    ENDIF

   ENDWHILE

 
RETURN,deglength

END
;########################################################

FUNCTION GET_FIL_AREA_DEG2,indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo
;WRONG!
  npoints = N_ELEMENTS(indices)
  degarea = 0.

  FOR ii = 0, npoints-1 DO BEGIN
    coord = PIX2CARR([indices[ii]],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)
    coord = (coord + 90.) MOD 360. - 90.
    ;theta = ASIN( SQRT( (COS(coord[0]/!radeg)*SIN(coord[1]/!radeg))^2 + $
    ;                    (SIN(coord[0]/!radeg))^2 ) )
    theta = ACOS(COS(coord[0]/!radeg)*COS(coord[1]/!radeg))
    pixar = (90D / rsun)^2 * (1D / COS(theta))
    degarea = degarea + pixar
  ENDFOR

RETURN,degarea

END
;########################################################

FUNCTION GET_FIL_AREA_DEG2_APPROX,indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo
;WRONG!!
  npoints = N_ELEMENTS(indices)

  mask    = Bytarr(nax1,nax2)
  mask[indices] = 1b

  xce     = LONG(Total(Total(mask,2)*Indgen(nax1))/FLOAT(npoints))
  yce     = LONG(Total(Total(mask,1)*Indgen(nax2))/FLOAT(npoints))

  coord = PIX2CARR([xce+yce*nax1],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)
  coord = (coord + 90.) MOD 360. - 90.
  coord = coord ;- coord/20. ;to avoid very high values near the limb
  ;theta = ASIN( SQRT( (COS(coord[0]/!radeg)*SIN(coord[1]/!radeg))^2 + $
  ;                    (SIN(coord[0]/!radeg))^2 ) )
  theta = ACOS(COS(coord[0]/!radeg)*COS(coord[1]/!radeg))
  pixar = (90D / rsun)^2 * (1D / COS(theta))
  degarea = pixar*npoints

RETURN,degarea

END
;###################################################################

FUNCTION GET_FIL_AREA,indices,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo

;indices must be ordered, see ORDER_IND

  npoints = N_ELEMENTS(indices)
  degarea = 0.

    coord0 = PIX2CARR([indices[0]],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)
    coord0 = (coord0 + 90.) MOD 360. - 90.
    lat1 = coord0[0] & lon1 = coord0[1]
    
  FOR ii = 1, npoints-1 DO BEGIN
    coord = PIX2CARR([indices[ii]],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,FIXL0=0.)
    coord = (coord + 90.) MOD 360. - 90.
    lat2 = coord[0] & lon2 = coord[1]
    degarea = degarea + (lon2+lon1)*(lat2-lat1)
    lat1 = lat2 & lon1 = lon2
  ENDFOR

    lat2 = coord0[0] & lon2 = coord0[1]
    degarea = degarea + (lon2+lon1)*(lat2-lat1)

    degarea = ABS(degarea)/2.


RETURN,degarea
END

;###################################################################

FUNCTION PRUNING,indices,xsize,ysize

  ;#########compute values depending on the neighbors count

  imske=MIXVALUE(indices,xsize,ysize)

  ;#########pruning

  wnp4 = WHERE(imske GE 4,NP4)

  WHILE NP4 GT 0 DO BEGIN

    wnp2 = WHERE(imske EQ 2,NP2)
    xwnp2= wnp2 MOD xsize
    ywnp2= wnp2 / xsize
    xwnp4= wnp4 MOD xsize
    ywnp4= wnp4 / xsize

    FOR ii=0,NP4-1 DO BEGIN
       dis = SQRT((xwnp2-xwnp4[ii])^2+(ywnp2-ywnp4[ii])^2) ;euclid
       ;dis = ABS(xwnp2-xwnp4[ii]) + ABS(ywnp2-ywnp4[ii]) ;city block
       mini = WHERE(dis EQ MIN(dis),nmin)
       ;IF nmin NE 1 THEN PRINT,'branches de taille egales!'
       xpt = wnp2[mini[0]] MOD xsize
       ypt = wnp2[mini[0]] / xsize
       imske[xpt,ypt]=0b
    ENDFOR

    ske_ind=SKELETON(WHERE(imske),xsize,ysize);remplace M_CONNECT
    imske=MIXVALUE(ske_ind,xsize,ysize)
    wnp4=WHERE(imske GE 4,NP4)

  ENDWHILE

  ske=WHERE(imske)

RETURN,ske
END
;###################################################################

FUNCTION PRUNING3,indices,xsize,ysize

;####NOT FINISHED###

  ;#########compute values depending on the neighbors count

  imske = MIXVALUE(indices,xsize,ysize)

  ;#########pruning

  wnp2 = WHERE(imske EQ 2,NP2)

  WHILE NP2 GT 2 DO BEGIN

    imske[wnp2] = 0b
    indices2 = WHERE(imske)
    imske = MIXVALUE(indices2,xsize,ysize)
    wnp2 = WHERE(imske EQ 2,NP2) 

  ENDWHILE
   
    imske[indices] = 1
    imske[indices2] = 2
    imske[wnp2] = 3

  tvscl,imske  

 ; FOR ii=0,1 DO BEGIN
    
    
 ; ENDFOR

 
RETURN,0
END

;###################################################################

FUNCTION PRUNING2,indices,xsize,ysize
;####NOT USED###
   set=[-1-xsize,-xsize,1-xsize,-1,+1,xsize-1,xsize,xsize+1]

  ;#########compute values depending on the neighbors count

  imske=MIXVALUE(indices,xsize,ysize)

  ;#########pruning

  wnp4 = WHERE(imske GE 4,NP4)

  WHILE NP4 GT 0 DO BEGIN

    FOR ii=0,NP4-1 DO BEGIN
       wnp3=WHERE(imske[set+wnp4[ii]] EQ 3,NP3)
       tab1=FLTARR(NP3)+10^3
       tab2=STRARR(NP3)
       imske2=imske
       imske2[wnp4[ii]]=0b
       FOR jj=0,NP3-1 DO BEGIN
print,ii,jj
print,wnp4[ii]
print,set[wnp3[jj]]+wnp4[ii]
print,'********'
           path=GET_SKE_PATH(set[wnp3[jj]]+wnp4[ii],WHERE(imske2),xsize,ysize)
           IF path[0] NE 0 THEN BEGIN
               tab1[jj]=path[0]
print,path[1:N_ELEMENTS(path)-1]
               tab2[jj]=STRING(path[1:N_ELEMENTS(path)-1],format='(1000I8)')
           ENDIF
       ENDFOR
       wtab1 = WHERE(tab1 LT 10^3,npath)
       IF npath GE 2 THEN BEGIN
          mni = MIN(tab1,subjj)
          dele = FIX(tab2[subjj])
          imske[dele]=0b
       ENDIF
tvscl,imske
    ENDFOR

    ske_ind=SKELETON(WHERE(imske),xsize,ysize)
    imske=MIXVALUE(ske_ind,xsize,ysize)
    wnp4=WHERE(imske GE 4,NP4)

  ENDWHILE

  ske=WHERE(imske)

RETURN,ske
END

;###################################################################

FUNCTION PRUNED_SKE,indices,xsize,ysize,EC=ec

  ;window,/free,xs=xsize,ys=ysize

  im=bytarr(xsize,ysize)
  im[indices]=1b

  ;#########full skeleton

   ske_ind=SKELETON(indices,xsize,ysize,EC=ec)
  ;ske_ind=SKELETON2(indices,xsize,ysize)
  ;ske_ind=M_CONNECT(ske_ind,xsize,ysize)

  ;#########pruning

  ske = PRUNING(ske_ind,xsize,ysize)   

RETURN,ske
END



