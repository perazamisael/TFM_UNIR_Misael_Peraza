FUNCTION CHAIN2IND,first,chainc,xsize,ysize
  
   ind=first[1]*LONG(xsize)+first[0]
   length=STRLEN(chainc)
   set=[1,xsize+1,xsize,xsize-1,-1,-xsize-1,-xsize,-xsize+1]
   newind=ind
   FOR jj=0,length-1 DO BEGIN
      next=FIX(STRMID(chainc,jj,1))
      newind=newind+set[next]
      ind=[ind,newind]
   ENDFOR      
   
RETURN,ind
END

;###########################################

FUNCTION STRUCT_DRAW_FIL,struct,num

;a corriger: fil_ske_chain -> fil_ske_chain_arcs

;#####draw filaments from skeletons
;#####stored in structure(struct) for one day(num)

  index=struct[*].fil_index
  windex=WHERE(index EQ 1)
  windex=[windex,N_ELEMENTS(index)]
  daystr=struct[windex[num]:windex[num+1]-1]
  print,daystr[0].source.im_obs_ut

  nn=N_ELEMENTS(daystr)
  im=bytarr(1024,1024)
  
  FOR ii=0,nn-1 DO BEGIN
   
      ptstart = daystr[ii].fil_ske_chain_strt
      chain   = daystr[ii].fil_ske_chain
      indices = CHAIN2IND(ptstart,chain,1024,1024)
      im[indices] = 1b

  ENDFOR

  window,2,xs=1024,ys=1024
  tvscl,im

;goto,no_bnd

  FOR ii=0,nn-1 DO BEGIN
   
      ptstart = daystr[ii].fil_bnd_chain_strt
      chain   = daystr[ii].fil_bnd_chain
      indices = CHAIN2IND(ptstart,chain,1024,1024)
      im[indices] = 1b

  ENDFOR
  
  tvscl,im

no_bnd:

RETURN,im

END

;##############################################

