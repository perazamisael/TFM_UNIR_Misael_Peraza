FUNCTION CHAIN2IND,first,chainc,xsize,ysize

;From a first point coordinates (first[0],first[1]) 
;and a chain code (chainc),convert the chain code
;to the corresponding indices in the array
;of size (xsize,ysize)  
;NF2003

   ind    = first[1]*LONG(xsize) + first[0]
   cctab  = BYTE(chainc)-"60b
   length = N_ELEMENTS(cctab)
   set    = [-1,-xsize-1,-xsize,-xsize+1,1,xsize+1,xsize,xsize-1]
   newind = ind

   FOR jj = 0,length-1 DO BEGIN
      newind = newind + set[cctab[jj]]
      ind    = [ind,newind]
   ENDFOR      
   
RETURN,ind
END

