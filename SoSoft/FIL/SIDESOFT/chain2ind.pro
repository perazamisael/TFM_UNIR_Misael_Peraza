FUNCTION CHAIN2IND,first,chainc,xsize,ysize

;From a first point (first) and a chain code (chainc),
;find the corresponding indices in the array
;of size (xsize,ysize)  
;NF2003

   ind    = first[1]*LONG(xsize) + first[0]
   length = STRLEN(chainc)
   set    = [-1,-xsize-1,-xsize,-xsize+1,1,xsize+1,xsize,xsize-1]
   newind = ind

   FOR jj = 0,length-1 DO BEGIN
      next   = FIX(STRMID(chainc,jj,1))
      newind = newind + set[next]
      ind    = [ind,newind]
   ENDFOR      
   
RETURN,ind
END

