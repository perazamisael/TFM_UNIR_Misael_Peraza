
RESTORE,DIALOG_PICKFILE()

ske_chains = str[*].fil_ske_chain

FOR ii=0,N_ELEMENTS(ske_chains)-1 DO BEGIN

    chainii = ske_chains[ii]
    siz     = STRLEN(chainii)
    tab     = INTARR(siz)

  FOR jj=0,siz-1 DO BEGIN
      tab[jj]=FIX(STRMID(chainii,jj,1))
  ENDFOR

    tab2    = INTARR(siz+1)
    tab2[0] = 0
  
  FOR kk=0,siz-2 DO BEGIN
    dir0 = tab[kk] & dir1 = tab[kk+1]
    diff = (dir1 - dir0) MOD 3
    tab2[kk+1] = diff*(-45) + tab2[kk]
  ENDFOR

  tab2 = smooth(tab2,5,/edge_truncate)
  curv = FIX(10.*(ABS(MAX(tab2) - MIN(tab2))/(180.))+0.5)

  arr = BYTARR(800,800)
  ind = CHAIN2IND([150,100],chainii,800,800)
  arr[ind]=1b
  
  arr2 = BYTARR(400,400)

  FOR a=0,399 DO BEGIN
      FOR b=0,399 DO BEGIN
          arr2[a,b]=arr(a*2,b*2)
      ENDFOR
  ENDFOR

  ;arr2=INTERPOLATE(arr,findgen(400)*2,findgen(400)*2,/GRID)
  tvscl,arr2
wait,2

ENDFOR

END