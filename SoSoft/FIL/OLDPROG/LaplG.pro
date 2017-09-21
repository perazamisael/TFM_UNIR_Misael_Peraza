FUNCTION LaplG,nn

coeff=-1.7
sig=0.33d
G=FLTARR(nn,nn)

FOR ii=(1-nn)/2.,(nn-1)/2. DO BEGIN
  FOR jj=(1-nn)/2.,(nn-1)/2. DO BEGIN
     G[ii+(nn-1)/2,jj+(nn-1)/2]=coeff*((ii^2+jj^2-sig^2)/sig^4)*exp(-1.*(ii^2+jj^2)/(2.*sig^2))
  ENDFOR
ENDFOR

print,G
RETURN,G
END
