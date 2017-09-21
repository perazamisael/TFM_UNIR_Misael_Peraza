@/home/fuller/IDL/FIL/pruned_ske
pro test

  input=bytarr(1024,1024)+1b
 ;####CREATE SOME MASKS
  mask_SUBS   = WHERE(input)
  mask_SUBS_Y = mask_SUBS /   1024
  mask_SUBS_X = mask_SUBS MOD 1024
  mask_DIST   = SQRT((mask_SUBS_X-(1024-1)/2.)^2 + (mask_SUBS_Y-(1024-1)/2.)^2) 
  somme    = 0.
  im       = bytarr(1024,1024)

for ii=0,420,5 do begin
  im       = im*0b
  ind      = WHERE(mask_DIST LT ii+5 AND mask_DIST GT ii,nind)
  im[ind]  = 1b
  im[512:515,512:1023]=0b
  ind      = WHERE(im)
  im       = im*0b 
  bound_ind  = GET_EXT_BOUNDARY(ind,4,1024,1024)     
;  im[bound_ind]=1b
;  bound_ind  = WHERE(im) 
  bound_ind  = M_CONNECT(bound_ind,1024,1024,/noprint)
;  im[bound_ind]=1b
;window,11,xs=1024,ys=1024
;tvscl,im        
  bound_ind  = ORDER_IND(bound_ind,1024,1024)
  area = get_fil_area(bound_ind,1024,1024,2.28369,2.28369,511.5,511.5,420,'2002-04-01T08:00:00.000')
  ;area2 = get_helarea(ind,1024,1024,511.5,511.5,2.28369,2.28369,'2002-04-01T08:00:00.000')
  print,area,ii,ii+5;,area2
  somme=somme+area
endfor
print,somme
END
