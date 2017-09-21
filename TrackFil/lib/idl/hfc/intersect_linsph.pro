;+
; NAME:
;		intersect_linsph
;
; PURPOSE:
; 		Compute the intersection(s) between a line and a sphere
;
; CATEGORY:
;		Geometry		
;
; GROUP:
;		
;
; CALLING SEQUENCE:
;		IDL>Results = intersect_linsph(V1,V2,V3,Radius)
;
; INPUTS:
;		V1   - 3-elements vector containing the coordinates [X,Y,Z] of one point of the line. 
;		V2   - 3-elements vector containing the coordinates [X,Y,Z] of one point of the line (must different of X1).
;		V3   - 3-elements vector containing the coordinates [X,Y,Z] of the sphere centre.
;		R    - a scalar containing the radius of the sphere.
;	
; OPTIONAL INPUTS:
;		None.
;
; KEYWORD PARAMETERS:
;		/SEGMENT - If set, then compute intersection between the sphere and the segment defined by V1 and V2.
;		/DOUBLE  - If set, then the computation is done in double floatting precision.
;
; OUTPUTS:
;		Returns the intersections in a [n,3] array (n=1 or 2).
;		
; OPTIONAL OUTPUTS:
;		hit - Equal to 1 if the line intersects the sphere once, to 2 if the line intersects the sphere twice, 0 else.		
;
; COMMON BLOCKS:		
;		None.
;	
; SIDE EFFECTS:
;		None.
;		
; RESTRICTIONS/COMMENTS:
;		None.
;		
; CALL:
;		None.
;
; EXAMPLE:
;		None.		
;
; MODIFICATION HISTORY:
;		Written by X.Bonnin, 20-DEC-2010.
;									
;-

FUNCTION intersect_linsph,V1,V2,V3,Radius,hit=hit,$
		                  SEGMENT=SEGMENT,DOUBLE=DOUBLE


hit = 0
if (n_params() lt 4) then begin
	message,/INFO,'Call is:'
	print,'Results = intersect_linsph(V1,V2,V3,Radius,hit=hit,/DOUBLE)'
	return,0.
endif

DOUBLE = keyword_set(DOUBLE)
SEGMENT = keyword_set(SEGMENT)

n1 = n_elements(V1)
n2 = n_elements(V2)
n3 = n_elements(V3)
if (n1 ne 3) then message,'Input argument X1 must have 3 elements!'
if (n2 ne 3) then message,'Input argument X2 must have 3 elements!'
if (n3 ne 3) then message,'Input argument X3 must have 3 elements!'

X1 = V1
X2 = V2
X3 = V3
R = Radius

;equation of the form a*u^2 + b*u + c = 0
a = total((X2-X1)^2)
b = 2.d*total((X2-X1)*(X1-X3))
c = total((X3-X1)^2) - R^2

delta = b*b - 4.d*a*c

if (delta lt 0.) then return,0.
if (delta gt 0.) then begin
	u1 = -b + sqrt(delta)
	u2 = -b - sqrt(delta)
	u = [u1,u2]/(2.d*a)
endif else u = -b/(2.d*a)
hit = n_elements(u)

Xout = dblarr(hit,3)
for i=0,hit-1 do Xout[i,*] = X1 + u[i]*(X2-X1)

if (SEGMENT) then begin
	X = reform(Xout[*,0]) & Y = reform(Xout[*,1]) & Z = reform(Xout[*,2])
	Xmin = min([X1[0],X2[0]],max=Xmax)
	Ymin = min([X1[1],X2[1]],max=Ymax)
	Zmin = min([X1[2],X2[2]],max=Zmax)
	w = where((X - Xmin) ge 0. and (Xmax - X) ge 0. and $
			  (Y - Ymin) ge 0. and (Ymax - Y) ge 0. and $
			  (Z - Zmin) ge 0. and (Zmax - Z) ge 0.,hit)
	if (w[0] eq -1) then return,0
	Xout = reform(Xout[w,*])
endif

if (~DOUBLE) then Xout = float(Xout)

return,Xout
END