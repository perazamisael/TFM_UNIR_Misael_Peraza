;+
; NAME:
;		curv_match
;
; PURPOSE:
;		Apply matching algorithm on two 2d curves.
;		(See Wolfson, "On Curve Matching", IEEE, 1990).
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		None.
;
; CALLING SEQUENCE:
;		IDL> Results = curv_match(X1,X2)
;
; INPUTS:
;		X1 - [2,n1] array containing the coordinates (X,Y) of the n1 points of the first curve.
;		X2 - [2,n2] array containing the coordinates (X,Y) of the n2 points of the second curve.
;
; OPTIONAL INPUTS:
;		None.
;
; KEYWORD PARAMETERS:
;		none.
;
; OUTPUTS:
;		delta - Vector containing the minimum distance delta (in term of least-squares sens), which gives the best matching.
;				delta is n elements vector, where n = abs(n1-n2) + 1.
;				If n1 and n2 are not equal (i.e., the length of the curves is different), the algorithm is applied between
;				the smallest curve and the n sub-segments of the longest which have the same length (i.e., same number of points).
;
; OPTIONAL OUTPUTS:
;		theta - Angle (in radian) to apply between the two curves to minimize delta.
;		a	  - Translation array (2,n) to apply between the two curves to minimize delta.
;		error - Equal to 1 if an error occurs, 0 else.
;
; COMMON BLOCKS:
;		None.
;
; SIDE EFFECTS:
;		None.
;
; RESTRICTIONS/COMMENTS:
;		The two curves must have the same number of points by arclength unit. 
;		(i.e., the curvilinear distance ds between two following points on the curves must be the same.) 
;
; CALL:
;		None.
;
; EXAMPLE:
;		None.		
;
; MODIFICATION HISTORY:
;		Written by: X.Bonnin,	31-MAR-2011.
;
;-

FUNCTION curv_match,X1,X2, $	
                    theta=theta,a=a, $
                    error=error


;[1]:Initialize input parameters
;[1]:===========================
error = 1
if (n_params() lt 2) then begin
	message,/INFO,'Incorrect number of input argument!'
	print,'Call is: Results = curv_match(X1,X2,theta=theta,a=a,error=error)'
	return,0
endif

C1 = reform(X1)
C2 = reform(X2)

s1 = size(C1)
if (s1[0] ne 2) or (s1[1] ne 2) then begin
	message,/INFO,'X1 input argument must be a [2,n] elements array!'
	return,0
endif
s2 = size(C2)
if (s2[0] ne 2) or (s2[1] ne 2) then begin
	message,/INFO,'X2 input argument must be a [2,n] elements array!'
	return,0
endif

n1 = s1[2]
n2 = s2[2]
;[1]:===========================


;[2]:Perform curve matching 
;[2]:======================
if (n1 ge n2) then begin
	ns = n2
	Cl = C1
	Cs = C2
endif else begin
	ns = n1
	Cl = C2
	Cs = C1
endelse

;Translate smallest curve to 0
Cs_c = total(Cs,2)/ns
Cs[0,*] = Cs[0,*] - Cs_c[0]
Cs[1,*] = Cs[1,*] - Cs_c[1]
;Compute corresponding complexe 
U = complex(Cs[0,*],Cs[1,*])

n = abs(n1 - n2) + 1L
a = fltarr(2,n) & theta = fltarr(n) & delta = fltarr(n)
for i=0L,n-1L do begin

	is = i + lindgen(ns)
	Cl_i = Cl[*,is]
	Vi = complex(Cl_i[0,*],Cl_i[1,*])
	Vi = conj(Vi)

	uv = total(U*Vi)

	theta[i] = -atan(imaginary(uv)/real_part(uv))			
	a[*,i] = total(Cl_i,2)/float(ns)
	delta[i] = sqrt(total(Cl_i[0,*]^2 + Cl_i[1,*]^2) - float(ns)*total(a[*,i]^2) + total(Cs[0,*]^2 + Cs[1,*]^2) $
			- 2.*abs(uv))
	a[*,i] = a[*,i] - Cs_c		
endfor
;[2]:======================

error = 0
return,delta
END
