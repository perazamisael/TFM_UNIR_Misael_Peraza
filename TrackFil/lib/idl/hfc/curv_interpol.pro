;+
; NAME:
;		curv_interpol
;
; PURPOSE:
; 		Interpolate a curve along 
;		its curvilinear direction 
;		with a constant spatial scale.
;
; CATEGORY:
;		Geometry		
;
; GROUP:
;		
;
; CALLING SEQUENCE:
;		IDL>Results = curv_interpol(X,Y,ds)
;
; INPUTS:
;		X   - Vector containing the X axis points of the curve.
;		Y   - Vector containing the Y axis points of the curve.
;		ds  - Scalar of double type containing the spatial scale.
;			  (i.e., the distance between two following points along the interpolated curve).
;	
; OPTIONAL INPUTS:
;		None.
;
; KEYWORD PARAMETERS:
;		None.
;
; OUTPUTS:
;		Returns the n ordered points of the interpolated curve in a [2,n] array.
;		
; OPTIONAL OUTPUTS:
;		s     - Vector containing the length of the subcurve [0,i] at each interpolated data points i.
;		error - Equal to 1 if an error occurs, 0 else.		
;
; COMMON BLOCKS:		
;		None.
;	
; SIDE EFFECTS:
;		None.
;		
; RESTRICTIONS/COMMENTS:
;		The coordinates X,Y of the input curve must be ordered.
;		(going from an end to the other).
;		
; CALL:
;		intersect_linsph
;
; EXAMPLE:
;		None.		
;
; MODIFICATION HISTORY:
;		Written by X.Bonnin, 20-DEC-2010.
;									
;-


FUNCTION curv_interpol,X,Y,ds,s=s,error=error

error = 1
if (n_params() lt 3) then begin
	message,/INFO,'Call is:'
	print,'IDL>Results = curv_interpol(X,Y,ds,s=s,error=error)'
	return,0.
endif

Xin = reform(X) & Yin = reform(Y)
n = n_elements(Xin)
len = double(ds[0])

s = 0. & i=0L
Xout = Xin[i] & Yout = Yin[i] 
Xj = Xin[0] & Yj = Yin[0]
while (1) do begin
	Xi = Xin[i:*] & Yi = Yin[i:*]
	
	;Define the two points of segment that intercepts the circle
	k = 0L 
	while (1) do begin
		if (i+k gt n-1L) then break
		r = norm([Xj,Yj]-[Xi[k],Yi[k]])
		if (r ge len) then break
		k = k + 1L
	endwhile
	if (i+k+1 gt n-1L) then begin
		Xout = [Xout,Xin[n-1L]]
		Yout = [Yout,Yin[n-1L]]
		break
	endif
	if (k eq 0) then Vk0 = [Xj,Yj] else Vk0 = [Xi[k-1L],Yi[k-1L]]
	Vk1 = [Xi[k],Yi[k]]

	;Calculation of the intersection
	Xj = intersect_linsph([Vk0,0.],[Vk1,0.],[Xj,Yj,0.],len,hit=hit,/SEGMENT)
	
	if (hit eq 0) then begin
		message,/INFO,'No intersection!'
		break
	endif
	Yj = Xj[1]
	Xj = Xj[0]
	
	s = [s,max(s) + norm([Xi[0],Yi[0]] - [Xj,Yj])]
	
	Xout = [Xout,Xj]
	Yout = [Yout,Yj]
	i = i+k
endwhile

error = 0
return,transpose([[Xout],[Yout]])
END