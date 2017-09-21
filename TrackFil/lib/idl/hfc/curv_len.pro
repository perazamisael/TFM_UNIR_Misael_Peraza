;+
;NAME:
;		curv_len
;
;PURPOSE:
;		Calculate the length of a curve defined by its (X,Y) coordinates.
;
;CALLING SEQUENCE:
;		IDL>Result = curv_len(X,Y)
;
;INPUTS:
;		X - Vector containing the X coordinates.
;		Y - Vector containing the Y coordinates.
;
;OPTIONAL INPUTS:
;		None.
;
;KEYWORD PARAMETERS:
;		None.
;
;OUTPUTS:
;		length - Length of the curve.
;
;OPTIONAL OUTPUTS:
;		dl - Vector of float type which contains distance between a point and the following one.
;
;RESTRICTIONS/COMMENTS:
;		X and Y input coordinates must be ordered.
;
;CALL:
;		None.
;
;EXAMPLES:
;		None.
;
;HISTORY:
;		Written by:		X.Bonnin, 17-DEC-2010.
;
;-

FUNCTION curv_len,X,Y, dl=dl     


;[1]:Initialize the program
;==========================
found = 0

if (n_params() lt 2) then begin
	message,/INFO,'Call is:'
	print,'Result = curv_len(X,Y,dl=dl)'
	return,0                 
endif

n = n_elements(X)
if (n ne n_elements(Y)) then begin
	message,/INFO,'X and Y vectors must have the number of elements!'
	return,0
endif
if (n lt 2) then begin
	message,/INFO,'X and Y vectors must have at least two elements!'
	return,0
endif
;==========================

;[2]:Calculate the length
;========================
Xs = shift(X,1)
Ys = shift(Y,1)

Vx = (X-Xs)[1:*]
Vy = (Y-Ys)[1:*]

dl = sqrt(Vx^2 + Vy^2)

length = total(dl)
;========================

return,length
END