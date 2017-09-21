FUNCTION hfc_hel2car, Xhel,date_obs, $
					   dec0=dec0,DC=DC,DRC=DRC

;+
; NAME:
;		hfc_hel2car
;
; PURPOSE:
; 		Converts the heliographic coordinates to carrington ones.
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		FILTRACK
;
; CALLING SEQUENCE:
;		IDL> Result = hfc_hel2car(Xhel,date_obs)
;
; INPUTS:
;		Xhel		- [2,n] array containing the heliographic coordinates (lon,lat) in degrees of n points.
;       date_obs    - observation date.
;
; OPTIONAL INPUTS:
;		dec0	 - fixes the central meridian Carrington long. value at origin to dec0 (in decimal Carrington rotation longitude). 
;
;				   
;
; KEYWORD PARAMETERS:
;		DC	- Returns Decimal Carrington rotation longitude 
;			  instead of longitude (in degrees) ones.
;		DRC	- Computes the Differential Rotation Corrected 
;			  Carrington coordinates using the 
;			  Ulrich and Boyden, Solar Physics, 2006 model.
;			  
; OUTPUTS:
;		Xcar - [2,n] array containing the Carrington coordinates.
;
; OPTIONAL OUTPUTS:
;		None.
;
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; CALL:
;		TIM2CARR
;
; EXAMPLE:
;		None.		
;
; MODIFICATION HISTORY:
;		Written by:		X.Bonnin 
;
;-

if (n_params() lt 2) then begin
	message,/INFO,'Call is:'
	print,'Results = hfc_hel2car(Xhel, date_obs,$'
	print,'                      dec0=dec0,/DC,/DRC)'
	return,[!values.f_nan,!values.f_nan]
endif

DC = keyword_set(DC)
DRC = keyword_set(DRC)

date = strtrim(date_obs,2)
hel_coord = reform(double(Xhel))

;  Calculate Decimal Carrington rotation longitude of the central meridian at the observation time
DCM = TIM2CARR(date,/DC) 
DCM0 = long(DCM) ;Carrington rotation number 
if (~n_elements(dec0)) then dc0 = DCM0 else dc0 = dec0
if (n_elements(dc0) eq 1) then begin
	DCM = DCM[0]
	DCM0 = DCM0[0]
	dc0 = dc0[0]
endif

;Carrington longitude of the central meridian (in degrees) at the observation time
l0 = 360.0D*(dc0 + 1.0d - DCM)  	

;  Calculate the Carrington coords (decimal Carrington rotation longitude)
dc_coord = hel_coord
dc_coord[0,*] = DCM - (hel_coord[0,*]/360.0d) 

;longitude in degree (origin 0° corresponds to dc0)
car_coord = dc_coord
car_coord[0,*]  = 360.0d*(dc0 + 1.0d - car_coord[0,*]) 


;Add differential rotation correction 
;using Ulrich and Boyden, SolPhy, 2006 model
if (DRC) then begin
	B = car_coord[1,*]/!radeg ;latitude in rad
	Vdif = 2.730d - 0.4100d*(sin(B)^2 + 1.0216d*sin(B)^4) ;Differential rotation speed in microrad/sec
	Vcar = 2.66622375d ;Carrington rotation speed in microrad/sec
	
	dl = (car_coord[0,*] - l0)
	
	dc_coord[0,*] = DCM - dl*Vcar/(360.0d*Vdif)
	car_coord[0,*]  = 360.0d*(dc0 + 1.0d - dc_coord[0,*])
endif 

;print,date_obs[0],hel_coord[0,0],dc_coord[0,0],car_coord[0,0],dc0

if (DC) then return,dc_coord else return,car_coord
;-------------------------------------------------------------------------------
END
