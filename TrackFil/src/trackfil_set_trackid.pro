FUNCTION trackfil_set_trackid, feat_data, ske_data, $
                               lmin=lmin,threshold=threshold, $
                               rmax=rmax,param=param, $
                               output_param=output_param, $
                               lvl_trust=lvl_trust,error=error, $
                               CURVE_ONLY=CURVE_ONLY, $
                               CLOSE_ONLY=CLOSE_ONLY, $
                               SILENT=SILENT, $
                               PROGRESS=PROGRESS

;+
; NAME:
;		trackfil_set_trackid
;
; PURPOSE:
;		Tracks the filaments over 1 solar disk crossing
;	    (if a skeleton length is greater or equal than length_min, perform the curve matching algorithm,
;       otherwise search for possible close features on a given area),
;       then set track_id indexes of track_stc structure in consequence.
;
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		TRACKFIL
;
; CALLING SEQUENCE:
;		IDL> track_id = trackfil_set_trackid(feat_data,ske_data)
;
; INPUTS:
;		feat_data  - Structure containing input feature data.
;		ske_data   - Structure containing the skeleton carrington coordinates computed using the chain code.
;
; OPTIONAL INPUTS:
;		rmax      - Maximum distance (in degrees) between two points of
;			        two distinct skeletons above which the tracking algorithm
;			        is not applied.
;					Default is 5 degrees.
;		lmin      - Minimum length of the skeleton above which the curve matching
;			        algorithm is applied.
;					Default is 5 degrees.
;		threshold - Probability (between 0 and 1) above which
;					the two skeletons are assumed to be the same.
;		param	  - 3 elements vector containing the input parameters
;					[a0,theta0,d0] used to compute the propability P.
;
; KEYWORD PARAMETERS:
;		/CURVE_ONLY - Perform only curve matching algorithm.
;		/CLOSE_ONLY - Perform only close feature algorithm.
;		/SILENT     - Quiet mode.
;             	/PROGRESS    - Print the progress of the
;                         		    computation in the prompt.
;
; OUTPUTS:
;		track_id   - List of tracking index numbers computed.
;
; OPTIONAL OUTPUTS:
;		lvl_trust     - Confidence level for each tracking.
;		output_param  - Return a structure containing the list
;					    of computed parameters
;					    (i.e., a, theta, d, P, and lengths of the skeletons).
;		error         - Equal to 1 if an error occurs, 0 else.
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
;		tim2carr
;		curv_len
;		curv_match
;
; EXAMPLE:
;		None.
;
; MODIFICATION HISTORY:
;		Written by: X.Bonnin,	02-MAR-2011.
;
;-

;[1]:Initialize the input parameters
;[1]:===============================
error = 1
if (n_params() lt 2) then begin
	message,/INFO,'Usage:'
	print,'trackfil_set_trackid, feat_data, ske_data, $'
             print,'                                   lmin=lmin,threshold=threshold, $'
             print,'                                   rmax=rmax,param=param, $'
             print,'                                   output_param=output_param, $'
             print,'                                   lvl_trust=lvl_trust,error=error, $'
             print,'                                   /CURVE_ONLY, $'
             print,'                                   /CLOSE_ONLY, $'
             print,'                                   /SILENT, / PROGRESS$'
             return,1
endif
if (size(feat_data,/TNAME) ne 'STRUCT') then message,'FEAT_DATA input parameter must be a structure!'
if (size(ske_data,/TNAME) ne 'STRUCT') then message,'SKE_DATA input parameter must be a structure!'

CURVE_ONLY = keyword_set(CURVE_ONLY)
CLOSE_ONLY = keyword_set(CLOSE_ONLY)
SILENT = keyword_set(SILENT)
PROGRESS = keyword_set(PROGRESS)

if (not CURVE_ONLY) and (not CLOSE_ONLY) then begin
	CURVE_ONLY = 1
	CLOSE_ONLY = 1
endif

data = feat_data
nfeat = n_elements(data)

if (not keyword_set(rmax)) then rmax = 5.0d
if (not keyword_set(lmin)) then lmin = 0.0d
if (not keyword_set(threshold)) then threshold = 0.5
if (not keyword_set(param)) then param = [5.0,30,4]
;[1]:===============================

;[2]:Initialize data
;[2]:===============

id_fil = data.id_fil
date_obs = data.date_obs
jd_obs = data.jdint + data.jdfrac
lnth = data.ske_length_deg
lvl_trust = fltarr(nfeat) - 1.0

id_ske = ske_data.index
Xske = ske_data.Xcarr
;[2]:=================

;[4]:Perform the curve matching algorithm
;[4]:====================================
output_param = {id_i:0l,id_j:0l,len_i:0.0d,len_j:0.0d, $
				a:0.0d,theta:0.0d,d:0.0d,r:0.0d,P:0.0}
output_param = replicate(output_param,nfeat*nfeat)
count = fltarr(nfeat) & iter=0l
;Loops on filaments
for i=0L,nfeat-1L do begin

	if (PROGRESS) then printl,'Computation completed: '+string(100.*(i+1L)/nfeat,format='(f6.2)')+'%'

	if not (CLOSE_ONLY) and (lnth[i] lt lmin) then continue

	;Get skeleton coordinates of the current filament
	where_id = where(id_fil[i] eq id_ske,ni,complement=where_not_id,ncomplement=no)
	if (where_id[0] eq -1) then continue
	Xi = reform(Xske[*,where_id])

	;Get skeleton coordinates of the others filaments
	Xo = reform(Xske[*,where_not_id])

	;Get id of the others filaments
	ido = id_ske[where_not_id]

	;Identify closer filaments
	close_id = 0L
	for j=0L,ni-1L do begin
		rj = sqrt((Xi[0,j]-Xo[0,*])^2 + (Xi[1,j]-Xo[1,*])^2)
		where_in = where(rj le rmax)
		if (where_in[0] eq -1) then continue
		close_id = [close_id,ido[where_in]]				;Almacena las distancias menores que rmax
	endfor
	if (n_elements(close_id) eq 1) then begin
		;print,'There is no filament in the neighborhood!'
		continue
	endif
	close_id = close_id[1:*]

	;list of close skeletons (order by id)
	close_id = close_id[uniq(close_id,sort(close_id))]
	nclose = n_elements(close_id)

	;loops to compare filament i with others ones
	for j=0L,nclose-1L do begin

		;if the close skeleton is not in the time range then skip comparison
		where_j = (where(close_id[j] eq id_fil))[0]
		if (where_j eq -1) then continue
		;if the comparison has been already done then skip it
		if ((where(id_fil[where_j] eq id_fil[0:i]))[0] ne -1) then continue

		if (not CLOSE_ONLY) and (lnth[where_j] lt lmin) then continue

        ;Get skeleton coordinates of close filament j
		where_id_j = where(close_id[j] eq ido,nj)
		Xj = reform(Xo[*,where_id_j])

		if (lnth[i] ge lmin) and (lnth[where_j] ge lmin) and (CURVE_ONLY) then begin

			;Compute Curve matching algorithm
			delta = curv_match(Xi,Xj,theta=theta,a=a)
			deltar = curv_match(Xi,reverse(Xj,2),theta=thetar,a=ar)

			md = [delta,deltar]
			ma = [reform(sqrt(a[0,*]^2 + a[1,*]^2)),reform(sqrt(ar[0,*]^2 + ar[1,*]^2))]
			mth = [abs(theta)*!radeg,abs(thetar)*!radeg]

			;Calculate the probabilities associated with the curve matching parameters a, theta, and d
			Pa = (0.5*(((param[0] - ma)/param[0]) + 1.))>(0.)<(1.)
			Pth = (0.5*(2. - (mth/param[1])))>(0.)<(1.)
			Pd = (0.5*(((param[2] - md)/param[2]) + 1.))>(0.)<(1.)

			;Calculate the probability that both skeletons come from the same co-rotating filament
			P = (Pa + Pth + Pd)/3.
			P = max(P,imax)
			
			if (P ge threshold) then begin
				i1 = i
				i2 = where_j

				j1 = where(data(i1).track_id eq data.track_id)
				j2 = where(data(i2).track_id eq data.track_id)
				
				;print,"=========================="
				;print,"Todos los track_id",data.track_id
				;print,"indexes:",i1,i2,"posiciones i1",j1,"posiciones i2",j2
				
				data([j1,j2]).track_id = min(data([j1,j2]).track_id)				
			
				
				lvl_trust(i1) = lvl_trust(i1) + P
				lvl_trust(i2) = lvl_trust(i2) + P
				count(i1) = count(i1) + 1. & count(i2) = count(i2) + 1.
			endif
			output_param(iter).a = ma[imax]
			output_param(iter).d = md[imax]
			output_param(iter).theta = mth[imax]
		endif else begin
			if (not CLOSE_ONLY) then continue
            Xi_gc = [mean(Xi[0,*]),mean(Xi[1,*])]
            Xj_gc = [mean(Xj[0,*]),mean(Xj[1,*])]

			;Get contour pixels coordinates of filament i
			ri = sqrt((Xi_gc[0] - Xj_gc[0])^2 + $
					  (Xi_gc[1] - Xj_gc[1])^2)

			P = (0.5*(2. - (ri/rmax)))>(0.)<(1.)

			if (P ge threshold) then begin
				if (lnth[i] ge lnth[where_j]) then begin
					data(where_j).track_id = data(i).track_id
					
				endif else begin
					data(i).track_id = data(where_j).track_id
					
				endelse
				
				lvl_trust(i) = lvl_trust(i) + P
				lvl_trust(where_j) = lvl_trust(where_j) + P
				count(i) = count(i) + 1. & count(where_j) = count(where_j) + 1.
			endif
			output_param(iter).r = ri
		endelse
		output_param(iter).id_i = id_fil[i]
		output_param(iter).id_j = id_fil[where_j]
		output_param(iter).P = P
		output_param(iter).len_i = lnth[i]
		output_param(iter).len_j = lnth[where_j]
		iter++
	endfor
endfor
;[4]:====================================

;[5]:Compute lvl of trust
;[5]:====================
ii = where(count gt 0.)
if (ii[0] ne -1) then lvl_trust(ii) = lvl_trust(ii)/count(ii)
lvl_trust = fix(lvl_trust*100.)>(0)<(100)
;[5]:====================

if (iter gt 0l) then output_param = output_param[0:iter-1l]

error = 0
return,data.track_id
END
