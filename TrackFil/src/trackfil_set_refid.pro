FUNCTION trackfil_set_refid, feat_data,ske_data, $
                             rmax=rmax,lmin=lmin, $
                             threshold=threshold, $
                             param=param, $
                             output_param=output_param, $
                             error=error,SILENT=SILENT,$
                             PROGRESS=PROGRESS

;+
; NAME:
;		trackfil_set_refid
;
; PURPOSE:
;		Identify filaments from a rotation to the following,
;       then set their ref_feat indexes in consequence.
;
; CATEGORY:
;		Image processing
;
; GROUP:
;		TRACKFIL
;
; CALLING SEQUENCE:
;		IDL> ref_feat = trackfil_set_refid(feat_data,ske_data)
;
; INPUTS:
;		feat_data  - Structure containing the filaments data.
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
;		param	  - 4 elements vector containing the input parameters
;					[a0,theta0,d0,dt0] used to compute the propability P.
;
; KEYWORD PARAMETERS:
;		/SILENT    - Quiet mode.
;               /PROGRESS  - Print the progress of the computation in
;                            the terminal.
;
; OUTPUTS:
;		ref_feat - ref_feat index updated.
;
; OPTIONAL OUTPUTS:
;		output_param  - Return a structure containing the list
;					    of computed parameters
;					    (i.e., a, theta, d, dt, P, and lengths of the skeletons).
;		error - Equal to 1 if an error occurs, 0 else.
;
; COMMON BLOCKS:
;		None.
;
; SIDE EFFECTS:
;		None.
;
; RESTRICTIONS:
;		None.
;
; CALL:
;		tim2carr
;		curv_match
;
; EXAMPLE:
;		None.
;
; MODIFICATION HISTORY:
;		Written by: X.Bonnin,	31-MAR-2011.
;
;-

;[1]:Initialize the input parameters
;[1]:===============================
error = 1
if (n_params() lt 2) then begin
    message,/INFO,'Usage:'
    print,'trackfil_set_refid, feat_data, ske_data, $'
    print,'                               rmax=rmax,lmin=lmin, $'
    print,'                               threshold=threshold, $'
    print,'                               param=param, $'
    print,'                               output_param=output_param, $'
    print,'                               error=error,/SILENT,$'
    print,'                               /PROGRESS'
    return,1
endif
if (size(feat_data,/TNAME) ne 'STRUCT') then message,'FEAT_DATA input parameter must be an IDL structure!'
if (size(ske_data,/TNAME) ne 'STRUCT') then message,'SKE_DATA input parameter must be an IDL structure!'

data = feat_data
nfeat = n_elements(data)
Xdata = ske_data
ske_id = Xdata.index
ske_lon = reform(Xdata.Xcarr[0])
ske_lat = reform(Xdata.Xcarr[1])
ske_jd = Xdata.jd

tracklist = data.track_id
tracklist = tracklist[uniq(tracklist,sort(tracklist))]
ntrack = n_elements(tracklist)
ref_feat = lonarr(nfeat)

if (not keyword_set(rmax)) then rmax = 5.0d
if (not keyword_set(lmin)) then lmin = 0.0d
if (not keyword_set(threshold)) then threshold = 0.5
if (not keyword_set(param)) then param = [5.0,30,4,4]

SILENT = keyword_set(SILENT)
PROGRESS = keyword_set(PROGRESS)
;[1]:===============================

;[2]:Perform tracking from a rotation to the following
;[2]:=================================================
output_param = {id_i:0l,id_j:0l,len_i:0.0d,len_j:0.0d, $
                a:0.0d,theta:0.0d,d:0.0d,dt:0.0d,P:0.0}
output_param = replicate(output_param,nfeat*nfeat)
iter = 0l
for i=0L,ntrack-1l do begin
    iw1 = where(tracklist[i] eq data.track_id,ni1)

    if (PROGRESS) then printl,'Computation completed: '+string(100.*(i+1L)/ntrack,format='(f6.2)')+'%'

    ;If feature is too small or not enough observed, then skip it
    len_i = total(data(iw1).ske_length_deg)
    if (len_i lt lmin) or (ni1 lt 3) then continue

    id_1 = 0l & lon_1 = 0. & lat_1 = 0. & jd_1 = 0.0d
    for j=0l,ni1-1l do begin
        jw = where(data(iw1[j]).id_fil eq ske_id,nj)

        id_1 = [id_1,ske_id[jw]]
        lon_1 = [lon_1,ske_lon[jw]]
        lat_1 = [lat_1,ske_lat[jw]]
        jd_1 = [jd_1,ske_jd[jw]]
    endfor
    id_1 = id_1[1:*] & lon_1 = lon_1[1:*] & lat_1 = lat_1[1:*] & jd_1 = jd_1[1:*]
    id_1list = id_1[uniq(id_1,sort(id_1))]
    nid_1 = n_elements(id_1list)

    ;Gravity center
    gc_1 = [mean(lon_1,/NAN),mean(lat_1,/NAN)]

    ;Location on the previous rotation
    gc_1[0] = gc_1[0] + 360.0d

    ;look for filaments around this location
    r_i = sqrt((gc_1[0] - ske_lon)^2 + (gc_1[1] - ske_lat)^2)
    rmin_i = min(r_i,imin)
    if (rmin_i gt rmax) then continue
    iw2 = (where(ske_id[imin] eq data.id_fil))[0]
    iw2 = where(data(iw2).track_id eq data.track_id,ni2)

    ;If feature is too small or not enough observed, then skip it
    len_j = total(data(iw2).ske_length_deg)
    if (len_j lt lmin) or (ni2 lt 3) then continue

    id_2 = 0l & lon_2 = 0. & lat_2 = 0. & jd_2 = 0.0d
    for j=0l,ni2-1l do begin
        jw = where(data(iw2[j]).id_fil eq ske_id,nj)

        id_2 = [id_2,ske_id[jw]]
        lon_2 = [lon_2,ske_lon[jw]]
        lat_2 = [lat_2,ske_lat[jw]]
        jd_2 = [jd_2,ske_jd[jw]]
    endfor
    id_2 = id_2[1:*] & lon_2 = lon_2[1:*] & lat_2 = lat_2[1:*] & jd_2 = jd_2[1:*]
    id_2list = id_2[uniq(id_2,sort(id_2))]
    nid_2 = n_elements(id_2list)

    ;Compare filaments
    P = fltarr(nid_1,nid_2) & outpar = fltarr(4,nid_1,nid_2)
    for i1=0l,nid_1-1l do begin
        where_i1 = where(id_1 eq id_1list[i1])
        Xi1 = transpose([[lon_1[where_i1]],[lat_1[where_i1]]])

        for i2=0l,nid_2-1l do begin
            where_i2 = where(id_2 eq id_2list[i2])
            Xi2 = transpose([[lon_2[where_i2]],[lat_2[where_i2]]])

            		;Compute Curve matching algorithm
            delta = curv_match(Xi1,Xi2,theta=theta,a=a)
            deltar = curv_match(Xi1,reverse(Xi2,2),theta=thetar,a=ar)

            md = [delta,deltar]
            ma = [reform(sqrt(a[0,*]^2 + a[1,*]^2)),reform(sqrt(ar[0,*]^2 + ar[1,*]^2))]
            mth = [abs(theta)*!radeg,abs(thetar)*!radeg]

			;Calculate the probabilities associated with the curve matching parameters a, theta, and d
            Pa = (0.5*((param[0] - ma)/param[0]) + 1.)>(0.)<(1.)
            Pth = (0.5*(2. - (mth/param[1])))>(0.)<(1.)
            Pd = (0.5*((param[2] - md)/param[2]) + 1.)>(0.)<(1.)

			;Calculate the additional time probability
            mdt = abs(jd_1[i1] - jd_2[i2])
            Pt =  (0.5*(2. - (mdt/param[3])))>(0.)<(1.)

			;Calculate the probability that both skeletons come from the same co-rotating filament
            P[i1,i2] = max(Pt*(Pa + Pth + Pd)/3.,imax)

            outpar[*,i1,i2] = [ma[imax],md[imax],mth[imax],mdt]
        endfor
    endfor
    P = mean(P)
    if (P ge threshold) then ref_feat[iw1] = data(iw2[0]).track_id

    output_param(iter).a = mean(outpar[0,*,*])
    output_param(iter).d = mean(outpar[1,*,*])
    output_param(iter).theta = mean(outpar[2,*,*])
    output_param(iter).dt = mean(outpar[3,*,*])
    output_param(iter).P = P
    output_param(iter).len_i = len_i
    output_param(iter).len_j = len_j
    output_param(iter).id_i = tracklist[i]
    output_param(iter).id_j = data(iw2[0]).track_id
    iter++
endfor
;[2]:=================================================

if (iter gt 0l) then output_param = output_param[0l:iter-1l]


error=0
return,ref_feat
END
