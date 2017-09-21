PRO trackfil, fil_data, results, $
                    config_file=config_file, SILENT=SILENT

;+
; NAME:
;       trackfil
;
; PURPOSE:
;        Trackfil main program
;
; CATEGORY:
;       Image processing
;
; GROUP:
;       TRACKFIL
;
; CALLING SEQUENCE:
;       IDL> trackfil, fil_data, results, config_file=config_file
;
; INPUTS:
;       fil_data  - Structure containing the filaments data to be updated with tracking information.
;
; OPTIONAL INPUTS:
;       config_file - Trackfil configuration file containing input parameters to be loaded.
;                             If not provided, search for a $TRACKFIL_HOME_DIR/config/trackfil_default.config file.
;
; KEYWORD PARAMETERS:
;       SILENT - Quiet mode.
;
; OUTPUTS:
;       Returns the fil_data structure with tracking information (i.e. lvl_trust, feat_id) updated in the results output.
;
; OPTIONAL OUTPUTS:
;       error - Equal to 1 if an error occurs, 0 else.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       Calls the Helio Software library.
;
; CALL:
;
;
; EXAMPLE:
;       None.
;
; MODIFICATION HISTORY:
;       Written by: X.Bonnin,   05-MAY-2011.
;
;-

sep = path_sep()

if n_params() lt 1 then begin
    message,/INFO,'Usage:'
    print,'Results = trackfil(fil_data, config_file=config_file, /SILENT)'
    return
endif
SILENT = keyword_set(SILENT)

if not keyword_set(config_file) then begin
    home_dir = getenv("TRACKFIL_HOME_DIR")
    if home_dir ne '' then config_file = home_dir + sep + 'config' + sep + 'trackfil_default.config'
endif

if not file_test(config_file) then message,'ERROR: Config. file not found! ('+config_file+')'
conf = trackfil_read_config(config_file)

trackfil_load_sysvar

results = fil_data

print,'Calculating filaments coordinates...'
trackfil_pix2mcar,results,ske_stc,ds=conf.ds,error=error,SILENT=SILENT
if (error) then message,'Calculating filaments coordinates...error'
print,'Calculating filaments coordinates...done'

print,'Tracking over one solar disc crossing...'
inp_param = [conf.a0,conf.theta0,conf.d0]
track_id = trackfil_set_trackid(results,ske_stc,$
                                rmax=conf.rmax, $
                                lmin=conf.lmin, $
                                threshold=conf.threshold, $
                                param=inp_param, $
                                lvl_trust=lvl_trust, $
                                error=error,$
                                SILENT=SILENT)
where_le0 = (where(track_id le 0l))[0]
; if (DEBUG) then stop
if (error) or (where_le0 ne -1) then message, 'Over one solar disc crossing...error'
results.track_id = track_id
results.track_lvl_trust = lvl_trust
print,'Tracking over one solar disc crossing...done'

;Determine the filaments behaviour
print,'Determining behaviours...'
phenom = trackfil_set_phenom(results, $
                             lmin=conf.lmin, $
                             hg_long_lim=conf.hg_long_lim, $
                             error=error,SILENT=SILENT)
if (error) then message,'ERROR: Can not set phenom field!'
results.phenom = phenom
print,'Determining behaviours...done'

print,'Identifying filaments from a rotation to the following...'
inp_param = [conf.a0,conf.theta0,conf.d0,conf.dt0]
ref_id = trackfil_set_refid(results,ske_stc, $
                            rmax=conf.rmax, $
                            lmin=conf.lmin, $
                            threshold=conf.threshold, $
                            param=inp_param, $
                            error=error, $
                            SILENT=SILENT)
if (error) then message,'ERROR:Can not set ref_feat values!'
results.ref_feat = ref_id
print,'Identifying filaments from a rotation to the following...done'

END
