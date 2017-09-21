FUNCTION trackfil_read_config,config_file,$
				config_dir=config_dir,$
				error=error,$
				SILENT=SILENT

;+
; NAME:
;		trackfil_read_config
;
; PURPOSE:
; 		Read input parameters written in the
;		given ascii format file.
;
; CATEGORY:
;		I/O
;
; GROUP:
;		TRACKFIL
;
; CALLING SEQUENCE:
;		IDL> config_struct = trackfile_read_config(config_file)
;
; INPUTS:
;		config_file - Name of the configuration file containing the inputs.
;
; OPTIONAL INPUTS:
;		config_dir - Path to the directory containing the config file.
;
; KEYWORD PARAMETERS:
;		/SILENT	- Quiet mode.
;
; OUTPUTS:
;		config_struct - Structure containing inputs code parameters:
;							.INSTITUTE
;							.CODE
;							.FEATURE_NAME
;							.CONTACT
;							.REFERENCE
;							.OBSERVAT
;							.INSTRUME
;							.TELESCOP
;							.
;
;
; OPTIONAL OUTPUTS:
;		error - Equal to 1 if an error occurs, 0 else.
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
;		Written by:		X.Bonnin, 26-JUL-2011.
;
;-

;On_error,2
error = 1
if (n_params() lt 1) then begin
	message,/INFO,'Call is:'
	print,'config_struct = trackfil_read_config(config_file, $'
	print,'                                  config_dir=config_dir, $'
	print,'                                  error=error, $'
	print,'               				     /SILENT)'
	return,0
endif

SILENT = keyword_set(SILENT)

file = strtrim(config_file[0],2)
if (keyword_set(config_dir)) then file = strtrim(config_dir[0],2) + path_sep() + file_basename(file)

if (~file_test(file)) then begin
	message,/CONT,'No configuration file found!'
	return,0
endif

config_struct = {trackfil_config}
ntags = n_tags(config_struct)

nlines  = file_lines(file)
if (nlines ne ntags) then begin
	message,/CONT,'Incompatible dimensions for input parameters!'
	return,0
endif


tags = strarr(nlines)
data = strarr(nlines)
openr,lun,file,/GET_LUN
for i=0,nlines-1 do begin
	data_i = ""
	readf,lun,data_i
	data_i = strtrim(strsplit(data_i,'=',/EXTRACT),2)
	tags[i] = strlowcase(data_i[0])
	data[i] = data_i[1]
	jflag = execute('config_struct.'+tags[i]+'=+data[i]')
endfor
close,lun
free_lun,lun

error = 0
return,config_struct
END
