PRO hfc_write_csv, struct, output_file, $
               	   separator=separator,error=error,$
		   APPEND=APPEND,NOHEADER=NOHEADER, $
		   OVERWRITE=OVERWRITE


;+
; NAME:
;	hfc_write_csv
;
; PURPOSE:
;   	Write values provided in an input structure 
;	into a csv format file.
;
; CATEGORY:
;	I/O
;
; GROUP:
;	None.
;
; CALLING SEQUENCE:
;	IDL>hfc_write_csv, struct, output_file
;
; INPUTS:
;       struct      - Structure containing the values to write.
;       output_file - Path and name of the output csv format file.    
;
; OPTIONAL INPUTS:
;       separator - Specify the separator character between fields.
;                   Default is ";"	    
;
; KEYWORD PARAMETERS:
;       /NOHEADER  - Do not write header on the first line. 
;       /APPEND    - Equivalent to /APPEND keyword for openw procedure.
;	/OVERWRITE - Overwrite existing file.
;
; OUTPUTS:
;	None.				
;
; OPTIONAL OUTPUTS:
;	error - Scalar equal to 1 if an error occurs, 0 otherwise.		
;		
; COMMON BLOCKS:
;	None.		
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS/COMMENTS:
;	None.
;
; CALL:
;	struct2csv
;
; EXAMPLE:
;	None.
;		
; MODIFICATION HISTORY:
;	Written by:		X.Bonnin (LESIA, CNRS).
;
;-

error = 1
if (n_params() lt 2) then begin
    message,/INFO,'Call is:'
    	print,'hfc_write_csv, struct, output_file, $'
	print,'               separator=separator, error=error, $'
	print,'               /APPEND,/NOHEADER,/OVERWRITE'
    return
endif
NOHEADER=keyword_set(NOHEADER)
APPEND = keyword_set(APPEND)
OVERWRITE = keyword_set(OVERWRITE)
if not (keyword_set(separator)) then sep = ';' else sep = strtrim(separator[0],2)

outfile = strtrim(output_file[0],2)
if not (file_test(file_dirname(outfile),/DIR)) then begin
    message,/CONT,'Output directory does not exist!'
    return
endif
if not (file_test(outfile)) then APPEND = 0

if (file_test(outfile)) and not (OVERWRITE) then begin
	message,/INFO,outfile+' already exists!'
	error=0
	return
endif

fields = struct2csv(struct,header=header,separator=sep)

openw, lun, outfile , /get_lun, APPEND=APPEND
if not (APPEND) and not (NOHEADER) then printf,lun,header
for i=0l,n_elements(fields)-1l do printf,lun,fields[i]
close,lun
free_lun,lun

error = 0
END
