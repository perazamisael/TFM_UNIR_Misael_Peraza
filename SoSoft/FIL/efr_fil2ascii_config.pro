;------------------------------------------------------------------------------------------------------
;         CONFIGURATION EFR_FIL2ASCII
;------------------------------------------------------------------------------------------------------

;----------------------------------------------------------
;OPTIONAL DISPLAY (0 or 1)
;----------------------------------------------------------
;follow the different steps on screen

DISPLAY_res = 1

;----------------------------------------------------------
;JPEG OPTIONAL OUTPUT (0 or 1)
;---------------------------------------------------------- 
;summarizes the steps in one jpeg for visual inspection

JPEG_output = 1

;----------------------------------------------------------
; CHOOSE THE FILES (1) OR NOT (0) 
;----------------------------------------------------------
;use dialog_pickfile (1) or use a default directory where the fits
;files are stored (0, directory defined below)

DIALOG = 1

;-------------------------------------------------
; WRITE THE RESULTS IN ASCII FILES 
;-------------------------------------------------
; yes (1), no (0)
  
ASCII = 1

;-------------------------------------------------------------------------
; DIRECTORIES
; ppath: where the program looks for the preprocessed fits files
; apath: path to the directory where the ascii files will be written
; jpath: path to the directory where jpeg files will be written
; opath: path to the original file (before preprocessing, this 
; is optional / will be written in the ascii file)
;--------------------------------------------------------------------------
  ppath = '/home/fuller/data2/FITS/Ha/PROCESSED/'
  apath = '/home/fuller/poub/SAV/FIL/DAILY_ASCII/'
  jpath = '/home/fuller/poub/SAV/FIL/TMPJPG/'
  opath = '/home/fuller/data2/FITS/' 

