PRO ROTATION2STRUCT,nr

;From the carrington rotation number (nr), saves a
;structure file describing the filaments
;over the whole rotation.
    

;-----------------------------------------------------------
;First calculate the dates where filaments for this rotation
;can be seen.

  nr = FIX(nr)

  dat1 = CARR2EX(nr,OFFSET=90)
  dat2 = CARR2EX(nr+1,OFFSET=-90)

  jdat1 = JULDAY(dat1(5),dat1(4),dat1(6),00,00,00)
  jdat2 = JULDAY(dat2(5),dat2(4),dat2(6),00,00,00)

  nbj  = FIX(jdat2) - FIX(jdat1) + 1
  tabj = INTARR(3,nbj)

  FOR aa=0,nbj-1 DO BEGIN
    CALDAT, jdat1 + aa , mo , da , ye
    tabj(0,aa) = da & tabj(1,aa) = mo & tabj(2,aa) = ye
  ENDFOR

;-----------------------------------------------------------
;Define the structure


  struct_im  = {ORIGIN:'', $
                WAVELNTH:'', $
                DATE_OBS:'', $
                DATE_OBS_CSI:'',$
                DATE_END:'', $
                FILENAME:'', $
                NAXIS1:0, $
                NAXIS2:0, $
                CDELT1:0D, $
                CDELT2:0D, $
                CRVAL1:0D, $
                CRVAL2:0D, $
                CENTER_X:0D, $
                CENTER_Y:0D, $
                SOLAR_R:0D }

  struct_fil = {srce:struct_im, $
                fil_index:0, $
                fil_ske_len_deg:0D, $
                fil_ske_cen_hel:DBLARR(2), $
                fil_curvature:0., $
                fil_ske_chain_strt_arcs:DBLARR(2), $
                fil_ske_chain:'', $
                fil_bnd_chain_strt_arcs:DBLARR(2), $
                fil_bnd_chain:'', $
                fil_area_deg2:0D, $
                fil_area_pix:0, $
                fil_centroi_hel:DBLARR(2), $
                fil_elongatedness:0D, $
                fil_bndg_rect_arcs:DBLARR(4), $
                fil_strength:0, $
                fil_orientation:0D} 

  str=struct_fil

;-----------------------------------------------------------
;LOOP on every date


  FOR ii=0,nbj-1 DO BEGIN

    ;-------------------------------------------------------
    ;Compute the name of the file and check if the processed
    ;version exists

    IF tabj(0,ii) LT 10 THEN t0 = '0'+STRTRIM(tabj(0,ii),2) $
    ELSE t0 = STRTRIM(tabj(0,ii),2)
    IF tabj(1,ii) LT 10 THEN t1 = '0'+STRTRIM(tabj(1,ii),2) $
    ELSE t1 = STRTRIM(tabj(1,ii),2)
    t2 = STRMID(STRTRIM(tabj(2,ii),2),2)
    file = 'mh'+t2+t1+t0+'*'+'_subtract_processed.fits'
    path = '/home/fuller/poub/FITS/PROCESSED/'
    ffile = FINDFILE(path+file,COUNT=cc)

    IF cc GT 0 THEN BEGIN
       gfile = ffile(cc-1) ;on prend la derniere obs du jour
       PRINT,gfile
    ENDIF ELSE BEGIN
       PRINT,'One file missing, checking other than meudon:',file
       file2 = '*'+t2+t1+t0+'*'+'_subtract_processed.fits' 
       ffile2 = FINDFILE(path+file2,COUNT=cc2)
       IF cc2 GT 0 THEN BEGIN    
          gfile = ffile2(cc2-1)
          PRINT,gfile
       ENDIF ELSE BEGIN   
          PRINT,'One file missing for good:',file2  
          GOTO,endloop
       ENDELSE
    ENDELSE

    ;--------------------------------------------------------
    ;Read the fits file

     arr = READFITS(gfile,head)

    ;--------------------------------------------------------
    ;Get info from the header of the fits file

     ;####use csi to get date in a common format
     filename = STRING(FXPAR(head,'FILENAME'))
     csi = ITOOL_SET_CSI(head,file=filename)

     h = struct_im

     h.ORIGIN        = STRING( FXPAR(head,'ORIGIN'))
     h.WAVELNTH      = STRING( FXPAR(head,'WAVELNTH'))
     h.DATE_OBS      = STRING( FXPAR(head,'DATE_OBS'))
     h.DATE_OBS_CSI  = csi.date_obs
     h.DATE_END      = STRING( FXPAR(head,'DATE_END'))
     h.FILENAME      = STRING(FXPAR(head,'FILENAME'))
     h.NAXIS1        = FIX( FXPAR(head,'NAXIS1'))
     h.NAXIS2        = FIX( FXPAR(head,'NAXIS2'))
     h.CDELT1        = DOUBLE( FXPAR(head,'CDELT1'))
     h.CDELT2        = DOUBLE( FXPAR(head,'CDELT2'))
     h.CRVAL1        = DOUBLE( FXPAR(head,'CRVAL1'))
     h.CRVAL2        = DOUBLE( FXPAR(head,'CRVAL2'))
     h.CENTER_X      = DOUBLE( FXPAR(head,'CENTER_X'))
     h.CENTER_Y      = DOUBLE( FXPAR(head,'CENTER_Y'))
     h.SOLAR_R       = DOUBLE( FXPAR(head,'SOLAR_R'))

    ;--------------------------------------------------------
    ;Find the filaments

     arrf = FILAMENT(INPUT=arr,CORRECTED=corrected)

    ;--------------------------------------------------------
    ;link close blobs and fill holes
    
     im = MORPH_CLOSE(arrf,REPLICATE(1,8,8))

    ;--------------------------------------------------------
    ;compute stat and geometry on each filament and fill the
    ;structure 

    stri = FILL_STRUCT(struct_fil,WHERE(im),h,corrected)
    str  = [str,stri]

    SAVE,str,filename='/home/fuller/IDL/FIL/test_struct.sav' 
    
  endloop:  
  ENDFOR

  str=str[1:N_ELEMENTS(str)-1]
  SAVE,str,filename='/home/fuller/IDL/FIL/test_struct.sav'

END

