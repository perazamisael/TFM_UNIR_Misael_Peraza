@/home/fuller/IDL/FIL/struct_draw_fil.pro

PRO FILELIST2STRUCT

;From a standardize fits file list, saves a
;structure file describing the filaments
;detected in each file.
    
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
;Get the filenames to compute

  p='/home/fuller/poub/FITS/PROCESSED/'
  filenames=DIALOG_PICKFILE(path=p, /multiple_files)
  IF filenames(0) EQ '' THEN GOTO,endf
  nbj=N_ELEMENTS(filenames)


;-----------------------------------------------------------
;LOOP on every date

  FOR ii=0,nbj-1 DO BEGIN

    ;--------------------------------------------------------
    ;Read the fits file

     gfile = filenames(ii)
     print,gfile
     arr = READFITS(gfile,head)

    ;--------------------------------------------------------
    ;Get info from the header of the fits file

     ;####use csi to get date in a common format
     filename = STRING(FXPAR(head,'FILENAME'))
     csi = ITOOL_SET_CSI(head,file=filename)

     si = struct_im

     si.ORIGIN        = STRING( FXPAR(head,'ORIGIN'))
     si.WAVELNTH      = STRING( FXPAR(head,'WAVELNTH'))
     si.DATE_OBS      = STRING( FXPAR(head,'DATE_OBS'))
     si.DATE_OBS_CSI  = csi.date_obs
     si.DATE_END      = STRING( FXPAR(head,'DATE_END'))
     si.FILENAME      = STRING(FXPAR(head,'FILENAME'))
     si.NAXIS1        = FIX( FXPAR(head,'NAXIS1'))
     si.NAXIS2        = FIX( FXPAR(head,'NAXIS2'))
     si.CDELT1        = DOUBLE( FXPAR(head,'CDELT1'))
     si.CDELT2        = DOUBLE( FXPAR(head,'CDELT2'))
     si.CRVAL1        = DOUBLE( FXPAR(head,'CRVAL1'))
     si.CRVAL2        = DOUBLE( FXPAR(head,'CRVAL2'))
     si.CENTER_X      = DOUBLE( FXPAR(head,'CENTER_X'))
     si.CENTER_Y      = DOUBLE( FXPAR(head,'CENTER_Y'))
     si.SOLAR_R       = DOUBLE( FXPAR(head,'SOLAR_R'))

    ;--------------------------------------------------------
    ;Find the filaments

     arrf = FILAMENT(INPUT=arr,CORRECTED=corrected)

    ;--------------------------------------------------------
    ;link close blobs and fill holes
    
     im = MORPH_CLOSE(arrf,REPLICATE(1,8,8))

    ;--------------------------------------------------------
    ;compute stat and geometry on each filament and fill the
    ;structure 

    stri = FILL_STRUCT(struct_fil,WHERE(im),si,corrected)
    str  = [str,stri]

;affichage journalier pour comparaison avec bass2000

goto,nocompare

    nn=N_ELEMENTS((stri[*].fil_index))
    
    toto=bytarr(1024,1024)
    window,/free,xs=1024,ys=1024

print,nn

    for kk = 0, nn-1 do begin
        firstpt = stri[kk].fil_ske_chain_strt_arcs
        firstptx = (firstpt[0]-si.CRVAL1)/si.CDELT1+si.CENTER_X
        firstpty = (firstpt[1]-si.CRVAL2)/si.CDELT2+si.CENTER_Y
        chainc = stri[kk].fil_ske_chain
        indices = CHAIN2IND([firstptx,firstpty],chainc,1024,1024)
        toto[indices]=1b
    endfor

tvscl,toto

    for kk = 0, nn-1 do begin
        firstpt = stri[kk].fil_ske_chain_strt_arcs
        firstptx = (firstpt[0]-si.CRVAL1)/si.CDELT1+si.CENTER_X
        firstpty = (firstpt[1]-si.CRVAL2)/si.CDELT2+si.CENTER_Y
        lgth = stri[kk].fil_ske_len_deg
        xyouts,firstptx+5,firstpty,STRTRIM(STRING(FIX(lgth+0.5)),2),/device
    endfor

;fin comparaison

nocompare:

    SAVE,str,filename='/home/fuller/IDL/FIL/test_struct.sav' 



  endloop:  
  ENDFOR

  str=str[1:N_ELEMENTS(str)-1]
  SAVE,str,filename='/home/fuller/IDL/FIL/test_struct.sav'

endf:
END
