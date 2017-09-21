PRO ROTATION,nr

;From the carrington rotation number (nr), write a
;CCCC.FIL ascii file describing the filaments
;over the whole rotation. The format of CCCC.FIL must be
;the same as Meudon files in order to suit the synthesis
;process giving the synoptic maps.
    

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
  ;write the final ascii file
     fname ='/home/fuller/IDL/FIL/'+STRTRIM(STRING(nr),2)+'.FIL'
     OPENW,wunit,fname,/GET_LUN
     FREE_LUN,wunit

;-----------------------------------------------------------
;LOOP on every date

  FOR ii=0,nbj-1 DO BEGIN

    OPENU,wunit,fname,/APPEND,/GET_LUN

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
          FREE_LUN,wunit
          GOTO,endloop
       ENDELSE
    ENDELSE

    ;--------------------------------------------------------
    ;Read the fits file

     arr = READFITS(gfile,head)

    ;--------------------------------------------------------
    ;Find the filaments

     arrf = FILAMENT(INPUT=arr)

    ;--------------------------------------------------------
    ;Filaments drawing

     tab = TWO_SIDES(arrf)

    ;--------------------------------------------------------
    ;Convert the coordinates to carrington and write the ascii

     ;tab = CONV_TAB(tab,head,nr,wunit)
     CONV_TAB,tab,head,nr,wunit

     FREE_LUN,wunit     
     
  endloop:  
  ENDFOR

END

