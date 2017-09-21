PRO FIL_ASCII_read2


;# This function reads the filament data from ascii files using RD_TFILE SSW function
;# and propose the user to chose filaments by specifying criteria such
;# as DATE,INTENSITY,LENGTH...

;# The user can also display the filaments skeleton and boundary on the
;# original image (CC_REDRAW)

;# The content of the array is described at the end of the program

;# BEFORE RUNNING THE THIS FUNCTION YOU NEED TO:

;#   - You MUST have access to Solar Soft routines
;#   - The fits images need to be saved in a specific directory
;#   - "pa" is the used path e.g. "/data/smith/FITS", "D:/test", ..
;#   - All the images must be saved in a subdirectory called "/Ha/2002/PROCESSED"
;#      E.g. create a directory called FITS in /data/smith, the images must be saved in
;#      "/data/smith/FITS/Ha/2002/PROCESSED/  and  pa='/data/smith/FITS'


;# Type the selected path
;# !!! no end "/" !!!

  pa = '/data2/fuller/FITS' ; change it to your path





;###### Locate the provided ascii data file

  asciifile = DIALOG_PICKFILE(FILE = 'filaments_ascii.txt', PATH = pa, $
                              TITLE = 'Please locate ASCII file')


;###### Read the ascii with rd_tfile()

  tab = RD_TFILE(asciifile,47,DELIM=';',/HSKIP,HEADER=head)
  
  nbf = (SIZE(tab))(2)

start:
;###### Menu

  PRINT, '#######################################'
  PRINT, 'You can display filaments by defining:'
  PRINT,   '- The INDEX        -> type 1'
  PRINT,   '- The LENGTH (deg) -> type 2'
  PRINT,   '- The DATE         -> type 3'
  PRINT,   '- The INTENSITY    -> type 4'
  PRINT,   '- EXIT             -> type 5'
  PRINT, 'Please select one of the criteria'
  PRINT, 'or combine them i.e. 1+2
  PRINT, '#######################################'
  crit = ''
  READ,crit
  res = INTARR(4)
  pluspos = STRPOS(crit,'+')
  IF pluspos EQ -1 THEN BEGIN

     res[0] = STRMID(STRTRIM(crit,2),0,1) 
     nsplit = 1

  ENDIF ELSE BEGIN

     split  = STRSPLIT(crit,'+',/EXTRACT)
     nsplit = N_ELEMENTS(split)
     res[0:nsplit-1] = STRTRIM(split[0:nsplit-1],2)

  ENDELSE

locs = [-1]

FOR ii=0,nsplit-1 DO BEGIN


CASE res[ii] OF

    1: BEGIN
       minind = MIN(FLOAT(tab[0,*]),MAX=maxind) 
       PRINT
       PRINT, '###################################################'
       PRINT, '# Please type the filaments index range: eg:> 12 37'
       PRINT, '# to display fil. with index between 12 and 37'
       PRINT, '# Min = ',STRTRIM(minind,2),' and Max = ',STRTRIM(maxind,2)
       PRINT, '###################################################'
       READ, range1, range2, PROMPT='Enter range: '
       loc = WHERE(FIX(tab[0,*]) GE FIX(range1) AND FIX(tab[0,*]) LE FIX(range2))
       END

    2: BEGIN
       minl = MIN(FLOAT(tab[38,*]),MAX=maxl)
       PRINT
       PRINT, '#######################################################'
       PRINT, '# Please type the filaments length range: eg:> 1.2 13.6'
       PRINT, '# to display fil. with length between 1.2° and 13.6°'
       PRINT, '# Min = ',STRTRIM(minl,2),' and Max = ',STRTRIM(maxl,2)
       PRINT, '###################################################'
       READ, range1, range2, PROMPT='Enter range: '
       loc = WHERE(FLOAT(tab[38,*]) GE FLOAT(range1) AND FLOAT(tab[38,*]) LE FLOAT(range2))
       END 

    3: BEGIN
            date1 =''
            date2= ''
            print
            print, '#########################################################################'
            print, 'Please type the required DATE in this format YYYY-MM-DD, e.g 2002-04-21'
            print, 'IF you would like the filaments for one day type same START and END'
            print, '########################################################################'

            READ, FORMAT = '(A10)', date1, PROMPT='START date (then ENTER): '
            READ, FORMAT = '(A10)', date2, PROMPT='END date (then ENTER): '
           

            jdstart = ANYTIM2JD(date1)
            jdend   = ANYTIM2JD(date2)
            jdstart2= LONG(jdstart.int)+DOUBLE(jdstart.frac)
            jdend2  = LONG(jdend.int)+DOUBLE(jdend.frac)
            tabtmp  = LONG(tab[3,*])+DOUBLE(tab[4,*])
            IF jdstart2 NE jdend2 THEN BEGIN
               loc    = WHERE(tabtmp GE jdstart2 AND tabtmp LE jdend2)
            ENDIF ELSE BEGIN
               loc    = WHERE(tabtmp GE jdstart2 AND tabtmp LE jdstart2+1.)
            ENDELSE

        END

    4: BEGIN
         minl = MIN(FLOAT(tab[20,*]),MAX=maxl)
         PRINT
         PRINT, '################################################################'
         PRINT, '# Please type the filaments intensity ratio range: eg:> 0.2 0.9'
         PRINT, '# to display fil. with intensity ratio  between 0.2 and 0.9'
         PRINT, '# Min = ',STRTRIM(minl,2),' and Max = ',STRTRIM(maxl,2)
         PRINT, '# Value corresp. to ratio btw filament Int. and Quiet Sun Int.' 
         PRINT, '################################################################'
         READ, range1, range2, PROMPT='Enter range: '
         loc = WHERE(FLOAT(tab[20,*]) GE FLOAT(range1) AND FLOAT(tab[20,*]) LE FLOAT(range2))
       END 

    5: BEGIN
         RETALL
       END 

ENDCASE

    locs = [locs,loc]
    IF ii GT 0 THEN BEGIN
      locs  = locs[SORT(locs)]
      locsw = WHERE(locs EQ SHIFT(locs,-1),nw)
      IF nw NE 0 THEN locs  = locs[locsw] ELSE locs=[-1]
    ENDIF

ENDFOR

    locm = WHERE(locs NE -1,nm)
    IF nm GT 0 THEN BEGIN
       locs = locs[locm]
       PRINT, 'number of matching filaments:=', N_ELEMENTS(locs)

;#### Display

        FIL_index = FIX(tab[0,locs])
        FIL_date  = STRING(tab[1,locs])
        FOR ii=0,N_ELEMENTS(locs)-1 DO PRINT, FIL_index[ii],' ; ',FIL_date[ii]

        PRINT, '#################################'
        PRINT, 'You can display filaments:'
        PRINT,   '- skeletons        -> type 1'
        PRINT,   '- boundary         -> type 2'
        PRINT,   '- both             -> type 3'
        PRINT,   '- EXIT             -> type 4'
        PRINT, 'Please select one of the display'
        PRINT, '#################################'
     
        res2 = GET_KBRD(1)
        res2 = LONG(STRMID(res2, 0, 1))
        
        CASE res2[0] OF

            1: BEGIN
               CC_REDRAW,tab,locs,pa,/SKE
            END
            2: BEGIN
               CC_REDRAW,tab,locs,pa,/BND
            END
            3: BEGIN
               CC_REDRAW,tab,locs,pa,/BOTH
            END
            4: BEGIN
               RETALL
            END

       ENDCASE

        PRINT, '##################################'
        PRINT, 'Try another search ? (Y or N)'
        PRINT, '##################################'
        res3 = GET_KBRD(1)
        res3 = STRUPCASE(res3)
        IF STRTRIM(res3,2) EQ 'Y' THEN GOTO,start ELSE RETALL

    ENDIF ELSE BEGIN

        PRINT, '##################################'
        PRINT, 'Sorry, no matching filaments found'
        PRINT, 'Try another search ? (Y or N)'
        PRINT, '##################################'
        res3 = GET_KBRD(1)
        res3 = STRUPCASE(res3)
        IF STRTRIM(res3,2) EQ 'Y' THEN GOTO,start ELSE RETALL
 
    ENDELSE

;##### TABLE DESCRIPTION
; tab[0,*]   -> INDEX
; tab[1,*]   -> DATE-OBS
; tab[2,*]   -> CARROT
; tab[3,*]   -> JDINT
; tab[4,*]   -> JDFRAC
; tab[5,*]   -> NAXIS1
; tab[6,*]   -> NAXIS2
; tab[7,*]   -> CDELT1
; tab[8,*]   -> CDELT2
; tab[9,*]   -> R_SUN
; tab[10,*]  -> CENTER_X
; tab[11,*]  -> CENTER_Y
; tab[12,*]  -> LOC_FILE
; tab[13,*]  -> QSUNINT
; tab[14,*]  -> GRAV_C_ARCX
; tab[15,*]  -> GRAV_C_ARCY
; tab[16,*]  -> GRAV_C_CAR_LAT
; tab[17,*]  -> GRAV_C_CAR_LON
; tab[18,*]  -> SAMPLECOUNT
; tab[19,*]  -> AREA
; tab[20,*]  -> MEAN_INT_RATIO
; tab[21,*]  -> BRARC_X_LL
; tab[22,*]  -> BRARC_Y_LL
; tab[23,*]  -> BRARC_X_UR
; tab[24,*]  -> BRARC_Y_UR
; tab[25,*]  -> BRPIX_X_LL
; tab[26,*]  -> BRPIX_Y_LL
; tab[27,*]  -> BRPIX_X_UR
; tab[28,*]  -> BRPIX_Y_UR
; tab[29,*]  -> FEAT_MAX_INT
; tab[30,*]  -> FEAT_MIN_INT
; tab[31,*]  -> FEAT_MEAN_INT
; tab[32,*]  -> ENC_MET
; tab[33,*]  -> COD_PIX_X
; tab[34,*]  -> COD_PIX_Y
; tab[35,*]  -> COD_ARC_X
; tab[36,*]  -> COD_ARC_Y
; tab[37,*]  -> CHAIN_CODE
; tab[38,*]  -> SKE_LENGTH
; tab[39,*]  -> CURVATURE
; tab[40,*]  -> ELONG
; tab[41,*]  -> ORIENTATION
; tab[42,*]  -> COD_SKE_PIX_X
; tab[43,*]  -> COD_SKE_PIX_Y
; tab[44,*]  -> COD_SKE_ARC_X
; tab[45,*]  -> COD_SKE_ARC_Y
; tab[46,*]  -> CHAIN_CODE_SKE


END
