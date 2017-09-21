;@/home/fuller/IDL/FIL/struct_draw_fil.pro

PRO FILELIST2STRUCT

;From a standardize fits file list, saves 
;structure files describing the filaments
;detected in each file.
    
;-----------------------------------------------------------
;Define the structures

  default = '\N'

  STRUCT_OBS = {OBSERVATIONALPARAMETERS, $
                IND:               default, $
                OBSERVAT:          default, $
                INSTRUME:          default, $
                TELESCOP:          default, $
                UNITS:             default, $
                WAVELNTH:          default, $
                WAVELNTH_NAME:     default, $
                WAVELNTH_UNIT:     default, $
                OBS_TYPE:          default, $
                DATE_OBS:          default, $
                DATE_END:          default, $
                JDINT:             default, $
                JDFRAC:            default, $
                EXPTIME:           default, $
                CARROT:            default, $
                BSCALE:            default, $
                BZERO:             default, $
                BITPIX:            default, $
                NAXIS1:            default, $
                NAXIS2:            default, $
                R_SUN:             default, $
                CENTER_X:          default, $
                CENTER_Y:          default, $
                CDELT1:            default, $
                CDELT2:            default, $ 
                QUALITY:           default, $
                FILENAME:          default, $
                COMMENT:           default, $
                IMAGE_COVER:       default, $
                IMAGE_COMP:        default, $
                DATE_OBS_STRING:   default, $
                DATE_END_STRING:   default, $
                LOCAL_FILENAME:    default, $
                EOF:              '-999.999'}

  STRUCT_PRO = {PROCESSINGPARAMETERS, $
                IND:               default, $
                RUN_DATE:          default, $
                LOC_FILE:          default, $
                EL_CEN_X:          default, $
                EL_CEN_Y:          default, $
                EL_AXIS1:          default, $
                EL_AXIS2:          default, $
                EL_ANGLE:          default, $
                STDEV:             default, $
                STDEVGEO:          default, $
                ALGERR:            default, $                
                CDELT1:            default, $
                CDELT2:            default, $
                BITPIX:            default, $
                QSUNINT:           default, $
                LOCAL_FILENAME:    default, $
                PP_LOCAL_FILENAME: default, $
                INSTITUT:          default, $
                CODE:              default, $ 
                VERSION:           default, $
                CONTACT:           default, $
                EFIT:              default, $
                STANDARD:          default, $
                LIMBDARK:          default, $
                BACKGROUND:        default, $
                LINECLEAN:         default, $
                QSUNINT_PRO:       default, $
                PERCENT:           default, $
                NAXIS1:            default, $
                NAXIS2:            default, $
                CENTER_X:          default, $
                CENTER_Y:          default, $
                R_SUN:             default, $
                DIVISION:          default, $
                INORM:             default, $
                EOF:              '-999.999'}

  STRUCT_FIL = {FEATUREPARAMETERS, $
                IND:               default, $
                RUN_DATE:          default, $
                GRAV_C_ARCX:       default, $
                GRAV_C_ARCY:       default, $
                GRAV_C_CAR_LAT:    default, $
                GRAV_C_CAR_LON:    default, $
                SAMPLECOUNT:       default, $
                AREA:              default, $ 
                MEAN_INT_RATIO:    default, $
                BRARC_X_LL:        default, $
                BRARC_Y_LL:        default, $
                BRARC_X_UR:        default, $
                BRARC_Y_UR:        default, $
                BRPIX_X_LL:        default, $
                BRPIX_Y_LL:        default, $
                BRPIX_X_UR:        default, $
                BRPIX_Y_UR:        default, $
                FEAT_MAX_INT:      default, $
                FEAT_MIN_INT:      default, $
                FEAT_MEAN_INT:     default, $
                ENC_MET:           default, $
                COD_PIX_X:         default, $
                COD_PIX_Y:         default, $
                COD_ARC_X:         default, $
                COD_ARC_Y:         default, $
                SKE_LEN_DEG:       default, $
                THICKNESS_PIX:     default, $
                CURVATURE:         default, $
                ELONG:             default, $
                ORIENTATION:       default, $
                COD_SKE_PIX_X:     default, $
                COD_SKE_PIX_Y:     default, $
                COD_SKE_ARC_X:     default, $
                COD_SKE_ARC_Y:     default, $
                CHAIN_CODE:        default, $
                CCODE_LNTH:        default, $
                CHAIN_CODE_SKE:    default, $
                CCODE_SKE_LNTH:    default, $
                ;RASTER_SCAN_LNTH:  default, $
                PP_LOCAL_FILENAME: default, $
                EOR:             '999.999', $
                EOF:              '-999.999'}

  ;STRUCT_CCO = {CHAINCODEPARAMETERS, $
  ;              IND:0L, $
  ;              IND_FEAT:0, $
  ;              COD_PIX_X:0, $
  ;              COD_PIX_Y:0, $
  ;              COD_ARC_X:0D, $
  ;              COD_ARC_Y:0D, $
  ;              COD_SKE_PIX_X:0, $
  ;              COD_SKE_PIX_Y:0, $
  ;              COD_SKE_ARC_X:0D, $
  ;              COD_SKE_ARC_Y:0D, $
  ;              SKE_CHAIN:'\N', $
  ;              BND_CHAIN:'\N', $
  ;              EOR:999.999}

;  strobs = STRUCT_OBS
;  strpro = STRUCT_PRO
;  strfil = STRUCT_FIL

;-----------------------------------------------------------
;Get the processed filenames to compute

  path = '/data2/fuller/FITS/Ha/'
  filenames = DIALOG_PICKFILE(PATH=path, /multiple_files, GET_PATH=wpath)
  IF filenames(0) EQ '' THEN RETALL
  nbj = N_ELEMENTS(filenames)


;-----------------------------------------------------------
;LOOP on every date

  FOR ii=0,nbj-1 DO BEGIN

    strobs = STRUCT_OBS
    strpro = STRUCT_PRO
    strfil = STRUCT_FIL

    ;--------------------------------------------------------
    ;Read the processed fits file

     gfile = filenames(ii)
     print,gfile
     arr = READFITS(gfile,headp)

    ;--------------------------------------------------------
    ;Read the original fits file header and get path/filenames

     splitf  = STRSPLIT(gfile,'/',/EXTRACT)
     nsplitf = N_ELEMENTS(splitf)
     fpath   = '/' + STRJOIN(splitf[0:nsplitf-3],'/') + '/'
     ofile   = splitf[nsplitf-1]
     splitf2 = STRSPLIT(ofile,'_subtract_processed',/EXTRACT,/REGEX)
     ogfile  = fpath + splitf2[0] + '*'
     SPAWN,'ls '+ogfile,res
     ;oarr    = READFITS(res[0],head,numrow=0)
     originalfname = res[0]
     head    = HEADFITS(originalfname) ;we only need the header info

     splitf3 = STRMID(splitf2[0],2)
     locfile = STRMID(gfile,STRPOS(gfile,'FITS')+4) 
     

    ;--------------------------------------------------------
    ;Get obs time info from date_obs -> julian day

     val     = FXPAR(head,'DATE_OBS',COUNT=count)
     IF count GE 1 THEN date = STRTRIM(STRING(val),2) ELSE BEGIN
        PRINT,'no DATE_OBS data found'
        RETALL
     ENDELSE
     year    = FIX(STRMID(date,0,4))
     month   = FIX(STRMID(date,5,2))                                    
     day     = FIX(STRMID(date,8,2))
     hour    = FIX(STRMID(date,11,2))
     min     = FIX(STRMID(date,14,2))
     sec     = FIX(STRMID(date,17,2))
     jdfloat = JULDAY(month,day,year,hour,min,sec)
     jdint   = LONG(jdfloat)
     jdfrac  = jdfloat - jdint   

    ;--------------------------------------------------------
    ;Get runtime info from systime

     CALDAT,SYSTIME(/JULIAN,/UTC),mm,dd,yy,hh,mi,ss
     ss = FIX(ss)
     IF mm LT 10 THEN mm='0'+STRTRIM(mm,2) ELSE mm=STRTRIM(mm,2)
     IF dd LT 10 THEN dd='0'+STRTRIM(dd,2) ELSE dd=STRTRIM(dd,2)
     IF hh LT 10 THEN hh='0'+STRTRIM(hh,2) ELSE hh=STRTRIM(hh,2)
     IF mi LT 10 THEN mi='0'+STRTRIM(mi,2) ELSE mi=STRTRIM(mi,2)
     IF ss LT 10 THEN ss='0'+STRTRIM(ss,2) ELSE ss=STRTRIM(ss,2)
     yy = STRTRIM(yy,2)    
     today_utc = yy+'-'+mm+'-'+dd+'T'+hh+':'+mi+':'+ss

 
    ;--------------------------------------------------------
    ;Get info from the header of the original fits file

     val01 = FXPAR(head,'NAXIS1'      ,COUNT=count01)
     val02 = FXPAR(head,'NAXIS2'      ,COUNT=count02)
     val03 = FXPAR(head,'CDELT1'      ,COUNT=count03)
     val04 = FXPAR(head,'CDELT2'      ,COUNT=count04)
     val05 = FXPAR(head,'CENTER_X'    ,COUNT=count05)
     val06 = FXPAR(head,'CENTER_Y'    ,COUNT=count06)
     val07 = FXPAR(head,'SOLAR_R'     ,COUNT=count07)  
     val08 = FXPAR(head,'INSTITUT'    ,COUNT=count08)
     val09 = FXPAR(head,'ORIGIN'      ,COUNT=count09)
     val10 = FXPAR(head,'INSTRUME'    ,COUNT=count10)
     val11 = FXPAR(head,'TELESCOP'    ,COUNT=count11)
     val12 = FXPAR(head,'WAVELNTH'    ,COUNT=count12)
     val13 = FXPAR(head,'WAVELNTH_NAME',COUNT=count13)
     val14 = FXPAR(head,'OBS_TYPE'    ,COUNT=count14)
     val15 = FXPAR(head,'DATE_OBS'    ,COUNT=count15)
     val16 = FXPAR(head,'DATE_END'    ,COUNT=count16)
     val17 = FXPAR(head,'BSCALE'      ,COUNT=count17)
     val18 = FXPAR(head,'BZERO'       ,COUNT=count18)
     val19 = FXPAR(head,'EXPTIME'     ,COUNT=count19)
     val20 = FXPAR(head,'SOLROT_N'    ,COUNT=count20)
     val21 = FXPAR(head,'BITPIX'      ,COUNT=count21)
     val22 = FXPAR(head,'FILENAME'    ,COUNT=count22)
     val23 = FXPAR(head,'UNITS'       ,COUNT=count23)
     val24 = FXPAR(head,'QUALITY'     ,COUNT=count24)
     val27 = FXPAR(head,'COMMENT'     ,COUNT=count27)

     IF count01 GE 1 THEN strobs.NAXIS1        = FIX(val01)
     IF count02 GE 1 THEN strobs.NAXIS2        = FIX(val02)
     IF count03 GE 1 THEN strobs.CDELT1        = DOUBLE(val03)
     IF count04 GE 1 THEN strobs.CDELT2        = DOUBLE(val04)
     IF count05 GE 1 THEN strobs.CENTER_X      = DOUBLE(val05)
     IF count06 GE 1 THEN strobs.CENTER_Y      = DOUBLE(val06)
     IF count07 GE 1 THEN strobs.R_SUN         = DOUBLE(val07)  
     IF count08 GE 1 THEN strobs.OBSERVAT      = STRING(val08)
     IF count09 GE 1 THEN strobs.OBSERVAT      = STRING(val09)
     IF count10 GE 1 THEN strobs.INSTRUME      = STRING(val10)
     IF count11 GE 1 THEN strobs.TELESCOP      = STRING(val11)
     IF count12 GE 1 THEN strobs.WAVELNTH      = STRING(val12)
     IF count13 GE 1 THEN strobs.WAVELNTH_NAME = STRING(val13) ELSE $
                          strobs.WAVELNTH_NAME = 'HALPHA'
     IF count14 GE 1 THEN strobs.OBS_TYPE      = STRING(val14) ELSE $
                          strobs.OBS_TYPE      = 'FUV'
     IF count15 GE 1 THEN strobs.DATE_OBS      = STRING(val15)
     IF count16 GE 1 THEN strobs.DATE_END      = STRING(val16)
     IF count17 GE 1 THEN strobs.BSCALE        = STRING(val17)
     IF count18 GE 1 THEN strobs.BZERO         = STRING(val18)
     IF count19 GE 1 THEN strobs.EXPTIME       = STRING(val19)
     IF count20 GE 1 THEN strobs.CARROT        = FIX(val20)
     IF count21 GE 1 THEN strobs.BITPIX        = FIX(val21)
     IF count22 GE 1 THEN strobs.FILENAME      = STRING(val22)
     IF count23 GE 1 THEN strobs.UNITS         = STRING(val23) ELSE $
                          strobs.UNITS         = 'COUNTS'
     IF count24 GE 1 THEN strobs.QUALITY       = STRING(val24)
     IF count27 GE 1 THEN strobs.COMMENT       = STRING(val27)


    ;--------------------------------------------------------
    ;Other info 
                          strobs.DATE_OBS_STRING = strobs.DATE_OBS
                          strobs.DATE_END_STRING = strobs.DATE_END
                          strobs.LOCAL_FILENAME  = STRING(originalfname)
                          strobs.JDINT           = jdint
                          strobs.JDFRAC          = jdfrac


    ;--------------------------------------------------------
    ;Get info from the header of the preprocessed fits file

     val01 = FXPAR(headp,'NAXIS1'      ,COUNT=count01)
     val02 = FXPAR(headp,'NAXIS2'      ,COUNT=count02)
     val03 = FXPAR(headp,'CDELT1'      ,COUNT=count03)
     val04 = FXPAR(headp,'CDELT2'      ,COUNT=count04)
     val05 = FXPAR(headp,'CENTER_X'    ,COUNT=count05)
     val06 = FXPAR(headp,'CENTER_Y'    ,COUNT=count06)
     val07 = FXPAR(headp,'SOLAR_R'     ,COUNT=count07)  
     val08 = FXPAR(headp,'DATE'        ,COUNT=count08)

     IF count01 GE 1 THEN strpro.NAXIS1        = FIX(val01)
     IF count02 GE 1 THEN strpro.NAXIS2        = FIX(val02)
     IF count03 GE 1 THEN strpro.CDELT1        = DOUBLE(val03)
     IF count04 GE 1 THEN strpro.CDELT2        = DOUBLE(val04)
     IF count05 GE 1 THEN strpro.CENTER_X      = DOUBLE(val05)
     IF count06 GE 1 THEN strpro.CENTER_Y      = DOUBLE(val06)
     IF count07 GE 1 THEN strpro.R_SUN         = DOUBLE(val07)  
     IF count08 GE 1 THEN PPRD                 = BIN_DATE(val08) 
 
     strpro.LOC_FILE          = locfile ;or gfile to have the whole path
     strpro.INSTITUT          = 'Meudon Observatory'
     strpro.LOCAL_FILENAME    = STRING(originalfname)
     strpro.PP_LOCAL_FILENAME = gfile
     ;### QSUNINT : calculation after cleaning


     IF FIX(strpro.CDELT1) EQ 0 OR FIX(strpro.CDELT2) EQ 0 OR strobs.CARROT EQ default THEN BEGIN
        PRINT,'no cdelt or carrot info in std. fits header!!, using GET_SUN function'
        tmp = GET_SUN(STRTRIM(strobs.DATE_OBS,2),SD=sd,CARR=carr)
        IF FIX(strpro.CDELT1) EQ 0 OR FIX(strpro.CDELT2) EQ 0 THEN BEGIN
          strpro.CDELT1 = sd/420.
          strpro.CDELT2 = strpro.CDELT1 ;(std. Sun is round)
        ENDIF
        IF strobs.CARROT EQ default THEN BEGIN
          strobs.CARROT = FIX(carr)
        ENDIF
     ENDIF

    ;--------------------------------------------------------
    ;Get preprocess runtime modifying 'date' header keyword

     IF count08 GE 1  THEN BEGIN
       IF PPRD[1] LT 10 THEN mm='0'+STRTRIM(PPRD[1],2) ELSE mm=STRTRIM(PPRD[1],2)
       IF PPRD[2] LT 10 THEN dd='0'+STRTRIM(PPRD[2],2) ELSE dd=STRTRIM(PPRD[2],2)
       IF PPRD[3] LT 10 THEN hh='0'+STRTRIM(PPRD[3],2) ELSE hh=STRTRIM(PPRD[3],2)
       IF PPRD[4] LT 10 THEN mi='0'+STRTRIM(PPRD[4],2) ELSE mi=STRTRIM(PPRD[4],2)
       IF PPRD[5] LT 10 THEN ss='0'+STRTRIM(PPRD[5],2) ELSE ss=STRTRIM(PPRD[5],2)
       yy = STRTRIM(PPRD[0],2)    
       PPRD = yy+'-'+mm+'-'+dd+' '+hh+':'+mi+':'+ss
       strpro.RUN_DATE = PPRD
     ENDIF
    
     ;-------------------------------------------------------
     ;define type of obs to use the correct parameters

     info1=STRTRIM(strobs.OBSERVAT,2)
     info2=STRTRIM(strobs.NAXIS1,2)
     IF info1 EQ 'BBSO' THEN obst = 3
     IF info1 NE 'BBSO' AND FIX(info2) EQ 1500 THEN obst = 2
     IF info1 NE 'BBSO' AND FIX(info2) NE 1500 THEN obst = 1

    ;--------------------------------------------------------
    ;Image cleaning (dust lines, bckg normalisation)

;     IF obst EQ 1 THEN corrected = CLEAN_IMAGE(TEMPORARY(arr),DOUBLE(strpro.R_SUN),/MEUDON1,/HOUGH)
;     IF obst EQ 2 THEN corrected = CLEAN_IMAGE(TEMPORARY(arr),DOUBLE(strpro.R_SUN),/MEUDON2,/HOUGH)
;     IF obst EQ 3 THEN corrected = CLEAN_IMAGE(TEMPORARY(arr),DOUBLE(strpro.R_SUN),/BBSO)
;     corrected = LONG(corrected)

;print,'##1=meudon1##2=meudon2##3=bbso##',obst

    ;--------------------------------------------------------
    ;Compute the quiet sun intensity

;     hist = MEDIAN(HISTOGRAM(corrected[WHERE(corrected)],min=0.),10)
;     maxh = MAX(hist)
;     wmax = ( WHERE(hist EQ maxh) )[0]
;     strpro.QSUNINT = FLOAT(wmax)


    ;--------------------------------------------------------
    ;Find the filaments

     ;IF obst EQ 1 THEN arrf = EFR_FILAMENT('meudon1',840,INPUT=corrected)
     ;IF obst EQ 2 THEN arrf = EFR_FILAMENT('meudon2',840,INPUT=corrected)
     ;IF obst EQ 3 THEN arrf = EFR_FILAMENT('bbso',840,INPUT=corrected)

  IF obst EQ 1 THEN arrf = EFR_FILAMENT('meudon1',DOUBLE(2.*strpro.R_SUN),INPUT=TEMPORARY(arr), $
                                        CORRECTED=corrected,/lin,/fla,/dus,/dis)
  IF obst EQ 2 THEN arrf = EFR_FILAMENT('meudon2',DOUBLE(2.*strpro.R_SUN),INPUT=TEMPORARY(arr), $
                                        CORRECTED=corrected,/lin,/fla,/dus,/dis)
  IF obst EQ 3 THEN arrf = EFR_FILAMENT('bbso',DOUBLE(2.*strpro.R_SUN),INPUT=TEMPORARY(arr), $
                                        CORRECTED=corrected,/fla,/dis)

    ;--------------------------------------------------------
    ;Compute the quiet sun intensity

     hist = MEDIAN(HISTOGRAM(corrected[WHERE(corrected)],min=0.),10)
     maxh = MAX(hist)
     wmax = ( WHERE(hist EQ maxh) )[0]
     strpro.QSUNINT = FLOAT(wmax)


    ;--------------------------------------------------------
    ;link close blobs and fill holes

;a placer dans le programme filament
    
     im = MORPH_CLOSE(TEMPORARY(arrf),REPLICATE(1,8,8))


    ;--------------------------------------------------------
    ;compute stat and geometry on each filament and fill the
    ;structure 

    strfil = EFR_FEAT_DESCRIBE(WHERE(im),strfil,FIX(strpro.NAXIS1),FIX(strpro.NAXIS2), $
                         DOUBLE(strpro.CDELT1),DOUBLE(strpro.CDELT2),DOUBLE(strpro.CENTER_X),$
                         DOUBLE(strpro.CENTER_Y),DOUBLE(strpro.R_SUN),STRTRIM(strobs.DATE_OBS,2), $
                         DOUBLE(strpro.QSUNINT),corrected)

    numfil = N_ELEMENTS(strfil)


    ;----------------------------------------------------------
    ;Other info for strfil
    
    ;strfil[*].NUM_FEAT         = numfil
    strfil[*].RUN_DATE          = today_utc
    strfil[*].ENC_MET           = 'CHAIN CODE'
    strfil[*].PP_LOCAL_FILENAME = gfile
    
    ;-----------------------------------------------------------
    ;assign a uniq ID to the file

    strobs.IND = ii+1;RANDOMN(seed,/LONG)
    strpro.IND = ii+1
    strfil[*].IND = ii+1

    ;--------------------------------------------------------
    ;Define path and filenames to store the results
   
     spath = '/home/fuller/poub/SAV/STRUCT/'
     idlfile1 = spath + splitf3[0] + '_fil.sav'
     idlfile2 = spath + splitf3[0] + '_fi_init.txt'
     idlfile3 = spath + splitf3[0] + '_fi_norm.txt'
     idlfile4 = spath + splitf3[0] + '_fi_feat.txt'
     idlfile5 = spath + splitf3[0] + '_fi_ccode.txt'
     idlfile6 = spath + splitf3[0] + '_fi_image.cdf'

    ;------------------------------------------------------------
    ;Save the 3 structures in an idl file


    SAVE,strobs,strpro,strfil,filename=idlfile1 



    ;------------------------------------------------------------
    ;Write and save ascii files


    ;FILE *fi_init.txt------------------------------------------
     OPENW,un,idlfile2,/GET_LUN

 PRINTF,un,STRTRIM(strobs.IND,2)+';'+$
          ;STRTRIM(strobs.OBSERVAT,2)+';'+$
          ;STRTRIM(strobs.INSTRUME,2)+';'+$
          ;STRTRIM(strobs.TELESCOP,2)+';'+$
          ;STRTRIM(strobs.WAVELNTH,2)+';'+$
          ;STRTRIM(strobs.WAVELNTH_NAME,2)+';'+$
          ;STRTRIM(strobs.OBS_TYPE,2)+';'+$
          ;STRTRIM(strobs.UNITS,2)+';'+$
           STRTRIM(strobs.DATE_OBS,2)+';'+$
           STRTRIM(strobs.DATE_END,2)+';'+$
           STRTRIM(strobs.JDINT,2)+';'+$
           STRTRIM(strobs.JDFRAC,2)+';'+$
           STRTRIM(strobs.EXPTIME,2)+';'+$
           STRTRIM(strobs.CARROT,2)+';'+$
           STRTRIM(strobs.BSCALE,2)+';'+$
           STRTRIM(strobs.BZERO,2)+';'+$
           STRTRIM(strobs.BITPIX,2)+';'+$
           STRTRIM(strobs.NAXIS1,2)+';'+$
           STRTRIM(strobs.NAXIS2,2)+';'+$
           STRTRIM(strobs.R_SUN,2)+';'+$
           STRTRIM(strobs.CENTER_X,2)+';'+$
           STRTRIM(strobs.CENTER_Y,2)+';'+$
           STRTRIM(strobs.CDELT1,2)+';'+$
           STRTRIM(strobs.CDELT2,2)+';'+$
           STRTRIM(strobs.QUALITY,2)+';'+$
           STRTRIM(strobs.FILENAME,2)+';'+$
           STRTRIM(strobs.LOCAL_FILENAME,2)+';'+$
           STRTRIM(strobs.DATE_OBS_STRING,2)+';'+$
           STRTRIM(strobs.DATE_END_STRING,2)+';'+$
           STRTRIM(strobs.COMMENT,2)+';';+$
           ;STRTRIM(strobs.EOF,2)+';'

     CLOSE,un
     FREE_LUN,un,/FORCE

    ;FILE *fi_norm.txt---------------------------------------------------

     OPENW,un,idlfile3,/GET_LUN

 PRINTF,un,STRTRIM(strpro.IND,2)+';'+$
           STRTRIM(strpro.RUN_DATE,2)+';'+$
           STRTRIM(strpro.LOC_FILE,2)+';'+$
           STRTRIM(strpro.CDELT1,2)+';'+$
           STRTRIM(strpro.CDELT2,2)+';'+$
           STRTRIM(strpro.QSUNINT,2)+';'+$
           STRTRIM(strpro.LOCAL_FILENAME,2)+';'+$
           STRTRIM(strpro.PP_LOCAL_FILENAME,2)+';';+$
           ;STRTRIM(strpro.EOF,2)+';'   

     CLOSE,un
     FREE_LUN,un,/FORCE

    ;FILE *fi_feat.txt---------------------------------------------------
     OPENW,un,idlfile4,/GET_LUN
     nele = N_ELEMENTS(strfil)

 ;PRINTF,un,STRTRIM(nele,2),';',$

       FOR nn = 0, nele-1 DO BEGIN

 PRINTF,un,STRTRIM(strfil[nn].IND,2)+';'+$
           STRTRIM(strfil[nn].RUN_DATE,2)+';'+$
           STRTRIM(strfil[nn].GRAV_C_ARCX,2)+';'+$
           STRTRIM(strfil[nn].GRAV_C_ARCY,2)+';'+$
           STRTRIM(strfil[nn].GRAV_C_CAR_LAT,2)+';'+$
           STRTRIM(strfil[nn].GRAV_C_CAR_LON,2)+';'+$
           STRTRIM(strfil[nn].SAMPLECOUNT,2)+';'+$
           STRTRIM(strfil[nn].AREA,2)+';'+$
           STRTRIM(strfil[nn].MEAN_INT_RATIO,2)+';'+$
           STRTRIM(strfil[nn].BRARC_X_LL,2)+';'+$
           STRTRIM(strfil[nn].BRARC_Y_LL,2)+';'+$
           STRTRIM(strfil[nn].BRARC_X_UR,2)+';'+$
           STRTRIM(strfil[nn].BRARC_Y_UR,2)+';'+$
           STRTRIM(strfil[nn].BRPIX_X_LL,2)+';'+$
           STRTRIM(strfil[nn].BRPIX_Y_LL,2)+';'+$
           STRTRIM(strfil[nn].BRPIX_X_UR,2)+';'+$
           STRTRIM(strfil[nn].BRPIX_Y_UR,2)+';'+$
           STRTRIM(strfil[nn].FEAT_MAX_INT,2)+';'+$
           STRTRIM(strfil[nn].FEAT_MIN_INT,2)+';'+$
           STRTRIM(strfil[nn].FEAT_MEAN_INT,2)+';'+$
           STRTRIM(strfil[nn].ENC_MET,2)+';'+$
           STRTRIM(strfil[nn].COD_PIX_X,2)+';'+$
           STRTRIM(strfil[nn].COD_PIX_Y,2)+';'+$
           STRTRIM(strfil[nn].COD_ARC_X,2)+';'+$
           STRTRIM(strfil[nn].COD_ARC_Y,2)+';'+$
           STRTRIM(strfil[nn].SKE_LEN_DEG,2)+';'+$
           ;STRTRIM(strfil[nn].THICKNESS_PIX,2)+';'+$
           STRTRIM(strfil[nn].CURVATURE,2)+';'+$
           STRTRIM(strfil[nn].ELONG,2)+';'+$
           STRTRIM(strfil[nn].ORIENTATION,2)+';'+$
           STRTRIM(strfil[nn].COD_SKE_PIX_X,2)+';'+$
           STRTRIM(strfil[nn].COD_SKE_PIX_Y,2)+';'+$
           STRTRIM(strfil[nn].COD_SKE_ARC_X,2)+';'+$
           STRTRIM(strfil[nn].COD_SKE_ARC_Y,2)+';'+$
           STRTRIM(strfil[nn].CHAIN_CODE,2)+';'+$
           STRTRIM(strfil[nn].CCODE_LNTH,2)+';'+$
           STRTRIM(strfil[nn].CHAIN_CODE_SKE,2)+';'+$
           STRTRIM(strfil[nn].CCODE_SKE_LNTH,2)+';'+$
           ;STRTRIM(strfil[nn].RASTER_SCAN_LNTH,2)+';'+$           
           STRTRIM(strfil[nn].PP_LOCAL_FILENAME,2)+';';+$     
           ;STRTRIM(strfil[nn].EOR,2)+';'

       ENDFOR

           ;PRINTF,un,STRTRIM(strfil[0].EOF,2)+';'

       CLOSE,un
       FREE_LUN,un,/FORCE

    ;FILE *fi_ccode.txt----------------------------------------------------
    ; OPENW,un,idlfile5,/GET_LUN
    ; nele = N_ELEMENTS(strfil)
    ;
    ;      ;PRINTF,un,strobs.IND
    ;      ;PRINTF,un,strfil[0].NUM_FEAT
    ;
    ;   FOR nn = 0, nele-1 DO BEGIN
    ;      PRINTF,un,strobs.IND
    ;      PRINTF,un,strfil[0].NUM_FEAT
    ;      PRINTF,un,strfil[nn].IND_FEAT
    ;      PRINTF,un,strfil[nn].COD_PIX_X
    ;      PRINTF,un,strfil[nn].COD_PIX_Y
    ;      PRINTF,un,strfil[nn].COD_ARC_X
    ;      PRINTF,un,strfil[nn].COD_ARC_Y
    ;      PRINTF,un,strfil[nn].COD_SKE_PIX_X
    ;      PRINTF,un,strfil[nn].COD_SKE_PIX_Y
    ;      PRINTF,un,strfil[nn].COD_SKE_ARC_X
    ;      PRINTF,un,strfil[nn].COD_SKE_ARC_Y
    ;      PRINTF,un,strfil[nn].SKE_CHAIN
    ;      PRINTF,un,strfil[nn].BND_CHAIN     
    ;      PRINTF,un,strfil[nn].EOR
    ;   ENDFOR
    ;
    ;      PRINTF,un,strfil[0].EOF
    ;
    ; CLOSE,un



    ;--------------------------------------------------------
    ;Save the subimage for each filament using bounding rectangle


    ;####store using cdf file

    id = CDF_CREATE(idlfile6,/SINGLE_FILE,/clobber)
    
    FOR kk=0,numfil-1 DO BEGIN
        brpix_xll = strfil[kk].BRPIX_X_LL
        brpix_yll = strfil[kk].BRPIX_Y_LL
        brpix_xur = strfil[kk].BRPIX_X_UR
        brpix_yur = strfil[kk].BRPIX_Y_UR       
        subimage = corrected[brpix_xll:brpix_xur-1,brpix_yll:brpix_yur-1]
        name = 'sub'+STRTRIM(kk,2)
        zvar = CDF_VARCREATE(id,name,[1,1], $
               DIM=[(SIZE(subimage))[1],(SIZE(subimage))[2]])
        CDF_VARPUT,id,name,subimage
    ENDFOR
    
    CDF_CLOSE,id

;#############################################################





;#############################################################
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
;################################################################



  endloop:  
  ENDFOR


endf:
END
