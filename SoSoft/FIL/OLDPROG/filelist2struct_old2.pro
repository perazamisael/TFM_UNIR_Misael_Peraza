;@/home/fuller/IDL/FIL/struct_draw_fil.pro

PRO FILELIST2STRUCT

;From a standardize fits file list, saves 
;structure files describing the filaments
;detected in each file.
    
;-----------------------------------------------------------
;Define the structures


  STRUCT_OBS = {OBSERVATIONALPARAMETERS, $
                IND:0L, $
                OBSERVAT:'\N', $
                INSTRUME:'\N', $
                TELESCOP:'\N', $
                WAVELNTH:'\N', $
                WAVELNTH_NAME:'\N', $
                OBS_TYPE:'\N', $
                DATE_OBS:'\N', $
                DATE_END:'\N', $
                JDINT:0L, $
                JDFRAC:0D, $
                EXPTIME:'\N', $
                CARROT:0, $
                BITPIX:0, $
                NAXIS1:0, $
                NAXIS2:0, $
                CENTER_X:0D, $
                CENTER_Y:0D, $
                R_SUN:0D, $
                FILENAME:'\N', $
                CDELT1:0D, $
                CDELT2:0D, $ 
                UNITS:'\N', $
                QUALITY:'\N',$
                IMAGE_COVER:'\N', $
                IMAGE_COMP:'\N',$
                DATE_OBS_STRING:'\N',$
                DATE_END_STRING:'\N',$
                LOCAL_FILENAME:'\N',$
                BSCALE:0.,$
                BZERO:0.,$
                EOF:-999.999}

  STRUCT_PRO = {PROCESSINGPARAMETERS, $
                FEAT_TYPE:'\N', $
                LOCAL_FILE:'\N', $
                QSUNINT:0D, $
                INSTITUT:'\N', $
                CODE:'\N', $ 
                VERSION:'\N', $
                RUN_DATE:'\N', $
                NAXIS1_P:0, $
                NAXIS2_P:0, $
                CENTER_X_P:0D, $
                CENTER_Y_P:0D, $
                R_SUN_P:0D, $
                CDELT1_P:0D, $
                CDELT2_P:0D, $
                EOF:-999.999}

  STRUCT_FIL = {FEATUREPARAMETERS, $
                NUM_FEAT:0, $
                IND_FEAT:0, $
                GRAV_C_X:0D, $
                GRAV_C_Y:0D, $
                ;GRAV_C_HEL_LAT:0D, $
                ;GRAV_C_HEL_LON:0D, $
                GRAV_C_CAR_LAT:0D, $
                GRAV_C_CAR_LON:0D, $
                SAMPLECOUNT:0L, $
                AREA:0D, $ 
                MEAN_INT_RATIO:0D, $
                BRARC_X_LL:0D, $
                BRARC_Y_LL:0D, $
                BRARC_X_UR:0D, $
                BRARC_Y_UR:0D, $
                BRPIX_X_LL:0, $
                BRPIX_Y_LL:0, $
                BRPIX_X_UR:0, $
                BRPIX_Y_UR:0, $
                FEAT_MAX_INT:0D, $
                FEAT_MIN_INT:0D, $
                FEAT_MEAN_INT:0D, $
                ENC_MET:'\N', $
                COD_PIX_X:0, $
                COD_PIX_Y:0, $
                COD_ARC_X:0D, $
                COD_ARC_Y:0D, $
                SKE_LEN_DEG:0D, $
                CURVATURE:0D, $
                ELONG:0D, $
                ORIENTATION:0D, $
                COD_SKE_PIX_X:0, $
                COD_SKE_PIX_Y:0, $
                COD_SKE_ARC_X:0D, $
                COD_SKE_ARC_Y:0D, $
                SKE_CHAIN:'\N', $
                BND_CHAIN:'\N', $
                EOR:999.999, $
                EOF:-999.999}

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
    ;Read the original fits file header

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

    ;--------------------------------------------------------
    ;Define path and filenames to store the results
   
     spath = '/home/fuller/poub/SAV/STRUCT/'
     idlfile1 = spath + splitf3[0] + '_fil.sav'
     idlfile2 = spath + splitf3[0] + '_fi_init.txt'
     idlfile3 = spath + splitf3[0] + '_fi_norm.txt'
     idlfile4 = spath + splitf3[0] + '_fi_feat.txt'
     ;idlfile5 = spath + splitf3[0] + '_fi_ccode.txt'
     idlfile6 = spath + splitf3[0] + '_fi_image.cdf'


    ;--------------------------------------------------------
    ;Get obs time info from date_obs -> julian day

     val     = FXPAR(head,'DATE_OBS',COUNT=count)
     IF count GE 1 THEN date = STRTRIM(STRING(val),2) ELSE BEGIN
        PRINT,'no DATE_OBS data found'
        RETALL
     ENDIF
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
     val17 = FXPAR(head,'BSCALE'      ,COUNT=count16)
     val18 = FXPAR(head,'BZERO'       ,COUNT=count16)
     val19 = FXPAR(head,'EXPTIME'     ,COUNT=count19)
     val20 = FXPAR(head,'SOLROT_N'    ,COUNT=count20)
     val21 = FXPAR(head,'BITPIX'      ,COUNT=count21)
     val22 = FXPAR(head,'FILENAME'    ,COUNT=count22)
     val23 = FXPAR(head,'UNITS'       ,COUNT=count23)
     val24 = FXPAR(head,'QUALITY'     ,COUNT=count24)
     val25 = FXPAR(head,'IMAGE_COMP'  ,COUNT=count25)
     val26 = FXPAR(head,'IMAGE_COVER' ,COUNT=count26)


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
     IF count25 GE 1 THEN strobs.IMAGE_COMP    = STRING(val25) ELSE $ 
                          strobs.IMAGE_COMP    = 'SINGLE'
     IF count26 GE 1 THEN strobs.IMAGE_COVER   = STRING(val26) ELSE $
                          strobs.IMAGE_COVER   = 'FULL DISK'

    ;--------------------------------------------------------
    ;Other info 
                          strobs.DATE_OBS_STRING = strobs.DATE_OBS
                          strobs.DATE_END_STRING = strobs.DATE_END
                          strobs.LOCAL_FILENAME  = STRING(originalfname)
                          strobs.JDINT           = jdint
                          strobs.JDFRAC          = jdfrac


    ;--------------------------------------------------------
    ;Get info from the header of the preprocessed fits file

     strpro.NAXIS1_P        = FIX( FXPAR(headp,'NAXIS1'))
     strpro.NAXIS2_P        = FIX( FXPAR(headp,'NAXIS2'))
     strpro.R_SUN_P         = DOUBLE( FXPAR(headp,'SOLAR_R'))
     strpro.CDELT1_P        = DOUBLE( FXPAR(headp,'CDELT1'))
     strpro.CDELT2_P        = DOUBLE( FXPAR(headp,'CDELT2'))
     IF strpro.CDELT1_P EQ 0. OR strpro.CDELT2_P EQ 0. THEN BEGIN
        PRINT,'no cdelt info in std. fits header!!, using GET_SUN function'
        tmp = GET_SUN(strobs.DATE_OBS,SD=sd)
        strpro.CDELT1_P = sd/420.
        strpro.CDELT2_P = strpro.CDELT1_P ;(std. Sun is round)
     ENDIF
     strpro.CENTER_X_P      = DOUBLE( FXPAR(headp,'CENTER_X'))
     strpro.CENTER_Y_P      = DOUBLE( FXPAR(headp,'CENTER_Y'))
     strpro.FEAT_TYPE       = 'FIL'
     strpro.LOCAL_FILE      = ofile ;or gfile to have also the path
     strpro.INSTITUT        = 'Meudon Observatory'
     ;strpro.CODE            = ''
     ;strpro.VERSION         = ''
     strpro.RUN_DATE        = today_utc

     ;-------------------------------------------------------
     ;define type of obs to use the correct parameters

     info1=strobs.OBSERVAT
     info2=strobs.NAXIS1
     IF info1 EQ 'BBSO' THEN obst = 3
     IF info1 NE 'BBSO' AND FIX(info2) EQ 1500 THEN obst = 2
     IF info1 NE 'BBSO' AND FIX(info2) NE 1500 THEN obst = 1

    ;--------------------------------------------------------
    ;Image cleaning (dust lines, bckg normalisation)

     IF obst EQ 1 THEN corrected = CLEAN_IMAGE(TEMPORARY(arr),strpro.R_SUN_P,/MEUDON1)
     IF obst EQ 2 THEN corrected = CLEAN_IMAGE(TEMPORARY(arr),strpro.R_SUN_P,/MEUDON2)
     IF obst EQ 3 THEN corrected = CLEAN_IMAGE(TEMPORARY(arr),strpro.R_SUN_P,/BBSO)

    ;--------------------------------------------------------
    ;Compute the quiet sun intensity

     hist = MEDIAN(HISTOGRAM(corrected[WHERE(corrected)],min=0.),10)
     maxh = MAX(hist)
     wmax = ( WHERE(hist EQ maxh) )[0]
     strpro.QSUNINT = wmax


    ;--------------------------------------------------------
    ;Find the filaments

     IF obst EQ 1 THEN arrf = FILAMENT(INPUT=corrected,/MEUDON1)
     IF obst EQ 2 THEN arrf = FILAMENT(INPUT=corrected,/MEUDON2)
     IF obst EQ 3 THEN arrf = FILAMENT(INPUT=corrected,/BBSO)


    ;--------------------------------------------------------
    ;link close blobs and fill holes

;a placer dans le programme filament
    
     im = MORPH_CLOSE(TEMPORARY(arrf),REPLICATE(1,8,8))


    ;--------------------------------------------------------
    ;compute stat and geometry on each filament and fill the
    ;structure 

    strfil = FILL_STRUCT(WHERE(im),strfil,strpro.NAXIS1_P,strpro.NAXIS2_P, $
                         strpro.CDELT1_P,strpro.CDELT2_P,strpro.CENTER_X_P,$
                         strpro.CENTER_Y_P,strpro.R_SUN_P,strobs.DATE_OBS, $
                         strpro.QSUNINT,corrected)

    numfil = N_ELEMENTS(strfil)
    
    strfil[*].NUM_FEAT = numfil


    ;-----------------------------------------------------------
    ;assign a uniq ID to the file
    strobs.IND = RANDOMN(seed,/LONG)


    ;------------------------------------------------------------
    ;Save the 3 structures in an idl file

    SAVE,strobs,strpro,strfil,filename=idlfile1 



    ;------------------------------------------------------------------
    ;Write and save ascii files


    ;FILE *fi_init.txt-------------------------------------------------
     OPENW,un,idlfile2,/GET_LUN

          PRINTF,un,strobs.IND
          ;PRINTF,un,strobs.OBSERVAT
          ;PRINTF,un,strobs.INSTRUME
          ;PRINTF,un,strobs.TELESCOP
          ;PRINTF,un,strobs.WAVELNTH
          ;PRINTF,un,strobs.WAVELNTH_NAME
          ;PRINTF,un,strobs.OBS_TYPE
          ;PRINTF,un,strobs.UNITS
          ;PRINTF,un,strobs.IMAGE_COVER
          ;PRINTF,un,strobs.IMAGE_COMP
          PRINTF,un,strobs.DATE_OBS
          PRINTF,un,strobs.DATE_END
          PRINTF,un,strobs.JDINT
          PRINTF,un,strobs.JDFRAC
          PRINTF,un,strobs.EXPTIME
          PRINTF,un,strobs.CARROT
          PRINTF,un,strobs.BSCALE;
          PRINTF,un,strobs.BZERO;
          PRINTF,un,strobs.BITPIX
          PRINTF,un,strobs.NAXIS1
          PRINTF,un,strobs.NAXIS2
          PRINTF,un,strobs.R_SUN
          PRINTF,un,strobs.CENTER_X
          PRINTF,un,strobs.CENTER_Y
          PRINTF,un,strobs.CDELT1
          PRINTF,un,strobs.CDELT2
          PRINTF,un,strobs.QUALITY
          PRINTF,un,strobs.FILENAME;
          PRINTF,un,strobs.LOCAL_FILENAME;
          PRINTF,un,strobs.DATE_OBS_STRING;
          PRINTF,un,strobs.DATE_END_STRING;
          PRINTF,un,strobs.EOF

     CLOSE,un


    ;FILE *fi_norm.txt---------------------------------------------------
     OPENW,un,idlfile3,/GET_LUN

          PRINTF,un,strobs.IND
          PRINTF,un,strpro.FEAT_TYPE
          PRINTF,un,strpro.LOCAL_FILE
          PRINTF,un,strpro.QSUNINT
          PRINTF,un,strpro.INSTITUT
          PRINTF,un,strpro.CODE
          PRINTF,un,strpro.VERSION
          PRINTF,un,strpro.RUN_DATE
          PRINTF,un,strpro.NAXIS1_P
          PRINTF,un,strpro.NAXIS2_P
          PRINTF,un,strpro.CENTER_X_P
          PRINTF,un,strpro.CENTER_Y_P
          PRINTF,un,strpro.R_SUN_P
          PRINTF,un,strpro.CDELT1_P
          PRINTF,un,strpro.CDELT2_P
          PRINTF,un,strpro.EOF

     CLOSE,un



    ;FILE *fi_feat.txt---------------------------------------------------
     OPENW,un,idlfile4,/GET_LUN
     nele = N_ELEMENTS(strfil)

          ;PRINTF,un,strobs.IND
          ;PRINTF,un,strfil[0].NUM_FEAT

       FOR nn = 0, nele-1 DO BEGIN
          PRINTF,un,strobs.IND
          PRINTF,un,strfil[0].NUM_FEAT
          PRINTF,un,strfil[nn].IND_FEAT
          PRINTF,un,strfil[nn].GRAV_C_X
          PRINTF,un,strfil[nn].GRAV_C_Y
          PRINTF,un,strfil[nn].GRAV_C_CAR_LAT
          PRINTF,un,strfil[nn].GRAV_C_CAR_LON
          PRINTF,un,strfil[nn].SAMPLECOUNT
          PRINTF,un,strfil[nn].AREA
          PRINTF,un,strfil[nn].MEAN_INT_RATIO
          PRINTF,un,strfil[nn].BRARC_X_LL
          PRINTF,un,strfil[nn].BRARC_Y_LL
          PRINTF,un,strfil[nn].BRARC_X_UR
          PRINTF,un,strfil[nn].BRARC_Y_UR
          PRINTF,un,strfil[nn].BRPIX_X_LL
          PRINTF,un,strfil[nn].BRPIX_Y_LL
          PRINTF,un,strfil[nn].BRPIX_X_UR
          PRINTF,un,strfil[nn].BRPIX_Y_UR
          PRINTF,un,strfil[nn].FEAT_MAX_INT
          PRINTF,un,strfil[nn].FEAT_MIN_INT
          PRINTF,un,strfil[nn].FEAT_MEAN_INT
          PRINTF,un,strfil[nn].ENC_MET
          PRINTF,un,strfil[nn].SKE_LEN_DEG
          PRINTF,un,strfil[nn].CURVATURE
          PRINTF,un,strfil[nn].ELONG
          PRINTF,un,strfil[nn].ORIENTATION
          ;PRINTF,un,strfil[nn].COD_PIX_X
          ;PRINTF,un,strfil[nn].COD_PIX_Y
          ;PRINTF,un,strfil[nn].COD_ARC_X
          ;PRINTF,un,strfil[nn].COD_ARC_Y
          ;PRINTF,un,strfil[nn].COD_SKE_PIX_X
          ;PRINTF,un,strfil[nn].COD_SKE_PIX_Y
          ;PRINTF,un,strfil[nn].COD_SKE_ARC_X
          ;PRINTF,un,strfil[nn].COD_SKE_ARC_Y
          ;PRINTF,un,strfil[nn].SKE_CHAIN
          ;PRINTF,un,strfil[nn].BND_CHAIN     
          PRINTF,un,strfil[nn].EOR
       ENDFOR

          PRINTF,un,strfil[0].EOF

     CLOSE,un

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
               DIM=[(SIZE(subimage))(1),(SIZE(subimage))(2)])
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
