;##########################################################
; MAIN PROGRAM FOR FILAMENT AUTOMATED RECOGNITION AND
; DESCRIPTION; THIS COPY IS PARAMETRIZED FOR AUTOMATED
; PROCESSING ON LESIA04
;##########################################################

;+
; NAME:
;       efr_fil2ascii_auto / SoSoFT / version for crontab
;
; PURPOSE:
;
;
; AUTHOR:
;
;       Fuller Nicolas
;       LESIA / POLE SOLAIRE
;       Observatoire de MEUDON
;       5 place Jules Janssen
;       92190 MEUDON
;       E-mail: nicolas.fuller@obspm.fr
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       and HELIO (Heliophysics Integrated Observatory) HFC
;
; CALLING SEQUENCE:
;
;       .r efr_fil2ascii_auto (within SolarSoft environment)
;
; INPUTS:
;    
;         The normalized FITS observations to be computed
;         This FITS file should have the following specifications
;         - filename: name of the original observations +'_subtract_processed.fits'
;           ex: mh100114.112200_subtract_processed.fits
;         - the size of image is 1024*1024 or 2048*2048
;         - the Sun disk is normalized to a round shape with
;           sun radius = 420px (or 840), centered at  (511.5,511.5)
;           or (1023.5,1023.5), background set to 0 value
;         - HISTORY part of fits header should contain information
;           about original file and ellipse fitting parameters
;         - You can use the EGSO dedicated software to fullfil all these requirements
;       
; OUTPUTS:
;
;       4 ascii files with filenames like [progname]_[version]_[dateTtime]_[obs]_[content].csv
;       Examples:
;       sosoft_10_20120219T180000_meu_obs.csv [original observation]
;       sosoft_10_20120219T180000_meu_pp.csv   [preprocessed image]
;       sosoft_10_20120219T180000_meu_feat.csv [detected filaments]
;       sosoft_10_20120219T180000_meu_frc.csv  [feature recognition code]
;
;        An optionnal Jpeg file which summarize the processing steps
;        and results
;        
;       A quicklook png of the original preprocessed observation
;
; MODIFICATION HISTORY:
;
;    NF 2004
;    NF 2010 small corrections for HELIO
;    NF 2010 big update of the structure parameters and output files
;    NF 2011/01 Display option, efr_remove_sunspot
;    NF 2012/03 update parameters list + new ones, new ascii filenames, include header in ascii
;    files -> V1.0 of SoSoFT
;    NF 2013 add /NOSCALE keyword to READFITS (see Meudon Solar tower
;    stackinf observations which could be unsigned integer with bscale
;    set to 1
;    NF 2013 add coimbra and Meudon solar tower (classical and stacking)
;    NF 2013 add LINEID header keyword for Meudon Solar tower stacking images
;    NF 2014 add png quicklook of the FITS file and change fits filename in its directory
;    NF 2014 Put configuration parameters in an external proc
;-

;----------------------------------------------------------
; Execute configuration file
;----------------------------------------------------------

@efr_fil2ascii_config_auto.pro

;----------------------------------------------------------
; Other parameters
;----------------------------------------------------------

    ;-------------------------------------------------------
    ; Cleaning Code Information
    ;-------------------------------------------------------
    INSTITUT         = 'OBSPM' ;Institute responsible for running the cleaning code
    CODE              = 'ANA' ;Name of the cleaning code
    VERSION         = '1.0' ;Version of the cleaning code
    PERSON          = 'Sylvain Cnudde' ;Person responsible for running cleaning code
    CONTACT        = 'nicolas.fuller@obspm.fr' ;Contact email

    ;-------------------------------------------------------
    ; Feature Recognition Code Information
    ;-------------------------------------------------------
    FRC_INSTITUT              = 'OBSPM' ;Institute responsible for running the FR code
    FRC_CODE                   = 'SoSoFT' ;Name of the code
    FRC_VERSION              = '1.0' ;Version of the code
    FRC_PERSON               = 'Nicolas Fuller' ;Person responsible for the feature recognition code
    FRC_CONTACT             = 'nicolas.fuller@obspm.fr' ;Contact email
    FRC_ENC_MET             = 'CHAINCODE'
    FRC_FEATURE_NAME  = 'FILAMENT'
    FRC_REFERENCE        = '2005SoPh..227...61F & previ.obspm.fr/articles/SoSoftSoSoProESWW8.pdf'

;----------------------------------------------------------------------------------
;    END OF CONFIGURATION
;----------------------------------------------------------------------------------



;----------------------------------------------------------
; Not use DIALOG_PICKFILE below but 'ls' instead
; to compute all the files in the directory
;----------------------------------------------------------

IF DIALOG EQ 0 THEN BEGIN

  command = 'ls '+ppath+'*processed*'
  SPAWN,command,filenames
  IF filenames[0] EQ '' THEN BEGIN
      PRINT,'---------------------------------------------------------------'
      PRINT, 'Did not find any file in '+STRTRIM(ppath,2)
      PRINT,'---------------------------------------------------------------'
      RETALL
  ENDIF
  nbj = N_ELEMENTS(filenames)
  PRINT, STRTRIM(nbj,2)+" files found in "+STRTRIM(ppath,2)

ENDIF ELSE BEGIN
;-----------------------------------------------------------
; Get the processed filenames to compute and number of
; selected files
;-----------------------------------------------------------

  filenames = DIALOG_PICKFILE(PATH=ppath, /multiple_files) 
  IF filenames[0] EQ '' THEN RETALL
  nbj = N_ELEMENTS(filenames)

ENDELSE

;----------------------------------------------------------
; display issues
;----------------------------------------------------------
 DEVICE,DECOMPOSED=0,RETAIN=2

;If display = 0 and jpeg_output = 1, use pixmap method
;(pixmap is used to create a jpeg output without displaying the window)
PIXMAP_method = 0
IF DISPLAY_res NE 1 AND JPEG_output EQ 1 THEN PIXMAP_method = 1 


;----------------------------------------------------------
; Define the structures to store parameters from
; header and computed ones.
;----------------------------------------------------------

  default = '\N'

  STRUCT_OBS = {OBSERVATIONALPARAMETERS, $
                OBSERVAT:            default, $
                INSTRUME:             default, $
                TELESCOP:            default, $
                UNITS:                    default, $
                WAVEMIN:              default, $
                WAVEMAX:             default, $
                WAVENAME:           default, $
                WAVEUNIT:             default, $
                OBS_TYPE:             default, $
                DATE_OBS:             default, $
                DATE_END:             default, $
                JDINT:                     default, $
                JDFRAC:                 default, $
                EXP_TIME:              default, $
                C_ROTATION:          default, $
                BSCALE:                 default, $
                BZERO:                    default, $
                BITPIX:                    default, $
                NAXIS1:                   default, $
                NAXIS2:                   default, $
                R_SUN:                    default, $
                CENTER_X:              default, $
                CENTER_Y:              default, $
                CDELT1:                  default, $
                CDELT2:                  default, $ 
                QUALITY:                 default, $
                FILENAME:              default, $
                COMMENT:              default, $
                DENSITY:                default, $
                SPECTRAL_NAME:  default, $
                LOC_FILENAME:      default}

  TAG_LIST_OBS = TAG_NAMES(STRUCT_OBS)
		
  STRUCT_PRO = {PROCESSINGPARAMETERS, $
                RUN_DATE:          default, $
                LOC_FILE:           default, $
                EL_CEN_X:          default, $
                EL_CEN_Y:          default, $
                EL_AXIS1:            default, $
                EL_AXIS2:            default, $
                EL_ANGLE:          default, $
                STDEV:                default, $
                STDEVGEO:         default, $
                ALGERR:              default, $                
                CDELT1:               default, $
                CDELT2:               default, $
                BITPIX:                 default, $
                QSUN_INT:            default, $
                PR_LOCFNAME:    default, $
                ORG_FNAME:        default,$
                INSTITUT:              default, $
                CODE:                  default, $ 
                PERSON:              default, $
                VERSION:             default, $
                CONTACT:            default, $
                EFIT:                    default, $
                STANDARD:          default, $
                BACKGROUND:     default, $
                LINECLEAN:          default, $
                LINEC_MAIND:      default, $
                LIMBDARK:           default, $
                PERCENT:            default, $
                NAXIS1:                default, $
                NAXIS2:                default, $
                CENTER_X:          default, $
                CENTER_Y:          default, $
                R_SUN:                default, $
                DIVISION:            default, $
                INORM:                default}

  TAG_LIST_PRO = TAG_NAMES(STRUCT_PRO)

  STRUCT_FRC = {FEAT_REC_CODE_PARAMETERS, $

                INSTITUT:            default, $
                CODE:                 default, $
                VERSION:            default, $
                PERSON:             default, $
                CONTACT:           default, $
                ENC_MET:           default, $
                FEATURE_NAME: default, $
                REFERENCE:       default }

  TAG_LIST_FRC = TAG_NAMES(STRUCT_FRC)

  STRUCT_FIL = {FEATUREPARAMETERS, $
                FEAT_X_ARCSEC:           default,$ ;'X Heliocentric coordinates of the filament skeleton gravity centre in arcsec'
                FEAT_Y_ARCSEC:           default,$ ;'Y Heliocentric coordinates of the filament skeleton gravity centre in arcsec'
                FEAT_X_PIX:                   default,$ ;'X image coordinates of the filament skeleton gravity centre in pixels'
                FEAT_Y_PIX:                   default,$ ;'Y image coordinates of the filament skeleton gravity centre in pixels'
                FEAT_HG_LONG_DEG:    default,$ ;'Heliographic longitude of the filament skeleton gravity centre in pixels'
                FEAT_HG_LAT_DEG:       default,$ ;'Heliographic latitude of the filament skeleton gravity centre in pixels'
                FEAT_CARR_LONG_DEG:default,$ ;'Carrington longitude of the filament skeleton gravity centre in pixels'
                FEAT_CARR_LAT_DEG:   default,$ ;'Carrington latitude of the filament skeleton gravity centre in pixels'
                BR_X0_ARCSEC:             default,$ ; 'Bounding rectangle X heliocentric coordinate South East most point in arcsec'
                BR_Y0_ARCSEC:             default,$ ; 'Bounding rectangle Y heliocentric coordinate South East most point in arcsec'
                BR_X1_ARCSEC:             default,$ ; 'Bounding rectangle X heliocentric coordinate North East most point in arcsec'
                BR_Y1_ARCSEC:             default,$ ; 'Bounding rectangle Y heliocentric coordinate North East most point in arcsec'
                BR_X2_ARCSEC:             default,$ ; 'Bounding rectangle X heliocentric coordinate South West most point in arcsec'
                BR_Y2_ARCSEC:             default,$ ; 'Bounding rectangle Y heliocentric coordinate South West most point in arcsec'
                BR_X3_ARCSEC:             default,$ ; 'Bounding rectangle X heliocentric coordinate North West most point in arcsec'
                BR_Y3_ARCSEC:             default,$ ; 'Bounding rectangle Y heliocentric coordinate North West most point in arcsec'
                BR_X0_PIX:                     default,$ ; 'Bounding rectangle X heliocentric coordinate South East most point in pixels'
                BR_Y0_PIX:                     default,$ ; 'Bounding rectangle Y heliocentric coordinate South East most point in pixels'
                BR_X1_PIX:                     default,$ ; 'Bounding rectangle X heliocentric coordinate North East most point in pixels'
                BR_Y1_PIX:                     default,$ ; 'Bounding rectangle Y heliocentric coordinate North East most point in pixels'
                BR_X2_PIX:                     default,$ ; 'Bounding rectangle X heliocentric coordinate South West most point in pixels'
                BR_Y2_PIX:                     default,$ ; 'Bounding rectangle Y heliocentric coordinate South West most point in pixels'
                BR_X3_PIX:                     default,$ ; 'Bounding rectangle X heliocentric coordinate North West most point in pixels'
                BR_Y3_PIX:                     default,$ ; 'Bounding rectangle Y heliocentric coordinate North West most point in  pixels'
                BR_HG_LONG0_DEG:      default,$ ; 'Bounding rectangle X heliographic coordinate South East most point in degrees'
                BR_HG_LAT0_DEG:         default,$ ; 'Bounding rectangle Y heliographic coordinate South East most point in degrees'
                BR_HG_LONG1_DEG:      default,$ ; 'Bounding rectangle X heliographic coordinate North East most point in degrees'
                BR_HG_LAT1_DEG:         default,$ ; 'Bounding rectangle Y heliographic coordinate North East most point in degrees'
                BR_HG_LONG2_DEG:      default,$ ; 'Bounding rectangle X heliographic coordinate South West most point in degrees'
                BR_HG_LAT2_DEG:         default,$ ; 'Bounding rectangle Y heliographic coordinate South West most point in degrees'
                BR_HG_LONG3_DEG:      default,$ ; 'Bounding rectangle X heliographic coordinate North West most point in degrees'
                BR_HG_LAT3_DEG:         default,$ ; 'Bounding rectangle Y heliographic coordinate North West most point in degrees'
                BR_CARR_LONG0_DEG:  default,$ ; 'Bounding rectangle X carrington coordinate South East most point in degrees'
                BR_CARR_LAT0_DEG:     default,$ ; 'Bounding rectangle Y carrington coordinate South East most point in degrees'
                BR_CARR_LONG1_DEG:  default,$ ; 'Bounding rectangle X carrington coordinate North East most point in degrees'
                BR_CARR_LAT1_DEG:     default,$ ; 'Bounding rectangle Y carrington coordinate North East most point in degrees'
                BR_CARR_LONG2_DEG:  default,$ ; 'Bounding rectangle X carrington coordinate South West most point in degrees'
                BR_CARR_LAT2_DEG:     default,$ ; 'Bounding rectangle Y carrington coordinate South West most point in degrees'
                BR_CARR_LONG3_DEG:  default,$ ; 'Bounding rectangle X carrington coordinate North West most point in degrees'
                BR_CARR_LAT3_DEG:     default,$ ; 'Bounding rectangle Y carrington coordinate North West most point in degrees'
                FEAT_AREA_PIX:            default, $ ;'number of pixels included in the filament'
                FEAT_AREA_MM2:           default, $ ;'Area in Mm2 of  the filament'
                FEAT_AREA_DEG2:         default, $ ;'Area of the filament in square degrees'
                FEAT_MEAN2QSUN:        default, $ ;'Mean of the Filament to QS instensity ratio'
                FEAT_MAX_INT:              default, $ ;Filament max. intensity value, in units of the original observation
                FEAT_MIN_INT:               default, $ ;Filament min. intensity value, in units of the original observation
                FEAT_MEAN_INT:            default, $ ;Filament mean intensity value, in units of the original observation
                CC_X_PIX:                      default, $ ;'X coordinate of chain code start position in pixels
                CC_Y_PIX:                      default, $  ;Y coordinate of chain code start position in pixels
                CC_X_ARCSEC:              default, $ ;X coordinate of chain code start position in arcsec
                CC_Y_ARCSEC:              default, $ ;Y coordinate of chain code start position in arcsec
                SKE_LENGTH_DEG:        default, $ ;'Length of the filament skeleton in degrees'
                SKE_CURVATURE:          default, $;'Index of curvature of the skeleton'
                FEAT_ELONG:                 default, $;'Elongation factor'
                SKE_ORIENTATION:        default, $ ;'Orientation of the filament skeleton'
                SKE_CC_X_PIX:             default, $ ;'X coordinate of skeleton chain code start in pixels
                SKE_CC_Y_PIX:             default, $; 'Y coordinate of skeleton chain code start in pixels
                SKE_CC_X_ARCSEC:     default, $;X coordinate of skeleton chain code start in arcsec'
                SKE_CC_Y_ARCSEC:     default, $;Y coordinate of skeleton chain code start in arcsec'
                CC:                                 default,$ ;boundary chain code
                SKE_CC:                        default,$ ;skeleton chain code
                CC_LENGTH:                  default,$ ;boundary chain code length
                SKE_CC_LENGTH:         default,$ ;skeleton chain code length
                PR_LOCFNAME:            default, $;'Name of the pre processed image used to perform detection
                FEAT_FILENAME:          default,$; 'Name of the data file used to fill the FILAMENTS table',
                RUN_DATE:                   default};date when the FR code was run
 
  TAG_LIST_FIL = TAG_NAMES(STRUCT_FIL)

;-----------------------------------------------------------
; Main loop on every file
;-----------------------------------------------------------

  FOR ii = 0, nbj-1 DO BEGIN


    ;-------------------------------------------------------
    ; Create instances of the structures for this file
    ;-------------------------------------------------------

    strobs   = STRUCT_OBS
    strpro   = STRUCT_PRO
    strfil      = STRUCT_FIL
    strfrc    = STRUCT_FRC

    ;-------------------------------------------------------
    ; Cleaning Code Information
    ;-------------------------------------------------------

    strpro.INSTITUT   = INSTITUT
    strpro.CODE        = CODE
    strpro.VERSION   = VERSION
    strpro.CONTACT  = CONTACT
    strpro.PERSON    = PERSON

     ;-------------------------------------------------------
    ; Feature Recognition Code Information
    ;-------------------------------------------------------
    strfrc.INSTITUT              = FRC_INSTITUT
    strfrc.CODE                  = FRC_CODE
    strfrc.VERSION             = FRC_VERSION
    strfrc.PERSON              = FRC_PERSON
    strfrc.CONTACT            = FRC_CONTACT
    strfrc.ENC_MET            = FRC_ENC_MET
    strfrc.FEATURE_NAME  = FRC_FEATURE_NAME
    strfrc.REFERENCE        = FRC_REFERENCE

    ;-------------------------------------------------------
    ; Read the processed fits file (image and header)
    ;-------------------------------------------------------
     gfile = filenames[ii]
     PRINT,'##################################################################################'
     PRINT,'Processing ',gfile
     PRINT,'##################################################################################'
     PRINT,' '
     arr = READFITS(gfile,headp,/NOSCALE)


    ;-------------------------------------------------------
    ; Filenames fields
    ;-------------------------------------------------------
     
     pfile = FILE_BASENAME(gfile)
     splitf2 = STRSPLIT(pfile,'_subtract_processed',/EXTRACT,/REGEX,COUNT=cc)
     IF cc LT 2 THEN BEGIN
        PRINT,'***********************************************************************************'
        PRINT,'Warning: the name of the preprocessed file does not match the required format'
        PRINT,'***********************************************************************************'
        splitf2 = [REFORM(splitf2),'']
     ENDIF


     ;name of the local preprocessed file
     pr_locfname = pfile
     ;name of the original file (as extracted from PP filename)
     org_fname = splitf2[0]+splitf2[1]
     ;name of the local preprocessed file, with local path
     loc_file = gfile
     ;name of the original file with path (as extracted from PP filename)
     loc_filename = opath + org_fname


    ;-------------------------------------------------------
    ; Get Solar parameters from the HISTORY of header
    ;-------------------------------------------------------
     ;(field name in history part = comment in first part of header)

     hist  = FXPAR(headp,'HISTORY' ,COUNT=cch)
     histb= hist
     com = STRARR(7)
     vals  = STRARR(7)
     h01  = FXPAR(headp,'NAXIS1'  ,COUNT=cc01, COMM = com0)
     h02  = FXPAR(headp,'NAXIS2'  ,COUNT=cc02, COMM = com1)
     h03  = FXPAR(headp,'CDELT1'  ,COUNT=cc03, COMM = com2)
     h04  = FXPAR(headp,'CDELT2'  ,COUNT=cc04, COMM = com3)
     h05  = FXPAR(headp,'CENTER_X',COUNT=cc05, COMM = com4)
     h06  = FXPAR(headp,'CENTER_Y',COUNT=cc06, COMM = com5)
     h07  = FXPAR(headp,'SOLAR_R' ,COUNT=cc07, COMM = com6) 
     com[0] = com0
     com[1] = com1
     com[2] = com2
     com[3] = com3
     com[4] = com4
     com[5] = com5
     com[6] = com6
    ;for each comment find the corresponding value in history
     FOR aa = 0,cch-1 DO hist[aa]  = STRJOIN(STRSPLIT(hist[aa],'/:',/EXTRACT))
     FOR bb = 0,6 DO BEGIN
           IF STRLEN(com[bb]) GT 4 THEN BEGIN
            comm  = '*'+STRTRIM(STRJOIN(STRSPLIT(com[bb],'/:',/EXTRACT)),2)+'*'   
            wwma  = (WHERE(STRMATCH(hist,comm,/FOLD_CASE) EQ 1))[0]
            splt  = STRSPLIT(histb[wwma],/EXTRACT,COUNT=nsplt)
            vals[bb] = splt[nsplt-1]
           ENDIF
     ENDFOR 


    ;-------------------------------------------------------
    ; Get Ellipse parameters from the HISTORY of header
    ;-------------------------------------------------------

      str = '*Ellipse Center*'
      wwma  = (WHERE(STRMATCH(histb,str,/FOLD_CASE) EQ 1))[0]
      splt  = STRSPLIT(histb[wwma],/EXTRACT,COUNT=nsplt)
      val109 = STRSPLIT(splt[nsplt-2],',',/EXTR)
      val110 = STRSPLIT(splt[nsplt-1],',',/EXTR)

      str = '*Ellipse Angle*'
      wwma  = (WHERE(STRMATCH(histb,str,/FOLD_CASE) EQ 1))[0]
      splt  = STRSPLIT(histb[wwma],/EXTRACT,COUNT=nsplt)
      val111 = STRSPLIT(splt[nsplt-1],',',/EXTR)

      str = '*Ellipse Axis*'
      wwma  = (WHERE(STRMATCH(histb,str,/FOLD_CASE) EQ 1))[0]
      splt  = STRSPLIT(histb[wwma],/EXTRACT,COUNT=nsplt)
      val112 = STRSPLIT(splt[nsplt-2],',',/EXTR)
      val113 = STRSPLIT(splt[nsplt-1],',',/EXTR)

      str = '*Ellipse Fit Deviation*'
      wwma  = (WHERE(STRMATCH(histb,str,/FOLD_CASE) EQ 1))[0]
      splt  = STRSPLIT(histb[wwma],/EXTRACT,COUNT=nsplt)
      val114 = STRSPLIT(splt[nsplt-1],',',/EXTR)


    ;-------------------------------------------------------
    ; Get parameters from the header of the processed file
    ;-------------------------------------------------------

     val101 = FXPAR(headp,'NAXIS1'      ,COUNT=count101)
     val102 = FXPAR(headp,'NAXIS2'      ,COUNT=count102)
     val103 = FXPAR(headp,'CDELT1'      ,COUNT=count103)
     val104 = FXPAR(headp,'CDELT2'      ,COUNT=count104)
     val105 = FXPAR(headp,'CENTER_X'    ,COUNT=count105)
     val106 = FXPAR(headp,'CENTER_Y'    ,COUNT=count106)
     val107 = FXPAR(headp,'SOLAR_R'     ,COUNT=count107)  
     val108 = FXPAR(headp,'DATE'        ,COUNT=count108)
     val08  = FXPAR(headp,'INSTITUT'     ,COUNT=count08)
     val09  = FXPAR(headp,'ORIGIN'       ,COUNT=count09)
     val10  = FXPAR(headp,'INSTRUME'     ,COUNT=count10)
     val11  = FXPAR(headp,'TELESCOP'     ,COUNT=count11)
     val12  = FXPAR(headp,'WAVELNTH'     ,COUNT=count12)
     val13  = FXPAR(headp,'WAVELNTH_NAME',COUNT=count13)
     val13b = FXPAR(headp,'WAVEUNIT'     ,COUNT=count13b)
     val14  = FXPAR(headp,'OBS_TYPE'     ,COUNT=count14)
     val15  = FXPAR(headp,'DATE_OBS'     ,COUNT=count15)
     val16  = FXPAR(headp,'DATE_END'     ,COUNT=count16)
     val17  = FXPAR(headp,'BSCALE'       ,COUNT=count17)
     val18  = FXPAR(headp,'BZERO'        ,COUNT=count18)
     val19  = FXPAR(headp,'EXPTIME'      ,COUNT=count19)
     val20  = FXPAR(headp,'SOLROT_N'     ,COUNT=count20)
     val21  = FXPAR(headp,'BITPIX'       ,COUNT=count21)
     val22  = FXPAR(headp,'FILENAME'     ,COUNT=count22)
     val23  = FXPAR(headp,'UNITS'        ,COUNT=count23)
     val24  = FXPAR(headp,'QUALITY'      ,COUNT=count24)
     val27  = FXPAR(headp,'COMMENT'      ,COUNT=count27)
     val28  = FXPAR(headp,'DATE-OBS'     ,COUNT=count28)
     val29  = FXPAR(headp,'TIME-OBS'     ,COUNT=count29)
     val30  = FXPAR(headp,'TIME_OBS'     ,COUNT=count30)
     val31  = FXPAR(headp,'SPECTRAL_NAME' ,COUNT=count31)
     val32  = FXPAR(headp,'LINEID'           ,COUNT=count32)

     ;------------------------------------------------------------------------
     ; if there is more than one comment, whole line is extracted
     ;and comments are concatenated like in this example (meu3)
     ;= 'H ALPHA'            /(char) = 'P,B in degrees; R in arcsec' /(char)
     ;Here we separate the different comments properly to get:
     ;H ALPHA & P,B in degrees & R in arcsec
     ;------------------------------------------------------------------------
     IF count27 GT 1 THEN BEGIN
        fcom = ' '
        FOR pp = 0,count27-1 DO BEGIN
           splitcom = STRSPLIT(val27[pp],'=/',/EXTRACT,COUNT=ccom)
           IF ccom LT 2 OR ccom GT 3 THEN BEGIN
              fcom=fcom
           ENDIF ELSE BEGIN
              IF ccom EQ 2 THEN fcom = [fcom,splitcom[0]]
              IF ccom EQ 3 THEN fcom = [fcom,splitcom[1]]
           ENDELSE
        ENDFOR
        nfcom = N_ELEMENTS(fcom)
        IF nfcom GT 1 THEN BEGIN
           FOR pp2=0,nfcom-1 DO BEGIN
                 fcom[pp2] = STRTRIM(fcom[pp2],2)
                 IF STRPOS(fcom[pp2],';') GT 0 THEN BEGIN
                    fcom[pp2] = STRJOIN(STRSPLIT(fcom[pp2],';',/EXTRACT),' & ')
                 ENDIF
                 IF STRPOS(fcom[pp2],"'") NE -1 THEN BEGIN
                    fcom[pp2] = STRJOIN(STRSPLIT(fcom[pp2],"'",/EXTRACT),',')
                 ENDIF
           ENDFOR
           fcom = STRJOIN(fcom[1:nfcom-1],' & ')
        ENDIF
        val27 = fcom
        PRINT,'modified COMMENT value in header------------------'
        PRINT,fcom
     ENDIF


     ;------------------------------------------------------------------------
     ; if filename is in header, correction of the extracted filenames
     ;------------------------------------------------------------------------
     IF count22 GE 1 THEN BEGIN
        ;name of the original file
        org_fname = val22
        ;name of the original file with path
        loc_filename = opath + org_fname
     ENDIF

    ;-------------------------------------------------------
    ; Try to find a complete date from header values
    ; Solve particular case of DATE_/-OBS and TIME_/-OBS
    ; If DATE-OBS='YYYY-MM-DD' and TIME-OBS='HH:MM:SS'
    ; Then DATE-OBS=DATE-OBS+T+TIME-OBS
    ;-------------------------------------------------------

     IF count15 EQ 0 THEN BEGIN
        IF count28 EQ 0 THEN BEGIN
          PRINT,'No observation date found (no DATE_OBS or DATE-OBS in header), returning...'
          RETALL
        ENDIF ELSE BEGIN
          val15 = val28
          count15 = 1
        ENDELSE 
     ENDIF

     ;datef will be part of the ascii output filenames
     datef = ''

     IF STRLEN(val15) GE 15 THEN BEGIN
     ;usually something like 2002-02-09T19:32:00.000Z
          datef0 = STRJOIN(STRSPLIT(val15,'[- :T]',/REGEX,/EXTRACT))
          datef = STRMID(datef0,0,8)+'T'+STRMID(datef0,8,6)
     ENDIF ELSE BEGIN
        ;try to compose the full date from date and time 
        IF count30 NE 0 THEN BEGIN
           val15 = STRTRIM(val15,2)+'T'+STRTRIM(val30,2)
           datef = STRJOIN(STRSPLIT(val15,'[- :]',/REGEX,/EXTRACT))
           datef = STRMID(datef,0,15)
        ENDIF
        IF count29 NE 0 THEN BEGIN
           val15 = STRTRIM(val15,2)+'T'+STRTRIM(val29,2)
           datef = STRJOIN(STRSPLIT(val15,'[- :]',/REGEX,/EXTRACT))
           datef = STRMID(datef,0,15)
        ENDIF
     ENDELSE

     IF STRLEN(datef) NE 15 THEN BEGIN
         PRINT,'No observation date found, returning...'
         RETALL
      ENDIF
  
    ;-------------------------------------------------------
    ; For uniformity of the historical observations
    ; database we overwrite some parameters which
    ; are found (or not) in the header
    ;-------------------------------------------------------
    ;### For Meudon Observatory (but not the solar tower)

    IF (STRPOS(STRLOWCASE(val08),'meudon') NE -1 OR STRPOS(STRLOWCASE(val08),'paris') NE -1  OR $
        STRPOS(STRLOWCASE(val09),'meudon') NE -1 OR STRPOS(STRLOWCASE(val09),'paris') NE -1 ) AND $
        STRPOS(STRLOWCASE(STRTRIM(STRING(val11),2)),'tower') EQ -1 $
        THEN BEGIN
              count09 = 0                            ;ORIGIN
              count11 = 0                            ;TELESCOP
              count27 = 0                            ;COMMENT
              val08 = 'Meudon'                     ;OBSERVAT
              val10 = 'Spectroheliograph'      ;INSTRUME
              val13 = 'Halpha'                      ;WAVENAME
              val12  = '656.3'                       ;WAVEMIN / WAVEMAX
              val13b = 'nm'                          ;WAVEUNIT -> nm
              val31 = 'visible'                       ;SPECTRAL_NAME
              val14 = 'remote sensing'         ;OBS_TYPE
              val23 = 'counts'                      ;UNITS
              count08 = 1 & count10 = 1 & count13 = 1 & count12 = 1
              count13b = 1 & count31 = 1 & count14 = 1 & count23 = 1
     ENDIF      
     ;#### Other observatory parameters could be included here...


    ;-------------------------------------------------------
    ; Fill strobs structure with header parameters
    ;-------------------------------------------------------

     IF vals[0] NE ' ' AND FIX(vals[0]) NE 0 THEN strobs.NAXIS1         = FIX(vals[0])
     IF vals[1] NE ' ' AND FIX(vals[1]) NE 0 THEN strobs.NAXIS2         = FIX(vals[1])
     IF vals[2] NE ' ' AND FLOAT(vals[2]) NE 0. THEN strobs.CDELT1  = DOUBLE(vals[2])
     IF vals[3] NE ' ' AND FLOAT(vals[3]) NE 0. THEN strobs.CDELT2  = DOUBLE(vals[3])
     IF vals[4] NE ' ' AND FIX(vals[4]) NE 0 THEN strobs.CENTER_X   = DOUBLE(vals[4])
     IF vals[5] NE ' ' AND FIX(vals[5]) NE 0 THEN strobs.CENTER_Y   = DOUBLE(vals[5])
     IF vals[6] NE ' ' AND FIX(vals[6]) NE 0 THEN strobs.R_SUN         = DOUBLE(vals[6])  
     IF count08 GE 1   THEN strobs.OBSERVAT     = STRING(val08)
     IF count09 GE 1   THEN strobs.OBSERVAT     = STRING(val09)
     IF count10 GE 1   THEN strobs.INSTRUME      = STRING(val10)
     IF count11 GE 1   THEN strobs.TELESCOP     = STRING(val11)
     IF count12 GE 1   THEN strobs.WAVEMIN       = STRING(val12)
     IF count12 GE 1   THEN strobs.WAVEMAX      = STRING(val12)
     IF count13 GE 1   THEN strobs.WAVENAME    = STRING(val13)
     IF count13b GE 1  THEN strobs.WAVEUNIT     = STRING(val13b)
     IF count14 GE 1   THEN strobs.OBS_TYPE      = STRING(val14)
     IF count15 GE 1   THEN strobs.DATE_OBS      = STRING(val15)
     IF count16 GE 1   THEN strobs.DATE_END      = STRING(val16)
     IF count17 GE 1   THEN strobs.BSCALE          = STRING(val17)
     IF count18 GE 1   THEN strobs.BZERO            = STRING(val18)
     IF count19 GE 1   THEN strobs.EXP_TIME       = STRING(val19)
     IF count20 GE 1   THEN strobs.C_ROTATION   = FIX(val20)
     IF count21 GE 1   THEN strobs.BITPIX             = FIX(val21)
     IF count22 GE 1   THEN strobs.FILENAME       = org_fname
     IF count23 GE 1   THEN strobs.UNITS              = STRING(val23) 
     IF count24 GE 1   THEN strobs.QUALITY          = STRING(val24)
     IF count27 EQ 1   THEN strobs.COMMENT       = STRING(val27)
     IF count27 GT 1   THEN strobs.COMMENT        = STRJOIN(val27,';')
     IF count31 GE 1   THEN strobs.SPECTRAL_NAME  = STRING(val31)
     IF count32 GE 1   AND count13 LT 1 THEN strobs.WAVENAME = STRING(val32);  LINEID = WAVENAME if no WAVENAME
    ;--------------------------------------------------------
     ; From the header param, try to determine the type/origin
     ; of image
     ;-------------------------------------------------------- 

     info1 = STRLOWCASE(STRTRIM(strobs.OBSERVAT,2))
     info2 = STRTRIM(strobs.NAXIS1,2)
     info3 = STRLOWCASE(STRTRIM(STRING(val11),2))
     info4 = STRLOWCASE(STRTRIM(STRING(val32),2))

     obst = info1
     IF STRPOS(info1,'bbso') NE -1 THEN obst = 'bbso'
     IF STRPOS(info1,'meudon') NE -1 OR STRPOS(info1,'paris') NE -1 AND $
        FIX(info2) GE 1500 THEN obst = 'meu2'
     IF STRPOS(info1,'meudon') NE -1 OR STRPOS(info1,'paris') NE -1 AND $
        FIX(info2) LT 1500 THEN obst = 'meu1'
     IF STRPOS(info1,'meudon') NE -1 OR STRPOS(info1,'paris') NE -1 AND $
         STRPOS(info3,'tower') NE -1 THEN BEGIN
         IF STRPOS(info4,'stacking')  NE -1 THEN obst = 'meu3s' ELSE obst = 'meu3'
     ENDIF
     IF STRPOS(info1,'coimbra') NE -1 THEN obst = 'coim' 
     IF STRPOS(info1,'yunnan') NE -1 THEN obst = 'ynao'     
     PRINT,'###############################' 
     IF obst EQ ' ' THEN BEGIN
        PRINT,'Can not find from which Obs. the image comes from, Returning'
        RETALL
     ENDIF ELSE PRINT,'Observation origin code:',obst

    ;-------------------------------------------------------
    ; Fill strpro structure with header parameters
    ;-------------------------------------------------------

     IF count101 GE 1 THEN strpro.NAXIS1         = FIX(val101)
     IF count102 GE 1 THEN strpro.NAXIS2         = FIX(val102)
     IF count103 GE 1 THEN strpro.CDELT1        = DOUBLE(val103)
     IF count104 GE 1 THEN strpro.CDELT2        = DOUBLE(val104)
     IF count105 GE 1 THEN strpro.CENTER_X   = DOUBLE(val105)
     IF count106 GE 1 THEN strpro.CENTER_Y   = DOUBLE(val106)
     IF count107 GE 1 THEN strpro.R_SUN         = DOUBLE(val107)  
     IF val109 NE ' ' THEN strpro.EL_CEN_X       = FLOAT(val109)
     IF val110 NE ' ' THEN strpro.EL_CEN_Y       = FLOAT(val110)
     IF val111 NE ' ' THEN strpro.EL_ANGLE       = FLOAT(val111)
     IF val112 NE ' ' THEN strpro.EL_AXIS1        = FLOAT(val112)
     IF val113 NE ' ' THEN strpro.EL_AXIS2        = FLOAT(val113)
     IF val114 NE ' ' THEN strpro.STDEVGEO     = FLOAT(val114)
     IF count21 GE 1  THEN strpro.BITPIX         = FIX(val21) ; !! here we put the same value for the processed and original file
     strpro.STANDARD = 1 ; The default is to consider standardization has been applied
     strpro.LIMBDARK = 1 ; The default is to consider limb darkening has been removed
     strpro.EFIT  = 1 ; the default is to consider ellipse fitting has been used
     strpro.PERCENT = 0.5 ; default value (see feature parameters doc)
     strpro.DIVISION = 0 ; method used to normalise image (1: division, 0: subtraction)
     strpro.INORM = '\N' ; normalizing factor for division method
     
    ;-------------------------------------------------------
    ; Get observation time from date_obs -> julian day
    ;-------------------------------------------------------

     date    = STRTRIM(strobs.DATE_OBS,2) 
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
    ; Other parameters for strobs
    ;--------------------------------------------------------

    strobs.LOC_FILENAME  = loc_filename
    strobs.JDINT                 = jdint
    strobs.JDFRAC             = jdfrac


    ;--------------------------------------------------------
    ; Check cdelt and carrot values, if null then compute
    ; them with GET_SUN function
    ;-------------------------------------------------------- 

   IF FIX(strpro.CDELT1) EQ 0 OR FIX(strpro.CDELT2) EQ 0 OR $
             strobs.C_ROTATION EQ default THEN BEGIN
        tmp = GET_SUN(STRTRIM(strobs.DATE_OBS,2),SD=sd,CARR=carr,PA=pa)
        IF FIX(strpro.CDELT1) EQ 0 OR FIX(strpro.CDELT2) EQ 0 THEN BEGIN
          strpro.CDELT1 = sd/FLOAT(strpro.R_SUN) ;420.
          strpro.CDELT2 = strpro.CDELT1 ;(std. Sun is round)
          PRINT,'################################'
          PRINT,'no cdelt info in processed fits header !'
          PRINT,'-> using GET_SUN function to compute them:',strpro.CDELT1,'/',strpro.CDELT2
          PRINT,' '
        ENDIF
        IF strobs.C_ROTATION EQ default THEN BEGIN
          strobs.C_ROTATION = FIX(carr)
          PRINT,'################################'
          PRINT,'no Carrington rot. info in processed fits header!!'
          PRINT,'-> using GET_SUN function to compute it:',strobs.C_ROTATION
          PRINT,' '
        ENDIF
     ENDIF

    ;-------------------------------------------------------
    ; Get runtime info from systime
    ;-------------------------------------------------------

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
    ; Get preprocess runtime with 'date' headp keyword
    ;--------------------------------------------------------

     IF count108 GE 1 THEN BEGIN
       PPRD = BIN_DATE(val108) 
       IF PPRD[1] LT 10 THEN mm='0'+STRTRIM(PPRD[1],2) ELSE mm=STRTRIM(PPRD[1],2)
       IF PPRD[2] LT 10 THEN dd='0'+STRTRIM(PPRD[2],2) ELSE dd=STRTRIM(PPRD[2],2)
       IF PPRD[3] LT 10 THEN hh='0'+STRTRIM(PPRD[3],2) ELSE hh=STRTRIM(PPRD[3],2)
       IF PPRD[4] LT 10 THEN mi='0'+STRTRIM(PPRD[4],2) ELSE mi=STRTRIM(PPRD[4],2)
       IF PPRD[5] LT 10 THEN ss='0'+STRTRIM(PPRD[5],2) ELSE ss=STRTRIM(PPRD[5],2)
       yy = STRTRIM(PPRD[0],2)    
       PPRD = yy+'-'+mm+'-'+dd+' '+hh+':'+mi+':'+ss
     ENDIF ELSE PPRD = default

    ;--------------------------------------------------------
    ; Other parameters for strpro
    ;-------------------------------------------------------- 

     strpro.RUN_DATE          = PPRD
     strpro.LOC_FILE            = loc_file
     strpro.PR_LOCFNAME   = pr_locfname
     strpro.ORG_FNAME       = org_fname

    ;--------------------------------------------------------
    ; Flatten/correct lines/correct dust/Find the filaments
    ;--------------------------------------------------------
      IF DISPLAY_res EQ 1 THEN BEGIN
             arrf = EFR_FILAMENT(obst,DOUBLE(2.*strpro.R_SUN), $
             INPUT=arr,CORRECTED=corrected,/lin,/fla,/dus,/dis,MAIND=maind)
      ENDIF ELSE BEGIN
             arrf = EFR_FILAMENT(obst,DOUBLE(2.*strpro.R_SUN), $
             INPUT=arr,CORRECTED=corrected,/lin,/fla,/dus,MAIND=maind)
      ENDELSE
      
    ;---------------------------------------------------------
    ; Nothing detected ? -> goto next one
    ;---------------------------------------------------------
    IF (WHERE(arrf))[0] EQ -1 THEN GOTO,endloop

    ;---------------------------------------------------------
    ; Some options above are written in strpro here
    ;---------------------------------------------------------
      strpro.LINECLEAN = 1
      strpro.BACKGROUND = 1
      strpro.LINEC_MAIND = STRTRIM(maind,2) 
 
     ;-------------------------------------------------------
      ;OPTIONAL DISPLAY
      ;-------------------------------------------------------
      IF DISPLAY_res EQ 1 OR PIXMAP_method EQ 1 THEN BEGIN
          hdisp = 512
          IF PIXMAP_method EQ 1 THEN WINDOW,10,XS=hdisp*2,YS=hdisp*2,/PIXMAP ELSE WINDOW,10,XS=hdisp*2,YS=hdisp*2
          imi = arr*0b
          maskbck = EFR_ROUNDMASK((SIZE(arr))[1],(SIZE(arr))[2],0.,strpro.R_SUN*1.,COMP=mask_comp)
          imi[maskbck] = 1b
          imi[WHERE(arrf)] = 2b
          TVSCL,REBIN(arr,hdisp,hdisp)
          TV,BYTSCL(REBIN(imi,hdisp,hdisp),0,2),hdisp,0
          XYOUTS,10,10,pfile,/DEVICE,COLOR=150,CHARSIZE=1.4
          XYOUTS,10,40,'1) Preprocessed image',/DEVICE,COLOR=150,CHARSIZE=1.4
          XYOUTS,10+hdisp,20,'2) Detected seeds',/DEVICE,COLOR=150,CHARSIZE=1.4
      ENDIF
      imi = 0

    ;--------------------------------------------------------
    ; Compute the quiet sun intensity and fill STRPRO
    ;--------------------------------------------------------

     hist = MEDIAN(HISTOGRAM(corrected[WHERE(corrected)],min=0.,BIN=1),10)
     maxh = MAX(hist)
     wmax = (WHERE(hist EQ maxh))[0]
     strpro.QSUN_INT = FLOAT(wmax)


    ;--------------------------------------------------------
    ; Link close blobs and fill holes with a closing operator
    ;--------------------------------------------------------
     rad = FIX(3*(strpro.NAXIS1/1024.))
     StrEl= SHIFT(DIST(2*rad+1),rad,rad) le rad
     im = MORPH_CLOSE(arrf,StrEl)
     
    ;-------------------------------------------------------- 
    ; Remove small isolated faint regions (possible to add a display keyword here)
    ;--------------------------------------------------------
     im = EFR_REMOVE_SFIR2(im,corrected);/dis)  

    ;---------------------------------------------------------
    ; Nothing left ? -> goto next one
    ;---------------------------------------------------------
    IF (WHERE(im))[0] EQ -1 THEN GOTO,endloop

    ;--------------------------------------------------------
    ; Remove potential sunspots (possible to add a display keyword here)
    ;--------------------------------------------------------
     tmp = im ;for display purpose
     im = EFR_REMOVE_SUNSPOT(im,corrected,DOUBLE(strpro.CDELT1),DOUBLE(strpro.CDELT2), $
             DOUBLE(strpro.CENTER_X),DOUBLE(strpro.CENTER_Y), $
             DOUBLE(strpro.R_SUN),STRTRIM(strobs.DATE_OBS,2),/dis)
     tmp = tmp - im

   ;--------------------------------------------------------
    ;OPTIONAL DISPLAY
    ;--------------------------------------------------------
    IF DISPLAY_res EQ 1 OR PIXMAP_method EQ 1 THEN BEGIN
       imi = corrected-im*0.5*strpro.QSUN_INT
       imi[mask_comp]=MIN(imi)
       ww= WHERE(tmp,ntache)
       IF ntache GT 0 THEN imi[ww] = MAX(imi)
       TVSCL,CONGRID(imi,hdisp,hdisp),hdisp,hdisp
       XYOUTS,10+hdisp,20+hdisp,'3) Segmented filaments (sunspots in white) on corrected PP image',/DEVICE,COLOR=150,CHARSIZE=1.4
       imi=0
    ENDIF
    tmp = 0

    ;--------------------------------------------------------
    ; Check if there is something in resulting image !
    ; damned, one more goto
    ;--------------------------------------------------------
     chk = WHERE(im,nchk)
     IF nchk EQ 0 THEN GOTO,endloop


    ;--------------------------------------------------------
    ; Filaments description: location intensity, geometry...
    ; fill STRFIL
    ;--------------------------------------------------------

    strfil = EFR_FIL_DESCRIBE(WHERE(im),WHERE(arrf),strfil,LONG(strpro.NAXIS1), $
             LONG(strpro.NAXIS2),DOUBLE(strpro.CDELT1),DOUBLE(strpro.CDELT2), $
             DOUBLE(strpro.CENTER_X),DOUBLE(strpro.CENTER_Y), $
             DOUBLE(strpro.R_SUN),STRTRIM(strobs.DATE_OBS,2), $
             DOUBLE(strpro.QSUN_INT),corrected,maind)

    ;--------------------------------------------------------
    ; Number of filaments
    ;--------------------------------------------------------

    numfil = N_ELEMENTS(strfil)


    ;----------------------------------------------------------
    ; Other parameters for STRFIL
    ;----------------------------------------------------------

    strfil[*].RUN_DATE         = today_utc
    strfil[*].PR_LOCFNAME  = pr_locfname
    
    ;----------------------------------------------------------
    ; Ascii filename definition
    ;----------------------------------------------------------

     FRC_V = STRJOIN(STRSPLIT(FRC_VERSION,'.',/EXTRACT))
     partial_name = STRTRIM(FRC_CODE+'_'+FRC_V+'_'+datef+'_'+obst,2)
     strfil[*].FEAT_FILENAME=partial_name+'_feat.csv'

    ;-------------------------------------------------------
    ;  Filenames and path where to store the results
    ;-------------------------------------------------------  

     resfile2 = apath+partial_name+'_obs.csv'
     resfile3 = apath+partial_name+'_pp.csv'
     resfile4 = apath+partial_name+'_feat.csv'
     resfile5 = apath+partial_name+'_frc.csv'
     resfile7 = jpath+partial_name+'.jpg'

    ;----------------------------------------------------------
    ; Write and save ascii files
    ;----------------------------------------------------------

    IF ASCII EQ 1 THEN BEGIN

        PRINT,'######################'
        PRINT,'Writing ascii files...'
        PRINT,'######################'
        PRINT,' '

    ;######## FILE *_obs.csv ########

        OPENW,un,resfile2,/GET_LUN

        ;print the field names list first
        line = ''
        FOR hh=0,N_ELEMENTS(TAG_LIST_OBS)-1 DO line = line+STRTRIM(TAG_LIST_OBS[hh],2)+';'
        PRINTF,un,line

        ;Then print the values
        line = ''
        FOR hh=0,N_ELEMENTS(TAG_LIST_OBS)-1 DO line = line+STRTRIM(strobs.(hh),2)+';'
        PRINTF,un,line
        CLOSE,un
        FREE_LUN,un,/FORCE

    ;######## FILE *_pp.csv ########

         OPENW,un,resfile3,/GET_LUN

         ;print the field names list first
         line = ''
         FOR hh=0,N_ELEMENTS(TAG_LIST_PRO)-1 DO line = line+STRTRIM(TAG_LIST_PRO[hh],2)+';'
         PRINTF,un,line

         ;Then print the values
         line = ''
         FOR hh=0,N_ELEMENTS(TAG_LIST_PRO)-1 DO line = line+STRTRIM(strpro.(hh),2)+';'
         PRINTF,un,line
         CLOSE,un
         FREE_LUN,un,/FORCE	
	

      ;######## FILE *_feat.csv ########

          OPENW,un,resfile4,/GET_LUN

          ;print the field names list first
           line = ''
           FOR hh=0,N_ELEMENTS(TAG_LIST_FIL)-1 DO line = line+STRTRIM(TAG_LIST_FIL[hh],2)+';'
           PRINTF,un,line

           ;then the fields values in the same order
           FOR nn = 0, numfil-1 DO BEGIN
              line = ''
              FOR hh=0,N_ELEMENTS(TAG_LIST_FIL)-1 DO line = line+STRTRIM(strfil[nn].(hh),2)+';'
              PRINTF,un,line
           ENDFOR
           CLOSE,un
           FREE_LUN,un,/FORCE


       ;######## FILE *_frc.csv ########

         OPENW,un,resfile5,/GET_LUN

         ;print the field names list first
         line = ''
         FOR hh=0,N_ELEMENTS(TAG_LIST_FRC)-1 DO line = line+STRTRIM(TAG_LIST_FRC[hh],2)+';'
         PRINTF,un,line

         ;Then print the values
         line = ''
         FOR hh=0,N_ELEMENTS(TAG_LIST_FRC)-1 DO line = line+STRTRIM(strfrc.(hh),2)+';'
         PRINTF,un,line
         CLOSE,un
         FREE_LUN,un,/FORCE

    ENDIF

    ;-----------------------------
    ; display the skeletons 
    ;-----------------------------

      IF DISPLAY_res EQ 1 OR PIXMAP_method EQ 1 AND numfil GT 0 THEN BEGIN
            IF ASCII EQ 1 THEN BEGIN
                IF strpro.NAXIS1 EQ 2048 THEN res = ascii2fil(FILE=resfile4,/SKE,/IM_2K,/THICK) ELSE res = ascii2fil(FILE=resfile4,/SKE,/THICK);,/PP_DISP)
               TVSCL,CONGRID(res,hdisp,hdisp),0,hdisp
               XYOUTS,10,hdisp+20,'4) Skeletons as extracted from *_feat.csv ascii file',COLOR=150,/DEVICE,CHARSIZE=1.4
            ENDIF ELSE BEGIN
                XYOUTS,50,hdisp+100,'Cannot display skeletons: you need to set ascii option to 1',COLOR=150,/DEVICE,CHARSIZE=1.3
            ENDELSE
            ;imi=0
       ENDIF

      ;--------------------------------------------------------
       ; WRITE a jpeg file of the res
      ;--------------------------------------------------------
      IF JPEG_output EQ 1 THEN BEGIN
          PRINT,'#####################'
          PRINT,'Writing JPEG quicklook of result...'
          PRINT,'#####################'
          PRINT,' '
          WRITE_JPEG,resfile7,TVRD()
      ENDIF


  endloop:

  ;###### Write a PNG QUICKLOOK of the FITS even if no detection

      resfile8 = jpath+datef+'__'+STRING(splitf2[0])+'_subtract_processed.png'
      PRINT,'#####################'
      PRINT,'Writing PNG quicklook of FITS...'
      PRINT,'#####################'
      PRINT,' '
      WINDOW,11,XS=(SIZE(arr))[1],YS=(SIZE(arr))[2],/PIXMAP
      TVSCL,arr
      WRITE_PNG,resfile8,TVRD()
      arr = 0 ;free memory

  ;###### Change the name of the FITS file (to be compatible with automatic
  ;script transfer to ftpbass2000)
      PRINT,'#####################'
      PRINT,'Changing name of FITS file for a temporary one...'
      PRINT,'#####################'
      PRINT,' '
      newname = ppath+datef+'__'+STRING(splitf2[0])+'_subtract_processed.fits'
      command = 'mv '+gfile+' '+newname
      SPAWN,command

  ;###### End of main loop ##### 
  ENDFOR


;###### End of Program #####
END
