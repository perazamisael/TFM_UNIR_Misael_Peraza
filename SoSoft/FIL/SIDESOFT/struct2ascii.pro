;+
; NAME:
;       struct2ascii
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
;       AUTOMATED MEUDON SYNOPTIC MAPS
;
; CALLING SEQUENCE:
;
;       .r struct2ascii
;
; CALLS
;
;       - carr2ex (SolarSoft)
;       - tim2carr (SolarSoft)
;       - anytim2utc (SolarSoft)
;       - y2kfix (SolarSoft)
;       - chain2ind (egso)
;       - efr_resample_ind in efr_shape_lib (egso)
;       - pix2carr (egso)
;
; INPUTS:
;    
;       - One Carrington rotation number NNNN
;       - IDL structures corresponding to Halpha observations
;         as produced by csefr_fil2ascii
;
; OUTPUTS:
;
;       The ascii file NNNN.FIL needed by the synoptic maps
;       application 
;  
; MODIFICATION HISTORY:
;
;    NF nov 2004 last modif
;-
@/home/fuller/IDL/FIL/efr_shape_lib
;----------------------------------------------------------
; Define path to store the idl structures
; Define path to store the ascii file
;----------------------------------------------------------

  spath = '/home/fuller/poub/SAV/FIL/STRUCT/'
  apath = '/home/fuller/poub/SAV/FIL/SYN/'


;----------------------------------------------------------
; Get the carrington rotation number from user
;----------------------------------------------------------

  PRINT,'Enter the Carrington rotation you want to compute,'
  PRINT,'for example: 1988'
  READ,carot,FORMAT='(A4)'
  carot = STRTRIM(carot,2)


;----------------------------------------------------------
; Compute the dates of interest for this Carrington rot. 
;----------------------------------------------------------

  dat1 = CARR2EX(FIX(carot),OFFSET=90)
  dat2 = CARR2EX(FIX(carot)+1,OFFSET=-90)

  jdat1 = JULDAY(dat1(5),dat1(4),dat1(6),dat1(0),dat1(1),dat1(2))
  jdat2 = JULDAY(dat2(5),dat2(4),dat2(6),dat2(0),dat2(1),dat2(2))
  timlim = (jdat1+jdat2)/2.
  nbj  = LONG(jdat2)-LONG(jdat1) + 1
  jdat = LONG(jdat1) + INDGEN(nbj)

  CALDAT, jdat, mo, da, ye

;----------------------------------------------------------
; Compute the structures filenames and check if they exist
;----------------------------------------------------------

  tabstr = STRARR(nbj)
  FOR ii = 0 ,nbj-1 DO BEGIN

    IF mo[ii] LT 10 THEN moii = '0'+STRTRIM(mo[ii],2) $
    ELSE moii = STRTRIM(mo[ii],2)
    IF da[ii] LT 10 THEN daii = '0'+STRTRIM(da[ii],2) $
    ELSE daii = STRTRIM(da[ii],2)
    yeii = STRMID(STRTRIM(ye[ii],2),2)
    name = yeii+moii+daii+'*.sav'
    ffile = FINDFILE(spath+name,COUNT=cc)
    IF cc GT 1 THEN ffile = ffile[n_ELEMENTS(ffile)-1]
    IF cc EQ 0 THEN BEGIN
       PRINT,name,' is missing, ignore [1] or cancel [0]?'
       READ,resp,FORMAT='(A1)'
       IF resp NE 1 THEN RETALL
       ffile = '\N'
    ENDIF
    tabstr[ii] = ffile
  ENDFOR

;----------------------------------------------------------
; Create the ascii file NNNN.FIL
;----------------------------------------------------------

   fname = apath+carot+'.FIL'
   OPENW,wunit,fname,/GET_LUN,/APPEND

;----------------------------------------------------------
; Main loop
;----------------------------------------------------------
  cmpt = 1

  FOR ii=0,nbj-1 DO BEGIN

     ;-----------------------------------------------------
     ; Restore structure
     ;-----------------------------------------------------
      structfn = tabstr[ii]
      IF structfn NE '\N' THEN BEGIN
         RESTORE,structfn,/RELAX

        ;--------------------------------------------------
        ; Get observation julian date
        ;--------------------------------------------------
         date  = strobs[0].DATE_OBS
         date  = Y2KFIX(ANYTIM2UTC(date,/ecs,/trunc))
         jday  = LONG(strobs.jdint)+DOUBLE(strobs.jdfrac)
         sjday = STRTRIM(STRING(jday,format='(f14.6)'),2)

        ;--------------------------------------------------
        ; Get carringt. long corresp. to observation date 
        ;--------------------------------------------------
         L0 = TIM2CARR(date)

        ;--------------------------------------------------
        ; Set the longitude range to check if a filament 
        ; belong to the rotation or not 
        ;--------------------------------------------------
         range = FLTARR(2)
         IF jday LT timlim THEN BEGIN
            range[0]=(L0+180.) MOD 360. 
            range[1]=360.
         ENDIF
         IF jday GE timlim THEN BEGIN
            range[0]= 0. 
            range[1]= (L0+180.) MOD 360.
         ENDIF

        ;--------------------------------------------------
        ; Filament loop
        ;--------------------------------------------------
         nfil = N_ELEMENTS(strfil)
         FOR jj = 0,nfil-1 DO BEGIN

          ;--------------------------------------------------
          ; Get gravity center coordinates from structure
          ;--------------------------------------------------          
           longfil = strfil[jj].grav_c_car_lon
           latfil  = strfil[jj].grav_c_car_lat

          ;--------------------------------------------------
          ; Check if the filament belong to the rotation
          ;--------------------------------------------------         
            IF longfil GE range[0] AND longfil LE range[1] THEN BEGIN  
              cmpt = cmpt + 1

             ;--------------------------------------------------
             ; Print first ascii line with julian day,index and
             ; gravity center coordinates
             ;--------------------------------------------------
              PRINTF,wunit,FORMAT='(A,"/",A,"/",A,"/",A)',sjday,STRTRIM(cmpt,2),$
              STRTRIM(longfil,2),STRTRIM(latfil,2) 

             ;--------------------------------------------------
             ; Find the subscripts of skeleton from chain code
             ; and convert to carrington
             ;--------------------------------------------------           
              npts_ori = strfil[jj].ccode_ske_lnth
              ccode = strfil[jj].chain_code_ske
              ccode_strtx = strfil[jj].cod_ske_pix_x
              ccode_strty = strfil[jj].cod_ske_pix_y
              skeind = CHAIN2IND([ccode_strtx,ccode_strty],ccode,1024,1024)
              skeind = EFR_RESAMPLE_IND(skeind,4)
              npts = N_ELEMENTS(skeind)
              cd1 = DOUBLE(strpro.cdelt1)
              cd2 = DOUBLE(strpro.cdelt2)
              carcoord = PIX2CARR(skeind,1024,1024,cd1,cd2,511.5,511.5,420,$
              date,fixl0=L0)

             ;--------------------------------------------------
             ; Skeleton elements loop
             ;--------------------------------------------------           
              FOR kk = 0, npts-1 DO BEGIN
                longi = carcoord[kk,1]
                lati  = carcoord[kk,0] 

               ;--------------------------------------------------
               ; Make the coordinates of the filaments which cross L0=0
               ; continuous (ex: 358,359,1,3,5,8 -> -2,-1,1,3,5,8 or
               ; 355,357,358,359,0,2 -> 355,357,358,359,360,362) 
               ;--------------------------------------------------
                IF ABS(longi-longfil) GT 180. AND longi LT 180. THEN longi=longi+360.
                IF ABS(longi-longfil) GT 180. AND longi GT 180. THEN longi=longi-360.
                slongi = STRTRIM(STRING(longi,format='(f10.6)'),2)
                slati  = STRTRIM(STRING(lati,format='(f10.6)'),2)                

               ;--------------------------------------------------
               ; Print coordinates of the filament
               ;--------------------------------------------------
                PRINTF,wunit,FORMAT='(A,"/",A,"/")',slongi,slati           

              ENDFOR ;ske points loop

             ;--------------------------------------------------
             ; End of a filament record
             ;--------------------------------------------------    
              PRINTF,wunit,'-999.99/'

           ENDIF ;long check

        ENDFOR ;nfil loop          

     ENDIF ;structfn

  ENDFOR ;main loop

  CLOSE,wunit
  FREE_LUN,wunit

END




