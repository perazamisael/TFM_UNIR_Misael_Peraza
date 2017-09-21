FUNCTION CHAIN2IND,first,chainc,xsize,ysize
  
   ind=first[1]*LONG(xsize)+first[0]
   length=STRLEN(chainc)
   set=[1,xsize+1,xsize,xsize-1,-1,-xsize-1,-xsize,-xsize+1]
   newind=ind
   FOR jj=0,length-1 DO BEGIN
      next=FIX(STRMID(chainc,jj,1))
      newind=newind+set[next]
      ind=[ind,newind]
   ENDFOR      
   
RETURN,ind
END

;#############################################################

PRO STRUCT2SYN,struct,nr

;#########################################
;#########EN COURS DE DEV.################
;#########################################

  ;-----------------------------------------------------------
  ;write the final ascii file
     fname ='/home/fuller/IDL/FIL/Test'+STRTRIM(STRING(nr),2)+'.FIL'
     OPENW,wunit,fname,/GET_LUN
     FREE_LUN,wunit

  index=struct[*].fil_index
  windex=WHERE(index EQ 1,nday)
  windex=[windex,N_ELEMENTS(index)]
  aa=0

  ;get the julian date of nr and nr+1
  nr1  = CARR2EX(nr)
  nr2  = CARR2EX(nr+1)
  jnr1 = JULDAY(nr1(5),nr1(4),nr1(6),nr1(0),nr1(1),nr1(2))
  jnr2 = JULDAY(nr2(5),nr2(4),nr2(6),nr2(0),nr2(1),nr2(2))
  timlim = DOUBLE(jnr1+jnr2)/2.

  FOR ii=0,nday-1 DO BEGIN

    OPENU,wunit,fname,/APPEND,/GET_LUN
    daystr=struct[windex[ii]:windex[ii+1]-1]
    date  = daystr[0].source.im_obs_UT
    date  = y2kfix(anytim2utc(date,/ecs,/trunc))
    year  = STRMID(date,0,4)
    month = STRMID(date,5,2)
    day   = STRMID(date,8,2) 
    hour  = STRMID(date,11,2)
    min   = STRMID(date,14,2)
    sec   = STRMID(date,17,2)
    jday  = JULDAY(month,day,year,hour,min,sec)    
    sjday = STRTRIM(STRING(jday,format='(f14.6)'),2)
    L0    = TIM2CARR(date)
    nfil  = N_ELEMENTS(daystr)

    FOR jj=0,nfil-1 DO BEGIN

      aa=aa+1
      centre  = daystr[jj].fil_ske_cen_carr
      ptstart = daystr[jj].fil_ske_chain_strt
      chain   = daystr[jj].fil_ske_chain
      indices = CHAIN2IND(ptstart,chain,1024,1024)
      ;carrpts = PIX2CARR()


       IF jday LT timlim THEN BEGIN
          range(0)=(L0+180.) MOD 360. 
          range(1)=360.
       ENDIF
       IF jday GE timlim THEN BEGIN
          range(0)= 0. 
          range(1)= (L0+180.) MOD 360.
       ENDIF

      ;region number 
      regn  = STRTRIM(STRING(aa),2)

    IF longb_check GE range(0) AND longb_check LE range(1)  THEN BEGIN

      ;print the barycenter coordinates
      slong_b = STRTRIM( STRING(centre[1], format='(f10.6)' ) ,2)
      slat_b  = STRTRIM( STRING(centre[0], format='(f10.6)' ) ,2)
      PRINTF,wunit,FORMAT='(A,"/",A,"/",A,"/",A,"/")',sjday,regn,slong_b,slat_b

      ;each point of the filament:
       ;FOR kk=0,nx-1 DO BEGIN

            ;calculate the coordinates of fil (+L0)
             ;longi  = ((coord(jj,1) + 360.) MOD 360. + L0) MOD 360.
             ;lati  = coord(jj,0)

            ;check the long in case the filament would cross the L0 long
             ;IF ABS(longi-longb_check) GT 180. AND longi LT 180. THEN longi=longi+360.
             ;IF ABS(longi-longb_check) GT 180. AND longi GT 180. THEN longi=longi-360.              

            ;print the coordinates
             ;slongi = STRTRIM( STRING( longi,format='(f10.6)' ) ,2)
             ;slati  = STRTRIM( STRING( lati ,format='(f10.6)' ) ,2)
             ;PRINTF,wunit,FORMAT='(A,"/",A,"/")',slongi,slati

       ;ENDFOR
        
       ;mark the end of filament
       PRINTF,wunit,'-999.99/'

    ENDFOR

 ENDIF

    FREE_LUN,wunit  

  ENDFOR

END




