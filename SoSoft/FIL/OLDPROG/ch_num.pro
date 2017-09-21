PRO CH_NUM
  
  file=dialog_pickfile(path='/home/fuller/IDL/FIL/')
  openr,runit,file,/get_lun
  openw,wunit,file+'.corr',/GET_LUN
  line=''
  ind=0

  WHILE NOT EOF(runit) DO BEGIN
     READF,runit,line
     ;print,line    
     sep=STR_SEP(line,'/')
     num=N_ELEMENTS(sep)
     IF num GE 4 THEN BEGIN
       sep[1]=STRTRIM(ind,2)
       ind=ind+1
       newline=sep[0]+'/'+sep[1]+'/'+sep[2]+'/'+sep[3]+'/'
       PRINTF,wunit,newline
     ENDIF ELSE BEGIN
       PRINTF,wunit,line        
     ENDELSE

  ENDWHILE 
  free_lun,runit
  free_lun,wunit
END