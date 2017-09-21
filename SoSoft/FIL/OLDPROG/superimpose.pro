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


FUNCTION SUPERIMPOSE,obs,CLIM=clim
  ;obs = meu1, meu2,...
  obs = STRTRIM(obs,2)
  ;superimpose segmented filaments to another Sun observation
  ;if this observation has been standardized 

  ;read the file on which the filaments will be superimposed
  filepath  = dialog_pickfile(path='/data2/fuller/FITS/Ha/2002/')
  file1 = READFITS(filepath,h0)

  splitname =STRSPLIT(filepath,'/',/EXTRACT)
  nsplitf = N_ELEMENTS(splitname)
  filename = splitname[nsplitf-1]
  splitf2 = STRSPLIT(filename,'_subtract_processed',/EXTRACT,/REGEX)
  filename  = STRMID(splitf2[0],2)

  ;get the date
  date  = FXPAR(h0,'DATE_OBS')
print,date
  year  = STRTRIM(STRMID(date,0,4),2)
  month = STRTRIM(STRMID(date,5,2),2)
  day   = STRTRIM(STRMID(date,8,2),2)

  ;get other info from header
  ;center_x  = FXPAR(h0,'CENTER_X')
  ;center_y  = FXPAR(h0,'CENTER_Y')
  center_x  = FXPAR(h0,'CRPIX1')
  center_y  = FXPAR(h0,'CRPIX2')
  cdelt1    = FXPAR(h0,'CDELT1')
  cdelt2    = FXPAR(h0,'CDELT2')
  naxis1    = FXPAR(h0,'NAXIS1')
  naxis2    = FXPAR(h0,'NAXIS2')
  cnumb     = FXPAR(h0,'SOLROT_N')

;print,center_x,center_y
;print,cdelt1,cdelt2
;print,naxis1,naxis2

  ;restore the corresponding structure if monthly stored
  ;fpath = '/home/fuller/poub/SAV/'+'fil_struct_'+month+year+'.sav'
  ;RESTORE,fpath

  ;restore the corresponding structure if stored for 1 rotation
;  fpath = '/home/fuller/poub/SAV/'+'fil_struct_rot'+STRTRIM(FIX(cnumb),2)+'.sav'
;  RESTORE,fpath

  ;restore the corresponding structure if daily stored
   fpath = '/home/fuller/poub/SAV/FIL/STRUCT/'+filename+obs+'_fil.sav'
   print,fpath
   RESTORE,fpath


;####temporary
;  nn=N_ELEMENTS(str)
;  tab=INDGEN(nn)
;#####


;###temporary  
;window,/free,xs=1024,ys=1024
;
  ngd = N_ELEMENTS(strfil) 

      ;####recuperation des valeurs de l'image source
      cdelt1srce = strpro.cdelt1_p
      cdelt2srce = strpro.cdelt2_p
      naxis1srce = strpro.naxis1_p
      naxis2srce = strpro.naxis2_p

IF KEYWORD_SET(clim) THEN file1=clim

  FOR ii=0,ngd-1 DO BEGIN


      ;####calcul pour tous les points de la position en pixels
      ;####sur l'image source
      first_pt_arcsx = strfil[ii].cod_ske_arc_x
      first_pt_arcsy = strfil[ii].cod_ske_arc_y
      xpos = FIX(DOUBLE(511.5) + first_pt_arcsx/DOUBLE(cdelt1srce))
      ypos = FIX(DOUBLE(511.5) + first_pt_arcsy/DOUBLE(cdelt2srce))
      schain = strfil[ii].ske_chain
      indices = CHAIN2IND([xpos,ypos],schain,naxis1srce,naxis2srce)

      ;####calcul de la position en arcs
      ;####a partir des valeurs de l'image source
      xpts = indices MOD naxis1srce
      ypts = indices / naxis2srce
      xptsarcs = DOUBLE(cdelt1srce)*( xpts - DOUBLE(511.5) )
      yptsarcs = DOUBLE(cdelt2srce)*( ypts - DOUBLE(511.5) )

      ;####calcul de la position sur l'image d'arrivee
      ;####a partir de ces propres valeurs (cdelt notamment)
      xptsnew = FIX(DOUBLE(center_x) + xptsarcs/DOUBLE(cdelt1))
      yptsnew = FIX(DOUBLE(center_y) + yptsarcs/DOUBLE(cdelt2))

      xposnew = FIX(DOUBLE(center_x) + first_pt_arcsx/DOUBLE(cdelt1))
      yposnew = FIX(DOUBLE(center_y) + first_pt_arcsy/DOUBLE(cdelt2))



      ;#####superposition
      file1[xposnew,yposnew] = MAX(file1)
      file1[xptsnew+naxis1*yptsnew] = MAX(file1)

;####temporary
   ;tvscl,file1
   ;xyouts,xposnew,yposnew-5,STRTRIM(tab[gdates[ii]],2),/device
   ;file1=TVRD() 

  ENDFOR      


;window,/free,xs=1024,ys=1024
tvscl,file1
;tvscl,hist_equal(file1,percent=1)

RETURN,file1
endp:
 
END
