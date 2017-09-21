PRO SUPERIMPOSE_SUNSPOTS

  tab = [21,31,35,39,73,84,88,89,92,140,146,149,191,195,202,204,251,253,$
         256,259,296,303,308,346,351,359,401,409,410,461,463,499,510,$
         518,519,560,580,583,620,635,638,642,676,678,692,697,734,736,$
         746,750,795,800,815,820,865,874,880,885,947,949,956,1003,1059,$
         1070,1121,1250,1264,1268,1272,1335,1401,1407,1410,1469,1533,$
         1599,1653,1655]

  ;read the file on which the filaments will be superimposed
  file1 = READFITS(dialog_pickfile(path='/data2/fuller/FITS/Ha/2002/PROCESSED/'),h0)


  ;get the date
  date  = FXPAR(h0,'DATE_OBS')
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


  ;restore the corresponding structure if monthly stored
  fpath = '/home/fuller/poub/SAV/'+'fil_struct_'+month+year+'.sav'
  RESTORE,fpath

  ;restore the corresponding structure if stored for 1 rotation
;  fpath = '/home/fuller/poub/SAV/'+'fil_struct_rot'+STRTRIM(FIX(cnumb),2)+'.sav'
;  RESTORE,fpath

  ;keep only the structure indices that are potential sunspots
   strt=str[tab]
 
  ;get the filaments parameters for the given date
  dates = STRMID(strt[*].srce.DATE_OBS,0,10)
  gdates = WHERE(STRMATCH(dates,year+'-'+month+'-'+day) EQ 1,ngd)
  IF ngd EQ 0 THEN goto,endp
  daystruct=strt[gdates]

print,daystruct[0].srce.DATE_OBS
  
window,/free,xs=1024,ys=1024

  ;
  FOR ii=0,ngd-1 DO BEGIN

      ;####recuperation des valeurs de l'image source
      cdelt1srce = daystruct[ii].srce.cdelt1
      cdelt2srce = daystruct[ii].srce.cdelt2

      ;####extraction de la position en arcs
      ;####sur l'image source du bounding rect
      bndrect_arcs = daystruct[ii].fil_bndg_rect_arcs
      bndrect_arcs_x = bndrect_arcs[0:1]
      bndrect_arcs_y = bndrect_arcs[2:3]

      ;####calcul de la position sur l'image d'arrivee
      ;####a partir de ces propres valeurs (cdelt notamment)
      xptsnew = FIX(DOUBLE(center_x) + bndrect_arcs_x/DOUBLE(cdelt1))
      yptsnew = FIX(DOUBLE(center_y) + bndrect_arcs_y/DOUBLE(cdelt2))


      ;#####superposition
      file1[xptsnew[0]-5:xptsnew[1]+5-1,yptsnew[0]-5] = MAX(file1)
      file1[xptsnew[0]-5:xptsnew[1]+5-1,yptsnew[1]+5] = MAX(file1)
      file1[xptsnew[0]-5,yptsnew[0]-5:yptsnew[1]+5-1] = MAX(file1)
      file1[xptsnew[1]+5,yptsnew[0]-5:yptsnew[1]+5-1] = MAX(file1)
      tvscl,file1
      ;xyouts,xptsnew[0],yptsnew[0]-20,strtrim(daystruct[ii].fil_index-1,2),/device
      xyouts,xptsnew[0],yptsnew[0]-20,STRTRIM(tab[gdates[ii]],2),/device
      file1=TVRD()
  ENDFOR      



tvscl,file1
;tvscl,hist_equal(file1,percent=1)

endp:
END

