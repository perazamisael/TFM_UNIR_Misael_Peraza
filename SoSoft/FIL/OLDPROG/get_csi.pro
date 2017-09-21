FUNCTION GET_CSI,file=file
   
  IF NOT KEYWORD_SET(file) THEN BEGIN
     file=DIALOG_PICKFILE(PATH='/home/fuller/poub/FITS/PROCESSED/',FILTER='*.fits')
  ENDIF

  arr = READFITS(file,h0)
  csi = ITOOL_SET_CSI(h0)
  print,csi
RETURN,csi
END