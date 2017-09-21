PRO testconv


   im=readfits(dialog_pickfile(path='/home/fuller/poub/FITS/PROCESSED/'))
   tvscl,im
   str=replicate(1,3,3)
   str[1,1]=-8
   conv=CONVOL(im,str)
   tvscl,conv
   mini=ABS(MIN(conv))
   mom=moment(conv,sdev=sdevc,mdev=mdevc) 
   print,sdevc,mdevc
   hist=MEDIAN(histogram(conv[where(conv)]),100)
   maxi=MAX(hist)
   tresh=maxi/7.
   ww=WHERE(hist LT tresh)
   ww3 = ww[1:N_ELEMENTS(ww)-1] - ww[0:N_ELEMENTS(ww)-2]
   pt = ww[where(ww3 EQ MAX(ww3))+1]
   res = pt - mini
   print,res
   tvscl,conv GT res[0]
END
   
