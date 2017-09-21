@/home/fuller/IDL/FIL/filament.pro
@/home/fuller/IDL/FIL/clean_image.pro

PRO uniform_test

list=dialog_pickfile(filter='*subtract_processed.fits', $ 
                     path='/home/fuller/poub/FITS/PROCESSED/' $
                     ,/multiple_files)

nb=n_elements(list)

for ii=0,nb-1 do begin
   print,'################'
   print,list[ii]

   im=readfits(list[ii],hii)
   cleanim=clean_image(im,420)
   uni=get_uniformity(cleanim)

   print,uni
   print,'############'

endfor

end