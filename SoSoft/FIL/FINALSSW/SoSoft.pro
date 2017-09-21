;The MIT License (MIT)

;Copyright (c) 2017 MISAEL ENRIQUE PERAZA LUIS

;Permission is hereby granted, free of charge, to any person obtaining a copy
;of this software and associated documentation files (the "Software"), to deal
;in the Software without restriction, including without limitation the rights
;to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;copies of the Software, and to permit persons to whom the Software is
;furnished to do so, subject to the following conditions:

;The above copyright notice and this permission notice shall be included in all
;copies or substantial portions of the Software.

;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;SOFTWARE.


;Este trabajo se ha hecho expresamente para el Trabajo Fin de Máster en Visual Analytics y Big Data de la UNIR.



pathfits='/SoSoft/TEST-DATA/'
pathimages='/SoSoft/TEST-IMAGES/'

;window,0,xsize=1200,ysize=600

;!p.charsize=0.9

n=file_search(pathfits+'*Lh*fits',count=count)

;xrange=[-1024,1024]
;yrange=[-1024,1024]



skip=1

;!p.multi=[0,2,1]

nelem=floor((count-1)/skip)

globalstrfil = []
globalidx = 1


for ind=0,n_elements(n)-1 do begin
   print,"IMAGEN: ",ind
   mreadfits,n(ind*skip),index,data,header=header,silent=silent
   print,"what is header", header
   index2map,index,data,map
   
;reduce the size of the map
   std_image=congrid(map.data,1024,1024)
   if ind eq 0 then mhacln=replicate(map,nelem)
   if ind eq 0 then mhabin=replicate(map,nelem)

   bin_image = EGSO_SFC_FILAMENT(std_image,DISPLAY=display,OBSKEY='',FLATCLEAN=flatclean,LINECLEAN=lineclean,DUSTCLEAN=dustclean,CLEANIM=cleanim)

;  mhacln(ind).data=congrid(cleanim,2048,2048,/interp)
;  mhabin(ind).data=congrid(bin_image,2048,2048,/interp)


   dateobs=map.time
   cdelt=map.dx*2.d0            ; el dos es por que reducimos la resolucion a la mitad
   indices=where(bin_image ge 1)
   str = EGSO_SFC_FIL_DESCRIBE(indices,dateobs,IMAGE=cleanim,CDELT=cdelt)

;   plot_map,mhacln(ind),xrange=xrange,yrange=yrange
;   oplot,str.GRAV_C_ARCX,str.GRAV_C_ARCY,psym=1
   
;   plot_map,mhabin(ind),xrange=xrange,yrange=yrange
   
   counter,ind,io
;;;;;;
;let's create fil_data to feed track_fil
;;;;;
   strfil = []

   help,str
   for inde=0,n_elements(str)-1 do begin
      fil = {id_fil:globalidx, $
             track_id:globalidx,$
             track_lvl_trust: 0,$
             date_obs: index.date_obs,$
             jdint: fix(index.j2000),$
             jdfrac: index.j2000 - fix(index.j2000), $
             ske_length_deg: float(str[inde].ske_len_deg),$
             ske_cc: str[inde].chain_code_ske,$
             ske_cc_x_pix: str[inde].cod_ske_pix_x,$
             ske_cc_y_pix: str[inde].cod_ske_pix_y,$
             feat_x_arcsec: str[inde].cod_ske_arc_x,$
             feat_y_arcsec: str[inde].cod_ske_arc_y,$
             cdelt1: map.dx,$
             cdelt2: map.dy,$
             center_x: (index.fndlmbxc)/2,$
             center_y: (index.fndlmbyc)/2,$
             r_sun: map.rsun,$
             grav_c_x:str[inde].GRAV_C_ARCX,$
             grav_c_y:str[inde].GRAV_C_ARCY,$
             phenom: 0,$
             ref_feat: 0}

      globalidx = globalidx + 1
      strfil = [strfil,fil]
   endfor
   
   globalstrfil = [globalstrfil,strfil]
   print,globalidx

   
endfor

end
