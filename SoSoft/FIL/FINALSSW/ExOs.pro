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



;;; Calcular conjunto de track_ids
trackids = results.track_id
sorted_trackid = trackids[Sort(trackids)]
uniq_trackid = sorted_trackid[Uniq(sorted_trackid)]

dateobs = results.date_obs
sorted_dateobs = dateobs[Sort(dateobs)]
uniq_dateobs = sorted_dateobs[Uniq(sorted_dateobs)]

str_cm=[]

for filid = 0,n_elements(uniq_trackid)-1 do begin
 for dateid = 0,n_elements(uniq_dateobs)-1 do begin
    encontrado = 0	
    b=[0,0]
    lj=0
    
  ; recorrer toda la lista buscando estructuras con el track_id y date_obs que coincidan
  for i=0,n_elements(results)-1 do begin
      if ((results[i].track_id eq uniq_trackid[filid]) && (results[i].date_obs eq uniq_dateobs[dateid])) then begin
      encontrado = 1
	lj=lj+results[i].ske_length_deg
	b=b+[results[i].ske_length_deg*results[i].grav_c_x,results[i].ske_length_deg*results[i].grav_c_y]
      endif
  endfor	

  if (encontrado eq 1) then begin
    cm = b/lj
    A = {track_id: uniq_trackid[filid], $
	      date_obs: uniq_dateobs[dateid], $
	      mass_c: cm}
    str_cm=[str_cm,A]
;   print,"filamento",  uniq_trackid[filid], "  ", uniq_dateobs[dateid], " CM= ", cm
  endif

  endfor
endfor

; SACAR centros de masas para cada track_id 
for indid=0,n_elements(uniq_trackid)-1 do begin
  print,"centros de masas en filamento(s) ", uniq_trackid[indid]
  centers=str_cm[where(str_cm.track_id eq uniq_trackid[indid])].mass_c
  print, centers
endfor

end
