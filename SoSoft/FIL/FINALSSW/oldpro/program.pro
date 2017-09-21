
pathfits='/scratch1/observationaldata/GONGDATA_EVENTS2014/20140518/'

window,0,xsize=1500,ysize=500


onlyreproduce='n'
if onlyreproduce eq 'y' then goto,labelreproduce

n=file_search(pathfits+'*Bh*fits',count=count)

;count=20

skip=30

nelem=floor((count-1)/skip)

;running differences step
delta=3


;im=fltarr(count,1024,1024)
clnim=fltarr(nelem,1024,1024)
imbin=fltarr(nelem,1024,1024)
rd=fltarr(nelem-1,1024,1024)

!p.multi=[0,3,1]

for ind=0,nelem-1 do begin

mreadfits,n(ind*skip),index,data,silent=silent
index2map,index,data,map
std_image=congrid(map.data,1024,1024)


bin_image = EGSO_SFC_FILAMENT(std_image,DISPLAY=display,OBSKEY=obskey,FLATCLEAN=flatclean,LINECLEAN=lineclean,DUSTCLEAN=dustclean,CLEANIM=cleanim)
;im(ind,*,*)=std_image(*,*)
clnim(ind,*,*)=cleanim(*,*)
imbin(ind,*,*)=bin_image(*,*)
map.data=cleanim
map.dx=map.dx*2
map.dy=map.dy*2
;****************************
dateobs=map.time
cdelt=map.dx*2.d0        ; el dos es por que reducimos la resolucion a la mitad
indices=where(bin_image ge 1)
str = EGSO_SFC_FIL_DESCRIBE(indices,dateobs,IMAGE=cleanim,CDELT=cdelt)
help,str,/st
;****************************

;rd(ind,*,*)=bin_image(*,*)

;plot_image,std_image
;plot_image,bin_image;,/noerase

;rd(ind,*,*)=imbin(ind+,*,*)-imbin(ind,*,*)

; if ind ge delta then begin
;; plot_image,cleanim;reform(clnim(ind,*,*))
;; plot_image,bin_image           ;reform(imbin(ind,*,*))
; if ind ge delta then rd=bin_image-oldim
; if ind ge delta then plot_image,rd;reform(rd(ind,*,*))
; endif
 oldim=bin_image

;counter,ind,io
;write_png,'frames_'+io+'.png',tvrd()

endfor


labelreproduce:

for ind=0,nelem-2 do begin

 rd(ind,*,*)=imbin(ind+1,*,*)-imbin(ind,*,*)


 plot_image,reform(clnim(ind,*,*))
 plot_image,reform(imbin(ind,*,*))
 plot_image,reform(rd(ind,*,*))
counter,ind,io
write_png,'frames_'+io+'.png',tvrd()
 
endfor



stop
end
