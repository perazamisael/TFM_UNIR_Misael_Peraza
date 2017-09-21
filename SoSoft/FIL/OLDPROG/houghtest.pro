
PRO slidercb,event
COMMON TOTO,array,binim,houghim,slidid,slidid2,sld1val,sld2val,rho,theta,winid

  widget_control,slidid,get_value=sld1val
  titi=(houghim-sld1val)>0
  backproject=HOUGH(titi,/backproject,rho=rho,theta=theta)
  wset,winid
  tvscl,congrid(backproject,400,400),2

END

;##################################################################

PRO slidercb2,event
COMMON TOTO,array,binim,houghim,slidid,slidid2,sld1val,sld2val,rho,theta,winid

  widget_control,slidid2,get_value=sld2val 
  binim=array lt sld2val
  r_l=label_region(binim,/all_neigh)
  h_r_l=HISTOGRAM(r_l,reverse_indices=r)
  FOR i=0,N_ELEMENTS(h_r_l)-1 DO BEGIN
      p=r[r[i]:r[i+1]-1]
    IF h_r_l[i] LT 20 THEN BEGIN
       ;p=r[r[i]:r[i+1]-1]
       px = p MOD 400
       py = p/400
       minx=MIN(px,MAX=maxx)
       miny=MIN(py,MAX=maxy)
       xc=(maxx+minx)/2
       yc=(maxy+miny)/2 
       ;simg=bytarr(400,400)
       ;simg[p]=1b
       ;CONTOUR,simg,path_xy=xy,/path_data_coords,level=[1]
       ;roi = OBJ_NEW('idlanroi',xy,type=2)
       ;status = roi -> COMPUTEGEOMETRY(centroid=centroid)
       binim[p] = 0b
       binim[xc,yc] = 1b

    ENDIF ELSE binim[p]=0b
  ENDFOR
  
  wset,winid 
  tvscl,binim,1

  houghim=HOUGH(binim,rho=rho,theta=theta)
  wset,winid
  tvscl,congrid(houghim,400,400),2

END

;##################################################################

PRO HOUGHTEST
COMMON TOTO,array,binim,houghim,slidid,slidid2,sld1val,sld2val,rho,theta,winid

  DEVICE,decompose=0
  loadct,0
  tlb=widget_base(title='Hough Transform',/column)
  winid=widget_draw(tlb,xs=400*3,ys=400)
  slidid =widget_slider(tlb,min=10,max=200,title='NBP to keep', event_pro='sliderCB')
  slidid2=widget_slider(tlb,min=100,max=2000,title='seuil', event_pro='sliderCB2')

 widget_control,tlb,/realize

 array=readfits(dialog_pickfile(path='/home/fuller/poub/FITS/PROCESSED'),h0)
 array=CONGRID(array,400,400)

  ;####APPLY A MEDIAN FILTER AND DIVIDE INPUT BY THE RESULT
  medv   = MEDIAN(array(WHERE(array)))
  im_MED = MEDIAN(array, 60)
  array  = array-im_MED+medv
 
 widget_control,winid,get_value=winid
 wset,winid
 tvscl,array,0

 xmanager,'houghex',tlb

end

