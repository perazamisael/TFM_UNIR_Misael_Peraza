@/home/fuller/IDL/FIL/pruned_ske.pro
@/home/fuller/IDL/FIL/Fit_Ellipse.pro

FUNCTION FILL_STRUCT,indices,strfil,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,qsi,cleanim


      ;######Image size
      xsize = nax1
      ysize = nax2


      ;######Draw the filament binary image
      mask  = bytarr(xsize,ysize)
      mask[indices] = 1b


      ;######Get stat on the corrected image to
      ;######compute the relative strength of the blobs
       ;qs  = qsi
       mom = MOMENT(cleanim(where(cleanim)),sdev=sdevc)


      ;######Create a structure that will contain
      ;######all the blobs info
      B = strfil
 

      ;######Label the blobs to get indices of each
      r_l   = LABEL_REGION(mask,/all_neigh)
      h_r_l = HISTOGRAM(r_l,reverse_indices=r)

      cmpt=0

   ;######LOOP ON EACH BLOB
   FOR i = 1, N_ELEMENTS(h_r_l)-1 DO BEGIN


       indices = r[r[i]:r[i+1]-1]
       maski = mask*0b
       maski[indices] = 1b


      ;#######Strength calculted from the mean of the 
      ;#######indices
       meanval  = MEAN(cleanim[indices])
       minval   = MIN(cleanim[indices],MAX=maxval)
       mir      = meanval / qsi


      ;#######Create a box that will contain the fil.
      ;#######to compute the parameters faster
       coordX = indices MOD xsize
       coordY = indices / xsize
       minX   = MIN(coordX,MAX=maxX)
       minY   = MIN(coordY,MAX=maxY)
       xsizeb = maxX-minX+20
       ysizeb = maxY-minY+20
       box    = maski[minX-10:maxX+10-1,minY-10:maxY+10-1]
       boxind = WHERE(box)


      ;#######give a bounding rectangle in pixels
       brp = [minX,minY,maxX,maxY]


      ;#######give a bounding rectangle in arcsecs
       mmX = DOUBLE(cd1*([minX,maxX]-cenx))
       mmY = DOUBLE(cd2*([minY,maxY]-ceny))          
       bra  = [mmX[0],mmY[0],mmX[1],mmY[1]]


      ;#######Check if the filament inside box
      ;#######contains holes and fill them
       boxind = CHECK_FILLING(boxind,xsizeb,ysizeb)
       box[boxind]=1b


       ;###### Compute the skeleton
       ;###### ec is the number of erosion needed to completly
       ;###### erode the structure 
       ske_ind = PRUNED_SKE(boxind,xsizeb,ysizeb,EC=ec)
       ske_ind = M_CONNECT(ske_ind,xsizeb,ysizeb) 
       ske_ind = ORDER_IND(ske_ind,xsizeb,ysizeb)


     IF N_ELEMENTS(ske_ind) GT 3 THEN BEGIN

       cmpt = cmpt + 1
      ;#######for the external boundary 
      ;#######(-> avoid problems with 1 pixel thick blob)
      ;essayer avec get_ext_boundary
       mask_b=DILATE(box,REPLICATE(1,3,3))
       dindices=WHERE(mask_b)

       ;######compute the boundary
       bound_ind  = GET_BOUNDARY(dindices,4,xsizeb,ysizeb)     
       bound_ind  = M_CONNECT(bound_ind,xsizeb,ysizeb)        
       bound_ind  = ORDER_IND(bound_ind,xsizeb,ysizeb)


       ;######Get the first points for the chain codes       
       ske_chain_strt  = ske_ind[0]
       bnd_chain_strt  = bound_ind[0]


       ;######Compute the chain codes
       ske_chain  = CHAIN_CODE(ske_ind,xsizeb,ysizeb,/ske)
       bnd_chain  = CHAIN_CODE(bound_ind,xsizeb,ysizeb)


       ;######Get the blob orientation from Fit_Ellipse (D.Fanning)
       ;######Orientation is given in deg counter-clockwise from the X axis
       ell = FIT_ELLIPSE(boxind,Xsize=xsizeb,Ysize=ysizeb,Orientation=orien)


       ;#######Give the first points of chain codes in pixel coordinates
       ske_chain_strt_pix = [ske_chain_strt MOD xsizeb + minX - 10 ,$
                             ske_chain_strt /   xsizeb + minY - 10]
       bnd_chain_strt_pix = [bnd_chain_strt MOD xsizeb + minX - 10 ,$
                             bnd_chain_strt /   xsizeb + minY - 10]

      ;#######In arcsecs
       ske_chain_strt_arcs = [DOUBLE(cd1*(ske_chain_strt_pix[0]-cenx)),$
                              DOUBLE(cd2*(ske_chain_strt_pix[1]-ceny))]
       bnd_chain_strt_arcs = [DOUBLE(cd1*(bnd_chain_strt_pix[0]-cenx)),$
                              DOUBLE(cd2*(bnd_chain_strt_pix[1]-ceny))]


      ;##get the ske indices for the full size image
       tmpmask = mask*0b
       tmpbox  = box*0b
       tmpbox[ske_ind]=1b
       tmpmask[minX-10:maxX+10-1,minY-10:maxY+10-1]=tmpbox
       ske_ind_full = WHERE(tmpmask)

      ;##get the bnd indices for the full size image 
       tmpmask = mask*0b
       tmpbox  = box*0b
       tmpbox[bound_ind]=1b
       tmpmask[minX-10:maxX+10-1,minY-10:maxY+10-1]=tmpbox
       bnd_ind_full = ORDER_IND(WHERE(tmpmask),xsize,ysize)        

        
      ;####Get the filament area in deg2
       
       area_deg2    = GET_FIL_AREA(bnd_ind_full,nax1,nax2,cd1,cd2, $
                                           cenx,ceny,rsun,dateo) 


       ;area_sergei  = GET_HELAREA(indices,nax1,nax2,cenx,ceny,cd1,cd2,dateo,res=0.1)


      ;#######Get the skeleton length (pixel) and center
      gspp          = GET_SKE_PROPERTIES_PIX(ske_ind,xsizeb,ysizeb)
      ske_len_pix   = gspp[0]
      ske_cen_pix   = [gspp[1] MOD xsizeb+minX-10,gspp[1]/xsizeb+minY-10]

      ;#######Get the skeleton center in arcs
      ske_cen_arc   = [DOUBLE(cd1*(ske_cen_pix[0]-cenx)),$
                       DOUBLE(cd2*(ske_cen_pix[1]-ceny))]


      ;#######Get the skeleton length in degrees and center in carrington
      ske_len_deg   = GET_SKE_LENGTH_DEG(ske_ind_full,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo)
      ske_cen_car   = PIX2CARR([LONG(ske_cen_pix[0]) + $
                      LONG(ske_cen_pix[1])*xsize],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo)


      ;#######blob area in pixels
      area_pix      = N_ELEMENTS(indices)

   ;PRINT,area_deg2,area_sergei,area_deg2/area_sergei,ske_cen_arc[0],ske_cen_arc[1]

;OLD WAY TO CALCULATE AREA IN DEG2-> WRONG
;      ;#######blob area in deg2 (computation depends on the length
;      ;#######of the filament and on distance from Sun center )
;      radarc = SQRT(ske_cen_arc[0]^2+ske_cen_arc[1]^2)
;
;      IF ske_len_deg GT 5. AND radarc GT 650. AND (ske_len_deg GT 20. $
;      OR radarc GT 850.) THEN BEGIN
;
;
;         ;#######blob area in deg2
;         area_deg2    = GET_FIL_AREA_DEG2(indices,nax1,nax2,cd1,cd2, $
;                                           cenx,ceny,rsun,dateo) 
;
;      ENDIF ELSE BEGIN
;
;
;         ;#######blob area in deg2 approx (centroid pixel size is applied to
;         ;#######the entire blob)
;         area_deg2    = GET_FIL_AREA_DEG2_APPROX(indices,nax1,nax2,cd1, $
;                                                 cd2,cenx,ceny,rsun,dateo)
; 
;      ENDELSE

;print,area_deg2;,area_deg2,(area_deg2a/area_deg2)

      ;#######test the error made with get_fil_area_deg2_aprox
      ;area_deg2_test    = GET_FIL_AREA_DEG2(indices,nax1,nax2,cd1, $
      ;                                  cd2,cenx,ceny,rsun,dateo)
      ;print,ske_len_deg,radarc,100.*ABS(area_deg2-area_deg2_test)/area_deg2_test



      ;#######centroid coordinates in pixel
      ;centroi_pix   = [Total(Total(box,2)*Indgen(xsizeb))/FLOAT(area_pix)+minX-10, $
      ;                Total(Total(box,1)*Indgen(ysizeb))/FLOAT(area_pix)+minY-10]

      ;#######in heliographic
      ;centroi_hel   = PIX2CARR([LONG(centroi_pix[0])+ $
      ;                LONG(centroi_pix[1])*xsize],h,FIXL0=0.)

      ;#######Thickness in pixels
      thick = ec

      ;#######Elongation
      elong         = FLOAT(area_pix)/((2.*ec)^2)


      ;######Compute an indice of the curvature (between 0 and 10)
      ;######if elong is bigger than 1.
      IF elong GE 1. THEN BEGIN
        ;ordered_ske = ORDER_IND(ske_ind,xsizeb,ysizeb)
        resampl_ske = RESAMPLE_IND(ske_ind,5)
        curv        = GET_CURVATURE_IND(resampl_ske,xsizeb,ysizeb)
      ENDIF ELSE curv = -1


      ;######define the string format of chain codes
      nbndc = N_ELEMENTS(bnd_chain)
      nskec = N_ELEMENTS(ske_chain)
      bndform = '('+STRTRIM(nbndc,2)+'I1)'
      skeform = '('+STRTRIM(nskec,2)+'I1)'


      ;#######Create a structure / fill it's fields
      A = strfil

      ;A.IND_FEAT                 = cmpt
      A.GRAV_C_ARCX              = ske_cen_arc[0]
      A.GRAV_C_ARCY              = ske_cen_arc[1]
      A.GRAV_C_CAR_LAT           = ske_cen_car[0]
      A.GRAV_C_CAR_LON           = ske_cen_car[1]
      A.SAMPLECOUNT              = area_pix
      A.AREA                     = area_deg2
      A.MEAN_INT_RATIO           = mir
      A.BRARC_X_LL               = bra[0]
      A.BRARC_Y_LL               = bra[1]
      A.BRARC_X_UR               = bra[2]
      A.BRARC_Y_UR               = bra[3]
      A.BRPIX_X_LL               = brp[0]
      A.BRPIX_Y_LL               = brp[1]
      A.BRPIX_X_UR               = brp[2]
      A.BRPIX_Y_UR               = brp[3]
      A.FEAT_MAX_INT             = maxval
      A.FEAT_MIN_INT             = minval
      A.FEAT_MEAN_INT            = meanval
      A.COD_PIX_X                = bnd_chain_strt_pix[0]
      A.COD_PIX_Y                = bnd_chain_strt_pix[1]
      A.COD_ARC_X                = bnd_chain_strt_arcs[0]
      A.COD_ARC_Y                = bnd_chain_strt_arcs[1]
      A.SKE_LEN_DEG              = ske_len_deg
      A.THICKNESS_PIX            = thick
      A.CURVATURE                = curv
      A.ELONG                    = elong
      A.ORIENTATION              = orien
      A.COD_SKE_PIX_X            = ske_chain_strt_pix[0]
      A.COD_SKE_PIX_Y            = ske_chain_strt_pix[1]
      A.COD_SKE_ARC_X            = ske_chain_strt_arcs[0]
      A.COD_SKE_ARC_Y            = ske_chain_strt_arcs[1]
      A.CHAIN_CODE               = STRING(bnd_chain,FORMAT=bndform)
      A.CCODE_LNTH               = nbndc
      A.CHAIN_CODE_SKE           = STRING(ske_chain,FORMAT=skeform)
      A.CCODE_SKE_LNTH           = nskec
      ;A.RASTER_SCAN_LNTH         = (brp[2]-brp[0])*(brp[3]-brp[1])

      ;A.CHAIN_CODE               = STRING(bnd_chain,FORMAT='(10000I1)')
      ;A.CHAIN_CODE_SKE           = STRING(ske_chain,FORMAT='(10000I1)')

      ;######test to suppress sunspots with circularity and
      ;######size criteria
      ;circ= (4.*!pi*area_pix)/(N_ELEMENTS(bound_ind))^2 ;circularity
      ;print,'circularity',circ
      ;print,'aire',area_pix
      
      ;IF circ LT AND cmptnss LT AND area_pix GT THEN B=[B,A]
      B=[B,A]

     ENDIF

endf:

   ENDFOR

   B=B[1:N_ELEMENTS(B)-1]

RETURN,B
END




