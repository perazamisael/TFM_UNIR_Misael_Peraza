@/home/fuller/IDL/FIL/efr_shape_lib.pro
@/home/fuller/IDL/FIL/Fit_Ellipse.pro

FUNCTION EFR_FIL_DESCRIBE,indices,strfil,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,qsi,cleanim,maind


      ;######Image size
      xsize = nax1
      ysize = nax2


      ;######Draw the filament binary image
      mask  = bytarr(xsize,ysize)
      mask[indices] = 1b



      ;######Create a structure that will contain
      ;######all the blobs info
      B = strfil
 

      ;######Label the blobs to get indices of each
      r_l   = LABEL_REGION(mask,/all_neigh)
      h_r_l = HISTOGRAM(r_l,reverse_indices=r)

      cmpt=0

window,11,xsize=1024,ysize=1024
testmask = cleanim
TVSCL,testmask

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
       ;boxind = EFR_CHECK_FILLING(boxind,xsizeb,ysizeb)
       boxind = EFR_LAKE2BAY(boxind,xsizeb,ysizeb)
       box[boxind]=1b

      
       ;###### Compute the skeleton
       ;###### ec is the number of erosion needed to completly
       ;###### erode the structure 
       ske_ind = EFR_SKELETON(boxind,xsizeb,ysizeb,EC=ec)
       ske_ind = EFR_M_CONNECT(ske_ind,xsizeb,ysizeb,/nodis)
       ske_ind = EFR_PRUNING(ske_ind,xsizeb,ysizeb)   
       ske_ind = EFR_M_CONNECT(ske_ind,xsizeb,ysizeb,/nodis);inserer dans pruning?
       ske_ind = EFR_ORDER_IND(ske_ind,xsizeb,ysizeb)


     IF N_ELEMENTS(ske_ind) GT 3 THEN BEGIN
       
       cmpt = cmpt + 1

       ;######Compute the boundary

       ;#####Boundary: first test the minimum thickness
       ;#####of the blob
       r_lbox = LABEL_REGION(box) ;4 connected regions
       nregb  = N_ELEMENTS(HISTOGRAM(r_lbox))
       ;#####If there are more than 2 regions then there are
       ;#####1 pixel thick places -> dilate the blob
       ;#####and get the inner boundary
       ;#####If not get the external boundary
       IF nregb EQ 2 THEN BEGIN
          bound_ind  = EFR_EXT_BOUNDARY(boxind,xsizeb,ysizeb)  
       ENDIF ELSE BEGIN
          boxdilateind = WHERE(DILATE(box,REPLICATE(1,3,3)))
          ;make sure we didn't create a bay object with the dilation
          boxdilateind = EFR_LAKE2BAY(boxdilateind,xsizeb,ysizeb)
          bound_ind    = EFR_INNER_BOUNDARY(boxdilateind,xsizeb,ysizeb)
       ENDELSE
       bound_ind  = EFR_M_CONNECT(bound_ind,xsizeb,ysizeb,/nodis)     
       bound_ind  = EFR_ORDER_IND(bound_ind,xsizeb,ysizeb)


       ;######Get the first points for the chain codes       
       ske_chain_strt  = ske_ind[0]
       bnd_chain_strt  = bound_ind[0]


       ;######Compute the chain codes
       ske_chain  = EFR_CHAIN_CODE(ske_ind,xsizeb,ysizeb)
       bnd_chain  = EFR_CHAIN_CODE(bound_ind,xsizeb,ysizeb)


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

      ;######Test to discard remaining false detection due to
      ;######dust lines
        nskec      = N_ELEMENTS(ske_chain)
        IF STRING(maind) NE '\N' THEN BEGIN
          IF ABS((orien+360) MOD 180 - maind) LT 6. AND ec LT 6 THEN BEGIN
            skec_hist  = HISTOGRAM(ske_chain)
            wdirect    = WHERE(skec_hist GE nskec/10,ndirect)
            wcomp      = WHERE(skec_hist LT nskec/10 AND skec_hist GT 0,nwcomp)
            IF nwcomp NE 0 THEN nother = TOTAL(skec_hist[wcomp]) ELSE nother=0
            IF (ndirect EQ 2 OR ndirect EQ 1) AND nskec GT 60 AND nother LT nskec/10 THEN BEGIN
             PRINT,'wwwwwww line detected from chain code, discarded! wwwwwwww'
             PRINT,'Skeleton number of pixels:',nskec
	     PRINT,'thickness (numb of erosions to get skeleton):',ec
             PRINT,'Number of chain code main directions:',ndirect
             PRINT,'number of elements for the main directions:',TOTAL(skec_hist[wdirect])
             PRINT,'Number of chain code divergent directions:',nwcomp
             PRINT,'Divergent direction number of elements',nother
             PRINT,'Dust lines Maind (deg)',maind
             PRINT,'Skeleton orien (deg)',(orien+360) MOD 180
             PRINT,'position (first pixel)',ske_chain_strt_pix
             PRINT,'wwwwwwwwwwwwwwwwwwwwwwwwwww'
             GOTO,endf
            ENDIF
          ENDIF
       ENDIF



      ;##get the ske indices for the full size image
       tmpmask = mask*0b
       tmpbox  = box*0b
       tmpbox[ske_ind]=1b
       tmpmask[minX-10:maxX+10-1,minY-10:maxY+10-1]=tmpbox
       ske_ind_full = EFR_ORDER_IND(WHERE(tmpmask),xsize,ysize) 


      ;##get the bnd indices for the full size image 
       tmpmask = mask*0b
       tmpbox  = box*0b
       tmpbox[bound_ind]=1b
       tmpmask[minX-10:maxX+10-1,minY-10:maxY+10-1]=tmpbox
       bnd_ind_full = EFR_ORDER_IND(WHERE(tmpmask),xsize,ysize) 

;#####TESTS 2 LINES    
testmask[ske_ind_full]=max(cleanim)*0.8
;testmask[bnd_ind_full]=max(cleanim)*0.8
TVSCL,testmask

      ;####Get the filament area in deg2
       area_deg2    = EFR_AREA_DEG2(bnd_ind_full,nax1,nax2,cd1,cd2, $
                                           cenx,ceny,rsun,dateo) 


       ;area_sergei  = GET_HELAREA(indices,nax1,nax2,cenx,ceny,cd1,cd2,dateo,res=0.1)


      ;#######Get the skeleton length (pixel) and center
      ske_len_pix   = EFR_LENGTH_PIX(ske_ind,xsizeb,ysizeb,SMP=smp)
      ske_cen_pix   = [smp MOD xsizeb+minX-10,smp/xsizeb+minY-10]


      ;#######Get the skeleton center in arcs
      ske_cen_arc   = [DOUBLE(cd1*(ske_cen_pix[0]-cenx)),$
                       DOUBLE(cd2*(ske_cen_pix[1]-ceny))]


      ;#######Get the skeleton length in degrees and center in carrington
      ske_len_deg   = EFR_LENGTH_DEG(ske_ind_full,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo)
      ske_cen_car   = PIX2CARR([LONG(ske_cen_pix[0]) + $
                      LONG(ske_cen_pix[1])*xsize],nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo)


      ;#######blob area in pixels
      area_pix      = N_ELEMENTS(indices)

   ;PRINT,area_deg2,area_sergei,area_deg2/area_sergei,ske_cen_arc[0],ske_cen_arc[1]


      ;#######Thickness in pixels
      thick = ec

      ;#######Elongation
      elong         = FLOAT(area_pix)/((2.*ec)^2)


      ;######Compute an indice of the curvature (between 0 and 10)
      ;######if elong is bigger than 1.
      IF elong GE 1. THEN BEGIN
        resampl_ske = EFR_RESAMPLE_IND(ske_ind,5) ;(necessary?faster?)
        curv        = EFR_CURL_IND(resampl_ske,xsizeb,ysizeb)
      ENDIF ELSE curv = -1

;      crit = 5.*mir+15.*(minval/qsi)+2.*(1./elong)+5.*(50./area_pix)
;print,'mir,min/qsi,elong,area,crit',mir,minval/qsi,elong,area_pix,crit

      ;######define the string format of chain codes
      nbndc = N_ELEMENTS(bnd_chain)
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
wset,10
RETURN,B
END




