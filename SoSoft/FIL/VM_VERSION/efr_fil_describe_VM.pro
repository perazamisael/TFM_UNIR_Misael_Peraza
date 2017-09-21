@/home/fuller/IDL/FIL/Fit_Ellipse.pro
@/home/fuller/IDL/FIL/efr_shape_lib.pro

FUNCTION EFR_FIL_DESCRIBE_VM,indices,rg_ind,strfil,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo,qsi,cleanim,maind


      ;######Image size
      xsize = nax1
      ysize = nax2


      ;######Draw the filament binary image (morph_close)
      mask  = BYTARR(xsize,ysize)
      mask[indices] = 1b
      
      
      ;######Draw the filament binary image (region_grow result)
      ;rg_mask  = BYTARR(xsize,ysize)
      ;rg_mask[rg_ind] = 1b

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
       box    = BYTARR(xsizeb,ysizeb)
       box[coordX-(minX-10),coordY-(minY-10)] = 1b
       boxind = WHERE(box)
      


      ;#######give a bounding rectangle in pixels
       brp = [minX,minY,maxX,maxY]


      ;#######give a bounding rectangle in arcsecs
       mmX = DOUBLE(cd1*([minX,maxX]-cenx))
       mmY = DOUBLE(cd2*([minY,maxY]-ceny))          
       bra  = [mmX[0],mmY[0],mmX[1],mmY[1]]


      ;####### Gray scale sub image
       boxint = cleanim[minX-10:maxX+10-1,minY-10:maxY+10-1]
       
      ;###### Remove potential holes
       boxind = EFR_LAKE2BAY(boxind,xsizeb,ysizeb)
       
      ;###### compute the smoothed gray scale skeleton and prune the result
       ske_gray = EFR_SKELETON_GRAY(boxind,boxint,xsizeb,ysizeb,EC=ec,NLEVEL=7,smoothw=7)
       ske_gray = EFR_M_CONNECT(ske_gray,xsizeb,ysizeb,/nodis)
       p_ske_gray = EFR_PRUNING_GRAY(ske_gray,boxint,xsizeb,ysizeb)   
       p_ske_gray = EFR_ORDER_IND(p_ske_gray,xsizeb,ysizeb)
       
     IF N_ELEMENTS(p_ske_gray) GT 5 THEN BEGIN
     
       ;###### to find chirality we use the skeleton from
       ;###### region grown result, before morph_close operator
        ;rg_box = box + rg_mask[minX-10:maxX+10-1,minY-10:maxY+10-1]
	;rg_box[p_ske_gray] = 2
	;rg_boxind = WHERE(rg_box EQ 2)
	;rg_boxind = EFR_LAKE2BAY(rg_boxind,xsizeb,ysizeb)
	;rg_ske_gray = EFR_SKELETON_GRAY(rg_boxind,boxint,xsizeb,ysizeb)
        ;rg_ske_gray = EFR_M_CONNECT(rg_ske_gray,xsizeb,ysizeb,/nodis)
        ;p_rg_ske_gray = EFR_PRUNING_GRAY(rg_ske_gray,boxint,xsizeb,ysizeb)   
        ;p_rg_ske_gray = EFR_ORDER_IND(p_rg_ske_gray,xsizeb,ysizeb)
   ;masko=box*0b
   ;masko[rg_boxind]=1b
   ;masko[rg_ske_gray]=2b
   ;tvscl,masko
   ;wait,2    
  ; IF N_ELEMENTS(p_ske_gray) GT 150 THEN BEGIN
       ;chir = EFR_CHIRALITY(ske_gray,p_ske_gray,xsizeb,ysizeb)
   ;retall
;   ENDIF    
      ;chirality test
       ;res = EFR_CHIRALITY3(p_ske_gray,boxind,boxint,xsizeb,ysizeb)
       
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
       ske_chain_strt  = p_ske_gray[0]
       bnd_chain_strt  = bound_ind[0]


       ;######Compute the chain codes
       ske_chain  = EFR_CHAIN_CODE(p_ske_gray,xsizeb,ysizeb)
       bnd_chain  = EFR_CHAIN_CODE(bound_ind,xsizeb,ysizeb)

;chirality test
IF N_ELEMENTS(p_ske_gray) GT 200 THEN BEGIN
;res=EFR_CHIRALITY0(p_ske_gray,bound_ind,xsizeb,ysizeb)
;retall
ENDIF 
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
       tmpbox[p_ske_gray]=1b
       tmpmask[minX-10:maxX+10-1,minY-10:maxY+10-1]=tmpbox
       p_ske_gray_full = EFR_ORDER_IND(WHERE(tmpmask),xsize,ysize) 
       
;tmpmask = mask*0b
;tmpbox  = box*0b
;tmpbox[rg_ske_gray]=1b
;tmpbox[p_rg_ske_gray]=0b
;tmpmask[minX-10:maxX+10-1,minY-10:maxY+10-1]=tmpbox      
;rg_ske_gray_full = WHERE(tmpmask)

;tmpmask = mask*0b
;tmpbox  = box*0b
;tmpbox[ske_gray]=1b
;tmpmask[minX-10:maxX+10-1,minY-10:maxY+10-1]=tmpbox      
;ske_gray_full = WHERE(tmpmask)

;toto[ske_gray_full]=toto[ske_gray_full]+3000
;toto[p_ske_gray_full]=toto[p_ske_gray_full]+3000


      ;##get the bnd indices for the full size image 
       tmpmask = mask*0b
       tmpbox  = box*0b
       tmpbox[bound_ind]=1b
       tmpmask[minX-10:maxX+10-1,minY-10:maxY+10-1]=tmpbox
       bnd_ind_full = EFR_ORDER_IND(WHERE(tmpmask),xsize,ysize) 
       
       ;AFFICHAGE CHIRALITE SUR PLOT
;xloc = bnd_ind_full[0] MOD xsize
;yloc = bnd_ind_full[0] / xsize
;IF chir GT 0 THEN toto[xloc+1:xloc+7,yloc-1:yloc]=0;+ de dex que de sin => angle -
;IF chir LT 0 THEN BEGIN;+ de sin que de dex => angle +
;  toto[xloc+1:xloc+6,yloc-1:yloc]=0
;  toto[xloc+3:xloc+4,yloc-3:yloc+2]=0
;ENDIF
;IF chir EQ 0 THEN BEGIN
;  toto[xloc+1:xloc+6,yloc-3:yloc+3]=0
;  toto[xloc+3:xloc+4,yloc-1:yloc+3]=3000
;ENDIF


;toto[bnd_ind_full]=toto[bnd_ind_full]+2500
;tvscl,REBIN(toto,1024,1024)


;IF N_ELEMENTS(p_ske_gray) GT 150 THEN BEGIN
; TVSCL,toto[minX-10:maxX+10-1,minY-10:maxY+10-1]
; wait,5
;ENDIF
      ;####Get the filament area in deg2
       area_deg2    = EFR_AREA_DEG2(bnd_ind_full,nax1,nax2,cd1,cd2, $
                                           cenx,ceny,rsun,dateo) 


       ;area_sergei  = GET_HELAREA(indices,nax1,nax2,cenx,ceny,cd1,cd2,dateo,res=0.1)


      ;#######Get the skeleton length (pixel) and center
      ske_len_pix   = EFR_LENGTH_PIX(p_ske_gray,xsizeb,ysizeb,SMP=smp)
      ske_cen_pix   = [smp MOD xsizeb+minX-10,smp/xsizeb+minY-10]


      ;#######Get the skeleton center in arcs
      ske_cen_arc   = [DOUBLE(cd1*(ske_cen_pix[0]-cenx)),$
                       DOUBLE(cd2*(ske_cen_pix[1]-ceny))]


      ;#######Get the skeleton length in degrees and center in carrington
      ske_len_deg   = EFR_LENGTH_DEG(p_ske_gray_full,nax1,nax2,cd1,cd2,cenx,ceny,rsun,dateo)
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
        resampl_ske = EFR_RESAMPLE_IND(p_ske_gray,5) ;(necessary?faster?)
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
;wset,10
RETURN,B
END




