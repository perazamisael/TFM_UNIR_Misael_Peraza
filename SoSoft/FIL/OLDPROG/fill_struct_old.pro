@/home/fuller/IDL/FIL/pruned_ske.pro
@/home/fuller/IDL/FIL/Fit_Ellipse.pro

FUNCTION FILL_STRUCT,struct,indices,h,cleanim


      ;######Image size
      xsize = h.NAXIS1 
      ysize = h.NAXIS2


      ;######Draw the filament binary image
      mask  = bytarr(xsize,ysize)
      mask[indices] = 1b


      ;######Get stat on the corrected image to
      ;######compute the relative strength of the blobs
       nonnul = WHERE(cleanim)
       hist = HISTOGRAM(cleanim[nonnul],min=0)
       qs = FLOAT(WHERE(hist EQ MAX(hist)))
       mom = MOMENT(cleanim(where(cleanim)),sdev=sdevc)


      ;######Create a structure that will contain
      ;######all the blobs info
      B = struct
 

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
       scale    = 10.
       nsd      = 6.
       meanval  = MEAN(cleanim[indices])
       strength = (FIX(scale*(qs-meanval)/(nsd*sdevc)) > 1 ) < 10
;print,'strength',strength

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


      ;#######give a bounding rectangle in arcsecs
      ;#######[mx,Mx,my,My]
       mmX = DOUBLE(h.CDELT1*([minX,maxX]-h.CENTER_X)+h.CRVAL1)
       mmY = DOUBLE(h.CDELT2*([minY,maxY]-h.CENTER_Y)+h.CRVAL2)          
       br  = [mmX,mmY]


      ;#######Check if the filament inside box
      ;#######contains holes and fill them
       boxind = CHECK_FILLING(boxind,xsizeb,ysizeb)
       box[boxind]=1b


      ;#######for the external boundary 
      ;#######(-> avoid problems with 1 pixel thick blob)
      ;essayer avec get_ext_boundary
       mask_b=DILATE(box,REPLICATE(1,3,3))
       dindices=WHERE(mask_b)


       ;###### Compute the skeleton
       ;###### ec is the number of erosion needed to completly
       ;###### erode the structure 
       ske_ind = PRUNED_SKE(boxind,xsizeb,ysizeb,EC=ec)


     IF N_ELEMENTS(ske_ind) GT 3 THEN BEGIN

       cmpt = cmpt + 1

       ;######compute the boundary
       bound_ind  = GET_BOUNDARY(dindices,4,xsizeb,ysizeb)     
       bound_ind  = M_CONNECT(bound_ind,xsizeb,ysizeb)        


       ;######compute the shape of the blob
       ;shape_type  = GET_SHAPE_TYPE(ske_ind,xsizeb,ysizeb)


       ;######Get the first points for the chain codes       
       mask_b = MIXVALUE(ske_ind,xsizeb,ysizeb)
       ske_chain_strt  = ([WHERE(mask_b EQ 2b)])[0]
       bnd_chain_strt  = bound_ind[0]


       ;######Compute the chain codes
       ske_chain  = CHAIN_CODE_SKE(ske_ind,ske_chain_strt,xsizeb,ysizeb)
       bnd_chain  = CHAIN_CODE_BND(bound_ind,bnd_chain_strt,xsizeb,ysizeb)


       ;######Get the blob orientation from Fit_Ellipse (D.Fanning)
       ;######Orientation is given in deg counter-clockwise from the X axis
       ell = FIT_ELLIPSE(boxind,Xsize=xsizeb,Ysize=ysizeb,Orientation=orien)


       ;#######Give the first points of chain codes in pixel coordinates
       ske_chain_strt_pix = [ske_chain_strt MOD xsizeb + minX - 10 ,$
                             ske_chain_strt /   xsizeb + minY - 10]
       bnd_chain_strt_pix = [bnd_chain_strt MOD xsizeb + minX - 10 ,$
                             bnd_chain_strt /   xsizeb + minY - 10]

      ;#######In arcsecs
       ske_chain_strt_arcs = [DOUBLE(h.CDELT1*(ske_chain_strt_pix[0]-h.CENTER_X)+h.CRVAL1),$
                              DOUBLE(h.CDELT1*(ske_chain_strt_pix[1]-h.CENTER_X)+h.CRVAL1)]
       bnd_chain_strt_arcs = [DOUBLE(h.CDELT1*(bnd_chain_strt_pix[0]-h.CENTER_X)+h.CRVAL1),$
                              DOUBLE(h.CDELT1*(bnd_chain_strt_pix[1]-h.CENTER_X)+h.CRVAL1)]


      ;##get the ske indices for the full size image
       tmpmask = mask*0b
       tmpbox  = box*0b
       tmpbox[ske_ind]=1b
       tmpmask[minX-10:maxX+10-1,minY-10:maxY+10-1]=tmpbox
       ske_ind_full = WHERE(tmpmask)


      ;#######Get the skeleton length (pixel) and center
      gspp          = GET_SKE_PROPERTIES_PIX(ske_ind,xsizeb,ysizeb)
      ske_len_pix   = gspp[0]
      ske_cen_pix   = [gspp[1] MOD xsizeb+minX-10,gspp[1]/xsizeb+minY-10]


      ;#######Get the skeleton length in degrees and center in heliographic
      ske_len_deg   = GET_SKE_LENGTH_DEG(ske_ind_full,h)
      ske_cen_hel   = PIX2CARR([LONG(ske_cen_pix[0]) + $
                      LONG(ske_cen_pix[1])*xsize],h,FIXL0=0.)


      ;#######blob area in pixels
      area_pix      = N_ELEMENTS(indices)



      ;#######blob area in deg2
      ;area_deg2     = GET_FIL_AREA_DEG2(indices,h) 


      ;#######blob area in deg2 approx (centroid is applied to
      ;#######the entire blob)
      area_deg2     = GET_FIL_AREA_DEG2_APPROX(indices,h)
 


      ;#######centroid coordinates in pixel
      centroi_pix   = [Total(Total(box,2)*Indgen(xsizeb))/FLOAT(area_pix)+minX-10, $
                      Total(Total(box,1)*Indgen(ysizeb))/FLOAT(area_pix)+minY-10]

      ;#######in heliographic
      centroi_hel   = PIX2CARR([LONG(centroi_pix[0])+ $
                      LONG(centroi_pix[1])*xsize],h,FIXL0=0.)


      ;#######Elongatedness
      elong         = FLOAT(area_pix)/((2.*ec)^2)


      ;######Compute an indice of the curvature (between 0 and 10)
      ;######if elong is bigger than 1.
      IF elong GE 1. THEN BEGIN
        ordered_ske = ORDER_IND(ske_ind,xsizeb,ysizeb)
        resampl_ske = RESAMPLE_IND(ordered_ske,5)
        curv        = GET_CURVATURE_IND(resampl_ske,xsizeb,ysizeb)
      ENDIF ELSE curv = -1



      ;#######Create a structure / fill it's fields
      A = struct
      A.srce = h      
      A.fil_index                = cmpt
print,'$$$$$$$$$$$$$$$$'
print,cmpt
print,'$$$$$$$$$$$$$$$$'
wait,1
      A.fil_area_pix             = area_pix
      A.fil_area_deg2            = area_deg2
      A.fil_bndg_rect_arcs       = br
      A.fil_ske_len_deg          = ske_len_deg
      A.fil_ske_cen_hel          = ske_cen_hel
      A.fil_curvature            = curv
      A.fil_ske_chain_strt_arcs  = ske_chain_strt_arcs
      A.fil_bnd_chain_strt_arcs  = bnd_chain_strt_arcs
      A.fil_ske_chain            = STRING(ske_chain,FORMAT='(10000I1)')
      A.fil_bnd_chain            = STRING(bnd_chain,FORMAT='(10000I1)')
      A.fil_centroi_hel          = centroi_hel
      A.fil_strength             = strength
      A.fil_elongatedness        = elong
      A.fil_orientation          = orien


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




