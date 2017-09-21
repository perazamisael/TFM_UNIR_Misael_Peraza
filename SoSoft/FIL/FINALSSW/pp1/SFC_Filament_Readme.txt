
README FILE FOR FILAMENT AUTOMATIC DETECTION AND DESCRIPTION
------------------------------------------------------------

  ! you need IDL with SolarSoft library !
 
  This Readme file describes the programs written in the frame of the
  EGSO Solar Feature Catalog to automatically detect and describe
  Solar  filaments on Sun Halpha full disk observations. The first
  part concerns the segmentation of Sun observations in Halpha, the
  second one the description of the segmented features, the third part
  is an example using these codes and and last part shortly describes
  auxilliary programs

  For a more complete description, one can refer to the following
  paper: "Filament Recognition and Image Cleaning On Meudon Halpha
  Spectroheliograms" by N. Fuller, J. Aboudarham and R.D. Bentley,
  Solar Physics 2005.

  We appreciate your comments on any bugs and problems you may
  encounter with this software, which we will promptly try to fix.
  The contact e-mails are nicolas.fuller@obspm.fr and
  jean.aboudarham@obspm.fr


PART I. Image segmentation
--------------------------
  
  IDL FILES LIST   

  Name                              Type          Modified     Size

  egso_sfc_filament.pro        IDL source file   05/03/2005    6KB
  egso_sfc_fil_region_grow.pro IDL source file   05/03/2005    5KB
  egso_sfc_fil_seeds.pro       IDL source file   05/03/2005    4KB
  egso_sfc_flatten.pro         IDL source file   05/03/2005    5KB
  egso_sfc_limbsym.pro         IDL source file   05/03/2005    3KB
  egso_sfc_line_direction.pro  IDL source file   05/03/2005    5KB
  egso_sfc_line_remove.pro     IDL source file   05/03/2005    7KB
  egso_sfc_obs_param.sav       IDL save   file   05/03/2005    3KB
  egso_sfc_roundmask.pro       IDL source file   05/03/2005    3KB
  egso_sfc_sharpen.pro         IDL source file   05/03/2005    2KB
  egso_sfc_dust_clean.pro      IDL source file   05/03/2005    3KB 


  INSTRUCTIONS

  One can use the main function EGSO_SFC_FILAMENT.PRO to get a
  segmented result (Filament pixels set to 1 and other to 0)
  providing that the input image is standardized (see EGSO
  standardization software). The images should be 1024*1024 pixels
  with Sun disk centred (511.5,511.5) and with a diameter of 840 pixels.
 
  The syntax is:

  bin_image = EGSO_SFC_FILAMENT(std_image[,DISPLAY=display,OBSKEY=obskey,
              FLATCLEAN=flatclean,LINECLEAN=lineclean,DUSTCLEAN=dustclean
              CLEANIM=cleanim])

  where bin_image is a 1024*1024 byte array and std_image the
  standardized image. Four options can be set:

  /DISPLAY   : Displays the different steps of program execution in an
               IDL window. 
 
  /LINECLEAN : Try to detect dark lines on the image and remove
               them. If your images are free from spurious lines
               don't use this option

  /FLATCLEAN : Flatten the Sun background. Gives better
               segmentation results with all the images tested so far.

  /DUSTCLEAN : Removes high gradient pixel values due to dust for example.
 

  CLEANIM=cleanim is an output keyword useful to retrieve the cleaned
  image. Set it to a named variable that receives the cleaned image
  (i.e flatten, dust points removed and dark lines removed, depending on
  the keywords which were set)
 

  The program uses coefficients to segment filaments (stored in 
  EGSO_SFC_OBS_PARAM.SAV). For better results these coefficients should
  be optimized for the type of observation you want to process. For
  example, to process an observation from Meudon Observatory using CCD 
  camera, one should set OBSKEY to 'meu2'. If not the default
  coefficients are used. All kind of Halpha observations have not been
  tested yet. The following list gives the possible entries for OBSKEY keyword:
 
  - OBSKEY='meu1' for Meudon Observatory photographic plate observations 
  - OBSKEY='meu2' for Meudon Observatory CCD observations
  - OBSKEY='bbso' for Big Bear Solar Observatory CCD observations
  - OBSKEY='ynao' for Yunnan Observatory CCD observations

  All these coefficients may be updated and other should be added, feel
  free to send us your updated or new coefficients.  



PART II. Filament description
-----------------------------


  IDL FILES LIST   

  Name                              Type           Modified   Size

  egso_sfc_fil_describe.pro     IDL source file   05/03/2005  17KB
  egso_sfc_area_deg2.pro        IDL source file   05/03/2005   3KB
  egso_sfc_chain2ind.pro        IDL source file   05/03/2005   1KB
  egso_sfc_chain_code.pro       IDL source file   05/03/2005   2KB
  egso_sfc_curl_ind.pro         IDL source file   05/03/2005   1KB
  egso_sfc_dforientation.pro    IDL source file   05/03/2005   2KB
  egso_sfc_inner_boundary.pro   IDL source file   05/03/2005   2KB
  egso_sfc_lake2bay.pro         IDL source file   05/03/2005   3KB
  egso_sfc_length_deg.pro       IDL source file   05/03/2005   3KB
  egso_sfc_length_pix.pro       IDL source file   05/03/2005   3KB
  egso_sfc_m_connect.pro        IDL source file   05/03/2005   3KB
  egso_sfc_order_ind.pro        IDL source file   05/03/2005   2KB
  egso_sfc_outer_boundary.pro   IDL source file   05/03/2005   2KB
  egso_sfc_pix2carr.pro         IDL source file   05/03/2005   4KB
  egso_sfc_pixcumul.pro         IDL source file   05/03/2005   2KB
  egso_sfc_pruning.pro          IDL source file   05/03/2005   2KB
  egso_sfc_resample_ind.pro     IDL source file   05/03/2005   1KB
  egso_sfc_skeleton.pro         IDL source file   05/03/2005   3KB


  INSTRUCTIONS

  One can use the main function EGSO_SFC_FIL_DESCRIBE.PRO to get a
  structure describing each segmented feature.
 
  The syntax is:

   str = EGSO_SFC_FIL_DESCRIBE(indices,dateobs[,IMAGE=image
         CDELT=cdelt,NOCLOSING=noclosing])

  Where indices is a 1D vector of pixel indices that describe the feature(s).
  For example, the indices may be returned as a result of the WHERE
  function (of the 1024*1024 array result of EGSO_SFC_FILAMENT for example).
  dateobs is the date of observation (ex. 2002-03-01T08:12:00.000).  
  For example the DATE_OBS or DATE_START keyword of the FITS
  header. One can use dateobs = STRTRIM(FXPAR(header,'DATE_OBS'),2)


  Three options can be set:
  
   IMAGE : Original or cleaned image leading to the segmentation in input 
           If Image is not set, a few parameters like mean intensity,
           max intensity etc... will not be computed

   CDELT  : Arcsec/pixel ratio, if not set the program computes
            it with Pb0r function from date input 

   NOCLOSING: Use only if a morphological closing was
              previously applied to the segmented features. The closing
              smoothes the features contours. If features contours are too
              complex the program may return an error message     

   The resulting structure contains the following values fo each feature:
 
       IND:            Indice in the structure
       GRAV_C_ARCX:    Gravity center X position (arcsecs) (*)
       GRAV_C_ARCY:    Gravity center Y position (arcsecs) (*)
       GRAV_C_CAR_LAT: Gravity center Carrington latitude  (*)
       GRAV_C_CAR_LON: Gravity center longitude            (*)
       SAMPLECOUNT:    Count of pixels
       AREA:           Area in square degrees
       MEAN_INT_RATIO: Mean intensity to Quiet Sun Intensity ratio
       BRARC_X_LL:     Bounding rectangle lower left corner X pos.(arcsec)
       BRARC_Y_LL:     Bounding rectangle lower left corner Y pos.(arcsec)
       BRARC_X_UR:     Bounding rectangle upper right corner X pos.(arcsec)
       BRARC_Y_UR:     Bounding rectangle upper right corner Y pos.(arcsec)
       BRPIX_X_LL:     Bounding rectangle lower left corner X pos.(pixels)
       BRPIX_Y_LL:     Bounding rectangle lower left corner Y pos.(pixels)
       BRPIX_X_UR:     Bounding rectangle upper right corner X pos.(pixels)   
       BRPIX_Y_UR:     Bounding rectangle upper right corner Y pos.(pixels)   
       FEAT_MAX_INT:   Maximum intensity (**)   
       FEAT_MIN_INT:   Minimum intensity (**)   
       FEAT_MEAN_INT:  Mean intensity (**)   
       COD_PIX_X:      Boundary chain code 1st pixel X pos.(pixels)    
       COD_PIX_Y:      Boundary chain code 1st pixel Y pos.(pixels)     
       COD_ARC_X:      Boundary chain code 1st pixel X pos.(arcsec)     
       COD_ARC_Y:      Boundary chain code 1st pixel Y pos.(arcsec)     
       SKE_LEN_DEG:    Skeleton length in degrees   
       THICKNESS_PIX:  Thickness in pixels   
       CURVATURE:      Curvature index 
       ELONG:          Elongation factor   
       ORIENTATION:    Orientation (in degrees counterclockwise)   
       COD_SKE_PIX_X:  Skeleton chain code 1st pixel X pos.(pixels)   
       COD_SKE_PIX_Y:  Skeleton chain code 1st pixel Y pos.(pixels)    
       COD_SKE_ARC_X:  Skeleton chain code 1st pixel X pos.(arcsec)    
       COD_SKE_ARC_Y:  Skeleton chain code 1st pixel Y pos.(arcsec)    
       CHAIN_CODE:     Boundary chain code   
       CCODE_LNTH:     Boundary chain code length  
       CHAIN_CODE_SKE: Skeleton chain code  
       CCODE_SKE_LNTH: Skeleton chain code length  

       (*)  For filaments the gravity center is replaced by the
            skeleton center
       (**) Only available with the image input keyword 



PART III. Example
-----------------

   1. First retrieve a standardize image with the READFITS function
      (and header)

      IDL> stdim=READFITS(DIALOG_PICKFILE(PATH='.../EGSO/FITS/PROCESSED')
                 ,header)
           ;try with Example_bbso_cleaned.fits
           ;see Example_bbso_cleaned.jpg

   2. Segment image (no dust lines or pixels) and display the result

      IDL> binim=EGSO_SFC_FILAMENT(stdim,OBSKEY='bbso',/fla,
                 CLEANIM=clim,/dis)
      (Approx. 8 sec. for a BBSO image with 480 seed regions detected
       resulting in 27 filaments; longer if some specific cleaning is needed)
          ;clim  -> see Example_bbso_cleaned2.jpg
          ;binim -> see Example_bbso_segm.jpg

   3. Retrieve the date of observation from the header

      IDL> date = STRTRIM(FXPAR(header,'DATE_OBS'),2)

   4. Describe the features using the cleaned image to compute
      intensity parameters

      IDL> str=EGSO_SFC_FIL_DESCRIBE(WHERE(binim),date,IMAGE=clim)
      (Approx. 4 sec. for image mentioned above)

   5. To display the chain codes (boundary and skeleton) one can use
      egso_sfc_chain2ind function

      IDL> nbfeat = MAX(FIX(str[*].ind)) ;one way to count features
      IDL> disp = BYTARR(1024,1024)      ;create an image for display
      IDL> FOR i=0,nbfeat-1 DO disp[EGSO_SFC_CHAIN2IND([str[i].cod_pix_x,
           str[i].cod_pix_y],str[i].chain_code,1024,1024)]=1b ;boundary
      IDL> FOR i=0,nbfeat-1 DO disp[EGSO_SFC_CHAIN2IND([str[i].cod_ske_pix_x,
           str[i].cod_ske_pix_y],str[i].chain_code_ske,1024,1024)]=1b ;skeleton
      IDL>WINDOW,/FREE,XS=1024,YS=1024 
      IDL>TVSCL,disp            ;see Example_bbso_skelet.jpg


 
 
PART IV. Other Programs Description
------------------------------------


  Programs can be run separately. Here is a short description of each
  one of them (for a more complete description see the programs headers).


  1. EGSO_SFC_FLATTEN.PRO
    
     Description:
       Normalize the intensity over the solar disk using
       advance median filtering.

     Calling:
       res = egso_sfc_flatten(image,diam[,DISPLAY=display])

 

  2. EGSO_SFC_LINE_REMOVE.PRO

     Description:
       This intend to remove dust lines on 
       standardized solar images using threshold/thinning/preferential
       line orientation query. It then replace possible line pixel
       values according to their neighbors values.

     Calling:
       res = egso_sfc_line_remove(imin,diam[,FLATOK=flatok,NOFLATRES=
             noflatres,DISPLAY=display,MAIND=maind])
    

  3. EGSO_SFC_DUST_CLEAN.PRO
  
     Description:
       This intend to remove dust points on solar images
       using median filtering (replace dust pixel values according
       to their neighboors values). Mainly for photographic plate
       observations

     Calling:
       res = egso_sfc_dust_clean(imin[,HIGHVAL2=highval2,DISPLAY=display])



  4. EGSO_SFC_SHARPEN.PRO
  
     Description:
       Sharpen the image using High-boost filtering

     Calling:
       res = egso_sfc_sharpen(imin,HBcoeff)


  
  5. EGSO_SFC_FIL_SEEDS.PRO
  
     Description:
       From a standardized Halpha Sun image, find seed pixels to be
       used by the region growing function to detect filaments

       NOTE: This program should be called by EGSO_SFC_FILAMENT. If
       not, the input image might be first flatten
       (egso_sfc_flatten), and enlarged (egso_sfc_limbsym)

     Calling:
       res = egso_sfc_fil_seeds(im_in,diam,coeff)



  6. EGSO_SFC_FIL_REGION_GROW.PRO
  
     Description:
       This function grows the seeds regions to filaments and return
       a byte array  with filament pixels set to 1 and other to 0.
       See also EGSO_SFC_FILAMENT and EGSO_SFC_FIL_SEEDS for the seeds. 

     Calling:
       res = egso_sfc_fil_region_grow(imori,imseed,diam,coeff)



  7. EGSO_SFC_ROUNDMASK.PRO
  
     Description:
       Get the subscripts of pixels in a ring (btw minr and maxr)
       in an image of size (xsize,ysize) and centered at 
       [(xsize-1)/2.,(ysize-1/)2.] (or set xcen / ycen keyword) 

     Calling:
         res = egso_sfc_roundmask(xsize,ysize,minr,maxr[,XCEN=xcen,
               YCEN=ycen,COMP=comp])



  8. EGSO_SFC_LIMBSYM.PRO
  
     Description:
        Symmetry in relation to the limb:
        copy a ring (limb to limb + delta)
        inside the solar disk (-> e.g. for prominences)
        If keyword 'inverse' is set, copy a
        ring (limb - delta to limb) outside the disk
        (->e.g. to avoid border effects)

     Calling:
       res = egso_sfc_limbsym(imin,limb,delta[,INVERSE=inverse,
             XCEN=xcen,YCEN=ycen])


  9. EGSO_SFC_LINE_DIRECTION.PRO
  
     Description:
       Given the subscripts of pixels corresponding to
       thinned shapes on the Sun disk, find the possible
       lines, and return these lines pixels subscripts and eventually
       their main direction.(see egso_sfc_line_remove for input) 
        
     Calling:
        subsout = egso_sfc_line_direction(subsin,diam,xs,ys[,MAIND=maind])


 10. EGSO_SFC_AREA_DEG2.PRO
 
  
     Description:
         Computes the area in heliographic square degrees

     Calling:
         res = egso_sfc_area_deg2(indices,nax1,nax2,cd1,cd2,
                                      cenx,ceny,rsun,dateo)


 11. EGSO_SFC_CHAIN2IND.PRO

  
     Description:
        From a first point (first) and a chain code (chainc),
        find the corresponding indices in the array
        of size (xsize,ysize)

     Calling:
        res = egso_sfc_chain2ind(first,chainc,xsize,ysize
                            [,START_RIGHT=start_right])
 
    Example:

     IDL> ind1 = EGSO_SFC_CHAIN2IND([10,10],'6666666666',50,50)
     IDL> ind2 = EGSO_SFC_CHAIN2IND([14,10],'66666666664443332222111000',50,50)
     IDL> ind3 = EGSO_SFC_CHAIN2IND([29,10],'000006666666666',50,50)           
     IDL> imi = BYTARR(50,50)                                                  
     IDL> imi[[ind1,ind2,ind3]] = 1b                                           
     IDL> TVSCL,REBIN(imi,200,200,/SAMPLE) 


 12.  EGSO_SFC_CHAIN_CODE.PRO

  
     Description:
        Computes the chain code (freeman's code) of the 
        chain pixels corresponding to indices. Could be
        a boundary or a pruned skeleton
        subscripts MUST be ordered (egso_sfc_order_ind)

     Calling:
        res = egso_sfc_chain_code(indices,xsize,ysize
              [,START_RIGHT=start_right])


 13. EGSO_SFC_CURL_IND.PRO

  
     Description:
        Gives an index of how much the skeleton
        is curled up based on the ratio between its length and
        the distance between end points

     Calling:
        res = egso_sfc_curl_ind(indices,xsize,ysize)



 14.  EGSO_SFC_DFORIENTATION.PRO

  
     Description:
        Find the main orientation of a region      

     Calling:
        ori = EGSO_SFC_DFORIENTATION(indices,xsize,ysize)



 15.  EGSO_SFC_INNER_BOUNDARY.PRO 
  
  
     Description:
          Find the inner boundary of the object defined by
          indices using erosion.

     Calling:
          res = egso_sfc_inner_boundary(indices,xsize,ysize
                 [,ALLDIR=alldir])


 16.  EGSO_SFC_LAKE2BAY.PRO

  
     Description:
         Before computing the boundary of the shape
         check the presence  of lake-like filaments
         (which lead to 2 different boundaries, outer and inner).
         Fill small gaps (see ratio keywd) or open large lakes

     Calling:
         res = egso_sfc_lake2bay(indices,xsize,ysize[,RATIO=ratio])


 17. EGSO_SFC_LENGTH_DEG.PRO

  
     Description:
         From the pixel chain computes the length in degrees of the chain. 
         The chain can be a boundary or a pruned skeleton
         Subscripts MUST be ordered (cf EGSO_SFC_ORDER_IND)

     Calling:
         res = egso_sfc_length_deg(indices,nax1,nax2,cd1,cd2,
                               cenx,ceny,rsun,dateo[,BND=bnd])


 18.  EGSO_SFC_LENGTH_PIX.PRO   
 
  
     Description:
         From the pixel chain computes the length in pixels. 
         The chain can be a boundary or a pruned skeleton
         Subscripts MUST be ordered (cf EGSO_SFC_ORDER_IND)

     Calling:
         res = egso_sfc_length_pix(indices,xsize,ysize[,SMP=smp])


 19.  EGSO_SFC_M_CONNECT.PRO
 
  
     Description:
          Delete pixels of a one pixel thick chain
          (from indices) to make the chain m_connected

     Calling:
          res = egso_sfc_m_connect(indices,xsize,ysize[,DISP=disp])


 20.  EGSO_SFC_ORDER_IND.PRO
  
  
     Description:
        Order subscripts of a 1 pixel thick chain (ex:pruned skeleton)
        from a first one to the successive neighbors
        In the case of a boundary (ie no end points), the first point
        is the smallest indice Pixels MUST be M-connected 
        (see egso_sfc_m_connect)  

     Calling:
        res = egso_sfc_order_ind(indices,xsize,ysize)


 21.  EGSO_SFC_OUTER_BOUNDARY.PRO
  
  
     Description:
         Find the external boundary of the object defined by
         indices using subscripts shifting in 4 or 8 directions

     Calling:
        res = egso_sfc_ext_boundary(indices,xsize,ysize[,ALLDIR=alldir])


 22.  EGSO_SFC_PIX2CARR.PRO
 
  
     Description:
        Convert pixel coordinate to Carrington or heliographic

     Calling:
        res = EGSO_SFC_PIXCARR(indices,nax1,nax2,cd1,cd2,
                        cenx,ceny,rsun,dateo[,fixl0=fixl0]) 


 23.  EGSO_SFC_PIXCUMUL.PRO 
  
  
     Description:
         Assign a value to every pixel of a chain
         depending on the count of its non-null
         M-connected neighbors

     Calling:
         res = egso_sfc_pixcumul(indices,xsize,ysize)


 24.  EGSO_SFC_PRUNING.PRO

  
     Description:
         From the pixel skeleton defined by indices, computes
         the fully pruned skeleton by calculating distance
         btw node points and end points, and removing pixels
         on this criterion.

     Calling:
         res = egso_sfc_pruning(indices,xsize,ysize[,CITY=city])


 25.  EGSO_SFC_RESAMPLE_IND.PRO 

  
     Description:
         Reduce the number of pixel subscripts by factor         
         Subscripts are picked up at regular interval 
         The new number of subscripts is:
         FIX(N_ELEMENTS(indices)/factor)

     Calling:
         res = egso_sfc_resample_ind(indices,factor)


 26.  EGSO_SFC_SKELETON.PRO  
  
     Description:
         Erode successively the shape corresponding 
         to the indices until the shape is reduced to
         its one pixel thick skeleton.
         The structures used to erode the shape are
         the following (cf. Hit-or-Miss transform):
          0 0 0     x 0 0
          x 1 x     1 1 0
          1 1 1     x 1 x
         and the four 90� rotations of them
         x stands for "don't care" values

     Calling:
         res = egso_sfc_skeleton(indices,xsize,ysize[,EC=ec])

