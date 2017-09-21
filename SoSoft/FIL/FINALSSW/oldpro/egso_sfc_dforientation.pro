;+
; NAME:
;
;    EGSO_SFC_DFORIENTATION
;
; PURPOSE:
;    
;    Find the main orientation of a region      
; 
; AUTHOR:
;
;       Adapted from D.Fanning Fit_Ellipse.pro
;       Fuller Nicolas
;       LESIA / POLE SOLAIRE
;       Observatoire de MEUDON
;       5 place Jules Janssen
;       92190 MEUDON
;       E-mail: nicolas.fuller@obspm.fr
;       copyright (C) 2005 Nicolas Fuller, Observatoire de Paris
;       This program is free software; you can redistribute it and/or modify it under the terms of the
;       GNU General Public License as published by the Free Software Foundation;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Solar Feature Catalog
;
; CALLING SEQUENCE:
;
;     ori = EGSO_SFC_DFORIENTATION(indices,xsize,ysize)
;
; INPUT
;
;     indices of pixels describing the region
;     xsize and ysize: dimensions of array
;
; OUTPUTS
;
;     orientation of the region in degrees CCWise
;    
; HISTORY:
;
;    NF last rev. mar 2005
;-

FUNCTION EGSO_SFC_DFORIENTATION,indices,xsize,ysize


   ;#### Find the center of mass
    array = BYTARR(xsize,ysize)
    array[indices] = 1b
    totalMass = TOTAL(array)
    xcm = TOTAL( TOTAL(array, 2) * INDGEN(xsize) ) / totalMass
    ycm = TOTAL( TOTAL(array, 1) * INDGEN(ysize) ) / totalMass
    center = [xcm, ycm]


   ;#### Obtain the position of every pixel in the image, with the origin
   ;#### at the center of mass of the ROI.
    x = FINDGEN(xsize)
    y = FINDGEN(ysize)
    xx = (x # (y * 0 + 1)) - xcm
    yy = ((x * 0 + 1) # y) - ycm
    npts = N_ELEMENTS(indices)


   ;#### Calculate the mass distribution tensor.
    i11 = TOTAL(yy[indices]^2) / npts
    i22 = TOTAL(xx[indices]^2) / npts
    i12 = -TOTAL(xx[indices] * yy[indices]) / npts
    tensor = [[ i11, i12],[i12,i22]]


   ;#### Find the orientation from the first eigenvector
    evals = EIGENQL(tensor, Eigenvectors=evecs)
    evec = evecs[*,0]


   ;#### Degrees counter-clockwise from the X axis.
    orientation = ATAN(evec[1], evec[0]) * 180. / !Pi - 90.0


RETURN,orientation
END

