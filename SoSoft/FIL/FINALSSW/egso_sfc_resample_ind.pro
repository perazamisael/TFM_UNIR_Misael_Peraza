;+
; NAME:
;         EGSO_SFC_RESAMPLE_IND
;
; PURPOSE:
;         Reduce the number of pixel subscripts by factor         
;         Subscripts are picked up at regular interval 
;         The new number of subscripts is:
;         FIX(N_ELEMENTS(indices)/factor)
;
; AUTHOR:
;
;       Fuller Nicolas
;       LESIA / POLE SOLAIRE
;       Observatoire de MEUDON
;       5 place Jules Janssen
;       92190 MEUDON
;       E-mail: nicolas.fuller@obspm.fr
;       copyright (C) 2005 Nicolas Fuller, Observatoire de Paris
;       This program is free software; you can redistribute it and/or modify it under the terms of the
;       GNU General Public License as published by the Free Software Foundation;
;
; PROJECT:
;
;       EGSO (European Grid of Solar Observations)
;       Solar Feature Catalog 
;
; CALLING SEQUENCE:
;         res = egso_sfc_resample_ind(indices,factor)
;
; INPUT
;         indices      subscripts of non-null pixels
;         factor       resampling factor
;
; OUTPUTS
;         Output is the resample subscripts array
;
; HISTORY:
;
;    NF Mar 2005 last rev.
;-

FUNCTION EGSO_SFC_RESAMPLE_IND,indices,factor

  nele       = N_ELEMENTS(indices)
  nb_samples = FIX(nele/factor) > 4
  samples    = LONARR(nb_samples)
  samples[0] = indices[0]
  samples[nb_samples-1] = indices[nele-1]
  dist       = (nele-1)*1./(nb_samples - 1)
  pick       = FIX( (FINDGEN(nb_samples-2)+1. )*dist )
  samples[1:nb_samples-2] = indices[pick]

  RETURN,samples

END
