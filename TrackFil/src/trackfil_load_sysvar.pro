PRO trackfil_load_sysvar

;Define some Local system variables
;==================================

;Temporary NaN value
defsysv,'!NAN_VALUE', !values.f_nan

; FILLVAL value
defsysv,'!TRACKFIL_FILLVAL', 1.e-30


;speed of light
defsysv,'!CARR_PERIOD', 27.275300d ;days


END