pro counter,iin,iout

if iin ge 9999 then stop

if iin lt 10 then iout= '000'+strtrim(iin,2)
if iin ge 10 and iin le 100 then iout = '00'+strtrim(iin,2)
if iin ge 100 and iin le 1000 then iout = '0'+strtrim(iin,2)
if iin ge 1000 and iin le 10000 then iout = ''+strtrim(iin,2)

end
