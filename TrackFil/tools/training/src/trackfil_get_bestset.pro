PRO trackfil_get_bestset,file,false_rate

data = readcsv(file,delimiter=';')
header = data[0,*]
data = data[1:*,*]
a = float(data[*,1])
theta = float(data[*,2])
d = float(data[*,3])
match = fix(data[*,0])

where_match = where(match eq 1,n1,complement=where_not_match,ncomplement=n0)
a1 = a[where_match] & a0 = a[where_not_match]
theta1 = theta[where_match] & theta0 = theta[where_not_match]
d1 = d[where_match] & d0 = d[where_not_match]

;range
arange = minmax([a0,a1])
thrange = minmax([theta0,theta1])
drange = minmax([d0,d1]) 

;Binsize
da = 0.1
dth = 1.0
dd = 0.1

;Number of bins
na = long((arange[1] - arange[0])/da) + 1l
nth = long((thrange[1] - thrange[0])/dth) + 1l
nd = long((drange[1] - drange[0])/dd) + 1l

;histogram x-axis
xa = da*findgen(na) + arange[0]
xth = dth*findgen(nth) + thrange[0]
xd = dd*findgen(nd) + drange[0]

h0 = Bin3D(a0, theta0, d0, $
		   xran=arange, yran=thrange, zran=drange, $
		   nvox=[na,nth,nd])
nh0 = total(h0)
h1 = Bin3D(a1, theta1, d1, $
		   xran=arange, yran=thrange, zran=drange, $
		   nvox=[na,nth,nd])
nh1 = total(h1)

htot0 = 0l & htot1 = 0l
for i=0l,na-1l do begin
	for j=0l,nth-1l do begin
		for k=0l,nd-1l do begin
			if (htot0 gt nh0*false_rate) then goto,break1
			htot0 = htot0 + h0[i,j,k]
			htot1 = htot1 + h1[i,j,k]
			set1 = [i,j,k]
		endfor
	endfor
endfor
break1:
print,'a = ',xa[set1[0]],', theta = ',xth[set1[1]],', d = ',xd[set1[2]]
print,'False rate: ',100.*(htot0/nh0),' - Positive rate: ',100.*(htot1/nh1)

htot0 = 0l & htot1 = 0l
for j=0l,nth-1l do begin
	for i=0l,na-1l do begin
		for k=0l,nd-1l do begin
			if (htot0 gt nh0*false_rate) then goto,break2
			htot0 = htot0 + h0[i,j,k]
			htot1 = htot1 + h1[i,j,k]
			set2 = [i,j,k]
		endfor
	endfor
endfor
break2:
print,'a = ',xa[set2[0]],', theta = ',xth[set2[1]],', d = ',xd[set2[2]]
print,'False rate: ',100.*(htot0/nh0),' - Positive rate: ',100.*(htot1/nh1)

htot0 = 0l & htot1 = 0l
for k=0l,nd-1l do begin
	for i=0l,na-1l do begin
		for j=0l,nth-1l do begin
			if (htot0 gt nh0*false_rate) then goto,break3
			htot0 = htot0 + h0[i,j,k]
			htot1 = htot1 + h1[i,j,k]
			set3 = [i,j,k]
		endfor
	endfor
endfor
break3:
print,'a = ',xa[set3[0]],', theta = ',xth[set3[1]],', d = ',xd[set3[2]]
print,'False rate: ',100.*(htot0/nh0),' - Positive rate: ',100.*(htot1/nh1)

htot0 = 0l & htot1 = 0l
for k=0l,nd-1l do begin
	for j=0l,nth-1l do begin
		for i=0l,na-1l do begin
			if (htot0 gt nh0*false_rate) then goto,break4
			htot0 = htot0 + h0[i,j,k]
			htot1 = htot1 + h1[i,j,k]
			set4 = [i,j,k]
		endfor
	endfor
endfor
break4:
print,'a = ',xa[set4[0]],', theta = ',xth[set4[1]],', d = ',xd[set4[2]]
print,'False rate: ',100.*(htot0/nh0),' - Positive rate: ',100.*(htot1/nh1)

stop
END