;+
; NAME:
;	decode_VOTable
; PURPOSE:
;	Converts VOTable string into an IDL structure
; CALLING SEQUENCE:
;	struct = decode_VOTable(votable_string)
;
; INPUTS:
;	votable_string	string read from file or received through SOAP
; KEYWORDS
;	quiet	removes some debug messages
; HISTORY:
;       2 Dec 2003  RDB  First version
;      20 Feb 2004  RDB  Added short, redefined INT, LONG - fudge...
;       9 Aug 2004  RDB  changed arr2str to strjoin; added comvert time
;      23 Aug 2004  RDB  optimized code, removed most SSW calls to increase
;			 speed; incl. changes suggested by S.Zharkov
;      22 Nov 2004  RDB  Made more forgiving about absence of <?xml...> string and
;                        whether FIELD variable type defined...
;       2 Jul 1010  RDB  Fixed but related to </TD> - code needed generalising
;      22 Oct 2010  RDB  Fixed case where value string is of zero length
;
;-

function vot_strsegs,tabledata,expr,count=count

;  routine similar to strsplit but low level and only returns pointers
;  this makes it much faster for very long strings... 

npts = strlen(tabledata)/strlen(expr)
pntrs = lonarr(3,npts)

pos=-1
for kpnt=0L,npts-1L do begin
  pos = strpos(tabledata,expr,pos+1)
  if pos lt 0 then goto, done
  pntrs(0,kpnt) = pos
endfor

done:
pntrs(0,kpnt) = strlen(tabledata)
pntrs(1,0:kpnt-1) = pntrs(0,1:kpnt)-1	;pntrs(0,0:kpnt-1)
pntrs = pntrs(*,0:kpnt-1)
pntrs(2,*) = pntrs(1,*)-pntrs(0,*)

count = kpnt

return, pntrs
end


function  decode_votable, input_string, quiet=quiet, ntab=ntab, seltab=seltab, info=hdx

;  This is a cludge until someone writes the proper routine
;  However, it has the advantage that it runs under any version of IDL...
  
ccstart=systime(/sec)

ans=''
qq = input_string

;  use strjoin rather than arr2str which is very slow...
if n_elements(qq) gt 1 then qq=strjoin(qq,/single)

;  Check there is VOTable header
p1 = strpos(strupcase(qq),'<VOTABLE VERSION')
if p1(0) ge 0 then begin              ; was gt!
  p2 = strpos(strmid(qq,p1,100),'>')
  if not keyword_set(quiet) then print,strmid(qq,0,p1+p2+1)
endif else begin
  message,'NOT a VOTable file',/info
  return,-1
  endelse

;  Get VOTABLE part of file
VOTABLE=strmid(qq,strpos(qq,'<VOTABLE>')+9,strpos(qq,'</VOTABLE>')-strpos(qq,'<VOTABLE>')-9)

;  could be other stuff in here related to definitions...
;
;  The supplied VOTable could contain several tables each within a RESOURCE section

ntab = 1
pres = vot_strsegs(VOTABLE,'<RESOURCE', count=npres)
if npres gt 1 then ntab = npres

if not keyword_set(quiet) then print,pres

qtab = 0
if keyword_set(seltab) then qtab=seltab

if ntab gt 1 then begin
  message,'>>>> More than one RESOURCE sections present',/info
  message,'>>>> Reading RESOURCE section '+string(qtab,format='(i2)'),/info
endif

votable=strmid(votable,pres(0,qtab),pres(2,qtab)+1)

;    lets look for any header information

pnt_tab = strpos(VOTABLE,'<TABLE ')
if pnt_tab gt 0 then begin
  HEADER = strmid(VOTABLE,strpos(votable,'<RESOURCE>')+10,strpos(votable,'<TABLE ')-strpos(votable,'<RESOURCE>')-10)
endif else begin
  HEADER = strmid(VOTABLE,strpos(votable,'<RESOURCE>')+10,strpos(votable,'</RESOURCE>'))
endelse
if not keyword_set(quiet) then print,HEADER

;    extract the DESCRIPTION and INFO records
;    what about VOTables without header records??

hdescr = strmid(HEADER,strpos(header,'<DESCRIPTION>'),strpos(header,'</DESCRIPTION>')-strpos(header,'<DESCRIPTION>')+14)
if not keyword_set(quiet) then print,hdescr
;;print,gt_brstr(hdescr,'N>','</')

pnt_lab = strpos(HEADER,'<INFO ')
if pnt_Lab gt 0 then begin

  xxyy = vot_strsegs(HEADER,'<INFO', count=ninfo)
  hdx = {description:'', info:replicate({name:'', value:""},ninfo)}
  if not keyword_set(quiet) then help,/st,hdx
  hdx.description = gt_brstr(hdescr,'N>','</')

  bhead = byte(HEADER)
;;help,xxyy,ninfo
  for jinfo=0,ninfo-1 do begin
    brow = bhead(xxyy(0,jinfo):xxyy(1,jinfo))
    sbrow = string(brow)
;;    if not keyword_set(quiet) then $
      if not keyword_set(quiet) then print,sbrow
    name = gt_brstr(sbrow,'name="','"')
    hdx.info(jinfo).name = name
    if strpos(sbrow,'</I') gt 0 then begin		;case??
      value = gt_brstr(sbrow,'>','</I')
    endif else value = gt_brstr(sbrow,'value="','"')
;;;;  this is a cludge!!!!! should check for <info>...</info>
;;    if name eq 'QUERY_URL' then value = gt_brstr(sbrow,' ><','></I') $
;;      else value = gt_brstr(sbrow,'value="','"')
    hdx.info(jinfo).value = value
  endfor
  if not keyword_set(quiet) then $
    print,hdx

endif else begin

  if not keyword_set(quiet) then message,'**** NO INFO records ****',/info
  hdx=''

endelse

;xx  Currently, only looks for TABLEDATA and makes structure by what is
;xx  defined in the TABLE definition...

;  Get TABLE part of VOTABLE
TABLE=strmid(VOTABLE,strpos(votable,'<TABLE')+7,strpos(votable,'</TABLE>')-strpos(votable,'<TABLE')-7)

; this may be wrong if there is a <DESCRIPTION> field!!
table_head=STRMID(TABLE,strpos(TABLE,'<FIELD'),strpos(TABLE,'<DATA')-strpos(TABLE,'<FIELD'))
fields = strsplit(table_head,'<FIELD',/extr,/regex)

;fields = fields(1:*)
if not keyword_set(quiet) then begin
  help,fields
  print,fields,format='(x,a)'
  endif
ncol = (n_elements(fields))[0]    ;No. of columns in TABLEDATA

;  Get DATA part within TABLE
;;?DATA=strmid(table,strpos(table,'<DATA>')+6,strpos(table,'</DATA>')-strpos(table,'<DATA>')-6)

;  Get TABLEDATA part  -  skip extraction layer for DATA...
TABLEDATA=strmid(table,strpos(table,'<TABLEDATA>')+11,strpos(table,'</TABLEDATA>')-strpos(table,'<TABLEDATA>')-11)

;  row by row, pick out value in each table elements and put into output array
if strlen(tabledata) eq 0 then begin
  message,'VOTable contains nothing in TABLEDATA area',/cont
  return,''
endif 

pntrs = vot_strsegs(tabledata,'<TR>', count=nrow)
table_array = strarr(ncol,nrow)

btab = byte(tabledata)
for jrow=0L,nrow-1L do begin
  brow = btab(pntrs(0,jrow)+4:pntrs(1,jrow)-5)		;eliminate <TR> & </TR>
  pp = vot_strsegs(string(brow),'<TD>')
  pp2 = vot_strsegs(string(brow),'</TD>')
;print,pp,pp2
;read,'Pause: ',ans

  for jcol=0L,ncol-1L do $
;;    if pp(2,jcol)-9 ge 0 then $                         ;support case of zero length string
;    table_array(jcol,jrow) = string(brow(pp(0,jcol)+4:pp(1,jcol)-5))	; eliminate <TD> & </TD>
    if (pp(0,jcol)+4)-(pp2(0,jcol)-1) le 0 then $                         ;support case of zero length string
    table_array(jcol,jrow) = string(brow(pp(0,jcol)+4:pp2(0,jcol)-1))	; eliminate <TD> & </TD>
endfor
  
; form the structure by building a string as work through the FIELDs
ss = '{'
for jcol=0L,ncol-1L do begin
  if jcol gt 0 then ss=ss+', '            ;variable seperator

;  separate different parts of FIELD definition
;  CLUDGE - assume only spaces between definitions, none elsewhere
  zz=strsplit(strtrim(FIELDS(jcol),2),' ',/extr)

;  extract name and data type
  p0 = strpos(fields(jcol),"name=")
  p1 = strpos(fields(jcol),">")
  bfield = byte(fields(jcol)) 
  pln = where(bfield(p0:p1) eq 34b)
  name = string((bfield(p0:p1))[pln(0)+1:pln(1)-1])  
  name = str_replace(name,' ','_')  
  name = str_replace(name,'-','$')  

;  pname = strmid(fields(jcol),p0,p1-p0) 
;  print,(strsplit(pname,'"',/extr))[1]      
goto, xxxx
  pname = (where(strpos(zz,"name") ge 0))[0]
  name = (strsplit(zz(pname),'"',/extr))[1]      ;variable name
  bname = byte(name)
  ; first char of '_' not allowed!
  wmin = where(bname eq 45b)    ;'-' char not allowed! => '$'
  if wmin(0) ge 1 then begin
      bname(wmin) = 36b
  endif
  wsp = where(bname eq 32b, nsp)
help,nsp
  if nsp eq 1 then bname(wsp) = 95b
  name = string(bname)
xxxx:

  type = 'char'   ; default to 'char' in case not defined
  pdtype = (where(strpos(zz,"datatype") ge 0))[0]
  if pdtype gt -1 then type = strlowcase((strsplit(zz(pdtype),'"',/extr))[1])     ;variable type

;  Note that VOTable uses slightly different meanings of variable
;  types to IDL and we need to fudge it a little
  ss = ss+name+':'           ; add to string that will define struct
  if type eq 'char'           then ss=ss+'" "'
  if type eq 'unsignedbyte'   then ss=ss+'0B'
  if type eq 'float'          then ss=ss+'0.0'
  if type eq 'double'         then ss=ss+'0.D0'
  if type eq 'short'          then ss=ss+'0'        ; VOTable I*2
  if type eq 'int'            then ss=ss+'0L'       ; VOTable I*4
  if type eq 'long'           then ss=ss+'0LL'      ; VOTable I*8
  if type eq 'boolean'        then ss=ss+'" "'      ;?? not sure how to deal
endfor
ss=ss+'}

qflag = execute('pp='+ss)
if qflag eq 0 then message,'HELP - problem with structure'
struct = replicate(pp,nrow)

;  load the structure
;  CLUDGE - should really pass names from above - is this necessary...
tags = tag_names(struct)
for jcol = 0L,ncol-1L do begin
  jflag = execute('struct.'+tags(jcol)+'=reform(table_array(jcol,*))')
  endfor

time_took = systime(/sec)-ccstart

if not keyword_set(quiet) then begin
  help,struct
  help,/st,struct
  

	print,'VOTable conversion took:',time_took,' secs', format='(a,f9.3,a)'
	print,'TABLEDATA was',fix(ncol),' cols x',fix(nrow),' rows'
endif

;read,'Pause: ',ans

return,struct
end
