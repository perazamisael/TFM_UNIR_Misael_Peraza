;+
; NAME:
;		stilts_query
;
; PURPOSE:
; 		Performs a query on a database using a stilts server.
;
; CATEGORY:
;		I/O 
;
; GROUP:
;		None.
;
; CALLING SEQUENCE:
;		IDL>Results = stilts_query(query,server=server,port=port,jdbc=jdbc,user=user,password=password,ofmt=ofmt)
;
; INPUTS:
;		query  	 - Scalar of string type that contains the SQL query.
;		server 	 - Scalar of string type that contains the name of the server where Stilts is deployed (without http:// prefixe).
;		jdbc   	 - Scalar of string type that contains the url of the jdbc.
;		user   	 - Scalar of string type that contains the name of the user login.
;		password - Scalar of string type that contains the password.		
;	
; OPTIONAL INPUTS:
;		port - Scalar of integer type that contains the port number. (80 by default.)
;		ofmt - Scalar of string type that contains the output format option (see Stilts documentation for available formats).
;
; KEYWORD PARAMETERS:
;		/STRUCTURE  - Returns an IDL structure structure instead of a vector of string type (works only if ofmt="votable").
;		/SILENT		- Quiet mode.
;
; OUTPUTS:
;		results - A vector of string type containing the response of the query (or an IDL structure of /STRUCTURE is set).		
;
; OPTIONAL OUTPUTS:
;		header - Returns the response header.
;		status - Equal to 1 if the request succeeds, 0 else.
;
; COMMON BLOCKS:
;		None.		
;
; SIDE EFFECTS:
;		None.
;
; RESTRICTIONS:
;		None.		
;
; CALL:
;		webget
;		decode_votable
;
; EXAMPLE:
;		;Get the Carrington coordinates (in degrees) of the center of gravity of the filaments   
;       ;observed between April 12, 2002 at 09:00:00, and April 14, 2002 at 22:30:00; and that are stored in the
;	    ;Heliophysics Feature Catalogue (HFC):
;       Results = stilts_query('SELECT SC_CAR_LON,SC_CAR_LAT FROM VIEW_FILAMENTS_FULL WHERE'+$
;							   '(DATE_OBS>="2002-04-12 09:00:00") AND (DATE_OBS<="2002-04-14 22:30:00")',$
;                              server='voparis-helio.obspm.fr',port=8080,$
;							   jdbc='jdbc:mysql://voparis-mysql5-paris.obspm.fr:3306/hfc1',$
;							   user='guest',password='guest')
;		Help,Results
;
;
; MODIFICATION HISTORY:
;		Written by:		X.Bonnin, 05-MAR-2011.
;					
;-

FUNCTION stilts_query,query,$
					  server=server,port=port,$
					  jdbc=jdbc,$
					  user=user,password=password,$
					  ofmt=ofmt,$
					  header=header,status=status,$
					  STRUCTURE=STRUCTURE,SILENT=SILENT


status = 0
SILENT = keyword_set(SILENT)
STRUCTURE = keyword_set(STRUCTURE)

if (n_params() lt 1) then begin
	message,/INFO,'Call is:'
	print,'Results = stilts_query(query,server=server,port=port,$'
	print,'                       jdbc=jdbc,user=user,password=password,$'
	print,'                       ofmt=ofmt,header=header,status=status,$'
	print,'                       /STRUCTURE,/SILENT)'
	return,''
endif

if not (keyword_set(port)) then port = '8080'
if (STRUCTURE) then ofmt = 'votable' 

qry = strtrim(query[0],2)
qry = strjoin(strsplit(qry,/EXTRACT),'%20')

;Build url of the query
url = 'http://' + strtrim(server[0],2) + ':' + strtrim(port[0],2) + '/stilts/task/sqlclient'
url = url + '?db='
url = url + strtrim(jdbc[0],2) + '&user=' + strtrim(user[0],2) + '&password=' + strtrim(password[0],2) 
url = url + '&sql=' + qry 
if keyword_set(ofmt) then url = url + '&ofmt=' + strtrim(ofmt[0],2) else ofmt = ''


if (~SILENT) then print,'Query --> '+url
response = webget(url,/SILENT)

header =response.header
results = response.text

if (~SILENT) then print,strtrim(n_elements(results),2) + ' rows returned.'
if (strupcase(strmid(header[0],13,2)) ne 'OK') then return,results 

if (STRUCTURE) then begin
	results = strjoin(results[2:n_elements(results)-3])
	results = decode_votable(results,/QUIET)
endif

status = 1
return,results
END