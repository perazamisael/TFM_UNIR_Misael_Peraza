FUNCTION hfc_stilts_client,table,$
						   starttime=starttime,$
						   endtime=endtime,$
						   maxrec=maxrec,ofmt=ofmt,$
						   free_query=free_query,$
                           database=database, $
						   server=server,hostname=hostname,$
						   port=port,username=username,$
						   password=password,error=error,$
						   header=header,$
						   SHOW_TABLES=SHOW_TABLES,$
						   DESCRIBE_TABLE=DESCRIBE_TABLE,$
						   STRUCTURE=STRUCTURE,VERBOSE=VERBOSE

;+
; NAME:
;		hfc_stilts_client
;
; PURPOSE:
; 		Performs a query on HFC using the stilts server.
;
; CATEGORY:
;		I/O 
;
; GROUP:
;		None.
;
; CALLING SEQUENCE:
;		IDL>Response = hfc_stilts_client(table)
;
; INPUTS:
;		table  	 - Scalar of string type that contains the table of HFC to query.
;				
;	
; OPTIONAL INPUTS:
;		starttime  - Scalar of string type that specifies the start time of the time range to return (iso 8601 format).
;		endtime    - Scalar of string type that specifies the end time of the time range to return (iso 8601 format).
;		maxrec     - Scalar of long type that specifies the maximum number of returned rows allowed. Default is 10000.
;		free_query - Scalar of string type containing the query to send to HFC.
;					 If query is set, table, starttime, endtime, and maxrec inputs will be ignored.
;		server 	   - Scalar of string type that contains the name of the server where Stilts is deployed (without http:// prefixe).
;					 Default is "voparis-helio.obspm.fr".
;		hostname   - Scalar of string type providing the host name of the database.
;                    Default is "voparis-mysql5-paris.obspm.fr".
;       database   - Scalar of string type providing the name of the database.
;                    Default is "hfc1".
;		port       - Scalar of integer type that contains the port number.
;					 Default is "8080".
;		username   - Scalar of string type that contains the name of the user login.
;					 Default is "guest".
;		password   - Scalar of string type that contains the password.
;					 Default is "guest".
;		ofmt       - Scalar of string type that contains the returned format.
;
; KEYWORD PARAMETERS:
;		/SHOW_TABLES    - Display all the tables available in HFC.
;		/DESCRIBE_TABLE - Describe the columns of the table given in input. 
;		/STRUCTURE      - Returns an IDL structure structure instead of a vector of string type.
;		/VERBOSE		- Talkative mode.
;
; OUTPUTS:
;		response - A vector of string type containing the returned votable (or an IDL structure if /STRUCTURE is set).		
;
; OPTIONAL OUTPUTS:
;		header - Returns the response header.
;		error  - Equal to 0 if the request succeeds, 1 else.
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
;		stilts_query
;
; EXAMPLE:
;		None
;
; MODIFICATION HISTORY:
;		Written by:		X.Bonnin, 05-MAR-2011.
;					
;-					   
						   
error = 1	
SHOW_TABLES = keyword_set(SHOW_TABLES)
if (SHOW_TABLES) then table = ' '	
if (keyword_set(free_query)) then begin
	FQUERY = 1 
	table = ' '
endif else FQUERY = 0			   
if (~keyword_set(table)) then begin
	message,/INFO,'Call is:'
	print,'response = hfc_stilts_client(table,$'
	print,'                             starttime=starttime,$'
	print,'                             endtime=endtime,maxrec=maxrec,$'
	print,'                             server=server,hostname=hostname,$'
    print,'                             database=database, $'
	print,'                             port=port,username=username,$'
	print,'                             free_query=free_query,$'
	print,'                             password=password,ofmt=ofmt,$'
	print,'                             header=header,error=error,$'
	print,'                             /SHOW_TABLES,/DESCRIBE_TABLE,$'
	print,'                             /STRUCTURE,/VERBOSE'
	return,''
endif

SHOW_TABLES = keyword_set(SHOW_TABLES)
DESCRIBE_TABLE = keyword_set(DESCRIBE_TABLE)
STRUCTURE = keyword_set(STRUCTURE)
SILENT = 1-keyword_set(VERBOSE)

if (~keyword_set(database)) then database = 'hfc1'
if (~keyword_set(server)) then server = 'voparis-helio.obspm.fr'
if (~keyword_set(hostname)) then hostname = 'voparis-mysql5-paris.obspm.fr'
if (~keyword_set(port)) then port = '8080'
if (~keyword_set(username)) then username = 'guest'
if (~keyword_set(password)) then password = 'guest' 
if (~keyword_set(starttime)) then starttime = '1900-01-01T00:00:00'
if (not keyword_set(maxrec)) then maxrec = 0l
if (~keyword_set(endtime)) then endtime = (strsplit(anytim(!stime, /ccsds),'.',/EXTRACT))[0]
if (STRUCTURE) then ofmt = 'votable'
tbl = strtrim(table[0],2)

jdbc = 'jdbc:mysql://'+strtrim(hostname[0],2)+':3306/'+strtrim(database[0],2)

stime = strjoin(strsplit(starttime[0],'T',/EXTRACT),' ')
etime = strjoin(strsplit(endtime[0],'T',/EXTRACT),' ')

case 1 of
	SHOW_TABLES:cmd = 'SHOW TABLES'
	DESCRIBE_TABLE:cmd = 'DESCRIBE '+tbl
	FQUERY:cmd = strtrim(free_query[0],2) 
	else:begin
		cmd = 'SELECT * FROM '+tbl+' WHERE (DATE_OBS BETWEEN "'+stime+'" AND "'+etime+'")'
	if (maxrec gt 0l) then cmd = cmd +' LIMIT '+strtrim(maxrec[0],2)
	end
endcase

response = stilts_query(cmd,server=server,jdbc=jdbc,port=port,user=username,password=password,ofmt=ofmt,$
						header=header,status=status,STRUCTURE=STRUCTURE,SILENT=SILENT)

error = 1 - status
return,response
END