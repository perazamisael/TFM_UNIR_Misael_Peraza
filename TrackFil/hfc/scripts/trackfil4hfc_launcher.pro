;Run tracking code for HFC

;additional options
NEW_TRACKING=0
VERBOSE=1
DEBUG = 0

;config file
config_file = 'trackfil_mh_config.txt'

;time range to process
starttime = '2001-05-01T00:00:00'
endtime   = '2001-06-30T23:59:59'

;hfc login
db = 'hfc1test'
host = 'helio-fc1.obspm.fr'

;Paths
output_dir = '../products'
config_dir = '../config'
src_dir = '../src'

pathsep = PATH_SEP(/SEARCH_PATH)
!PATH = expand_path('+'+src_dir) + pathsep + !PATH
!PATH = expand_path('+'+src_dir+'/aux') + pathsep + !PATH
!PATH = expand_path('+'+src_dir+'/classes') + pathsep + !PATH

@compile_trackfil4hfc

trackfil4hfc,config_file, $
	     starttime=starttime, $
	     endtime=endtime, $
	     config_dir=config_dir, $
	     output_dir=output_dir,$
	     database=db,host=host,$
	     NEW_TRACKING=NEW_TRACKING, $
             /WRITE_CSV,/WRITE_PNG,$
	     DEBUG=DEBUG,VERBOSE=VERBOSE
