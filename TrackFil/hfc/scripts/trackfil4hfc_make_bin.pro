;IDL batch file to build an binary runtime file (.sav)
;to run trackfil code using a command line or a script.
;time SSW packages must be loaded before
;running this script.
;X.Bonnin, 21-DEC-2015

sep = path_sep()
trackfil_home_dir=getenv("TRACKFIL_HOME_DIR")
if (trackfil_home_dir eq '') then message, '$TRACKFIL_HOME_DIR must be defined!'
trackfil4hfc_home_dir = trackfil_home_dir + sep + 'hfc'
src_dir = trackfil_home_dir + sep + 'src'
lib_dir = trackfil_home_dir + sep + 'lib' + sep + 'idl'
pathsep = path_sep(/search_path)
!PATH = expand_path('+'+src_dir) + pathsep + !PATH
!PATH = expand_path('+'+lib_dir) + pathsep + !PATH
binfile = trackfil_home_dir + sep + 'bin' + sep +'trackfil4hfc.sav'

@compile_trackfil4hfc

cd,trackfil4hfc_home_dir + sep + 'wrapper',current=cur_dir
proname= ['trackfil4hfc']
;funcname = ['sock_header']
classname=['hfc_filaments_tracking','hfc_frc_info','hfc_fil_hqi','trackfil_config']
resolve_all, /CONTINUE_ON_ERROR, class=classname, resolve_procedure=proname, resolve_function=funcname
save, /ROUTINES, filename=binfile, description='Runtime IDL program to call trackfil4hfc.pro', /VERBOSE, /EMBEDDED
if not file_test(binfile) then message,binfile+ ' has not been saved correctly!' else print,binfile+' saved'
cd,cur_dir