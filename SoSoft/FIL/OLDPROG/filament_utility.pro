@/home/fuller/IDL/EllipseFit_Cleaning.pro
@/home/fuller/IDL/CleaningStart.pro
;@/home/fuller/IDL/FitEllipse.pro

PRO filament_utility_event, event

COMMON INDEX,numb,curr,tab
COMMON TEXT,wtext,infotab
COMMON BUTTON,file_menu,file_bttn1,file_bttn2,file_bttn3,file_bttn4,file_bttn5, $
              proc_menu,proc_bttn1,proc_bttn2,proc_bttn3,tool_menu,tool_bttn1, $
              tool_bttn2,window_menu,window_bttn1,window_bttn2

; This is the event handler for a basic menu bar.

; Use WIDGET_CONTROL to get the user value of any widget touched and put
; that value into 'eventval':

WIDGET_CONTROL, event.id, GET_UVALUE = eventval


; The selection of a menu item is easily handled with a CASE statement.
; When a menu item is selected, the value of 'eventval' is the user value
; of the selected menu item.

CASE eventval OF
        'QUIT':BEGIN
                ; Quit the menu bar example.
                WIDGET_CONTROL, event.top, /Destroy
                END
        'SAV':BEGIN
                ;open a .sav file
                file = dialog_pickfile(PATH='/home/fuller/poub/SAV', $
                       FILTER='*_p.sav',/MUST_EXIST)
                IF file NE '' THEN BEGIN
                  WIDGET_CONTROL,file_bttn1,SENSITIVE=0
                  WIDGET_CONTROL,file_bttn2,SENSITIVE=0
                  WIDGET_CONTROL,file_bttn3,SENSITIVE=1
                  WIDGET_CONTROL,file_bttn5,SENSITIVE=1
                  WIDGET_CONTROL,proc_bttn1,SENSITIVE=1
                  WIDGET_CONTROL,proc_bttn2,SENSITIVE=1
                  WIDGET_CONTROL,proc_bttn3,SENSITIVE=1
                  RESTORE,file
                  tab=INTARR(1024,1024,6)
                  infotab=STRARR(6)
                  tab(*,*,0)=arr
                  TVSCL,tab(*,*,0)
                  infotab(0)=file
                  WIDGET_CONTROL,wtext,SET_VALUE=infotab(0)
                  numb=numb+1
                  curr=numb-1
                ENDIF
                END
        'FITS':BEGIN
                ;open a fits file
                file=dialog_pickfile(PATH='/home/fuller/poub/FITS', $
                     FILTER='*.fits',/MUST_EXIST)
                IF file NE '' THEN BEGIN
                  WIDGET_CONTROL,file_bttn1,SENSITIVE=0
                  WIDGET_CONTROL,file_bttn2,SENSITIVE=0
                  WIDGET_CONTROL,file_bttn3,SENSITIVE=1
                  WIDGET_CONTROL,file_bttn5,SENSITIVE=1
                  WIDGET_CONTROL,proc_bttn1,SENSITIVE=1
                  WIDGET_CONTROL,proc_bttn2,SENSITIVE=1
                  WIDGET_CONTROL,proc_bttn3,SENSITIVE=1
                  arr=MRDFITS(file,0,h)
                  ;new meudon images :
                  IF (SIZE(arr))(0) NE 2 THEN arr=arr(*,*,2)
                  tab=INTARR(1024,1024,6)
                  infotab=STRARR(6)
                  IF (SIZE(arr))(1) NE 1024 OR (SIZE(arr))(2) NE 1024 THEN $
                  arr = CONGRID(arr,1024,1024)
                  tab(*,*,0)=arr
                  TVSCL,tab(*,*,0)
                  infotab(0)=file
                  WIDGET_CONTROL,wtext,SET_VALUE=infotab(0)
                  numb=numb+1
                  curr=numb       
                ENDIF
                END
        'BACK':BEGIN
                curr=curr-1
                IF curr LT 0 THEN curr=0
                TVSCL,tab(*,*,curr)
                WIDGET_CONTROL,wtext,SET_VALUE=infotab(curr)
                END
        'NEXT':BEGIN
                curr=curr+1
                IF curr GE numb THEN curr=curr-1
                TVSCL,tab(*,*,curr)
                WIDGET_CONTROL,wtext,SET_VALUE=infotab(curr)
                END
        'SAVE':BEGIN
                filename=DIALOG_PICKFILE(PATH='/home/fuller/poub/SAV')
                arr = tab(*,*,curr)
                SAVE,arr,FILENAME=filename
                END
        'CLOSE':BEGIN
                ;close a file
                WIDGET_CONTROL,file_bttn1,SENSITIVE=1
                WIDGET_CONTROL,file_bttn2,SENSITIVE=1
                WIDGET_CONTROL,file_bttn3,SENSITIVE=0
                WIDGET_CONTROL,file_bttn5,SENSITIVE=0
                WIDGET_CONTROL,proc_bttn1,SENSITIVE=0
                WIDGET_CONTROL,proc_bttn2,SENSITIVE=0
                WIDGET_CONTROL,proc_bttn3,SENSITIVE=0
                tab=tab*0
                infotab(*)=''
                TVSCL,tab(*,*,0)
                WIDGET_CONTROL,wtext,SET_VALUE=infotab(0)
                curr=0
                numb=0
                 END
        'REGION':BEGIN
                 WIDGET_CONTROL,/HOURGLASS
                 numb=numb+1
                 curr=numb-1
                 tab(*,*,curr)=FILAMENT(INPUT=tab(*,*,curr-1))
                 TVSCL,tab(*,*,curr)
                 infotab(curr)=infotab(curr-1)+' + region growing'
                 WIDGET_CONTROL,wtext,SET_VALUE=infotab(curr)
                 END
        'PREPROCESS':BEGIN
                 WIDGET_CONTROL,/HOURGLASS    
                 numb=numb+1
                 curr=numb-1
                 tab(*,*,curr)=CLEANINGSTART(tab(*,*,curr-1))  
                 TVSCL,tab(*,*,curr)
                 infotab(curr)=infotab(curr-1)+' + preprocessed'
                 WIDGET_CONTROL,wtext,SET_VALUE=infotab(curr)           
                 END
        'SUNSPOT':BEGIN
                 WIDGET_CONTROL,/HOURGLASS    
                 numb=numb+1
                 curr=numb-1
                 tab(*,*,curr)=SUNSPOT(tab(*,*,curr-1))  
                 TVSCL,tab(*,*,curr)
                 infotab(curr)=infotab(curr-1)+' + sunspot suppr'
                 WIDGET_CONTROL,wtext,SET_VALUE=infotab(curr)                     
                 END
        'HIST':BEGIN
                 im_hist=HIST_EQUAL(tab(*,*,curr),percent=1)  
                 TVSCL,im_hist
                 END
        'COLOR':BEGIN
                 XLOADCT
                 END
        ELSE:BEGIN
                ; Print the button's user value to the IDL window:
                PRINT, 'Widget User Value = ' + eventval
                END
ENDCASE
END



PRO filament_utility, GROUP = GROUP

COMMON INDEX,numb,curr,tab
COMMON TEXT,wtext,infotab
COMMON BUTTON,file_menu,file_bttn1,file_bttn2,file_bttn3,file_bttn4,file_bttn5, $
              proc_menu,proc_bttn1,proc_bttn2,proc_bttn3,tool_menu,tool_bttn1, $
              tool_bttn2,window_menu,window_bttn1,window_bttn2
numb=0
curr=0

IF STRPOS(!path,'/home/fuller/IDL:') LT 0 THEN $
   !path = EXPAND_PATH('/home/fuller/IDL')+':'+!path

;dll='/home/fuller/C_LIB/gauss.so'
;dll2='/home/fuller/C_LIB/canny.so'
;linkimage, 'gauss_smoothing', dll, 1, 'GaussSmoothing'
;linkimage, 'canny', dll2, 1, 'CannyHT'

; This is the procedure that creates an example menu bar.

; A top-level base widget with the title "Menu Bar Example" will
; hold the menu bar.   Since this is a top-level base (no parent),
; the "MBAR" keyword may be used.

base = WIDGET_BASE(TITLE = 'Filament utility', $
;                   XSIZE = 1024, $
;                   YSIZE = 1024, $
;                   /SCROLL,      $
;                   X_SCROLL_SIZE = 900, $
;                   Y_SCROLL_SIZE = 800, $
                   MBAR=bar_base)

; The "bar_base" variable now contains the widget base id for the menu bar.

; The menus may now be constructed by creating button widgets with the
; "MENU" keyword.

file_menu = WIDGET_BUTTON(bar_base, Value='File', /Menu)
file_bttn1 = WIDGET_BUTTON(file_menu, Value='Open Fits', Uvalue='FITS')
file_bttn2 = WIDGET_BUTTON(file_menu, Value='Open sav file', Uvalue='SAV')
file_bttn5 = WIDGET_BUTTON(file_menu, Value='Save', Uvalue='SAVE',SENSITIVE=0)
file_bttn3 = WIDGET_BUTTON(file_menu, Value='Close', Uvalue='CLOSE',SENSITIVE=0)
file_bttn4 = WIDGET_BUTTON(file_menu, Value='Quit', Uvalue='QUIT')

proc_menu = WIDGET_BUTTON(bar_base, Value='Operations', /Menu)
proc_bttn1 = WIDGET_BUTTON(proc_menu, Value='Preprocess',Uvalue='PREPROCESS',SENSITIVE=0)
proc_bttn2 = WIDGET_BUTTON(proc_menu, Value='Region Growing',Uvalue='REGION',SENSITIVE=0)
proc_bttn3 = WIDGET_BUTTON(proc_menu, Value='Suppr. Sunspot', Uvalue='SUNSPOT',SENSITIVE=0)
;ope_pr = WIDGET_BUTTON(ope_menu, Value='Options Pull-Right', /Menu)
;   pr_bttn1 = WIDGET_BUTTON(ope_pr, Value='Pull-Right Item 1', Uvalue='PR 1')
;   pr_bttn2 = WIDGET_BUTTON(ope_pr, Value='Pull-Right Item 2', Uvalue='PR 2')
;ope_bttn3 = WIDGET_BUTTON(ope_menu, Value='Options Item 5', Uvalue='FILE 5')

tool_menu = WIDGET_BUTTON(bar_base, Value='Tools', /Menu)
tool_bttn1 = WIDGET_BUTTON(tool_menu, Value='Hist_equal', Uvalue='HIST')
tool_bttn2 = WIDGET_BUTTON(tool_menu, Value='Color', Uvalue='COLOR')

window_menu = WIDGET_BUTTON(bar_base, Value='Window', /Menu)
window_bttn1 = WIDGET_BUTTON(window_menu, Value='Back', Uvalue='BACK')
window_bttn2 = WIDGET_BUTTON(window_menu, Value='Next', Uvalue='NEXT')

;drawing window
wtext = WIDGET_TEXT(base,/NO_NEWLINE,SCR_XSIZE=920,XOFFSET=2,YOFFSET=2)
wdraw = WIDGET_DRAW(base,XSIZE=1024,$
                    YSIZE=1024,/SCROLL,$
                    X_SCROLL_SIZE=900,$
                    Y_SCROLL_SIZE=770,YOFFSET=30)

; Realize the widgets:
WIDGET_CONTROL, base, /REALIZE

; Hand off control of the widget to the XMANAGER:
XMANAGER, "filament_utility", base, GROUP_LEADER = GROUP, /NO_BLOCK

END

