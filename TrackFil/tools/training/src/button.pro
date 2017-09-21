FUNCTION button,xoffset=xoffset, $
				yoffset=yoffset

COMMON button_status,status

base = WIDGET_BASE(title='Same feature?',/ROW, $
					xoffset=xoffset,yoffset=yoffset, $
					/ALIGN_CENTER)
no_button = WIDGET_BUTTON(base, value='No', uvalue='no', $
								/ALIGN_CENTER)
yes_button = WIDGET_BUTTON(base, value='Yes', uvalue='yes', $
								/ALIGN_CENTER)
skip_button = WIDGET_BUTTON(base, value='Skip', uvalue='skip', $
								/ALIGN_CENTER)
quit_button = WIDGET_BUTTON(base, value='Save and quit', uvalue='quit', $
								/ALIGN_CENTER)
exit_button = WIDGET_BUTTON(base, value='Exit without save', uvalue='exit', $
								/ALIGN_CENTER)

WIDGET_CONTROL, base, /REALIZE
XMANAGER,'wid',base

return,status
END

PRO wid_event,event
	
	COMMON button_status,status

	widget_control,event.id,get_uvalue=uval
	
	case uval of
		'no':status=-1
		'yes':status=1
		'skip':status=2
		'quit':status=3
		'exit':status=4
		else:status=0
	endcase

	widget_control,event.top,/DESTROY
END

