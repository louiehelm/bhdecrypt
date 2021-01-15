if timer-loadngramtimer>1 then
	loadngramtimer=timer
	ot=str(solver_file_name_ngrams)+lb
	ot+="--------------------------------------------------------"+lb
	ot+="Loading progress: "+format((curr_items/total_items)*100,"0.00")+"%"
	if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)
end if