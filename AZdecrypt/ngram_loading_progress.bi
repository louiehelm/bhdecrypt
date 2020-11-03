if timer-loadngramtimer>1 then
	loadngramtimer=timer
	ot=str(solver_file_name_ngrams)+chr(13)+chr(10)
	ot+="--------------------------------------------------------"+chr(13)+chr(10)
	ot+="Loading progress: "+rdc((acu_len_s/filelen1)*100,2)+"%"
	if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)
end if