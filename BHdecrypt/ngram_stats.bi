dim as string load_status=""
load_status+=str(solver_file_name_ngrams)+lb
load_status+="---------------------------------------------------------"+lb
load_status+="N-gram file format: "+ngram_format+lb
if fileformat > 2 then ' gov
	load_status+="N-gram system: GOV"+lb
else
	select case ngram_size
		case 2 to 7:load_status+="N-gram system: default"+lb
		case 8:load_status+="N-gram system: beijinghouse"+lb
	end select
end if

load_status+="N-gram size: "+str(ngram_size)+lb
load_status+="N-gram entropy weight: "+str(solvesub_entweight)+lb
load_status+="N-gram factor: "+format(solvesub_ngramfactor,"0.00000")+lb
load_status+="N-gram temperature: "+str(solvesub_temperature)+lb
load_status+="N-gram alphabet: "
for i=0 to ngram_alphabet_size-1
	load_status+=chr(alphabet(i))
next i
load_status+=" ("+str(ngram_alphabet_size)+" letters)"+lb
load_status+="---------------------------------------------------------"+lb
load_status+="N-gram items: "+str(ngram_count)+" ("+format(ngram_count/(ngram_alphabet_size^ngram_size)*100,"0.00")+"% coverage)"+lb
if ngram_maxtableindex>0 then load_status+="N-gram system table size: "+str(ngram_maxtableindex)+ " ("+format(trimmed_table_ratio*100,"0.00")+"% usage)"+lb
if distinct_values>0 then load_status+="N-gram log value range: "+str(ngram_lowval)+" to "+str(ngram_highval)+" ("+str(distinct_values)+" distinct values)"+lb
if ngram_value_entropy1<>0 then load_status+="N-gram log value entropy: "+format(abs(ngram_value_entropy1),"0.00")+" / "+format(abs(ngram_value_entropy2),"0.00")+lb
load_status+="N-gram high value: "+str(highgram)+lb
load_status+="N-gram memory usage: "+format(ngram_mem/1073741824.0,"0.00")+" GB RAM ("+str(ngram_mem)+" bytes)"+lb
load_status+="Bits per n-gram (mem): "+format(8*ngram_mem/ngram_count,"0.00")+" bits"+lb
load_status+="Bits per n-gram (disk): "+format(8*ngram_file_size/ngram_count,"0.00")+" bits"+lb
if count3 > 0 then load_status+="GOV 3-gram keyguards: "+str(count3)+" ("+format(count3/(ngram_alphabet_size^3)*100,"0.00")+"% coverage)"+lb
if count4 > 0 then load_status+="GOV 4-gram keyguards: "+str(count4)+" ("+format(count4/(ngram_alphabet_size^4)*100,"0.00")+"% coverage)"+lb
if count5 > 0 then load_status+="GOV 5-gram keyguards: "+str(count5)+" ("+format(count5/(ngram_alphabet_size^5)*100,"0.00")+"% coverage)"+lb
if ngram_size=8 andalso solvesub_ngramcaching=1 then
	dim as uinteger cachebh8_nfb=8*(ngram_alphabet_size^3)*ngram_maxtableindex
	load_status+="N-gram caching memory usage: "+format(cachebh8_nfb/1073741824,"0.00")+" GB RAM ("+str(cachebh8_nfb)+" bytes)"+lb
end if

load_status+="N-gram Loading time: "+format(ngram_loading_time,"0.00")+" seconds"
if loadngrams_showmsg=1 then ui_editbox_settext(output_text,load_status)