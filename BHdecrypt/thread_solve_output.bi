os+="Score: "+format(thread(t).score,"0.00")+" IOC: "+format(thread(t).ioc,"0.0000")+" Multiplicity: "+format(thread(t).multiplicity,"0.0000")+stt(thread(t).sectime)+lb
if solvesub_advstats=1 then
	os+=str(thread(t).repeats)+lb+"PC-cycles: "+str(thread(t).pccycles) '+lb
	'if ngram_standardalphabet=1 then os+=" Word-flow: "+rdc(thread(t).wordflow,2)
	os+=lb
end if