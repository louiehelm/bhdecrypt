os+="Score: "+rdc(thread(t).score,2)+" IOC: "+rdc(thread(t).ioc,4)+" Multiplicity: "+rdc(thread(t).multiplicity,4)+stt(thread(t).sectime)+lb
if solvesub_advstats=1 then
	os+=str(thread(t).repeats)+lb+"PT-to-CT cycles: "+str(thread(t).pccycles)+lb
end if