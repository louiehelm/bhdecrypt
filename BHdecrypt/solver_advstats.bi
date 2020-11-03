if local_advstats=1 then
	'thread(tn).repeats=m_repeats(thread(tn).sol(),l,thread(tn).num)
	thread(tn).repeats=m_repeats(thread(tn).sol(),l,0)
	if local_pcmode=0 then
		thread(tn).pccycles=m_pccycles_longshort(thread(tn).sol(),nba(),l,s)
	else 'use untransposed texts
		dim as short utp_nba(l),utp_sol(l)
		for i=1 to l
			utp_nba(thread(tn).key(i))=nba(i)
			utp_sol(thread(tn).key(i))=thread(tn).sol(i)
		next i
		thread(tn).pccycles=m_pccycles_shortshort(utp_sol(),utp_nba(),l,s)
	end if
end if