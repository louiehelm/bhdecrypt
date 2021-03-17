if solvesub_wordgrams_enabled=1 then
	'word gram score addon:
	'----------------------
	'idea: somehow allow a exception for words that end with ING,LY,etc
	'issue: long strings of characters are moved into long unrecognizable words to minizize impact on word n-gram score
	'beijinghouse: add stemed 6-gram version to do jarls idea above
	'beijinghouse: then can tighten up the () section of the differentiation code
	words=0
	nwlen=0
	wordlen=0
	for i=1 to l-(wngs-1) 'copy known values 
		wordgrams2(i)=wordgrams(i)
	next i
	for h=1 to wordmap2(curr_symbol,0)  ' beijinghouse: added caching so only have to check changes
			i=wordmap2(curr_symbol,h)
			if solvesub_7gwordgrams=0 then
				wordgrams2(i)=g6w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
			else
				wordgrams2(i)=g7w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
			end if
	next h
	for i=1 to l 'convert sol array to words
		j = wordgrams2(i)
		if j>0 then
			select case nwlen
				case 1:k=g6w2(nwor(1),26,26,26,26,26):if k>0 then words+=1:wor(words)=k:nwlen=0
				case 2:k=g6w2(nwor(1),nwor(2),26,26,26,26):if k>0 then words+=1:wor(words)=k:nwlen=0
				case 3:k=g6w2(nwor(1),nwor(2),nwor(3),26,26,26):if k>0 then words+=1:wor(words)=k:nwlen=0
				case 4:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),26,26):if k>0 then words+=1:wor(words)=k:nwlen=0
				case 5:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),nwor(5),26):if k>0 then words+=1:wor(words)=k:nwlen=0
				case 6:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),nwor(5),nwor(6)):if k>0 then words+=1:wor(words)=k:nwlen=0
			end select
			if nwlen>0 then
				words+=1
				wor(words)=0
				wordlen+=nwlen
				nwlen=0
			end if
			if wl(j,0)>wngs then 'if word length >7 then check word vs wordlist entry
				e=1
				if wl(j,0)>l-(i-1) then
				   e=0
				else
					for k=wngs+1 to wl(j,0) 'k=0 to wl(j,0)-1
						if wl(j,k)<>sol(i+k-1)+65 then
							e=0
							exit for
						end if
					next k
				end if
				if e=1 then ' main word works, check for longer ones first though
				   j2=j ' best found so far
				else
					j2=0
				end if
			   new_j=j ' next item being tested
			   ez=1 ' base string still matches
			   while (ez<>0)
			    new_j+=1
				 if wl(new_j,0) > wl(j2,0) then
               ez=1
					for tz=1 to wngs 'wl(j,0)
						if wl(new_j,tz) <> wl(j,tz) then
						   ez=0
						   exit for
						end if
					next tz
					ez2=1
					if ez=1 then ' if lower string matches
						if wl(new_j,0)>l-(i-1) then ' and checks out that cipher long enough for bigger string to fit
						   ez2=0
						else
							for tz=wngs+1 to wl(new_j,0) 'wl(j,0) to wl(new_j,0)-1 ' check if new part matches
								if wl(new_j,tz) <> sol(i+tz-1)+65 then
								   ez2=0
								   exit for
								end if
							next tz
						end if
						if ez2=1 then
							if not ( wordgrams2(i+wl(j2,0))>0 and wordgrams2(i+wl(new_j,0))=0 ) then ' don't over-extend words by stealing front of next word
								j2=new_j
							end if
						end if
					end if
				 else
				   ez=0
				 end if
				wend
				
				if j2 > 0 then
					words+=1
					wor(words)=j2
					'wordlen+=wl(j,0)
					i+=wl(j2,0)-1 'skip other letters of the found word
				else
					nwlen+=1
					nwor(nwlen)=sol(i)
				end if
			else
				if wl(j,0)>l-(i-1) then
					nwlen+=1
					nwor(nwlen)=sol(i)
				else
					
					
				   ' main word works, check for longer ones first though
				   j2=j ' best found so far
				   if wl(j2,0) > 3 then
					   new_j=j ' next item being tested
					   ez=1 ' base string still matches
					   while (ez<>0)
					    new_j+=1
						 if wl(new_j,0) > wl(j2,0) then
	                  ez=1
							for tz=1 to wl(j,0)
								if wl(new_j,tz) <> wl(j,tz) then
								   ez=0
								   exit for
								end if
							next tz
							ez2=1
							if ez=1 then ' if lower string matches
								if wl(new_j,0)>l-(i-1) then ' and checks out that cipher long enough for bigger string to fit
								   ez2=0
								else
									for tz=wl(j,0)+1 to wl(new_j,0) 'wl(j,0) to wl(new_j,0)-1 ' check if new part matches
										if wl(new_j,tz) <> sol(i+tz-1)+65 then
										   ez2=0
										   exit for
										end if
									next tz
								end if
								if ez2=1 then
									if not ( wordgrams2(i+wl(j2,0))>0 and wordgrams2(i+wl(new_j,0))=0 ) then ' don't over-extend words by stealing front of next word
										j2=new_j
									end if
								end if
							end if
						 else
						   ez=0
						 end if
						wend
				   end if
					
					words+=1				
					wor(words)=j2
					i+=wl(j2,0)-1 'skip other letters of the found word
				end if
			end if
		else
			nwlen+=1
			nwor(nwlen)=sol(i)
		end if
	next i
	select case nwlen
		case 1:k=g6w2(nwor(1),26,26,26,26,26):if k>0 then words+=1:wor(words)=k:nwlen=0
		case 2:k=g6w2(nwor(1),nwor(2),26,26,26,26):if k>0 then words+=1:wor(words)=k:nwlen=0
		case 3:k=g6w2(nwor(1),nwor(2),nwor(3),26,26,26):if k>0 then words+=1:wor(words)=k:nwlen=0
		case 4:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),26,26):if k>0 then words+=1:wor(words)=k:nwlen=0
		case 5:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),nwor(5),26):if k>0 then words+=1:wor(words)=k:nwlen=0
		case 6:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),nwor(5),nwor(6)):if k>0 then words+=1:wor(words)=k:nwlen=0
	end select
	if nwlen>0 then
		words+=1
		wor(words)=0
		wordlen+=nwlen
		nwlen=0
	end if

	if words>1 then
		wscore=0
		wscore_current=0
		wscore2=0
		wscore3=0
		valid_triples=0
		for i=1 to words-1 'score words with word 2-grams
			wscore_current=wl2(wor(i),wor(i+1))
			wscore+=wscore_current
			if (wscore_current>0 and wscore2>0) then
				w31=wl13(wor(i-1))
				if (w31 > 0) then
					w32=wl13(wor(i))
					if (w32 > 0) then
						w33=wl13(wor(i+1))
						if (w33 > 0) then
							valid_triples+=1
							wscore3+=wl3(w31,w32,w33)
						end if	
					end if
				end if
			end if	
			wscore2=wscore_current  ' save trailing wordpair score
		next i
		if wscore>0 then
			wscore/=(words-1 + wordlen/6.0)   ' beijinghouse last term punishes unassigned "wordless" letters
			wscore/=4.0   							  ' beijinghouse only score triples in top 2k most common words
			if valid_triples>0 then wscore+=(wscore3/valid_triples)/4.0 ' but don't punish words for not being in this set
														 ' just discouraging repeativite stop-word strings like "IS THIS IS"
		end if
	end if


end if