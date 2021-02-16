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
				'wordlen+=nwlen
				nwlen=0
			end if
			if wl(j,0)>wngs then 'if word length >7 then check word vs wordlist entry
				e=1
				'if wl(j,0)>=l-(i-1) then
					for k=0 to wl(j,0)-1
						if wl(j,k+1)<>sol(i+k)+65 then
							e=0
							exit for
						end if
					next k
				'end if
				if e=1 then
					words+=1
					wor(words)=j
					'wordlen+=wl(j,0)
					i+=wl(j,0)-1 'skip other letters of the found word
				else
					nwlen+=1 ''
					nwor(nwlen)=sol(i) ''
''													words+=1
''													wor(words)=j
					'wordlen+=wl(j,0)
''													i+=wl(j,0)-1 'skip other letters of the found word
				end if
			else
				words+=1
				if wl(j,0)>l-(i-1) then wor(words)=0 else wor(words)=j
				'wordlen+=wl(j,0)
				i+=wl(j,0)-1 'skip other letters of the found word
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
		'wordlen+=nwlen
		nwlen=0
	end if
	if words>1 then
		wscore = 0
		j = 0
		for i=1 to words-1 'score words with word 2-grams
			wscore+=wl2(wor(i),wor(i+1))
'			j += wl(wor(i),0)
		next i
'		j += wl(wor(words),0)
		if wscore>0 then
			wscore/=words-1   ' normal
'			wscore/=(12-j)         ' avg relation score / avg length
		end if
	end if
end if