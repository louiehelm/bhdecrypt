sub stats_unigrams
	
	randomize 12345
	dim as string os,t
	dim as integer e,o,h,i,j,k,x,y,ns,vow,con
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer num=info_numerical
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer grid(dx,dy)
	dim as long grid2(dx,dy)
	dim as long gridline(l)
	dim as long nuba2(l)
	dim as integer row_total
	dim as integer col_total
	dim as double c1
	
	dim as long cip(l)
	dim as long sym(l)
	dim as long frq(s)
	dim as double frq0(25)
	dim as double frq1(25)
	dim as double frq2(25)
	dim as short f0,f1,f2,spaces,numbers,symbols
	
	for i=1 to l
		cip(i)=info(i)
		sym(nuba(i))=info(i)
		frq(nuba(i))+=1
	next i
	
	i=0
	for y=1 to dy
		for x=1 to dx
			i+=1
			grid(x,y)=nuba(i)
			grid2(x,y)=info(i)
			if i=l then exit for,for
		next x
	next y
	
	'm_bigramstructure(info(),l,s,dx,dy,1,10)
	'm_bigramstructure(info(),l,s,dx,dy,1,29)
	
	os+="BHdecrypt unigrams stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	os+="Length: "+str(l)+lb
	os+="Symbols: "+str(s)+lb
	os+="Dimensions: "+str(dx)+" by "+str(dy)+lb
	os+="Multiplicity: "+str(s/l)+lb
	os+="Entropy: "+str(m_entropy(freq(),l,s))+lb
	'os+="Normalized entropy: "+str(m_entropy(freq(),l,s)/m_entropymax(l,s))+lb
	'os+="Smoothness: "+str(m_smoothness(freq(),l,s,1))+lb
	os+="Index of coincidence:"+lb
	os+="- Raw: "+str(m_ioc(freq(),l,s,0))+lb
	os+="- Normalized: "+str(m_ioc(freq(),l,s,1))+lb
	os+="- Versus random: "+str(m_ioc(freq(),l,s,1)/(1/s))+lb
	os+="- Flatness: "+str(m_ioc(freq(),l,s,2))+lb
	os+="Alternative flatness: "+str(m_flatness(nuba(),info_length,info_symbols,1))+lb
	os+="Unigram distance: "+str(m_unigramdistance(nuba(),l,s))+lb
	
	if num=0 then
		os+=lb
		for i=1 to l
			select case cip(i)
				case 32 'spaces
					spaces+=1
				case 48 to 57 'numbers
				 	numbers+=1
				case 97 to 122 'lowercase
					f0+=1
					f2+=1
					frq0(cip(i)-97)+=1
					frq2(cip(i)-97)+=1
					select case cip(i)
						case 97,101,105,111,117
							vow+=1
						case else
							con+=1
					end select
				case 65 to 90 'uppercase
					f1+=1
					f2+=1
					frq1(cip(i)-65)+=1
					frq2(cip(i)-65)+=1
					select case cip(i)
						case 65,69,73,79,85
							vow+=1
						case else
							con+=1
					end select
				case else
					symbols+=1
			end select
		next i
		os+="Spaces: "+str(spaces)+" ("+format(spaces/l*100,"0.00")+"%)"+lb
		os+="Symbols: "+str(symbols)+" ("+format(symbols/l*100,"0.00")+"%)"+lb
		os+="Numbers: "+str(numbers)+" ("+format(numbers/l*100,"0.00")+"%)"+lb
		os+="Lowercase letters: "+str(f0)+" ("+format(f0/l*100,"0.00")+"%)"+lb
		os+="Uppercase letters: "+str(f1)+" ("+format(f1/l*100,"0.00")+"%)"+lb
		os+="Vowels: "+str(vow)+" ("+format(vow/l*100,"0.00")+"%)"+lb
		os+="Consonants: "+str(con)+" ("+format(con/l*100,"0.00")+"%)"+lb
		if f0+f1>0 then
			os+=lb
			os+="Chi-square versus English, first-letter English:"+lb
			os+="---------------------------------------------------------"+lb
		end if
		if f0>0 then os+="Lowercase: "+format(m_chi2_english(chi2(),frq0(),f0),"0.00")+", "+format(m_chi2_english(chi2fl(),frq0(),f0),"0.00")+lb
		if f1>0 then os+="Uppercase: "+format(m_chi2_english(chi2(),frq1(),f1),"0.00")+", "+format(m_chi2_english(chi2fl(),frq1(),f1),"0.00")+lb
		if f0>0 andalso f1>0 then os+="Lowercase + uppercase: "+format(m_chi2_english(chi2(),frq2(),f2),"0.00")+", "+format(m_chi2_english(chi2fl(),frq2(),f2),"0.00")+lb
		if f0+f1>0 then os+=lb
		if f0>0 then os+="Normor lowercase: "+format(m_normor(frq0()),"0.00")+lb
		if f1>0 then os+="Normor uppercase: "+format(m_normor(frq1()),"0.00")+lb
		if f0>0 andalso f1>0 then os+="Normor lowercase + uppercase: "+format(m_normor(frq2()),"0.00")+lb
	end if
	
	'if f0+f1>0 then
	'	os+=lb
	'	os+="Chi-square versus Hafer shifts English:"+lb
	'	os+="---------------------------------------------------------"+lb
	'	dim as short ita(len(intext)),itacopy(len(intext))
	'	dim as double itafrq(25)
	'	for i=1 to len(intext)
	'		itacopy(i)=asc(intext,i)-65 'uppercase
	'	next i
	'	for i=0 to 25 'shifts
	'		erase itafrq
	'		for j=1 to len(intext)
	'			ita(j)=itacopy(j)
	'			if int(rnd*2)=0 then
	'				ita(j)-=i
	'				if ita(j)<0 then ita(j)+=26
	'			else
	'				ita(j)+=i
	'				if ita(j)>25 then ita(j)-=26
	'			end if
	'			itafrq(ita(j))+=1/len(intext)
	'		next j
	'		os+="-"+str(i)+"/+"+str(i)+": "+rdc(m_chi2_english(itafrq(),frq2(),f2),2)+lb
	'	next i
	'end if
	
	t=frequencies(info(),info_length,info_symbols,1,info_numerical,0) '1-gram frequencies
	if t<>"" then
		os+=lb
		os+=t
	end if
	
	os+=lb+lb
	os+="Grouped unigram repeats, offsets: "+lb
	os+="---------------------------------------------------------"
	dim as integer dxcap=dx
	if dxcap>100 then dxcap=100
	for i=2 to dxcap
		os+=lb
		os+="Per "+str(i)+": "
		for j=1 to i
			h=j
			for k=1 to l
				nuba2(h)=nuba(k)
				h+=1
				if h>l then h=1
			next k
			c1=m_unigrams(nuba2(),info_length,info_symbols,i,0)
			os+=str(c1)
			if j<>i then os+=", "
		next j
	next i
	
	os+=lb+lb
	dim as short lx
	dim as double me,me_total
	os+="Per row unigram repeats:"+lb
	os+="---------------------------------------------------------"
	for y=1 to dy
		lx=0
		for x=1 to dx
			if grid(x,y)>0 then 
				lx+=1
				gridline(x)=grid(x,y)
			end if
		next x
		if lx>1 then
			ns=nba_to_info_out(gridline(),lx,s)
			os+=lb
			j=m_unigrams(info_out(),lx,ns,lx,0)
			row_total+=j
			me_total+=me
			os+="Row "+str(y)+": "+str(j)
		end if	
	next y
	os+=lb
	os+="Row total: "+str(row_total)
	
	os+=lb
	os+=lb
	os+="Per column unigram repeats:"+lb
	os+="---------------------------------------------------------"
	for x=1 to dx
		lx=0
		for y=1 to dy
			if grid(x,y)>0 then
				lx+=1
				gridline(y)=grid(x,y)
			end if
		next y
		if lx>1 then
			ns=nba_to_info_out(gridline(),lx,s)
			os+=lb
			j=m_unigrams(info_out(),lx,ns,lx,0)
			col_total+=j
			os+="Column "+str(x)+": "+str(j)
		end if
	next x
	os+=lb
	os+="Column total: "+str(col_total)
	
	dim as double avgrc
	os+=lb
	os+=lb
	os+="Row count without unigram repeats, offsets:"+lb
	os+="---------------------------------------------------------"+lb
	for x=2 to dxcap
		y=l/x
		if x*y<l then y+=1
		os+="Row length "+str(x)+": "
		avgrc=0
		for j=0 to x-1
			'avgrc+=m_unigramunitnorepeats(nuba(),l,s,x,y,0,j,0)
			os+=str(m_unigramunitnorepeats(nuba(),l,s,x,y,0,j,0))
			if j<>x-1 then os+=", "
		next j
		'os+=str(avgrc/x)
		if x<>dxcap then os+=lb
	next x
	 
	dim as short udf1(s,l),udf2(l),udfcurr
	dim as integer udfreps,dx2=dx
	dim as double udftot,udfdiff,udftot2
	for i=1 to l
		udf1(nuba(i),0)+=1
		udf1(nuba(i),udf1(nuba(i),0))=i
	next i

	os+=lb
	os+=lb
	os+="Unigram distance frequencies modulo x:"+lb
	os+="---------------------------------------------------------"
	if dx2>100 then dx2=100
	
	for h=1 to dxcap
		udfreps=0
		udftot=0
		udfdiff=0
		for i=0 to l
			udf2(i)=0
		next i
		os+=lb
		os+="Modulo "+str(h)+": "
		for i=1 to s
			for j=1 to udf1(i,0)-1
				udfcurr=udf1(i,j+1)-udf1(i,j)
				udf2(udfcurr mod h)+=1
			next j
		next i
		for i=0 to h-1
			'if udf2(i)>0 then
				os+=str(udf2(i))
				if i<>h-1 then os+=", "
				'udftot+=udf2(i)*i
				'udfreps+=(udf2(i)-1)
				udftot+=udf2(i)
				udfreps+=1
			'end if
		next i
		udftot2=(udftot-(udftot/udfreps))+((udftot/udfreps)*(h-1))
		udftot/=udfreps
		for i=0 to h-1
			udfdiff+=abs(udf2(i)-udftot)
		next i
		if h>1 then
			os+=" ("+str(int((udfdiff/udftot2)*100))+"%)"
		else
			os+=" (0%)"
		end if
	next h
	
	os+=lb+lb
	os+="Numerically summed row totals:"+lb
	os+="---------------------------------------------------------"+lb
	k=0
	for y=1 to dy
		k=0
		for x=1 to dx
			k+=grid2(x,y)
		next x
		os+="Row "+str(y)+": "+str(k)
		if y<>dy then os+=lb
	next y
	
	os+=lb+lb
	os+="Numerically summed column totals:"+lb
	os+="---------------------------------------------------------"+lb
	for x=1 to dx
		k=0
		for y=1 to dy
			k+=grid2(x,y)
		next y
		os+="Column "+str(x)+": "+str(k)
		if x<>dx then os+=lb
	next x
	
	dim as double hlean(s),vlean(s)
	dim as double avgh1,avgv1,avgh2,avgv2,dh,dv
	dim as integer avgh1c,avgh2c,avgv1c,avgv2c
	dim as long sym1(s),sym2(s),fr1(s),fr2(s)
	for y=1 to dy
		for x=1 to dx
			hlean(grid(x,y))+=x
			vlean(grid(x,y))+=y
		next x
	next y
	for i=1 to s
		fr1(i)=frq(i)
		fr2(i)=frq(i)
		sym1(i)=sym(i)
		sym2(i)=sym(i)
		hlean(i)=hlean(i)-(((1+dx)/2)*frq(i))
		vlean(i)=vlean(i)-(((1+dy)/2)*frq(i))
	next i
	do
		e=0
		for i=1 to s-1
			if abs(hlean(i))<abs(hlean(i+1)) then
				e=1
				swap hlean(i),hlean(i+1)
				swap sym1(i),sym1(i+1)
				swap fr1(i),fr1(i+1)
			end if
		next i
	loop until e=0
	do
		e=0
		for i=1 to s-1
			if abs(vlean(i))<abs(vlean(i+1)) then
				e=1
				swap vlean(i),vlean(i+1)
				swap sym2(i),sym2(i+1)
				swap fr2(i),fr2(i+1)
			end if
		next i
	loop until e=0
	dim as string osh,osv
	os+=lb+lb
	for i=1 to s
		if num=0 then osh+=chr(sym1(i)) else osh+=str(sym1(i))
		if hlean(i)>=0 then osh+=": +" else osh+=": "
		osh+=format(hlean(i),"0.00")+" ("+str(fr1(i))+")"+lb
		if hlean(i)>0 then avgh1+=abs(hlean(i)):avgh1c+=1
		if hlean(i)<0 then avgh2+=abs(hlean(i)):avgh2c+=1
	next i
	os+="Horizontal symbol leaning (average "+format((avgh1+avgh2)/(avgh1c+avgh2c),"0.00")+", -"+format(avgh2/avgh2c,"0.00")+", +"+format(avgh1/avgh1c,"0.00")+"):"+lb
	os+="---------------------------------------------------------"+lb
	os+=osh+lb
	for i=1 to s
		if num=0 then osv+=chr(sym2(i)) else osv+=str(sym2(i))
		if vlean(i)>=0 then osv+=": +" else osv+=": "
		osv+=format(vlean(i),"0.00")+" ("+str(fr2(i))+")"
		if i<>s then osv+=lb
		if vlean(i)>0 then avgv1+=abs(vlean(i)):avgv1c+=1
		if vlean(i)<0 then avgv2+=abs(vlean(i)):avgv2c+=1
	next i
	os+="Vertical symbol leaning (average "+format((avgv1+avgv2)/(avgv1c+avgv2c),"0.00")+", -"+format(avgv2/avgv2c,"0.00")+", +"+format(avgv1/avgv1c,"0.00")+"):"+lb
	os+="---------------------------------------------------------"+lb
	os+=osv
	
	ui_editbox_settext(output_text,os)
	randomize timer
	
end sub

sub stats_hafershifts
	
	randomize 12345
	dim as string os
	dim as integer h,i,j,k,x,y,a,b,c,e,r,it
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer num=info_numerical
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long cip(l)
	dim as double frq2(25)
	dim as short itacopy(len(intext))
	dim as double itafrq(25)
	dim as short f2
	
	for i=1 to l
		cip(i)=info(i)
	next i
	
	if num=0 then
		for i=1 to l
			select case cip(i)
				case 97 to 122 'lowercase
					f2+=1
					frq2(cip(i)-97)+=1
				case 65 to 90 'uppercase
					f2+=1
					frq2(cip(i)-65)+=1
			end select
		next i
	end if
	
	'To do: test versus randomly generated
	
	os+="AZdecrypt Hafer shifts stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb+lb
	if f2>0 then
		os+="Chi^2 versus English: -N/+N"+lb
		os+="----------------------------------"+lb
		for i=1 to len(intext)
			itacopy(i)=asc(intext,i)-65 'uppercase
		next i
		for i=0 to 13 'shifts
			erase itafrq
			for j=1 to len(intext)
				it=itacopy(j)
				if int(rnd*2)=0 then
					it-=i
					if it<0 then it+=26
				else
					it+=i
					if it>25 then it-=26
				end if
				itafrq(it)+=1/len(intext)
			next j
			os+="-"+str(i)+"/+"+str(i)+": "+format(m_chi2_english(itafrq(),frq2(),f2),"0.00")+lb
		next i
		os+=lb
		os+="Chi^2 versus English: any 2 shifts"+lb
		os+="----------------------------------"+lb
		for i=1 to len(intext)
			itacopy(i)=asc(intext,i)-65 'uppercase
		next i
		dim as double scheck(25,25,2),onelen=1/len(intext)
		for i=0 to 25 'shifts
			for h=0 to 25 'shifts
				for k=0 to 2 '--,++,-+
					if scheck(i,h,k)=0 andalso scheck(h,i,k)=0 then 'no doubles
						erase itafrq
						for j=1 to len(intext)
							it=itacopy(j)
							if int(rnd*2)=0 then
								select case k
									case 0:it-=i
									case 1:it+=i
									case 2:it-=i
								end select
								if it<0 then it+=26
								if it>25 then it-=26
							else
								select case k
									case 0:it-=h
									case 1:it+=h
									case 2:it+=h
								end select
								if it<0 then it+=26
								if it>25 then it-=26
							end if
							itafrq(it)+=onelen
						next j
						scheck(i,h,k)=m_chi2_english(itafrq(),frq2(),f2)
					end if
				next k
			next h
		next i
		dim as bytebytebytedouble list(26*26*3)
		h=0
		for i=0 to 25
			for j=0 to 25
				for k=0 to 2
					if scheck(i,j,k)<>0 then
						h+=1
						list(h).b1=i
						list(h).b2=j
						list(h).b3=k
						list(h).d1=scheck(i,j,k)
					end if
				next k
			next j
		next i
		do
			e=0
			for i=1 to h-1
				if list(i).d1>list(i+1).d1 then
					e=1
					swap list(i).b1,list(i+1).b1
					swap list(i).b2,list(i+1).b2
					swap list(i).b3,list(i+1).b3
					swap list(i).d1,list(i+1).d1
				end if
			next i
		loop until e=0
		for i=1 to h
			select case list(i).b3
				case 0:os+="-"+str(list(i).b1)+"/-"+str(list(i).b2)+": "
				case 1:os+="+"+str(list(i).b1)+"/+"+str(list(i).b2)+": "
				case 2:os+="-"+str(list(i).b1)+"/+"+str(list(i).b2)+": "
			end select
			os+=format(list(i).d1,"0.00")
			if i<>h then os+=lb
		next i
	else
		os+="Error: cipher must contain letters"
	end if
	
	ui_editbox_settext(output_text,os)
	
end sub

sub stats_ngrams
	
	dim as string os,t
	dim as integer i,j
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer num=info_numerical
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as double avg_bigrams
	
	os+="BHdecrypt n-grams stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	os+="Bigrams: "+str(m_bigrams(nuba(),l,s,0))+lb
	os+="- Normalized: "+str(m_bigrams(nuba(),l,s,1))+lb
	os+="Bigram IOC: "+str(m_bigrams(nuba(),l,s,2))+lb
	os+="- Normalized: "+str(m_bigrams(nuba(),l,s,3))+lb
	'os+="Deep bigrams: "+str(int(m_deep(nuba(),l,s,1,0,0)))+lb
	os+="Trigrams: "+str(int(m_fasttrigrams(nuba(),l,s)))+lb
	'os+="Repeats: "+str(m_ngrams(nuba(),l,0))+lb
	'os+="Asymmetry: "+str(int(m_asymmetry(nuba(),l,s,0)))+lb
	os+=m_repeats(info(),l,num)+lb
	
	dim as short a,b
	dim as short id(s,l)
	dim as long bmap(l)
	for i=1 to l
		if num=0 then
			bmap(i)=32
		else
			bmap(i)=123456789
		end if
	next i
	for i=1 to l-1
		a=nuba(i)
		b=nuba(i+1)
		id(a,b)+=1
	next i
	for i=1 to l-1
		a=nuba(i)
		b=nuba(i+1)
		if id(a,b)>1 then
			bmap(i)=info(i)
			bmap(i+1)=info(i+1)
		end if
	next i
	
	os+=lb
	os+="Bigram map:"+lb
	os+="---------------------------------------------------------"+lb
	dim as short oldunispacing=unispacing
	unispacing=1
	os+=info_to_string(bmap(),l,dx,dy,num,0,0)
	unispacing=oldunispacing
	
	for i=2 to info_length
		t=frequencies(info(),l,s,i,num,1)
		if t<>"" then
			os+=lb
			os+=lb
			os+=t
		else exit for
		end if
	next i
	
	os+=lb+lb
	os+="Bigrams exclusive to positions modulo x:"+lb
	os+="---------------------------------------------------------"
	for i=2 to dx
		'if i<>dx then 
		os+=lb
		os+="Bigrams mod "+str(i)+": "
		for j=0 to i-1
			os+=str(m_bigramposmodn(nuba(),l,s,i,j))
			if j<>i-1 then os+=", "		
		next j
	next i
	
	ui_editbox_settext(output_text,os)
	randomize timer
	
end sub

sub stats_observations(byval tn_ptr as any ptr)
	
	stats_running=1
	randomize 12345
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	
	dim as string os,ot
	dim as integer h,i,j,k,e,c
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long cip(l)
	dim as long nba(l)
	dim as long nba2(l),nba0(l)
	dim as integer state=1
	
	for i=1 to l
		cip(i)=info(i)
		nba(i)=nuba(i)
		nba0(i)=nuba(i)
	next i
	
	dim as double arg(100)
	dim as integer shuffles=10000
	dim as integer odds=shuffles/100
	dim as integer statscount=4
	dim as integer ov
	dim as double obs(statscount,l,5) 'stat, obs, obs_count/score/odds/tp/highlow/period
	dim as double stats(statscount,shuffles)
	dim as long stats2(1,shuffles,l/2)
	dim as double curr,curr1,curr2
	
	dim as integer r1,r2
	for i=1 to 1000 'warm up rng
		state=48271*state and 2147483647
	next i
	
	for i=1 to l
		cstate(11,i)=nba(i)
	next i
	
	for j=1 to l*l 'pre-shuffle
		state=48271*state and 2147483647
		r1=1+l*state shr 31
		state=48271*state and 2147483647
		r2=1+l*state shr 31
		swap nba(r1),nba(r2)
	next j
	
	dim as double obstimer=timer
	for i=1 to shuffles
		for j=1 to l
			state=48271*state and 2147483647
			r1=1+l*state shr 31
			state=48271*state and 2147483647
			r2=1+l*state shr 31
			swap nba(r1),nba(r2)
			'swap nba(int(rnd*l)+1),nba(int(rnd*l)+1)
		next j
		stats(0,i)=m_fastbigrams(nba(),l,s)
		stats(1,i)=m_ngramfragments(nba(),l,s,5)
		stats(2,i)=m_2cycles(nba(),l,s,5)
		stats(3,i)=m_unigrams(nba(),l,s,dx,1)
		for k=1 to l/2
			stats2(0,i,k)=m_gridioc(nba(),l,s,0,k)
			stats2(1,i,k)=m_gridioc(nba(),l,s,1,k)
		next k
		if timer-obstimer>1 then
			obstimer=timer
			ot="Observations: "+format((i/shuffles)*100,"0.00")+"% complete" '+lb
			'ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
	next i
	
	os+="BHdecrypt observations stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	
	for h=0 to 1
		for i=1+h to l/2
			arg(1)=l
			arg(2)=s
			arg(5)=h 'transpostion
			arg(7)=i 'period
			dim as string opstring=cstate_operation(11,12,"Period",arg())
			for j=1 to l
				nba2(j)=cstate(12,j)
			next j
			for k=0 to statscount
				select case k
					case 0:curr=m_fastbigrams(nba2(),l,s)
					case 1:curr=m_ngramfragments(nba2(),l,s,5)
					case 2:curr=m_2cycles(nba2(),l,s,5)
					case 3:curr=m_unigrams(nba2(),l,s,dx,1)
					case 4:curr=m_gridioc(nba0(),l,s,h,i)
				end select	
				curr1=0
				curr2=0
				select case k
					case 0 to 3
						for j=1 to shuffles
							if stats(k,j)>=curr then curr1+=1
							if stats(k,j)<=curr then curr2+=1
						next j
					case 4
						for j=1 to shuffles
							if stats2(h,j,i)>=curr then curr1+=1
							if stats2(h,j,i)<=curr then curr2+=1
						next j
				end select
				if curr1<=odds then
					obs(k,0,0)+=1 'obs_count
					j=obs(k,0,0)
					obs(k,j,1)=curr 'score
					obs(k,j,2)=curr1 'odds
					obs(k,j,3)=h 'tp
					obs(k,j,4)=1 'high
					obs(k,j,5)=i 'period
				end if
				if curr2<=odds then
					obs(k,0,0)+=1 'obs_count
					j=obs(k,0,0)
					obs(k,j,1)=curr 'score
					obs(k,j,2)=curr2 'odds
					obs(k,j,3)=h 'tp
					obs(k,j,4)=0 'low
					obs(k,j,5)=i 'period
				end if
			next k
		next i
	next h
	
	for h=0 to statscount 'sort
		do
			e=0
			for i=1 to obs(h,0,0)-1 'obs
				if obs(h,i,2)>obs(h,i+1,2) then
					e=1
					for j=1 to 5
						swap obs(h,i,j),obs(h,i+1,j)
					next j
				end if
			next i
		loop until e=0
	next h
	
	j=0
	for h=0 to statscount
		j+=obs(h,0,0)
	next h
	os+="Normalized observations: "+format((j/(statscount+1))/int(l/2),"0.00")
	
	dim as string tp,stat,hi
	
	for h=0 to statscount
		for i=1 to obs(h,0,0)
			if h<4 then
				if obs(h,i,3)=0 then tp="TP" else tp="UTP"
			else
				if obs(h,i,3)=0 then tp="By rows" else tp="By columns"
			end if
			if obs(h,i,4)=0 then hi=" (Low)" else hi=" (High)"
			select case h
				case 0:stat="Period: Bigram repeats, Odds ("+str(obs(h,0,0))+")"
				case 1:stat="Period: 5-gram fragment repeats, Odds ("+str(obs(h,0,0))+")"
				case 2:stat="Period: 2-symbol cycles score, Odds ("+str(obs(h,0,0))+")"
				case 3:stat="Period: Unigram repeats, Odds ("+str(obs(h,0,0))+")"
				case 4:stat="Row/column length: Raw IOC, Odds ("+str(obs(h,0,0))+")"
			end select
			if i=1 then
				os+=lb+lb
				os+=stat+lb
				os+="---------------------------------------------------------"+lb
			end if
			select case h
				case 0 to 3
					os+="Period("+tp+","+str(obs(h,i,5))+"): "+str(int(obs(h,i,1)))+hi+", Odds: "+str(obs(h,i,2))+" in "+str(shuffles)
				case 4
					os+="Length("+tp+","+str(obs(h,i,5))+"): "+str(int(obs(h,i,1)))+hi+", Odds: "+str(obs(h,i,2))+" in "+str(shuffles)
			end select
			if i<>obs(h,0,0) then os+=lb
		next i	
	next h
	
	ui_editbox_settext(output_text,os)
	stats_running=0
	thread_ptr(threadsmax+2)=0
	randomize timer
	
end sub

sub stats_symbolcyclepatterns(byval tn_ptr as any ptr)
	
	stats_running=1
	randomize 12345
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
		
	dim as string os,os2,ot,his,los,ng
	dim as integer i,j,k,g
	dim as integer rnds=stats_symbolcyclepatternsrndtrials
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as long cip(l)
	dim as long nba(l)
	for i=1 to l
		cip(i)=info(i)
		nba(i)=nuba(i)
	next i
	dim as short s1,s2,s3,s4,s5,s6,s7,s8,s9,s10
	dim as integer n,n1,n2,n3,n4,n5,n6,n7,n8,n9
	dim as double diff,d,av,hi,lo=999999999
	dim as short cs=stats_symbolcyclepatternscs
	dim as short fl=stats_symbolcyclepatternsfl
	dim as integer csfl=cs^fl
	
	if s<cs then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,"Error: input has too few symbols")
		exit sub
	end if
	
	n1=cs
	n2=cs^2
	n3=cs^3
	n4=cs^4
	n5=cs^5
	n6=cs^6
	n7=cs^7
	n8=cs^8
	n9=cs^9
	
	os+="BHdecrypt symbol cycle patterns stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	os+=str(cs)+"-symbol cycle "+str(fl)+"-gram patterns:"+lb
	
	'erase graph
	redim nscf(rnds,csfl)
	dim as double sda(rnds)
	
	for i=0 to rnds
		for j=0 to csfl
			nscf(i,j)=0
		next j
	next i
	
	get_cyclepatterns(nba(),l,s,cs,fl,0)

	dim as double prgtimer=timer
	
	for i=1 to rnds
		for j=1 to l*10
			swap nba(int(rnd*l)+1),nba(int(rnd*l)+1)
		next j
		nba_to_info_out(nba(),l,s)
		get_cyclepatterns(info_out(),l,s,cs,fl,i)
		if timer-prgtimer>1 then
			prgtimer=timer
			ot="Symbol cycle patterns: "+format((i/rnds)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		if stoptask=1 then
			stoptask=0
			stats_running=0
			ui_editbox_settext(output_text,"")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if
	next i
	
	j=0
	select case fl
		case 2
			for s2=0 to cs-1
				for s1=0 to cs-1
					n=s1+(s2*n1) '2
					for i=1 to rnds
						sda(i)=nscf(i,n)
					next i
					d=stdev(nscf(0,n),rnds,sda())
					os2+=lb
					ng=chr(s1+65)+chr(s2+65)
					os2+=ng+": "+format(d,"0.00") '4
					if d>hi then hi=d:his=ng
					if d<lo then lo=d:los=ng
					av+=abs(d)
					j+=1
					'graph(1,1,j)=d
				next s1
			next s2
		case 3
			for s3=0 to cs-1
				for s2=0 to cs-1
					for s1=0 to cs-1
						n=s1+(s2*n1)+(s3*n2) '3
						for i=1 to rnds
							sda(i)=nscf(i,n)
						next i
						d=stdev(nscf(0,n),rnds,sda())
						os2+=lb
						ng=chr(s1+65)+chr(s2+65)+chr(s3+65)
						os2+=ng+": "+format(d,"0.00") '4
						if d>hi then hi=d:his=ng
						if d<lo then lo=d:los=ng
						av+=abs(d)
						j+=1
						'graph(1,1,j)=d
					next s1
				next s2
			next s3
		case 4
			for s4=0 to cs-1
				for s3=0 to cs-1
					for s2=0 to cs-1
						for s1=0 to cs-1
							n=s1+(s2*n1)+(s3*n2)+(s4*n3) '4
							for i=1 to rnds
								sda(i)=nscf(i,n)
							next i
							d=stdev(nscf(0,n),rnds,sda())
							os2+=lb
							ng=chr(s1+65)+chr(s2+65)+chr(s3+65)+chr(s4+65)
							os2+=ng+": "+format(d,"0.00") '4
							if d>hi then hi=d:his=ng
							if d<lo then lo=d:los=ng
							av+=abs(d)
							j+=1
							'graph(1,1,j)=d
						next s1
					next s2
				next s3
			next s4
		case 5
			for s5=0 to cs-1
				for s4=0 to cs-1
					for s3=0 to cs-1
						for s2=0 to cs-1
							for s1=0 to cs-1
								n=s1+(s2*n1)+(s3*n2)+(s4*n3)+(s5*n4) '5
								for i=1 to rnds
									sda(i)=nscf(i,n)
								next i
								d=stdev(nscf(0,n),rnds,sda())
								os2+=lb
								ng=chr(s1+65)+chr(s2+65)+chr(s3+65)+chr(s4+65)+chr(s5+65)
								os2+=ng+": "+format(d,"0.00") '5
								if d>hi then hi=d:his=ng
								if d<lo then lo=d:los=ng
								av+=abs(d)
								j+=1
								'graph(1,1,j)=d
							next s1
						next s2
					next s3
				next s4
			next s5
		case 6
			for s6=0 to cs-1
				for s5=0 to cs-1
					for s4=0 to cs-1
						for s3=0 to cs-1
							for s2=0 to cs-1
								for s1=0 to cs-1
									n=s1+(s2*n1)+(s3*n2)+(s4*n3)+(s5*n4)+(s6*n5) '6
									for i=1 to rnds
										sda(i)=nscf(i,n)
									next i
									d=stdev(nscf(0,n),rnds,sda())
									os2+=lb
									ng=chr(s1+65)+chr(s2+65)+chr(s3+65)+chr(s4+65)+chr(s5+65)+chr(s6+65)
									os2+=ng+": "+format(d,"0.00") '6
									if d>hi then hi=d:his=ng
									if d<lo then lo=d:los=ng
									av+=abs(d)
									j+=1
									'graph(1,1,j)=d
								next s1
							next s2
						next s3
					next s4
				next s5
			next s6
		case 7
			for s7=0 to cs-1
				for s6=0 to cs-1
					for s5=0 to cs-1
						for s4=0 to cs-1
							for s3=0 to cs-1
								for s2=0 to cs-1
									for s1=0 to cs-1
										n=s1+(s2*n1)+(s3*n2)+(s4*n3)+(s5*n4)+(s6*n5)+(s7*n6) '7
										for i=1 to rnds
											sda(i)=nscf(i,n)
										next i
										d=stdev(nscf(0,n),rnds,sda())
										os2+=lb
										ng=chr(s1+65)+chr(s2+65)+chr(s3+65)+chr(s4+65)+chr(s5+65)+chr(s6+65)+chr(s7+65)
										os2+=ng+": "+format(d,"0.00") '7
										if d>hi then hi=d:his=ng
										if d<lo then lo=d:los=ng
										av+=abs(d)
										j+=1
										'graph(1,1,j)=d
									next s1
								next s2
							next s3
						next s4
					next s5
				next s6
			next s7
		case 8
			for s8=0 to cs-1
				for s7=0 to cs-1
					for s6=0 to cs-1
						for s5=0 to cs-1
							for s4=0 to cs-1
								for s3=0 to cs-1
									for s2=0 to cs-1
										for s1=0 to cs-1
											n=s1+(s2*n1)+(s3*n2)+(s4*n3)+(s5*n4)+(s6*n5)+(s7*n6)+(s8*n7) '8
											for i=1 to rnds
												sda(i)=nscf(i,n)
											next i
											d=stdev(nscf(0,n),rnds,sda())
											os2+=lb
											ng=chr(s1+65)+chr(s2+65)+chr(s3+65)+chr(s4+65)+chr(s5+65)+chr(s6+65)+chr(s7+65)+chr(s8+65)
											os2+=ng+": "+format(d,"0.00") '8
											if d>hi then hi=d:his=ng
											if d<lo then lo=d:los=ng
											av+=abs(d)
											j+=1
											'graph(1,1,j)=d
										next s1
									next s2
								next s3
							next s4
						next s5
					next s6
				next s7
			next s8
		case 9
			for s9=0 to cs-1
				for s8=0 to cs-1
					for s7=0 to cs-1
						for s6=0 to cs-1
							for s5=0 to cs-1
								for s4=0 to cs-1
									for s3=0 to cs-1
										for s2=0 to cs-1
											for s1=0 to cs-1
												n=s1+(s2*n1)+(s3*n2)+(s4*n3)+(s5*n4)+(s6*n5)+(s7*n6)+(s8*n7)+(s9*n8) '9
												for i=1 to rnds
													sda(i)=nscf(i,n)
												next i
												d=stdev(nscf(0,n),rnds,sda())
												os2+=lb
												ng=chr(s1+65)+chr(s2+65)+chr(s3+65)+chr(s4+65)+chr(s5+65)+chr(s6+65)+chr(s7+65)+chr(s8+65)+chr(s9+65)
												os2+=ng+": "+format(d,"0.00") '9
												if d>hi then hi=d:his=ng
												if d<lo then lo=d:los=ng
												av+=abs(d)
												j+=1
												'graph(1,1,j)=d
											next s1
										next s2
									next s3
								next s4
							next s5
						next s6
					next s7
				next s8
			next s9
		case 10
			for s10=0 to cs-1
				for s9=0 to cs-1
					for s8=0 to cs-1
						for s7=0 to cs-1
							for s6=0 to cs-1
								for s5=0 to cs-1
									for s4=0 to cs-1
										for s3=0 to cs-1
											for s2=0 to cs-1
												for s1=0 to cs-1
													n=s1+(s2*n1)+(s3*n2)+(s4*n3)+(s5*n4)+(s6*n5)+(s7*n6)+(s8*n7)+(s9*n8)+(s10*n9) '10
													for i=1 to rnds
														sda(i)=nscf(i,n)
													next i
													d=stdev(nscf(0,n),rnds,sda())
													os2+=lb
													ng=chr(s1+65)+chr(s2+65)+chr(s3+65)+chr(s4+65)+chr(s5+65)+chr(s6+65)+chr(s7+65)+chr(s8+65)+chr(s9+65)+chr(s10+65)
													os2+=ng+": "+format(d,"0.00") '10
													if d>hi then hi=d:his=ng
													if d<lo then lo=d:los=ng
													av+=abs(d)
													j+=1
													'graph(1,1,j)=d
												next s1
											next s2
										next s3
									next s4
								next s5
							next s6
						next s7
					next s8
				next s9
			next s10
	end select
	
	os+=lb
	os+="Average distance from 0: "+format(av/csfl,"0.00")+lb
	os+="Highest: "+his+": "+format(hi,"0.00")+lb
	os+="Lowest: "+los+": "+format(lo,"0.00")+lb
	
	'graph(1,1,0)=csfl
	'output_graph2(1,1,str(cs)+"-symbol cycle "+str(fl)+"-gram patterns: "+str(left(file_name,len(file_name)-4)),"Symbol cycle patterns")
	
	ui_editbox_settext(output_text,os+os2)
	stats_running=0
	thread_ptr(threadsmax+2)=0
	randomize timer

end sub

sub stats_cycletypes(byval tn_ptr as any ptr)
	
	stats_running=1
	dim as string os,osi
	dim as integer i,j,k,c,t,e,ct,cs,c1,c2,a,b,maxcs
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as long cip(l)
	dim as long nba0(l),nba1(l),nts(s)
	dim as integer num=info_numerical
	dim as short cta(200)
	dim as double variance(20)
	dim as double std(20)
	dim as double sigma(20)
	dim as double score(20)
	dim as double sortedscore(20,1)
	dim as double statustimer=timer
	dim as double runtimer=timer
	'dim as integer ctt(10,100)
	dim as integer rits
	for i=1 to l
		cip(i)=info(i)
		nba0(i)=nuba(i)
		nba1(i)=nuba(i)
		nts(nba0(i))=cip(i)
	next i
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	
	if s<stats_nsymbolcycles then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,"Error: cipher has too few symbols")
		exit sub
	end if
	
	randomize 12345
	
	os+="BHdecrypt "+str(stats_nsymbolcycles)+"-symbol cycle types stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	
	maxcs=5
	rits=stats_symbolcyclepatternsrndtrials
	c2=(maxcs-1)*rits
	erase cto
	
	'redim cto(0 to 0,0,0,0)
	'redim cto(2 to 7,15,10,constcip)
	
	for cs=stats_nsymbolcycles to stats_nsymbolcycles
		c1=0
		select case cs
			case 2 
			case 3
				cta(1)=1 'cycles
				cta(2)=1 'increasingly random cycles
				cta(3)=1 'decreasingly random cycles
				'cta(4)=1 'shortened cycles
				'cta(5)=1 'lengthened cycles
				'cta(6)=1 'alternating length cycles
				'cta(7)=1 'palindromic cycles
				'cta(8)=1 'anti cycles
				'cta(11)=0 'unique cycles
				'cta(12)=0 'pattern cycles
				'cta(13)=0 'random shift cycles
			case 4
				cta(1)=1 'cycles
				cta(2)=1 'increasingly random cycles
				cta(3)=1 'decreasingly random cycles
			case 5
				cta(1)=1 'cycles
				cta(2)=1 'increasingly random cycles
				cta(3)=1 'decreasingly random cycles
			case 6
				cta(1)=1 'cycles
				cta(2)=1 'increasingly random cycles
				cta(3)=1 'decreasingly random cycles
			case 7
		end select
		cta(0)=0
		for i=1 to 20
			if cta(i)=1 then
				erase ctmean
				erase variance
				cta(0)=i 'set to max used
			end if
		next i	
		for i=1 to rits
			for j=1 to l
				swap nba1(int(rnd*l)+1),nba1(int(rnd*l)+1)
			next j
			recbestcycle=0
			e=m_cycletypes(nba1(),l,s,cs,cta())
			for j=1 to cta(0)
				if cta(j)=1 then
					ctmean(j,i)=cto(cs,j,0,0)
					ctmean(j,0)+=cto(cs,j,0,0)
				end if
			next j
			'--------------------------------------------------	
			c1+=1
			if timer-statustimer>1 then
				statustimer=timer	
				osi="BHdecrypt "+str(cs)+"-symbol cycle types: "+format((c1/rits)*100,"0.00")+"% complete"+lb	
				osi+="(click stop task to cancel)"
				ui_editbox_settext(output_text,osi)
			end if
			if stoptask=1 then
				stoptask=0
				stats_running=0
				ui_editbox_settext(output_text,"")
				thread_ptr(threadsmax+2)=0
				exit sub
			end if
			'--------------------------------------------------
		next i
		for i=1 to cta(0)
			if cta(i)=1 then
				ctmean(i,0)/=rits
				for j=1 to rits
					variance(i)+=(ctmean(i,j)-ctmean(i,0))^2
				next j
				variance(i)/=rits
				std(i)=sqr(variance(i))
				cto(cs,i,1,0)=0 'clear best scores
			end if
		next i
		recbestcycle=1
		e=m_cycletypes(nba0(),l,s,cs,cta())
		'os+=lb
		'os+=str(cs)+"-symbol cycles:"+lb
		'os+="---------------------------------------------------------"+lb
		for i=1 to cta(0)
			if cta(i)=1 then
				score(i)=cto(cs,i,0,0)
				sigma(i)=(score(i)-ctmean(i,0))/std(i)
				os+=lb
				select case i
					case 1:os+="Cycles: "
					case 2:os+="Increasingly random cycles: "
					case 3:os+="Decreasingly random cycles: "
					case 4:os+="Shortened cycles: "
					case 5:os+="Lengthened cycles: "
					case 6:os+="Alternating length cycles: "
					case 7:os+="Palindromic cycles: "
					'case 7:os+="Even palindromic cycles: "
					case 8:os+="Anti cycles: "
					'case 11:os+="Unique cycles: "
					'case 12:os+="Pattern cycles: "
					'case 13:os+="Random shift cycles: "
				end select
				os+=format(sigma(i),"0.00")+" sigma"+lb	
				os+="---------------------------------------------------------"+lb	
				for j=1 to 10
					if cto(cs,i,j,0)>0 then
						sortedscore(j,0)=j
						sortedscore(j,1)=cto(cs,i,j,0)
					end if
				next j
				do 'sort
					e=0
					for k=1 to 9
						if sortedscore(k,1)<sortedscore(k+1,1) then
							e=1
							swap sortedscore(k,0),sortedscore(k+1,0)
							swap sortedscore(k,1),sortedscore(k+1,1)
						end if
					next k
				loop until e=0
				for j=1 to 10	
					if sortedscore(j,1)>0 then
						a=sortedscore(j,0)
						for k=1 to cto(cs,i,a,1)
							if info_numerical=1 then
								os+=str(nts(cto(cs,i,a,k+1)))
								if k<int(cto(cs,i,a,1)) then os+=" "
							else
								os+=chr(nts(cto(cs,i,a,k+1)))
							end if
						next k
						os+=": "+format(cto(cs,i,a,0),"0.00")
						if j=10 then
							if i<>cta(0) then os+=lb
						else
							os+=lb
						end if
					end if
				next j		
			end if
		next i
	next cs
	
	
	'os+=lb
	'os+="Runtime: "+format(timer-runtimer,"0.00")
	
	ui_editbox_settext(output_text,os)
	thread_ptr(threadsmax+2)=0
	stats_running=0
	recbestcycle=0
	randomize timer

end sub

sub stats_compare_symbolcyclepatterns(byval tn_ptr as any ptr)
	
	stats_running=1
	dim as integer h,i,j,k,g
	
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer l1=info_length
	dim as integer s1=info_symbols
	dim as long cip1(l1)
	dim as long nba1(l1)
	for i=1 to l1
		cip1(i)=info(i)
		nba1(i)=nuba(i)
	next i
	
	soi=string_to_info(ui_editbox_gettext(output_text))
	if soi<>"Ok" then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer l2=info_length
	dim as integer s2=info_symbols
	dim as long cip2(l2)
	dim as long nba2(l2)
	for i=1 to l2
		cip2(i)=info(i)
		nba2(i)=nuba(i)
	next i
	
	ui_editbox_settext(output_text,"Please wait..."+lb)
	
	dim as string os,os2,ot
	dim as integer rnds=stats_symbolcyclepatternsrndtrials
	dim as short x1,x2,x3,x4,x5,x6,x7,x8,x9
	dim as integer n,n1,n2,n3,n4,n5,n6,n7,n8
	dim as double d ',av,hi,lo=999999999
	dim as short cs=2 'stats_symbolcyclepatternscs
	dim as short fl=6 'stats_symbolcyclepatternsfl
	dim as integer csfl=cs^fl
	
	if s1<cs or s2<cs then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,"Error: input has too few symbols")
		exit sub
	end if
	
	n1=cs
	n2=cs^2
	n3=cs^3
	n4=cs^4
	n5=cs^5
	'n6=cs^6
	'n7=cs^7
	'n8=cs^8
	
	os+="BHdecrypt compare symbol cycle patterns stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	os+=str(cs)+"-symbol cycle "+str(fl)+"-gram patterns:"+lb
	
	erase graph
	redim nscf(rnds,csfl)
	dim as double sda(rnds)
	dim as double prgtimer=timer
	
	for h=1 to 2
		
		randomize 12345
		
		for i=0 to rnds
			for j=0 to csfl
				nscf(i,j)=0
			next j
		next i
		
		if h=1 then
			get_cyclepatterns(nba1(),l1,s1,cs,fl,0)
			for i=1 to rnds
				k+=1
				for j=1 to l1*10
					swap nba1(int(rnd*l1)+1),nba1(int(rnd*l1)+1)
				next j
				nba_to_info_out(nba1(),l1,s1)
				get_cyclepatterns(info_out(),l1,s1,cs,fl,i)	
				if timer-prgtimer>1 then
					prgtimer=timer
					ot="Compare input and output: "+format((k/(rnds*2))*100,"0.00")+"% complete"+lb
					ot+="(click stop task to cancel)"
					ui_editbox_settext(output_text,ot)
				end if
				if stoptask=1 then
					stoptask=0
					stats_running=0
					ui_editbox_settext(output_text,"")
					thread_ptr(threadsmax+2)=0
					exit sub
				end if
			next i
		else
			get_cyclepatterns(nba2(),l2,s2,cs,fl,0)
			for i=1 to rnds
				k+=1
				for j=1 to l2*10
					swap nba2(int(rnd*l2)+1),nba2(int(rnd*l2)+1)
				next j
				nba_to_info_out(nba2(),l2,s2)
				get_cyclepatterns(info_out(),l2,s2,cs,fl,i)
				if timer-prgtimer>1 then
					prgtimer=timer
					ot="Compare input and output: "+format((k/(rnds*2))*100,"0.00")+"% complete"+lb
					ot+="(click stop task to cancel)"
					ui_editbox_settext(output_text,ot)
				end if
				if stoptask=1 then
					stoptask=0
					stats_running=0
					ui_editbox_settext(output_text,"")
					thread_ptr(threadsmax+2)=0
					exit sub
				end if		
			next i
		end if
		
		j=0
		for x6=0 to cs-1
		for x5=0 to cs-1
		for x4=0 to cs-1
		for x3=0 to cs-1
		for x2=0 to cs-1
		for x1=0 to cs-1
			n=x1+(x2*n1)+(x3*n2)+(x4*n3)+(x5*n4)+(x6*n5)
			for i=1 to rnds
				sda(i)=nscf(i,n)
			next i
			d=stdev(nscf(0,n),rnds,sda())
			'os2+=lb
			'os2+=chr(s1+65)+chr(s2+65)+chr(s3+65)+chr(s4+65)+chr(s5+65)+": "+format(d,"0.00")
			'if d>hi then hi=d
			'if d<lo then lo=d
			'av+=abs(d)
			j+=1
			graph(1,h,j)=d
		next x1
		next x2
		next x3
		next x4
		next x5
		next x6
	
	next h
	
	d=0
	for i=1 to csfl
		d+=abs(graph(1,1,i)-graph(1,2,i))
	next i
	os+=lb
	os+="Average difference: "+format(d/csfl,"0.00")+lb
	os+=lb
	os+="Created \Output\Symbol cycle patterns comparison.bmp"
	
	graph(1,1,0)=csfl
	output_graph2(1,2,str(cs)+"-symbol cycle "+str(fl)+"-gram patterns: "+str(left(file_name,len(file_name)-4)),"Symbol cycle patterns comparison")
	
	ui_editbox_settext(output_text,os+os2)
	stats_running=0
	thread_ptr(threadsmax+2)=0
	randomize timer

end sub

sub stats_perfectsymbolcycles(byval tn_ptr as any ptr)
	
	stats_running=1
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as string os,nsc,report,osi
	dim as short i,j,k,d,g,e,al,cl,fm,bl
	dim as uinteger ci,ct,ctd
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as short cip(l)
	dim as short nba(l)
	dim as short sym(s)
	dim as integer num=info_numerical
	dim as short nsymbolcycles=stats_nsymbolcycles
	dim as double statustimer=timer
	dim as short l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,cs,blc
	dim as short t(10),c(10),p(10)
	dim as short frq(s)
	dim as double score
	dim as integer cycles,cyclelengths,lioc
	for i=1 to l
		cip(i)=info(i)
		nba(i)=nuba(i)
		sym(nba(i))=cip(i)
		frq(nba(i))+=1
	next i
	ct=1
	ctd=1
	for i=1 to nsymbolcycles
		ct*=(s-(i-1))
		ctd*=i
	next i
	ct/=ctd
	
	for i=1 to s
		for j=i+1 to s
			ct+=1
		next j
	next i
	select case nsymbolcycles
		case 2:nsc="Perfect 2-symbol cycles"
		case 3:nsc="Perfect 3-symbol cycles"
		case 4:nsc="Perfect 4-symbol cycles"
		case 5:nsc="Perfect 5-symbol cycles"
		case 6:nsc="Perfect 6-symbol cycles"
		case 7:nsc="Perfect 7-symbol cycles"
		case 8:nsc="Perfect 8-symbol cycles"
	end select
	
	os+="BHdecrypt "+lcase(nsc)+" stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	select case nsymbolcycles	
		case 2 'perfect 2-symbol cycles
			cs=2
			lioc=(l-(cs-1))*(l-cs)
			blc=(cs*(cs-1))/2
		   for i=1 to s
		   	if frq(i)>fm then fm=frq(i)
		   next i
			dim as short map(l,fm+1)
			dim as short z(fm*cs)
			for i=1 to l
				map(nba(i),0)+=1
				map(nba(i),map(nba(i),0))=i
			next i
			for i=1 to s
				map(i,map(i,0)+1)=l+1
			next i
			for l1=1 to s
				for l2=l1+1 to s
					if timer-statustimer>1 then
						statustimer=timer
						osi=nsc+": "+format((ci/ct)*100,"0.00")+"% complete"+lb
						osi+="(click stop task to cancel)"
						ui_editbox_settext(output_text,osi)
					end if
					if stoptask=1 then
						stoptask=0
						stats_running=0
						ui_editbox_settext(output_text,"")
						thread_ptr(threadsmax+2)=0
						exit sub
					end if
					ci+=1
					if frq(l1)>1 andalso frq(l2)>1 then
						e=1
						al=0
						cl=0
						for i=1 to cs
							p(i)=1
						next i
						t(1)=l1
						t(2)=l2
						do
							c(1)=map(l1,p(1))
							c(2)=map(l2,p(2))
							d=l+1
							for i=1 to cs
								if c(i)<d then
									d=c(i)
									g=i
								end if
							next i
							if d=l+1 then exit do
							p(g)+=1
							cl+=1
							z(cl)=t(g)
							if cl>(cs-1) then
								i=cl-(cs-1)
								for j=i to i+(cs-2)
									for k=j+1 to i+(cs-1)
										if z(j)=z(k) then
											e=0
											exit do
										end if
									next k
								next j
							end if
						loop		 
						if e=1 then
							al=cl-(cs-1)
							score+=al*(al-1)
							cycles+=1
							cyclelengths+=al
							report+=lb
							for i=1 to cl
								if num=0 then
									report+=chr(sym(z(i)))
								else
									report+=str(sym(z(i)))
									if i<>cl then report+=" "
								end if
							next i
							report+=" ("+str(al*(al-1))+")"
						end if 
					end if
				next l2
			next l1
		case 3 'perfect 3-symbol cycles
			cs=3
			lioc=(l-(cs-1))*(l-cs)
			blc=(cs*(cs-1))/2
		   for i=1 to s
		   	if frq(i)>fm then fm=frq(i)
		   next i
			dim as short map(l,fm+1)
			dim as short z(fm*cs)
			for i=1 to l
				map(nba(i),0)+=1
				map(nba(i),map(nba(i),0))=i
			next i
			for i=1 to s
				map(i,map(i,0)+1)=l+1
			next i
			for l1=1 to s
				if timer-statustimer>1 then
					statustimer=timer
					osi=nsc+": "+format((ci/ct)*100,"0.00")+"% complete"+lb
					osi+="(click stop task to cancel)"
					ui_editbox_settext(output_text,osi)
				end if
				if stoptask=1 then
					stoptask=0
					stats_running=0
					ui_editbox_settext(output_text,"")
					thread_ptr(threadsmax+2)=0
					exit sub
				end if
				for l2=l1+1 to s
					for l3=l2+1 to s
						ci+=1
						if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 then
							e=1
							al=0
							cl=0
							for i=1 to cs
								p(i)=1
							next i
							t(1)=l1
							t(2)=l2
							t(3)=l3
							do
								c(1)=map(l1,p(1))
								c(2)=map(l2,p(2))
								c(3)=map(l3,p(3))
								d=l+1
								for i=1 to cs
									if c(i)<d then
										d=c(i)
										g=i
									end if
								next i
								if d=l+1 then exit do
								p(g)+=1
								cl+=1
								z(cl)=t(g)
								if cl>(cs-1) then
									i=cl-(cs-1)
									for j=i to i+(cs-2)
										for k=j+1 to i+(cs-1)
											if z(j)=z(k) then
												e=0
												exit do
											end if
										next k
									next j
								end if
							loop		 
							if e=1 then
								al=cl-(cs-1)
								score+=al*(al-1)
								cycles+=1
								cyclelengths+=al
								report+=lb
								for i=1 to cl
									if num=0 then
										report+=chr(sym(z(i)))
									else
										report+=str(sym(z(i)))
										if i<>cl then report+=" "
									end if
								next i
								report+=" ("+str(al*(al-1))+")"
							end if 
						end if 
					next l3
				next l2
			next l1
		case 4 'perfect 4-symbol cycles
			cs=4
			lioc=(l-(cs-1))*(l-cs)
			blc=(cs*(cs-1))/2
		   for i=1 to s
		   	if frq(i)>fm then fm=frq(i)
		   next i
			dim as short map(l,fm+1)
			dim as short z(fm*cs)
			for i=1 to l
				map(nba(i),0)+=1
				map(nba(i),map(nba(i),0))=i
			next i
			for i=1 to s
				map(i,map(i,0)+1)=l+1
			next i
			for l1=1 to s
				if timer-statustimer>1 then
					statustimer=timer
					osi=nsc+": "+format((ci/ct)*100,"0.00")+"% complete"+lb
					osi+="(click stop task to cancel)"
					ui_editbox_settext(output_text,osi)
				end if
				if stoptask=1 then
					stoptask=0
					stats_running=0
					ui_editbox_settext(output_text,"")
					thread_ptr(threadsmax+2)=0
					exit sub
				end if
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							ci+=1
							if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 then
								e=1
								al=0
								cl=0
								for i=1 to cs
									p(i)=1
								next i
								t(1)=l1
								t(2)=l2
								t(3)=l3
								t(4)=l4
								do
									c(1)=map(l1,p(1))
									c(2)=map(l2,p(2))
									c(3)=map(l3,p(3))
									c(4)=map(l4,p(4))
									d=l+1
									for i=1 to cs
										if c(i)<d then
											d=c(i)
											g=i
										end if
									next i
									if d=l+1 then exit do
									p(g)+=1
									cl+=1
									z(cl)=t(g)
									if cl>(cs-1) then
										i=cl-(cs-1)
										for j=i to i+(cs-2)
											for k=j+1 to i+(cs-1)
												if z(j)=z(k) then
													e=0
													exit do
												end if
											next k
										next j
									end if
								loop		 
								if e=1 then
									al=cl-(cs-1)
									score+=al*(al-1)
									cycles+=1
									cyclelengths+=al
									report+=lb
									for i=1 to cl
										if num=0 then
											report+=chr(sym(z(i)))
										else
											report+=str(sym(z(i)))
											if i<>cl then report+=" "
										end if
									next i
									report+=" ("+str(al*(al-1))+")"
								end if 
							end if
						next l4
					next l3
				next l2
			next l1
		case 5 'perfect 5-symbol cycles
			cs=5
			lioc=(l-(cs-1))*(l-cs)
			blc=(cs*(cs-1))/2
		   for i=1 to s
		   	if frq(i)>fm then fm=frq(i)
		   next i
			dim as short map(l,fm+1)
			dim as short z(fm*cs)
			for i=1 to l
				map(nba(i),0)+=1
				map(nba(i),map(nba(i),0))=i
			next i
			for i=1 to s
				map(i,map(i,0)+1)=l+1
			next i
			for l1=1 to s
				for l2=l1+1 to s
					if timer-statustimer>1 then
						statustimer=timer
						osi=nsc+": "+format((ci/ct)*100,"0.00")+"% complete"+lb
						osi+="(click stop task to cancel)"
						ui_editbox_settext(output_text,osi)
					end if
					if stoptask=1 then
						stoptask=0
						stats_running=0
						ui_editbox_settext(output_text,"")
						thread_ptr(threadsmax+2)=0
						exit sub
					end if
					for l3=l2+1 to s
						for l4=l3+1 to s
							for l5=l4+1 to s
								ci+=1
								if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 then
									e=1
									al=0
									cl=0
									for i=1 to cs
										p(i)=1
									next i
									t(1)=l1
									t(2)=l2
									t(3)=l3
									t(4)=l4
									t(5)=l5
									do
										c(1)=map(l1,p(1))
										c(2)=map(l2,p(2))
										c(3)=map(l3,p(3))
										c(4)=map(l4,p(4))
										c(5)=map(l5,p(5))
										d=l+1
										for i=1 to cs
											if c(i)<d then
												d=c(i)
												g=i
											end if
										next i
										if d=l+1 then exit do
										p(g)+=1
										cl+=1
										z(cl)=t(g)
										if cl>(cs-1) then
											i=cl-(cs-1)
											for j=i to i+(cs-2)
												for k=j+1 to i+(cs-1)
													if z(j)=z(k) then
														e=0
														exit do
													end if
												next k
											next j
										end if
									loop		 
									if e=1 then
										al=cl-(cs-1)
										score+=al*(al-1)
										cycles+=1
										cyclelengths+=al
										report+=lb
										for i=1 to cl
											if num=0 then
												report+=chr(sym(z(i)))
											else
												report+=str(sym(z(i)))
												if i<>cl then report+=" "
											end if
										next i
										report+=" ("+str(al*(al-1))+")"
									end if 
								end if
							next l5
						next l4
					next l3
				next l2
			next l1
		case 6 'perfect 6-symbol cycles
			cs=6
			lioc=(l-(cs-1))*(l-cs)
			blc=(cs*(cs-1))/2
		   for i=1 to s
		   	if frq(i)>fm then fm=frq(i)
		   next i
			dim as short map(l,fm+1)
			dim as short z(fm*cs)
			for i=1 to l
				map(nba(i),0)+=1
				map(nba(i),map(nba(i),0))=i
			next i
			for i=1 to s
				map(i,map(i,0)+1)=l+1
			next i
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						if timer-statustimer>1 then
							statustimer=timer
							osi=nsc+": "+format((ci/ct)*100,"0.00")+"% complete"+lb
							osi+="(click stop task to cancel)"
							ui_editbox_settext(output_text,osi)
						end if
						if stoptask=1 then
							stoptask=0
							stats_running=0
							ui_editbox_settext(output_text,"")
							thread_ptr(threadsmax+2)=0
							exit sub
						end if
						for l4=l3+1 to s
							for l5=l4+1 to s
								for l6=l5+1 to s
									ci+=1
									if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 andalso frq(l6)>1 then
										e=1
										al=0
										cl=0
										for i=1 to cs
											p(i)=1
										next i
										t(1)=l1
										t(2)=l2
										t(3)=l3
										t(4)=l4
										t(5)=l5
										t(6)=l6
										do
											c(1)=map(l1,p(1))
											c(2)=map(l2,p(2))
											c(3)=map(l3,p(3))
											c(4)=map(l4,p(4))
											c(5)=map(l5,p(5))
											c(6)=map(l6,p(6))
											d=l+1
											for i=1 to cs
												if c(i)<d then
													d=c(i)
													g=i
												end if
											next i
											if d=l+1 then exit do
											p(g)+=1
											cl+=1
											z(cl)=t(g)
											if cl>(cs-1) then
												i=cl-(cs-1)
												for j=i to i+(cs-2)
													for k=j+1 to i+(cs-1)
														if z(j)=z(k) then
															e=0
															exit do
														end if
													next k
												next j
											end if
										loop		 
										if e=1 then
											al=cl-(cs-1)
											score+=al*(al-1)
											cycles+=1
											cyclelengths+=al
											report+=lb
											for i=1 to cl
												if num=0 then
													report+=chr(sym(z(i)))
												else
													report+=str(sym(z(i)))
													if i<>cl then report+=" "
												end if
											next i
											report+=" ("+str(al*(al-1))+")"
										end if 
									end if
								next l6
							next l5
						next l4
					next l3
				next l2
			next l1
		case 7 'perfect 7-symbol cycles
			cs=7
			lioc=(l-(cs-1))*(l-cs)
			blc=(cs*(cs-1))/2
		   for i=1 to s
		   	if frq(i)>fm then fm=frq(i)
		   next i
			dim as short map(l,fm+1)
			dim as short z(fm*cs)
			for i=1 to l
				map(nba(i),0)+=1
				map(nba(i),map(nba(i),0))=i
			next i
			for i=1 to s
				map(i,map(i,0)+1)=l+1
			next i
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							if timer-statustimer>1 then
								statustimer=timer
								osi=nsc+": "+format((ci/ct)*100,"0.00")+"% complete"+lb
								osi+="(click stop task to cancel)"
								ui_editbox_settext(output_text,osi)
							end if
							if stoptask=1 then
								stoptask=0
								stats_running=0
								ui_editbox_settext(output_text,"")
								thread_ptr(threadsmax+2)=0
								exit sub
							end if
							for l5=l4+1 to s
								for l6=l5+1 to s
									for l7=l6+1 to s
										ci+=1
										if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 andalso frq(l6)>1 andalso frq(l7)>1 then
											e=1
											al=0
											cl=0
											for i=1 to cs
												p(i)=1
											next i
											t(1)=l1
											t(2)=l2
											t(3)=l3
											t(4)=l4
											t(5)=l5
											t(6)=l6
											t(7)=l7
											do
												c(1)=map(l1,p(1))
												c(2)=map(l2,p(2))
												c(3)=map(l3,p(3))
												c(4)=map(l4,p(4))
												c(5)=map(l5,p(5))
												c(6)=map(l6,p(6))
												c(7)=map(l7,p(7))
												d=l+1
												for i=1 to cs
													if c(i)<d then
														d=c(i)
														g=i
													end if
												next i
												if d=l+1 then exit do
												p(g)+=1
												cl+=1
												z(cl)=t(g)
												if cl>(cs-1) then
													'bl=0
													i=cl-(cs-1)
													for j=i to i+(cs-2)
														for k=j+1 to i+(cs-1)
															if z(j)=z(k) then
																e=0
																exit do
															end if
														next k
													next j
												end if
											loop		 
											if e=1 then
												al=cl-(cs-1)
												score+=al*(al-1)
												cycles+=1
												cyclelengths+=al
												report+=lb
												for i=1 to cl
													if num=0 then
														report+=chr(sym(z(i)))
													else
														report+=str(sym(z(i)))
														if i<>cl then report+=" "
													end if
												next i
												report+=" ("+str(al*(al-1))+")"
											end if 
										end if
									next l7
								next l6
							next l5
						next l4
					next l3
				next l2
			next l1
		case 8 'perfect 8-symbol cycles
			cs=8
			lioc=(l-(cs-1))*(l-cs)
			blc=(cs*(cs-1))/2
		   for i=1 to s
		   	if frq(i)>fm then fm=frq(i)
		   next i
			dim as short map(l,fm+1)
			dim as short z(fm*cs)
			for i=1 to l
				map(nba(i),0)+=1
				map(nba(i),map(nba(i),0))=i
			next i
			for i=1 to s
				map(i,map(i,0)+1)=l+1
			next i
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							for l5=l4+1 to s
								if timer-statustimer>1 then
									statustimer=timer
									osi=nsc+": "+format((ci/ct)*100,"0.00")+"% complete"+lb
									osi+="(click stop task to cancel)"
									ui_editbox_settext(output_text,osi)
								end if
								if stoptask=1 then
									stoptask=0
									stats_running=0
									ui_editbox_settext(output_text,"")
									thread_ptr(threadsmax+2)=0
									exit sub
								end if
								for l6=l5+1 to s
									for l7=l6+1 to s
										for l8=l7+1 to s
											ci+=1
											if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 andalso frq(l6)>1 andalso frq(l7)>1 andalso frq(l8)>1 then
												e=1
												al=0
												cl=0
												for i=1 to cs
													p(i)=1
												next i
												t(1)=l1
												t(2)=l2
												t(3)=l3
												t(4)=l4
												t(5)=l5
												t(6)=l6
												t(7)=l7
												t(8)=l8
												do
													c(1)=map(l1,p(1))
													c(2)=map(l2,p(2))
													c(3)=map(l3,p(3))
													c(4)=map(l4,p(4))
													c(5)=map(l5,p(5))
													c(6)=map(l6,p(6))
													c(7)=map(l7,p(7))
													c(8)=map(l8,p(8))
													d=l+1
													for i=1 to cs
														if c(i)<d then
															d=c(i)
															g=i
														end if
													next i
													if d=l+1 then exit do
													p(g)+=1
													cl+=1
													z(cl)=t(g)
													if cl>(cs-1) then
														'bl=0
														i=cl-(cs-1)
														for j=i to i+(cs-2)
															for k=j+1 to i+(cs-1)
																if z(j)=z(k) then
																	e=0
																	exit do
																end if
															next k
														next j
													end if
												loop		 
												if e=1 then
													al=cl-(cs-1)
													score+=al*(al-1)
													cycles+=1
													cyclelengths+=al
													report+=lb
													for i=1 to cl
														if num=0 then
															report+=chr(sym(z(i)))
														else
															report+=str(sym(z(i)))
															if i<>cl then report+=" "
														end if
													next i
													report+=" ("+str(al*(al-1))+")"
												end if 
											end if
										next l8
									next l7
								next l6
							next l5
						next l4
					next l3
				next l2
			next l1
	end select
	if score>0 then
		os+="Index of coincidence:"+lb
		os+="- Raw: "+str(score)+lb
		os+="- Normalized: "+format(score/lioc,"0.00000")+lb
		os+="- Flatness: "+format((((cyclelengths/cycles)*((cyclelengths/cycles)-1))*cycles)/score,"0.00000")+lb
		os+="Cycles: "+str(cycles)+lb
		os+="Average cycle length: "+str(cyclelengths/cycles)+lb
		os+=lb
		os+="Cycles by appearance:"+lb
		os+="---------------------------------------------------------"
	else
		os+="No cycles found"
	end if	
	os+=report
	ui_editbox_settext(output_text,os)
	stats_running=0
	thread_ptr(threadsmax+2)=0
	randomize timer

end sub

sub stats_encodingrandomization(byval tn_ptr as any ptr)
	
	stats_running=1
	randomize 12345
	dim as string os,ost,osu,osm,osl,osmss,fos
	dim as string lfn=file_name
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer h,i,j,k,c,e,o,t,r1,r2,l2,kk,x,y
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long cip(l)
	dim as integer sym(s)
	dim as long nba0(l)
	dim as long nba1(l)
	dim as short used(l)
	dim as short frq1(s)
	dim as double clm(100,l)
	dim as double clr1(l)
	dim as double clr2(l)
	dim as integer map(s,l)
	dim as integer num=info_numerical
	dim as double low,avgb,avgt
	dim as double avg0,avg1
	dim as short encm=encodingnulls_m
	dim as short enct=encodingnulls_t
	dim as short encu=encodingnulls_u
	dim as integer total
	dim as short shape(dx,dy)
	dim as short sx,sy,sxmax,symax
	dim as short customshape=1
	dim as short cc,oldngsize=ngram_size
	
	if lfn="" then lfn="unknown"
	
	for i=1 to l
		frq1(nuba(i))+=1
		cip(i)=info(i)
		nba0(i)=nuba(i)
		map(nba0(i),0)+=1
		map(nba0(i),map(nba0(i),0))=i
		sym(nba0(i))=cip(i)
	next i
	
	if encu=5 then
		ngram_size=1
		soi=string_to_info(ui_editbox_gettext(output_text))
		if soi<>"Ok" then
			customshape=0
		end if
		ngram_size=oldngsize
		if info_numerical=1 then
			stats_running=0
			ui_editbox_settext(output_text,"Error: custom shape cannot be numerical")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if
		sxmax=info_x
		symax=info_y
		if sxmax>dx or symax>dy then
			stats_running=0
			ui_editbox_settext(output_text,"Error: custom shape dimensions > input")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if	
		i=0
		for y=1 to symax
			for x=1 to sxmax
				i+=1
				if i>l then exit for,for
				if info(i)<>32 then shape(x,y)=1
			next x
		next y
	end if
	info_numerical=num
	
	os+="BHdecrypt encoding randomization stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	os+="- Attempts to detect encoding randomization from the "+lb
	os+="input given that it has sequential properties."+lb
	os+="- A higher percentage (improvement rate) may be "+lb
	os+="more indicative of randomization."
	
	dim as byte use_perfectcycles=1
	
	dim as double base_score(3)
	base_score(1)=m_sequential(nba0(),l,s,1,0)
	if use_perfectcycles=1 then
		base_score(2)=m_2cycles_perfect(nba0(),l,s)
		base_score(3)=m_3cycles_perfect(nba0(),l,s)
	else
		base_score(2)=m_2cycles(nba0(),l,s,stats_nsymbolcyclesweight)
		base_score(3)=m_3cycles(nba0(),l,s,stats_nsymbolcyclesweight)
	end if
	
	dim as integer rits(7)
	rits(1)=stats_encrndtrials
	rits(2)=stats_encrndtrials
	rits(3)=stats_encrndtrials
	rits(4)=stats_encrndtrials
	rits(5)=stats_encrndtrials
	rits(6)=stats_encrndtrials
	rits(7)=stats_encrndtrials
	
	dim as double progresstimer=timer
	dim as double list(l*l,2)
	
	select case enct
		case 1:ost="randomize characters"
		case 2:ost="randomize positions"
		case 3:ost="remove characters"
	end select
	
	select case encm
		case 1
			osm="sequential"
			osmss="seq"
		case 2
			osm="2-symbol cycles"
			osmss="2sc"+str(stats_nsymbolcyclesweight)
		case 3
			osm="3-symbol cycles"
			osmss="3sc"+str(stats_nsymbolcyclesweight)
	end select
	
	select case encu
	
		case 1 'test symbols
			
			total=s
			avg1=0
			low=999999999
			for i=1 to s
				c+=1
				if timer-progresstimer>1 then
					progresstimer=timer
					osl="Encoding randomization: "+osm
					osl+=": "+format(((c-1)/total)*100,"0.00")+"% complete"+lb
					osl+="Testing symbols... (click stop task to cancel)"
					ui_editbox_settext(output_text,osl)
				end if
				for j=1 to l
					nba1(j)=nba0(j)
				next j
				avgb=0
				for j=1 to rits(encm)
					for k=1 to map(i,0)
						nba1(map(i,k))=nba0(int(rnd*l)+1)
					next k
					select case encm
						case 1:avgt=m_sequential(nba1(),l,s,1,0)
						case 2
							if use_perfectcycles=1 then
								avgt=m_2cycles_perfect(nba1(),l,s)
							else
								avgt=m_2cycles(nba1(),l,s,stats_nsymbolcyclesweight)
							end if
						case 3
							if use_perfectcycles=1 then
								avgt=m_3cycles_perfect(nba1(),l,s)
							else
								avgt=m_3cycles(nba1(),l,s,stats_nsymbolcyclesweight)
							end if
					end select
					if avgt>base_score(encm) then avgb+=1
				next j
				list(i,0)=i
				list(i,1)=(avgb/rits(encm))*100	 
				avg1+=list(i,1)
				if list(i,1)<low then low=list(i,1)
				avg0=0
				if stoptask=1 then
					stoptask=0
					stats_running=0
					ui_editbox_settext(output_text,"")
					thread_ptr(threadsmax+2)=0
					exit sub
				end if
			next i	
			do 'sort
				e=0
				for i=1 to s-1
					if list(i,1)<list(i+1,1) then
						e=1
						swap list(i,0),list(i+1,0)
						swap list(i,1),list(i+1,1)
					end if
				next i
			loop until e=0
			os+=lb
			os+=lb
			os+="Symbols, "+ost+", using "+osm+":"+lb
			os+="---------------------------------------------------------" '+lb
			for i=1 to s
				os+=lb
				if num=0 then
					os+=chr(sym(list(i,0)))+": "+format(list(i,1),"0.00")+"% ("+str(map(list(i,0),0))+")"
				else
					os+=str(sym(list(i,0)))+": "+format(list(i,1),"0.00")+"% ("+str(map(list(i,0),0))+")"
				end if
			next i
			os+=lb
			os+="--------------------"+lb
			os+="Average: "+format(avg1/s,"0.00")+"%"
		
		case 2 'test rows
			
			total=dy
			avg1=0
			low=999999999
			for i=1 to dy
				c+=1
				if timer-progresstimer>1 then
					progresstimer=timer
					osl="Encoding randomization: "+osm
					osl+=": "+format(((c-1)/total)*100,"0.00")+"% complete"+lb
					osl+="Testing rows... (click stop task to cancel)"
					ui_editbox_settext(output_text,osl)
				end if
				for j=1 to l
					nba1(j)=nba0(j)
				next j
				avgb=0
				for j=1 to rits(encm)
					cc=0
					for k=1 to dx 'step 2
						kk=k+(dx*(i-1))
						if kk>l then exit for
						cc+=1
						nba1(kk)=nba0(int(rnd*l)+1)
					next k
					select case encm
						case 1:avgt=m_sequential(nba1(),l,s,1,0)
						case 2
							if use_perfectcycles=1 then
								avgt=m_2cycles_perfect(nba1(),l,s)
							else
								avgt=m_2cycles(nba1(),l,s,stats_nsymbolcyclesweight)
							end if
						case 3
							if use_perfectcycles=1 then
								avgt=m_3cycles_perfect(nba1(),l,s)
							else
								avgt=m_3cycles(nba1(),l,s,stats_nsymbolcyclesweight)
							end if
					end select
					if avgt>base_score(encm) then avgb+=1
				next j
				avgb*=(cc/dx)
				list(i,0)=i
				list(i,1)=(avgb/rits(encm))*100	 
				avg1+=list(i,1)
				if list(i,1)<low then low=list(i,1)
				avg0=0
				if stoptask=1 then
					stoptask=0
					stats_running=0
					ui_editbox_settext(output_text,"")
					thread_ptr(threadsmax+2)=0
					exit sub
				end if
			next i
			k=0
			os+=lb
			os+=lb
			os+="Rows, "+ost+", using "+osm+":"+lb
			os+="---------------------------------------------------------" '+lb
			for i=1 to dy
				os+=lb
				os+="Row "+str(list(i,0))+": "+format(list(i,1),"0.00")+"%"
			next i
			os+=lb
			os+="--------------------"+lb
			os+="Average: "+format(avg1/dy,"0.00")+"%"
		
		case 3 'test columns
	
			total=dx
			avg1=0
			for i=1 to dx
				c+=1
				if timer-progresstimer>1 then
					progresstimer=timer
					osl="Encoding randomization: "+osm
					osl+=": "+format(((c-1)/total)*100,"0.00")+"% complete"+lb
					osl+="Testing columns... (click stop task to cancel)"
					ui_editbox_settext(output_text,osl)
				end if
				for j=1 to l
					nba1(j)=nba0(j)
				next j
				avgb=0
				for j=1 to rits(encm)
					cc=0
					for k=1 to dy
						kk=i+(dx*(k-1))
						if kk>l then exit for
						cc+=1
						nba1(kk)=nba0(int(rnd*l)+1)
					next k
					select case encm
						case 1:avgt=m_sequential(nba1(),l,s,1,0)
					case 2
							if use_perfectcycles=1 then
								avgt=m_2cycles_perfect(nba1(),l,s)
							else
								avgt=m_2cycles(nba1(),l,s,stats_nsymbolcyclesweight)
							end if
						case 3
							if use_perfectcycles=1 then
								avgt=m_3cycles_perfect(nba1(),l,s)
							else
								avgt=m_3cycles(nba1(),l,s,stats_nsymbolcyclesweight)
							end if
					end select
					if avgt>base_score(encm) then avgb+=1
				next j
				avgb*=(cc/dy)
				list(i,0)=i
				list(i,1)=(avgb/rits(encm))*100	 
				avg1+=list(i,1)
				if list(i,1)<low then low=list(i,1)
				avg0=0
				if stoptask=1 then
					stoptask=0
					stats_running=0
					ui_editbox_settext(output_text,"")
					thread_ptr(threadsmax+2)=0
					exit sub
				end if
			next i
			os+=lb
			os+=lb
			os+="Columns, "+ost+", using "+osm+":"+lb
			os+="---------------------------------------------------------" '+lb
			for i=1 to dx
				os+=lb
				os+="Column "+str(list(i,0))+": "+format(list(i,1),"0.00")+"%"
			next i
			os+=lb
			os+="--------------------"+lb
			os+="Average: "+format(avg1/dx,"0.00")+"%"
		
		case 4 'test periodic
			
			for i=1 to l/10
				for j=1 to i
					k+=1
				next j
			next i
			total=k
			dim as double tmax
			for i=1 to l/10
				for o=0 to i-1
					t+=1
					c+=1
					if timer-progresstimer>1 then
						progresstimer=timer
						osl="Encoding randomization: "+osm
						osl+=": "+format(((c-1)/total)*100,"0.00")+"% complete"+lb
						osl+="Testing periodic... (click stop task to cancel)"
						ui_editbox_settext(output_text,osl)
					end if
					for j=1 to l
						nba1(j)=nba0(j)
					next j
					avgb=0
					dim as integer li=int(l/i)
					for j=1 to rits(encm) 
						cc=0
						for k=1+o to l step i
							cc+=1
							nba1(k)=nba0(int(rnd*l)+1)
						next k
						select case encm
							case 1:avgt=m_sequential(nba1(),l,s,1,0)
							case 2
								if use_perfectcycles=1 then
									avgt=m_2cycles_perfect(nba1(),l,s)
								else
									avgt=m_2cycles(nba1(),l,s,stats_nsymbolcyclesweight)
								end if
							case 3
								if use_perfectcycles=1 then
									avgt=m_3cycles_perfect(nba1(),l,s)
								else
									avgt=m_3cycles(nba1(),l,s,stats_nsymbolcyclesweight)
								end if
						end select
						if avgt>base_score(encm) then avgb+=1
					next j
					avgb*=(cc/li)
					list(t,0)=i
					list(t,1)=(avgb/rits(encm))*100
					avgb=0
					for j=1 to l
						nba1(j)=nba0(j)
					next j
					for j=1 to rits(encm)
						cc=0
						for k=1+(li*o) to (li*(o+1))
							if k>l then exit for
							cc+=1
							nba1(k)=nba0(int(rnd*l)+1)
						next k
						select case encm
							case 1:avgt=m_sequential(nba1(),l,s,1,0)
							case 2
								if use_perfectcycles=1 then
									avgt=m_2cycles_perfect(nba1(),l,s)
								else
									avgt=m_2cycles(nba1(),l,s,stats_nsymbolcyclesweight)
								end if
							case 3
								if use_perfectcycles=1 then
									avgt=m_3cycles_perfect(nba1(),l,s)
								else
									avgt=m_3cycles(nba1(),l,s,stats_nsymbolcyclesweight)
								end if
						end select
						if avgt>base_score(encm) then avgb+=1
					next j
					avgb*=(cc/li)
					list(t,0)=i
					list(t,2)=(avgb/rits(encm))*100
					for k=1+o to l step i
						if k>l then exit for
						if list(t,1)>clm(1,k) then clm(1,k)=list(t,1)
						clm(2,k)+=list(t,1)/(1+(i/2))
					next k
					for k=1+(li*o) to (li*(o+1))
						if k>l then exit for
						if list(t,2)>clm(1,k) then clm(1,k)=list(t,2)
						clm(3,k)+=list(t,2)/(1+(i/2))
					next k	 
					if stoptask=1 then
						stoptask=0
						stats_running=0
						ui_editbox_settext(output_text,"")
						thread_ptr(threadsmax+2)=0
						exit sub
					end if
				next o
			next i
			for j=1 to l
				clr1(j)=clm(1,j)
				if clm(2,j)>clm(3,j) then
					clr2(j)=clm(2,j)
				else
					clr2(j)=clm(3,j)
				end if
			next j
			os+=lb
			os+=lb
			clr1(0)=0
			fos=left(lfn,len(lfn)-4)+"_periodic_encodingrandomization_"+osmss+"_map1"
			output_colormap(clr1(),cip(),l,dx,dy,100,fos)
			os+="Created Output/"+fos+".bmp"+lb
			clr2(0)=99999999999
			fos=left(lfn,len(lfn)-4)+"_periodic_encodingrandomization_"+osmss+"_map2"
			output_colormap(clr2(),cip(),l,dx,dy,0,fos)
			os+="Created Output/"+fos+".bmp"+lb
			o=0
			os+=lb
			os+="Periodic, "+ost+", using "+osm+":"+lb
			os+="---------------------------------------------------------"
			os+=lb
			for i=1 to t
				if list(i,0)<>list(i-1,0) then
					o=0
					os+="Period "+str(list(i,0))+":"+lb
				end if
				os+="- Row/column "+str(o+1)+": "+format(list(i,2),"0.00")+"%"
				os+=", "+format(list(i,1),"0.00")+"%"
				o+=1
				if i<>t then os+=lb
			next i
		
		case 5 'custom shape
			
			total=l
			dim as short xx,yy,gx,gy
			dim as short cx
			dim as short cy
			os+=lb
			os+=lb
			if customshape=1 then
				os+="Custom shape "+str(sxmax)+" by "+str(symax)+":"+lb
				os+="-------------------------"+lb
				cx=sxmax
				cy=symax
				for y=1 to cy
					for x=1 to cx
						if shape(x,y)=1 then
							os+="*"
						else
							os+=" "
						end if
					next x
					os+=lb
				next y
			else
				os+="Default shape 2 by 2:"+lb
				os+="-------------------------"+lb
				cx=2
				cy=2
				for y=1 to cy
					for x=1 to cx
						shape(x,y)=1
						if shape(x,y)=1 then
							os+="*"
						else
							os+=" "
						end if
					next x
					os+=lb
				next y
			end if
			os+=lb
			avg1=0
			low=999999999	
			dim as short grid0(dx,dy)
			dim as short grid1(dx,dy)
			dim as short gridn(dx,dy)
			dim as short nbaxy(l,1)
			i=0
			for y=1 to dy
				for x=1 to dx
					i+=1
					if i>l then exit for,for
					grid0(x,y)=nba0(i)
					gridn(x,y)=i
					nbaxy(i,0)=x
					nbaxy(i,1)=y
				next x
			next y
			for yy=1 to dy 
				for xx=1 to dx
					if gridn(xx,yy)>0 then
						c+=1
						t+=1
						if timer-progresstimer>1 then
							progresstimer=timer
							osl="Encoding randomization: "+osm
							osl+=": "+format(((c-1)/total)*100,"0.00")+"% complete"+lb
							osl+="Testing custom shape... (click stop task to cancel)"
							ui_editbox_settext(output_text,osl)
						end if
						if stoptask=1 then
							stoptask=0
							stats_running=0
							ui_editbox_settext(output_text,"")
							thread_ptr(threadsmax+2)=0
							exit sub
						end if
						for y=1 to dy
							for x=1 to dx
								grid1(x,y)=grid0(x,y)
							next x
						next y
						h=0
						avgb=0 
						for j=1 to rits(encm) 
							for y=0 to (cy-1)
								for x=0 to (cx-1)
									gx=xx+x
									gy=yy+y
									if gx>dx then gx=gx-dx
									if gy>dy then gy=gy-dy
									if gridn(gx,gy)>0 andalso shape(x+1,y+1)=1 then grid1(gx,gy)=nba0(int(rnd*l)+1)
								next x
							next y
							k=0
							for y=1 to dy
								for x=1 to dx
									k+=1
									if k>l then exit for,for
									nba1(k)=grid1(x,y)
								next x
							next y
							select case encm
								case 1:avgt=m_sequential(nba1(),l,s,1,0)
								case 2
									if use_perfectcycles=1 then
										avgt=m_2cycles_perfect(nba1(),l,s)
									else
										avgt=m_2cycles(nba1(),l,s,stats_nsymbolcyclesweight)
									end if
								case 3
									if use_perfectcycles=1 then
										avgt=m_3cycles_perfect(nba1(),l,s)
									else
										avgt=m_3cycles(nba1(),l,s,stats_nsymbolcyclesweight)
									end if
							end select
							if avgt>base_score(encm) then avgb+=1
						next j
						list(t,0)=i
						list(t,1)=(avgb/rits(encm))*100
						for y=0 to (cy-1)
							for x=0 to (cx-1)
								gx=xx+x
								gy=yy+y
								if gx>dx then gx=gx-dx
								if gy>dy then gy=gy-dy
								if gridn(gx,gy)>0 andalso shape(x+1,y+1)=1 then 
									if list(t,1)>clr1(gridn(gx,gy)) then
										if list(t,1)>clr1(gridn(gx,gy)) then clr1(gridn(gx,gy))=list(t,1)
										clr2(gridn(gx,gy))+=list(t,1)
									end if
								end if
							next x
						next y
						avg1+=list(t,1)
					end if
				next xx
			next yy	
			clr1(0)=0
			fos=left(lfn,len(lfn)-4)+"_encodingrandomization_"+osmss+"_map1"
			output_colormap(clr1(),cip(),l,dx,dy,100,left(lfn,len(lfn)-4)+"_customshape_encodingrandomization_"+osmss+"_map1")
			os+="Created Output/"+fos+".bmp"+lb
			clr2(0)=999999999999
			fos=left(lfn,len(lfn)-4)+"_encodingrandomization_"+osmss+"_map2"
			output_colormap(clr2(),cip(),l,dx,dy,0,left(lfn,len(lfn)-4)+"_customshape_encodingrandomization_"+osmss+"_map2")
			os+="Created Output/"+fos+".bmp"+lb
			os+=lb
			o=0
			os+="Custom shape, "+ost+", using "+osm+":"+lb
			os+="---------------------------------------------------------"+lb
			for i=1 to t
				os+="- Row "+str(nbaxy(i,0))+", column "+str(nbaxy(i,1))+": "+format(list(i,1),"0.00")+"%"
				if i<>t then os+=lb
			next i
			os+=lb
			os+="--------------------"+lb
			os+="Average: "+format(avg1/total,"0.00")+"%"
		
		case 6 'grid		
			
			dim as short xx,yy,gx,gy,dz
			dim as short grid0(dx,dy)
			dim as short grid1(dx,dy)
			dim as short gridn(dx,dy)
			dim as short nbaxy(l,1)
			dim as short shp(constcip,dx,dy)
			dim as short cx(constcip)
			dim as short cy(constcip)
			dim as double score=0
			dim as short shp_info(10000) '2000?
			dim as double score1,score2	
			i=0
			for y=1 to dy
				for x=1 to dx
					i+=1
					if i>l then exit for,for
					grid0(x,y)=nba0(i)
					gridn(x,y)=i
					nbaxy(i,0)=x
					nbaxy(i,1)=y
				next x
			next y
			total=0
			if dx<dy then dz=dx else dz=dy
			for i=2 to dz
				total+=1
				shp_info(total)=1
				cx(total)=i
				cy(total)=1
				shp(total,0,0)=i
				shp(total,0,1)=dz-(i-1)
				shp(total,1,0)=dz
				for j=1 to i
					shp(total,j,1)=1
				next j
			next i
			for i=2 to dz
				total+=1
				shp_info(total)=2
				cx(total)=1
				cy(total)=i
				shp(total,0,0)=i
				shp(total,0,1)=dz-(i-1)
				shp(total,1,0)=dz
				for j=1 to i
					shp(total,1,j)=1
				next j
			next i
			'
			'for i=1 to dx 'square
			'	total+=1
			'	shp_info(total)=3
			'	cx(total)=i
			'	cy(total)=i
			'	shp(total,0,0)=i 'i*i
			'	shp(total,0,1)=dx-(i-1) '(dx*dx)-(((i-1)*(i-1)))
			'	shp(total,1,0)=dx 'dx*dx
			'	for y=1 to i
			'		for x=1 to i
			'			shp(total,x,y)=1
			'		next x
			'	next y
			'next i
			'
			'for i=2 to dx 'diag 1
			'	total+=1
			'	shp_info(total)=3
			'	cx(total)=i
			'	cy(total)=i
			'	shp(total,0,0)=i
			'	shp(total,0,1)=dx-(i-1)
			'	shp(total,1,0)=dx
			'	x=0:y=0
			'	for y=1 to i
			'		x+=1
			'		shp(total,x,y)=1
			'	next y
			'next i
			'
			'for i=2 to dx 'diag 2
			'	total+=1
			'	shp_info(total)=4
			'	cx(total)=i
			'	cy(total)=i
			'	shp(total,0,0)=i
			'	shp(total,0,1)=dx-(i-1)
			'	shp(total,1,0)=dx
			'	x=i:y=0
			'	for y=1 to i
			'		shp(total,x,y)=1
			'		x-=1
			'	next y
			'next i
			'
			for i=1 to total
				for yy=1 to dy 
					for xx=1 to dx
						if gridn(xx,yy)>0 then
							c+=1
							if timer-progresstimer>1 then
								progresstimer=timer
								osl="Encoding randomization: "+osm
								osl+=": "+format(((c-1)/(total*l))*100,"0.00")+"% complete"+lb
								osl+="Testing grid... (click stop task to cancel)"
								ui_editbox_settext(output_text,osl)
							end if
							if stoptask=1 then
								stoptask=0
								stats_running=0
								ui_editbox_settext(output_text,"")
								thread_ptr(threadsmax+2)=0
								exit sub
							end if
							k=0
							for y=1 to dy
								for x=1 to dx
									k+=1
									if k>l then exit for,for
									grid1(x,y)=grid0(x,y)
								next x
							next y
							h=0
							avgb=0
							score1=0
							score2=0
							for j=1 to rits(encm) 
								for y=0 to (cy(i)-1)
									for x=0 to (cx(i)-1)
										gx=xx+x
										gy=yy+y
										if gx>dx then gx=gx-dx
										if gy>dy then gy=gy-dy
										if gridn(gx,gy)>0 andalso shp(i,x+1,y+1)=1 then grid1(gx,gy)=nba0(int(rnd*l)+1)
									next x
								next y
								k=0
								for y=1 to dy
									for x=1 to dx
										k+=1
										if k>l then exit for,for
										nba1(k)=grid1(x,y)
									next x
								next y
								select case encm
									case 1:avgt=m_sequential(nba1(),l,s,1,0)
									case 2
										if use_perfectcycles=1 then
											avgt=m_2cycles_perfect(nba1(),l,s)
										else
											avgt=m_2cycles(nba1(),l,s,stats_nsymbolcyclesweight)
										end if
									case 3
										if use_perfectcycles=1 then
											avgt=m_3cycles_perfect(nba1(),l,s)
										else
											avgt=m_3cycles(nba1(),l,s,stats_nsymbolcyclesweight)
										end if
								end select
								if avgt>base_score(encm) then score2+=1	
							next j
							score2=(score2/rits(encm))*100
							for y=0 to (cy(i)-1)
								for x=0 to (cx(i)-1)
									gx=xx+x
									gy=yy+y
									if gx>dx then gx=gx-dx
									if gy>dy then gy=gy-dy
									if gridn(gx,gy)>0 andalso shp(i,x+1,y+1)=1 then	
										if (score2*((shp(i,0,0)/shp(i,1,0))^(10/10)))>clm(1,gridn(gx,gy)) then 
											clm(1,gridn(gx,gy))=(score2*((shp(i,0,0)/shp(i,1,0))^(10/10)))
										end if
										if (score2*((shp(i,0,0)/shp(i,1,0))^(5/10)))>clm(2,gridn(gx,gy)) then 
											clm(2,gridn(gx,gy))=(score2*((shp(i,0,0)/shp(i,1,0))^(5/10)))
										end if
										if (score2*((shp(i,0,0)/shp(i,1,0))^(3/10)))>clm(3,gridn(gx,gy)) then 
											clm(3,gridn(gx,gy))=(score2*((shp(i,0,0)/shp(i,1,0))^(3/10)))
										end if
										if shp_info(i)=1 then
											clm(4,gridn(gx,gy))+=((score2/((shp(i,0,0)/shp(i,1,0))))*(dy/dx))
										end if
										if shp_info(i)=2 then
											clm(5,gridn(gx,gy))+=(score2/((shp(i,0,0)/shp(i,1,0))))
										end if
									end if
								next x
							next y
						end if
					next xx
				next yy
			next i
			dim as double ch
			for i=4 to 5
				for j=1 to l
					ch=0
					for k=0 to 1
						if clm(i+(k*1),j)>ch then 
							ch=clm(i+(k*1),j)
							clm(i,j)=ch
						end if
					next k
				next j
			next i	
			os+=lb	
			for i=1 to 3
				for j=1 to l
					clr1(j)=clm(i,j)
				next j
				clr1(0)=0
				fos=left(lfn,len(lfn)-4)+"_grid_encodingrandomization_"+osmss+"_map"+str(i)
				output_colormap(clr1(),cip(),l,dx,dy,100,fos)
				os+=lb
				os+="Created Output/"+fos+".bmp"
			next i	
			for i=4 to 4
				for j=1 to l
					clr1(j)=clm(i,j)
				next j
				clr1(0)=999999999999999
				fos=left(lfn,len(lfn)-4)+"_grid_encodingrandomization_"+osmss+"_map"+str(i)
				output_colormap(clr1(),cip(),l,dx,dy,0,fos)
				os+=lb
				os+="Created Output/"+fos+".bmp"
			next i
		
		case 7 'horizontal slide
		
			dim as short fs
			dim as short fo
			dim as integer total
			dim as double score1
			for i=2 to l/10
				for j=1 to l-(fs-1)
					total+=1
				next j		
			next i
			for fs=2 to l/10
				for fo=1 to l-(fs-1)
					if timer-progresstimer>1 then
						progresstimer=timer
						osl="Encoding randomization: "+osm
						osl+=": "+format((c/total)*100,"0.00")+"% complete"+lb
						osl+="Testing slide... (click stop task to cancel)"
						ui_editbox_settext(output_text,osl)
					end if
					if stoptask=1 then
						stoptask=0
						stats_running=0
						ui_editbox_settext(output_text,"")
						thread_ptr(threadsmax+2)=0
						exit sub
					end if
					c+=1
					for i=1 to l
						nba1(i)=nba0(i)
					next i
					avgb=0
					for i=1 to rits(encm) 
						'cc=0
						for j=fo to fo+(fs-1)
							'cc+=1
							nba1(j)=nba0(int(rnd*l)+1)
						next j
						select case encm
							case 1:avgt=m_sequential(nba1(),l,s,1,0)
							case 2
								if use_perfectcycles=1 then
									avgt=m_2cycles_perfect(nba1(),l,s)
								else
									avgt=m_2cycles(nba1(),l,s,stats_nsymbolcyclesweight)
								end if
							case 3
								if use_perfectcycles=1 then
									avgt=m_3cycles_perfect(nba1(),l,s)
								else
									avgt=m_3cycles(nba1(),l,s,stats_nsymbolcyclesweight)
								end if
						end select
						if avgt>base_score(encm) then avgb+=1
					next i
					'avgb*=(cc/fs)
					'list(c,0)=i
					score1=(avgb/rits(encm))*100
					for i=fo to fo+(fs-1)
						if i>l then exit for
						if (score1*((fs/(l/10)))^(5/10))>clm(1,i) then clm(1,i)=(score1*((fs/(l/10)))^(5/10))
						if (score1*((fs/(l/10)))^(2.5/10))>clm(2,i) then clm(2,i)=(score1*((fs/(l/10)))^(2.5/10))
						if (score1*((fs/(l/10)))^(1.5/10))>clm(3,i) then clm(3,i)=(score1*((fs/(l/10)))^(1.5/10))
						clm(4,i)+=score1
						clm(5,i)+=1
					next i	
				next fo
			next fs
			os+=lb
			os+=lb
			clr1(0)=0
			for i=1 to 3
				for j=1 to l
					clr1(j)=clm(i,j)
				next j
				fos=left(lfn,len(lfn)-4)+"_slide_encodingrandomization_"+osmss+"_map"+str(i)
				output_colormap(clr1(),cip(),l,dx,dy,100,fos)
				os+="Created Output/"+fos+".bmp"+lb
			next i
			for i=1 to l
				clr1(i)=clm(4,i)/clm(5,i)
			next i
			clr1(0)=99999999999
			fos=left(lfn,len(lfn)-4)+"_slide_encodingrandomization_"+osmss+"_map4"
			output_colormap(clr1(),cip(),l,dx,dy,0,fos)
			os+="Created Output/"+fos+".bmp"+lb
		
	end select
	
	stats_running=0
	ui_editbox_settext(output_text,os)
	thread_ptr(threadsmax+2)=0
	randomize timer

end sub

sub generate_nullsandskips
	
	dim as string cips=string_to_info(ui_editbox_gettext(input_text))
	dim as string o
	dim as integer h,i,j,k
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long cip(constcip)
	dim as long cip2(constcip)
	
	for i=1 to l
		cip(i)=info(i)
	next i
	
	for i=1 to l 'nulls
		k=0
		for j=1 to l
			if j<>i then 
				k+=1
				cip2(k)=cip(j)
			end if
		next j
		o+="cipher_information=null_"+str(i)+lb
		o+=info_to_string(cip2(),l-1,dx,dy,0,0,0)+lb+lb
	next i
	
	for i=1 to l+1 'skips
		k=0
		for j=1 to l+1
			if j=i then
				k+=1
				cip2(k)=63 '"?"
			end if
			if j<>l+1 then 
				k+=1
				cip2(k)=cip(j)
			end if	
		next j
		o+="cipher_information=skip_"+str(i)+lb
		o+=info_to_string(cip2(),l+1,dx,dy+1,0,0,0)+lb+lb
	next i
	
	ui_editbox_settext(output_text,o)
	
end sub

sub generate_substrings
	
	dim as string cips=string_to_info(ui_editbox_gettext(input_text))
	
	dim as string o
	dim as integer h,i,j,k,ns,nl
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	
	dim as long cip(l)
	dim as long sol(l)
	dim as long cip2(l)
	dim as long sol2(l)
	'dim as string cips=string_to_info(ui_editbox_gettext(input_text))
	for i=1 to l
		cip(i)=info(i)
	next i
	dim as string sols=string_to_info(ui_editbox_gettext(output_text))
	for i=1 to l
		sol(i)=info(i)
	next i
	
	dim as short id(constcip)
	dim as double lowb=0.3
	dim as double upb=0.35
	
	for h=1 to l
		for i=h to l
			
			nl=0
			ns=0
			erase id
			
			for j=h to i
				nl+=1
				if id(cip(j))=0 then ns+=1
				id(cip(j))=1
				cip2(nl)=cip(j)
				'sol2(nl)=sol(j)
			next j
			
			if (ns/nl)>=lowb andalso (ns/nl)<=upb then 'output
				o+="cipher_information="+str(h)+"-"+str(i)+"_"+str(int((ns/nl)*1000))+lb
				o+=info_to_string(cip2(),nl,dx,dy,0,0,0)+lb
				'o+="solution_plaintext="+lb
				'o+=info_to_string(sol2(),nl,dx,dy,0)+lb
				o+=lb
			end if
			
		next i
	next h
	
	ui_editbox_settext(output_text,o)
	
end sub

sub stats_encoding
	
	randomize 12345
	dim as string os
	dim as integer h,i,j,k,c,t,r
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long cip(l)
	dim as long nba(l)
	dim as long nba2(l)
	dim as integer nts(l)
	dim as short cma(100)
	dim as integer num=info_numerical
	for i=1 to l
		cip(i)=info(i)
		nba(i)=nuba(i)
		nba2(i)=nuba(i)
		nts(nba(i))=cip(i)
	next i
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	
	os+="BHdecrypt encoding stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	os+="Midpoint shift:"+lb
	os+="- Raw: "+str(int(m_midpointshift(nba(),l,s,0)))+lb
	os+="- Normalized: "+str(m_midpointshift(nba(),l,s,1))+lb
	'os+="Prime phobia:"+lb
	'os+="- Raw: "+str(m_primephobia(nba(),l,s,0))+lb
	'os+="- Normalized: "+str(m_primephobia(nba(),l,s,1))+lb
	os+="Slope:"+lb
	os+="- Raw: "+str(m_slope(cip(),l,s,0))+lb
	os+="- Normalized: "+str(m_slope(cip(),l,s,1))+lb
	os+="Distance patterns:"+lb
	os+="- Raw: "+str(m_isdp(nba(),l,s,0))+lb
	os+="- Normalized: "+str(m_isdp(nba(),l,s,1))+lb
	os+="Sequential:"+lb
	os+="- Raw: "
	for i=1 to 5
		os+=str(m_sequential(nba(),l,s,i,0))
		if i<>5 then os+=", "
	next i
	os+=lb
	erase freq
	os+="- Normalized: "+str(m_sequential(nba(),l,s,1,1))+lb
	
	dim as integer rits=5000,max1
	dim as short frq0(5,l)
	dim as short frq1(5,rits,l)
	dim as double a,sda(rits)
	
	for i=1 to 5
		erase freq
		j=m_sequential(nba(),l,s,i,0)
		for j=0 to freq(0)
			frq0(i,j)=freq(j)
		next j
	next i
	
	for r=1 to rits
		for i=1 to l*2
			swap nba(int(rnd*l)+1),nba(int(rnd*l)+1)
		next i
		for i=1 to 5
			erase freq
			j=m_sequential(nba(),l,s,i,0)
			for j=1 to freq(0)
				frq1(i,r,j)=freq(j)
			next j
		next i	
	next r
	
	dim as short allow=2 '-1
	for i=1 to allow
		os+=lb
		if i-1=1 then
			os+="Unique sequence frequencies, allow "+str(i-1)+" repeat:"+lb
		else
			os+="Unique sequence frequencies, allow "+str(i-1)+" repeats:"+lb
		end if
		os+="---------------------------------------------------------"+lb
		for j=1 to frq0(i,0)+1
			a=frq0(i,j)
			for k=1 to rits
				sda(k)=frq1(i,k,j)
			next k
			os+="Length "+str(j)+": "+str(frq0(i,j))+" (sigma: "+format(stdev(a,rits,sda()),"0.00")+")"
			if i=allow andalso j=frq0(i,0)+1 then exit for,for
			os+=lb
		next j
	next i
	
	'-------------------------------------------
	
	'i=m_2cycles_perfect_cyclebreaks(nuba(),l,s)
	'output_colormap(cmap(),info(),l,dx,dy,0,file_name+"_norm0")
	'output_colormap(cmap(),info(),l,dx,dy,1,file_name+"_norm1")
	'erase cmap
	
	'-------------------------------------------
	
	'dim as string li,o2
	''dim as short plain(200)
	'dim as double arg(100)
	'dim as double mioc,mult=0.10
	'dim as byte da(680,340)
	'dim as integer kk
	'
	'randomize 5678
	'
	'for kk=1 to 8
	'	
	'	mult+=0.05
	'	
	'	o2=""
	'
	'	for h=1 to 200
	'		
	'		'do
	'		'	r=int(rnd*124)+1
	'		'loop until plain(r)=0
	'		
	'		r=h '+25
	'		'plain(r)=1
	'		
	'		arg(1)=0
	'		open basedir+"/ciphers/plaintext/length 1000 plaintexts/p"+str(r)+".txt" for binary as #1
	'		do
	'			line input #1,li
	'			for i=1 to len(li)
	'				select case asc(li,i)
	'					case 65 to 90
	'						arg(1)+=1 'length
	'						cstate(11,arg(1))=asc(li,i)
	'					case 97 to 122
	'						arg(1)+=1 'length
	'						cstate(11,arg(1))=asc(li,i)-32
	'					case else
	'						beep
	'						os+=" "+str(h)+" "
	'				end select
	'			next i
	'		loop until eof(1)
	'		close #1
	'		
	'		erase da
	'		
	'		'arg(2)=cstate_nba(11,12,1000,26) 'plaintext symbols
	'		
	'		for k=1 to 5
	'			
	'			do
	'				
	'				'arg(1)=int(rnd*(680-51))+52 'random length
	'				arg(1)=int(rnd*(680-172))+173 'random length
	'				arg(2)=cstate_nba(11,12,arg(1),26) 'plaintext symbols
	'				arg(7)=1 'from
	'				arg(8)=arg(1) 'to
	'				'arg(10)=int(rnd*((arg(1)/2)-(arg(2)-1)))+arg(2) 'symbols, random multiplicity
	'				arg(10)=int(arg(1)*(mult+(mult/50))) 'symbols, random multiplicity
	'				mioc=((arg(1)/arg(10))*((arg(1)/arg(10))-1))*arg(10) 'calculate minimum raw ioc
	'				arg(11)=int(rnd*(mioc+1))+mioc 'random target raw ioc
	'				arg(12)=int(rnd*101) 'cycle randomness 0 to 100
	'				
	'			loop until da(arg(1),arg(10))=0
	'			
	'			da(arg(1),arg(10))=1
	'			
	'			cstate_operation(12,13,"Encode: homophonic substitution",arg())
	'			
	'			o2+="output_sub_directory=p"+str(r)+"_"+str(arg(1))+"_"+str(arg(10))+lb
	'			for i=1 to arg(1)
	'				o2+=str(cstate(13,i)+32)
	'				if i mod 40=0 then 
	'					if i<>arg(1) then o2+=lb
	'				else
	'					o2+=" "
	'				end if
	'			next i
	'			o2+=lb
	'			o2+="solution_plaintext="+lb
	'			for i=1 to arg(1)
	'				o2+=chr(cstate(11,i))
	'				if i<>arg(1) andalso i mod 40=0 then o2+=lb
	'			next i
	'			o2+=lb+lb
	'		
	'		next k
	'		
	'	next h
	'	
	'	open basedir+"/ciphers/multiplicity_"+str(kk)+".txt" for output as #1
	'	print #1,o2;
	'	close #1
	'
	'next kk
	
	'-------------------------------------------
	
	ui_editbox_settext(output_text,os)
	randomize timer
	
end sub

sub stats_omnidirectional(byval ng as short)
	
	dim as integer i,j,k,d,x,y,c1,e,a
	dim as string os,soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer num=info_numerical
	dim as double sda(1000)
	
	dim as integer cip1(l)
	dim as integer cip2(l)
	dim as long cip3(l)
	dim as integer num1(l)
	dim as integer num2(l)
	for i=1 to l
		num1(i)=i
		cip1(i)=info(i)
		cip2(i)=info(i)
	next i
	
	randomize 12345
	
	dim as string dirtable(16) 
	'dirtable(1)="Normal" 
	'dirtable(2)="Mirror"
	'dirtable(3)="Flip"
	dirtable(4)="Reverse"
	dirtable(5)="Columnar 1"
	'dirtable(6)="Columnar 2"
	dirtable(7)="Columnar 3"
	'dirtable(8)="Columnar 4"
	dirtable(9)="Diagonal 1"
	dirtable(10)="Diagonal 2"
	dirtable(11)="Diagonal 3"
	dirtable(12)="Diagonal 4"
	dirtable(13)="Diagonal 5"
	dirtable(14)="Diagonal 6"
	dirtable(15)="Diagonal 7"
	dirtable(16)="Diagonal 8"
	
	dim as double arg(20)
	dim as short oldunispacing=unispacing
	unispacing=1
	
	arg(1)=l
	arg(2)=s
	arg(3)=dx
	arg(4)=dy
	
	os+="BHdecrypt omnidirectional "+str(ng)+"-grams stats for: "+file_name+lb
	os+="---------------------------------------------------------" '+lb
	
	arg(5)=1 'transpose
	
	for d=1 to 16
		if dirtable(d)<>"" then
			for i=1 to l
				if num=0 then
					cip3(i)=32
				else
					cip3(i)=123456789
				end if
				cstate(11,i)=cip1(i)
			next i
			cstate_operation(11,12,dirtable(d),arg())
			for i=1 to l
				cip2(i)=cstate(12,i)
			next i
			for i=1 to l
				cstate(11,i)=num1(i)
			next i
			cstate_operation(11,12,dirtable(d),arg())
			for i=1 to l
				num2(i)=cstate(12,i)
			next i
			for i=1 to l-(ng-1)
				for j=1 to l-(ng-1)
					e=1
					for k=0 to (ng-1)
						if cip1(i+k)<>cip2(j+k) then
							e=0
							exit for
						end if
					next k
					if e=1 then
						for k=0 to (ng-1)
							cip3(i+k)=cip1(i+k)
							cip3(num2(j+k))=cip1(i+k)
						next k
					end if
				next j
			next i
			c1=0
			for i=1 to l
				if num=0 then
					if cip3(i)<>32 then c1+=1
				else	
					if cip3(i)<>123456789 then c1+=1
				end if
			next i
			if c1>0 then
				os+=lb
				os+=lb
				os+="Input versus "+lcase(dirtable(d))+": "+format((c1/l)*100,"0.00")+"%"+lb
				'os+="Input versus "+lcase(dirtable(d))+": "+str(c1)+" ("+format(stdev(c1,1000,sda()),"0.00")+")"+lb
				for i=1 to 30
					os+="-"
				next i
				os+=lb
				os+=info_to_string(cip3(),l,dx,dy,num,0,0)
			end if
		end if
	next d
	
	'os+=lb
	
	arg(5)=0 'untranspose
	
	'for d=1 to l-1
	for d=2 to l/2
		for i=1 to l
			if num=0 then
				cip3(i)=32
			else
				cip3(i)=123456789
			end if
			cstate(11,i)=cip1(i)
		next i
		arg(7)=d
		cstate_operation(11,12,"Period",arg())
		for i=1 to l
			cip2(i)=cstate(12,i)
		next i
		for i=1 to l
			cstate(11,i)=num1(i)
		next i
		cstate_operation(11,12,"Period",arg())
		for i=1 to l
			num2(i)=cstate(12,i)
		next i
		for i=1 to l-(ng-1)
			for j=1 to l-(ng-1)
				e=1
				for k=0 to (ng-1)
					if cip1(i+k)<>cip2(j+k) then
						e=0
						exit for
					end if
				next k
				if e=1 then
					for k=0 to (ng-1)
						cip3(i+k)=cip1(i+k)
						cip3(num2(j+k))=cip1(i+k)
					next k
				end if
			next j
		next i
		c1=0
		for i=1 to l
			if num=0 then
				if cip3(i)<>32 then c1+=1
			else	
				if cip3(i)<>123456789 then c1+=1
			end if
		next i
		if c1>0 then
			os+=lb
			os+=lb
			os+="Input versus untransposed period "+str(d)+": "+format((c1/l)*100,"0.00")+"%"+lb
			'os+="Input versus untransposed period "+str(d)+": "+str(c1)+" ("+format(stdev(c1,1000,sda()),"0.00")+")"+lb
			for i=1 to 30
				os+="-"
			next i
			os+=lb
			os+=info_to_string(cip3(),l,dx,dy,num,0,0)
		end if
	next d
	
	unispacing=oldunispacing	
	ui_editbox_settext(output_text,os)
	randomize timer

end sub

sub stats_findrearrangement(byval tn_ptr as any ptr)
	
	stats_running=1
	randomize 12345
	dim as string os,ot
	dim as integer i,j,k,x,y,c
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer num=info_numerical
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long cip(l)
	dim as double arg(100)
	dim as short its=1000
	dim as double sc0(its)
	dim as double score
	dim as double st(dx)
	dim as short total=(dx-1)+(dy-1)
	dim as double frtimer=timer
	dim as double pop(its)
	
	for i=1 to l
		cstate(11,i)=nuba(i)
		cip(i)=nuba(i)
	next i
	
	score=m_2cycles(cip(),l,s,5)
	
	os+="BHdecrypt find rearrangement stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	os+="- Attempts to find a set of dimensions in which a rearrangement "+lb
	os+="of rows or columns was applied after sequential encoding."+lb
	os+="- A higher percentage (improvement rate) may be "+lb
	os+="more indicative of a rearrangement."+lb
	os+=lb
	
	os+="Columnar rearrangement:"+lb
	os+="---------------------------------------------------------"+lb
	x=1
	do
		x+=1
		arg(1)=l
		arg(2)=s
		arg(3)=x
		arg(4)=l\x
		if frac(l/x)>0 then arg(4)+=1
		c+=1
		if timer-frtimer>1 then
			frtimer=timer
			ot="BHdecrypt find rearrangement: "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		dim as short su=0
		for i=1 to its
			dim as string cso=cstate_operation(11,12,"Randomize column order",arg())
			if left(cso,5)<>"Error" then
				for j=1 to l
					cip(j)=cstate(12,j)
				next j
				if m_2cycles(cip(),l,s,5)>score then su+=1
			end if
		next i				
		os+=str(x)+"*"+str(arg(4))+": "+format((su/its)*100,"0.00")+"%"+lb		
	loop until x=dx or stoptask=1
	
	os+=lb
	os+="Row rearrangement:"+lb
	os+="---------------------------------------------------------"+lb
	y=1
	do
		y+=1
		arg(1)=l
		arg(2)=s
		arg(3)=l\y
		arg(4)=y
		if frac(l/y)>0 then arg(3)+=1
		c+=1
		if timer-frtimer>1 then
			frtimer=timer
			ot="BHdecrypt find rearrangement: "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		dim as short su=0
		for i=1 to its
			dim as string cso=cstate_operation(11,12,"Randomize row order",arg())
			if left(cso,5)<>"Error" then
				for j=1 to l
					cip(j)=cstate(12,j)
				next j
				if m_2cycles(cip(),l,s,5)>score then su+=1
			end if
		next i	
		os+=str(arg(3))+"*"+str(y)+": "+format((su/its)*100,"0.00")+"%"
		if y<>dy then os+=lb
	loop until y=dy or stoptask=1
	
	stoptask=0
	stats_running=0
	ui_editbox_settext(output_text,os)
	thread_ptr(threadsmax+2)=0
	randomize timer

end sub

sub stats_direction(byval tn_ptr as any ptr)
	
	stats_running=1
	dim as integer m=stats_direction_m
	dim as string os,t,ot
	dim as string cso
	dim as string operation
	dim as string namedir
	dim as integer i,j,k,c,e
	dim as double arg(100)
	dim as integer depth=1
	dim as integer meas=0
	dim as integer norm=0
	dim as integer higher
	dim as double dirtimer=timer
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		stats_running=1
		ui_editbox_settext(output_text,soi)
		thread_ptr(threadsmax+2)=0
		exit sub
	end if
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long rnds=stats_dirrndtrials
	for i=1 to l-1
		if gcd(l,i)=1 then k+=1
	next i
	dim as integer total=16+l+dx+dy+(dx-1)+(dy-1)+(rnds)+k
	dim as long scores0(total) 'long
	dim as long scores1(total) 'long
	dim as long scores2(total) 'long
	dim as double sda(1000)
	dim as double avg0,avg1,avg2
	dim as double high0,high1,high2 'double
	dim as double highd
	dim as integer d=0
	dim as long cipout(l)
	dim as byte nba
	dim as short bmod=stats_bigramsmod
	
	'erase graph
	
	dim as string oper(16) 
	oper(1)="Normal"
	oper(2)="Mirror"
	oper(3)="Flip"
	oper(4)="Reverse"
	oper(5)="Columnar 1"
	oper(6)="Columnar 2"
	oper(7)="Columnar 3"
	oper(8)="Columnar 4"
	oper(9)="Diagonal 1"
	oper(10)="Diagonal 2"
	oper(11)="Diagonal 3"
	oper(12)="Diagonal 4"
	oper(13)="Diagonal 5"
	oper(14)="Diagonal 6"
	oper(15)="Diagonal 7"
	oper(16)="Diagonal 8"
	higher=1
	
	dim as string ps1
	ps1="BHdecrypt plaintext direction stats for: "+file_name+lb
	ps1+="---------------------------------------------------------"+lb	
	dim as string es1
	es1="BHdecrypt encoding direction stats for: "+file_name+lb
	es1+="---------------------------------------------------------"+lb
	dim as string pt1
	pt1="- Attempts to detect the plaintext direction from "+lb
	pt1+="the input using "
	dim as string en1
	en1="- Attempts to detect the encoding direction from "+lb
	en1+="the input given that it has sequential properties "+lb
	en1+="using "
	dim as string en2
	en2="- Attempts to detect the encoding direction from "+lb
	en2+="the input using "
	dim as string dh1
	dh1="- A higher score may be more indicative of the direction."+lb
	dim as string dl1
	dl1="- A lower score may be more indicative of the direction."+lb
		 
	select case m
		case 1
			namedir="Plaintext direction: bigrams"
			os=ps1+pt1+"bigrams."+lb+dh1
		case 2
			namedir="Plaintext direction: raw bigram IOC"
			os=ps1+pt1+"raw bigram IOC."+lb+dh1
		case 3
			namedir="Plaintext direction: bigrams alphabet="+str(bmod)
			os=ps1+pt1+"bigrams alphabet="+str(bmod)+"."+lb+dh1
		case 4
			namedir="Plaintext direction: asymmetry"
			os=ps1+pt1+"asymmetry."+lb+dh1
		case 5
			namedir="Encoding direction: 2-symbol cycles"
			os=es1+en1+"2-symbol cycles."+lb+dh1
		case 6
			namedir="Encoding direction: 3-symbol cycles"
			os=es1+en1+"3-symbol cycles."+lb+dh1
		case 7
			namedir="Encoding direction: sequential"
			os=es1+en1+"sequential."+lb+dh1
		case 8
			higher=0
			namedir="Encoding direction: appearance"
			os=es1+en1+"appearance."+lb+dl1
		case 9
			higher=0
			namedir="Encoding direction: unigrams"
			os+=es1+en1+"unigrams."+lb+dl1
		case 10
			higher=0
			namedir="Encoding direction: unigrams sliding"
			os=es1+en1+"sliding unigrams."+lb+dl1
		case 11
			higher=0
			namedir="Encoding direction: midpoint shift"
			os=es1+en1+"midpoint shift."+lb+dl1
		case 12
			namedir="Plaintext direction: bigrams depth=2"
			os=ps1+pt1+"bigrams depth=2."+lb+dh1
		case 13
			namedir="Plaintext direction: repeats"
			os=ps1+pt1+"repeats."+lb+dh1
		case 14
			higher=0
			namedir="Encoding direction: smoothness"
			os=es1+en1+"smoothness."+lb
		case 15
			namedir="Encoding direction: slope"
			os=es1+en1+"slope."+lb+dh1
		case 16
			namedir="Encoding direction: keyword length"
			os=es1+en2+"keyword length."+lb+dh1
		case 17
			namedir="Plaintext direction: odd bigrams"
			os=ps1+pt1+"odd bigrams."+lb+dh1
		case 18
			namedir="Encoding direction: unigram distance"
			os=es1+en1+"unigram distance."+lb+dh1
		case 19
			namedir="Encoding direction: 2-symbol cycles depth=2"
			os=es1+en1+"2-symbol cycles depth=2."+lb+dh1
		case 20
			namedir="Plaintext direction: raw bigram IOC depth=2"
			os=ps1+pt1+"raw bigram IOC depth=2."+lb+dh1
		case 21
			namedir="Encoding direction: slope NBA"
			os=es1+en1+"slope NBA."+lb+dh1
		case 22
			namedir="Encoding direction: smoothness NBA"
			os=es1+en1+"smoothness NBA."+lb
		case 23
			namedir="Plaintext direction: deep bigrams"
			os=ps1+pt1+"deep bigrams."+lb+dh1
		case 24
			namedir="Plaintext direction: reversal bigrams"
			os=ps1+pt1+"reversal bigrams."+lb
		case 25
			namedir="Plaintext direction: trigrams"
			os=ps1+pt1+"trigrams."+lb+dh1
		case 26
			namedir="Plaintext direction: 5-gram fragments"
			os=ps1+pt1+"5-gram fragments."+lb+dh1
		case 27
			'higher=0
			namedir="Plaintext direction: doublets"
			os=ps1+pt1+"doublets."+lb+dl1
		case 28
			'higher=0
			namedir="Plaintext direction: triplets"
			os=ps1+pt1+"triplets."+lb+dl1
		case 29
			namedir="Encoding direction: prime phobia"
			os=es1+en1+"prime phobia."+lb+dh1
		case 30
			namedir="Encoding direction: perfect 2-symbol cycles"
			os=es1+en1+"perfect 2-symbol cycles."+lb+dh1
		case 31
			namedir="Encoding direction: perfect 3-symbol cycles"
			os=es1+en1+"perfect 3-symbol cycles."+lb+dh1
		case 32
			higher=0
			namedir="Encoding direction: sub string position"
			os=es1+en1+"sub string position."+lb+dl1
		case 33
			higher=0
			namedir="Plaintext direction: contact variety"
			os=ps1+pt1+"contact variety."+lb+dl1
		case 34
			namedir="Encoding direction: cycle spectrum"
			os=es1+en1+"cycle spectrum."+lb+dh1
		case 35
			higher=0
			namedir="Encoding direction: horizontal symbol leaning"
			os=es1+en1+"horizontal symbol leaning."+lb+dl1
		case 36
			higher=0
			namedir="Encoding direction: vertical symbol leaning"
			os=es1+en1+"vertical symbol leaning."+lb+dl1
	end select
	
	for i=1 to l
		select case m
			case 14,15:cstate(11,i)=info(i)
			case else:cstate(11,i)=nuba(i)
		end select
	next i
	
	select case m 'nba
		case 14,15:nba=0
		case else:nba=1
	end select		
	
	if higher=0 then
		high0=999999999
		high1=999999999
		high2=999999999
	else
		high0=0
		high1=0
		high2=0
	end if
	
	arg(1)=l
	arg(2)=s
	arg(3)=dx
	arg(4)=dy
	arg(6)=0 'keepnulls
	
	randomize 12345
	
	dim as byte sigma=1
	dim as double mean,sd
	if sigma=1 then 'calculate stdev
		dim as integer items=1000
		total+=1000
		for i=1 to items
			c+=1
			if timer-dirtimer>1 then
				dirtimer=timer
				ot=namedir+": "+format((c/total)*100,"0.00")+"% complete"+lb
				ot+="(click stop task to cancel)"
				ui_editbox_settext(output_text,ot)
			end if
			operation="Randomize"
			cso=cstate_operation(11,12,operation,arg())
			if nba=1 then
				cstate_nba(12,13,l,s)
				for j=1 to l
					cstate(12,j)=cstate(13,j)
				next j
			end if
			if left(cso,5)<>"Error" then
				for j=1 to l
					cipout(j)=cstate(12,j)
				next j
			end if
			select case m
				case 1:sda(i)=m_fastbigrams(cipout(),l,s)
				case 2:sda(i)=m_bigrams(cipout(),l,s,2)
				case 3:sda(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
				case 4:sda(i)=m_asymmetry(cipout(),l,s,norm)
				case 5:sda(i)=m_2cycles(cipout(),l,s,5)
				case 6:sda(i)=m_3cycles(cipout(),l,s,5)
				case 7:sda(i)=m_sequential(cipout(),l,s,1,0)
				case 8:sda(i)=m_appearance(cipout(),l,s,1,0)
				case 9:sda(i)=m_unigrams(cipout(),l,s,dx,0)
				case 10:sda(i)=m_unigrams(cipout(),l,s,dx,1)
				case 11:sda(i)=m_midpointshift(cipout(),l,s,0)
				case 12:sda(i)=m_depth(cipout(),l,s,0,0,0)
				case 13:sda(i)=m_ngrams(cipout(),l,0)
				case 14:sda(i)=m_smoothness(cipout(),l,s,0)
				case 15:sda(i)=m_slope(cipout(),l,s,0)
				case 16:sda(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
				case 17:sda(i)=m_bigrams(cipout(),l,s,4)
				case 18:sda(i)=m_unigramdistance(cipout(),l,s)
				case 19:sda(i)=m_depth(cipout(),l,s,0,2,0)
				case 20:sda(i)=m_depth(cipout(),l,s,0,1,0)
				case 21:sda(i)=m_slope(cipout(),l,s,0)
				case 22:sda(i)=m_smoothness(cipout(),l,s,0)
				case 23:sda(i)=m_deep(cipout(),l,s,depth,0,0)
				case 24:sda(i)=m_reversal(cipout(),l,s)
				case 25:sda(i)=m_fasttrigrams(cipout(),l,s)
				case 26:sda(i)=m_ngramfragments(cipout(),l,s,5)
				case 27:sda(i)=m_npairs(cipout(),l,2)
				case 28:sda(i)=m_npairs(cipout(),l,3)
				case 29:sda(i)=m_primephobia(cipout(),l,s,0)
				case 30:sda(i)=m_2cycles_perfect(cipout(),l,s)
				case 31:sda(i)=m_3cycles_perfect(cipout(),l,s)
				case 32:sda(i)=m_shortestsubstring(cipout(),l,s)
				case 33:sda(i)=m_contactvariety(cipout(),l,s)*10000
				case 34:sda(i)=m_2cyclespectrum(cipout(),l,s)*10000
				case 35:sda(i)=m_lean(cipout(),l,s,dx,dy,0)
				case 36:sda(i)=m_lean(cipout(),l,s,dx,dy,1)
			end select
		next i
		for i=1 to items
			mean+=sda(i)
		next i
		mean/=items
		for i=1 to items
			sd+=(sda(i)-mean)^2
		next i
		sd=sqr(sd/items)
	end if
	
	for i=1 to 16 'directions
		c+=1
		if timer-dirtimer>1 then
			dirtimer=timer
			ot=namedir+": "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		operation=oper(i)
		arg(5)=0 'transposition
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		randomize 12345
		select case m
			case 1:scores0(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores0(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores0(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores0(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores0(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores0(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores0(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores0(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores0(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores0(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores0(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores0(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores0(i)=m_ngrams(cipout(),l,0)
			case 14:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores0(i)=m_slope(cipout(),l,s,0)
			case 16:scores0(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores0(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores0(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores0(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores0(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores0(i)=m_slope(cipout(),l,s,0)
			case 22:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores0(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores0(i)=m_reversal(cipout(),l,s)
			case 25:scores0(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores0(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores0(i)=m_npairs(cipout(),l,2)
			case 28:scores0(i)=m_npairs(cipout(),l,3)
			case 29:scores0(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores0(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores0(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores0(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores0(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores0(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores0(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores0(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select	
		arg(5)=1 'untransposition
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		randomize 12345
		select case m
			case 1:scores1(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores1(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores1(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores1(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores1(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores1(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores1(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores1(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores1(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores1(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores1(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores1(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores1(i)=m_ngrams(cipout(),l,0)
			case 14:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores1(i)=m_slope(cipout(),l,s,0)
			case 16:scores1(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores1(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores1(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores1(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores1(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores1(i)=m_slope(cipout(),l,s,0)
			case 22:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores1(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores1(i)=m_reversal(cipout(),l,s)
			case 25:scores1(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores1(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores1(i)=m_npairs(cipout(),l,2)
			case 28:scores1(i)=m_npairs(cipout(),l,3)
			case 29:scores1(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores1(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores1(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores1(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores1(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores1(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores1(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores1(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select
		avg0+=scores0(i)
		avg1+=scores1(i)
		if higher=0 then
			if val(format(scores0(i),"0.00"))<high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))<high1 then high1=val(format(scores1(i),"0.00"))
		else
			if val(format(scores0(i),"0.00"))>high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))>high1 then high1=val(format(scores1(i),"0.00"))
		end if
		if stoptask=1 then
			stoptask=0
			stats_running=0
			ui_editbox_settext(output_text,"")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if
	next i
	avg0/=16
	avg1/=16
	os+=lb
	os+="Directional: (transposition, untransposition)"+lb
	os+="---------------------------------------------------------"
	for i=1 to 16
		operation=oper(i)
		select case i	 
			case 5,9
				os+=lb
				os+="---------------------------------------------------------"
		end select
		os+=lb
		os+=operation+": "+format(scores0(i),"0.00")+", "+format(scores1(i),"0.00")
		if sigma=1 then os+=" ("+format((scores0(i)-mean)/sd,"0.00")+", "+format((scores1(i)-mean)/sd,"0.00")+")"
		if val(format(scores0(i),"0.00"))=high0 or val(format(scores1(i),"0.00"))=high1 then os+=" <---"
	next i
	os+=lb
	os+="---------------------------------------------------------"+lb
	os+="Transposition average: "+format(avg0,"0.00")+lb
	os+="Untransposition average: "+format(avg1,"0.00")+lb
	avg0=0
	avg1=0
	avg2=0
	if higher=0 then
		high0=999999999
		high1=999999999
		high2=999999999
	else
		high0=0
		high1=0
		high2=0
	end if
	
	for i=0 to dy-1 'row offset
		c+=1
		if timer-dirtimer>1 then
			dirtimer=timer
			ot=namedir+": "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		operation="Offset row order"
		arg(5)=0 'transposition
		arg(7)=i
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		else
			beep
		end if
		randomize 12345
		select case m
			case 1:scores0(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores0(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores0(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores0(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores0(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores0(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores0(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores0(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores0(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores0(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores0(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores0(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores0(i)=m_ngrams(cipout(),l,0)
			case 14:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores0(i)=m_slope(cipout(),l,s,0)
			case 16:scores0(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores0(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores0(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores0(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores0(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores0(i)=m_slope(cipout(),l,s,0)
			case 22:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores0(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores0(i)=m_reversal(cipout(),l,s)
			case 25:scores0(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores0(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores0(i)=m_npairs(cipout(),l,2)
			case 28:scores0(i)=m_npairs(cipout(),l,3)
			case 29:scores0(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores0(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores0(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores0(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores0(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores0(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores0(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores0(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select	
		avg0+=scores0(i)
		if higher=0 then
			if val(format(scores0(i),"0.00"))<high0 then high0=val(format(scores0(i),"0.00"))
		else
			if val(format(scores0(i),"0.00"))>high0 then high0=val(format(scores0(i),"0.00"))
		end if
		if stoptask=1 then
			stoptask=0
			stats_running=0
			ui_editbox_settext(output_text,"")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if
	next i
	avg0/=dy
	os+=lb
	os+="Offset row order: (transposition)"+lb
	os+="---------------------------------------------------------"
	for i=0 to dy-1
		operation="Offset row order "+str(i)
		os+=lb
		os+=operation+": "+format(scores0(i),"0.00")
		if sigma=1 then os+=" ("+format((scores0(i)-mean)/sd,"0.00")+")"
		if val(format(scores0(i),"0.00"))=high0 then os+=" <---"
	next i
	os+=lb
	os+="---------------------------------------------------------"+lb
	os+="Transposition average: "+format(avg0,"0.00")+lb
	avg0=0
	avg1=0
	avg2=0
	if higher=0 then
		high0=999999999
		high1=999999999
		high2=999999999
	else
		high0=0
		high1=0
		high2=0
	end if
	
	for i=0 to dx-1 'column offset
		c+=1
		if timer-dirtimer>1 then
			dirtimer=timer
			ot=namedir+": "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		operation="Offset column order"
		arg(5)=0 'transposition
		arg(7)=i
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		else
			beep
		end if
		randomize 12345
		select case m
			case 1:scores0(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores0(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores0(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores0(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores0(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores0(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores0(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores0(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores0(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores0(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores0(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores0(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores0(i)=m_ngrams(cipout(),l,0)
			case 14:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores0(i)=m_slope(cipout(),l,s,0)
			case 16:scores0(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores0(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores0(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores0(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores0(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores0(i)=m_slope(cipout(),l,s,0)
			case 22:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores0(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores0(i)=m_reversal(cipout(),l,s)
			case 25:scores0(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores0(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores0(i)=m_npairs(cipout(),l,2)
			case 28:scores0(i)=m_npairs(cipout(),l,3)
			case 29:scores0(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores0(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores0(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores0(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores0(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores0(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores0(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores0(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select		
		avg0+=scores0(i)
		if higher=0 then
			if val(format(scores0(i),"0.00"))<high0 then high0=val(format(scores0(i),"0.00"))
		else
			if val(format(scores0(i),"0.00"))>high0 then high0=val(format(scores0(i),"0.00"))
		end if
		if stoptask=1 then
			stoptask=0
			stats_running=0
			ui_editbox_settext(output_text,"")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if
	next i
	avg0/=dx
	os+=lb
	os+="Offset column order: (transposition)"+lb
	os+="---------------------------------------------------------"
	for i=0 to dx-1
		operation="Offset column order "+str(i)
		os+=lb
		os+=operation+": "+format(scores0(i),"0.00")
		if sigma=1 then os+=" ("+format((scores0(i)-mean)/sd,"0.00")+")"
		if val(format(scores0(i),"0.00"))=high0 then os+=" <---"
	next i
	os+=lb
	os+="---------------------------------------------------------"+lb
	os+="Transposition average: "+format(avg0,"0.00")+lb
	avg0=0
	avg1=0
	avg2=0
	if higher=0 then
		high0=999999999
		high1=999999999
		high2=999999999
	else
		high0=0
		high1=0
		high2=0
	end if
	
	for i=1 to dy-1 'period row order
		c+=1
		if timer-dirtimer>1 then
			dirtimer=timer
			ot=namedir+": "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		operation="Period row order"
		arg(5)=0 'transposition
		arg(7)=i
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		randomize 12345
		select case m
			case 1:scores0(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores0(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores0(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores0(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores0(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores0(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores0(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores0(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores0(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores0(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores0(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores0(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores0(i)=m_ngrams(cipout(),l,0)
			case 14:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores0(i)=m_slope(cipout(),l,s,0)
			case 16:scores0(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores0(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores0(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores0(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores0(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores0(i)=m_slope(cipout(),l,s,0)
			case 22:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores0(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores0(i)=m_reversal(cipout(),l,s)
			case 25:scores0(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores0(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores0(i)=m_npairs(cipout(),l,2)
			case 28:scores0(i)=m_npairs(cipout(),l,3)
			case 29:scores0(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores0(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores0(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores0(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores0(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores0(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores0(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores0(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select	
		arg(5)=1 'untransposition
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		randomize 12345
		select case m
			case 1:scores1(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores1(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores1(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores1(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores1(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores1(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores1(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores1(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores1(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores1(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores1(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores1(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores1(i)=m_ngrams(cipout(),l,0)
			case 14:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores1(i)=m_slope(cipout(),l,s,0)
			case 16:scores1(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores1(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores1(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores1(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores1(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores1(i)=m_slope(cipout(),l,s,0)
			case 22:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores1(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores1(i)=m_reversal(cipout(),l,s)
			case 25:scores1(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores1(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores1(i)=m_npairs(cipout(),l,2)
			case 28:scores1(i)=m_npairs(cipout(),l,3)
			case 29:scores1(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores1(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores1(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores1(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores1(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores1(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores1(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores1(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select
		avg0+=scores0(i)
		avg1+=scores1(i)
		if higher=0 then
			if val(format(scores0(i),"0.00"))<high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))<high1 then high1=val(format(scores1(i),"0.00"))
		else
			if val(format(scores0(i),"0.00"))>high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))>high1 then high1=val(format(scores1(i),"0.00"))
		end if
		if stoptask=1 then
			stoptask=0
			stats_running=0
			ui_editbox_settext(output_text,"")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if
	next i
	avg0/=(dy-1)
	avg1/=(dy-1)
	os+=lb
	os+="Period row order: (transposition, untransposition)"+lb
	os+="---------------------------------------------------------"
	for i=1 to dy-1
		operation="Period row order "+str(i)
		os+=lb
		os+=operation+": "+format(scores0(i),"0.00")+", "+format(scores1(i),"0.00")
		if sigma=1 then os+=" ("+format((scores0(i)-mean)/sd,"0.00")+", "+format((scores1(i)-mean)/sd,"0.00")+")"
		if val(format(scores0(i),"0.00"))=high0 or val(format(scores1(i),"0.00"))=high1 then os+=" <---"
	next i
	os+=lb
	os+="---------------------------------------------------------"+lb
	os+="Transposition average: "+format(avg0,"0.00")+lb
	os+="Untransposition average: "+format(avg1,"0.00")+lb
	avg0=0
	avg1=0
	avg2=0
	if higher=0 then
		high0=999999999
		high1=999999999
		high2=999999999
	else
		high0=0
		high1=0
		high2=0
	end if
	
	for i=1 to dx-1 'period column order
		c+=1
		if timer-dirtimer>1 then
			dirtimer=timer
			ot=namedir+": "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		operation="Period column order"
		arg(5)=0 'transposition
		arg(7)=i
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		randomize 12345
		select case m
			case 1:scores0(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores0(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores0(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores0(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores0(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores0(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores0(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores0(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores0(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores0(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores0(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores0(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores0(i)=m_ngrams(cipout(),l,0)
			case 14:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores0(i)=m_slope(cipout(),l,s,0)
			case 16:scores0(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores0(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores0(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores0(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores0(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores0(i)=m_slope(cipout(),l,s,0)
			case 22:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores0(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores0(i)=m_reversal(cipout(),l,s)
			case 25:scores0(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores0(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores0(i)=m_npairs(cipout(),l,2)
			case 28:scores0(i)=m_npairs(cipout(),l,3)
			case 29:scores0(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores0(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores0(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores0(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores0(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores0(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores0(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores0(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select
		arg(5)=1 'untransposition
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		randomize 12345
		select case m
			case 1:scores1(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores1(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores1(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores1(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores1(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores1(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores1(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores1(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores1(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores1(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores1(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores1(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores1(i)=m_ngrams(cipout(),l,0)
			case 14:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores1(i)=m_slope(cipout(),l,s,0)
			case 16:scores1(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores1(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores1(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores1(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores1(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores1(i)=m_slope(cipout(),l,s,0)
			case 22:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores1(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores1(i)=m_reversal(cipout(),l,s)
			case 25:scores1(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores1(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores1(i)=m_npairs(cipout(),l,2)
			case 28:scores1(i)=m_npairs(cipout(),l,3)
			case 29:scores1(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores1(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores1(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores1(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores1(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores1(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores1(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores1(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select
		avg0+=scores0(i)
		avg1+=scores1(i)
		if higher=0 then
			if val(format(scores0(i),"0.00"))<high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))<high1 then high1=val(format(scores1(i),"0.00"))
		else
			if val(format(scores0(i),"0.00"))>high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))>high1 then high1=val(format(scores1(i),"0.00"))
		end if
		if stoptask=1 then
			stoptask=0
			stats_running=0
			ui_editbox_settext(output_text,"")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if
	next i
	avg0/=(dx-1)
	avg1/=(dx-1)
	os+=lb
	os+="Period column order: (transposition, untransposition)"+lb
	os+="---------------------------------------------------------"
	for i=1 to dx-1
		operation="Period column order "+str(i)
		os+=lb
		os+=operation+": "+format(scores0(i),"0.00")+", "+format(scores1(i),"0.00")
		if sigma=1 then os+=" ("+format((scores0(i)-mean)/sd,"0.00")+", "+format((scores1(i)-mean)/sd,"0.00")+")"
		if val(format(scores0(i),"0.00"))=high0 or val(format(scores1(i),"0.00"))=high1 then os+=" <---"
	next i
	os+=lb
	os+="---------------------------------------------------------"+lb
	os+="Transposition average: "+format(avg0,"0.00")+lb
	os+="Untransposition average: "+format(avg1,"0.00")+lb
	avg0=0
	avg1=0
	avg2=0
	if higher=0 then
		high0=999999999
		high1=999999999
		high2=999999999
	else
		high0=0
		high1=0
		high2=0
	end if
	
	for i=1 to l-1 'periodic
		c+=1
		if timer-dirtimer>1 then
			dirtimer=timer
			ot=namedir+": "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		operation="Period"
		arg(5)=0 'transposition
		arg(7)=i
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		randomize 12345
		select case m
			case 1:scores0(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores0(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores0(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores0(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores0(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores0(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores0(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores0(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores0(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores0(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores0(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores0(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores0(i)=m_ngrams(cipout(),l,0)
			case 14:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores0(i)=m_slope(cipout(),l,s,0)
			case 16:scores0(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores0(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores0(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores0(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores0(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores0(i)=m_slope(cipout(),l,s,0)
			case 22:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores0(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores0(i)=m_reversal(cipout(),l,s)
			case 25:scores0(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores0(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores0(i)=m_npairs(cipout(),l,2)
			case 28:scores0(i)=m_npairs(cipout(),l,3)
			case 29:scores0(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores0(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores0(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores0(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores0(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores0(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores0(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores0(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select	
		arg(5)=1 'untransposition	
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		randomize 12345
		select case m
			case 1:scores1(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores1(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores1(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores1(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores1(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores1(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores1(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores1(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores1(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores1(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores1(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores1(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores1(i)=m_ngrams(cipout(),l,0)
			case 14:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores1(i)=m_slope(cipout(),l,s,0)
			case 16:scores1(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores1(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores1(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores1(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores1(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores1(i)=m_slope(cipout(),l,s,0)
			case 22:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores1(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores1(i)=m_reversal(cipout(),l,s)
			case 25:scores1(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores1(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores1(i)=m_npairs(cipout(),l,2)
			case 28:scores1(i)=m_npairs(cipout(),l,3)
			case 29:scores1(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores1(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores1(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores1(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores1(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores1(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores1(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores1(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select
		avg0+=scores0(i)
		avg1+=scores1(i)
		if higher=0 then
			if val(format(scores0(i),"0.00"))<high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))<high1 then high1=val(format(scores1(i),"0.00"))
		else
			if val(format(scores0(i),"0.00"))>high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))>high1 then high1=val(format(scores1(i),"0.00"))
		end if
		if stoptask=1 then
			stoptask=0
			stats_running=0
			ui_editbox_settext(output_text,"")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if
	next i
	avg0/=l-1
	avg1/=l-1
	os+=lb
	os+="Periodic: (transposition, untransposition)"+lb
	os+="---------------------------------------------------------"
	for i=1 to l-1
		operation="Period "+str(i)
		os+=lb
		os+=operation+": "+format(scores0(i),"0.00")+", "+format(scores1(i),"0.00")
		if sigma=1 then os+=" ("+format((scores0(i)-mean)/sd,"0.00")+", "+format((scores1(i)-mean)/sd,"0.00")+")"
		if val(format(scores0(i),"0.00"))=high0 or val(format(scores1(i),"0.00"))=high1 then os+=" <---"
	next i
	os+=lb
	os+="---------------------------------------------------------"+lb
	os+="Transposition average: "+format(avg0,"0.00")+lb
	'os+="- Landscape smoothness: "+str(m_smoothness(scores0(),l,0,1))+lb
	os+="Untransposition average: "+format(avg1,"0.00")+lb
	'os+="- Landscape smoothness: "+str(m_smoothness(scores1(),l,0,1))
	avg0=0
	avg1=0
	avg2=0
	if higher=0 then
		high0=999999999
		high1=999999999
		high2=999999999
	else
		high0=0
		high1=0
		high2=0
	end if
	
	k=0
	for i=1 to l-1 'skytale
		if gcd(l,i)=1 then 
			c+=1
			k+=1
			if timer-dirtimer>1 then
				dirtimer=timer
				ot=namedir+": "+format((c/total)*100,"0.00")+"% complete"+lb
				ot+="(click stop task to cancel)"
				ui_editbox_settext(output_text,ot)
			end if
			operation="Skytale"
			arg(5)=0 'transposition
			arg(7)=i
			cso=cstate_operation(11,12,operation,arg())
			if nba=1 then
				cstate_nba(12,13,l,s)
				for j=1 to l
					cstate(12,j)=cstate(13,j)
				next j
			end if
			if left(cso,5)<>"Error" then
				for j=1 to l
					cipout(j)=cstate(12,j)
				next j
			end if
			randomize 12345
			select case m
				case 1:scores0(i)=m_fastbigrams(cipout(),l,s)
				case 2:scores0(i)=m_bigrams(cipout(),l,s,2)
				case 3:scores0(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
				case 4:scores0(i)=m_asymmetry(cipout(),l,s,norm)
				case 5:scores0(i)=m_2cycles(cipout(),l,s,5)
				case 6:scores0(i)=m_3cycles(cipout(),l,s,5)
				case 7:scores0(i)=m_sequential(cipout(),l,s,1,0)
				case 8:scores0(i)=m_appearance(cipout(),l,s,1,0)
				case 9:scores0(i)=m_unigrams(cipout(),l,s,dx,0)
				case 10:scores0(i)=m_unigrams(cipout(),l,s,dx,1)
				case 11:scores0(i)=m_midpointshift(cipout(),l,s,0)
				case 12:scores0(i)=m_depth(cipout(),l,s,0,0,0)
				case 13:scores0(i)=m_ngrams(cipout(),l,0)
				case 14:scores0(i)=m_smoothness(cipout(),l,s,0)
				case 15:scores0(i)=m_slope(cipout(),l,s,0)
				case 16:scores0(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
				case 17:scores0(i)=m_bigrams(cipout(),l,s,4)
				case 18:scores0(i)=m_unigramdistance(cipout(),l,s)
				case 19:scores0(i)=m_depth(cipout(),l,s,0,2,0)
				case 20:scores0(i)=m_depth(cipout(),l,s,0,1,0)
				case 21:scores0(i)=m_slope(cipout(),l,s,0)
				case 22:scores0(i)=m_smoothness(cipout(),l,s,0)
				case 23:scores0(i)=m_deep(cipout(),l,s,depth,0,0)
				case 24:scores0(i)=m_reversal(cipout(),l,s)
				case 25:scores0(i)=m_fasttrigrams(cipout(),l,s)
				case 26:scores0(i)=m_ngramfragments(cipout(),l,s,5)
				case 27:scores0(i)=m_npairs(cipout(),l,2)
				case 28:scores0(i)=m_npairs(cipout(),l,3)
				case 29:scores0(i)=m_primephobia(cipout(),l,s,0)
				case 30:scores0(i)=m_2cycles_perfect(cipout(),l,s)
				case 31:scores0(i)=m_3cycles_perfect(cipout(),l,s)
				case 32:scores0(i)=m_shortestsubstring(cipout(),l,s)
				case 33:scores0(i)=m_contactvariety(cipout(),l,s)*10000
				case 34:scores0(i)=m_2cyclespectrum(cipout(),l,s)*10000
				case 35:scores0(i)=m_lean(cipout(),l,s,dx,dy,0)
				case 36:scores0(i)=m_lean(cipout(),l,s,dx,dy,1)
			end select	
			arg(5)=1 'untransposition	
			cso=cstate_operation(11,12,operation,arg())
			if nba=1 then
				cstate_nba(12,13,l,s)
				for j=1 to l
					cstate(12,j)=cstate(13,j)
				next j
			end if
			if left(cso,5)<>"Error" then
				for j=1 to l
					cipout(j)=cstate(12,j)
				next j
			end if
			randomize 12345
			select case m
				case 1:scores1(i)=m_fastbigrams(cipout(),l,s)
				case 2:scores1(i)=m_bigrams(cipout(),l,s,2)
				case 3:scores1(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
				case 4:scores1(i)=m_asymmetry(cipout(),l,s,norm)
				case 5:scores1(i)=m_2cycles(cipout(),l,s,5)
				case 6:scores1(i)=m_3cycles(cipout(),l,s,5)
				case 7:scores1(i)=m_sequential(cipout(),l,s,1,0)
				case 8:scores1(i)=m_appearance(cipout(),l,s,1,0)
				case 9:scores1(i)=m_unigrams(cipout(),l,s,dx,0)
				case 10:scores1(i)=m_unigrams(cipout(),l,s,dx,1)
				case 11:scores1(i)=m_midpointshift(cipout(),l,s,0)
				case 12:scores1(i)=m_depth(cipout(),l,s,0,0,0)
				case 13:scores1(i)=m_ngrams(cipout(),l,0)
				case 14:scores1(i)=m_smoothness(cipout(),l,s,0)
				case 15:scores1(i)=m_slope(cipout(),l,s,0)
				case 16:scores1(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
				case 17:scores1(i)=m_bigrams(cipout(),l,s,4)
				case 18:scores1(i)=m_unigramdistance(cipout(),l,s)
				case 19:scores1(i)=m_depth(cipout(),l,s,0,2,0)
				case 20:scores1(i)=m_depth(cipout(),l,s,0,1,0)
				case 21:scores1(i)=m_slope(cipout(),l,s,0)
				case 22:scores1(i)=m_smoothness(cipout(),l,s,0)
				case 23:scores1(i)=m_deep(cipout(),l,s,depth,0,0)
				case 24:scores1(i)=m_reversal(cipout(),l,s)
				case 25:scores1(i)=m_fasttrigrams(cipout(),l,s)
				case 26:scores1(i)=m_ngramfragments(cipout(),l,s,5)
				case 27:scores1(i)=m_npairs(cipout(),l,2)
				case 28:scores1(i)=m_npairs(cipout(),l,3)
				case 29:scores1(i)=m_primephobia(cipout(),l,s,0)
				case 30:scores1(i)=m_2cycles_perfect(cipout(),l,s)
				case 31:scores1(i)=m_3cycles_perfect(cipout(),l,s)
				case 32:scores1(i)=m_shortestsubstring(cipout(),l,s)
				case 33:scores1(i)=m_contactvariety(cipout(),l,s)*10000
				case 34:scores1(i)=m_2cyclespectrum(cipout(),l,s)*10000
				case 35:scores1(i)=m_lean(cipout(),l,s,dx,dy,0)
				case 36:scores1(i)=m_lean(cipout(),l,s,dx,dy,1)
			end select
			avg0+=scores0(i)
			avg1+=scores1(i)
			if higher=0 then
				if val(format(scores0(i),"0.00"))<high0 then high0=val(format(scores0(i),"0.00"))
				if val(format(scores1(i),"0.00"))<high1 then high1=val(format(scores1(i),"0.00"))
			else
				if val(format(scores0(i),"0.00"))>high0 then high0=val(format(scores0(i),"0.00"))
				if val(format(scores1(i),"0.00"))>high1 then high1=val(format(scores1(i),"0.00"))
			end if
			if stoptask=1 then
				stoptask=0
				stats_running=0
				ui_editbox_settext(output_text,"")
				thread_ptr(threadsmax+2)=0
				exit sub
			end if
		end if
	next i
	avg0/=k
	avg1/=k
	os+=lb
	os+="Skytale: (transposition, untransposition)"+lb
	os+="---------------------------------------------------------"
	for i=1 to l-1
		if gcd(l,i)=1 then
			operation="Skytale "+str(i)
			os+=lb
			os+=operation+": "+format(scores0(i),"0.00")+", "+format(scores1(i),"0.00")
			if sigma=1 then os+=" ("+format((scores0(i)-mean)/sd,"0.00")+", "+format((scores1(i)-mean)/sd,"0.00")+")"
			if val(format(scores0(i),"0.00"))=high0 or val(format(scores1(i),"0.00"))=high1 then os+=" <---"
		end if
	next i
	os+=lb
	os+="---------------------------------------------------------"+lb
	os+="Transposition average: "+format(avg0,"0.00")+lb
	'os+="- Landscape smoothness: "+str(m_smoothness(scores0(),l,0,1))+lb
	os+="Untransposition average: "+format(avg1,"0.00")+lb
	'os+="- Landscape smoothness: "+str(m_smoothness(scores1(),l,0,1))
	avg0=0
	avg1=0
	avg2=0
	if higher=0 then
		high0=999999999
		high1=999999999
		high2=999999999
	else
		high0=0
		high1=0
		high2=0
	end if
	
	for i=1 to rnds
		c+=1
		if timer-dirtimer>1 then
			dirtimer=timer
			ot=namedir+": "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		operation="Randomize"
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		'randomize 12345
		select case m
			case 1:scores0(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores0(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores0(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores0(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores0(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores0(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores0(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores0(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores0(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores0(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores0(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores0(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores0(i)=m_ngrams(cipout(),l,0)
			case 14:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores0(i)=m_slope(cipout(),l,s,0)
			case 16:scores0(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores0(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores0(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores0(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores0(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores0(i)=m_slope(cipout(),l,s,0)
			case 22:scores0(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores0(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores0(i)=m_reversal(cipout(),l,s)
			case 25:scores0(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores0(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores0(i)=m_npairs(cipout(),l,2)
			case 28:scores0(i)=m_npairs(cipout(),l,3)
			case 29:scores0(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores0(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores0(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores0(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores0(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores0(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores0(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores0(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select
		operation="Randomize row order"
		cso=cstate_operation(11,12,operation,arg())
		if nba=1 then
			cstate_nba(12,13,l,s)
			for j=1 to l
				cstate(12,j)=cstate(13,j)
			next j
		end if
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		'randomize 12345
		select case m
			case 1:scores1(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores1(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores1(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores1(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores1(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores1(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores1(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores1(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores1(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores1(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores1(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores1(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores1(i)=m_ngrams(cipout(),l,0)
			case 14:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores1(i)=m_slope(cipout(),l,s,0)
			case 16:scores1(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores1(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores1(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores1(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores1(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores1(i)=m_slope(cipout(),l,s,0)
			case 22:scores1(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores1(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores1(i)=m_reversal(cipout(),l,s)
			case 25:scores1(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores1(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores1(i)=m_npairs(cipout(),l,2)
			case 28:scores1(i)=m_npairs(cipout(),l,3)
			case 29:scores1(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores1(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores1(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores1(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores1(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores1(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores1(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores1(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select
		operation="Randomize column order"
		cso=cstate_operation(11,12,operation,arg())
		if left(cso,5)<>"Error" then
			for j=1 to l
				cipout(j)=cstate(12,j)
			next j
		end if
		'randomize 12345
		select case m
			case 1:scores2(i)=m_fastbigrams(cipout(),l,s)
			case 2:scores2(i)=m_bigrams(cipout(),l,s,2)
			case 3:scores2(i)=m_fastbigrams_alphabet(cipout(),l,s,bmod)
			case 4:scores2(i)=m_asymmetry(cipout(),l,s,norm)
			case 5:scores2(i)=m_2cycles(cipout(),l,s,5)
			case 6:scores2(i)=m_3cycles(cipout(),l,s,5)
			case 7:scores2(i)=m_sequential(cipout(),l,s,1,0)
			case 8:scores2(i)=m_appearance(cipout(),l,s,1,0)
			case 9:scores2(i)=m_unigrams(cipout(),l,s,dx,0)
			case 10:scores2(i)=m_unigrams(cipout(),l,s,dx,1)
			case 11:scores2(i)=m_midpointshift(cipout(),l,s,0)
			case 12:scores2(i)=m_depth(cipout(),l,s,0,0,0)
			case 13:scores2(i)=m_ngrams(cipout(),l,0)
			case 14:scores2(i)=m_smoothness(cipout(),l,s,0)
			case 15:scores2(i)=m_slope(cipout(),l,s,0)
			case 16:scores2(i)=m_posshift2(cipout(),l,s,sqr(l))*10000
			case 17:scores2(i)=m_bigrams(cipout(),l,s,4)
			case 18:scores2(i)=m_unigramdistance(cipout(),l,s)
			case 19:scores2(i)=m_depth(cipout(),l,s,0,2,0)
			case 20:scores2(i)=m_depth(cipout(),l,s,0,1,0)
			case 21:scores2(i)=m_slope(cipout(),l,s,0)
			case 22:scores2(i)=m_smoothness(cipout(),l,s,0)
			case 23:scores2(i)=m_deep(cipout(),l,s,depth,0,0)
			case 24:scores2(i)=m_reversal(cipout(),l,s)
			case 25:scores2(i)=m_fasttrigrams(cipout(),l,s)
			case 26:scores2(i)=m_ngramfragments(cipout(),l,s,5)
			case 27:scores2(i)=m_npairs(cipout(),l,2)
			case 28:scores2(i)=m_npairs(cipout(),l,3)
			case 29:scores2(i)=m_primephobia(cipout(),l,s,0)
			case 30:scores2(i)=m_2cycles_perfect(cipout(),l,s)
			case 31:scores2(i)=m_3cycles_perfect(cipout(),l,s)
			case 32:scores2(i)=m_shortestsubstring(cipout(),l,s)
			case 33:scores2(i)=m_contactvariety(cipout(),l,s)*10000
			case 34:scores2(i)=m_2cyclespectrum(cipout(),l,s)*10000
			case 35:scores2(i)=m_lean(cipout(),l,s,dx,dy,0)
			case 36:scores2(i)=m_lean(cipout(),l,s,dx,dy,1)
		end select
		avg0+=scores0(i)
		avg1+=scores1(i)
		avg2+=scores2(i)
		if higher=0 then
			if val(format(scores0(i),"0.00"))<high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))<high1 then high1=val(format(scores1(i),"0.00"))
			if val(format(scores2(i),"0.00"))<high2 then high2=val(format(scores2(i),"0.00"))
		else
			if val(format(scores0(i),"0.00"))>high0 then high0=val(format(scores0(i),"0.00"))
			if val(format(scores1(i),"0.00"))>high1 then high1=val(format(scores1(i),"0.00"))
			if val(format(scores2(i),"0.00"))>high2 then high2=val(format(scores2(i),"0.00"))
		end if
		if stoptask=1 then
			stoptask=0
			stats_running=0
			ui_editbox_settext(output_text,"")
			thread_ptr(threadsmax+2)=0
			exit sub
		end if
	next i
	avg0/=rnds
	avg1/=rnds
	avg2/=rnds
	os+=lb
	os+="Randomize order: (by characters, rows, columns)"+lb
	os+="---------------------------------------------------------"
	for i=1 to rnds
		operation="Trial "+str(i)
		os+=lb
		os+=operation+": "+format(scores0(i),"0.00")+", "+format(scores1(i),"0.00")+", "+format(scores2(i),"0.00")
		if sigma=1 then os+=" ("+format((scores0(i)-mean)/sd,"0.00")+", "+format((scores1(i)-mean)/sd,"0.00")+", "+format((scores2(i)-mean)/sd,"0.00")+")"
		if val(format(scores0(i),"0.00"))=high0 or val(format(scores1(i),"0.00"))=high1 or val(format(scores2(i),"0.00"))=high2 then os+=" <---"
	next i
	os+=lb
	os+="---------------------------------------------------------"+lb
	os+="Characters average: "+format(avg0,"0.00")+lb
	os+="Rows average: "+format(avg1,"0.00")+lb
	os+="Columns average: "+format(avg2,"0.00")
	
	stats_running=0
	ui_editbox_settext(output_text,os)
	thread_ptr(threadsmax+2)=0
	randomize timer

end sub

sub stats_keywordlength(byval tn_ptr as any ptr)
	
	stats_running=1
	randomize 12345
	dim as string os,t,ot
	dim as double avg(100)
	dim as double mtimer=timer
	dim as integer i,j,k,c
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		stats_running=0
		thread_ptr(threadsmax+2)=0
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer avgcnt,divd=2,dl
	dim as integer total=(l\divd)-1
	dim as long cip(l)
	dim as long cipr(l)
	dim as double avg1,scores(l\divd),scores2(l\divd)
	dim as integer sdsc=1000
	dim as double sds(sdsc)
	dim as double high1,low1=999999999
	dim as double high2,low2=999999999
	dim as double a
	for i=1 to l
		cip(i)=nuba(i)
		cipr(i)=cip(i)
	next i
	os+="BHdecrypt vigenère keyword length stats for: "+file_name+lb
	os+="---------------------------------------------------------"
	i=1
	do
		i+=1:c+=1
		if timer-mtimer>1 then
			mtimer=timer
			ot="Vigenère keyword length: "+format((c/total)*100,"0.00")+"% complete"+lb
			ot+="(click stop task to cancel)"
			ui_editbox_settext(output_text,ot)
		end if
		for j=1 to sdsc
			for k=1 to l*2
				swap cipr(int(rnd*l)+1),cipr(int(rnd*l)+1)
			next k
			sds(j)=m_posshift(cipr(),l,s,i,0)
		next j
		a=m_posshift(cip(),l,s,i,0)
		scores(i)=stdev(a,sdsc,sds())
		if scores(i)>high1 then high1=scores(i)
		if scores(i)<low1 then low1=scores(i)
		if stoptask=1 then stoptask=0:exit do
	loop until stop_measurement=1 or i=l\divd
	dl=i
	for i=2 to dl
		os+=lb
		os+="Length "+str(i)+": (sigma: "+format(scores(i),"0.00")+")"
		if high1=scores(i) then os+=" <---"
		if low1=scores(i) then os+=" <---"
	next i
	os+=lb
	os+="---------------------------------------------------------"
	for i=2 to dl\2
		avg1=0
		avgcnt=0
		for j=i to dl
			if j mod i=0 then
				avg1+=scores(j)
				avgcnt+=1
			end if
		next j
		scores2(i)=(avg1/avgcnt)
		if scores2(i)>high2 then high2=scores2(i)
		if scores2(i)<low2 then low2=scores2(i)
	next i
	for i=2 to dl\2
		os+=lb
		os+="Lengths modulo "+str(i)+"=0: (sigma: "+format(scores2(i),"0.00")+")"
		if high2=scores2(i) then os+=" <---"
		if low2=scores2(i) then os+=" <---"
	next i
	stop_measurement=0
	stats_running=0
	ui_editbox_settext(output_text,os)
	thread_ptr(threadsmax+2)=0
	randomize timer

end sub

sub stats_compare_keymapping
	
	randomize 12345
	
	dim as string os,s
	dim as integer h,i,j,k,x,y,e,dx,dy,hi
	dim as string soi
	soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as long cip1i(constcip)
	dim as long cip1n(constcip)
	dim as long sym1(constcip)
	dim as integer l1=info_length
	dim as integer s1=info_symbols
	dim as integer dx1=info_x
	dim as integer dy1=info_y
	dim as integer num1=info_numerical
	for i=1 to l1
		cip1i(i)=info(i)
		cip1n(i)=nuba(i)
		sym1(nuba(i))=info(i)
		if cip1i(i)>hi then hi=cip1i(i)
	next i
	soi=string_to_info(ui_editbox_gettext(output_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as long cip2i(constcip)
	dim as long cip2n(constcip)
	dim as long sym2(constcip)
	dim as integer l2=info_length
	dim as integer s2=info_symbols
	dim as integer dx2=info_x
	dim as integer dy2=info_y
	dim as integer num2=info_numerical
	if num1<>num2 then
		ui_editbox_settext(output_text,"Error: format mismatch")
		exit sub
	end if
	for i=1 to l2
		cip2i(i)=info(i)
		cip2n(i)=nuba(i)
		sym2(nuba(i))=info(i)
		if cip2i(i)>hi then hi=cip2i(i)
	next i
	dim as integer lmin,lmax
	dim as integer smin,smax
	if l1>l2 then 
		lmax=l1
		lmin=l2
	else 
		lmax=l2
		lmin=l1
	end if
	if s1>s2 then 
		smax=s1
		smin=s2
	else 
		smax=s2
		smin=s1
	end if
	
	os+="BHdecrypt compare key mapping stats for: "+file_name+lb 
	os+="---------------------------------------------------------"+lb
	os+=lb
	dim as short id1(smax)
	dim as short key1(smax,lmax,2)
	dim as long cycle1(lmax)
	dim as integer cs1,cst1
	os+="Input to output key:"+lb
	os+="---------------------------------------------------------" '+lb
	for i=1 to lmin
		key1(cip1n(i),0,0)+=1
		key1(cip1n(i),0,1)=cip1n(i)
		key1(cip1n(i),key1(cip1n(i),0,0),0)=i 'position
		key1(cip1n(i),key1(cip1n(i),0,0),2)=cip2n(i)
	next i
	for i=1 to s1
		if key1(i,0,0)>0 then
			os+=lb
			if num1=0 then
				os+=chr(sym1(key1(i,0,1)))+": "
			else
				os+=str(sym1(key1(i,0,1)))+": "	
			end if
			for j=1 to key1(i,0,0)
				cycle1(j)=key1(i,j,2)
				if num1=0 then
					os+=chr(sym2(key1(i,j,2)))
				else
					os+=str(sym2(key1(i,j,2)))	
				end if
				if j<>key1(i,0,0) then 
					if num1=1 then
						os+=","
					end if
				end if
			next j
			cs1=m_unicycle(cycle1(),key1(i,0,0))
			cst1+=cs1
			os+=" ("+str(cs1)+")"
		end if
	next i
	os+=lb
	os+="--------------------"+lb
	os+="Cycle score: "+format(cst1,"0.00")
	os+=lb
	
	os+=lb
	dim as short id2(smax)
	dim as short key2(smax,lmax,2)
	dim as long cycle2(lmax)
	dim as long cycle2p(lmax)
	dim as integer cs2,cst2
	os+="Output to input key:"+lb
	os+="---------------------------------------------------------" '+lb
	for i=1 to lmin
		key2(cip2n(i),0,0)+=1
		key2(cip2n(i),0,1)=cip2n(i)
		key2(cip2n(i),key2(cip2n(i),0,0),0)=i 'position
		key2(cip2n(i),key2(cip2n(i),0,0),2)=cip1n(i)
	next i
	for i=1 to s2
		if key2(i,0,0)>0 then
			os+=lb
			if num1=0 then
				os+=chr(sym2(key2(i,0,1)))+": "
			else
				os+=str(sym2(key2(i,0,1)))+": "	
			end if
			for j=1 to key2(i,0,0)
				cycle2(j)=key2(i,j,2)
				cycle2p(j)=key2(i,j,0) 'position
				if num1=0 then
					os+=chr(sym1(key2(i,j,2)))
				else
					os+=str(sym1(key2(i,j,2)))	
				end if
				if j<>key2(i,0,0) then 
					if num1=1 then
						os+=","
					end if
				end if
			next j
			cs2=m_unicycle(cycle2(),key2(i,0,0))
			cst2+=cs2
			os+=" ("+str(cs2)+")"
			
			dim as short idnew1(constcip)
			dim as integer cs=0,al=0
			erase idnew1
			for j=1 to key2(i,0,0)
				if idnew1(cycle2(j))=0 then
					cs+=1
					idnew1(cycle2(j))=1
				end if
			next j
			if cs>1 then
				for h=1 to key2(i,0,0)-(cs-1)
					for j=h to h+(cs-2)
						for k=j+1 to h+(cs-1)
							if cycle2(j)=cycle2(k) then 
								for e=cycle2p(j) to cycle2p(k)
									cmap(e)+=1
								next e
								'cmap(cycle2p(k))+=1
							end if
						next k
					next j
				next h
			end if
			
		end if
	next i
	os+=lb
	os+="--------------------"+lb
	os+="Cycle score: "+format(cst2,"0.00")
	
	ui_editbox_settext(output_text,os)
	randomize timer

end sub

sub stats_compare_equalitytest
	
	randomize 12345
	
	dim as string os,s,os2
	dim as integer h,i,j,k,x,y,e,dx,dy,hi
	dim as string soi
	soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as long cip1i(constcip)
	dim as long cip1n(constcip)
	dim as long sym1(constcip)
	dim as integer l1=info_length
	dim as integer s1=info_symbols
	dim as integer dx1=info_x
	dim as integer dy1=info_y
	dim as integer num1=info_numerical
	for i=1 to l1
		cip1i(i)=info(i)
		cip1n(i)=nuba(i)
		sym1(nuba(i))=info(i)
		if cip1i(i)>hi then hi=cip1i(i)
	next i
	soi=string_to_info(ui_editbox_gettext(output_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as long cip2i(constcip)
	dim as long cip2n(constcip)
	dim as long sym2(constcip)
	dim as integer l2=info_length
	dim as integer s2=info_symbols
	dim as integer dx2=info_x
	dim as integer dy2=info_y
	dim as integer num2=info_numerical
	if num1<>num2 then
		ui_editbox_settext(output_text,"Error: format mismatch")
		exit sub
	end if
	for i=1 to l2
		cip2i(i)=info(i)
		cip2n(i)=nuba(i)
		sym2(nuba(i))=info(i)
		if cip2i(i)>hi then hi=cip2i(i)
	next i
	dim as integer lmin,lmax
	dim as integer smin,smax
	if l1>l2 then 
		lmax=l1
		lmin=l2
	else 
		lmax=l2
		lmin=l1
	end if
	if s1>s2 then 
		smax=s1
		smin=s2
	else 
		smax=s2
		smin=s1
	end if
	
	os+="BHdecrypt compare equality test stats for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	
	dim as long c1rnd(l1)
	dim as long c2rnd(l2)
	dim as double a,sda(1000)
	
	for i=1 to l2
		c2rnd(i)=cip2n(i)
	next i
	a=m_equality(cip1n(),cip2n(),l1,l2,s1,s2,0)
	os+="Input is "+format(a,"0.00")+"% a set of output. ("
	for i=1 to 1000
		for j=1 to l2*10
			swap c2rnd(int(rnd*l2)+1),c2rnd(int(rnd*l2)+1)		
		next j
		sda(i)=m_equality(cip1n(),c2rnd(),l1,l2,s1,s2,0)
	next i
	os+=format(stdev(a,1000,sda()),"0.00")+")"+lb
	
	for i=1 to l1
		c1rnd(i)=cip1n(i)
	next i
	a=m_equality(cip1n(),cip2n(),l1,l2,s1,s2,1)
	os+="Output is "+format(a,"0.00")+"% a set of input. ("
	for i=1 to 1000
		for j=1 to l1*10
			swap c1rnd(int(rnd*l1)+1),c1rnd(int(rnd*l1)+1)		
		next j
		sda(i)=m_equality(c1rnd(),cip2n(),l1,l2,s1,s2,1)
	next i
	os+=format(stdev(a,1000,sda()),"0.00")+")"+lb
	
	if dx1>dx2 then dx=dx1 else dx=dx2
	if dy1>dy2 then dy=dy1 else dy=dy2
	dim as integer gr1(dx,dy),gr2(dx,dy)
	dim as long match1
	i=0
	for y=1 to dy1
		for x=1 to dx1
			i+=1
			gr1(x,y)=cip1i(i)
		next x
	next y
	i=0
	for y=1 to dy2
		for x=1 to dx2
			i+=1
			gr2(x,y)=cip2i(i)
		next x
	next y
	'if dx1<dx2 then dx=dx1 else dx=dx2
	'if dy1<dy2 then dy=dy1 else dy=dy2
	os2=""
	for y=1 to dy
		for x=1 to dx
			if gr1(x,y)>0 andalso gr2(x,y)>0 then
				if gr1(x,y)=gr2(x,y) then
					match1+=1
					if num1=0 then
						os2+=chr(gr1(x,y))
					else
						os2+=str(gr1(x,y))
						if x<>dx then os+=space(hi-len(str(gr1(x,y))))
					end if
				else
					if num1=0 then
						os2+=" "
					else
						os2+=space(hi)
					end if
				end if
			end if
		next x
		os2+=lb
	next y
	
	hi=len(str(hi))+1
	os+=lb
	os+="Grid matches: "+str(match1)+" ("+format((match1/l1)*100,"0.00")+"% accurate)"+lb
	os+="---------------------------------------------------------"+lb
	os+=os2
	
	hi=len(str(hi))+1
	os+=lb
	os+="Grid changes:"+lb
	os+="---------------------------------------------------------"+lb
	'if dx1>dx2 then dx=dx1 else dx=dx2
	'if dy1>dy2 then dy=dy1 else dy=dy2
	i=0
	for y=1 to dy1
		for x=1 to dx1
			i+=1
			gr1(x,y)=cip1i(i)
		next x
	next y
	i=0
	for y=1 to dy2
		for x=1 to dx2
			i+=1
			gr2(x,y)=cip2i(i)
		next x
	next y
	'if dx1<dx2 then dx=dx1 else dx=dx2
	'if dy1<dy2 then dy=dy1 else dy=dy2
	for y=1 to dy
		for x=1 to dx
			if gr1(x,y)>0 andalso gr2(x,y)>0 then
				if gr1(x,y)<>gr2(x,y) then
					if num1=0 then
						os+=chr(gr1(x,y))
					else
						os+=str(gr1(x,y))
						if x<>dx then os+=space(hi-len(str(gr1(x,y))))
					end if
				else
					if num1=0 then
						os+=" "
					else
						os+=space(hi)
					end if
				end if
			end if
		next x
		if y<>dy then os+=lb
	next y
	
	ui_editbox_settext(output_text,os)
	randomize timer
	
end sub

sub stats_compare_kasiskeexamination
	
	randomize 12345
	
	dim as string os,s
	dim as integer h,i,j,k,x,y,e,dx,dy,hi
	dim as string soi
	soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as long cip1i(constcip)
	dim as long cip1n(constcip)
	dim as long sym1(constcip)
	dim as integer l1=info_length
	dim as integer s1=info_symbols
	dim as integer dx1=info_x
	dim as integer dy1=info_y
	dim as integer num1=info_numerical
	for i=1 to l1
		cip1i(i)=info(i)
		cip1n(i)=nuba(i)
		sym1(nuba(i))=info(i)
		if cip1i(i)>hi then hi=cip1i(i)
	next i
	soi=string_to_info(ui_editbox_gettext(output_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as long cip2i(constcip)
	dim as long cip2n(constcip)
	dim as long sym2(constcip)
	dim as integer l2=info_length
	dim as integer s2=info_symbols
	dim as integer dx2=info_x
	dim as integer dy2=info_y
	dim as integer num2=info_numerical
	if num1<>num2 then
		ui_editbox_settext(output_text,"Error: format mismatch")
		exit sub
	end if
	for i=1 to l2
		cip2i(i)=info(i)
		cip2n(i)=nuba(i)
		sym2(nuba(i))=info(i)
		if cip2i(i)>hi then hi=cip2i(i)
	next i
	dim as integer lmin,lmax
	dim as integer smin,smax
	if l1>l2 then 
		lmax=l1
		lmin=l2
	else 
		lmax=l2
		lmin=l1
	end if
	if s1>s2 then 
		smax=s1
		smin=s2
	else 
		smax=s2
		smin=s1
	end if
	
	dim as integer khi
	dim as integer count1(constcip)
	for i=0 to lmax-1
		k=i
		for j=1 to lmax
			k+=1
			if k>l2 then k=1
			if cip1i(j)=cip2i(k) then count1(i)+=1
		next j
		if i>0 andalso count1(i)>khi then khi=count1(i)
	next i
	
	os+="BHdecrypt compare kasiski examination stats for: "+file_name+lb
	os+="---------------------------------------------------------"
	for i=0 to lmax-1
		os+=lb
		os+="Offset "+str(i)+": "+str(count1(i))
		if khi=count1(i) then os+=" <---"
	next i
	
	os+=lb
	os+=lb
	os+="Kasiski examination (grid offset):"+lb
	os+="---------------------------------------------------------"
	dim as double arg(100),scr1,k1_hi,k2_hi 'dx,dy
	dim as long newcip(identmax) 'lmax
	for i=1 to l1
		cstate(11,i)=cip1i(i)	
	next i
	for y=1 to dy1-1
		for x=1 to dx1-1
			arg(1)=l1
			arg(2)=s1
			arg(3)=dx1
			arg(4)=dy1
			arg(5)=1
			arg(7)=x:cstate_operation(11,12,"Offset column order",arg())
			arg(7)=y:cstate_operation(12,13,"Offset row order",arg())
			for i=1 to l1
				newcip(i)=cstate(13,i)
			next i
			scr1=m_gridmatch(newcip(),cip2i(),dx1,dy1,dx2,dy2,0)
			if scr1>k1_hi then k1_hi=scr1
			os+=lb
			os+="Left "+str(x)+" down "+str(y)+": "+str(scr1)
			if scr1=k1_hi then os+=" <---"
		next x
		'if x<>dx1 andalso y<>dy1 andalso scr1(x,y)>k1_hi then k1_hi=scr1(x,y)
	next y
	
	ui_editbox_settext(output_text,os)
	randomize timer

end sub

sub stats_periodic(byval m as integer)
	
	dim as string os
	dim as integer i,j,k,o,nl0,nl1,ns0,ns1
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as long cip0(l),cip1(l),starto,endo
	dim as double frq1(s),frq2(s)
	dim as double avg,score(l,1),diff
	dim as double avg0,avg1
	dim as long nba(l)
	for i=1 to l
		nba(i)=nuba(i)
		frq2(nba(i))+=1
	next i
	for i=1 to s
		frq2(i)/=l
	next i
	select case m
		case 1:os+="BHdecrypt periodic raw IOC stats for: "
		case 2:os+="BHdecrypt periodic 2-symbol cycles stats for: "
		case 3:os+="BHdecrypt periodic symbols stats for: "
		case 4:os+="BHdecrypt periodic midpoint shift stats for: "
		case 5:os+="BHdecrypt periodic unigram distance stats for: "
		case 6:os+="BHdecrypt periodic unique unigrams stats for: "
		case 7:os+="BHdecrypt periodic exclusive unigrams stats for: "
		case 8:os+="BHdecrypt periodic unique unigrams versus stats for: "
		case 9:os+="BHdecrypt periodic exclusive unigrams versus stats for: "
		case 10:os+="BHdecrypt periodic perfect 2-symbol cycles stats for: "
		case 11:os+="BHdecrypt periodic 3-symbol cycles stats for: "
		case 12:os+="BHdecrypt periodic perfect 3-symbol cycles stats for: "
		case 13:os+="BHdecrypt periodic bigrams stats for: " 
		case 14:os+="BHdecrypt periodic self-compare chi^2 stats for: "
	end select
	os+=file_name+lb
	os+="---------------------------------------------------------"+lb
	for i=1 to l\4
		nl0=0
		for o=1 to i
			
			for j=1 to (l/i)
				nl0+=1
				if nl0>l then
					j+=1
					exit for
				end if
				cip0(j)=nba(nl0)
			next j
			
			ns0=nba_to_info_out(cip0(),j-1,s)
			select case m
				case 1:score(o,0)=m_ioc3(info_out(),j-1,ns0,0)
				case 2:score(o,0)=int(m_2cycles(info_out(),j-1,ns0,5))
				case 3:score(o,0)=ns0
				case 4:score(o,0)=int(m_midpointshift(info_out(),j-1,ns0,0))
				case 5:score(o,0)=m_unigramdistance(info_out(),j-1,ns0)
				case 6:score(o,0)=m_unigramperiodic(nba(),l,s,0,0,i,o)
				case 7:score(o,0)=m_unigramperiodic(nba(),l,s,0,1,i,o)
				case 8:score(o,0)=m_unigramperiodicvs(nba(),l,s,0,0,i,o)
				case 9:score(o,0)=m_unigramperiodicvs(nba(),l,s,0,1,i,o)
				case 10:score(o,0)=m_2cycles_perfect(info_out(),j-1,ns0)
				case 11:score(o,0)=int(m_3cycles(info_out(),j-1,ns0,5))
				case 12:score(o,0)=m_3cycles_perfect(info_out(),j-1,ns0)
				case 13:score(o,0)=m_fastbigrams(info_out(),j-1,ns0)
				case 14
					for k=0 to s
						frq1(k)=0
					next k
					for k=1 to j-1
						frq1(cip0(k))+=1
					next k
					score(o,0)=int(m_chi2(frq1(),frq2(),s,j-1))
			end select
			avg0+=score(o,0)
			
			nl1=0
			for j=o to l step i
				if j>l then exit for
				nl1+=1
				cip1(nl1)=nba(j)
			next j
			
			ns1=nba_to_info_out(cip1(),nl1,s)
			select case m
				case 1:score(o,1)=m_ioc3(info_out(),nl1,ns1,0)
				case 2:score(o,1)=int(m_2cycles(info_out(),nl1,ns1,5))
				case 3:score(o,1)=ns1
				case 4:score(o,1)=int(m_midpointshift(info_out(),nl1,ns1,0))
				case 5:score(o,1)=m_unigramdistance(info_out(),nl1,ns1)
				case 6:score(o,1)=m_unigramperiodic(nba(),l,s,1,0,i,o)
				case 7:score(o,1)=m_unigramperiodic(nba(),l,s,1,1,i,o)
				case 8:score(o,1)=m_unigramperiodicvs(nba(),l,s,1,0,i,o)
				case 9:score(o,1)=m_unigramperiodicvs(nba(),l,s,1,1,i,o)
				case 10:score(o,1)=m_2cycles_perfect(info_out(),nl1,ns1)
				case 11:score(o,1)=int(m_3cycles(info_out(),nl1,ns1,5))
				case 12:score(o,1)=m_3cycles_perfect(info_out(),nl1,ns1)
				case 13:score(o,1)=m_fastbigrams(info_out(),nl1,ns1)
				case 14
					for k=0 to s
						frq1(k)=0
					next k
					for k=1 to nl1
						frq1(cip1(k))+=1
					next k
					score(o,1)=int(m_chi2(frq1(),frq2(),s,nl1))
			end select
			avg1+=score(o,1)
			
		next o
		'if avg0>0 andalso avg1>0 then
			if i>1 then
				os+=lb
				'os+=lb
			end if
			os+="Period "+str(i)+":"
			for o=1 to i
				os+=lb
				os+="- Row/column "+str(o)+": "+str(score(o,0))+", "+str(score(o,1))
			next o
			os+=" ("+format((avg0/avg1)*100,"0.00")+"%)"
		'end if	
		avg0=0
		avg1=0
	next i	
	ui_editbox_settext(output_text,os)
	randomize timer

end sub

sub stats_outputgraphs(byval tn_ptr as any ptr)
	
	stats_running=1
	dim as integer e,h,i,j,k,x,y,old,xa,ya,a,b,c,r,p,utp
	dim as string os,soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer num=info_numerical
	dim as integer dx=info_x
	dim as integer dy=info_y
	
	dim as long cip(l)
	dim as long cip2(constcip)
	dim as long cip3(constcip)
	dim as integer nba(l)
	dim as long nms(l)
	dim as double dbl(constcip)
	dim as long gr1(dx,dy)
'	dim as short cv1(constcip)
'	dim as short cv2(constcip,constcip)
	dim as short frq(s)
	
	dim as string file_name2=remext(file_name)
	
	for i=1 to l
		cip(i)=info(i)
		nba(i)=nuba(i)
		dbl(i)=info(i)
	next i
	
	for i=1 to l
		frq(nba(i))+=1
	next i
	
	i=0
	for y=1 to dy
		for x=1 to dx
			i+=1
			gr1(x,y)=nba(i)
			if i=l then exit for,for
		next x
	next y
	
	os="BHdecrypt graphs and maps for: "+file_name2+lb
	os+="---------------------------------------------------------"+lb
	
	output_colormap(dbl(),cip(),l,dx,dy,0,file_name2+" Symbol map")
	os+="Created \Output\"+file_name2+" Symbol map.bmp"+lb
	
	for i=1 to l
		dbl(i)=frq(nba(i))
		'nms(i)=dbl(i)
	next i
	output_colormap(dbl(),cip(),l,dx,dy,0,file_name2+" Symbol frequency map")
	os+="Created \Output\"+file_name2+" Symbol frequency map.bmp"+lb
	
	mkdir basedir+"\Output\Symbol map modulo graphs\"
	for i=2 to 10
		for j=1 to l
			dbl(j)=cip(j)mod i
		next j
		output_colormap(dbl(),cip(),l,dx,dy,0,"\Symbol map modulo graphs\"+file_name2+" Symbol map modulo "+str(i))
		os+="Created \Output\Symbol map modulo graphs\"+file_name2+" Symbol map modulo "+str(i)+".bmp"+lb
	next i
	
	mkdir basedir+"\Output\Symbol positions graphs\"
	for i=1 to s
		for j=1 to l
			if nba(j)=i then dbl(j)=1 else dbl(j)=0
		next j
		output_colormap(dbl(),cip(),l,dx,dy,0,"\Symbol positions graphs\"+file_name2+" Symbol "+str(i))
		os+="Created \Output\Symbol positions graphs\"+file_name2+" Symbol "+str(i)+".bmp"+lb
	next i
	
	k=0
	erase graph
	dim as short id(65536)
	for i=1 to l
		k=0
		erase id
		for j=i to l
			if id(nba(j))=0 then
				k+=1
				id(nba(j))=1
			else
				exit for
			end if
		next j
		dbl(i)=k
		nms(i)=k
		graph(1,1,k)+=1
		if k>h then h=k
	next i
	
	old=info_numerical
	info_numerical=1
	output_colormap(dbl(),nms(),l,dx,dy,0,file_name2+" Unique sequences map")
	info_numerical=old
	os+="Created \Output\"+file_name2+" Unique sequences map.bmp"+lb
	
	output_graph(1,1,"Unique sequences: "+str(file_name),file_name2+" Unique sequences graph")
	os+="Created \Output\"+file_name2+" Unique sequences graph.bmp"+lb
	
	mkdir basedir+"\Output\Contact variety graphs\"
	for r=1 to 10	
		i=0
		k=r*2:h=k/2
		for y=1 to dy
			for x=1 to dx
				a=0
				i+=1
				for ya=0 to k
					for xa=0 to k
						if x+(xa-h)=x andalso y+(ya-h)=y then
						else
							if x+(xa-h)>0 andalso x+(xa-h)<=dx then
								if y+(ya-h)>0 andalso y+(ya-h)<=dy then
									if gr1(x+(xa-h),y+(ya-h))>0 then
										a+=1
										cv1(gr1(x+(xa-h),y+(ya-h)))+=1
										cv2(nba(i),gr1(x+(xa-h),y+(ya-h)))+=1
									end if
								end if
							end if
						end if
					next xa
				next ya
				b=0
				for j=1 to s
					if cv1(j)>0 then b+=1
					cv1(j)=0
				next j
				dbl(i)=abs(((b/a)*100)-100)
				if i=l then exit for,for
			next x
		next y	
		output_colormap(dbl(),cip(),l,dx,dy,0,"\Contact variety graphs\"+file_name2+" Contact variety by position reach "+str(r))
		os+="Created \Output\Contact variety graphs\"+file_name2+" Contact variety by position reach "+str(r)+".bmp"+lb
		
		for i=1 to l
			a=0
			b=0
			for j=1 to s
				if cv2(nba(i),j)>0 then b+=1
				a+=cv2(nba(i),j)
			next j
			dbl(i)=abs(((b/a)*100)-100)
		next i
		output_colormap(dbl(),cip(),l,dx,dy,0,"\Contact variety graphs\"+file_name2+" Contact variety by symbol reach "+str(r))
		os+="Created \Output\Contact variety graphs\"+file_name2+" Contact variety by symbol reach "+str(r)+".bmp"
		if r<>10 then os+=lb
		erase cv1,cv2
		
	next r
	
	stop_measurement=0
	stats_running=0
	ui_editbox_settext(output_text,os)
	thread_ptr(threadsmax+2)=0
	randomize timer

end sub