function m_ioc(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
	
	if l=0 then return 0
	if l=s then return 0
	dim as double score=0
	dim as integer i
	for i=1 to s
		if array(i)>1 then score+=array(i)*(array(i)-1)
	next i
	select case n
		case 0
		case 1
			score/=l*(l-1)	
		case 2
			score=(((l/s)*((l/s)-1))*s)/score
	end select
	return score

end function

function m_ioc2(array()as short,byval l as integer,byval s as integer,byval n as integer)as double
	
	if l=0 then return 0
	if l=s then return 0
	dim as double score
	dim as integer i
	dim as short frq(s)
	for i=1 to l
		frq(array(i))+=1
	next i
	for i=1 to s
		score+=ioctable(frq(i)) 'frq(i)*(frq(i)-1)
	next i
	select case n
		case 0
		case 1
			score/=l*(l-1)	
		case 2
			score=(((l/s)*((l/s)-1))*s)/score
	end select
	return score

end function

function m_ioc3(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
	
	if l=0 then return 0
	if l=s then return 0
	dim as double score
	dim as integer i
	dim as short frq(s)
	for i=1 to l
		frq(array(i))+=1
	next i
	for i=1 to s
		score+=ioctable(frq(i)) 'frq(i)*(frq(i)-1)
	next i
	select case n
		case 0
		case 1
			score/=l*(l-1)	
		case 2
			score=(((l/s)*((l/s)-1))*s)/score
	end select
	return score
	
end function

function m_highestfrequency(array()as long,byval l as integer,byval s as integer)as double
	
	if l=0 then return 0
	if l=s then return 0
	dim as double score
	dim as integer i
	dim as short frq(s)
	for i=1 to l
		frq(array(i))+=1
	next i
	for i=1 to s
		if frq(i)>score then score=frq(i)
	next i
	return score
	
end function

function m_entropy(array()as long,byval l as integer,byval s as integer)as double
	
	if l=0 then return 0
	dim as double score
	dim as integer i
	for i=1 to s
		if array(i)>0 then score+=logbx(array(i)/l,2)*(array(i)/l)
	next i
	return abs(score)
	
end function

function m_equality(array1()as long,array2()as long,byval l1 as integer,byval l2 as integer,byval s1 as integer,byval s2 as integer,byval o as integer)as double
	
	dim as integer i,j,im,lmin
	dim as long ab(s1,s2)
	dim as double score1,score2
	
	if l1>l2 then lmin=l2 else lmin=l1
	
	for i=1 to lmin
		ab(array1(i),array2(i))+=1
	next i
	
	if o=0 then
		for i=1 to s1
			im=0
			for j=1 to s2
				if ab(i,j)>im then im=ab(i,j)
			next j
			score1+=im
		next i
		return (score1/lmin)*100
	else
		for i=1 to s2
			im=0
			for j=1 to s1
				if ab(j,i)>im then im=ab(j,i)
			next j
			score2+=im
		next i
		return (score2/lmin)*100
	end if

end function

function m_flatness(array()as long,byval l as integer,byval s as integer,byval n as integer)as double

	if l=0 then return 0
	if l=s then return 1
	if s=1 then return 0
	dim as double mf,score
	dim as double ls=l/s
	dim as integer i,j
	dim as integer frq(s)
	for i=1 to l
		frq(array(i))+=1
	next i
	
	for i=1 to s
		score+=abs(frq(i)-ls)
	next i
	if n=0 then
		return score
	else
		mf+=(s-1)*(abs(1-ls))
		mf+=abs((l-(s-1))-ls)
		return abs((score/mf)-1)
	end if
			
end function

function m_smoothness(array()as long,byval l as integer,byval s as integer,byval n as integer)as double

	if l=0 then return 0
	dim as integer i,e
	dim as double a,b
	for i=1 to l-1
		a+=abs(array(i)-array(i+1))
	next i
	if n=0 then
		return a
	else
		dim as double sorted_array(l)
		for i=1 to l
			sorted_array(i)=array(i)
		next i
		do
			e=0
			for i=1 to l-1
				if sorted_array(i)>sorted_array(i+1) then
					e=1
					swap sorted_array(i),sorted_array(i+1)
				end if
			next i			
		loop until e=0
		for i=1 to l-1
			b+=abs(sorted_array(i)-sorted_array(i+1))
		next i
		if a>0 then
			return b/a
		else
			return 1
		end if
	end if
			
end function

function m_midpointshift(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
	
	if l=0 then return 0
	dim as double a,b
	dim as integer i,j,e
	dim as integer pos1(s,1)
	for i=1 to l
		pos1(array(i),0)+=1
		pos1(array(i),1)+=i
	next i
	for i=1 to s
		'if pos1(i,0)>0 then
			if l mod 2=0 then
				a+=abs(((pos1(i,1)/pos1(i,0))-((l+1)/2))*pos1(i,0))
			else
				a+=abs(((pos1(i,1)/pos1(i,0))-(l/2))*pos1(i,0))
			end if
		'end if
	next i
	if n=0 then
		return a
	else
		dim as short sorted_array(l)
		for i=1 to l
			sorted_array(i)=array(i)
		next i
		dim as integer pos2(s,1)
		do
			e=0
			for i=1 to l-1
				if sorted_array(i)>sorted_array(i+1) then
					e=1
					swap sorted_array(i),sorted_array(i+1)
				end if
			next i
		loop until e=0
		for i=1 to l
			pos2(sorted_array(i),0)+=1
			pos2(sorted_array(i),1)+=i
		next i
		for i=1 to s
			if l mod 2=0 then
				b+=abs(((pos2(i,1)/pos2(i,0))-((l+1)/2))*pos2(i,0))
			else
				b+=abs(((pos2(i,1)/pos2(i,0))-(l/2))*pos2(i,0))
			end if
		next i
		return a/b
	end if

end function

function m_appearance(array()as long,byval l as integer,byval s as integer,byval o as integer,byval n as integer)as double
	
	if l=0 then return 0
	dim as double score1,score2
	dim as integer i,e,ns,score,ident(s),ident2(s)
	dim as integer sorted_array(l)
	for i=1 to l
		ident(array(i))+=1
		if ident(array(i))=o then
			ns+=1
			score1+=i
		end if
	next i
	select case n
	 	case 0:return score1
		case 1
			'for i=1 to l
			'	sorted_array(i)=array(i)
			'next i
			'do
			'	e=0
			'	for i=1 to l-1
			'		if sorted_array(i)>sorted_array(i+1) then
			'			e=1
			'			swap sorted_array(i),sorted_array(i+1)
			'		end if
			'	next i
			'loop until e=0
			'for i=1 to l
			'	ident2(sorted_array(i))+=1
			'	if ident2(sorted_array(i))=o then
			'		score2+=i
			'	end if
			'next i
			'return score2-score1 'score1/score2
		case 2:return score1/ns
	end select

end function

function m_shortestsubstring(array()as long,byval l as integer,byval s as integer)as double
	
	dim as integer i,j,k,p,sh=l
	dim as long ident(s)
	dim as long sum=(s*(s+1))/2
	for i=1 to l
		k=0
		for j=i to l
			if ident(array(j))<i then
				ident(array(j))=i
				k+=array(j)
			end if
			if k=sum then
				if sh>j-(i-1) then
					p=i
					sh=j-(i-1)
				end if
			end if
		next j
	next i
	return p

end function

function m_sequential(array()as long,byval l as integer,byval s as integer,byval o as integer,byval n as integer)as double
	
	if l=0 then return 0
	'if l=s then return 1
	dim as double score ',subscore
	dim as integer a,i,j ',cnt
	dim as integer ident(s)
	for i=0 to l
		freq(i)=0
	next i
	for i=1 to l
		for j=i to l
			if ident(array(j))<o then
				a+=1
				score+=1
				ident(array(j))+=1
			else
				exit for
			end if	
		next j
		freq(a)+=1
		if a>freq(0) then freq(0)=a
		'score+=subscore
		'subscore=0
		'cnt+=1
		a=0
		for j=1 to s
			ident(j)=0
		next j
	next i
	select case n
		case 0
			return score '(score/cnt)*100
		case 1
			return score/(l*s-s*(s-1)/2)
		case 2
			dim as integer high
			for i=1 to freq(0)
				if freq(i)>high then 
					high=freq(i)
					a=val(str(i)+"0"+str(high)) '*high
				end if
			next i
			return a
			'dim as string suffix
			'select case len(str(freq(freq(0))))
			'	case 1:suffix="00"+str(freq(freq(0)))
			'	case 2:suffix="0"+str(freq(freq(0)))
			'	case 3:suffix=str(freq(freq(0)))
			'end select
			'return val(str(freq(0))+suffix)
		case is>2
			return freq(n)
	end select

end function

function m_depth(array()as long,byval l as integer,byval s as integer,byval d as integer,byval m as integer,byval n as integer)as double
	
	'd=depth
	'm=measurement
	'n=normalization
	
	dim as integer i,j,k,h
	dim as long cip1(l)
	dim as long cip2(l)
	dim as double high,high1,high2,score1,score2
	for i=1 to l-1 'periods
		for j=1 to i
			for k=j to l step i
				h+=1
				cip1(k)=array(h)
				cip2(h)=array(k)
			next k
		next j
		h=0
		if d=0 then
			select case m
				case 0
					score1=m_fastbigrams(cip1(),l,s)
					score2=m_fastbigrams(cip2(),l,s)
				case 1
					score1=m_bigrams(cip1(),l,s,2)
					score2=m_bigrams(cip2(),l,s,2)
					'score1=m_ngramfragments(cip1(),l)
					'score2=m_ngramfragments(cip2(),l)
				case 2
					score1=m_2cycles(cip1(),l,s,5)
					score2=m_2cycles(cip1(),l,s,5)
			end select
		else
			'd-=1
			'score1=m_deep(cip1(),l,s,d,m,n)
			'score2=m_deep(cip2(),l,s,d,m,n)
		end if
		if score1>high1 then high1=score1
		if score2>high2 then high2=score2
	next i
	if high1>high2 then high=high1 else high=high2
	return high

end function

function m_deep(array()as long,byval l as integer,byval s as integer,byval d as integer,byval m as integer,byval n as integer)as double
	
	'd=depth
	'm=measurement
	'n=normalization
	
	dim as integer i,j,k,h
	dim as long a1(l)
	dim as long a2(l)
	dim as double a1p(l-1)
	dim as double a2p(l-1)
	dim as double avg1
	dim as double avg2
	dim as double score
	for i=1 to l-1
		for j=1 to i
			for k=j to l step i
				h+=1
				a1(k)=array(h)
				a2(h)=array(k)
			next k
		next j
		h=0
		if d=0 then
			select case m
				case 0
					a1p(i)=m_fastbigrams(a1(),l,s)
					a2p(i)=m_fastbigrams(a2(),l,s)
				case 1
					a1p(i)=m_sequential(a1(),l,s,1,0)
					a2p(i)=m_sequential(a2(),l,s,1,0)
			end select
		else
			d-=1
			a1p(i)=m_deep(a1(),l,s,d,m,n)
			a2p(i)=m_deep(a2(),l,s,d,m,n)
		end if
		avg1+=a1p(i)
		avg2+=a2p(i)
	next i
	avg1/=(l-1)
	avg2/=(l-1)
	for i=1 to (l-1)
		score+=abs(a1p(i)-avg1)
		score+=abs(a2p(i)-avg2)
	next i
	return score 'score/(l-1)

end function

function m_ngrams(array()as long,byval l as integer,byval o as integer)as double
   
   dim as integer i,j,e,a,b,n,r,m
   dim as double score
   dim as short id(l,l)
   dim as short gram(l,l)
   dim as short arraycopy(l)
   for i=1 to l
      arraycopy(i)=array(i)
   next i
   for n=2 to 10 'l-1
      for i=1 to l-(n-1)
         a=arraycopy(i)
         b=arraycopy(i+1)
         gram(a,b)+=1
         if id(a,b)=0 then 
            e+=1
            id(a,b)=e
         end if
         arraycopy(i)=id(a,b)
         if gram(a,b)>1 then r+=1
      next i
      if o=0 then
      	if r>0 then score+=r*(2^(n-2))
      	if r<2 then return score
      else
     		if r=0 then return n-1
      end if
      for i=1 to l
      	for j=1 to l
      		id(i,j)=0
      		gram(i,j)=0
      	next j
      next i  
      e=0
      r=0
   next n
   if o=0 then return score else return n-1
   
end function

function m_repeats(cip()as long,byval l as short,byval num as short)as string
	
	'optimize
		
	dim as string s
	dim as integer m=15,h,i,j,k,c,t,r,e
	dim as short bp(m),bc(m),br(m)
	dim as ubyte reps(25,l),cip2(l),cip3(l)
	
	for i=1 to l
		cip2(i)=cip(i)
	next i
	
	for h=1 to m
		
		for i=1 to l
			c=0
			for j=1 to l-i
				if cip2(i+j)=cip2(j) then
					if cip2(j)<>0 then
						c+=1
						if c>1 andalso c>bc(h) then
							bc(h)=c
							bp(h)=j
						end if
					else
						c=0
					end if
				else
					c=0
				end if
			next j
		next i
		
		j=0
		for i=bp(h)-(bc(h)-1) to bp(h)
			j+=1
			cip3(j)=cip2(i)
			cip2(i)=0
		next i
		
		e=0
		for i=1 to r
			if reps(i,0)=bc(h) then
				e=1
				for j=1 to reps(i,0)
					if reps(i,j)<>cip3(j) then
						e=0
						exit for
					end if
				next j
			end if
			if e=1 then exit for
		next i
		if e=0 then
			r+=1
			br(h)=1
			reps(r,l)=h
			reps(r,0)=bc(h)
			for i=1 to bc(h)
				reps(r,i)=cip3(i)
			next i
		else
			bc(h)=0
			bp(h)=0
			br(h)=0
			h-=1
			br(reps(i,l))+=1
		end if
		
		t+=bc(h)
		if t=0 then return "No repeats found!"
		if bc(h)=0 or t+(h-1)>51 then 'do not exceed window width
			h-=1
			if h=0 then h=1
			exit for
		end if
		
	next h
	
	m=h
	for h=1 to m
		for i=bp(h)-(bc(h)-1) to bp(h)
			if num=0 then
				s+=chr(cip(i))
			else
				s+=str(cip(i))
				if i<>bp(h) then s+=" " else if h<m then s+=","
			end if
		next i
		if br(h)>1 then s+=" ("+str(br(h))+")"
		if h<m then s+=" "
		'if len(s)>=65 then exit for
	next h
	return "Repeats: "+s
	
end function

function m_ngrams_nba(cip()as long,byval l as short)as double
   
   dim as integer i,j,e,a,b,n,r,m
   dim as double score
   dim as short id1(255)
   dim as short id2(l,l)
   dim as short gram(l,l)
   dim as short copy(l)
   for i=1 to l
   	if id1(cip(i))=0 then
   		e+=1
   		id1(cip(i))=e
   		copy(i)=e
   	else
   		copy(i)=id1(cip(i))
   	end if
   next i
   e=0
   for n=2 to 10 'l-1
      for i=1 to l-(n-1)
         a=copy(i)
         b=copy(i+1)
         gram(a,b)+=1
         if id2(a,b)=0 then 
            e+=1
            id2(a,b)=e
         end if
         copy(i)=id2(a,b)
         if gram(a,b)>1 then r+=1
      next i
      if r>0 then score+=r*(2^(n-2))
      if r<2 then return score
      for i=1 to l
      	for j=1 to l
      		id2(i,j)=0
      		gram(i,j)=0
      	next j
      next i  
      e=0
      r=0
   next n
   return score
   
end function

function m_ngramfragments(array()as long,byval l as integer,byval s as integer,byval ngs as integer)as double
	
	dim as integer i,j,k,c,a
	for i=1 to l-(ngs-1)
		for j=i+1 to l-(ngs-1)
			c=0
			for k=0 to ngs-1
			   if array(i+k)=array(j+k) then c+=1
			next k
			a+=(c*(c-1))
		next j
	next i
	return a
	
end function

function m_reversal(array()as long,byval l as integer,byval s as integer)as integer
	
	dim as integer score
	dim as short i,a,b,array2(l),id(s,s),id2(s,s)
	
	for i=1 to l
		array2(l-(i-1))=array(i)
	next i
	
	'for i=1 to l-1
	'	a=array(i)
	'	b=array(i+1)
	'	if id(a,b)=0 then id(a,b)=1
	'next i
	'for i=1 to l-1
	'	a=array2(i)
	'	b=array2(i+1)
	'	if id(a,b)=1 then score+=1
	'next i
	
	for i=1 to l-1
		a=array(i)
		b=array(i+1)
		id(a,b)+=1
	next i
	for i=1 to l-1
		a=array2(i)
		b=array2(i+1)
		id2(a,b)+=1
	next i
	
	for a=1 to s
		for b=1 to s
			
			if id(a,b)>id2(a,b) then
				score+=id2(a,b)
			else
				score+=id(a,b)
			end if
			
		next b
	next a
	
	return score
	
end function

function m_fastbigrams(array()as long,byval l as integer,byval s as integer)as integer

	dim as short i,score,id(s,s)
	for i=1 to l-1
		s=array(i)
		l=array(i+1)
		if id(s,l)=0 then
			id(s,l)=1
		else 
			score+=1
		end if
	next i
	return score
	
end function

function m_fasttrigrams(array()as long,byval l as integer,byval s as integer)as integer
	
	if s>400 then return 0 '64mb memory limit (400 unique symbols)
	dim as short i,j,score,id(s,s,s)
	for i=1 to l-2
		s=array(i)
		l=array(i+1)
		j=array(i+2)
		if id(s,l,j)=0 then
			id(s,l,j)=1
		else 
			score+=1
		end if
	next i
	return score
	
end function

function m_fastbigrams_alphabet(array()as long,byval l as integer,byval s as integer,byval m as integer)as double
	
	'array()=cipher numbered by appearance
	'l=cipher length
	's=cipher symbols
	'm=new alphabet size
	
	dim as integer h,i,j,a,b,c,d,score,avg
	dim as short id(s,s)
	dim as short nts(s)
	'dim as short array2(l)
	'dim as double sda(10000)
	if m>s then m=s
	
	for i=1 to 10000
		for j=1 to s
			nts(j)=int(rnd*m) 'h
		next j
		c+=1
		for j=1 to l-1
			a=nts(array(j))
			b=nts(array(j+1))
			if id(a,b)<>c then
				id(a,b)=c
			else 
				score+=1
			end if
		next j
		'sda(i)=score
		avg+=score
		if score>((avg/i)*0.8) then d+=1
		score=0
	next i

	'return stdev(hi,10000,sda())*1000
	'return score/c
	return d
	
end function

function m_fastbigrams_short(array()as short,byval l as integer,byval s as integer)as integer

	dim as short i,score,id(s,s)
	for i=1 to l-1
		s=array(i)
		l=array(i+1)
		if id(s,l)=0 then
			id(s,l)=1
		else 
			score+=1
		end if
	next i
	return score
	
end function

function m_fastbigrams_cstate(byval instate as short,byval l as short,byval s as short)as short

	dim as short i,score,id(s,s)
	for i=1 to l-1
		s=cstate(instate,i)
		l=cstate(instate,i+1)
		if id(s,l)=0 then
			id(s,l)=1
		else 
			score+=1
		end if
	next i
	return score
	
end function

function m_bigrams(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
	
	if l=0 then return 0
	if l=s then return 0
	dim as double score1,score2
	dim as integer i,a,b,e
	dim as integer list(l-1)
	dim as integer eu(l-1,1)
	dim as integer ident(s,s)		  
	dim as integer symbs(s,s)
	for i=1 to l-1
		a=array(i)
		b=array(i+1)
		symbs(a,b)+=1
		if symbs(a,b)>0 then
			if ident(a,b)=0 then
				e+=1
				ident(a,b)=e
				list(e)=1
				eu(e,i mod 2)+=1
			else
				list(ident(a,b))+=1
				eu(ident(a,b),i mod 2)+=1
			end if
		end if
	next i
	select case n
		case 0 'bigrams
			for i=1 to e
				score1+=list(i)-1
			next i
			return score1
		case 1 'normalized bigrams
			for i=1 to e
				score1+=list(i)-1
			next i
			dim as integer frq(s)
			for i=1 to l
				frq(array(i))+=1
			next i
			for i=1 to s
				sort2(i,0)=frq(i)
				sort2(i,1)=i
			next i
			quicksort_sort2(1,s)
			dim as integer array2(l)
			a=0
			for i=1 to l
				do
					a+=1
					if a>s then a=1
				loop until sort2(a,0)>0
				array2(i)=sort2(a,1)
				sort2(a,0)-=1	
			next i
			e=0
			dim as integer list2(l-1)
			dim as integer ident2(s,s)
			dim as integer symbs2(s,s)
			for i=1 to l-1
				a=array2(i)
				b=array2(i+1)
				symbs2(a,b)+=1
				if symbs2(a,b)>0 then
					if ident2(a,b)=0 then
						e+=1
						ident2(a,b)=e
						list2(e)=1
					else
						list2(ident2(a,b))+=1										
					end if					
				end if
			next i
			for i=1 to e
				score2+=list2(i)-1
			next i
			return score1/score2
		case 2 'raw bigram ioc
	 		for i=1 to e
				score1+=list(i)*(list(i)-1)
	 		next i
	 		return score1
		case 3 'normalized bigram ioc
	 		for i=1 to e
				score1+=list(i)*(list(i)-1)
	 		next i
	 		return score1/((l-1)*(l-2))
		case 4 'odd bigrams
	 		for i=1 to e
	 			if list(i)>1 then
	 				if eu(i,0)=0 then
	 					score1+=eu(i,1)-1
	 				end if
	 			end if
	 		next i
			return score1
		case 5 'even bigrams
	 		for i=1 to e
	 			if list(i)>1 then
	 				if eu(i,1)=0 then
	 					score1+=eu(i,0)-1
	 				end if
	 			end if
	 		next i
			return score1
	end select

end function

function m_bigramposmodn(array()as long,byval l as integer,byval s as integer,byval m as integer,byval n as integer)as double
	
	if l=0 then return 0
	if l=s then return 0
	dim as double score1
	dim as integer i,a,b,e
	dim as integer list(l-1)
	dim as integer eu(l-1,m)
	dim as integer ident(s,s)		  
	dim as integer symbs(s,s)
	for i=1 to l-1
		a=array(i)
		b=array(i+1)
		symbs(a,b)+=1
		if symbs(a,b)>0 then
			if ident(a,b)=0 then
				e+=1
				ident(a,b)=e
				list(e)=1
				eu(e,i mod m)+=1
			else
				list(ident(a,b))+=1
				eu(ident(a,b),i mod m)+=1		
			end if					
		end if
	next i
	for i=1 to e
		if list(i)>1 andalso eu(i,n)=list(i) then
			score1+=eu(i,n)-1 
		end if
	next i
	return score1
	
end function

function m_bigramstructure(cip()as long,byval l as short,byval s as short,byval dx as short,byval dy as short,byval utp as short,byval p as short)as double
	
	dim as short i,j,k,x,y,e,b
	dim as short cip2(l)
	dim as short cip3(l)
	dim as short grid(dx,dy)
	dim as double score
	
	for i=1 to p 'transpose
	   for j=i to l step p
	      k+=1
	      if utp=1 then
	         cip2(k)=cip(j)
	      else
	      	cip2(j)=cip(k)
	      end if
	   next j
	next i
	
	for i=1 to l-2 'mark bigrams
		if cip3(i)=0 andalso cip3(i+1)=0 then
			for j=i+1 to l-1
				e=1
				for k=0 to 1
					if cip2(i+k)<>cip2(j+k) then 
						e=0
						exit for
					end if
				next k
				if e=1 then
					b+=1
					cip3(i)=1
					cip3(i+1)=1
					cip3(j)=1
					cip3(j+1)=1
				end if
			next j
		end if
	next i
	
	k=0
	for i=1 to p 'untranspose
	   for j=i to l step p
	      k+=1
	      if utp=1 then
	         cip2(j)=cip3(k)
	      else
	      	cip2(k)=cip3(j)
	      end if
	   next j
	next i
	
	i=0
	for y=1 to dy
		for x=1 to dx
			i+=1
			grid(x,y)=cip2(i)
			if i=l then exit for,for
		next x
	next y
	
	dim as double vls(l)
	for i=1 to l
		vls(i)=cip2(i)
	next i
	output_colormap(vls(),cip(),l,dx,dy,0,"bigram_structure_p"+str(p)+".bmp")
	
	return score
	
end function

function m_chi2_english(frq1()as double,frq2()as double,byval l as short)as double
	
	'frq1 = expected
	
	dim as short i
	dim as double score,a
	for i=0 to 25
		score+=((frq2(i)-frq1(i)*l)^2)/(frq1(i)*l)
	next i
	return score

end function

function m_chi2(set1()as double,set2()as double,byval s as integer,byval l as integer)as double
	
	'set2 = expected
	
	dim as integer i
	dim as double score
	for i=1 to s
		score+=((set1(i)-set2(i)*l)^2)/(set2(i)*l)
	next i
	return score

end function

function m_contactvariety(array()as long,byval l as integer,byval s as integer)as double
	
	dim as short cv(s,s)
	dim as integer i,j,a,maxscore,score
	for i=1 to l
		if i-1>0 then cv(array(i),array(i-1))+=1 'left-hand contact
		if i+1<=l then cv(array(i),array(i+1))+=1 'right-hand contact
	next i
	for i=1 to s
		a=0
		for j=1 to s
			if cv(i,j)>0 then score+=1
			a+=cv(i,j)
		next j
		if a>s then a=s
		maxscore+=a
	next i
	return score/maxscore
	
end function

function m_cycletypes(array()as long,byval l as integer,byval s as integer,byval cs as integer,cta()as short)as double
	
	dim as short ctmax=cta(0)
	dim as short l1,l2,l3,l4,l5,l6,l7
	dim as short t(cs),c(cs),p(cs),cf(cs),cscf(cs,cs)
	dim as short h,i,j,k,d,g,al,cl,fm,bl,a1,a2,a,b,e,o,al1,al2,p1,p2,ct
	dim as short frq(s)
	dim as double score,lowscore
	dim as integer n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n
	if cta(1)=1 then
		n1=cs
		n2=cs^2
		n3=cs^3
		n4=cs^4
		n5=cs^5
		n6=cs^6
		n7=cs^7
		n8=cs^8
		n9=cs^9
		n10=cs^10
	end if
	for i=1 to ctmax
		cto(cs,i,0,0)=0
	next i
   for i=1 to l
  		frq(array(i))+=1
   next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	for i=1 to l
		map(array(i),0)+=1
		map(array(i),map(array(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	dim as short z(fm*cs),z2(fm*cs)
	select case cs
		case 2
			for l1=1 to s
				for l2=l1+1 to s
					'if frq(l1)>1 andalso frq(l2)>1 then
						al=0
						cl=0
						for i=1 to cs
							p(i)=1
							t(i)=i '-1
						next i
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
						loop
						#include "m_cycletypes.bi"
					'end if
				next l2
			next l1	
		case 3 '3-symbol cycles
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 then		
							al=0
							cl=0
							for i=1 to cs
								p(i)=1
								t(i)=i '-1
							next i
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
							loop
							#include "m_cycletypes.bi"
						'end if
					next l3
				next l2
			next l1
		case 4 '4-symbol cycles	
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 then
								al=0
								cl=0
								for i=1 to cs
									p(i)=1
									t(i)=i '-1
								next i
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
								loop
								#include "m_cycletypes.bi"
							'end if
						next l4
					next l3
				next l2
			next l1
		case 5 '5-symbol cycles	
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							for l5=l4+1 to s
								'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 then
									al=0
									cl=0
									for i=1 to cs
										p(i)=1
										t(i)=i '-1
									next i
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
									loop
									#include "m_cycletypes.bi"
								'end if
							next l5
						next l4
					next l3
				next l2
			next l1
		case 6 '6-symbol cycles	
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							for l5=l4+1 to s
								for l6=l5+1 to s
									'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 then
										al=0
										cl=0
										for i=1 to cs
											p(i)=1
											t(i)=i '-1
										next i
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
										loop
										#include "m_cycletypes.bi"
									'end if
								next l6
							next l5
						next l4
					next l3
				next l2
			next l1
		case 7 '7-symbol cycles	
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							for l5=l4+1 to s
								for l6=l5+1 to s
									for l7=l6+1 to s
										'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 then
											al=0
											cl=0
											for i=1 to cs
												p(i)=1
												t(i)=i '-1
											next i
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
											loop
											#include "m_cycletypes.bi"
										'end if
									next l7
								next l6
							next l5
						next l4
					next l3
				next l2
			next l1
	end select
	return score

end function

function m_unigrams(array()as long,byval l as integer,byval s as integer,byval ml as integer,byval o as integer)as double
	
	dim as double score
	dim as integer i,j,m
	dim as integer ident(s)
	if o=0 then
		for i=1 to l step ml
			for j=i to i+(ml-1)
				ident(array(j))+=1
				if j=l then exit for
			next j
			for j=1 to s
				if ident(j)>1 then score+=(ident(j)-1)
				ident(j)=0
			next j
		next i
		return score
	else 'continuous
		'for i=1 to l-(ml+1)
		for i=1 to l-(ml-1)
			m+=(ml-1)
			for j=i to i+(ml-1)
				ident(array(j))+=1
			next j
			for j=1 to s
				if ident(j)>1 then score+=(ident(j)-1)
				ident(j)=0
			next j
		next i
		return score '/m
	end if

end function

function m_unigramperiodic(array()as long,byval l as integer,byval s as integer,byval m as integer,byval ue as integer,byval p as integer,byval o as integer)as double
	
	dim as integer i,j,k,dx
	dim as short sa1(s),sa2(s)
	dim as double score
	select case m
		case 0 'by rows
			dx=l/p
			if l*p<dx then dx+=1
			for i=1 to p
				for j=1 to dx
					k+=1
					if i=o then sa1(array(k))+=1 else sa2(array(k))+=1	
				next j
			next i
		case 1 'by columns
			for i=1 to p
				for j=i to l step p
					if i=o then sa1(array(j))+=1 else sa2(array(j))+=1
				next j
			next i
	end select
	for i=1 to s
		if ue=0 then 'unigrams unique to period
			if sa1(i)>0 andalso sa2(i)=0 then score+=sa1(i)
		else 'unigrams exclusive to period
			if sa2(i)>0 andalso sa1(i)=0 then score+=sa2(i)
		end if
	next i
	return score

end function

function m_unigramperiodicvs(array()as long,byval l as integer,byval s as integer,byval m as integer,byval ue as integer,byval p as integer,byval o as integer)as double
	
	dim as integer i,j,k,dx
	dim as short sa(p,s) ',sa2(s)
	dim as double score
	select case m
		case 0 'by rows
			dx=l/p
			if l*p<dx then dx+=1
			for i=1 to p
				for j=1 to dx
					k+=1
					'if i=o then sa1(array(k))+=1 else sa2(array(k))+=1	
					sa(i,array(k))+=1
				next j
			next i
		case 1 'by columns
			for i=1 to p
				for j=i to l step p
					'if i=o then sa1(array(j))+=1 else sa2(array(j))+=1
					sa(i,array(j))+=1
				next j
			next i
	end select
	for i=1 to p
		if i<>o then
			for j=1 to s
				if ue=0 then 'unigrams unique versus other periods
					if sa(o,j)>0 andalso sa(i,j)=0 then score+=sa(o,j)
				else 'unigrams exclusive versus other periods
					if sa(i,j)>0 andalso sa(o,j)=0 then score+=sa(i,j)
				end if		
			next j
		end if
	next i
	'for i=1 to s
	'	if ue=0 then 'unigrams unique to period
	'		if sa1(i)>0 andalso sa2(i)=0 then score+=sa1(i)
	'	else 'unigrams exclusive to period
	'		if sa2(i)>0 andalso sa1(i)=0 then score+=sa2(i)
	'	end if
	'next i
	return score

end function

function m_iocperiodic(array()as long,byval l as integer,byval s as integer,byval m as integer,byval ue as integer,byval p as integer,byval o as integer)as double
	
	dim as integer i,j,k,dx
	dim as short sa1(s),sa2(s)
	dim as double score
	select case m
		case 0 'by rows
			dx=l/p
			if dx<l/p then dx+=1 '???
			for i=1 to p
				for j=1 to dx
					k+=1
					if i=o then sa1(array(k))+=1 else sa2(array(k))+=1	
				next j
			next i
		case 1 'by columns
			for i=1 to p
				for j=i to l step p
					if i=o then sa1(array(j))+=1 else sa2(array(j))+=1
				next j
			next i
	end select
	for i=1 to s
		'if ue=0 then 'unigrams unique to period
			if sa1(i)>1 then score+=sa1(i)*(sa1(i)-1)
			'if sa1(i)>0 andalso sa2(i)=0 then score+=sa1(i)
		'else 'unigrams exclusive to period
			'if sa2(i)>0 andalso sa1(i)=0 then score+=sa2(i)
		'end if
	next i
	return score

end function

function m_unigramdistance(array()as long,byval l as integer,byval s as integer)as double
	
	dim as integer i,j,a,b,score
	dim as short aud(s,l)
	for i=1 to l
		aud(array(i),0)+=1
		aud(array(i),aud(array(i),0))=i
	next i
	for i=1 to s
		'a=0
		for j=1 to aud(i,0)-1
			'b=aud(i,j+1)-aud(i,j)
			'if b>a then a=b
			score+=aud(i,j+1)-aud(i,j)
		next j
		'score+=a
	next i
	return score
	
end function

function m_unigramdistanceuo(array()as long,byval l as integer,byval s as integer,byval uo as integer,byval u as integer)as double
	
	dim as integer i,j,a,b,score
	dim as short aud(s,l)
	for i=1 to l
		aud(array(i),0)+=1
		aud(array(i),aud(array(i),0))=i
	next i
	for i=1 to s
		'a=0
		for j=1 to aud(i,0)-1
			'b=aud(i,j+1)-aud(i,j)
			'if b>a then a=b
			a=aud(i,j+1)-aud(i,j)
			if uo=0 then
				if a<u then score+=a
			else
				if a>u then score+=a
			end if
			'score+=aud(i,j+1)-aud(i,j)
		next j
		'score+=a
	next i
	return score
	
end function

function m_slope(array()as long,byval l as integer,byval s as integer,byval n as integer)as double

	dim as integer i,j
	dim as integer rup,rdown
	dim as double score
	select case n
		case 0,1
			for i=1 to l-1
				if array(i)<array(i+1) then score+=1
				if array(i)>array(i+1) then score-=1
			next i
		case 2,3
			for i=1 to l-1
				if array(i)+1=array(i+1) then score+=1
			next i
		case 4,5
			for i=1 to l-1
				if array(i)-1=array(i+1) then score+=1
			next i
	end select
	select case n
		case 0,2,4:return score
		case 1,3,5:return score/(l-1)
	end select

end function

function m_gridmatch(g1a()as long,g2a()as long,byval x1 as integer,byval y1 as integer,byval x2 as integer,byval y2 as integer,byval o as integer)as double

	dim as double score
	dim as integer i,j,k,x,y,gx,gy,c1,c2
	dim as long gr1(x1,y1)
	dim as long gr2(x2,y2)
	for y=1 to y1
		for x=1 to x1
			i+=1
			gr1(x,y)=g1a(i)	
		next x
	next y
	for y=1 to y2
		for x=1 to x2
			j+=1
			gr2(x,y)=g2a(j)	
		next x
	next y
	if x1>x2 then gx=x1 else gx=x2
	if y1>y2 then gy=y1 else gy=y2
	for y=1 to gy
		for x=1 to gx
			if gr1(x,y)>0 andalso gr2(x,y)>0 andalso gr1(x,y)=gr2(x,y) then	 
				c1+=1
			else
				if o=0 then 
					if c1>0 then c2+=c1
				else
					if c1>1 then c2+=c1*(c1-1)
				end if
				c1=0
			end if
		next x
	next y
	if o=0 then 
		if c1>0 then c2+=c1
	else
		if c1>1 then c2+=c1*(c1-1)
	end if
	score=c2
	return score

end function

function m_gridioc(array()as long,byval l as integer,byval s as integer,byval hv as integer,byval kl as integer)as double
	
	dim as integer x,y=1,i,j,dx,dy
	dim as double score
	dim as long reps(s)
	
	'if hv=0 then
		dx=kl
		dy=l/kl
		if l/kl>dy then dy+=1
	'else
	'	dy=kl
	'	dx=l/kl
	'	if l/kl>dx then dx+=1
	'end if
	
	dim as long grid(dx,dy)
	
	for i=1 to l
		x+=1
		if x>dx then
			x=1
			y+=1
		end if
		grid(x,y)=array(i)
	next i
	
	if hv=0 then 'horizontal
		for y=1 to dy
			for x=1 to dx
				reps(grid(x,y))+=1
			next x
			for i=1 to s
				if reps(i)>0 then score+=reps(i)*(reps(i)-1)
				reps(i)=0
			next i
		next y
	else 'vertical
		for x=1 to dx
			for y=1 to dy
				reps(grid(x,y))+=1
			next y
			for i=1 to s
				if reps(i)>0 then score+=reps(i)*(reps(i)-1)
				reps(i)=0
			next i
		next x
	end if	
	
	return score
	
end function

function m_lean(array()as long,byval l as integer,byval s as integer,byval dx as integer,byval dy as integer,byval o as integer)as double
	
	dim as integer i,x,y
	dim as double lean(s),score,m
	dim as short frq(s)
	if o=0 then m=(1+dx)/2 else m=(1+dy)/2
	for y=1 to dy
		for x=1 to dx
			i+=1
			frq(array(i))+=1
			if o=0 then 'hor
				lean(array(i))+=x
			else 'ver
				lean(array(i))+=y
			end if	
			if i=l then exit for,for
		next x
	next y
	for i=1 to s
		score+=abs(lean(i)-(m*frq(i)))
	next i
	return score
	
end function

function m_unigram_tmb(array()as long,byval l as integer,byval s as integer,byval tmb as integer,byval m as integer)as double
	
	dim as integer i,j,k,x,y
	dim as short ga(3,s)
	dim as double score
	for i=1 to tmb*17
		ga(1,array(i))+=1
	next i
	for i=(tmb*17)+1 to 340-(tmb*17)
		ga(2,array(i))+=1
	next i
	for i=341-(tmb*17) to 340
		ga(3,array(i))+=1
	next i
	for i=1 to s
		if ga(2,i)=0 andalso ga(1,i)>0 andalso ga(3,i)>0 then
			if m=0 then
				score+=ga(1,i)+ga(3,i)
			else
				score+=1
			end if
		end if
	next i
	return score

end function

function m_npairs(array()as long,l as integer,n as integer)as double
	
	dim as integer i,j,e,score
	for i=1 to l-(n-1)
		e=1
		for j=1 to n-1
			if array(i)<>array(i+j) then
				e=0
				exit for
			end if
		next j
		if e=1 then score+=1
	next i
	return score
	
end function

function m_normor(frq()as double)as double
	
	'adapted: Normor by THE RAT
	
	'The Rule:
	'----------------------------------------------------------------
	'Ciphers that tend to encipher high-frequency letters in whole
	'or part with themselves or other high-frequency letters 
	'have low scores. Those that do not, have high scores. 
	
	'frq() array:
	'--------------------------------
	'index 0 is frequency of letter A
	'index 1 is frequency of letter B
	'index 2 is frequency of letter C
	'etc...
	
	dim as integer i,j,k,n,score,nor(25),m=-1
	dim as string normalorder="ETAOINSRHLDUCMGFYPWBVKXJZQ"
	
	for i=0 to 25
		nor(i)=asc(normalorder,i+1)-65
	next i
	
	for i=0 to 25
		for j=0 to 25
			if frq(j)>m then
				k=j 'position frq
				m=frq(j)
			end if
		next j
		for j=0 to 25
			if nor(j)=k then
				n=j 'position nor
				exit for
			end if
		next j
		score+=abs(n-i)
		frq(k)=-1
		m=-1
	next i
	
	return score
	
end function

function m_unigramunitnorepeats(array()as long,byval l as integer,byval s as integer,byval dx as integer,byval dy as integer,byval m as integer,byval o as integer,byval n as integer)as double
	
	dim as integer i,j,k,x,y,e,dxy
	dim as short gra(dx,dy),seq(l)
	dim as double score
	for y=1 to dy
		for x=1 to dx
			i+=1
			j=i+o
			if j>l then j-=l
			gra(x,y)=array(j)
			if i=l then exit for,for
		next x
	next y
	select case m
		case 0 'row
			for y=1 to dy
				e=0
				for x=1 to dx
					if seq(gra(x,y))=y then
						e=1
						exit for
					end if
					seq(gra(x,y))=y
				next x
				if e=0 then score+=1
			next y
			dxy=dy
		case 1 'column
			for x=1 to dx
				e=0
				for y=1 to dy	
					if seq(gra(x,y))=x then
						e=1
						exit for
					end if
					seq(gra(x,y))=x	
				next y
				if e=0 then score+=1
			next x
			dxy=dx
	end select
	if n=0 then 
		return score
	else
		return score/dxy
	end if

end function

function m_wordflow(sol()as long,byval l as integer)as double
	
	dim as integer i,j,k
	dim as double score
	j=g5p(alpharev(sol(1)),alpharev(sol(2)),alpharev(sol(3)),alpharev(sol(4)),alpharev(sol(5)))
	for i=2 to l-4
		k=g5p(alpharev(sol(i)),alpharev(sol(i+1)),alpharev(sol(i+2)),alpharev(sol(i+3)),alpharev(sol(i+4)))
		if k-j=1 then score+=1
		j=k
	next i
	return score/l
	
end function

function m_adjacency(array()as long,byval l as integer,byval dx as integer,byval dy as integer)as double
	
	dim as integer i,x,y
	dim as ushort grid(dx,dy)
	dim as byte sym(l)
	dim as double score
	sym(12)=1 'numbered by appearance
	sym(16)=1
	sym(21)=1
	sym(23)=1
	sym(33)=1
	sym(40)=1
	sym(45)=1
	sym(50)=1
	sym(53)=1
	sym(57)=1
	sym(63)=1
	'if array(12)=12 then beep
	for y=1 to dy
		for x=1 to dx
			i+=1
			grid(x,y)=array(i)
		next x
	next y
	i=0
	for y=1 to dy
		for x=1 to dx
			i+=1
			if sym(grid(x,y))=1 then
				if y-1>0 andalso sym(grid(x,y-1))=1 then score+=1 'up
				if y+1<=dy andalso sym(grid(x,y+1))=1 then score+=1 'down
				if x-1>0 andalso sym(grid(x-1,y))=1 then score+=1 'left
				if x+1<=dx andalso sym(grid(x+1,y))=1 then score+=1 'right
				if y-1>0 andalso x-1>0 andalso sym(grid(x-1,y-1))=1 then score+=1 'up-left
				if y-1>0 andalso x+1<=dx andalso sym(grid(x+1,y-1))=1 then score+=1 'up-right
				if y+1<=dy andalso x-1>0 andalso sym(grid(x-1,y+1))=1 then score+=1 'down-left
				if y+1<=dy andalso x+1<=dx andalso sym(grid(x+1,y+1))=1 then score+=1 'down-right
			end if 
			if i=l then exit for,for
		next x
	next y
	return score/2
	
end function

function m_posshift(array()as long,byval l as integer,byval s as integer,byval lk as integer,byval n as integer)as double
	
	if l=s then return 0
	if lk<2 then return 0
	dim as double score
	dim as double a
	dim as integer c,i,j,k
	dim as short sp(s,lk-1)
	dim as short sp2(lk-1)
	dim as short frq(s)
	for i=1 to l
		frq(array(i))+=1
		sp(array(i),((i-1) mod lk))+=1
	next i
	if n=0 then
		for i=1 to s
			for j=0 to lk-1
				for k=j+1 to lk-1
					c=abs(sp(i,j)-sp(i,k))
					if c=1 then c=0
					score+=c
				next k
			next j
		next i
		return score
	else
		for i=1 to s
			sp2(0)=frq(i)
			for j=0 to lk-1
				for k=j+1 to lk-1
					c=abs(sp(i,j)-sp(i,k))
					if c=1 then c=0
					score+=c
				next k
				for k=j+1 to lk-1
					c=abs(sp2(j)-sp2(k))
					if c=1 then c=0
					a+=c
				next k
			next j
		next i
		return score/a
	end if
	
end function

function m_posshift2(array()as long,byval l as integer,byval s as integer,byval lk as integer)as double
	
	if l=s then return 0
	dim as double a,score1,score2
	dim as integer c,h,i,j,k
	dim as ulong sp(s,l) '2000,1000
	dim as integer sp2(l) '2000
	dim as integer frq(s) '2000
	for h=2 to lk
		
		'erase sp
		'erase sp2
		'erase frq
		
		for i=0 to s
			frq(i)=0
			for j=0 to l
				sp(i,j)=0
			next j	
		next i
		for i=0 to l
			sp2(i)=0
		next i
		
		for i=1 to l
			frq(array(i))+=1
			sp(array(i),((i-1) mod h))+=1
		next i
		a=0
		score1=0
		for i=1 to s
			sp2(0)=frq(i)
			for j=0 to h-1
				for k=j+1 to h-1
					c=abs(sp(i,j)-sp(i,k))
					if c=1 then c=0
					score1+=c
				next k
				for k=j+1 to h-1
					c=abs(sp2(j)-sp2(k))
					if c=1 then c=0
					a+=c
				next k	
			next j
		next i
		score1/=a
		if score1>score2 then score2=score1	 
	next h
	return score2
	
end function

function m_periods(array()as long,byval l as integer,byval s as integer)as double
	
	'for i=1 to a
	'	for j=i to l step a
	'		k+=1
	'		if untransposed=0 then 'transpose
	'			cstate(outstate,j)=cstate(instate,k)
	'		else 'untranspose
	'			cstate(outstate,k)=cstate(instate,j)
	'		end if
	'	next j
	'next i
	
	dim as short i,j,k,a,b,c,x,y,xx
	dim as short p=l/2
	dim as short cip(l) ',cip2(l)
	'dim as short gra(dx,dy)
	dim as double score
	'for y=1 to dy
	'	for x=1 to dx
	'		c+=1
	'		gra(x,y)=array(c)
	'		if c=l then exit for,for
	'	next x
	'next y
	'for i=0 to dx-1 'offset column order
		c=0
		'for y=1 to dy
		'	for x=1 to dx
		'		xx=x+i
		'		if xx>dx then xx-=dx
		'		if gra(xx,y)>0 then
		'			c+=1
		'			cip(c)=gra(xx,y)
		'		end if
		'	next x
	   'next y
		for b=1 to p
			c=0
			for j=1 to b
				for k=j to l step b
					c+=1
					cip(c)=array(k) 'untranspose
				next k
			next j
			a=m_fastbigrams_short(cip(),l,s)
			if a>score then score=a
		next b
	'next i
	return score

end function

function m_periodsplusoco(array()as long,byval l as integer,byval s as integer,byval dx as integer,byval dy as integer)as double
	
	'for i=1 to a
	'	for j=i to l step a
	'		k+=1
	'		if untransposed=0 then 'transpose
	'			cstate(outstate,j)=cstate(instate,k)
	'		else 'untranspose
	'			cstate(outstate,k)=cstate(instate,j)
	'		end if
	'	next j
	'next i
	
	dim as short i,j,k,a,b,c,x,y,xx
	dim as short p=l/2
	dim as short cip(l),cip2(l)
	dim as short gra(dx,dy)
	dim as double score
	for y=1 to dy
		for x=1 to dx
			c+=1
			gra(x,y)=array(c)
			if c=l then exit for,for
		next x
	next y
	for i=0 to dx-1 'offset column order
		c=0
		for y=1 to dy
			for x=1 to dx
				xx=x+i
				if xx>dx then xx-=dx
				if gra(xx,y)>0 then
					c+=1
					cip(c)=gra(xx,y)
				end if
			next x
	   next y
		for b=1 to p
			c=0
			for j=1 to b
				for k=j to l step b
					c+=1
					cip2(c)=cip(k) 'untranspose
				next k
			next j
			a=m_fastbigrams_short(cip2(),l,s)
			if a>score then score=a
		next b
	next i
	return score

end function

function m_pccycles_longshort(pt()as long,ct()as short,byval l as short,byval s2 as short)as double
	
	dim as short h,i,j,k,e,s1,cs,cl,al
	dim as short id1(255)
	dim as short pt2(l)
	dim as double score
	for i=1 to l
   	if id1(pt(i))=0 then
   		s1+=1
   		id1(pt(i))=s1
   		pt2(i)=s1
   	else
   		pt2(i)=id1(pt(i))
   	end if
	next i
	dim as short cyc(s1,l)
	dim as short idc(s1,s2)
	for i=1 to l
		cyc(pt2(i),0)+=1
		cyc(pt2(i),cyc(pt2(i),0))=ct(i)
		if idc(pt2(i),ct(i))=0 then
			idc(pt2(i),0)+=1
			idc(pt2(i),ct(i))=1
		end if
	next i
	for h=1 to s1
		al=0
		cs=idc(h,0)
		cl=cyc(h,0)
		if cs=1 then 
			score+=cl '(cl*(cl-1))
		else
			for i=1 to cl-(cs-1)
				e=1
				for j=i to i+(cs-2)
					for k=j+1 to i+(cs-1)
						if cyc(h,j)=cyc(h,k) then
							e=0
							exit for,for
						end if
					next k	
				next j
				if e=1 then al+=1
			next i
			if al>1 then score+=(al*(al-1))*cs
		end if
	next h
	return score

end function

function m_pccycles_shortshort(pt()as short,ct()as short,byval l as short,byval s2 as short)as double
	
	dim as short h,i,j,k,e,s1,cs,cl,al
	dim as short id1(255)
	dim as short pt2(l)
	dim as double score
	for i=1 to l
   	if id1(pt(i))=0 then
   		s1+=1
   		id1(pt(i))=s1
   		pt2(i)=s1
   	else
   		pt2(i)=id1(pt(i))
   	end if
	next i
	dim as short cyc(s1,l)
	dim as short idc(s1,s2)
	for i=1 to l
		cyc(pt2(i),0)+=1
		cyc(pt2(i),cyc(pt2(i),0))=ct(i)
		if idc(pt2(i),ct(i))=0 then
			idc(pt2(i),0)+=1
			idc(pt2(i),ct(i))=1
		end if
	next i
	for h=1 to s1
		al=0
		cs=idc(h,0)
		cl=cyc(h,0)
		if cs=1 then 
			score+=cl '(cl*(cl-1))
		else
			for i=1 to cl-(cs-1)
				e=1
				for j=i to i+(cs-2)
					for k=j+1 to i+(cs-1)
						if cyc(h,j)=cyc(h,k) then
							e=0
							exit for,for
						end if
					next k	
				next j
				if e=1 then al+=1
			next i
			if al>1 then score+=(al*(al-1))*cs
		end if
	next h
	return score

end function

function m_unicycle(cycle()as long,byval l as integer)as double
	
	dim as short i,j,k,e,cs,al
	dim as short id(constcip)
	for i=1 to l
		if id(cycle(i))=0 then
			cs+=1
			id(cycle(i))=1
		end if
	next i
	if cs=1 then return l*(l-1)
	for i=1 to l-(cs-1)
		e=1
		for j=i to i+(cs-2)
			for k=j+1 to i+(cs-1)
				if cycle(j)=cycle(k) then
					e=0
					exit for,for
				end if
			next k
		next j
		if e=1 then al+=1
	next i
	if al>1 then return (al*(al-1))*cs else return 0

end function

function m_2cyclespectrum(nba()as long,byval l as integer,byval s as integer)as double
	
	'safe for use with the standard deviation
	'unsafe as fitness measurement in hill-climbers
	'unsafe with very short texts
	
	dim as integer i,j,k,h,a,b,d,g,t
	dim as integer p1,p2,fm,cm ',state=12345
	'dim as integer crnd=1000000
	dim as short frq(s)
	dim as double score,maxscore,al,cl
	'static as double cyclefactor(100,100)
	
   for i=1 to l
  		frq(nba(i))+=1
   next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	for i=1 to l
		map(nba(i),0)+=1
		map(nba(i),map(nba(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	dim as short z(fm*2)
	for i=1 to s
		for j=i+1 to s
			if frq(i)>1 andalso frq(j)>1 then
				if abs(frq(i)-frq(j))<2 then
					p1=1
					p2=1
					al=0
					cl=0
					do
						a=map(i,p1)
						b=map(j,p2)
						if a<b then 
							d=a
							p1+=1
							g=0
						else 
							d=b
							p2+=1
							g=1
						end if
						if d=l+1 then exit do
						cl+=1
						z(cl)=g
					loop
					for k=1 to cl-1
						if z(k)<>z(k+1) then al+=1
					next k		
					al-=1
					cl-=2
					score+=al
					maxscore+=cl			
				end if
			end if
		next j
	next i
	return score/maxscore

end function

function m_2cyclespectrum_short(nba()as short,byval l as integer,byval s as integer)as double
	
	'safe for use with the standard deviation
	'unsafe as fitness measurement in hill-climbers
	'unsafe with very short texts
	
	dim as short i,j,k,p1,p2,a,b,c,d,e,g,cl,fm
	dim as short frq(s)
	dim as double score,maxscore,al
   for i=1 to l
  		frq(nba(i))+=1
   next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	for i=1 to l
		map(nba(i),0)+=1
		map(nba(i),map(nba(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	dim as short z(fm*2)
	for i=1 to s
		for j=i+1 to s
			if frq(i)>1 andalso frq(j)>1 then
				if abs(frq(i)-frq(j))<2 then
					e=0
					p1=1
					p2=1
					al=0
					cl=0
					do
						a=map(i,p1)
						b=map(j,p2)
						if a<b then 
							d=a
							p1+=1
							g=0
						else 
							d=b
							p2+=1
							g=1
						end if
						if d=l+1 then exit do
						cl+=1
						z(cl)=g
					loop
					for k=1 to cl-1
						if z(k)<>z(k+1) then al+=1
					next k
					al-=1
					score+=al
					maxscore+=cl-2
				end if
			end if
		next j
	next i
	return (score/maxscore)

end function

function m_2cycles(array()as long,byval l as integer,byval s as integer,byval weight as double)as double
	
	'array = cipher numbered by appearance
	'l = cipher length
	's = cipher symbols
	'weight = cycle weight (try 5)
	
	dim as short i,j,k,p1,p2,a,b,c,d,e,al,cl,fm
	dim as short frq(s)
	dim as double score
	for i=1 to l
		frq(array(i))+=1
	next i
	for i=1 to s
		if frq(i)>fm then fm=frq(i)
	next i
	dim as short map(l,fm+1)
	for i=1 to l
		map(array(i),0)+=1
		map(array(i),map(array(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	for i=1 to s
		for j=i+1 to s
			if frq(i)>1 andalso frq(j)>1 then
				e=0
				p1=1
				p2=1
				al=0
				cl=0
				do
					a=map(i,p1)
					b=map(j,p2)
					if a<b then
						c=a
						p1+=1
						d=1
					else
						c=b
						p2+=1
						d=2
					end if
					if c=l+1 then exit do
					cl+=1
					if e>0 andalso e<>d then al+=1
					e=d
				loop
				'score+=al-1
				if al>1 then score+=(cl-1)*((al/(cl-1))^stats_nsymbolcyclesweight)
				'if al>1 then score+=scs_table(al,cl-1) 'look up table
			end if
		next j
	next i
	return score

end function

function m_2cycles_perfect(array()as long,byval l as integer,byval s as integer)as double
	
	dim as short cs=2
	dim as short i,j,k,d,e,g,al,cl,fm
	dim as short l1,l2
	dim as short t(cs),c(cs),p(cs)
	dim as integer lioc=(l-(cs-1))*(l-cs)
	dim as short blc=(cs*(cs-1))/2
	dim as short frq(s)
	dim as integer score
	for i=1 to l
		frq(array(i))+=1
	next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	dim as short z(fm*cs)
	for i=1 to l
		map(array(i),0)+=1
		map(array(i),map(array(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	for l1=1 to s
		for l2=l1+1 to s
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
					'cycles+=1
					'cyclelengths+=al
				end if 
			end if
		next l2
	next l1
	return score

end function

function m_2cycles_stats(array()as long,byval l as integer,byval s as integer,byval weight as double,byval n as integer)as double
	
	dim as short h,i,j,k,p1,p2,a,b,c,d,e,al,g,cl,fm
	dim as short frq(s)
	dim as integer cycles,cyclelengths,lioc=(l-1)*(l-2)
	dim as double score
	dim as short sym(s)
	dim as short num=info_numerical
	if n=6 then 'enumerate
		report=""
		dim as short id(identmax)
		for i=1 to l
			if id(array(i))=0 then
				e+=1
				id(array(i))=e
				sym(e)=array(i)
			end if
		next i
		for i=1 to l
			array(i)=id(array(i))
		next i
	end if
   for i=1 to l
  		frq(array(i))+=1
   next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	for i=1 to l
		map(array(i),0)+=1
		map(array(i),map(array(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	dim as short z(fm*2)
	for i=1 to s
		for j=i+1 to s
			if frq(i)>1 andalso frq(j)>1 then
				e=0
				p1=1
				p2=1
				al=0
				cl=0
				do
					a=map(i,p1)
					b=map(j,p2)
					if a<b then 
						d=a
						p1+=1
						g=i
					else 
						d=b
						p2+=1
						g=j
					end if
					if d=l+1 then exit do
					cl+=1
					z(cl)=g
					if cl>1 andalso z(cl)<>z(cl-1) then al+=1
				loop
				select case n
					case 0 'all cycles
						'if al>1 then score+=scs_table(al,cl-1) 
						'if al>1 then score+=(cl-2)*((al/(cl-2))^weight)
						if al>1 then score+=(cl-1)*((al/(cl-1))^stats_nsymbolcyclesweight)
					case 1,2 'perfect cycles ioc raw
						if al=cl-1 then score+=al*(al-1)
					case 3 'perfect cycles ioc flatness
						if al=cl-1 then
							score+=al*(al-1)
							cycles+=1
							cyclelengths+=al
						end if	
					case 4 'perfect cycles amount
						if al=cl-1 then score+=1
					case 5 'perfect cycles average cycle length
						if al=cl-1 then
							cycles+=1
							cyclelengths+=al
						end if
					case 6 'report cycles
						if al=cl-1 then
							score+=al*(al-1)
							cycles+=1
							cyclelengths+=al
							report+=lb
							for h=1 to cl
								if num=0 then
									report+=chr(sym(z(h)))
								else
									report+=str(sym(z(h)))
									if h<>cl then report+=" "
								end if
							next h
							report+=" ("+str(al*(al-1))+")"
						end if
				end select
			end if
		next j
	next i
	select case n
		case 0,1,4
			return score
		case 2
			return score/lioc
		case 3
			return (((cyclelengths/cycles)*((cyclelengths/cycles)-1))*cycles)/score
		case 5
			return cyclelengths/cycles
		case 6
			if score=0 then 
				report="No cycles found"
				return 0
			end if
			dim as string os
			os+="Index of coincidence:"+lb
			os+="- Raw: "+str(score)+lb
			os+="- Normalized: "+str(score/lioc)+lb
			os+="- Flatness: "+str((((cyclelengths/cycles)*((cyclelengths/cycles)-1))*cycles)/score)+lb
			os+="Cycles: "+str(cycles)+lb
			os+="Average cycle length: "+str(cyclelengths/cycles)+lb
			os+=lb
			os+="Cycles by appearance:"+lb
			os+="---------------------------------------------------------"
			report=os+report
	end select

end function

function m_2cycles_perfect_cyclebreaks(array()as long,byval l as integer,byval s as integer)as double
	
	dim as short cs=2
	dim as short i,j,k,d,e,g,al,cl,fm,b
	dim as short l1,l2
	dim as short t(cs),c(cs),p(cs)
	dim as integer lioc=(l-(cs-1))*(l-cs)
	dim as short blc=(cs*(cs-1))/2
	dim as short frq(s)
	dim as integer score
	for i=1 to l
		frq(array(i))+=1
	next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	dim as short z(fm*cs)
	dim as short zp(fm*cs)
	for i=1 to l
		map(array(i),0)+=1
		map(array(i),map(array(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	for l1=1 to s
		for l2=l1+1 to s
			if frq(l1)>1 andalso frq(l2)>1 then
				
				if abs(frq(l1)-frq(l2))<=int(solvesub_matchweight) then 'solvesub_matchweight
				
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
						zp(cl)=c(g) 'position
						if cl>(cs-1) then
							i=cl-(cs-1)
							for j=i to i+(cs-2)
								for k=j+1 to i+(cs-1)
									if z(j)=z(k) then
										e=0
										b+=1
										'cmap(zp(k))+=1
										'exit do
									end if
								next k
							next j
						end if
					loop
					
					if b<=int(solvesub_matchweight) then 'solvesub_matchweight
						for i=1 to cl-1
							if z(i)=z(i+1) then
								cmap(zp(i+1))+=1
							end if
						next i
					end if
					b=0
					
					if e=1 then
						al=cl-(cs-1)
						score+=al*(al-1)
						'cycles+=1
						'cyclelengths+=al
					end if
				
			end if
				
			end if
		next l2
	next l1
	return score

end function

function m_3cycles(array()as long,byval l as integer,byval s as integer,byval weight as double)as double
	
	dim as short h,i,j,k,p1,p2,p3,a,b,c,d,e,al,g,cl,fm
	dim as short frq(s)
	dim as double score
    for i=1 to l
  		frq(array(i))+=1
   next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	for i=1 to l
		map(array(i),0)+=1
		map(array(i),map(array(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	dim as short z(fm*3)
	for i=1 to s
		for j=i+1 to s
			for k=j+1 to s
				if frq(i)>1 andalso frq(j)>1 andalso frq(k)>1 then
					e=0
					p1=1
					p2=1
					p3=1
					al=0
					cl=0
					do
						a=map(i,p1)
						b=map(j,p2)
						c=map(k,p3)
						if a<b then 
							d=a
							if d>c then
								d=c
								p3+=1
								g=3
							else
								p1+=1
								g=1
							end if
						else 
							d=b
							if d>c then 
								d=c
								p3+=1
								g=3
							else
								p2+=1
								g=2
							end if
						end if
						if d=l+1 then exit do
						cl+=1
						z(cl)=g
						if cl>2 andalso z(cl-2)<>z(cl-1) andalso z(cl-2)<>z(cl) andalso z(cl-1)<>z(cl) then al+=1
					loop
					'if al>1 then score+=scs_table(al,cl-2)
					if al>1 then score+=(cl-2)*((al/(cl-2))^stats_nsymbolcyclesweight)
				end if
			next k
		next j
	next i
	return score

end function

function m_3cycles_perfect(array()as long,byval l as integer,byval s as integer)as double
	
	dim as short cs=3
	dim as short i,j,k,d,e,g,al,cl,fm
	dim as short l1,l2,l3
	dim as short t(cs),c(cs),p(cs)
	dim as integer lioc=(l-(cs-1))*(l-cs)
	dim as short blc=(cs*(cs-1))/2
	dim as short frq(s)
	dim as integer score
	for i=1 to l
		frq(array(i))+=1
	next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	dim as short z(fm*cs)
	for i=1 to l
		map(array(i),0)+=1
		map(array(i),map(array(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	for l1=1 to s
		for l2=l1+1 to s
			for l3=l2+1 to s
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
						'cycles+=1
						'cyclelengths+=al
					end if 
				end if
			next l3
		next l2
	next l1
	return score

end function

function m_3cycles_stats(array()as long,byval l as integer,byval s as integer,byval weight as double,byval n as integer)as double
	
	dim as short h,i,j,k,p1,p2,p3,a,b,c,d,e,al,g,cl,fm
	dim as short frq(s)
	dim as integer cycles,cyclelengths,lioc=(l-2)*(l-3)
	dim as double score
	dim as short sym(s)
	dim as short num=info_numerical
	if n=6 then 'enumerate
		report=""
		dim as short id(identmax)
		for i=1 to l
			if id(array(i))=0 then
				e+=1
				id(array(i))=e
				sym(e)=array(i)
			end if
		next i
		for i=1 to l
			array(i)=id(array(i))
		next i
	end if
   for i=1 to l
  		frq(array(i))+=1
   next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	for i=1 to l
		map(array(i),0)+=1
		map(array(i),map(array(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	dim as short z(fm*3)
	for i=1 to s
		for j=i+1 to s
			for k=j+1 to s
				if frq(i)>1 andalso frq(j)>1 andalso frq(k)>1 then
					e=0
					p1=1
					p2=1
					p3=1
					al=0
					cl=0
					do
						a=map(i,p1)
						b=map(j,p2)
						c=map(k,p3)
						if a<b then 
							d=a
							if d>c then
								d=c
								p3+=1
								g=k
							else
								p1+=1
								g=i
							end if
						else 
							d=b
							if d>c then 
								d=c
								p3+=1
								g=k
							else
								p2+=1
								g=j
							end if
						end if
						if d=l+1 then exit do
						cl+=1
						z(cl)=g 
						if cl>2 andalso z(cl-2)<>z(cl-1) andalso z(cl-2)<>z(cl) andalso z(cl-1)<>z(cl) then al+=1
					loop
					select case n
						case 0 'all cycles
							'if al>1 then score+=scs_table(al,cl-2) 
							if al>1 then score+=(cl-2)*((al/(cl-2))^stats_nsymbolcyclesweight)
						case 1,2 'perfect cycles ioc raw
							if al=cl-2 then score+=al*(al-1)
						case 3 'perfect cycles ioc flatness
							if al=cl-2 then
								score+=al*(al-1)
								cycles+=1
								cyclelengths+=al
							end if	
						case 4 'perfect cycles amount
							if al=cl-2 then score+=1
						case 5 'perfect cycles average cycle length
							if al=cl-2 then
								cycles+=1
								cyclelengths+=al
							end if
						case 6 'report cycles
							if al=cl-2 then
								score+=al*(al-1)
								cycles+=1
								cyclelengths+=al
								report+=lb
								for h=1 to cl
									if num=0 then
										report+=chr(sym(z(h)))
									else
										report+=str(sym(z(h)))
										if h<>cl then report+=" "
									end if
								next h
								report+=" ("+str(al*(al-1))+")"
							end if
					end select
				end if
			next k
		next j
	next i
	select case n
		case 0,1,4
			return score
		case 2
			return score/lioc
		case 3
			return (((cyclelengths/cycles)*((cyclelengths/cycles)-1))*cycles)/score
		case 5
			return cyclelengths/cycles
		case 6
			if score=0 then 
				report="No cycles found"
				return 0
			end if
			dim as string os
			os+="Index of coincidence:"+lb
			os+="- Raw: "+str(score)+lb
			os+="- Normalized: "+str(score/lioc)+lb
			os+="- Flatness: "+str((((cyclelengths/cycles)*((cyclelengths/cycles)-1))*cycles)/score)+lb
			os+="Cycles: "+str(cycles)+lb
			os+="Average cycle length: "+str(cyclelengths/cycles)+lb
			os+=lb
			os+="Cycles by appearance:"+lb
			os+="---------------------------------------------------------"
			report=os+report
	end select

end function

function m_primephobia(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
   
   dim as integer mod_from=2
   dim as integer mod_to=l/2
   dim as integer i,j,k,t
   dim as double score,s2
   dim as short symbols(s,l)
   for i=1 to l
      symbols(array(i),0)+=1
      symbols(array(i),symbols(array(i),0))=i
   next i
   for i=1 to s
      for j=mod_from to mod_to
         for k=1 to symbols(i,0)
            if symbols(i,k)mod j=0 then t+=1
         next k
         score+=t*(t-1)
         t=0
      next j
   next i
   if n=0 then
   	return score
   else
   	return score/(l*(l-1))
   end if
   
end function

function m_pivots(array()as long,byval l as integer,byval dx as integer,byval dy as integer,byval pl as integer)as double
	
	dim as long i,j,k,x,y,e,score
	dim as short grid(dx,dy)
	for y=1 to dy
		for x=1 to dx
			i+=1
			grid(x,y)=array(i)
			if i=l then exit for,for
		next x
	next y
	for y=1 to dy-pl
		for x=1 to dx-pl
			e=1
			for i=0 to pl-1
				j=grid(x+pl,y+i)
				k=grid(x+i,y+pl)
				if j=0 then e=0
				if k=0 then e=0
				if j<>k then e=0
				if e=0 then exit for
			next i
			if e=1 then score+=1
		next x
	next y
	return score

end function

function m_isdp(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
	
	'intersymbol distance patterns	
	dim as integer i,j,k,t,c,e
	dim as short spo(s,l)
	dim as short tmp(l)
	dim as double score
	for i=1 to l
      spo(array(i),0)+=1
      spo(array(i),spo(array(i),0))=i
	next i
	for i=1 to s
		if spo(i,0)>2 then
			t+=spo(i,0)-1
			for j=1 to spo(i,0)-2 'depth
				for k=1 to spo(i,0)-j
					spo(i,k)=abs(spo(i,k)-spo(i,k+1))
				next k
				e=1
				for k=1 to spo(i,0)-j
					if spo(i,1)<>spo(i,k) then
						e=0
						exit for
					end if		
				next k
				if e=1 then
					score+=spo(i,0)-j
					exit for
				end if
			next j
		end if
	next i
	if n=0 then
		return score
	else
		if t>0 then return score/t else return 0
	end if

end function

function m_asymmetry(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
	
	dim as integer i,j,k,l2
	dim as double a,score
	dim as double sym(s,s)
	for i=1 to l
		'l2=i
		'if l2>l/2 then l2=(l+1)-l2
		k=0
		for j=1 to i-1 'before
			'k+=1
			'if k=l2 then exit for
			sym(array(j),array(i))+=((l-1)/(i-j)) '(l2-1)/(i-j)
		next j
		k=0
		for j=i+1 to l 'after
			'k+=1
			'if k=l2 then exit for
			sym(array(i),array(j))+=((l-1)/(j-i)) '(l2-1)/(j-i)
		next j
	next i
	for i=1 to s
		for j=i+1 to s
			a=abs(sym(i,j)-sym(j,i))
			if a>1 then score+=a*a
		next j
	next i
	return score/(l*l)

end function

function cstate_symbolcount(byval instate as integer,byval l as integer,byval s as integer)as integer
	
	dim as integer i,score
	dim as integer ident(s)
	for i=1 to l
		if ident(cstate(instate,i))=0 then
			score+=1
			ident(cstate(instate,i))=1
		end if	
	next i
	return score
	
end function

function cstate_nba(byval instate as integer,byval outstate as integer,byval l as integer,byval s as integer)as integer
	
	dim as integer i,j
	dim as integer ident(65536)
	for i=1 to l
		if ident(cstate(instate,i))=0 then
			j+=1
			cstate(outstate,i)=j
			ident(cstate(instate,i))=j
		else
			cstate(outstate,i)=ident(cstate(instate,i))
		end if
	next i
	return j
	
end function

function prime(byval n as integer)as integer
	
	if n=1 then return 0
	dim as integer i
	for i=2 to sqr(n)
		if n mod i=0 then return 0
	next i
	return 1

end function

function stdev(byval a as double,byval items as integer,sda()as double)as double
	
	dim as integer i
	dim as double mean,sd
	for i=1 to items
		mean+=sda(i)
	next i
	mean/=items
	for i=1 to items
		sd+=(sda(i)-mean)^2
	next i
	sd=sqr(sd/items)
	return (a-mean)/sd
	
end function

function gcd(byval a as integer,byval b as integer)as integer
	
  	do while b<>0
      var t=b
      b=a mod b
      a=t
  	loop
  	return a
  	
end function

function stt(byval sec as double)as string
	
	dim as double days=sec/86400
	dim as double hours=frac(days)*24
	dim as double mins=frac(hours)*60
	dim as double secs=frac(mins)*60
	if days>=1 then return " Days: "+format(days,"0.00")
	if hours>=1 then return " Hours: "+format(hours,"0.00")
	if mins>=1 then return " Minutes: "+format(mins,"0.00")
	return " Seconds: "+format(secs,"0.00")

end function

function yesno(byval a as integer)as string
	
	if a=0 then return "No"
	if a=1 then return "Yes"
	return "Error"
	
end function

function fastpow1_single(byval x as single,byval b as single)as single
	
	'by richard @ freebasic.net forum
	'---------------------------------------------------------------------------------------
	'you can change the internal value of n beyond 11 to make a bigger log2 table with lower errors.
	'there is no speed penalty for changing n, just the static table storage requirement.
	'since the minimum error is fixed at about 0.5% by the quadratic antilog approximation,
	'you only need to increase n if you are using higher values of b, say greater than 4.
	'---------------------------------------------------------------------------------------
    #define n 11     ' number of bits used to address lu table
    #define max_addr ( 2^n - 1 ) ' also used as table index address mask
    static as single table( 0 to max_addr )  ' log2 fractional mantissa table
    static as short i = 0
    if i = 0 then           ' initialise the table on first call
        for i = 0 to 2^n - 1
            table( i ) = log( 1 + i / 2^n ) / log( 2 )  ' table of log2
        next i
    end if  ' i is now non-zero so will not initialise again
    ' ieee 754 single   seeeeeeeefffffffffffffffffffffff ' sign_log_linear format
    #define frac_bits 23 ' bits used to store fraction, ignoring implicit msb
    #define bias 127 ' the exponent that makes a single range from 1.0 to 1.999
    dim as long ptr fp = cptr( long ptr, @x )   ' pointer to bit pattern of x
    dim as single expo = ( ( *fp shr frac_bits ) - bias ) ' integer part of log2
    expo += table( ( *fp shr ( frac_bits - n ) ) and max_addr ) ' add mantissa
    x = expo * b
    '-------- approximate 2^x, is antilog2( x ) ------------------------
    expo = int( x ) ' this integer part adjusts the exponent of alog2(x)
    x -= expo   ' x reduced to fraction in range 0.000 thru 0.999
    x = 1e0 + ( 0.6607687e0 + 0.3392313e0 * x ) * x  ' approx 2^x over range 0 to 1
    *fp += ( expo shl frac_bits ) ' restore early integer to biased log exponent
    return x
    
end function

sub generate_permutations(n as long) 
	
	'adapted from: https://rosettacode.org/wiki/Permutations#FreeBASIC
	dim as ulong i,j,k,t=1
	dim as ulong a(0 to n-1),c(0 to n-1)
	for i=2 to n
		t*=i
	next i
	redim permu(t-1,n-1)
	for j=0 to n-1
		a(j)=j+1
		permu(0,j)=a(j)
	next
	i=0
	while i<n
		if c(i)<i then
			if (i and 1)=0 then
				swap a(0),a(i)
			else
				swap a(c(i)),a(i)
			end if
			k+=1
			for j=0 to n-1
				permu(k,j)=a(j)
			next
			c(i)+=1
			i=0
		else
			c(i)=0
			i+=1
		end if
	wend
	
end sub

function crc_32(buf as byte ptr,buflen as ulong) as ulong
	
	'adapted from rosetta code
	static as ulong table(255),have_table
	dim as ulong crc,i
	if have_table=0 then
		dim as ulong j,k
		for i=0 to 255
			k=i
			for j=0 to 7
				if (k and 1) then
					k shr=1
					k xor=&hedb88320
				else
					k shr=1
				end if
				table(i)=k
			next
		next
		have_table=1
	end if
	crc=not crc 
	for i=0 to buflen-1
		crc=(crc\256)xor table((crc and &hff)xor buf[i])
	next
	return not crc
	
end function