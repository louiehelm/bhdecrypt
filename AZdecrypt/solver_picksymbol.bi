if use_cribs=0 then
	
	if mi=s then mi=1 else mi+=1
	curr_symbol=maps(mi)
   mj+=1
   state=48271*state and 2147483647
   maps2(mj)=1+s*state shr 31
   if mj=s then
   	mj=0
   	for i=1 to s-1 step 2
			swap maps(maps2(i)),maps(maps2(i+1))
   	next i
   end if
	
	'if mi=s then mi=1 else mi+=1
	'curr_symbol=maps(mi)
   'mj+=1
   'if mj=s then
   '	mj=0
   '	for i=1 to s/2
   '		state=48271*state and 2147483647
	'		j=1+s*state shr 31
	'		state=48271*state and 2147483647
	'		k=1+s*state shr 31
	'		swap maps(j),maps(k)
   '	next i
   'end if
   
   'curr_symbol=pcg(tn).range(1,s)
 	
	'do
	'	state=48271*state and 2147483647
	'	i=1+s*state shr 31
	'loop until i<>curr_symbol
	'curr_symbol=i
	
	'state=48271*state and 2147483647
	'curr_symbol=1+s*state shr 31
	
	'do
	'	e=0
	'	curr_symbol+=1
	'	if curr_symbol>s then curr_symbol=1
	'	if cf(curr_symbol)>=iterations/solvesub_matchweight then
	'		e=1
	'		cf(curr_symbol)-=1
	'		if it<iterations then 'skip iteration
	'			it+=1
	'			thread(tn).iterations_completed+=1
	'			old_score-=temp*map1(curr_symbol,0)*onesixl
	'			temp-=temp_min
	'		end if
	'	end if
	'loop until e=0
	
	'if curr_symbol=s then curr_symbol=1 else curr_symbol+=1
	
else
	
	if use_cribs<2 then 'for "row bound fragments" solver
	
		do
			
			if mi=s then mi=1 else mi+=1
			curr_symbol=maps(mi)
		   mj+=1
		   state=48271*state and 2147483647
		   maps2(mj)=1+s*state shr 31
		   if mj=s then
		   	mj=0
		   	for i=1 to s-1 step 2
					swap maps(maps2(i)),maps(maps2(i+1))
		   	next i
		   end if
			
			'state=48271*state and 2147483647
			'curr_symbol=1+s*state shr 31
			
			'if curr_symbol=s then curr_symbol=1 else curr_symbol+=1
			
		loop until cribkey(curr_symbol)=0
		
	else
	'	
		curr_symbol=1
	'	new_letter=stl(1)
	'	old_letter=stl(1)
	'	'older_letter=stl(1)
	'	
	end if
	
end if