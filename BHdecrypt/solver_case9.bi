
If ninegramformat = 4 orelse ninegramformat = 6 then
   z_offset=0
   if (z1 and &H100000) > 0 then
   	z_offset = 1
   	z1 and= &HFFFFF
   endif
   if (z2 and &H100000) > 0 then
   	z_offset +=2
   	z2 and= &HFFFFF
   endif
elseif ninegramformat = 8 then
	z_offset=0
	if z1 > ninegram_half(sol(j+4)) then
		z_offset = 1
		z1 -= ninegram_half(sol(j+4))
	endif		
elseif ninegramformat = 5 then
   if (z1 and &H1000000) > 0 then
   	z1 and= &HFFFFF
	   if (z2 and &H1000000) > 0 then
	   	z_offset = 8
	   	z2 and= &HFFFFF
	   elseif (z2 and &H100000) > 0 then
	   	z_offset = 6
	   	z2 and= &HFFFFF
	   else
	   	z_offset = 3
	   endif
   elseif (z1 and &H100000) > 0 then
   	z1 and= &HFFFFF
   	if (z2 and &H1000000) > 0 then
	   	z_offset = 7
	   	z2 and= &HFFFFF
   	elseif (z2 and &H100000) > 0 then
	   	z_offset = 4
	   	z2 and= &HFFFFF
   	else
	   	z_offset = 1
   	endif
   else
  		if (z2 and &H1000000) > 0 then
	   	z_offset = 5
	   	z2 and= &HFFFFF
  		elseif (z2 and &H100000) > 0 then
	   	z_offset = 2
	   	z2 and= &HFFFFF
  		else
	   	z_offset = 0
  		endif
   endif
endif

Select case sol(j+4)
Case 0: ngrams(j)=gxa9(z1,z2)
Case 1: ngrams(j)=gxb9(z1,z2)
Case 2: ngrams(j)=gxc9(z1,z2)
Case 3: ngrams(j)=gxd9(z1,z2)
Case 4: ngrams(j)=gxe9(z1,z2)
Case 5: ngrams(j)=gxf9(z1,z2)
Case 6: ngrams(j)=gxg9(z1,z2)
Case 7: ngrams(j)=gxh9(z1,z2)
Case 8: ngrams(j)=gxi9(z1,z2)
Case 9: ngrams(j)=gxj9(z1,z2)
Case 10:ngrams(j)=gxk9(z1,z2)
Case 11:ngrams(j)=gxl9(z1,z2)
Case 12:ngrams(j)=gxm9(z1,z2)
Case 13:ngrams(j)=gxn9(z1,z2)
Case 14:ngrams(j)=gxo9(z1,z2)
Case 15:ngrams(j)=gxp9(z1,z2)
Case 16:ngrams(j)=gxq9(z1,z2)
Case 17:ngrams(j)=gxr9(z1,z2)
Case 18:ngrams(j)=gxs9(z1,z2)
Case 19:ngrams(j)=gxt9(z1,z2)
Case 20:ngrams(j)=gxu9(z1,z2)
Case 21:ngrams(j)=gxv9(z1,z2)
Case 22:ngrams(j)=gxw9(z1,z2)
Case 23:ngrams(j)=gxx9(z1,z2)
Case 24:ngrams(j)=gxy9(z1,z2)
Case 25:ngrams(j)=gxz9(z1,z2)
End select

if ngrams(j) <> 0 then

If ninegramformat = 6 then
	if z_offset = (ngrams(j) and &H3) then 
      ngrams(j) = ngrams(j) shr 2  ' correct quartile, shift quartile designation away to recover true score
	else
   	ngrams(j) = 0   ' ignore scores in wrong quartiles
	endif
elseif ninegramformat = 8 then
	if z_offset = 1 then
		ngrams(j) = ngrams(j) shr 4
	else
		ngrams(j) and= &HF
	endif
elseif ninegramformat = 4 then
   if (z_offset = 0) andalso (ngrams(j) > ninegram_quartile) then   ' ignore scores in wrong quartiles
      ngrams(j) = 0
   elseif (z_offset = 1) andalso ((ngrams(j) > 2*ninegram_quartile) orelse (ngrams(j) <= ninegram_quartile))  then
      ngrams(j) = 0
   elseif (z_offset = 2) andalso ((ngrams(j) > 3*ninegram_quartile) orelse (ngrams(j) <= 2*ninegram_quartile))  then
      ngrams(j) = 0
   elseif (z_offset = 3) andalso (ngrams(j) <= 3*ninegram_quartile)  then
      ngrams(j) = 0
   else
   	ngrams(j) -= z_offset*ninegram_quartile-ninegram_offset ' move score down to correct zone  + add in offset (which is - here)
   endif
elseif ninegramformat = 5 then
	if (z_offset = 0) andalso (ngrams(j) > ninegram_quartile) then   ' ignore scores in wrong quartiles
      ngrams(j) = 0
	elseif (z_offset = 1) andalso ((ngrams(j) > 2*ninegram_quartile) orelse (ngrams(j) <= ninegram_quartile))  then
      ngrams(j) = 0
	elseif (z_offset = 2) andalso ((ngrams(j) > 3*ninegram_quartile) orelse (ngrams(j) <= 2*ninegram_quartile))  then
      ngrams(j) = 0
	elseif (z_offset = 3) andalso ((ngrams(j) > 4*ninegram_quartile) orelse (ngrams(j) <= 3*ninegram_quartile))  then
      ngrams(j) = 0
	elseif (z_offset = 4) andalso ((ngrams(j) > 5*ninegram_quartile) orelse (ngrams(j) <= 4*ninegram_quartile))  then
      ngrams(j) = 0
	elseif (z_offset = 5) andalso ((ngrams(j) > 6*ninegram_quartile) orelse (ngrams(j) <= 5*ninegram_quartile))  then
      ngrams(j) = 0
	elseif (z_offset = 6) andalso ((ngrams(j) > 7*ninegram_quartile) orelse (ngrams(j) <= 6*ninegram_quartile))  then
      ngrams(j) = 0
	elseif (z_offset = 7) andalso ((ngrams(j) > 8*ninegram_quartile) orelse (ngrams(j) <= 7*ninegram_quartile))  then
      ngrams(j) = 0
	elseif (z_offset = 8) andalso (ngrams(j) <= 8*ninegram_quartile)  then
      ngrams(j) = 0
	else
   	ngrams(j) -= z_offset*ninegram_quartile ' can ignore offset for triple folded since it's always 0 -ninegram_offset ' move score down to correct zone  + add in offset (which is - here)
   endif
endif
endif