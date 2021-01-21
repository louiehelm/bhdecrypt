
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
Case 0: blt=gxa9(z1,z2)
Case 1: blt=gxb9(z1,z2)
Case 2: blt=gxc9(z1,z2)
Case 3: blt=gxd9(z1,z2)
Case 4: blt=gxe9(z1,z2)
Case 5: blt=gxf9(z1,z2)
Case 6: blt=gxg9(z1,z2)
Case 7: blt=gxh9(z1,z2)
Case 8: blt=gxi9(z1,z2)
Case 9: blt=gxj9(z1,z2)
Case 10:blt=gxk9(z1,z2)
Case 11:blt=gxl9(z1,z2)
Case 12:blt=gxm9(z1,z2)
Case 13:blt=gxn9(z1,z2)
Case 14:blt=gxo9(z1,z2)
Case 15:blt=gxp9(z1,z2)
Case 16:blt=gxq9(z1,z2)
Case 17:blt=gxr9(z1,z2)
Case 18:blt=gxs9(z1,z2)
Case 19:blt=gxt9(z1,z2)
Case 20:blt=gxu9(z1,z2)
Case 21:blt=gxv9(z1,z2)
Case 22:blt=gxw9(z1,z2)
Case 23:blt=gxx9(z1,z2)
Case 24:blt=gxy9(z1,z2)
Case 25:blt=gxz9(z1,z2)
End select

if blt <> 0 then
If ninegramformat = 6 then
	if z_offset = (blt and &H3) then 
      blt = blt shr 2  ' correct quartile, shift quartile designation away to recover true score
	else
   	blt = 0   ' ignore scores in wrong quartiles
	endif
elseif ninegramformat = 8 then
	if z_offset = 1 then
		blt = blt shr 4
	else
		blt and= &HF
	endif
elseif ninegramformat = 4 then
   if (z_offset = 0) andalso (blt > ninegram_quartile) then   ' ignore scores in wrong quartiles
      blt = 0
   elseif (z_offset = 1) andalso ((blt > 2*ninegram_quartile) orelse (blt <= ninegram_quartile))  then
      blt = 0
   elseif (z_offset = 2) andalso ((blt > 3*ninegram_quartile) orelse (blt <= 2*ninegram_quartile))  then
      blt = 0
   elseif (z_offset = 3) andalso (blt <= 3*ninegram_quartile)  then
      blt = 0
   else
   	blt -= z_offset*ninegram_quartile-ninegram_offset ' move score down to correct zone  + add in offset (which is - here)
   endif
elseif ninegramformat = 5 then
	if (z_offset = 0) andalso (blt > ninegram_quartile) then   ' ignore scores in wrong quartiles
      blt = 0
	elseif (z_offset = 1) andalso ((blt > 2*ninegram_quartile) orelse (blt <= ninegram_quartile))  then
      blt = 0
	elseif (z_offset = 2) andalso ((blt > 3*ninegram_quartile) orelse (blt <= 2*ninegram_quartile))  then
      blt = 0
	elseif (z_offset = 3) andalso ((blt > 4*ninegram_quartile) orelse (blt <= 3*ninegram_quartile))  then
      blt = 0
	elseif (z_offset = 4) andalso ((blt > 5*ninegram_quartile) orelse (blt <= 4*ninegram_quartile))  then
      blt = 0
	elseif (z_offset = 5) andalso ((blt > 6*ninegram_quartile) orelse (blt <= 5*ninegram_quartile))  then
      blt = 0
	elseif (z_offset = 6) andalso ((blt > 7*ninegram_quartile) orelse (blt <= 6*ninegram_quartile))  then
      blt = 0
	elseif (z_offset = 7) andalso ((blt > 8*ninegram_quartile) orelse (blt <= 7*ninegram_quartile))  then
      blt = 0
	elseif (z_offset = 8) andalso (blt <= 8*ninegram_quartile)  then
      blt = 0
	else
   	blt -= z_offset*ninegram_quartile ' can ignore offset for triple folded since it's always 0 -ninegram_offset ' move score down to correct zone  + add in offset (which is - here)
   endif
endif
endif