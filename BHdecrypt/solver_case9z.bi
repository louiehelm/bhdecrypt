
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
case 0: z=gxa9(z1,z2)
Case 1: z=gxb9(z1,z2)
Case 2: z=gxc9(z1,z2)
Case 3: z=gxd9(z1,z2)
Case 4: z=gxe9(z1,z2)
Case 5: z=gxf9(z1,z2)
Case 6: z=gxg9(z1,z2)
Case 7: z=gxh9(z1,z2)
Case 8: z=gxi9(z1,z2)
Case 9: z=gxj9(z1,z2)
Case 10:z=gxk9(z1,z2)
Case 11:z=gxl9(z1,z2)
Case 12:z=gxm9(z1,z2)
Case 13:z=gxn9(z1,z2)
Case 14:z=gxo9(z1,z2)
Case 15:z=gxp9(z1,z2)
Case 16:z=gxq9(z1,z2)
Case 17:z=gxr9(z1,z2)
Case 18:z=gxs9(z1,z2)
Case 19:z=gxt9(z1,z2)
Case 20:z=gxu9(z1,z2)
Case 21:z=gxv9(z1,z2)
Case 22:z=gxw9(z1,z2)
Case 23:z=gxx9(z1,z2)
Case 24:z=gxy9(z1,z2)
Case 25:z=gxz9(z1,z2)
End select

if z <> 0 then
If ninegramformat = 6 then
	if z_offset = (z and &H3) then
      z = z shr 2  ' correct quartile, shift quartile designation away to recover true score
	else
   	z = 0   ' ignore scores in wrong quartiles
	endif
elseif ninegramformat = 8 then
	if z_offset = 1 then
		z = z shr 4
	else
		z and= &HF
	endif
elseif ninegramformat = 4 then
   if (z_offset = 0) andalso (z > ninegram_quartile) then   ' ignore scores in wrong quartiles
      z = 0
   elseif (z_offset = 1) andalso ((z > 2*ninegram_quartile) orelse (z <= ninegram_quartile))  then
      z = 0
   elseif (z_offset = 2) andalso ((z > 3*ninegram_quartile) orelse (z <= 2*ninegram_quartile))  then
      z = 0
   elseif (z_offset = 3) andalso (z <= 3*ninegram_quartile)  then
      z = 0
   else
   	z -= z_offset*ninegram_quartile-ninegram_offset ' move score down to correct zone  + add in offset (which is - here)
   endif
elseif ninegramformat = 5 then
	if (z_offset = 0) andalso (z > ninegram_quartile) then   ' ignore scores in wrong quartiles
      z = 0
	elseif (z_offset = 1) andalso ((z > 2*ninegram_quartile) orelse (z <= ninegram_quartile))  then
      z = 0
	elseif (z_offset = 2) andalso ((z > 3*ninegram_quartile) orelse (z <= 2*ninegram_quartile))  then
      z = 0
	elseif (z_offset = 3) andalso ((z > 4*ninegram_quartile) orelse (z <= 3*ninegram_quartile))  then
      z = 0
	elseif (z_offset = 4) andalso ((z > 5*ninegram_quartile) orelse (z <= 4*ninegram_quartile))  then
      z = 0
	elseif (z_offset = 5) andalso ((z > 6*ninegram_quartile) orelse (z <= 5*ninegram_quartile))  then
      z = 0
	elseif (z_offset = 6) andalso ((z > 7*ninegram_quartile) orelse (z <= 6*ninegram_quartile))  then
      z = 0
	elseif (z_offset = 7) andalso ((z > 8*ninegram_quartile) orelse (z <= 7*ninegram_quartile))  then
      z = 0
	elseif (z_offset = 8) andalso (z <= 8*ninegram_quartile)  then
      z = 0
	else
   	z -= z_offset*ninegram_quartile ' can ignore offset for triple folded since it's always 0 -ninegram_offset ' move score down to correct zone  + add in offset (which is - here)
   endif
endif
endif