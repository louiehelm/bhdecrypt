select case map2b(curr_symbol,k)
	case 0
		z1 = g31(sol(j),sol(j+1),sol(j+2))
		If z1 <> 0 Then
			for i=0 to mc
				z2 = g32(sol(j+3),sol(j+4),rl(i))
				If z2 <> 0 Then blt=g61(z1,z2) else blt=0
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		Else
			bl2=rl(mc)
		EndIf
	case 1
		z1 = g31(sol(j),sol(j+1),sol(j+2))
		If z1 <> 0 Then			
			for i=0 to mc
				z2 = g32(sol(j+3),rl(i),sol(j+5))
				If z2 <> 0 Then blt=g61(z1,z2) else blt=0
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf
	case 2
		z1 = g31(sol(j),sol(j+1),sol(j+2))
		If z1 <> 0 Then			
			for i=0 to mc
				z2 = g32(rl(i),sol(j+4),sol(j+5))
				If z2 <> 0 Then blt=g61(z1,z2) else blt=0
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf
	Case 3
		z2 = g32(sol(j+3),sol(j+4),sol(j+5))
		If z2 <> 0 Then			
			for i=0 to mc
				z1 = g31(sol(j),sol(j+1),rl(i))
				If z1 <> 0 Then blt=g61(z1,z2) else blt=0
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf
	case 4
		z2 = g32(sol(j+3),sol(j+4),sol(j+5))
		If z2 <> 0 Then			
			for i=0 to mc
				z1 = g31(sol(j),rl(i),sol(j+2))
				If z1 <> 0 Then blt=g61(z1,z2) else blt=0
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf
	Case 5
		z2 = g32(sol(j+3),sol(j+4),sol(j+5))
		If z2 <> 0 Then
			for i=0 to mc
				z1 = g31(rl(i),sol(j+1),sol(j+2))
				If z1 <> 0 Then blt=g61(z1,z2) else blt=0
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf
End select