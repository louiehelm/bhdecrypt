select case map2b(curr_symbol,k)
	case 0	
		z1=g43(sol(j),sol(j+1),sol(j+2),sol(j+3))
		if z1=0 Then
				z3=g45(sol(j),sol(j+1),sol(j+2),sol(j+3))
				If z3=0 Then
					bl2=rl(mc)
				Else
					For i=0 to mc
						z4=g46(sol(j+4),sol(j+5),sol(j+6),rl(i))
						blt=g82(z3,z4)
						If blt>bls then bls=blt:bl2=rl(i)
					Next i
				EndIf
		Else
			For i=0 to mc
				z2=g44(sol(j+4),sol(j+5),sol(j+6),rl(i))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf
		
	case 1
		z1=g43(sol(j),sol(j+1),sol(j+2),sol(j+3))
		if z1=0 Then
				z3=g45(sol(j),sol(j+1),sol(j+2),sol(j+3))
				If z3=0 Then
					bl2=rl(mc)
				Else
					For i=0 to mc
						z4=g46(sol(j+4),sol(j+5),rl(i),sol(j+7))
						blt=g82(z3,z4)
						If blt>bls then bls=blt:bl2=rl(i)
					Next i
				EndIf
		Else
			For i=0 to mc
				z2=g44(sol(j+4),sol(j+5),rl(i),sol(j+7))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf
		
	case 2
		z1=g43(sol(j),sol(j+1),sol(j+2),sol(j+3))
		if z1=0 Then
				z3=g45(sol(j),sol(j+1),sol(j+2),sol(j+3))
				If z3=0 Then
					bl2=rl(mc)
				Else
					For i=0 to mc
						z4=g46(sol(j+4),rl(i),sol(j+6),sol(j+7))
						blt=g82(z3,z4)
						If blt>bls then bls=blt:bl2=rl(i)
					Next i
				EndIf
		Else
			For i=0 to mc
				z2=g44(sol(j+4),rl(i),sol(j+6),sol(j+7))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf
		
	case 3
		z1=g43(sol(j),sol(j+1),sol(j+2),sol(j+3))
		if z1=0 Then
				z3=g45(sol(j),sol(j+1),sol(j+2),sol(j+3))
				If z3=0 Then
					bl2=rl(mc)
				Else
					For i=0 to mc
						z4=g46(rl(i),sol(j+5),sol(j+6),sol(j+7))
						blt=g82(z3,z4)
						If blt>bls then bls=blt:bl2=rl(i)
					Next i
				EndIf
		Else
			For i=0 to mc
				z2=g44(rl(i),sol(j+5),sol(j+6),sol(j+7))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf		

	case 4		
		z2=g44(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
		if z2=0 Then
				z4=g46(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
				If z4=0 Then
					bl2=rl(mc)
				Else
					For i=0 to mc
						z3=g45(sol(j),sol(j+1),sol(j+2),rl(i))
						blt=g82(z3,z4)
						If blt>bls then bls=blt:bl2=rl(i)
					Next i
				EndIf
		Else
			For i=0 to mc
				z1=g43(sol(j),sol(j+1),sol(j+2),rl(i))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf
		
	case 5
		z2=g44(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
		if z2=0 Then
				z4=g46(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
				If z4=0 Then
					bl2=rl(mc)
				Else
					For i=0 to mc
						z3=g45(sol(j),sol(j+1),rl(i),sol(j+3))
						blt=g82(z3,z4)
						If blt>bls then bls=blt:bl2=rl(i)
					Next i
				EndIf
		Else
			For i=0 to mc
				z1=g43(sol(j),sol(j+1),rl(i),sol(j+3))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf

	case 6
		z2=g44(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
		if z2=0 Then
				z4=g46(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
				If z4=0 Then
					bl2=rl(mc)
				Else
					For i=0 to mc
						z3=g45(sol(j),rl(i),sol(j+2),sol(j+3))
						blt=g82(z3,z4)
						If blt>bls then bls=blt:bl2=rl(i)
					Next i
				EndIf
		Else
			For i=0 to mc
				z1=g43(sol(j),rl(i),sol(j+2),sol(j+3))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf

	case 7
		z2=g44(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
		if z2=0 Then
				z4=g46(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
				If z4=0 Then
					bl2=rl(mc)
				Else
					For i=0 to mc
						z3=g45(rl(i),sol(j+1),sol(j+2),sol(j+3))
						blt=g82(z3,z4)
						If blt>bls then bls=blt:bl2=rl(i)
					Next i
				EndIf
		Else
			For i=0 to mc
				z1=g43(rl(i),sol(j+1),sol(j+2),sol(j+3))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf

End select