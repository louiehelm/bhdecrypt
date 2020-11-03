select case map2b(curr_symbol,k)
	case 0 '7 '0	
'		z1=g47(sol(j),sol(j+1),sol(j+2),sol(j+3))
		if z1=0 Then
			bl2=rl(mc)
		Else
			For i=0 to mc
				z2=g47(sol(j+4),sol(j+5),sol(j+6),rl(i))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf
		
	case 1 '6 '1
'		z1=g47(sol(j),sol(j+1),sol(j+2),sol(j+3))
		if z1=0 Then
			bl2=rl(mc)
		Else
			For i=0 to mc
				z2=g47(sol(j+4),sol(j+5),rl(i),sol(j+7))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf
		
	case 2 '5 '2
'		z1=g47(sol(j),sol(j+1),sol(j+2),sol(j+3))
		if z1=0 Then
			bl2=rl(mc)
		Else
			For i=0 to mc
				z2=g47(sol(j+4),rl(i),sol(j+6),sol(j+7))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf
		
	case 3 '4 '3
'		z1=g47(sol(j),sol(j+1),sol(j+2),sol(j+3))
		if z1=0 Then
			bl2=rl(mc)
		Else
			For i=0 to mc
				z2=g47(rl(i),sol(j+5),sol(j+6),sol(j+7))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf		

	case 4 '3 '4		
'		z2=g47(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
		if z2=0 Then
			bl2=rl(mc)
		Else
			For i=0 to mc
				z1=g47(sol(j),sol(j+1),sol(j+2),rl(i))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf
		
	case 5 '2 '5
'		z2=g47(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
		if z2=0 Then
			bl2=rl(mc)
		Else
			For i=0 to mc
				z1=g47(sol(j),sol(j+1),rl(i),sol(j+3))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf

	case 6' 1 '6
'		z2=g47(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
		if z2=0 Then
			bl2=rl(mc)
		Else
			For i=0 to mc
				z1=g47(sol(j),rl(i),sol(j+2),sol(j+3))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf

	case 7 '0 '7
'		z2=g47(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
		if z2=0 Then
			bl2=rl(mc)
		Else
			For i=0 to mc
				z1=g47(rl(i),sol(j+1),sol(j+2),sol(j+3))
				blt=g81(z1,z2)
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		EndIf

End select