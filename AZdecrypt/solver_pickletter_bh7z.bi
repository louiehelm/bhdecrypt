select case map2b(curr_symbol,k)
	case 0
		z1=g41(sol(j),sol(j+1),sol(j+2),sol(j+3))
		If z1 <> 0 Then
			for i=0 to mc
				z2=g42(sol(j+3),sol(j+4),sol(j+5),rl(i))
				If z2 <> 0 Then
					Select case sol(j+3)
						Case 0: blt=gxa7(z1,z2)
						Case 1: blt=gxb7(z1,z2)
						Case 2: blt=gxc7(z1,z2)
						Case 3: blt=gxd7(z1,z2)
						Case 4: blt=gxe7(z1,z2)
						Case 5: blt=gxf7(z1,z2)
						Case 6: blt=gxg7(z1,z2)
						Case 7: blt=gxh7(z1,z2)
						Case 8: blt=gxi7(z1,z2)
						Case 9: blt=gxj7(z1,z2)
						Case 10:blt=gxk7(z1,z2)
						Case 11:blt=gxl7(z1,z2)
						Case 12:blt=gxm7(z1,z2)
						Case 13:blt=gxn7(z1,z2)
						Case 14:blt=gxo7(z1,z2)
						Case 15:blt=gxp7(z1,z2)
						Case 16:blt=gxq7(z1,z2)
						Case 17:blt=gxr7(z1,z2)
						Case 18:blt=gxs7(z1,z2)
						Case 19:blt=gxt7(z1,z2)
						Case 20:blt=gxu7(z1,z2)
						Case 21:blt=gxv7(z1,z2)
						Case 22:blt=gxw7(z1,z2)
						Case 23:blt=gxx7(z1,z2)
						Case 24:blt=gxy7(z1,z2)
						Case 25:blt=gxz7(z1,z2)
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf
		
	case 1
		z1=g41(sol(j),sol(j+1),sol(j+2),sol(j+3))
		If z1 <> 0 Then
			for i=0 to mc
				z2=g42(sol(j+3),sol(j+4),rl(i),sol(j+6))
				If z2 <> 0 Then
					Select case sol(j+3)
						Case 0: blt=gxa7(z1,z2)
						Case 1: blt=gxb7(z1,z2)
						Case 2: blt=gxc7(z1,z2)
						Case 3: blt=gxd7(z1,z2)
						Case 4: blt=gxe7(z1,z2)
						Case 5: blt=gxf7(z1,z2)
						Case 6: blt=gxg7(z1,z2)
						Case 7: blt=gxh7(z1,z2)
						Case 8: blt=gxi7(z1,z2)
						Case 9: blt=gxj7(z1,z2)
						Case 10:blt=gxk7(z1,z2)
						Case 11:blt=gxl7(z1,z2)
						Case 12:blt=gxm7(z1,z2)
						Case 13:blt=gxn7(z1,z2)
						Case 14:blt=gxo7(z1,z2)
						Case 15:blt=gxp7(z1,z2)
						Case 16:blt=gxq7(z1,z2)
						Case 17:blt=gxr7(z1,z2)
						Case 18:blt=gxs7(z1,z2)
						Case 19:blt=gxt7(z1,z2)
						Case 20:blt=gxu7(z1,z2)
						Case 21:blt=gxv7(z1,z2)
						Case 22:blt=gxw7(z1,z2)
						Case 23:blt=gxx7(z1,z2)
						Case 24:blt=gxy7(z1,z2)
						Case 25:blt=gxz7(z1,z2)
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf

	case 2
		z1=g41(sol(j),sol(j+1),sol(j+2),sol(j+3))
		If z1 <> 0 Then
			for i=0 to mc
				z2=g42(sol(j+3),rl(i),sol(j+5),sol(j+6))
				If z2 <> 0 Then
					Select case sol(j+3)
						Case 0: blt=gxa7(z1,z2)
						Case 1: blt=gxb7(z1,z2)
						Case 2: blt=gxc7(z1,z2)
						Case 3: blt=gxd7(z1,z2)
						Case 4: blt=gxe7(z1,z2)
						Case 5: blt=gxf7(z1,z2)
						Case 6: blt=gxg7(z1,z2)
						Case 7: blt=gxh7(z1,z2)
						Case 8: blt=gxi7(z1,z2)
						Case 9: blt=gxj7(z1,z2)
						Case 10:blt=gxk7(z1,z2)
						Case 11:blt=gxl7(z1,z2)
						Case 12:blt=gxm7(z1,z2)
						Case 13:blt=gxn7(z1,z2)
						Case 14:blt=gxo7(z1,z2)
						Case 15:blt=gxp7(z1,z2)
						Case 16:blt=gxq7(z1,z2)
						Case 17:blt=gxr7(z1,z2)
						Case 18:blt=gxs7(z1,z2)
						Case 19:blt=gxt7(z1,z2)
						Case 20:blt=gxu7(z1,z2)
						Case 21:blt=gxv7(z1,z2)
						Case 22:blt=gxw7(z1,z2)
						Case 23:blt=gxx7(z1,z2)
						Case 24:blt=gxy7(z1,z2)
						Case 25:blt=gxz7(z1,z2)
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf

	case 3
		for i=0 to mc
			z1=g41(sol(j),sol(j+1),sol(j+2),rl(i))
			If z1 <> 0 Then
				z2=g42(rl(i),sol(j+4),sol(j+5),sol(j+6))
				If z2 <> 0 Then
					Select case rl(i)
						Case 0: blt=gxa7(z1,z2)
						Case 1: blt=gxb7(z1,z2)
						Case 2: blt=gxc7(z1,z2)
						Case 3: blt=gxd7(z1,z2)
						Case 4: blt=gxe7(z1,z2)
						Case 5: blt=gxf7(z1,z2)
						Case 6: blt=gxg7(z1,z2)
						Case 7: blt=gxh7(z1,z2)
						Case 8: blt=gxi7(z1,z2)
						Case 9: blt=gxj7(z1,z2)
						Case 10:blt=gxk7(z1,z2)
						Case 11:blt=gxl7(z1,z2)
						Case 12:blt=gxm7(z1,z2)
						Case 13:blt=gxn7(z1,z2)
						Case 14:blt=gxo7(z1,z2)
						Case 15:blt=gxp7(z1,z2)
						Case 16:blt=gxq7(z1,z2)
						Case 17:blt=gxr7(z1,z2)
						Case 18:blt=gxs7(z1,z2)
						Case 19:blt=gxt7(z1,z2)
						Case 20:blt=gxu7(z1,z2)
						Case 21:blt=gxv7(z1,z2)
						Case 22:blt=gxw7(z1,z2)
						Case 23:blt=gxx7(z1,z2)
						Case 24:blt=gxy7(z1,z2)
						Case 25:blt=gxz7(z1,z2)
					End Select
				Else
					blt=0
				EndIf
			Else
				blt=0
			EndIf
			
			if blt>bls then bls=blt:bl2=rl(i)		
		next i

	case 4
		z2=g42(sol(j+3),sol(j+4),sol(j+5),sol(j+6))
		If z2 <> 0 Then
			for i=0 to mc
				z1=g41(sol(j),sol(j+1),rl(i),sol(j+3))
				If z1 <> 0 Then
					Select case sol(j+3)
						Case 0: blt=gxa7(z1,z2)
						Case 1: blt=gxb7(z1,z2)
						Case 2: blt=gxc7(z1,z2)
						Case 3: blt=gxd7(z1,z2)
						Case 4: blt=gxe7(z1,z2)
						Case 5: blt=gxf7(z1,z2)
						Case 6: blt=gxg7(z1,z2)
						Case 7: blt=gxh7(z1,z2)
						Case 8: blt=gxi7(z1,z2)
						Case 9: blt=gxj7(z1,z2)
						Case 10:blt=gxk7(z1,z2)
						Case 11:blt=gxl7(z1,z2)
						Case 12:blt=gxm7(z1,z2)
						Case 13:blt=gxn7(z1,z2)
						Case 14:blt=gxo7(z1,z2)
						Case 15:blt=gxp7(z1,z2)
						Case 16:blt=gxq7(z1,z2)
						Case 17:blt=gxr7(z1,z2)
						Case 18:blt=gxs7(z1,z2)
						Case 19:blt=gxt7(z1,z2)
						Case 20:blt=gxu7(z1,z2)
						Case 21:blt=gxv7(z1,z2)
						Case 22:blt=gxw7(z1,z2)
						Case 23:blt=gxx7(z1,z2)
						Case 24:blt=gxy7(z1,z2)
						Case 25:blt=gxz7(z1,z2)
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf

	case 5
		z2=g42(sol(j+3),sol(j+4),sol(j+5),sol(j+6))
		If z2 <> 0 Then
			for i=0 to mc
				z1=g41(sol(j),rl(i),sol(j+2),sol(j+3))
				If z1 <> 0 Then
					Select case sol(j+3)
						Case 0: blt=gxa7(z1,z2)
						Case 1: blt=gxb7(z1,z2)
						Case 2: blt=gxc7(z1,z2)
						Case 3: blt=gxd7(z1,z2)
						Case 4: blt=gxe7(z1,z2)
						Case 5: blt=gxf7(z1,z2)
						Case 6: blt=gxg7(z1,z2)
						Case 7: blt=gxh7(z1,z2)
						Case 8: blt=gxi7(z1,z2)
						Case 9: blt=gxj7(z1,z2)
						Case 10:blt=gxk7(z1,z2)
						Case 11:blt=gxl7(z1,z2)
						Case 12:blt=gxm7(z1,z2)
						Case 13:blt=gxn7(z1,z2)
						Case 14:blt=gxo7(z1,z2)
						Case 15:blt=gxp7(z1,z2)
						Case 16:blt=gxq7(z1,z2)
						Case 17:blt=gxr7(z1,z2)
						Case 18:blt=gxs7(z1,z2)
						Case 19:blt=gxt7(z1,z2)
						Case 20:blt=gxu7(z1,z2)
						Case 21:blt=gxv7(z1,z2)
						Case 22:blt=gxw7(z1,z2)
						Case 23:blt=gxx7(z1,z2)
						Case 24:blt=gxy7(z1,z2)
						Case 25:blt=gxz7(z1,z2)
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf

	case 6
		z2=g42(sol(j+3),sol(j+4),sol(j+5),sol(j+6))
		If z2 <> 0 Then
			for i=0 to mc
				z1=g41(rl(i),sol(j+1),sol(j+2),sol(j+3))
				If z1 <> 0 Then
					Select case sol(j+3)
						Case 0: blt=gxa7(z1,z2)
						Case 1: blt=gxb7(z1,z2)
						Case 2: blt=gxc7(z1,z2)
						Case 3: blt=gxd7(z1,z2)
						Case 4: blt=gxe7(z1,z2)
						Case 5: blt=gxf7(z1,z2)
						Case 6: blt=gxg7(z1,z2)
						Case 7: blt=gxh7(z1,z2)
						Case 8: blt=gxi7(z1,z2)
						Case 9: blt=gxj7(z1,z2)
						Case 10:blt=gxk7(z1,z2)
						Case 11:blt=gxl7(z1,z2)
						Case 12:blt=gxm7(z1,z2)
						Case 13:blt=gxn7(z1,z2)
						Case 14:blt=gxo7(z1,z2)
						Case 15:blt=gxp7(z1,z2)
						Case 16:blt=gxq7(z1,z2)
						Case 17:blt=gxr7(z1,z2)
						Case 18:blt=gxs7(z1,z2)
						Case 19:blt=gxt7(z1,z2)
						Case 20:blt=gxu7(z1,z2)
						Case 21:blt=gxv7(z1,z2)
						Case 22:blt=gxw7(z1,z2)
						Case 23:blt=gxx7(z1,z2)
						Case 24:blt=gxy7(z1,z2)
						Case 25:blt=gxz7(z1,z2)
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		Else
			bl2=rl(mc)
		EndIf
		
End select