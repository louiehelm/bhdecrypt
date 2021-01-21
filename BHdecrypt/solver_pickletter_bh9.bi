select case map2b(curr_symbol,k)
	case 0
'		z1=g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
'		If z1 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),rl(i))
				Else
					z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),rl(i))
				EndIf
				If z2 <> 0 then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=rl(i)		
			next i
'		Else
'			new_letter=rl(mc)
'		EndIf
		
	case 1
'		z1=g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
'		If z1 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),sol(j+5),sol(j+6),rl(i),sol(j+8))
				Else
					z2 = g54(sol(j+4),sol(j+5),sol(j+6),rl(i),sol(j+8))
				EndIf
				If z2 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=rl(i)		
			next i
'		Else
'			new_letter=rl(mc)
'		EndIf

	case 2
'		z1=g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
'		If z1 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),sol(j+5),rl(i),sol(j+7),sol(j+8))
				Else
					z2 = g54(sol(j+4),sol(j+5),rl(i),sol(j+7),sol(j+8))
				EndIf
				If z2 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=rl(i)		
			next i
'		Else
'			new_letter=rl(mc)
'		EndIf

	case 3
'		z1=g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
'		If z1 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),rl(i),sol(j+6),sol(j+7),sol(j+8))
				Else
					z2 = g54(sol(j+4),rl(i),sol(j+6),sol(j+7),sol(j+8))
				EndIf
				If z2 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=rl(i)		
			next i
'		Else
'			new_letter=rl(mc)
'		EndIf
		
	case 4
		for i=0 to mc
			If ninegramformat = 0 Then
				z1 = g51(sol(j),sol(j+1),sol(j+2),sol(j+3),rl(i))
			Else
				z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),rl(i))
			EndIf
			If z1 <> 0 Then
				If ninegramformat = 0 Then
					z2 = g52(rl(i),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
				Else
					z2 = g54(rl(i),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
				EndIf
				If z2 <> 0 then
						#include "solver_case9bltrl.bi"
				Else
					blt=0
				EndIf
			Else
				blt=0
			EndIf
			if blt>bls then bls=blt:new_letter=rl(i)		
		next i

	case 5
'		z2=g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
'		If z2 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z1 = g51(sol(j),sol(j+1),sol(j+2),rl(i),sol(j+4))
				Else
					z1 = g53(sol(j),sol(j+1),sol(j+2),rl(i),sol(j+4))
				EndIf
				If z1 <> 0 Then
					#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=rl(i)		
			next i
'		Else
'			new_letter=rl(mc)
'		EndIf

	case 6
'		z2=g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
'		If z2 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z1 = g51(sol(j),sol(j+1),rl(i),sol(j+3),sol(j+4))
				Else
					z1 = g53(sol(j),sol(j+1),rl(i),sol(j+3),sol(j+4))
				EndIf
				If z1 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=rl(i)		
			next i
'		Else
'			new_letter=rl(mc)
'		EndIf

	case 7
'		z2=g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
'		If z2 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z1 = g51(sol(j),rl(i),sol(j+2),sol(j+3),sol(j+4))
				Else
					z1 = g53(sol(j),rl(i),sol(j+2),sol(j+3),sol(j+4))
				EndIf
				If z1 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=rl(i)		
			next i
'		Else
'			new_letter=rl(mc)
'		EndIf

	case 8
'		z2=g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
'		If z2 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z1 = g51(rl(i),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
				Else
					z1 = g53(rl(i),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
				EndIf
				If z1 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=rl(i)		
			next i
'		Else
'			new_letter=rl(mc)
'		EndIf
			
End Select