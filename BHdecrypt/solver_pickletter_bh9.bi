select case map2b(curr_symbol,k)
	case 0
		If ninegramformat = 0 Then
			z1 = g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		Else
			z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		endif
		If z1 <> 0 Then
			for i=0 to abc_sizem1
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),i)
				Else
					z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),i)
				EndIf
				If z2 <> 0 then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf
		
	case 1
		If ninegramformat = 0 Then
			z1 = g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		Else
			z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		endif
		If z1 <> 0 Then
			for i=0 to abc_sizem1
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),sol(j+5),sol(j+6),i,sol(j+8))
				Else
					z2 = g54(sol(j+4),sol(j+5),sol(j+6),i,sol(j+8))
				EndIf
				If z2 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 2
		If ninegramformat = 0 Then
			z1 = g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		Else
			z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		endif
		If z1 > 0 Then
			for i=0 to abc_sizem1
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),sol(j+5),i,sol(j+7),sol(j+8))
				Else
					z2 = g54(sol(j+4),sol(j+5),i,sol(j+7),sol(j+8))
				EndIf
				If z2 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 3
		If ninegramformat = 0 Then
			z1 = g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		Else
			z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		endif
		If z1 > 0 Then
			for i=0 to abc_sizem1
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),i,sol(j+6),sol(j+7),sol(j+8))
				Else
					z2 = g54(sol(j+4),i,sol(j+6),sol(j+7),sol(j+8))
				EndIf
				If z2 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf
		
	case 4
		for i=0 to abc_sizem1
			If ninegramformat = 0 Then
				z1 = g51(sol(j),sol(j+1),sol(j+2),sol(j+3),i)
			Else
				z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),i)
			EndIf
			If z1 <> 0 Then
				If ninegramformat = 0 Then
					z2 = g52(i,sol(j+5),sol(j+6),sol(j+7),sol(j+8))
				Else
					z2 = g54(i,sol(j+5),sol(j+6),sol(j+7),sol(j+8))
				EndIf
				If z2 <> 0 then
						#include "solver_case9bltrl.bi"
				Else
					blt=0
				EndIf
			Else
				blt=0
			EndIf
			if blt>bls then bls=blt:new_letter=i
		next i

	case 5
		If ninegramformat = 0 Then
			z2 = g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		Else
			z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		endif
		If z2 > 0 Then
			for i=0 to abc_sizem1
				If ninegramformat = 0 Then
					z1 = g51(sol(j),sol(j+1),sol(j+2),i,sol(j+4))
				Else
					z1 = g53(sol(j),sol(j+1),sol(j+2),i,sol(j+4))
				EndIf
				If z1 <> 0 Then
					#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 6
		If ninegramformat = 0 Then
			z2 = g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		Else
			z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		endif
		If z2 > 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z1 = g51(sol(j),sol(j+1),i,sol(j+3),sol(j+4))
				Else
					z1 = g53(sol(j),sol(j+1),i,sol(j+3),sol(j+4))
				EndIf
				If z1 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 7
		If ninegramformat = 0 Then
			z2 = g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		Else
			z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		endif
		If z2 > 0 Then
			for i=0 to abc_sizem1
				If ninegramformat = 0 Then
					z1 = g51(sol(j),i,sol(j+2),sol(j+3),sol(j+4))
				Else
					z1 = g53(sol(j),i,sol(j+2),sol(j+3),sol(j+4))
				EndIf
				If z1 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 8
		If ninegramformat = 0 Then
			z2 = g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		Else
			z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		endif
		If z2 > 0 Then
			for i=0 to abc_sizem1
				If ninegramformat = 0 Then
					z1 = g51(i,sol(j+1),sol(j+2),sol(j+3),sol(j+4))
				Else
					z1 = g53(i,sol(j+1),sol(j+2),sol(j+3),sol(j+4))
				EndIf
				If z1 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf
			
End Select