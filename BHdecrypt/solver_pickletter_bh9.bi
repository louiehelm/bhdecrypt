select case map2b(curr_symbol,k)
	case 0
		z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		If z1 <> 0 Then
			for i=0 to abc_sizem1
				z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),i)
				If z2 <> 0 then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf
		
	case 1
		z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		If z1 <> 0 Then
			for i=0 to abc_sizem1
				z2 = g54(sol(j+4),sol(j+5),sol(j+6),i,sol(j+8))
				If z2 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 2
		z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		If z1 > 0 Then
			for i=0 to abc_sizem1
				z2 = g54(sol(j+4),sol(j+5),i,sol(j+7),sol(j+8))
				If z2 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 3
		z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		If z1 > 0 Then
			for i=0 to abc_sizem1
				z2 = g54(sol(j+4),i,sol(j+6),sol(j+7),sol(j+8))
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
			z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),i)
			If z1 <> 0 Then
				z2 = g54(i,sol(j+5),sol(j+6),sol(j+7),sol(j+8))
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
		z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		If z2 > 0 Then
			for i=0 to abc_sizem1
				z1 = g53(sol(j),sol(j+1),sol(j+2),i,sol(j+4))
				If z1 <> 0 Then
					#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 6
		z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		If z2 > 0 Then
			for i=0 to mc
				z1 = g53(sol(j),sol(j+1),i,sol(j+3),sol(j+4))
				If z1 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 7
		z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		If z2 > 0 Then
			for i=0 to abc_sizem1
				z1 = g53(sol(j),i,sol(j+2),sol(j+3),sol(j+4))
				If z1 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf

	case 8
		z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		If z2 > 0 Then
			for i=0 to abc_sizem1
				z1 = g53(i,sol(j+1),sol(j+2),sol(j+3),sol(j+4))
				If z1 <> 0 Then
						#include "solver_case9blt.bi"
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:new_letter=i
			next i
		EndIf
			
End Select