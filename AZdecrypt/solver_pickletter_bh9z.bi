select case map2b(curr_symbol,k)
	case 0
		If ninegramformat = 0 then
			z1=g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		else
			z1=g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		end if
		If z1 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),rl(i))
				Else
					z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),rl(i))
				EndIf
				If z2 <> 0 Then
					Select case sol(j+4)
						#include "solver_case9blt.bi"
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
		If ninegramformat = 0 then
			z1=g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		else
			z1=g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		end if
		If z1 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),sol(j+5),sol(j+6),rl(i),sol(j+8))
				Else
					z2 = g54(sol(j+4),sol(j+5),sol(j+6),rl(i),sol(j+8))
				EndIf
				If z2 <> 0 Then
					Select case sol(j+4)
						#include "solver_case9blt.bi"
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
		If ninegramformat = 0 then
			z1=g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		else
			z1=g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		end if
		If z1 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),sol(j+5),rl(i),sol(j+7),sol(j+8))
				Else
					z2 = g54(sol(j+4),sol(j+5),rl(i),sol(j+7),sol(j+8))
				EndIf
				If z2 <> 0 Then
					Select case sol(j+4)
						#include "solver_case9blt.bi"
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
		If ninegramformat = 0 then
			z1=g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		else
			z1=g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
		end if
		If z1 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z2 = g52(sol(j+4),rl(i),sol(j+6),sol(j+7),sol(j+8))
				Else
					z2 = g54(sol(j+4),rl(i),sol(j+6),sol(j+7),sol(j+8))
				EndIf
				If z2 <> 0 Then
					Select case sol(j+4)
						#include "solver_case9blt.bi"
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf
		
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
				If z2 <> 0 Then
					Select case rl(i)
						#include "solver_case9blt.bi"
					End Select
				Else
					blt=0
				EndIf
			Else
				blt=0
			endif
			if blt>bls then bls=blt:bl2=rl(i)		
		next i

	case 5
		If ninegramformat = 0 then
			z2=g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		else
			z2=g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		end if
		If z2 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z1 = g51(sol(j),sol(j+1),sol(j+2),rl(i),sol(j+4))
				Else
					z1 = g53(sol(j),sol(j+1),sol(j+2),rl(i),sol(j+4))
				EndIf
				If z1 <> 0 Then
					Select case sol(j+4)
						#include "solver_case9blt.bi"
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
		If ninegramformat = 0 then
			z2=g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		else
			z2=g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		end if
		If z2 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z1 = g51(sol(j),sol(j+1),rl(i),sol(j+3),sol(j+4))
				Else
					z1 = g53(sol(j),sol(j+1),rl(i),sol(j+3),sol(j+4))
				EndIf
				If z1 <> 0 Then
					Select case sol(j+4)
						#include "solver_case9blt.bi"
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf

	case 7
		If ninegramformat = 0 then
			z2=g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		else
			z2=g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		end if
		If z2 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z1 = g51(sol(j),rl(i),sol(j+2),sol(j+3),sol(j+4))
				Else
					z1 = g53(sol(j),rl(i),sol(j+2),sol(j+3),sol(j+4))
				EndIf
				If z1 <> 0 Then
					Select case sol(j+4)
						#include "solver_case9blt.bi"
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf

	case 8
		If ninegramformat = 0 then
			z2=g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		else
			z2=g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
		end if
		If z2 <> 0 Then
			for i=0 to mc
				If ninegramformat = 0 Then
					z1 = g51(rl(i),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
				Else
					z1 = g53(rl(i),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
				EndIf
				If z1 <> 0 Then
					Select case sol(j+4)
						#include "solver_case9blt.bi"
					End Select
				Else
					blt=0
				EndIf
				if blt>bls then bls=blt:bl2=rl(i)		
			next i
		Else
			bl2=rl(mc)
		EndIf
			
End Select