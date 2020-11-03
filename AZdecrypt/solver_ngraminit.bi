select case ngram_system
	case 0 'default
		select case ngs
			case 2
				for i=1 to al
					ngrams(i)=g2(sol(i),sol(i+1))
					new_ngram_score+=ngrams(i)
				next i
			case 3
				for i=1 to al
					ngrams(i)=g3(sol(i),sol(i+1),sol(i+2))
					new_ngram_score+=ngrams(i)
				next i
			case 4
				for i=1 to al
					ngrams(i)=g4(sol(i),sol(i+1),sol(i+2),sol(i+3))
					new_ngram_score+=ngrams(i)
				next i
			case 5
				for i=1 to al
					ngrams(i)=g5(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4))
					new_ngram_score+=ngrams(i)
				next i
			case 6
				for i=1 to al
					ngrams(i)=g6(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
					new_ngram_score+=ngrams(i)
				next i
			case 7
				for i=1 to al
					ngrams(i)=g7(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
					new_ngram_score+=ngrams(i)
				next i
			case 8
				for i=1 to al
					ngrams(i)=g8(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6),sol(i+7))
					new_ngram_score+=ngrams(i)
				next i
		end select
	case 1 'successive approximation
		select case ngram_size
			case 8
				for i=1 to al
					r=sol(i)+(sol(i+1)*26)+(sol(i+2)*676)+(sol(i+3)*17576)+(sol(i+4)*456976)+(sol(i+5)*11881376)+(sol(i+6)*308915776)+(sol(i+7)*8031810176)
					pl=sa_pl8(r/dv)
					pu=sa_pu8(r/dv)
					e=1
					do
						p=(pl+pu)/2
						if r>sa_ng8(p) then pl=p else pu=p
						if r=sa_ng8(p) then ng=p:exit do
						if pu=pl then e=0:exit do
						if pu-pl=1 then
							if r=sa_ng8(pl) then ng=pl:exit do
							if r=sa_ng8(pu) then ng=pu:exit do
							e=0:exit do
						end if
					loop
					if e=1 then ngrams(i)=sa_log8(ng) else ngrams(i)=0
					new_ngram_score+=ngrams(i)
				next i
			case 9
				for i=1 to al
					r=sol(i)+(sol(i+1)*26)+(sol(i+2)*676)+(sol(i+3)*17576)+(sol(i+4)*456976)+(sol(i+5)*11881376)+(sol(i+6)*308915776)+(sol(i+7)*8031810176)+(sol(i+8)*208827064576)
					pl=sa_pl9(r/dv)
					pu=sa_pu9(r/dv)
					e=1
					do
						p=(pl+pu)/2
						if r>sa_ng9(p) then pl=p else pu=p
						if r=sa_ng9(p) then ng=p:exit do
						if pu=pl then e=0:exit do
						if pu-pl=1 then
							if r=sa_ng9(pl) then ng=pl:exit do
							if r=sa_ng9(pu) then ng=pu:exit do
							e=0:exit do
						end if
					loop
					if e=1 then ngrams(i)=sa_log9(ng) else ngrams(i)=0
					new_ngram_score+=ngrams(i)
				next i
		end select
	case 2 'beijinghouse
		select case ngs
			case 6
				for i=1 to al
					z1 = g31(sol(i),sol(i+1),sol(i+2))
					If z1 <> 0 Then
						z2 = g32(sol(i+3),sol(i+4),sol(i+5))
						If z2 <> 0 Then
							ngrams(i)=g61(z1,z2)
						Else
							ngrams(i)=0
						EndIf
					Else
						ngrams(i)=0
					EndIf
					new_ngram_score+=ngrams(i)
				next i
			case 7
				for j=1 to al
					z1 = g41(sol(j),sol(j+1),sol(j+2),sol(j+3))
					If z1 = 0 Then
						ngrams(j) = 0
					Else
						z2 = g42(sol(j+3),sol(j+4),sol(j+5),sol(j+6))
						If z2 = 0 Then
							ngrams(j) = 0
						Else
							Select case sol(j+3)
								#include "solver_case7.bi"
							End Select
						EndIf
					endif
					new_ngram_score+=ngrams(j)
				next j
			case 8
				if eightgramformat=0 then
					for i=1 to al
						if g43(sol(i),sol(i+1),sol(i+2),sol(i+3))=0 or g44(sol(i+4),sol(i+5),sol(i+6),sol(i+7))=0 then
							ngrams(i)=g82(g45(sol(i),sol(i+1),sol(i+2),sol(i+3)),g46(sol(i+4),sol(i+5),sol(i+6),sol(i+7)))
						else
							ngrams(i)=g81(g43(sol(i),sol(i+1),sol(i+2),sol(i+3)),g44(sol(i+4),sol(i+5),sol(i+6),sol(i+7)))
						endif
						new_ngram_score+=ngrams(i)
					next i
				else
					for i=1 to al
						ngrams(i)=g81(g47(sol(i),sol(i+1),sol(i+2),sol(i+3)),g47(sol(i+4),sol(i+5),sol(i+6),sol(i+7)))
						new_ngram_score+=ngrams(i)
					next i
				end if
			case 9
				for j=1 to al
					If ninegramformat = 9 then
						ngrams(j) = 1 ' REMOVED FOR NOW get_ninegram(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6),sol(i+7),sol(i+8))
					else
						If ninegramformat = 0 Then
							z1 = g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
						Else
							z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
						endif
						If z1 = 0 Then
							ngrams(j) = 0
						Else
							If ninegramformat = 0 Then
								z2 = g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
							Else
								z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
							endif
							If z2 = 0 Then
								ngrams(j) = 0
							Else
								Select case sol(j+4)
									#include "solver_case9.bi"
								End Select
							EndIf
						EndIf
					endif
					new_ngram_score+=ngrams(j)
				next j
		end select
end select