if thread(tn).solkey=1 andalso new_score>10000 then 'accuracy short-circuit
	e=1
	for i=1 to l
		if thread(tn).key(i)<>42 then
			if thread(tn).sol(i)<>thread(tn).key(i) then
				e=0
				exit for
			end if
		end if
	next i
	if e=1 then
		for g=1 to s 'curr_symbol
			for h=0 to abc_sizem1 'new_letter
				if h<>stl(g) then
					dim as double entropy2=entropy
					dim as integer new_ngram_score2=new_ngram_score
					entropy2+=enttable(frq(stl(g))-map15(g,0))-enttable(frq(stl(g)))
					entropy2+=enttable(frq(h)+map15(g,0))-enttable(frq(h))
					num_ngrams=map2(g,0)
					for i=1 to num_ngrams
						j=map2(g,i)
						new_ngram_score2-=ngrams(j)
					next i
					#include "solver_fastent_bh.bi"
					score_needed=(old_score/ent2/ngfal-new_ngram_score2)*hi
					if num_ngrams>score_needed then
						for i=1 to map1(g,0)
							sol(map1(g,i))=h
						next i
						select case ngram_size
							case 6
								for i=1 to num_ngrams
									j=map2(g,i)
									z1 = g31(sol(j),sol(j+1),sol(j+2))
									If z1 <> 0 Then
										z2 = g32(sol(j+3),sol(j+4),sol(j+5))
										If z2 <> 0 Then
											z=g61(z1,z2)
										Else
											z = 0
										EndIf
									Else
										z=0
									EndIf
									score_needed-=z*hi
									if num_ngrams-i<score_needed then exit for
									'new_ngram_score2+=z
									if score_needed<0 then exit for
								next i
							case 7
								for i=1 to num_ngrams
									j=map2(g,i)
									z1 = g41(sol(j),sol(j+1),sol(j+2),sol(j+3))
									If z1 = 0 Then
										z = 0
									Else
										z2 = g42(sol(j+3),sol(j+4),sol(j+5),sol(j+6))
										If z2 = 0 Then
											z = 0
										Else
											Select case sol(j+3)
												#include "solver_case7z.bi"
											End select
											score_needed-=z*hi
										EndIf
									endif
									if num_ngrams-i<score_needed then exit for
									'new_ngram_score2+=z
									if score_needed<0 then exit for
								next i
							case 8
								if eightgramformat=0 then
									for i=1 to num_ngrams
										z = 0
										j=map2(g,i)
										z1=g43(sol(j),sol(j+1),sol(j+2),sol(j+3))
										if z1=0 Then
											z3=g45(sol(j),sol(j+1),sol(j+2),sol(j+3))
											If z3 <> 0 Then
												z4=g46(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
												If z4 <> 0 Then
													z=g82(z3,z4)
												EndIf
											EndIf
										else
											z2=g44(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
											if z2=0 Then
												z3=g45(sol(j),sol(j+1),sol(j+2),sol(j+3))
												If z3 <> 0 Then
													z4=g46(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
													If z4 <> 0 Then
														z=g82(z3,z4)
													EndIf
												EndIf
											else
												z=g81(z1,z2)
											EndIf
										endif
										score_needed-=z*hi
										if (num_ngrams-i)<score_needed then exit For  ' -1 since the k position ngram is added outside loop
										'new_ngram_score2+=z
										if score_needed<0 then exit for
									next i
								else
									for i=1 to num_ngrams
										z = 0
										j=map2(g,i)
										z1=g47(sol(j),sol(j+1),sol(j+2),sol(j+3))
										if z1<>0 Then
											z2=g47(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
											if z2<>0 Then
												z=g81(z1,z2)
											EndIf
										endif
										score_needed-=z*hi
										if (num_ngrams-i)<score_needed then exit For  ' -1 since the k position ngram is added outside loop
										'new_ngram_score2+=z
										if score_needed<0 then exit for
									next i
								end if
							case 9
								for i=1 to num_ngrams
									j=map2(g,i)
									If ninegramformat = 9 then
										z = 1 ' REMOVED FOR NOW get_ninegram(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))	
									Else
										If ninegramformat = 0 Then
											z1 = g51(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
										Else
											z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
										endif
										If z1 = 0 Then
											z = 0
										Else
											If ninegramformat = 0 Then
												z2 = g52(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
											Else
												z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
											endif
											If z2 = 0 Then
												z = 0
											Else
												Select case sol(j+4)
													#include "solver_case9z.bi"
												End Select
											EndIf
										endif
									endif
									score_needed-=z*hi
									if num_ngrams-i<score_needed then exit for
									'new_ngram_score2+=z
									if score_needed<0 then exit for
								next i	
						end select
						if score_needed>=0 then
							for i=1 to map1(g,0)
								sol(map1(g,i))=stl(g)
							next i
						endif
					endif
					if score_needed<0 then 'improvement found
						e=0
						exit for,for
					end if
				end if
			next h
		next g
		if e=1 then
			thread(tn).iterations_completed+=1
			exit for,for,for,for,for
		end if
	end if
end if