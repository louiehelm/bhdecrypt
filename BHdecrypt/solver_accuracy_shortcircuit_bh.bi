if solvesub_accshortcircuit=1 andalso thread(tn).solkey=1 andalso solvesub_reversesolve=0 andalso new_score>10000 then 'accuracy short-circuit
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
					entropy2+=enttable(frq(stl(g))-mape2(g))-enttable(frq(stl(g)))
					entropy2+=enttable(frq(h)+mape2(g))-enttable(frq(h))
					num_ngrams=map2(g,0)
					for i=1 to num_ngrams
						j=map2(g,i)
						new_ngram_score2-=ngrams(j)
					next i
					fg = fg6(sol(1),sol(2),sol(3),sol(4),sol(5),sol(6))
					new_ngram_score2+=fg-old_fg

					sg = sg6(sol(2),sol(3),sol(4),sol(5),sol(6),sol(7))
					new_ngram_score2+=sg-old_sg

					slg = slg6(sol(l-6),sol(l-5),sol(l-4),sol(l-3),sol(l-2),sol(l-1))
					new_ngram_score2+=slg-old_slg

					lg = lg6(sol(l-5),sol(l-4),sol(l-3),sol(l-2),sol(l-1),sol(l))
					new_ngram_score2+=lg-old_lg
					#include "solver_fastent_bh.bi"
					#include "solver_wordscore.bi"
					' works by adding in upcoming wordscore to test for score needed
					score_needed=(old_score/ent2/ngfal/(1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size))-new_ngram_score2)*hi
					if num_ngrams>score_needed then
						for i=1 to map1(g,0) 'do
							sol(map1(g,i))=h
						next i
						select case ngram_size
							case 8	 
								for i=1 to num_ngrams
									z=0
									j=map2(g,i)
									z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
									if z1<>0 then
										z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
										if z2<>0 then z=bh8(z1,z2)
									end if
									score_needed-=z*hi
									if (num_ngrams-i)<score_needed then exit for
									if score_needed<0 then exit for
								next i
							case 9
								for i=1 to num_ngrams
									j=map2(curr_symbol,i)
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
										else
												#include "solver_case9z.bi"
										EndIf
									endif
									score_needed-=z*hi
									if num_ngrams-i<score_needed then exit for
									new_ngram_score+=z
									if score_needed<0 then exit for
								next i
						end select
						if score_needed>=0 then
							for i=1 to map1(g,0) 'undo
								sol(map1(g,i))=stl(g)
							next i
						endif
					endif
					if score_needed<0 then 'improvement found
						e=0						' TODO: does it work to dump this updated solution outside of the scoring routine without updating ngrams() array, wordgrams, etc??
						accept=1
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