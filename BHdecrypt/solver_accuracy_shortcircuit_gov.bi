if solvesub_accshortcircuit=1 andalso thread(tn).solkey=1 andalso new_score>10000 then 'accuracy short-circuit
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
			for h=65 to 90 'new_letter
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
					#include "solver_fastent_bh.bi"
					score_needed=(old_score/ent2/ngfal-new_ngram_score2)*hi
					if num_ngrams>score_needed then
						for i=1 to map1(g,0)
							sol(map1(g,i))=h
						next i

						if ngram_size < 6 then
							for i=1 to num_ngrams
								j=map2(g,i)
								#include "solver_gov_small.bi"
								score_needed-=z*hi
								if num_ngrams-i<score_needed then exit For
								if score_needed<0 then exit for
							next i
						elseif ngram_size = 6 Then
							for i=1 to num_ngrams
								j=map2(g,i)
								#include "solver_gov_6.bi"
								score_needed-=z*hi
								if num_ngrams-i<score_needed then exit For
								if score_needed<0 then exit for
							next i
						elseif ngram_size = 7 Then
							for i=1 to num_ngrams
								j=map2(g,i)
								#include "solver_gov_7.bi"
								score_needed-=z*hi
								if num_ngrams-i<score_needed then exit For
								if score_needed<0 then exit for
							next i
						elseif ngram_size = 8 Then
							for i=1 to num_ngrams
								j=map2(g,i)
								#include "solver_gov_8.bi"
								score_needed-=z*hi
								if num_ngrams-i<score_needed then exit For
								if score_needed<0 then exit for
							next i
						elseif ngram_size = 9 then
							for i=1 to num_ngrams
								j=map2(g,i)
								#include "solver_gov_9.bi"
								score_needed-=z*hi
								if num_ngrams-i<score_needed then exit For
								if score_needed<0 then exit for
							next i
						elseif ngram_size = 10 then
							for i=1 to num_ngrams
								j=map2(g,i)
								#include "solver_gov_10.bi"
								score_needed-=z*hi
								if num_ngrams-i<score_needed then exit For
								if score_needed<0 then exit for
							next i
						endif

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