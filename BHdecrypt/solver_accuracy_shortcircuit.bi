if solvesub_accshortcircuit=1 andalso thread(tn).solkey=1 andalso solvesub_reversesolve=0 andalso new_score>10000 then 'accuracy short-circuit
	e=1
	for i=1 to l
		if thread(tn).key(i)<>42 then 'wildcard
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
					curr_symbol=g
					old_letter=stl(g)
					new_letter=h
					dim as double entropy2=entropy
					dim as integer new_ngram_score2=new_ngram_score
					for i=1 to map1(curr_symbol,0) 'do
						sol(map1(curr_symbol,i))=new_letter
					next i
					fg = fg6(sol(1),sol(2),sol(3),sol(4),sol(5),sol(6))
					new_ngram_score2+=fg-old_fg

					sg = sg6(sol(2),sol(3),sol(4),sol(5),sol(6),sol(7))
					new_ngram_score2+=sg-old_sg

					slg = slg6(sol(l-6),sol(l-5),sol(l-4),sol(l-3),sol(l-2),sol(l-1))
					new_ngram_score2+=slg-old_slg
	
					lg = lg6(sol(l-5),sol(l-4),sol(l-3),sol(l-2),sol(l-1),sol(l))
					new_ngram_score2+=lg-old_lg
					#include "solver_ngram_main.bi"
					entropy2+=enttable(frq(old_letter)-mape2(curr_symbol))-enttable(frq(old_letter))
					entropy2+=enttable(frq(new_letter)+mape2(curr_symbol))-enttable(frq(new_letter))
					#include "solver_fastent.bi"
					#include "solver_wordscore.bi"
					new_score*=1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size) 'multiplicative

					if new_score>best_score then 'improvement found
						e=0
						accept=1
						exit for,for					' TODO: is this kosher to change letter but not update ngrams(), wordgrams, fg/sg...
					end if
					for i=1 to map1(curr_symbol,0) 'undo
						sol(map1(curr_symbol,i))=old_letter
					next i
				end if
			next h
		next g
		if e=1 then
			thread(tn).iterations_completed+=1
			exit for,for,for,for,for
		end if
	end if
end if