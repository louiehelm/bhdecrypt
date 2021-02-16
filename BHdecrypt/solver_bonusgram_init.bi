if solvesub_bonusgrams_enabled=1 then
	fg = fg6(sol(1),sol(2),sol(3),sol(4),sol(5),sol(6))
	old_fg = fg
	new_ngram_score+=fg
	
	sg = sg6(sol(2),sol(3),sol(4),sol(5),sol(6),sol(7))
	old_sg = sg
	new_ngram_score+=sg
	
	slg = slg6(sol(l-6),sol(l-5),sol(l-4),sol(l-3),sol(l-2),sol(l-1))
	old_slg = slg
	new_ngram_score+=slg
	
	lg = lg6(sol(l-5),sol(l-4),sol(l-3),sol(l-2),sol(l-1),sol(l))
	old_lg = lg
	new_ngram_score+=lg
end if