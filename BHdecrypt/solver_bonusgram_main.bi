if solvesub_bonusgrams_enabled=1 then
	fg = fg6(sol(1),sol(2),sol(3),sol(4),sol(5),sol(6))
	new_ngram_score+=fg-old_fg
	
	sg = sg6(sol(2),sol(3),sol(4),sol(5),sol(6),sol(7))
	new_ngram_score+=sg-old_sg
	
	slg = slg6(sol(l-6),sol(l-5),sol(l-4),sol(l-3),sol(l-2),sol(l-1))
	new_ngram_score+=slg-old_slg
	
	lg = lg6(sol(l-5),sol(l-4),sol(l-3),sol(l-2),sol(l-1),sol(l))
	new_ngram_score+=lg-old_lg
end if