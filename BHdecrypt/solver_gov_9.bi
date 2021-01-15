z=0 ' TODO fix top optimization
'If (in_picker = 0 OrElse m=8) AndAlso ngrams(j-1) > 0 Then
'	If g4keyguard(sol(j+5),sol(j+6),sol(j+7),sol(j+8)) > 0 Then  ' skip most checks if previous ngram passed them
'		select case fileformat
'			case 3
'				z=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1)
'				if z > 0 then
'					z+=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
'				endif
'			case 4
'				z=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1)
'				if z > 0 then
'					z+=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
'				endif
'		end Select
'	EndIf
'Else
   If g5keyguard(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)) > 0 AndAlso _
    g5keyguard(sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6)) > 0 AndAlso _
    g5keyguard(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8)) > 0 Then
	select case fileformat
		case 3
			z=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1)
			if z > 0 then
				if csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-1) <> 0 then
					z+=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
				endif
			endif
		case 4
			z=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1)
			if z > 0 then
				if csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-1) <> 0 then
					z+=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
				endif
			endif
	end Select
   EndIf
'EndIf