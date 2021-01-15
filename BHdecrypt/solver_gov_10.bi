' check keyguard first
/'
if g5keyguard(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)) = 0 OrElse _
   g5keyguard(sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6)) = 0 OrElse _
   g5keyguard(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8)) = 0 OrElse _
   g4keyguard(sol(j+6),sol(j+7),sol(j+8),sol(j+9)) = 0 Then
	z = 0
Else
		select case ngram_format
			case 2
				if csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-2) = 0 OrElse _
				   csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-2) = 0 OrElse _
				   csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+2), ngram_size-2) = 0 OrElse _
				   csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-1) = 0 OrElse _
				   csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1) = 0 Then
					z = 0
				Else
				    z = csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)																															
				EndIf
			case 3
				if csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-2) = 0 OrElse _
				   csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-2) = 0 OrElse _
				   csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+2), ngram_size-2) = 0 OrElse _
				   csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-1) = 0 OrElse _
				   csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1) = 0 Then
					z = 0
				Else
				    z = csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)																															
				EndIf
		end Select
EndIf
'/

z=0
If (in_picker = 0 OrElse m=9) AndAlso ngrams(j-1) > 0 Then
	If g4keyguard(sol(j+6),sol(j+7),sol(j+8),sol(j+9)) > 0 Then  ' skip most checks if previous ngram passed them
		select case fileformat
			case 3
				If csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+2), ngram_size-2) > 0 Then
					z=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1)
					if z > 0 then
						z+=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
					endif
				EndIf
			case 4
				If csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+2), ngram_size-2) > 0 Then
					z=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1)
					if z > 0 then
						z+=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
					endif
				EndIf
		end Select
	EndIf
Else
	If g5keyguard(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)) > 0 AndAlso _
       g5keyguard(sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6)) > 0 AndAlso _
       g5keyguard(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8)) > 0 AndAlso _
       g4keyguard(sol(j+6),sol(j+7),sol(j+8),sol(j+9)) > 0 Then
		select case fileformat
			case 3
				if csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-2) > 0 AndAlso _
				   csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-2) > 0 AndAlso _
				   csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+2), ngram_size-2) > 0 AndAlso _
				   csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-1) > 0  Then
					z=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1)
					if z > 0 then
						z+=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
					endif
				EndIf
			case 4
				if csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-2) > 0 AndAlso _
				   csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-2) > 0 AndAlso _
				   csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+2), ngram_size-2) > 0 AndAlso _
				   csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size-1) > 0  Then
					z=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j+1), ngram_size-1)
					if z > 0 then
						z+=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
					endif
				EndIf
		end Select
   EndIf
EndIf