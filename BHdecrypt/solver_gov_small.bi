select case fileformat
	case 5:z=sf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_width, gov_multiplier, gov_globalseed, @sol(j), ngram_size)
	case 6:z=sf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_width, gov_multiplier, gov_globalseed, @sol(j), ngram_size)
	case 3:z=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
	case 4:z=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, @sol(j), ngram_size)
end select