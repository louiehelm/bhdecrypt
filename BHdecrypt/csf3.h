/*
 * Sux: Succinct data structures
 *
 * Copyright (C) 2018 Sebastiano Vigna
 *
 *  This library is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU Lesser General Public License as published by the Free
 *  Software Foundation; either version 3 of the License, or (at your option)
 *  any later version.
 *
 *  This library is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 */

// #include "csf.h"
#include <stdint.h>

int64_t csf3_get_byte_array_fb(const uint64_t *gov_array, const uint64_t *gov_offset_and_seed, uint64_t gov_multiplier, uint64_t gov_globalseed, const int w, const uint64_t end, const uint64_t start, const uint64_t *gov_symbol, const uint64_t *gov_last_codeword_plus_one, const uint32_t *gov_how_many_up_to_block, const uint8_t *gov_shift, char *key, uint64_t length);
//int64_t csf3_get_byte_array_fb(const uint64_t *gov_array, const uint64_t *gov_offset_and_seed, uint64_t gov_multiplier, uint64_t gov_globalseed, uint64_t gov_global_max_codeword_length, uint64_t gov_escaped_symbol_length, uint64_t gov_escape_length, const uint64_t *gov_symbol, const uint64_t *gov_last_codeword_plus_one, const uint32_t *gov_how_many_up_to_block, const uint8_t *gov_shift, char *key, uint64_t length);