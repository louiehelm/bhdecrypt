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

#include "csf3.h"
#include "spooky.h"

static uint64_t inline decode2(const uint64_t *gov_symbol, const uint64_t *gov_last_codeword_plus_one, const uint32_t *gov_how_many_up_to_block, const uint8_t *gov_shift, const uint64_t value) {	
	for (int curr = 0;; curr++)
		if (value < gov_last_codeword_plus_one[curr]) {
			const int s = gov_shift[curr];
			return gov_symbol[(value >> s) - (gov_last_codeword_plus_one[curr] >> s) + gov_how_many_up_to_block[curr]];
		}
}


static void inline signature_to_equation(const uint64_t *signature, const uint64_t seed, int num_variables, int *e) {
	uint64_t hash[4];
	spooky_short_rehash(signature, seed, hash);
	const int shift = __builtin_clzll(num_variables);
	const uint64_t mask = (UINT64_C(1) << shift) - 1;
	e[0] = ((hash[0] & mask) * num_variables) >> shift;
	e[1] = ((hash[1] & mask) * num_variables) >> shift;
	e[2] = ((hash[2] & mask) * num_variables) >> shift;
}

#define OFFSET_MASK (UINT64_C(-1) >> 10)

static uint64_t inline get_value(const uint64_t * const array, uint64_t pos, const int width) {
	const int l = 64 - width;
	const uint64_t start_word = pos / 64;
	const int start_bit = pos % 64;
	if (start_bit <= l) return array[start_word] << l - start_bit >> l;
	return array[start_word] >> start_bit | array[start_word + 1] << 64 + l - start_bit >> l;
}

int64_t csf3_get_byte_array_fb(const uint64_t *gov_array, const uint64_t *gov_offset_and_seed, uint64_t gov_multiplier, uint64_t gov_globalseed, const int w, const uint64_t end, const uint64_t start, const uint64_t *gov_symbol, const uint64_t *gov_last_codeword_plus_one, const uint32_t *gov_how_many_up_to_block, const uint8_t *gov_shift, char *key, uint64_t length) {
	uint64_t signature[4];
	spooky_short(key, length, gov_globalseed, signature);
	const int bucket = ((__uint128_t)(signature[0] >> 1) * (__uint128_t)gov_multiplier) >> 64;
	const uint64_t offset_seed = gov_offset_and_seed[bucket];
	const uint64_t bucket_offset = offset_seed & OFFSET_MASK;
	const int num_variables = (gov_offset_and_seed[bucket + 1] & OFFSET_MASK) - bucket_offset - w;
	int e[3];
	signature_to_equation(signature, offset_seed & ~OFFSET_MASK, num_variables, e);
	const int64_t t = decode2(gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, get_value(gov_array, e[0] + bucket_offset, w) ^ get_value(gov_array, e[1] + bucket_offset, w) ^ get_value(gov_array, e[2] + bucket_offset, w));
	if (t != -1) return t;
	return get_value(gov_array, e[0] + start, e[0] + end) ^ get_value(gov_array, e[1] + start, e[1] + end) ^ get_value(gov_array, e[2] + start, e[2] + end);
}