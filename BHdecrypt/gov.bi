#ifndef __gov_bi__
#define __gov_bi__


#inclib "gov"

declare Function sf3_get_byte_array_fb cdecl Alias "sf3_get_byte_array_fb" (byval gov_array as ulongint ptr, byval gov_offset_and_seed as ulongint ptr, byval gov_width as ulongint, byval gov_multiplier as ulongint, byval gov_globalseed as ulongint, byval key as zstring ptr, byval length as ulongint)as ulongint
declare Function sf4_get_byte_array_fb cdecl Alias "sf4_get_byte_array_fb" (byval gov_array as ulongint ptr, byval gov_offset_and_seed as ulongint ptr, byval gov_width as ulongint, byval gov_multiplier as ulongint, byval gov_globalseed as ulongint, byval key as zstring ptr, byval length as ulongint)as ulongint
declare Function csf3_get_byte_array_fb cdecl Alias "csf3_get_byte_array_fb" (byval gov_array as ulongint ptr, byval gov_offset_and_seed as ulongint ptr, byval gov_multiplier as ulongint, byval gov_globalseed as ulongint, byval gov_w as integer, byval gov_end as ulongint, byval gov_start as ulongint, byval gov_symbol as ulongint ptr, byval gov_last_codeword_plus_one as ulongint ptr, byval gov_how_many_up_to_block as uinteger ptr, byval gov_shift as ubyte ptr, byval key as zstring ptr, byval length as ulongint)as ulongint
declare Function csf4_get_byte_array_fb cdecl Alias "csf4_get_byte_array_fb" (byval gov_array as ulongint ptr, byval gov_offset_and_seed as ulongint ptr, byval gov_multiplier as ulongint, byval gov_globalseed as ulongint, byval gov_w as integer, byval gov_end as ulongint, byval gov_start as ulongint, byval gov_symbol as ulongint ptr, byval gov_last_codeword_plus_one as ulongint ptr, byval gov_how_many_up_to_block as uinteger ptr, byval gov_shift as ubyte ptr, byval key as zstring ptr, byval length as ulongint)as ulongint


#endif