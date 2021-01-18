#pragma once

#inclib "z"
#inclib "zstd"
 
#include once "crt/long.bi"
#include once "crt/stddef.bi"
#include once "crt/limits.bi"
#include once "crt/sys/types.bi"
#include once "crt/stdarg.bi"

extern "C"

#define ZSTD_ZLIBWRAPPER_H
#define ZLIB_CONST
#define Z_PREFIX
#define ZLIB_INTERNAL
#define ZLIB_H
#define ZCONF_H
#define Z_PREFIX_SET
#define _dist_code z__dist_code
#define _length_code z__length_code
#define _tr_align z__tr_align
#define _tr_flush_bits z__tr_flush_bits
#define _tr_flush_block z__tr_flush_block
#define _tr_init z__tr_init
#define _tr_stored_block z__tr_stored_block
#define _tr_tally z__tr_tally
#define adler32_combine64 z_adler32_combine64
#define crc32_combine64 z_crc32_combine64
#define deflateInit z_deflateInit
#define deflateInit2 z_deflateInit2
#define deflate_copyright z_deflate_copyright
#define gz_error z_gz_error
#define gz_intmax z_gz_intmax
#define gz_strwinerror z_gz_strwinerror
#define gzgetc z_gzgetc
#define gzoffset64 z_gzoffset64
#define gzopen64 z_gzopen64
#define gzseek64 z_gzseek64
#define gztell64 z_gztell64
#define inflateBackInit z_inflateBackInit
#define inflateInit z_inflateInit
#define inflateInit2 z_inflateInit2
#define inflate_copyright z_inflate_copyright
#define inflate_fast z_inflate_fast
#define inflate_table z_inflate_table
#define zcalloc z_zcalloc
#define zcfree z_zcfree

''type Byte as z_Byte
type Bytef as z_Bytef
Type alloc_func as z_alloc_func
Type charf as z_charf
Type free_func as z_free_func
Type gzFile as z_gzFile
Type gz_header as z_gz_header
type gz_headerp as z_gz_headerp
type in_func as z_in_func
type intf as z_intf
type out_func as z_out_func
'type uInt as z_uInt
type uIntf as z_uIntf
'type uLong as z_uLong
type uLongf as z_uLongf
type voidp as z_voidp
type voidpc as z_voidpc
type voidpf as z_voidpf
#define internal_state z_internal_state

#ifdef __FB_DOS__
	#define UNALIGNED_OK
#endif

#define STDC
#define STDC99
type z_longlong as longint
type z_size_t as uinteger
#undef z_longlong
const MAX_MEM_LEVEL = 9
const MAX_WBITS = 15
#define OF(args) args
#define Z_ARG(args) args
'' TODO: # define ZEXTERN extern
#define ZEXPORT
#define ZEXPORTVA
#define FAR

type z_Byte as ubyte
type z_uInt as ulong
type z_uLong as culong
type z_Bytef as z_Byte
type z_charf as zstring
type z_intf as long
type z_uIntf as z_uInt
type z_uLongf as z_uLong
type z_voidpc as const any ptr
type z_voidpf as any ptr
type z_voidp as any ptr
type z_crc_t as culong

const SEEK_SET = 0
const SEEK_CUR = 1
const SEEK_END = 2
type z_off_t as clong
type z_off64_t as z_off_t
#define ZLIB_VERSION "1.2.11"
const ZLIB_VERNUM = &h12b0
const ZLIB_VER_MAJOR = 1
const ZLIB_VER_MINOR = 2
const ZLIB_VER_REVISION = 11
const ZLIB_VER_SUBREVISION = 0
type z_alloc_func as function(byval opaque as z_voidpf, byval items as z_uInt, byval size as z_uInt) as z_voidpf
type z_free_func as sub(byval opaque as z_voidpf, byval address as z_voidpf)

type z_stream_s
	next_in as const z_Bytef ptr
	avail_in as z_uInt
	total_in as z_uLong
	next_out as z_Bytef ptr
	avail_out as z_uInt
	total_out as z_uLong
	msg as const zstring ptr
'	state as z_internal_state Ptr  ' TODO: is this OK?? was:   state as z_internal_state Ptr
	zalloc as z_alloc_func
	zfree as z_free_func
	opaque as z_voidpf
	data_type as long
	adler as z_uLong
	reserved as z_uLong
end type

type z_stream as z_stream_s
type z_streamp as z_stream ptr

type z_gz_header_s
	text as long
	time as z_uLong
	xflags as long
	os as long
	extra as z_Bytef ptr
	extra_len as z_uInt
	extra_max as z_uInt
	name as z_Bytef ptr
	name_max as z_uInt
	comment as z_Bytef ptr
	comm_max as z_uInt
	hcrc as long
	done as long
end type

type gz_header_s as z_gz_header_s
type z_gz_header as z_gz_header_s
type z_gz_headerp as z_gz_header ptr

const Z_NO_FLUSH = 0
const Z_PARTIAL_FLUSH = 1
const Z_SYNC_FLUSH = 2
const Z_FULL_FLUSH = 3
const Z_FINISH = 4
const Z_BLOCK = 5
const Z_TREES = 6
const Z_OK = 0
const Z_STREAM_END = 1
const Z_NEED_DICT = 2
const Z_ERRNO = -1
const Z_STREAM_ERROR = -2
const Z_DATA_ERROR = -3
const Z_MEM_ERROR = -4
const Z_BUF_ERROR = -5
const Z_VERSION_ERROR = -6
const Z_NO_COMPRESSION = 0
const Z_BEST_SPEED = 1
const Z_BEST_COMPRESSION = 9
const Z_DEFAULT_COMPRESSION = -1
const Z_FILTERED = 1
const Z_HUFFMAN_ONLY = 2
const Z_RLE = 3
const Z_FIXED = 4
const Z_DEFAULT_STRATEGY = 0
const Z_BINARY = 0
const Z_TEXT = 1
const Z_ASCII = Z_TEXT
const Z_UNKNOWN = 2
const Z_DEFLATED = 8
const Z_NULL = 0
'#define zlib_version zlibVersion()

declare function z_zlibVersion() as const zstring ptr
declare function zlibVersion alias "z_zlibVersion"() as const zstring ptr
declare function z_deflate(byval strm as z_streamp, byval flush as long) as long
declare function deflate alias "z_deflate"(byval strm as z_streamp, byval flush as long) as long
declare function z_deflateEnd(byval strm as z_streamp) as long
declare function deflateEnd alias "z_deflateEnd"(byval strm as z_streamp) as long
declare function z_inflate(byval strm as z_streamp, byval flush as long) as long
declare function inflate alias "z_inflate"(byval strm as z_streamp, byval flush as long) as long
declare function z_inflateEnd(byval strm as z_streamp) as long
declare function inflateEnd alias "z_inflateEnd"(byval strm as z_streamp) as long
declare function z_deflateSetDictionary(byval strm as z_streamp, byval dictionary as const z_Bytef ptr, byval dictLength as z_uInt) as long
declare function deflateSetDictionary alias "z_deflateSetDictionary"(byval strm as z_streamp, byval dictionary as const z_Bytef ptr, byval dictLength as z_uInt) as long
declare function z_deflateGetDictionary(byval strm as z_streamp, byval dictionary as z_Bytef ptr, byval dictLength as z_uInt ptr) as long
declare function deflateGetDictionary alias "z_deflateGetDictionary"(byval strm as z_streamp, byval dictionary as z_Bytef ptr, byval dictLength as z_uInt ptr) as long
declare function z_deflateCopy(byval dest as z_streamp, byval source as z_streamp) as long
declare function deflateCopy alias "z_deflateCopy"(byval dest as z_streamp, byval source as z_streamp) as long
declare function z_deflateReset(byval strm as z_streamp) as long
declare function deflateReset alias "z_deflateReset"(byval strm as z_streamp) as long
declare function z_deflateParams(byval strm as z_streamp, byval level as long, byval strategy as long) as long
declare function deflateParams alias "z_deflateParams"(byval strm as z_streamp, byval level as long, byval strategy as long) as long
declare function z_deflateTune(byval strm as z_streamp, byval good_length as long, byval max_lazy as long, byval nice_length as long, byval max_chain as long) as long
declare function deflateTune alias "z_deflateTune"(byval strm as z_streamp, byval good_length as long, byval max_lazy as long, byval nice_length as long, byval max_chain as long) as long
declare function z_deflateBound(byval strm as z_streamp, byval sourceLen as z_uLong) as z_uLong
declare function deflateBound alias "z_deflateBound"(byval strm as z_streamp, byval sourceLen as z_uLong) as z_uLong
declare function z_deflatePending(byval strm as z_streamp, byval pending as ulong ptr, byval bits as long ptr) as long
declare function deflatePending alias "z_deflatePending"(byval strm as z_streamp, byval pending as ulong ptr, byval bits as long ptr) as long
declare function z_deflatePrime(byval strm as z_streamp, byval bits as long, byval value as long) as long
declare function deflatePrime alias "z_deflatePrime"(byval strm as z_streamp, byval bits as long, byval value as long) as long
declare function z_deflateSetHeader(byval strm as z_streamp, byval head as z_gz_headerp) as long
declare function deflateSetHeader alias "z_deflateSetHeader"(byval strm as z_streamp, byval head as z_gz_headerp) as long
declare function z_inflateSetDictionary(byval strm as z_streamp, byval dictionary as const z_Bytef ptr, byval dictLength as z_uInt) as long
declare function inflateSetDictionary alias "z_inflateSetDictionary"(byval strm as z_streamp, byval dictionary as const z_Bytef ptr, byval dictLength as z_uInt) as long
declare function z_inflateGetDictionary(byval strm as z_streamp, byval dictionary as z_Bytef ptr, byval dictLength as z_uInt ptr) as long
declare function inflateGetDictionary alias "z_inflateGetDictionary"(byval strm as z_streamp, byval dictionary as z_Bytef ptr, byval dictLength as z_uInt ptr) as long
declare function z_inflateSync(byval strm as z_streamp) as long
declare function inflateSync alias "z_inflateSync"(byval strm as z_streamp) as long
declare function z_inflateCopy(byval dest as z_streamp, byval source as z_streamp) as long
declare function inflateCopy alias "z_inflateCopy"(byval dest as z_streamp, byval source as z_streamp) as long
declare function z_inflateReset(byval strm as z_streamp) as long
declare function inflateReset alias "z_inflateReset"(byval strm as z_streamp) as long
declare function z_inflateReset2(byval strm as z_streamp, byval windowBits as long) as long
declare function inflateReset2 alias "z_inflateReset2"(byval strm as z_streamp, byval windowBits as long) as long
declare function z_inflatePrime(byval strm as z_streamp, byval bits as long, byval value as long) as long
declare function inflatePrime alias "z_inflatePrime"(byval strm as z_streamp, byval bits as long, byval value as long) as long
declare function z_inflateMark(byval strm as z_streamp) as clong
declare function inflateMark alias "z_inflateMark"(byval strm as z_streamp) as clong
declare function z_inflateGetHeader(byval strm as z_streamp, byval head as z_gz_headerp) as long
declare function inflateGetHeader alias "z_inflateGetHeader"(byval strm as z_streamp, byval head as z_gz_headerp) as long
type z_in_func as function(byval as any ptr, byval as const ubyte ptr ptr) as ulong
type z_out_func as function(byval as any ptr, byval as ubyte ptr, byval as ulong) as long
declare function z_inflateBack(byval strm as z_streamp, byval in as z_in_func, byval in_desc as any ptr, byval out as z_out_func, byval out_desc as any ptr) as long
declare function inflateBack alias "z_inflateBack"(byval strm as z_streamp, byval in as z_in_func, byval in_desc as any ptr, byval out as z_out_func, byval out_desc as any ptr) as long
declare function z_inflateBackEnd(byval strm as z_streamp) as long
declare function inflateBackEnd alias "z_inflateBackEnd"(byval strm as z_streamp) as long
declare function z_zlibCompileFlags() as z_uLong
declare function zlibCompileFlags alias "z_zlibCompileFlags"() as z_uLong
declare function z_compress(byval dest as z_Bytef ptr, byval destLen as z_uLongf ptr, byval source as const z_Bytef ptr, byval sourceLen as z_uLong) as long
declare function compress alias "z_compress"(byval dest as z_Bytef ptr, byval destLen as z_uLongf ptr, byval source as const z_Bytef ptr, byval sourceLen as z_uLong) as long
declare function z_compress2(byval dest as z_Bytef ptr, byval destLen as z_uLongf ptr, byval source as const z_Bytef ptr, byval sourceLen as z_uLong, byval level as long) as long
declare function compress2 alias "z_compress2"(byval dest as z_Bytef ptr, byval destLen as z_uLongf ptr, byval source as const z_Bytef ptr, byval sourceLen as z_uLong, byval level as long) as long
declare function z_compressBound(byval sourceLen as z_uLong) as z_uLong
declare function compressBound alias "z_compressBound"(byval sourceLen as z_uLong) as z_uLong
declare function z_uncompress(byval dest as z_Bytef ptr, byval destLen as z_uLongf ptr, byval source as const z_Bytef ptr, byval sourceLen as z_uLong) as long
declare function uncompress alias "z_uncompress"(byval dest as z_Bytef ptr, byval destLen as z_uLongf ptr, byval source as const z_Bytef ptr, byval sourceLen as z_uLong) as long
declare function z_uncompress2(byval dest as z_Bytef ptr, byval destLen as z_uLongf ptr, byval source as const z_Bytef ptr, byval sourceLen as z_uLong ptr) as long
declare function uncompress2 alias "z_uncompress2"(byval dest as z_Bytef ptr, byval destLen as z_uLongf ptr, byval source as const z_Bytef ptr, byval sourceLen as z_uLong ptr) as long
type z_gzFile as gzFile_s ptr
declare function z_gzdopen(byval fd as long, byval mode as const zstring ptr) as z_gzFile
declare function gzdopen alias "z_gzdopen"(byval fd as long, byval mode as const zstring ptr) as z_gzFile
declare function z_gzbuffer(byval file as z_gzFile, byval size as ulong) as long
declare function gzbuffer alias "z_gzbuffer"(byval file as z_gzFile, byval size as ulong) as long
declare function z_gzsetparams(byval file as z_gzFile, byval level as long, byval strategy as long) as long
declare function gzsetparams alias "z_gzsetparams"(byval file as z_gzFile, byval level as long, byval strategy as long) as long
declare function z_gzread(byval file as z_gzFile, byval buf as z_voidp, byval len as ulong) as long
declare function gzread alias "z_gzread"(byval file as z_gzFile, byval buf as z_voidp, byval len as ulong) as long
declare function z_gzfread(byval buf as z_voidp, byval size as z_size_t, byval nitems as z_size_t, byval file as z_gzFile) as z_size_t
declare function gzfread alias "z_gzfread"(byval buf as z_voidp, byval size as z_size_t, byval nitems as z_size_t, byval file as z_gzFile) as z_size_t
declare function z_gzwrite(byval file as z_gzFile, byval buf as z_voidpc, byval len as ulong) as long
declare function gzwrite alias "z_gzwrite"(byval file as z_gzFile, byval buf as z_voidpc, byval len as ulong) as long
declare function z_gzfwrite(byval buf as z_voidpc, byval size as z_size_t, byval nitems as z_size_t, byval file as z_gzFile) as z_size_t
declare function gzfwrite alias "z_gzfwrite"(byval buf as z_voidpc, byval size as z_size_t, byval nitems as z_size_t, byval file as z_gzFile) as z_size_t
declare function z_gzprintf(byval file as z_gzFile, byval format as const zstring ptr, ...) as long
declare function gzprintf alias "z_gzprintf"(byval file as z_gzFile, byval format as const zstring ptr, ...) as long
declare function z_gzputs(byval file as z_gzFile, byval s as const zstring ptr) as long
declare function gzputs alias "z_gzputs"(byval file as z_gzFile, byval s as const zstring ptr) as long
declare function z_gzgets(byval file as z_gzFile, byval buf as zstring ptr, byval len as long) as zstring ptr
declare function gzgets alias "z_gzgets"(byval file as z_gzFile, byval buf as zstring ptr, byval len as long) as zstring ptr
declare function z_gzputc(byval file as z_gzFile, byval c as long) as long
declare function gzputc alias "z_gzputc"(byval file as z_gzFile, byval c as long) as long
declare function z_gzgetc(byval file as z_gzFile) as long
declare function z_gzungetc(byval c as long, byval file as z_gzFile) as long
declare function gzungetc alias "z_gzungetc"(byval c as long, byval file as z_gzFile) as long
declare function z_gzflush(byval file as z_gzFile, byval flush as long) as long
declare function gzflush alias "z_gzflush"(byval file as z_gzFile, byval flush as long) as long
declare function z_gzrewind(byval file as z_gzFile) as long
declare function gzrewind alias "z_gzrewind"(byval file as z_gzFile) as long
declare function z_gzeof(byval file as z_gzFile) as long
declare function gzeof alias "z_gzeof"(byval file as z_gzFile) as long
declare function z_gzdirect(byval file as z_gzFile) as long
declare function gzdirect alias "z_gzdirect"(byval file as z_gzFile) as long
declare function z_gzclose(byval file as z_gzFile) as long
declare function gzclose alias "z_gzclose"(byval file as z_gzFile) as long
declare function z_gzclose_r(byval file as z_gzFile) as long
declare function gzclose_r alias "z_gzclose_r"(byval file as z_gzFile) as long
declare function z_gzclose_w(byval file as z_gzFile) as long
declare function gzclose_w alias "z_gzclose_w"(byval file as z_gzFile) as long
declare function z_gzerror(byval file as z_gzFile, byval errnum as long ptr) as const zstring ptr
declare function gzerror alias "z_gzerror"(byval file as z_gzFile, byval errnum as long ptr) as const zstring ptr
declare sub z_gzclearerr(byval file as z_gzFile)
declare sub gzclearerr alias "z_gzclearerr"(byval file as z_gzFile)
declare function z_adler32(byval adler as z_uLong, byval buf as const z_Bytef ptr, byval len as z_uInt) as z_uLong
declare function adler32 alias "z_adler32"(byval adler as z_uLong, byval buf as const z_Bytef ptr, byval len as z_uInt) as z_uLong
declare function z_adler32_z(byval adler as z_uLong, byval buf as const z_Bytef ptr, byval len as z_size_t) as z_uLong
declare function adler32_z alias "z_adler32_z"(byval adler as z_uLong, byval buf as const z_Bytef ptr, byval len as z_size_t) as z_uLong
declare function z_crc32(byval crc as z_uLong, byval buf as const z_Bytef ptr, byval len as z_uInt) as z_uLong
declare function crc32 alias "z_crc32"(byval crc as z_uLong, byval buf as const z_Bytef ptr, byval len as z_uInt) as z_uLong
declare function z_crc32_z(byval adler as z_uLong, byval buf as const z_Bytef ptr, byval len as z_size_t) as z_uLong
declare function crc32_z alias "z_crc32_z"(byval adler as z_uLong, byval buf as const z_Bytef ptr, byval len as z_size_t) as z_uLong
declare function z_deflateInit_(byval strm as z_streamp, byval level as long, byval version as const zstring ptr, byval stream_size as long) as long
declare function deflateInit_ alias "z_deflateInit_"(byval strm as z_streamp, byval level as long, byval version as const zstring ptr, byval stream_size as long) as long
declare function z_inflateInit_(byval strm as z_streamp, byval version as const zstring ptr, byval stream_size as long) as long
declare function inflateInit_ alias "z_inflateInit_"(byval strm as z_streamp, byval version as const zstring ptr, byval stream_size as long) as long
declare function z_deflateInit2_(byval strm as z_streamp, byval level as long, byval method as long, byval windowBits as long, byval memLevel as long, byval strategy as long, byval version as const zstring ptr, byval stream_size as long) as long
declare function deflateInit2_ alias "z_deflateInit2_"(byval strm as z_streamp, byval level as long, byval method as long, byval windowBits as long, byval memLevel as long, byval strategy as long, byval version as const zstring ptr, byval stream_size as long) as long
declare function z_inflateInit2_(byval strm as z_streamp, byval windowBits as long, byval version as const zstring ptr, byval stream_size as long) as long
declare function inflateInit2_ alias "z_inflateInit2_"(byval strm as z_streamp, byval windowBits as long, byval version as const zstring ptr, byval stream_size as long) as long
declare function z_inflateBackInit_(byval strm as z_streamp, byval windowBits as long, byval window as ubyte ptr, byval version as const zstring ptr, byval stream_size as long) as long
declare function inflateBackInit_ alias "z_inflateBackInit_"(byval strm as z_streamp, byval windowBits as long, byval window as ubyte ptr, byval version as const zstring ptr, byval stream_size as long) as long

#define z_deflateInit(strm, level) deflateInit_((strm), (level), ZLIB_VERSION, clng(sizeof(z_stream)))
#define z_inflateInit(strm) inflateInit_((strm), ZLIB_VERSION, clng(sizeof(z_stream)))
#define z_deflateInit2(strm, level, method, windowBits, memLevel, strategy) deflateInit2_((strm), (level), (method), (windowBits), (memLevel), (strategy), ZLIB_VERSION, clng(sizeof(z_stream)))
#define z_inflateInit2(strm, windowBits) inflateInit2_((strm), (windowBits), ZLIB_VERSION, clng(sizeof(z_stream)))
#define z_inflateBackInit(strm, windowBits, window) inflateBackInit_((strm), (windowBits), (window), ZLIB_VERSION, clng(sizeof(z_stream)))

type gzFile_s
	have as ulong
	next as ubyte ptr
	pos as clong
end type

declare function z_gzgetc_(byval file as z_gzFile) as long
declare function gzgetc_ alias "z_gzgetc_"(byval file as z_gzFile) as long
#undef z_gzgetc
'' TODO: # define z_gzgetc(g) ((g)->have ? ((g)->have--, (g)->pos++, *((g)->next)++) : (gzgetc)(g))
declare function z_gzopen(byval as const zstring ptr, byval as const zstring ptr) as z_gzFile
declare function gzopen alias "z_gzopen"(byval as const zstring ptr, byval as const zstring ptr) as z_gzFile
declare function z_gzseek(byval as z_gzFile, byval as clong, byval as long) as clong
declare function gzseek alias "z_gzseek"(byval as z_gzFile, byval as clong, byval as long) as clong
declare function z_gztell(byval as z_gzFile) as clong
declare function gztell alias "z_gztell"(byval as z_gzFile) as clong
declare function z_gzoffset(byval as z_gzFile) as clong
declare function gzoffset alias "z_gzoffset"(byval as z_gzFile) as clong
declare function z_adler32_combine(byval as z_uLong, byval as z_uLong, byval as clong) as z_uLong
declare function adler32_combine alias "z_adler32_combine"(byval as z_uLong, byval as z_uLong, byval as clong) as z_uLong
declare function z_crc32_combine(byval as z_uLong, byval as z_uLong, byval as clong) as z_uLong
declare function crc32_combine alias "z_crc32_combine"(byval as z_uLong, byval as z_uLong, byval as clong) as z_uLong
declare function z_zError(byval as long) as const zstring ptr
declare function zError alias "z_zError"(byval as long) as const zstring ptr
declare function z_inflateSyncPoint(byval as z_streamp) as long
declare function inflateSyncPoint alias "z_inflateSyncPoint"(byval as z_streamp) as long
declare function z_get_crc_table() as const z_crc_t ptr
declare function get_crc_table alias "z_get_crc_table"() as const z_crc_t ptr
declare function z_inflateUndermine(byval as z_streamp, byval as long) as long
declare function inflateUndermine alias "z_inflateUndermine"(byval as z_streamp, byval as long) as long
declare function z_inflateValidate(byval as z_streamp, byval as long) as long
declare function inflateValidate alias "z_inflateValidate"(byval as z_streamp, byval as long) as long
declare function z_inflateCodesUsed(byval as z_streamp) as culong
declare function inflateCodesUsed alias "z_inflateCodesUsed"(byval as z_streamp) as culong
declare function z_inflateResetKeep(byval as z_streamp) as long
declare function inflateResetKeep alias "z_inflateResetKeep"(byval as z_streamp) as long
declare function z_deflateResetKeep(byval as z_streamp) as long
declare function deflateResetKeep alias "z_deflateResetKeep"(byval as z_streamp) as long

#ifdef __FB_WIN32__
	declare function z_gzopen_w(byval path as const wstring ptr, byval mode as const zstring ptr) as z_gzFile
	declare function gzopen_w alias "z_gzopen_w"(byval path as const wstring ptr, byval mode as const zstring ptr) as z_gzFile
#elseif defined(__FB_CYGWIN__)
	declare function gzopen_w(byval path as const wstring ptr, byval mode as const zstring ptr) as z_gzFile
#endif

declare function z_gzvprintf(byval file as z_gzFile, byval format as const zstring ptr, byval va as va_list) as long
declare function gzvprintf alias "z_gzvprintf"(byval file as z_gzFile, byval format as const zstring ptr, byval va as va_list) as long
declare function zstdVersion() as const zstring ptr
declare sub ZWRAP_useZSTDcompression(byval turn_on as long)
declare function ZWRAP_isUsingZSTDcompression() as long
declare function ZWRAP_setPledgedSrcSize(byval strm as z_streamp, byval pledgedSrcSize as ulongint) as long
declare function ZWRAP_deflateReset_keepDict(byval strm as z_streamp) as long

type ZWRAP_decompress_type as long
enum
	ZWRAP_FORCE_ZLIB
	ZWRAP_AUTO
end enum

declare sub ZWRAP_setDecompressionType(byval type as ZWRAP_decompress_type)
declare function ZWRAP_getDecompressionType() as ZWRAP_decompress_type
declare function ZWRAP_isUsingZSTDdecompression(byval strm as z_streamp) as long
declare function ZWRAP_inflateReset_keepDict(byval strm as z_streamp) as long

end extern
