''
''
'' jemalloc -- header translated with help of SWIG FB wrapper
''
'' NOTICE: This file is part of the FreeBASIC Compiler package and can't
''         be included in other distributions without authorization.
''
''
#ifndef __jemalloc_bi__
#define __jemalloc_bi__

#inclib "jemalloc"

'' weak symbol: resolved at runtime by the linker if we are using jemalloc, nullptr otherwise
Declare Function mallctl Alias "mallctl" (byval gov_array as zstring ptr, byval oldp as integer ptr, byval oldlenp as integer ptr, byval newp as integer ptr, byval newlen as integer ptr)as integer



#define LG_SIZEOF_PTR 3
#define JEMALLOC_VERSION "5.2.1-0-gea6b3e973b477b8061e0076bb257dbd7f3faa756"
#define JEMALLOC_VERSION_MAJOR 5
#define JEMALLOC_VERSION_MINOR 2
#define JEMALLOC_VERSION_BUGFIX 1
#define JEMALLOC_VERSION_NREV 0
#define JEMALLOC_VERSION_GID "ea6b3e973b477b8061e0076bb257dbd7f3faa756"
#define MALLCTL_ARENAS_ALL 4096
#define MALLCTL_ARENAS_DESTROYED 4097

type extent_hooks_t as extent_hooks_s
type extent_alloc_t as any
type extent_dalloc_t as integer
type extent_destroy_t as any
type extent_commit_t as integer
type extent_decommit_t as integer
type extent_purge_t as integer
type extent_split_t as integer
type extent_merge_t as integer

type extent_hooks_s
	alloc as extent_alloc_t ptr
	dalloc as extent_dalloc_t ptr
	destroy as extent_destroy_t ptr
	commit as extent_commit_t ptr
	decommit as extent_decommit_t ptr
	purge_lazy as extent_purge_t ptr
	purge_forced as extent_purge_t ptr
	split as extent_split_t ptr
	merge as extent_merge_t ptr
end type

#endif
