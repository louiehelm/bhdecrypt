'BHdecrypt 1.19
'--------------
' adapted from AZdecrypt v1.19 by jarl
' 
' icon: Unlock by Graphicloads
'
' major improvements:
'
' - Uses firstgrams, secondgrams, second-to-last-grams, and lastgrams to stablize solves and improve solve accuracy + convergance speed 
' 
' - GOV static function n-gram file formats (sf3, sf4, csf3, csf4)
'   - Allows 9-gram and 10-gram files
'   - Allows 8-grams to load in 2GB mem vs 14-16GB in regular bh or binary bh format
'
'
' - Experimental "folded" 9-gram file support
'
' - Wordgram support with 1-word and 2-word guided solves enabled
'   - Works in regular substitution, mono groups, and bigram solver so far
'
' minor improvements:
'
' - max cipher length increased from 2000 to 5000
' - clearer monospaced font
' - custom zlibWrapper zstd static library linked in for zstd support (50% faster loading than zlib)
' - higher performance static Intel/AMD-optimized zlib (+15-30% speed improvement)
' - stack size reduced from 98MB -> 1MB per thread
' - better memmory allocator with less overshoot on large allocations (jemalloc)
' - total mem usage on startup reduced 10-50%
' - uses more CPU threads by default
'
'--------------
'Known bugs:
'--------------
'- Possible small issue with Crib grid and GCC 8.1.0.
'- Possible threading issues with GAS (not GCC)

'disable 60 secs window for batch ciphers
'problem with nulls & skip solver: over skip issue? does not solve ciphers anymore?

'cstate table
'--------------
'- 1 to 10: manipulation etc
'- 11 to 20: stats
'- 21 to 100: tasks (solvers etc)

'#1: load n-grams etc
'#2: file load/save
'#3: output to batch

'thread_ptr(threadsmax+1): load n-grams, secondary hill-climbers, batch stuff...
'thread_ptr(threadsmax+2): statistics, measurements...
'thread_ptr(threadsmax+3): sub threads inside thread_ptr(threadsmax+1)

'zlib crc32/adler32 usage: crc32(0,@array(0),arraylen) 'bytes

#macro logbx(a,b)
log(a)/log(b)
#EndMacro

randomize timer,1 'rng=rand()

'includes
'------------------------------------------------------------
'#include "jemalloc.bi"


#ifndef __fb_linux__
	#include "win_gui.bi"
	#include "win_dialogs.bi"
#endif

#include "fbgfx.bi"
#include "file.bi"

#define ZWRAP_USE_ZSTD 1
'#include "zlib.bi"
#include "zstd_zlibwrapper.bi"
#include "ui_specific.bi"
#include "win/shlobj.bi"
#include "string.bi" ' for format function to replace format

'#include once "md/util/mdMap.bi"
'mdMapDeclare(string, ubyte)
'dim shared as mdMap(string, ubyte) map

'#include "jaggedarray.bi"
'jaggedArray(ubyte)
'Dim As Integer size(...) = {5, 3, 4}
'Dim As jaggedArrayubyte array = size()

'dim as ubyte tempubyte
'tempubyte = map.put("THAT", 255)
'tempubyte = map.put("THIS", 250)

'#include "PCG32II.bas"
'dim shared pcg(65536) as pcg32

'constants
'------------------------------------------------------------
const lb=chr(13)+chr(10) 'line break
const constcip=5000 'max cipher length
const constfrq=100 'max alphabet size (ABC...)
const constent=constcip*10 'max entropy table = constcip * max ngram_size

'window_main
'------------------------------------------------------------
dim shared as hwnd window_main
dim shared as hwnd window_graph
dim shared as hwnd input_text
dim shared as hwnd output_text
dim shared as hwnd solver_text
dim shared as hwnd label
dim shared as hwnd list_main
dim shared as hwnd button_main_process
dim shared as hwnd button_main_pauseresume
dim shared as hwnd button_main_stoptask
dim shared as hwnd button_main_openfile
dim shared as hwnd button_main_statesave
dim shared as hwnd button_main_stateload
dim shared as hwnd button_main_command
dim shared as hwnd button_main_swapinout
dim shared as hwnd button_main_openoutputdir
dim shared as hwnd editbox_main_command
dim shared as msg msg
dim shared as hmenu hmenu
dim shared as hmenu hfile
dim shared as hmenu hedit
dim shared as hmenu hfunctions
dim shared as hmenu hstats
dim shared as hmenu hoptions
dim shared as hmenu hhplaindir
dim shared as hmenu hhencodedir
dim shared as hmenu hhperiodic
dim shared as hmenu hhperfectcycles
dim shared as hmenu hhcyclepatterns
dim shared as hmenu hhencodernd
dim shared as hmenu hhrandomize
dim shared as hmenu hhconvert
dim shared as hmenu hhoffset
dim shared as hmenu hhremove
dim shared as hmenu hhcompare
dim shared as hmenu hhcycletypes
dim shared as hmenu hhomnidirectional
dim shared as hmenu hhhcyclepatterns2s
dim shared as hmenu hhhcyclepatterns3s
dim shared as hmenu hhhcyclepatterns4s
dim shared as hmenu hhhencodernd2s
dim shared as hmenu hhhencodernd3s

'window_dimension
'------------------------------------------------------------
dim shared as hwnd window_dimension
dim shared as hwnd list_dimension
dim shared as hwnd editbox_dimension_custom
dim shared as hwnd button_dimension_update
dim shared as hwnd button_dimension_custom

'window_transposition
'------------------------------------------------------------
dim shared as hwnd window_transposition
dim shared as hwnd list_transposition
dim shared as hwnd editbox_transposition_a1
dim shared as hwnd editbox_transposition_a2
dim shared as hwnd editbox_transposition_a3
dim shared as hwnd editbox_transposition_a4
dim shared as hwnd editbox_transposition_a5
dim shared as hwnd editbox_transposition_a6
dim shared as hwnd editbox_transposition_a7
dim shared as hwnd editbox_transposition_a8
dim shared as hwnd button_transposition_transpose
dim shared as hwnd button_transposition_untranspose
dim shared as hwnd checkbox_transposition_keepnulls

'window_optionssolver
'------------------------------------------------------------
dim shared as hwnd window_optionssolver
dim shared as hwnd list_optionssolver
dim shared as hwnd editbox_optionssolver_a1
dim shared as hwnd button_optionssolver_change
dim shared as hwnd button_optionssolver_normalize

'window_optionsstats
'------------------------------------------------------------
dim shared as hwnd window_optionsstats
dim shared as hwnd list_optionsstats
dim shared as hwnd editbox_optionsstats_a1
dim shared as hwnd button_optionsstats_change

'window_combine
'------------------------------------------------------------
dim shared as hwnd window_combine
dim shared as hwnd list_combine_operations
dim shared as hwnd list_combine_stack
dim shared as hwnd list_combine_measurements
dim shared as hwnd button_combine_add
dim shared as hwnd button_combine_remove
dim shared as hwnd button_combine_process
dim shared as hwnd checkbox_combine_normalized
dim shared as hwnd checkbox_combine_omitlist
dim shared as hwnd checkbox_combine_hypergraph
dim shared as hwnd checkbox_combine_forcelinear
dim shared as hwnd editbox_combine_a1
dim shared as hwnd editbox_combine_a2
dim shared as hwnd editbox_combine_a3
dim shared as hwnd editbox_combine_a4
dim shared as hwnd editbox_combine_a5
dim shared as hwnd editbox_combine_a6
dim shared as hwnd editbox_combine_a7
dim shared as hwnd editbox_combine_a8
dim shared as hwnd editbox_combine_ma1
dim shared as hwnd editbox_combine_ma2
dim shared as hwnd editbox_combine_getsigma
dim shared as hwnd editbox_combine_combinations
dim shared as hwnd radiobutton_combine_additive
dim shared as hwnd radiobutton_combine_multiplicative
dim shared as hwnd radiobutton_combine_transposed
dim shared as hwnd radiobutton_combine_untransposed
dim shared as hwnd editbox_combine_minioc
dim shared as hwnd editbox_combine_maxioc
dim shared as hwnd editbox_combine_fromlen
dim shared as hwnd editbox_combine_tolen

'window_cribs
'------------------------------------------------------------
dim shared as hwnd window_cribs
dim shared as hwnd button_cribs_solve
dim shared as hwnd button_cribs_reload
dim shared as hwnd button_cribs_clear
dim shared as hwnd checkbox_cribs_editcipher
dim shared as hwnd checkbox_cribs_showcipher
dim shared as hwnd wc_cribs(100,100)
dim shared as hwnd wc_cipher(100,100)
dim shared as hwnd list_cribs_words
dim shared as hwnd editbox_cribs_words
dim shared as hwnd button_cribs_words
dim shared as long wc_dx
dim shared as long wc_dy
dim shared as long wc_l
dim shared as long wc_s
dim shared as byte wc_num
dim shared as byte wc_prevcheck_editcipher
dim shared as byte wc_prevcheck_showcipher
redim shared as string wc_pgrid(0,0,0)
redim shared as long wc_nuba(0,0)
dim shared as byte wc_windowup

'window_manipulation
'------------------------------------------------------------
dim shared as hwnd window_manipulation
dim shared as hwnd list_manipulation_operations
dim shared as hwnd editbox_manipulation_a1
dim shared as hwnd editbox_manipulation_a2
dim shared as hwnd editbox_manipulation_a3
dim shared as hwnd editbox_manipulation_a4
dim shared as hwnd editbox_manipulation_a5
dim shared as hwnd editbox_manipulation_a6
dim shared as hwnd button_manipulation_process

'window_symbols
'------------------------------------------------------------
dim shared as hwnd window_symbols
dim shared as hwnd list_symbols_ngrams
dim shared as hwnd list_symbols_operations
dim shared as hwnd button_symbols_ngramsize
dim shared as hwnd editbox_symbols_ngramsize
dim shared as hwnd button_symbols_update
dim shared as hwnd button_symbols_process
dim shared as hwnd editbox_symbols_a1

'window_createtranspositionmatrix
'------------------------------------------------------------
dim shared as hwnd window_creatematrix
redim shared as hwnd buttons_creatematrix()
dim shared as hwnd button_creatematrix_clear
dim shared as hwnd button_creatematrix_exportoutput
dim shared as hwnd button_creatematrix_undo
dim shared as hwnd button_creatematrix_saveas
dim shared as hwnd button_creatematrix_open
dim shared as byte cm_windowup
dim shared as hwnd button_creatematrix_transpose
dim shared as hwnd button_creatematrix_untranspose

'window_transpositionsolver
'------------------------------------------------------------
dim shared as hwnd window_transpositionsolver
dim shared as hwnd list_transpositionsolver_operations
dim shared as hwnd list_transpositionsolver_stack
dim shared as hwnd button_transpositionsolver_start
dim shared as hwnd button_transpostionsolver_add
dim shared as hwnd button_transpostionsolver_addall
dim shared as hwnd button_transpostionsolver_remove
dim shared as hwnd button_transpostionsolver_removeall
dim shared as hwnd editbox_transpositionsolver_stacksize
dim shared as hwnd editbox_transpositionsolver_searchstates
dim shared as hwnd button_transpostionsolver_start
dim shared as hwnd button_transpostionsolver_batchciphers
dim shared as hwnd editbox_transpositionsolver_batchciphersrestarts

'window_units
'------------------------------------------------------------
dim shared as hwnd window_units
dim shared as hwnd radiobutton_units_symbol
dim shared as hwnd radiobutton_units_row
dim shared as hwnd radiobutton_units_column
dim shared as hwnd radiobutton_units_hseq
dim shared as hwnd radiobutton_units_remove
dim shared as hwnd radiobutton_units_expand
dim shared as hwnd radiobutton_units_separate
dim shared as hwnd radiobutton_units_replace
dim shared as hwnd radiobutton_units_reverse
dim shared as hwnd radiobutton_units_period_tp
dim shared as hwnd radiobutton_units_period_utp
dim shared as hwnd editbox_units_period
dim shared as hwnd editbox_units_hseqsize
dim shared as hwnd editbox_units_replacesymbols
dim shared as hwnd editbox_units_replaceleft
dim shared as hwnd editbox_units_replaceright
dim shared as hwnd editbox_units_mulweight
dim shared as hwnd editbox_units_klstart
dim shared as hwnd editbox_units_klend
dim shared as hwnd button_units_start
dim shared as byte un_windowup

'window_polyphones
'------------------------------------------------------------
dim shared as hwnd window_polyphones
dim shared as hwnd list_polyphones_stl
dim shared as hwnd button_polyphones_reload
dim shared as hwnd button_polyphones_solve
dim shared as hwnd button_polyphones_setall
dim shared as hwnd button_polyphones_setsel
dim shared as hwnd editbox_polyphones_setall
dim shared as hwnd editbox_polyphones_setsel
dim shared as hwnd editbox_polyphones_extraletters
dim shared as hwnd editbox_polyphones_shifts
dim shared as hwnd editbox_polyphones_mulweight
dim shared as hwnd editbox_polyphones_sdbias
dim shared as hwnd radiobutton_polyphones_user
dim shared as hwnd radiobutton_polyphones_auto
dim shared as hwnd radiobutton_polyphones_hafer1
dim shared as hwnd radiobutton_polyphones_hafer2
dim shared as hwnd checkbox_polyphones_increment
dim shared as hwnd checkbox_polyphones_override
dim shared as byte pp_windowup

'types
'------------------------------------------------------------
type list1
	itemnumber as integer
	itemname as string
	itemname2 as string
	l as short
	s as short
	dim_x as integer
	dim_y as integer
	num as integer
	cip(constcip)as long '4000
	sol(constcip)as long
	hkey(100,20)as long
	gkey(constcip,4)as short
	key(constcip)as long
	ckey(constcip)as byte 'cribkey
	graph(constcip)as double
	update as integer
	score as double
	ngrams as double
	pccycles as double
	chi as double
	ent as double
	ioc as double
	ioc2 as double
	iterations as integer
	iterationsfactor as double
	temperature as double
	restarts as integer
	subrestartlevels as integer
	ngramfactor as double
	multiplicity as double
	multiplicityweight as double
	entweight as double
	solver_waiting as integer
	solver_processing as short
	thread_active as integer
	solver_active as integer
	thread_stop as integer
	solver_stop as integer
	iterations_completed as integer
	restarts_completed as integer
	combine_output as integer
	outputdir as string
	repeats as string
	effectivesymbols as double
	arg1 as double
	tmpd1 as double
	match as double
	matchweight as double
	cyclealphabetsize as double
	cyclelengthweight as double
	cyclequalityweight as double
	cyclesizeweight as double
	pcmode as byte
	advstats as byte
	solkey as byte
	solkeyacc as double
	sectime as double
	avgscore as double
	avgioc as double
	avgpccycles as double
	wordflow as double
end type

type combo
	n as string
	m as double
	c as integer
end type

type combo2
	s as string
	n as integer
	a1 as integer
	a2 as integer
	a3 as integer
	a4 as integer
	a5 as integer
	a6 as integer
	u as byte
end type

type list_combine
	operation as string
	arg(10) as string
	multiplicative as integer
	untransposed as integer
end type

type generic_loop
	f as integer 'from
	t as integer 'to
	s as integer 'step
	c as integer 'current
	of as integer 'option from
	ofi as integer 'option from i
	ofm as integer 'option from modifier
end type

type bytebytebytedouble
	b1 as byte
	b2 as byte
	b3 as byte
	d1 as double
end type

'variables, arrays
'------------------------------------------------------------
dim shared as integer info_length
dim shared as integer identmax=65535
dim shared as integer info_symbols
dim shared as integer info_x
dim shared as integer info_y
dim shared as integer info_numerical
dim shared as long info(constcip*2)
dim shared as long info2(constcip*2)
dim shared as long pinfo(constcip*2)
dim shared as long nuba(constcip*2)
dim shared as long freq(constcip*2)
dim shared as integer rlen(constcip*2)
dim shared as integer mark(constcip*2)
dim shared as integer cpol(constcip*2) 'letter per symbols for poly solver
dim shared as double sort2(constcip*2,4)
dim shared as double cmap(constcip)
dim shared as long ioctable(constcip)
redim shared as short cstate(100,constcip*2)
dim shared as long info_out(constcip*2)
dim shared as short extarg(constcip)
dim shared as integer empty(0)
redim shared as ulong nscf(0,0)
redim shared as double ngram_list(0,0)
redim shared as short extcip(0,0,0)
redim shared as short extkey(0,0,0,0)
redim shared as short extinf(0,0,0)
redim shared as short extpcm(0,0,0)
dim shared as string ngramitems(2)
dim shared as integer prev_index_list_dimension
dim shared as integer prev_index_list_textual
dim shared as integer prev_index_list_transposition
dim shared as integer prev_index_list_combine_operations
dim shared as integer prev_index_list_manipulation_operations
dim shared as integer prev_index_list_symbols
dim shared as integer ascii_table(256)
dim shared as integer unispacing
dim shared as string file,text
dim shared as string filter
dim shared as string program_name
dim shared as string file_name
dim shared as string text_state
dim shared as string shared_string
dim shared as string basedir
dim shared as string ngramfilename
dim shared as integer stoptask
dim shared as integer stats_direction_m
dim shared as integer stop_measurement
dim shared as integer loadngrams_showmsg
dim shared as integer symbols_ngramsize
dim shared as integer ngrams_clearprevious
dim shared as integer ngrams_inmem(9)
dim shared as short encodingnulls_m
dim shared as short encodingnulls_t
dim shared as short encodingnulls_u
redim shared as string tma()
redim shared as short tma_his1()
redim shared as short tma_his2()
dim shared as integer tma_c
dim shared as integer tma_c2
dim shared as integer tma_dx
dim shared as integer tma_dy
dim shared as integer tma_lpx
dim shared as integer tma_lpy
dim shared as byte tma_ok
dim shared as string report
dim shared as ubyte mcl(255)
dim shared as double chi2(255)
dim shared as double chi2fl(255)
dim shared as double graph(1,2,1048576)
dim shared as short recbestcycle
dim shared as string output_string
dim shared as integer ossi
dim shared as short exit_prog
dim shared as double twait=3
dim shared as double shared_cycle_table(constcip)
dim shared as byte memcheck
dim shared as byte screensizecheck
dim shared as string path_file_name
dim shared as uinteger highgram
dim shared as integer threadsmax=65536
redim shared as list1 thread(0)
dim shared as any ptr thread_ptr(threadsmax+10)
dim shared as any ptr thread_ptr2(threadsmax+10)
dim shared as integer threads
dim shared as double ips_timer
dim shared as integer solve_list_max=1
dim shared as integer solve_list_number=1
dim shared as integer ngram_alphabet_size
dim shared as integer ngram_size
dim shared as integer ngram_count
dim shared as integer ngram_maxtableindex
dim shared as integer ngram_lowval
dim shared as integer ngram_highval
dim shared as uinteger ngram_avgval
dim shared as uinteger ngram_values(255)

dim shared as string ngram_format
dim shared as short alphabet(255)
dim shared as short alpharev(255)
dim shared as integer solvesub_iterations
dim shared as double solvesub_iterationsfactor
dim shared as integer solvesub_hciterations
dim shared as double solvesub_hciterationsfactor
dim shared as integer solvesub_restarts
dim shared as integer solvesub_subrestartlevels
dim shared as double solvesub_ngramfactor
dim shared as double solvesub_wgramfactor=1.00
dim shared as double solvesub_temperature
dim shared as double solvesub_entweight
dim shared as integer solvesub_cputhreads
dim shared as integer solvesub_outputdir
dim shared as integer solvesub_outputbatch
dim shared as integer solvesub_outputimp
dim shared as double solvesub_multiplicityweight
dim shared as double solvesub_matchweight
dim shared as double solvesub_avgscore
dim shared as double solvesub_avgioc
dim shared as string solvesub_ngramloc
dim shared as string solvesub_ngramloctemp
dim shared as integer solvesub_polyphones
dim shared as integer solvesub_batchngramsrestarts
dim shared as integer solvesub_scoreover
dim shared as integer solvesub_transstack
dim shared as double solvesub_bhmaxgb
dim shared as byte solvesub_ngramcaching
dim shared as integer solvesub_vigenerekeylength
dim shared as byte solvesub_vigenerebycolumns
dim shared as byte solvesub_vigeneresubtract
dim shared as integer solvesub_nullsymbolskeylength
dim shared as integer solvesub_advstats
dim shared as double solvesub_cyclelengthweight
dim shared as double solvesub_cyclequalityweight
dim shared as double solvesub_cyclesizeweight
dim shared as double solvesub_rndcyclearg
dim shared as integer solvesub_bigramdepth
dim shared as integer solvesub_accshortcircuit
dim shared as string solver_status
dim shared as string solver_file_name_ngrams
dim shared as integer solver_status_processing
dim shared as string task_active
dim shared as double global_best_score
dim shared as string solver_active
dim shared as short solvesub_cyclealphabetsize
dim shared as short solvesub_pcmode
dim shared as short solvesub_batchshutdown
dim shared as integer batchnr
dim shared as short symbolngramfrqsize
dim shared as short solvesub_ctcolumns
dim shared as short solvesub_ctdepth
dim shared as short solvesub_ctmode
dim shared as short solvesub_ctpcre
dim shared as double curr_avgscore
dim shared as short ext_hc
dim shared as string genhc_mode
dim shared as short solvesub_pnperiod
dim shared as short solvesub_pnnulls
dim shared as short solvesub_pndepth
dim shared as short solvesub_defects
dim shared as short solvesub_defdepth
dim shared as short solvesub_pnbias
dim shared as byte solvesub_tpseqhom
dim shared as short solvesub_searchdepth
dim shared as short stats_bigramsmod
dim shared as short solvesub_pnmannulls
dim shared as short solvesub_pnmanskips
dim shared as short solvesub_pnkl
dim shared as integer solvesub_pnover
dim shared as byte solvesub_pnoverskip
dim shared as byte development
dim shared as byte combine
dim shared as double	solvesub_nshctemp
dim shared as short solvesub_nshcshift
dim shared as double	solvesub_nshcshiftdiv
dim shared as integer solvesub_nshcrestartsmax
dim shared as integer solvesub_fastent
dim shared as short solvesub_subr(10)
dim shared as double solvesub_ngramentweight(10)
dim shared as double solvesub_ngramfactor2(10)
dim shared as byte stats_symbolcyclepatternscs
dim shared as byte stats_symbolcyclepatternsfl
dim shared as integer stats_symbolcyclepatternsrndtrials
dim shared as byte pausetask
dim shared as byte ts_windowup
dim shared as byte use_cribs
dim shared as double solvesub_seqweight
dim shared as byte solvesub_incpolyphones
dim shared as byte solvesub_newcipher
dim shared as uinteger ngram_mem
dim shared as uinteger ngram_file_size
dim shared as byte solverexist
dim shared as byte batchciphers_showmsg
redim shared as double bcsstats(0,0)
dim shared as integer bcsi
dim shared as string bcsfilename
dim shared as double sectimer
dim shared as byte solvesub_overwriteoutput
dim shared as byte solvesub_pccyclesformat
dim shared as uinteger csol(constcip,constfrq)
dim shared as byte solvesub_cribgridinstance
dim shared as byte solvesub_bigramheatmap
dim shared as double solvesub_bigrambestsol
dim shared as uinteger solvesub_bigramhomwdiv
dim shared as uinteger solvesub_bigramautocrib
dim shared as byte bigramsolver
dim shared as integer individual_accuracy
redim shared as combo indacc(0)
dim shared as integer solvesub_reversesolve
dim shared as integer solvesub_rowboundfragments
dim shared as double solvesub_rowboundtemp
dim shared as double solvesub_rowboundhcmode1itfact
dim shared as integer solvesub_rowboundhkeys
dim shared as double solvesub_rowbounddistinct
dim shared as double solvesub_rowboundfine
dim shared as double	solvesub_rowboundover
dim shared as integer solvesub_rowboundcribfragments
dim shared as integer solvesub_rowboundsubrestarts
dim shared as byte solvesub_rowboundcheckhistory
dim shared as byte solvesub_rowbounddistinctmode
dim shared as byte solvesub_transpositionbatchciphers
dim shared as byte solvesub_tsbatchciphersrestarts
dim shared as double solvesub_higherorderhomophonic
dim shared as double solvesub_higherorderhomophonicweight
dim shared as integer solvesub_batchciphersbigrams
dim shared as integer solvesub_batchciphersbigramsskipped
dim shared as integer solvesub_ngramlogcutoff
dim shared as uinteger distinct_values
dim shared as double ngram_value_entropy1
dim shared as double ngram_value_entropy2
dim shared as double trimmed_table_ratio
dim shared as double ngram_loading_time
dim shared as byte ngrambias_showmsg
dim shared as string ngrambias_text
dim shared as double ngrambias_factor
dim shared as double solvesub_tempdiv
static shared as ubyte permu()
dim shared as double solvesub_avgpccycles
dim shared as string intext
dim shared as double solvesub_sdbias
dim shared as double solvesub_solutionreleasetimer
dim shared as integer solvesub_addspaces
dim shared as integer ngram_standardalphabet
dim shared as integer firststart
dim shared as integer addspaces_ngrams
dim shared as integer solvesub_addspacesquality
dim shared as integer solvesub_bonusgrams_enabled
dim shared as integer solvesub_wordgrams_enabled
dim shared as integer solvesub_7gwordgrams

'thread mutex
'------------------------------------------------------------
dim shared as any ptr ml1
ml1=mutexcreate()
dim shared as any ptr csolmutex
csolmutex=mutexcreate()

'stats variables
'------------------------------------------------------------
dim shared as integer stats_dirrndtrials
dim shared as integer stats_encrndtrials
dim shared as double stats_nsymbolcyclesweight
dim shared as integer stats_running
dim shared as short stats_nsymbolcycles

'combine variables
'------------------------------------------------------------
dim shared as integer combine_listlength
dim shared as string combine_measurement
dim shared as integer combine_normalized
dim shared as integer combine_omitlist
dim shared as integer combine_combinations
dim shared as integer combine_maxlistlength
dim shared as integer combine_maxgroupentries
dim shared as integer combine_maxgroupsize
dim shared as integer combine_hypergraph
redim shared as string combine_item(0)
redim shared as double combine_score(0)
redim shared as unsigned short combine_dims(0,0)
dim shared as list_combine combine_stack(256)
dim shared as integer combine_stacksize
dim shared as integer combine_forcelinear
dim shared as double combine_getsigma
dim shared as double combine_ma1
dim shared as double combine_ma2
dim shared as integer combine_minioc
dim shared as integer combine_maxioc
dim shared as integer combine_fromlen
dim shared as integer combine_tolen

'default n-gram arrays:
'------------------------------------------------------------
static shared as ubyte g1(25)
static shared as ubyte g2(),g2b()
static shared as ubyte g3(),g3b()
static shared as ubyte g4(),g4b()
static shared as ubyte g5(),g5b()
static shared as ubyte g6(),g6b()
static shared as ubyte g7(),g7b()

static shared as ubyte g5p(25,25,25,25,25)
static shared as ubyte g5s(26,26,26,26,26) '+spaces

static shared as ushort g6w() 'letter n-grams to words
static shared as ushort g6w2()
static shared as ushort g7w()

static shared as ubyte wl()
static shared as ubyte wl2()
static shared as ushort wl13() ' translation of wl indexes to wl3 indexes
static shared as ubyte wl3()
static shared as ushort wlptr()

'beijinghouse n-gram arrays/variables:
'------------------------------------------------------------

static shared as ubyte fg6(0,0,0,0,0,0)		' first gram array
static shared as ubyte sg6(0,0,0,0,0,0)		' second gram array
static shared as ubyte slg6(0,0,0,0,0,0)		' second last gram array
static shared as ubyte lg6(0,0,0,0,0,0)		' last gram array

static shared as ulong max_allowed_table_index
static shared as ubyte bh8()

'static shared as ubyte bh8f()								' 8-gram first, last, second, secondlast-grams
'static shared as ubyte bh8l()
'static shared as ubyte bh8s()
'static shared as ubyte bh8sl()


static shared as ulong bh4()
'static shared as ubyte bh10()
'static shared as ulong bh5()
static shared as ubyte cachebh80()
static shared as ubyte cachebh81()
static shared as ubyte cachebh82()
static shared as ubyte cachebh83()
static shared as ubyte cachebh84()
static shared as ubyte cachebh85()
static shared as ubyte cachebh86()
static shared as ubyte cachebh87()

dim shared as Integer ninegramformat
dim shared as Integer ninegram_offset
dim shared as Integer ninegram_half(26)
dim shared as Integer ninegram_quartile
dim shared as Integer tengramformat
dim shared as Integer tengram_quartile
dim shared as Integer z_offset

static shared as uinteger g53()   ' 9-gram / 10-gram subarrays
static shared as uinteger g54()

static shared as UByte gxa9()
static shared as UByte gxb9()
static shared as UByte gxc9()
static shared as UByte gxd9()
static shared as UByte gxe9()
static shared as UByte gxf9()
static shared as UByte gxg9()
static shared as UByte gxh9()
static shared as UByte gxi9()
static shared as UByte gxj9()
static shared as UByte gxk9()
static shared as UByte gxl9()
static shared as UByte gxm9()
static shared as UByte gxn9()
static shared as UByte gxo9()
static shared as UByte gxp9()
static shared as UByte gxq9()
static shared as UByte gxr9()
static shared as UByte gxs9()
static shared as UByte gxt9()
static shared as UByte gxu9()
static shared as UByte gxv9()
static shared as UByte gxw9()
static shared as UByte gxx9()
static shared as UByte gxy9()
static shared as UByte gxz9()

'beijinghouse gov n-gram variables:
'------------------------------------------------------------
	
' only for uncompressed static functions
static shared as ulongint gov_width 
static shared as  integer fileformat

' for both uncompressed and compressed static functions
static shared as ulongint gov_multiplier
static shared as ulongint gov_globalseed
static shared as ulongint gov_offset_and_seed_length
static shared as ulongint gov_array_length
static shared as ulongint ptr gov_offset_and_seed
static shared as ulongint ptr gov_array

' only for compressed static functions
static shared as ulongint gov_global_max_codeword_length
static shared as ulongint gov_escaped_symbol_length
static shared as ulongint gov_escape_length
static shared as  integer gov_w
static shared as ulongint gov_end
static shared as ulongint gov_start
static shared as ulongint gov_decoding_table_length
static shared as ulongint gov_num_symbols
static shared as ulongint ptr gov_last_codeword_plus_one
static shared as uinteger ptr gov_how_many_up_to_block
static shared as ubyte ptr gov_shift
static shared as ulongint ptr gov_symbol

static shared as ubyte g3keyguard()
static shared as ubyte g4keyguard()
static shared as ubyte g5keyguard()

static shared as integer count3
static shared as integer count4
static shared as integer count5

static shared as integer in_picker	
Static Shared As UByte ptr filetail
Static Shared As ZString Ptr keyguardstr

#include "gov.bi"

'load n-gram bias arrays:
'------------------------------------------------------------
redim shared as ubyte ngbn(0,0)
redim shared as ulong ngbf(0)
redim shared as ubyte ngbt(0)
redim shared as ubyte ngbd(0)
redim shared as ulong ngbl(0,0,0,0)

'asc2num
'------------------------------------------------------------
dim shared as ushort asc2num(255)
dim shared as ushort asc2num10(255)
dim shared as ushort asc2num100(255)
dim shared as ushort asc2num1000(255)
dim shared as ulong asc2num10000(255)
dim shared as ulong asc2num100000(255)
dim shared as ulong asc2num1000000(255)

' declare some large shared mem objects
static shared as short snba(0)
static shared as short snba1(0)
static shared as short snba2(0,0)
'static shared as uinteger firstbyte(0,0,0)
static shared as short cv1(0)
static shared as short cv2(0,0)
static shared as short symn(0,0)


'cycle types arrays
'------------------------------------------------------------
static shared as double ctmean(0,0)
redim shared as double cto(0 to 0,0,0,0)

'declare subs
'------------------------------------------------------------
declare sub file_load()
declare sub file_load2()
declare sub file_save()
declare sub file_save_as()

declare sub create_window_main
declare sub create_window_dimension
declare sub create_window_transposition
declare sub create_window_creatematrix(byval x0 as long,byval y0 as long)
declare sub create_window_combine
declare sub create_window_manipulation
declare sub create_window_optionssolver
declare sub create_window_cribgrid(byval x0 as long,byval y0 as long,byval fresh as byte)
declare sub create_window_polyphones

declare sub mainloop

declare sub stats_unigrams
declare sub stats_ngrams
declare sub stats_encoding
declare sub stats_perfectsymbolcycles(byval tn_ptr as any ptr)
declare sub stats_direction(byval tn_ptr as any ptr)
declare sub stats_keywordlength(byval tn_ptr as any ptr)
declare sub stats_encodingrandomization(byval tn_ptr as any ptr)
declare sub stats_omnidirectional(byval ng as short)
declare sub stats_periodic(byval m as integer)
declare sub stats_findrearrangement(byval tn_ptr as any ptr)
declare sub stats_outputgraphs(byval tn_ptr as any ptr)
declare sub stats_symbolcyclepatterns(byval tn_ptr as any ptr)
declare sub stats_cycletypes(byval tn_ptr as any ptr)
declare sub stats_compare_keymapping
declare sub stats_compare_kasiskeexamination
declare sub stats_compare_equalitytest
declare sub stats_compare_symbolcyclepatterns(byval tn_ptr as any ptr)
declare sub stats_observations(byval tn_ptr as any ptr)
declare sub stats_hafershifts

declare sub get_native_dimensions
declare sub get_symbols(byval instance as byte)
declare sub output_graph(byval t as short,byval chs as short,byval hs as string,byval ns as string)
declare sub output_graph2(byval t as short,byval chs as short,byval hs as string,byval ns as string)
declare sub get_cyclepatterns(array()as long,byval l as integer,byval s as integer,byval cs as integer,byval fl as integer,byval slot as integer)
declare sub output_colormap(vls()as double,cip()as long,byval l as integer,byval dx as integer,byval dy as integer,byval norm as double,byval ns as string)
declare sub quicksort_sort2(byval low as integer,byval high as integer)
declare sub quicksort_short(array()as short,byval low as integer,byval high as integer)
declare sub normalize_ngramfactor
declare sub thread_batch_settings(byval none as any ptr)

declare sub update_solver_status
declare sub toggle_solverthreads(array()as integer,byval length as integer,byval symbols as integer,byval dim_x as integer,byval dim_y as integer,byval outputdir as string,byval toggle as integer,byval thread_from as integer,byval thread_to as integer)
declare sub thread_combine(byval none as any ptr)
declare sub get_combinations
declare sub stop_current_task
declare sub pause_current_task
declare sub file_load_settings
declare sub set_solverhighlight(byval solvername as string)
declare sub clean_thread_information
declare sub pickletter_caching(byval showmsg as ubyte)
declare sub generate_permutations(n as long)

'tests
'------------------------------------------------------------
declare sub generate_substrings
declare sub generate_nullsandskips

'declare solvers
'------------------------------------------------------------
declare sub bhdecrypt_234567810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_rowbound_34567g(byval tn_ptr as any ptr)
declare sub bhdecrypt_seqhom_234567810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_sparsepoly_567810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_vigenere_34567810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_poly_567810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_poly_hafer_567810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_mergeseqhom(byval tn_ptr as any ptr)
declare sub bhdecrypt_bigram_810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_groups_810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_higherorder_810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_allg_bhgov(byval tn_ptr as any ptr)
declare sub bhdecrypt_810g(byval tn_ptr as any ptr)
declare sub bhdecrypt_fast_5g(byval tn_ptr as any ptr)

'declare threads
'------------------------------------------------------------
declare sub thread_benchmark(byval none as any ptr)
declare sub thread_load_ngrams(byval none as any ptr)
declare sub thread_load_ngrambias(byval none as any ptr)

declare sub thread_batch_ngrams_substitution(byval none as any ptr)
declare sub thread_batch_ciphers_substitution(byval none as any ptr)
declare sub thread_batch_ciphers_mergeseqhom(byval none as any ptr)

declare sub thread_solve_substitution(byval none as any ptr)
declare sub thread_solve_polyphones_user(byval none as any ptr)
declare sub thread_solve_polyphones_auto(byval none as any ptr)
declare sub thread_solve_polyphones_hafer(byval none as any ptr)
declare sub thread_solve_simpletransposition(byval none as any ptr)
declare sub thread_solve_vigenere(byval none as any ptr)
declare sub thread_solve_vigenerelist(byval none as any ptr)
declare sub thread_solve_rowbound(byval none as any ptr)
declare sub thread_solve_mergeseqhom(byval none as any ptr)
declare sub thread_solve_genhc(byval none as any ptr)
declare sub thread_solve_wordcribs(byval none as any ptr)
declare sub thread_solve_cribgrid(byval none as any ptr)
declare sub thread_solve_criblist(byval none as any ptr)
declare sub thread_solve_units(byval none as any ptr)
declare sub thread_solve_rowbound_fragments(byval none as any ptr)
declare sub thread_solve_rectangles(byval none as any ptr)

declare sub ext_bigram_beam_columnartransposition(byval t as short,byval l as short,byval s as short,byval kl as short)
declare sub ext_bigram_beam_columnarrearrangement(byval t as short,byval l as short,byval s as short,byval kl as short)
declare sub ext_bhd_beam_columnarrearrangement(byval t as short,byval l as short,byval s as short,byval kl as short)
declare sub ext_bhd_beam_columnartransposition(byval t as short,byval l as short,byval s as short,byval kl as short)
declare sub ext_bhd_beam_nullsandskips(byval t as short,byval l as short,byval s as short,byval kl as short)

'declare functions
'------------------------------------------------------------
declare function remext(byval s as string)as string
Declare function m_ioc(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_entropy(array()as long,byval l as integer,byval s as integer)as double
declare function m_flatness(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_smoothness(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_midpointshift(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_appearance(array()as long,byval l as integer,byval s as integer,byval o as integer,byval n as integer)as double
declare function m_sequential(array()as long,byval l as integer,byval s as integer,byval o as integer,byval n as integer)as double
declare function m_bigrams(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_unigrams(array()as long,byval l as integer,byval s as integer,byval ml as integer,byval o as integer)as double
declare function m_posshift(array()as long,byval l as integer,byval s as integer,byval lk as integer,byval n as integer)as double
declare function info_to_string(array()as long,byval l as integer,byval dx as integer,byval dy as integer,byval numerical as integer,byval addspaces as integer,byval nolinebreaks as integer)as string
declare function frequencies(array()as long,byval l as integer,byval s as integer,byval ngs as integer,byval num as integer,byval mf as integer)as string
declare function gcd(byval a as integer,byval b as integer)as integer
declare function cstate_operation(byval instate as integer,byval outstate as integer,byval operation as string,arg()as double)as string
declare function cstate_symbolcount(byval instate as integer,byval l as integer,byval s as integer)as integer
declare function cstate_nba(byval instate as integer,byval outstate as integer,byval l as integer,byval s as integer)as integer
declare function m_ioc2(array()as short,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_ioc3(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_deep(array()as long,byval l as integer,byval s as integer,byval d as integer,byval m as integer,byval n as integer)as double
declare function string_to_info(byval s as string)as string
declare function m_asymmetry(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function nba_to_info_out(array()as long,byval l as integer,byval s as integer)as integer
declare function nba_to_info_out_short(array()as short,byval l as integer,byval s as integer)as integer
declare function m_slope(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_highestfrequency(array()as long,byval l as integer,byval s as integer)as double
declare function m_depth(array()as long,byval l as integer,byval s as integer,byval d as integer,byval m as integer,byval n as integer)as double
declare function m_fastbigrams(array()as long,byval l as integer,byval s as integer)as integer
declare function m_gridmatch(g1a()as long,g2a()as long,byval x1 as integer,byval y1 as integer,byval x2 as integer,byval y2 as integer,byval o as integer)as double
declare function m_primephobia(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_isdp(array()as long,byval l as integer,byval s as integer,byval n as integer)as double
declare function m_ngrams(array()as long,byval l as integer,byval o as integer)as double
declare function m_posshift2(array()as long,byval l as integer,byval s as integer,byval lk as integer)as double
declare function m_bigramposmodn(array()as long,byval l as integer,byval s as integer,byval m as integer,byval n as integer)as double
declare function m_unigramdistance(array()as long,byval l as integer,byval s as integer)as double
declare function m_2cycles(array()as long,byval l as integer,byval s as integer,byval weight as double)as double
declare function m_2cycles_stats(array()as long,byval l as integer,byval s as integer,byval weight as double,byval n as integer)as double
declare function m_3cycles(array()as long,byval l as integer,byval s as integer,byval weight as double)as double
declare function m_3cycles_stats(array()as long,byval l as integer,byval s as integer,byval weight as double,byval n as integer)as double
declare function m_2cycles_perfect(array()as long,byval l as integer,byval s as integer)as double
declare function m_3cycles_perfect(array()as long,byval l as integer,byval s as integer)as double
declare function m_unicycle(cycle()as long,byval l as integer)as double
declare function m_ngrams_nba(cip()as long,byval l as short)as double
declare function m_pccycles_longshort(pt()as long,ct()as short,byval l as short,byval s2 as short)as double
declare function m_pccycles_shortshort(pt()as short,ct()as short,byval l as short,byval s2 as short)as double
declare function m_fastbigrams_short(array()as short,byval l as integer,byval s as integer)as integer
declare function m_fastbigrams_cstate(byval instate as short,byval l as short,byval s as short)as short
declare function m_cycletypes(array()as long,byval l as integer,byval s as integer,byval cs as integer,cta()as short)as double
declare function m_unigramunitnorepeats(array()as long,byval l as integer,byval s as integer,byval dx as integer,byval dy as integer,byval m as integer,byval o as integer,byval n as integer)as double
declare function m_periods(array()as long,byval l as integer,byval s as integer)as double
declare function m_periodsplusoco(array()as long,byval l as integer,byval s as integer,byval dx as integer,byval dy as integer)as double
declare function m_unigramperiodic(array()as long,byval l as integer,byval s as integer,byval m as integer,byval ue as integer,byval p as integer,byval o as integer)as double
declare function m_iocperiodic(array()as long,byval l as integer,byval s as integer,byval m as integer,byval ue as integer,byval p as integer,byval o as integer)as double
declare function m_unigramperiodicvs(array()as long,byval l as integer,byval s as integer,byval m as integer,byval ue as integer,byval p as integer,byval o as integer)as double
declare function m_unigramdistanceuo(array()as long,byval l as integer,byval s as integer,byval uo as integer,byval u as integer)as double
declare function m_unigram_tmb(array()as long,byval l as integer,byval s as integer,byval tmb as integer,byval m as integer)as double
declare function stdev(byval a as double,byval items as integer,sda()as double)as double
declare function prime(byval n as integer)as integer
declare function m_chi2_english(frq1()as double,frq2()as double,byval l as short)as double
declare function m_bigramstructure(cip()as long,byval l as short,byval s as short,byval dx as short,byval dy as short,byval utp as short,byval p as short)as double
declare function m_fastbigrams_alphabet(array()as long,byval l as integer,byval s as integer,byval m as integer)as double
declare function m_pivots(array()as long,byval l as integer,byval dx as integer,byval dy as integer,byval pl as integer)as double
declare function m_chi2(set1()as double,set2()as double,byval s as integer,byval l as integer)as double
declare function m_equality(array1()as long,array2()as long,byval l1 as integer,byval l2 as integer,byval s1 as integer,byval s2 as integer,byval o as integer)as double
declare function m_reversal(array()as long,byval l as integer,byval s as integer)as integer
declare function m_fasttrigrams(array()as long,byval l as integer,byval s as integer)as integer
declare function m_ngramfragments(array()as long,byval l as integer,byval s as integer,byval ngs as integer)as double
declare function m_normor(frq()as double)as double
declare function m_npairs(array()as long,l as integer,n as integer)as double
declare function fastpow1_single(byval x as single,byval b as single)as single
declare function stt(byval sec as double)as string
declare function m_shortestsubstring(array()as long,byval l as integer,byval s as integer)as double
declare function yesno(byval a as integer)as string
declare function m_contactvariety(array()as long,byval l as integer,byval s as integer)as double
declare function m_gridioc(array()as long,byval l as integer,byval s as integer,byval hv as integer,byval kl as integer)as double
declare function m_2cyclespectrum(nba()as long,byval l as integer,byval s as integer)as double
declare function m_2cyclespectrum_short(nba()as short,byval l as integer,byval s as integer)as double
declare function m_repeats(cip()as long,byval l as short,byval o as short)as string
declare function m_lean(array()as long,byval l as integer,byval s as integer,byval dx as integer,byval dy as integer,byval o as integer)as double
declare function m_2cycles_perfect_cyclebreaks(array()as long,byval l as integer,byval s as integer)as double
declare function m_wordflow(sol()as long,byval l as integer)as double
declare function m_adjacency(array()as long,byval l as integer,byval dx as integer,byval dy as integer)as double
declare function crc_32(buf as byte ptr,buflen as ulong) as ulong

'program root
'------------------------------------------------------------
program_name="BHdecrypt 1.19"
using fb
mainloop

sub file_load_settings
	
	dim as integer i
	
	solvesub_ngramloctemp="\N-grams\6-grams_english_beijinghouse_v6.txt.zst"
	solvesub_batchngramsrestarts=50
	solvesub_iterations=500000
	solvesub_iterationsfactor=1.04
	solvesub_hciterations=5000
	solvesub_hciterationsfactor=1.04
	solvesub_restarts=1
	solvesub_subrestartlevels=3
	solvesub_temperature=1000
	'solvesub_cputhreads=2
	solvesub_outputdir=1
	solvesub_outputbatch=0
	solvesub_outputimp=1
	solvesub_advstats=1
	solvesub_overwriteoutput=0
	solvesub_pccyclesformat=0
	solvesub_scoreover=0
	solvesub_multiplicityweight=0
	solvesub_matchweight=1
	solvesub_vigenerekeylength=6
	solvesub_polyphones=5
	solvesub_cyclealphabetsize=26
	solvesub_cyclelengthweight=0.73
	solvesub_rndcyclearg=0
	solvesub_pcmode=1
	solvesub_transstack=3
	solvesub_bigramdepth=729
	solvesub_vigenerebycolumns=1
	solvesub_batchshutdown=0
	'solvesub_ctcolumns=6
	solvesub_ctdepth=5
	solvesub_ctmode=1
	solvesub_pnperiod=1
	solvesub_pnnulls=5
	solvesub_pndepth=1
	solvesub_tpseqhom=0
	solvesub_pnmannulls=0
	solvesub_pnmanskips=0
	solvesub_pnover=22500
	solvesub_vigeneresubtract=0
	solvesub_seqweight=5
	solvesub_nshctemp=40
	solvesub_nshcshift=30
	solvesub_nshcshiftdiv=2
	solvesub_nshcrestartsmax=100
	solvesub_incpolyphones=1
	solvesub_tsbatchciphersrestarts=1
	solvesub_higherorderhomophonicweight=2
	solvesub_higherorderhomophonic=2
	solvesub_accshortcircuit=1
	solvesub_bhmaxgb=195
	solvesub_batchciphersbigrams=0
	solvesub_ngramlogcutoff=0
	ngrambias_showmsg=1
	solvesub_ngramcaching=0
	solvesub_sdbias=0.55
	solvesub_solutionreleasetimer=10
	solvesub_addspaces=1
	solvesub_addspacesquality=25
	
	solvesub_bigramheatmap=0
	solvesub_bigrambestsol=0.5
	solvesub_bigramhomwdiv=300
	solvesub_bigramautocrib=0
	
	twait=4
	memcheck=1
	screensizecheck=0
	
	stats_encrndtrials=100
	stats_dirrndtrials=100
	stats_nsymbolcyclesweight=5
	stats_bigramsmod=26
	stats_symbolcyclepatternsrndtrials=1000
	
	solvesub_rowboundfragments=20
	solvesub_rowboundcribfragments=20
	solvesub_rowboundtemp=400
	solvesub_rowboundhcmode1itfact=2
	solvesub_rowboundhkeys=10
	solvesub_rowbounddistinct=0.8
	solvesub_rowboundfine=0.2
	solvesub_rowboundover=999999999 '22500
	solvesub_rowboundsubrestarts=3
	solvesub_rowboundcheckhistory=0
	
	wc_prevcheck_showcipher=0 '1 to show cipher on program start
	batchciphers_showmsg=1
   
   #ifndef __FB_LINUX__
		dim as SYSTEM_INFO sysinf
		getsysteminfo(@sysinf)
		solvesub_cputhreads=(sysinf.dwnumberofprocessors)-1 'use one CPU thread less than total by default
	#else
		solvesub_cputhreads=GetCPUCores
	#endif
   
   if solvesub_cputhreads=0 then solvesub_cputhreads=1
   threads=solvesub_cputhreads
	
	if fileexists("settings.ini")=0 then 'does not exist
		open "settings.ini" for output as #1
			print #1,"Default n-grams: \N-grams\6-grams_english_beijinghouse_v6.txt.zst"
			print #1,"(General) Thread wait: "+str(twait)
			print #1,"(General) Iterations: "+str(solvesub_iterations)
			print #1,"(General) Iterations factor: "+str(solvesub_iterationsfactor)
			print #1,"(General) Hill climber iterations: "+str(solvesub_hciterations)
			print #1,"(General) Hill climber Iterations factor: "+str(solvesub_hciterationsfactor)
			print #1,"(General) Multiplicity weight: "+str(solvesub_multiplicityweight)
			print #1,"(General) Output to file: "+yesno(solvesub_outputdir)
			print #1,"(General) Output to batch: "+yesno(solvesub_outputbatch)
			print #1,"(General) Output scores over: "+str(solvesub_scoreover)
			print #1,"(General) Output improvements only: "+yesno(solvesub_outputimp)
			print #1,"(General) Output additional stats: "+yesno(solvesub_advstats)
			print #1,"(General) Overwrite existing solver output: "+yesno(solvesub_overwriteoutput)	
			print #1,"(General) Add PC-cycles to file output format: "+yesno(solvesub_pccyclesformat)
			print #1,"(General) Restarts: "+str(solvesub_restarts)
			'print #1,"(General) Temperature: "+str(solvesub_temperature)
			print #1,"(General) Enable memory checks: "+yesno(memcheck)
			print #1,"(General) Enable screen size checks: "+yesno(screensizecheck)
			print #1,"(General) 8-gram memory limit: "+str(solvesub_bhmaxgb)
			print #1,"(General) 8-gram caching: "+yesno(solvesub_ngramcaching)
			print #1,"(General) Add spaces to output: "+yesno(solvesub_addspaces)
			print #1,"(General) Add spaces to output iterations: "+str(solvesub_addspacesquality)
			print #1,"(General) N-gram log value cut-off: "+str(solvesub_ngramlogcutoff)
			print #1,"(Batch n-grams) Restarts: "+str(solvesub_batchngramsrestarts)
			print #1,"(Batch ciphers) Only process ciphers with bigram repeats over: "+str(solvesub_batchciphersbigrams)
			print #1,"(Batch ciphers & n-grams) Shutdown computer after task completion: "+yesno(solvesub_batchshutdown)
			print #1,"(Batch ciphers & settings) Accuracy short circuit: "+yesno(solvesub_accshortcircuit)
			'print #1,"(Bigram substitution) Output heatmap: "+yesno(solvesub_bigramheatmap)
			'print #1,"(Bigram substitution) Reuse best solution ratio: "+str(solvesub_bigrambestsol)
			'print #1,"(Bigram substitution) Auto-crib restarts: "+str(solvesub_bigramautocrib)
			'print #1,"(Bigram substitution) Bigram homophone weight divider: "+str(solvesub_bigramhomwdiv)
			print #1,"(Higher-order homophonic) N-order: "+str(solvesub_higherorderhomophonic)
			print #1,"(Higher-order homophonic) Separation weight divider: "+str(solvesub_higherorderhomophonicweight)
			print #1,"(Substitution + columnar transposition & rearrangement) Search depth: "+str(solvesub_ctdepth)
			print #1,"(Substitution + columnar transposition & rearrangement) Use bigrams: "+yesno(solvesub_ctmode)
			print #1,"(Substitution + nulls and skips) Period: "+str(solvesub_pnperiod)
			print #1,"(Substitution + nulls and skips) Nulls and skips: "+str(solvesub_pnnulls)
			print #1,"(Substitution + nulls and skips) Manual nulls: "+str(solvesub_pnmannulls)
			print #1,"(Substitution + nulls and skips) Manual skips: "+str(solvesub_pnmanskips)
			'print #1,"(Substitution + nulls and skips) Search depth: "+str(solvesub_pndepth)
			'print #1,"(Substitution + polyphones [auto]) Extra letters: "+str(solvesub_polyphones)
			'print #1,"(Substitution + polyphones [auto]) Increment extra letters: "+yesno(solvesub_incpolyphones)
			print #1,"(Substitution + sequential homophones) Sequential weight: "+str(solvesub_seqweight)
			print #1,"(Substitution + sparse polyalphabetism) Polyalphabetism weight: "+str(solvesub_matchweight)
			print #1,"(Substitution + simple transposition) Use sequential homophones: "+yesno(solvesub_tpseqhom)
			print #1,"(Substitution + simple transposition) PC-cycles, use untransposed texts: "+yesno(solvesub_pcmode)
			print #1,"(Substitution + vigen�re) Keyword length: "+str(solvesub_vigenerekeylength)
			print #1,"(Substitution + vigen�re) By columns: "+yesno(solvesub_vigenerebycolumns)
			print #1,"(Substitution + vigen�re) Use subtraction: "+yesno(solvesub_vigeneresubtract)
			print #1,"(Merge sequential homophones) Cycle weight: "+str(solvesub_cyclelengthweight)
			print #1,"(Merge sequential homophones) Alphabet size: "+str(solvesub_cyclealphabetsize)
			print #1,"(Merge sequential homophones) Use random arguments: "+yesno(solvesub_rndcyclearg);
		close #1
		threads=solvesub_cputhreads
	else
		dim as integer a
		dim as string s,pre,suf
		open "settings.ini" for binary as #1
		do
			line input #1,s
			a=instr(s,":")
			pre=""
			suf=""
			pre=ltrim(rtrim(left(s,a-1)))
			suf=ltrim(rtrim(right(s,len(s)-a)))
			if lcase(chr(asc(suf,1)))="n" then suf="0"
			if lcase(chr(asc(suf,1)))="y" then suf="1"
			select case lcase(pre)
				case "development"
					if val(suf)=1 then development=1
				case "combine"
					if val(suf)=1 then combine=1
				case "bigram"
					if val(suf)=1 then bigramsolver=1
				case "(general) sub restarts"
					if val(suf)>0 then
						solvesub_subr(1)=val(chr(asc(suf,1)))
						solvesub_subr(2)=val(chr(asc(suf,2)))
						solvesub_subr(3)=val(chr(asc(suf,3)))
						for i=1 to 3
							if solvesub_subr(i)=0 then solvesub_subr(i)=1
						next i
					end if
				case "default n-grams"
					if len(suf)>0 then solvesub_ngramloctemp=suf
				case "(general) thread wait"
					if val(suf)>=0 then twait=val(suf)
				case "(general) iterations"
					if val(suf)>=100000 then solvesub_iterations=val(suf)	
				case "(general) iterations factor"
					if val(suf)>=1 then solvesub_iterationsfactor=val(suf)
				case "(general) hill climber iterations"
					if val(suf)>=1000 then solvesub_hciterations=val(suf)	 
				case "(general) hill climber iterations factor"
					if val(suf)>=1 then solvesub_hciterationsfactor=val(suf)
				case "(general) multiplicity weight"
					if val(suf)>=0 then solvesub_multiplicityweight=val(suf)
				case "(general) output to file"
					if val(suf)=0 or val(suf)=1 then solvesub_outputdir=val(suf)
				case "(general) output to batch"
					if val(suf)=0 or val(suf)=1 then solvesub_outputbatch=val(suf)
				case "(general) output improvements only"
					if val(suf)=0 or val(suf)=1 then solvesub_outputimp=val(suf)
				case "(general) output scores over"
					if val(suf)>=0 then solvesub_scoreover=val(suf)
				case "(general) output additional stats"
					if val(suf)=0 or val(suf)=1 then solvesub_advstats=val(suf)
				case "(general) overwrite existing solver output"
					if val(suf)=0 or val(suf)=1 then solvesub_overwriteoutput=val(suf)
				case "(general) add pc-cycles to file output format"
					if val(suf)=0 or val(suf)=1 then solvesub_pccyclesformat=val(suf)
				case "(general) restarts"
					if val(suf)>=1 then solvesub_restarts=val(suf)
				case "(general) temperature"
					if val(suf)>0 then solvesub_temperature=val(suf)
				case "(general) enable memory checks"
					if val(suf)=0 or val(suf)=1 then memcheck=val(suf)
				case "(general) enable screen size checks"
					if val(suf)=0 or val(suf)=1 then screensizecheck=val(suf)
				case "(general) 8-gram memory limit"
					if val(suf)>=1 and val(suf)<=1048576 then solvesub_bhmaxgb=val(suf)
				case "(general) 8-gram caching"
					if val(suf)=0 or val(suf)=1 then solvesub_ngramcaching=val(suf)
				case "(general) add spaces to output"
					if val(suf)=0 or val(suf)=1 then solvesub_addspaces=val(suf)
				case "(general) add spaces to output iterations"
					if val(suf)>=0 andalso val(suf)<=1000000 then solvesub_addspacesquality=val(suf)
				case "(general) n-gram log value cut-off"
					if val(suf)>=0 andalso val(suf)<=254 then solvesub_ngramlogcutoff=val(suf)
				case "(batch n-grams) iterations"
					if val(suf)>=1 then solvesub_batchngramsrestarts=val(suf)
				case "(batch ciphers) only process ciphers with bigram repeats over"
					if val(suf)>=0 andalso val(suf)<=constcip then solvesub_batchciphersbigrams=val(suf)
				case "(batch ciphers & n-grams) shutdown computer after task completion"
					if val(suf)=0 or val(suf)=1 then solvesub_batchshutdown=val(suf)
				case "(batch ciphers & settings) accuracy short circuit"
					if val(suf)=0 or val(suf)=1 then solvesub_accshortcircuit=val(suf)
				case "(higher-order homophonic) n-order"
					if val(suf)>=2 andalso val(suf)<=5 then solvesub_higherorderhomophonic=val(suf)
				case "(higher-order homophonic) separation weight divider"
					if val(suf)>=0 andalso val(suf)<=1000000 then solvesub_higherorderhomophonicweight=val(suf)
				case "(bigram substitution) output heatmap"
					if val(suf)=0 or val(suf)=1 then solvesub_bigramheatmap=val(suf)
				case "(bigram substitution) reuse best solution ratio"
					if val(suf)>=0 or val(suf)<=1 then solvesub_bigrambestsol=val(suf)
				case "(bigram substitution) auto-crib restarts"
					if val(suf)>=0 andalso val(suf)<=1000000 then solvesub_bigramautocrib=val(suf)
				case "(bigram substitution) bigram homophone weight divider"
					if val(suf)>=0 andalso val(suf)<=1000000 then solvesub_bigramhomwdiv=val(suf)
				case "(substitution + columnar transposition & rearrangement) search depth"
					if val(suf)>=1 andalso val(suf)<=7 then solvesub_ctdepth=val(suf)
				case "(substitution + columnar transposition & rearrangement) use bigrams"
					if val(suf)=0 or val(suf)=1 then solvesub_ctmode=val(suf)
				case "(substitution + nulls and skips) period"
					if val(suf)>=1 andalso val(suf)<=constcip then solvesub_pnperiod=val(suf)
				case "(substitution + nulls and skips) nulls and skips"
					if val(suf)>=1 andalso val(suf)<=1000 then solvesub_pnnulls=val(suf)
				case "(substitution + nulls and skips) manual nulls"
					if val(suf)>=0 andalso val(suf)<=1000 then solvesub_pnmannulls=val(suf)
				case "(substitution + nulls and skips) manual skips"
					if val(suf)>=0 andalso val(suf)<=1000 then solvesub_pnmanskips=val(suf)
				case "(substitution + polyphones [auto]) extra letters"
					if val(suf)>0 then solvesub_polyphones=val(suf)
				case "(substitution + polyphones [auto]) increment extra letters"
					if val(suf)=0 or val(suf)=1 then solvesub_incpolyphones=val(suf)
				case "(substitution + sequential homophones) sequential weight"
					if val(suf)>0 then solvesub_seqweight=val(suf)
				case "(substitution + sparse polyalphabetism) polyalphabetism weight"
					if val(suf)>=0 then solvesub_matchweight=val(suf)
				case "(substitution + simple transposition) use sequential homophones"
					if val(suf)=0 or val(suf)=1 then solvesub_tpseqhom=val(suf)
				case "(substitution + simple transposition) pc-cycles, use untransposed texts"
					if val(suf)=0 or val(suf)=1 then solvesub_pcmode=val(suf)
				case "(substitution + vigen�re) keyword length"
					if val(suf)>0 then solvesub_vigenerekeylength=val(suf)
				case "(substitution + vigen�re) by columns"
					if val(suf)=0 or val(suf)=1 then solvesub_vigenerebycolumns=val(suf)
				case "(substitution + vigen�re) use subtraction"
					if val(suf)=0 or val(suf)=1 then solvesub_vigeneresubtract=val(suf)
				case "(merge sequential homophones) alphabet size"
					if val(suf)>2 andalso val(suf)<256 then solvesub_cyclealphabetsize=val(suf)
				case "(merge sequential homophones) cycle weight"
					if val(suf)>0 andalso val(suf)<=1 then solvesub_cyclelengthweight=val(suf)
				case "(merge sequential homophones) use random arguments"
					if val(suf)=0 or val(suf)=1 then solvesub_rndcyclearg=val(suf)
			end select
		loop until eof(1)
		close #1
	end if
	
end sub

sub file_load()
	
	dim as string buffer,tmp_file,old_file_name=file_name
	tmp_file=ui_loadsavedialog(0,"Open file",filter,1,basedir+"\Ciphers\")
	if len(tmp_file)>0 then
		file=tmp_file
		'path_file_name=tmp_file
		file_name=right(tmp_file,len(tmp_file)-instrrev(tmp_file,"\"))
		if tmp_file<>"" then
			open tmp_file for binary as #2
			if lof(2)>10000 then 
				close #2
				ui_editbox_settext(output_text,"Error: file size > 10000 characters")
				file_name=old_file_name
				exit sub
			end if
			buffer=space(lof(2))
			get #2,,buffer
			close #2
			ui_editbox_settext(input_text,buffer)
			ui_setwindowtext(window_main,program_name+" - "+tmp_file)
			path_file_name=tmp_file
		end if
	end if
	
end sub

sub file_load2()
	
	dim as string buffer,tmp_file,old_file_name=file_name
	tmp_file=ui_loadsavedialog(0,"Open file",filter,1,basedir+"\Ciphers\")
	if len(tmp_file)>0 then
		file=tmp_file
		'path_file_name=tmp_file
		if tmp_file<>"" then
			open tmp_file for binary as #2
			if lof(2)>10000 then 
				close #2
				ui_editbox_settext(output_text,"Error: file size > 10000 characters")
				exit sub
			end if
			buffer=space(lof(2))
			get #2,,buffer
			close #2
			ui_editbox_settext(output_text,buffer)
			path_file_name=tmp_file
		end if
	end if
	
end sub

sub file_save()
	
	'file=file_name
	if path_file_name<>"" then
		open path_file_name for output as #2
		put #2,,ui_editbox_gettext(input_text)
		close #2
		ui_setwindowtext(window_main,program_name+" - "+path_file_name)
	end if

end sub

sub file_save_as()
	
	file=ui_loadsavedialog(1,"Save file as",filter,1,basedir+"\Ciphers\") 
	if file<>"" then
		if right(file,4)<>".txt" then file+=".txt"
		open file for output as #2
		put #2,,ui_editbox_gettext(input_text)
		close #2
		ui_setwindowtext(window_main,program_name+" - "+file)
		path_file_name=file
		file_name=right(file,len(file)-instrrev(file,"\"))
	end if

end sub

sub create_window_main
	
	window_main=ui_window_new(100,100,1190,750,program_name,WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	hmenu=ui_createmenu()
	
	'file
	'------------------------------------------------------------
	hfile=ui_createmenu()
	ui_menutitle(hmenu,hfile,"File") 
	ui_menuitem(hfile,1,"Open file")
	ui_menuitem(hfile,34,"Open file to output window")
	ui_appendmenu(hfile,MF_SEPARATOR,99,"")
	ui_menuitem(hfile,2,"Save file")
	ui_menuitem(hfile,3,"Save file as")
	ui_appendmenu(hfile,MF_SEPARATOR,99,"")
	ui_menuitem(hfile,16,"Batch ciphers (substitution)")
	ui_menuitem(hfile,44,"Batch ciphers (merge sequential homophones)")
	ui_menuitem(hfile,47,"Batch settings")
	ui_appendmenu(hfile,MF_SEPARATOR,99,"")
	ui_menuitem(hfile,22,"Batch n-grams (substitution)")
	ui_appendmenu(hfile,MF_SEPARATOR,99,"")
	ui_menuitem(hfile,14,"Load n-grams")
	ui_appendmenu(hfile,MF_SEPARATOR,99,"")
	ui_menuitem(hfile,21,"Load n-gram bias")
	'ui_appendmenu(hfile,MF_SEPARATOR,99,"")
	'ui_menuitem(hfile,54,"Output n-grams to binary")
	ui_appendmenu(hfile,MF_SEPARATOR,99,"")
	ui_menuitem(hfile,4,"Exit program")
	
	'functions
	'------------------------------------------------------------	
	hfunctions=ui_createmenu()
	ui_menutitle(hmenu,hfunctions,"Functions")
	ui_menuitem(hfunctions,6,"Symbols")
	ui_menuitem(hfunctions,5,"Dimension")
	ui_menuitem(hfunctions,20,"Manipulation")
	ui_menuitem(hfunctions,7,"Transposition")
	if combine=1 then
		ui_appendmenu(hfunctions,MF_SEPARATOR,99,"")
		ui_menuitem(hfunctions,18,"Combine")
	end if
	ui_appendmenu(hfunctions,MF_SEPARATOR,99,"")
	'ui_menuitem(hfunctions,53,"Generate unique substrings")
	ui_menuitem(hfunctions,33,"Create transposition matrix")
	
	'format
	'------------------------------------------------------------
	hedit=ui_createmenu()
	ui_menutitle(hmenu,hedit,"Format")
	'ui_menuitem(hedit,45,"Mark consonants")
	'ui_menuitem(hedit,46,"Mark vowels")
	ui_menuitem(hedit,15,"Unispace numerical")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	ui_menuitem(hedit,58,"Add spaces to plaintext")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	ui_menuitem(hedit,11,"Convert to numbered by appearance")
	ui_menuitem(hedit,10,"Convert to random ASCII symbols")
	ui_menuitem(hedit,8,"Convert symbols to ASCII numbers")
	ui_menuitem(hedit,9,"Convert numbers to ASCII symbols")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	ui_menuitem(hedit,23,"Convert letters to lowercase")
	ui_menuitem(hedit,24,"Convert letters to uppercase")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	ui_menuitem(hedit,32,"Convert tabs to spaces")
	ui_menuitem(hedit,40,"Convert spaces to rows")
	ui_menuitem(hedit,41,"Convert rows to spaces")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	hhconvert=ui_createmenu()
	ui_menutitle(hedit,hhconvert,"Convert ...")
	ui_menuitem(hhconvert,50,"Into numbered by appearance per symbol")
	ui_menuitem(hhconvert,51,"Into intersymbol distances")
	ui_menuitem(hhconvert,52,"Into incrementality map")
	ui_menuitem(hhconvert,49,"Into symbol frequencies")
	ui_appendmenu(hhconvert,MF_SEPARATOR,99,"")
	ui_menuitem(hhconvert,55,"Character pairs to numbers")
	ui_menuitem(hhconvert,56,"Character triples to numbers")
	ui_menuitem(hhconvert,57,"Character groups between spaces to numbers")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	ui_menuitem(hedit,28,"Remove spaces")
	ui_menuitem(hedit,26,"Remove numbers")
	ui_menuitem(hedit,25,"Remove line breaks")
	ui_menuitem(hedit,27,"Remove punctuation")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	ui_menuitem(hedit,29,"Randomize row order")
	ui_menuitem(hedit,30,"Randomize column order")
	ui_menuitem(hedit,19,"Randomize character order")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	ui_menuitem(hedit,43,"Rearrange sequentially")
	ui_menuitem(hedit,17,"Rearrange asequentially")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	ui_menuitem(hedit,39,"Square with spaces")
	ui_menuitem(hedit,31,"Add row and column numbers")
	ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	ui_menuitem(hedit,35,"Offset rows top-to-bottom")
	ui_menuitem(hedit,36,"Offset rows bottom-to-top")
	'ui_appendmenu(hedit,MF_SEPARATOR,99,"")
	
	'statistics
	'------------------------------------------------------------
	hstats=ui_createmenu()
	ui_menutitle(hmenu,hstats,"Statistics")
	ui_menuitem(hstats,100,"Unigrams")
	ui_menuitem(hstats,101,"N-grams")
	ui_menuitem(hstats,102,"Encoding")
	ui_menuitem(hstats,109,"Observations")
	ui_appendmenu(hstats,MF_SEPARATOR,99,"")
	ui_menuitem(hstats,110,"Hafer shifts")
	ui_menuitem(hstats,105,"Output graphs")
	
	hhperiodic=ui_createmenu()
	ui_menutitle(hstats,hhperiodic,"Periodic analysis")
	ui_menuitem(hhperiodic,134,"Using bigrams")
	ui_menuitem(hhperiodic,129,"Using symbols")
	ui_menuitem(hhperiodic,103,"Using raw IOC")
	'ui_menuitem(hhperiodic,153,"Using chi^2")
	ui_menuitem(hhperiodic,139,"Using self-compare chi^2")
	ui_menuitem(hhperiodic,126,"Using 2-symbol cycles")
	ui_menuitem(hhperiodic,137,"Using 3-symbol cycles")
	ui_menuitem(hhperiodic,136,"Using perfect 2-symbol cycles")
	ui_menuitem(hhperiodic,138,"Using perfect 3-symbol cycles")
	
	hhcompare=ui_createmenu()
	ui_menutitle(hstats,hhcompare,"Compare input and output")
	ui_menuitem(hhcompare,122,"Equality test")
	ui_menuitem(hhcompare,120,"Key mapping")
	ui_menuitem(hhcompare,121,"Kasiski examination")
	ui_menuitem(hhcompare,106,"2-symbol cycle 6-gram patterns")
	ui_appendmenu(hstats,MF_SEPARATOR,99,"")
	
	ui_menuitem(hstats,104,"Find vigen�re keyword length")
	
	hhomnidirectional=ui_createmenu()
	ui_menutitle(hstats,hhomnidirectional,"Find omnidirectional repeats")
	ui_menuitem(hhomnidirectional,123,"Using 2-grams")
	ui_menuitem(hhomnidirectional,124,"Using 3-grams")
	ui_appendmenu(hstats,MF_SEPARATOR,99,"")
	
	hhplaindir=ui_createmenu()
	ui_menutitle(hstats,hhplaindir,"Find plaintext direction")
	ui_menuitem(hhplaindir,201,"Using bigrams")
	ui_menuitem(hhplaindir,225,"Using trigrams")
	ui_menuitem(hhplaindir,212,"Using bigrams depth=2")
	ui_menuitem(hhplaindir,217,"Using odd bigrams")
	ui_menuitem(hhplaindir,202,"Using raw bigram IOC")
	ui_menuitem(hhplaindir,223,"Using deep bigrams")
	'ui_menuitem(hhplaindir,224,"Using reversal bigrams")
	'ui_menuitem(hhplaindir,220,"Using raw bigram IOC depth=2")
	ui_menuitem(hhplaindir,226,"Using 5-gram fragments")
	ui_menuitem(hhplaindir,204,"Using asymmetry")
	ui_menuitem(hhplaindir,233,"Using contact variety")
	ui_menuitem(hhplaindir,213,"Using repeats")
	ui_menuitem(hhplaindir,227,"Using doublets")
	ui_menuitem(hhplaindir,228,"Using triplets")
	
	hhencodedir=ui_createmenu()
	ui_menutitle(hstats,hhencodedir,"Find encoding direction")
	ui_menuitem(hhencodedir,234,"Using cycle spectrum")
	ui_menuitem(hhencodedir,205,"Using 2-symbol cycles")
	ui_menuitem(hhencodedir,206,"Using 3-symbol cycles")
	ui_menuitem(hhencodedir,230,"Using perfect 2-symbol cycles")
	ui_menuitem(hhencodedir,231,"Using perfect 3-symbol cycles")
	ui_menuitem(hhencodedir,207,"Using sequential")
	ui_menuitem(hhencodedir,208,"Using appearance")
	ui_menuitem(hhencodedir,211,"Using midpoint shift")
	ui_menuitem(hhencodedir,216,"Using keyword length")
	ui_menuitem(hhencodedir,215,"Using slope")
	ui_menuitem(hhencodedir,221,"Using slope NBA")
	ui_menuitem(hhencodedir,209,"Using unigrams")
	ui_menuitem(hhencodedir,210,"Using unigrams sliding")
	ui_menuitem(hhencodedir,218,"Using unigram distance")
	ui_menuitem(hhencodedir,229,"Using prime phobia")
	ui_menuitem(hhencodedir,232,"Using sub string position")
	ui_menuitem(hhencodedir,235,"Using horizontal symbol leaning")
	ui_menuitem(hhencodedir,236,"Using vertical symbol leaning")
	ui_appendmenu(hstats,MF_SEPARATOR,99,"")
	
	hhperfectcycles=ui_createmenu()
	ui_menutitle(hstats,hhperfectcycles,"Find perfect n-symbol cycles")
	ui_menuitem(hhperfectcycles,160,"Using 2-symbol cycles")
	ui_menuitem(hhperfectcycles,161,"Using 3-symbol cycles")
	ui_menuitem(hhperfectcycles,162,"Using 4-symbol cycles")
	ui_menuitem(hhperfectcycles,163,"Using 5-symbol cycles")
	ui_menuitem(hhperfectcycles,164,"Using 6-symbol cycles")
	ui_menuitem(hhperfectcycles,165,"Using 7-symbol cycles")
	ui_menuitem(hhperfectcycles,166,"Using 8-symbol cycles")
	ui_appendmenu(hstats,MF_SEPARATOR,99,"")
	
	hhcycletypes=ui_createmenu()
	ui_menutitle(hstats,hhcycletypes,"Find n-symbol cycle types")
	ui_menuitem(hhcycletypes,171,"Using 3-symbol cycles")
	ui_menuitem(hhcycletypes,172,"Using 4-symbol cycles")
	ui_menuitem(hhcycletypes,173,"Using 5-symbol cycles")
	ui_menuitem(hhcycletypes,174,"Using 6-symbol cycles")
	
	hhcyclepatterns=ui_createmenu()
	ui_menutitle(hstats,hhcyclepatterns,"Find n-symbol cycle patterns")
	hhhcyclepatterns2s=ui_createmenu()
	ui_menutitle(hhcyclepatterns,hhhcyclepatterns2s,"In 2-symbol cycles")
	ui_menuitem(hhhcyclepatterns2s,251,"Using 2-gram repeats")
	ui_menuitem(hhhcyclepatterns2s,252,"Using 3-gram repeats")
	ui_menuitem(hhhcyclepatterns2s,253,"Using 4-gram repeats")
	ui_menuitem(hhhcyclepatterns2s,254,"Using 5-gram repeats")
	ui_menuitem(hhhcyclepatterns2s,255,"Using 6-gram repeats")
	ui_menuitem(hhhcyclepatterns2s,256,"Using 7-gram repeats")
	ui_menuitem(hhhcyclepatterns2s,257,"Using 8-gram repeats")
	ui_menuitem(hhhcyclepatterns2s,258,"Using 9-gram repeats")
	ui_menuitem(hhhcyclepatterns2s,259,"Using 10-gram repeats")
	hhhcyclepatterns3s=ui_createmenu()
	ui_menutitle(hhcyclepatterns,hhhcyclepatterns3s,"In 3-symbol cycles")
	ui_menuitem(hhhcyclepatterns3s,260,"Using 2-gram repeats")
	ui_menuitem(hhhcyclepatterns3s,261,"Using 3-gram repeats")
	ui_menuitem(hhhcyclepatterns3s,262,"Using 4-gram repeats")
	ui_menuitem(hhhcyclepatterns3s,263,"Using 5-gram repeats")
	ui_menuitem(hhhcyclepatterns3s,264,"Using 6-gram repeats")
	ui_menuitem(hhhcyclepatterns3s,265,"Using 7-gram repeats")
	ui_menuitem(hhhcyclepatterns3s,266,"Using 8-gram repeats")
	ui_menuitem(hhhcyclepatterns3s,267,"Using 9-gram repeats")
	ui_menuitem(hhhcyclepatterns3s,268,"Using 10-gram repeats")
	hhhcyclepatterns4s=ui_createmenu()
	ui_menutitle(hhcyclepatterns,hhhcyclepatterns4s,"In 4-symbol cycles")
	ui_menuitem(hhhcyclepatterns4s,269,"Using 2-gram repeats")
	ui_menuitem(hhhcyclepatterns4s,270,"Using 3-gram repeats")
	ui_menuitem(hhhcyclepatterns4s,271,"Using 4-gram repeats")
	ui_menuitem(hhhcyclepatterns4s,272,"Using 5-gram repeats")
	ui_menuitem(hhhcyclepatterns4s,273,"Using 6-gram repeats")
	ui_menuitem(hhhcyclepatterns4s,274,"Using 7-gram repeats")
	ui_menuitem(hhhcyclepatterns4s,275,"Using 8-gram repeats")
	ui_menuitem(hhhcyclepatterns4s,276,"Using 9-gram repeats")
	ui_menuitem(hhhcyclepatterns4s,277,"Using 10-gram repeats")
	ui_appendmenu(hstats,MF_SEPARATOR,99,"")
	
	hhencodernd=ui_createmenu()
	ui_menutitle(hstats,hhencodernd,"Find sequential homophonic randomization")
	ui_menuitem(hhencodernd,140,"With symbols using sequential")
	hhhencodernd2s=ui_createmenu()
	ui_menutitle(hhencodernd,hhhencodernd2s,"Using 2-symbol cycles")
	ui_menuitem(hhhencodernd2s,141,"With rows")
	ui_menuitem(hhhencodernd2s,143,"With columns")
	ui_menuitem(hhhencodernd2s,149,"With grid")
	ui_menuitem(hhhencodernd2s,145,"With periodic")
	ui_menuitem(hhhencodernd2s,151,"With slide")
	ui_menuitem(hhhencodernd2s,147,"With custom shape")
	hhhencodernd3s=ui_createmenu()
	ui_menutitle(hhencodernd,hhhencodernd3s,"Using 3-symbol cycles")
	ui_menuitem(hhhencodernd3s,142,"With rows")
	ui_menuitem(hhhencodernd3s,144,"With columns")
	ui_menuitem(hhhencodernd3s,150,"With grid")
	ui_menuitem(hhhencodernd3s,146,"With periodic")
	ui_menuitem(hhhencodernd3s,152,"With slide")
	ui_menuitem(hhhencodernd3s,148,"With custom shape")
	
	ui_menuitem(hstats,107,"Find post sequential homophonic row or columnar rearrangement")
	
	'options
	'------------------------------------------------------------
	hoptions=ui_createmenu()
	ui_menutitle(hmenu,hoptions,"Options")
	ui_menuitem(hoptions,12,"Solvers")
	ui_menuitem(hoptions,42,"Statistics")
	ui_appendmenu(hoptions,MF_SEPARATOR,99,"")
	ui_menuitem(hoptions,13,"Benchmark")
	ui_menuitem(hoptions,48,"Solver n-gram statistics")
	'ui_appendmenu(hoptions,MF_SEPARATOR,99,"")
	'ui_menuitem(hoptions,37,"Demote solution for next run")
	
	ui_setmenu(window_main,hmenu)
	button_main_openfile=ui_button_new(20,20,100,30,"Open file",,window_main)
	button_main_statesave=ui_button_new(20,50,100,30,"Save state",,window_main)
	button_main_stateload=ui_button_new(20,80,100,30,"Load state",,window_main)
	'button_main_openoutputdir=ui_button_new(20,110,100,30,"Output folder",,window_main)
	
	button_main_process=ui_button_new(140,20,100,30,"Solve",,window_main)
	button_main_pauseresume=ui_button_new(140,50,100,30,"Pause",,window_main)
	button_main_stoptask=ui_button_new(140,80,100,30,"Stop task",,window_main)
	button_main_swapinout=ui_button_new(140,110,100,30,"Swap",,window_main)
	
	list_main=ui_listbox_new(260,20,313,140,,window_main)
	
	ui_listbox_addstring(list_main,"Substitution")
	ui_listbox_addstring(list_main,"Substitution + columnar rearrangement")
	ui_listbox_addstring(list_main,"Substitution + columnar transposition")
	ui_listbox_addstring(list_main,"Substitution + crib grid")
	ui_listbox_addstring(list_main,"Substitution + crib list")
	ui_listbox_addstring(list_main,"Substitution + mono groups + crib list")
	ui_listbox_addstring(list_main,"Substitution + monoalphabetic groups")
	ui_listbox_addstring(list_main,"Substitution + nulls and skips")
	ui_listbox_addstring(list_main,"Substitution + polyphones")
	'ui_listbox_addstring(list_main,"Substitution + rectangles")
	ui_listbox_addstring(list_main,"Substitution + row bound")
	ui_listbox_addstring(list_main,"Substitution + row bound fragments")
	ui_listbox_addstring(list_main,"Substitution + sequential homophones")
	ui_listbox_addstring(list_main,"Substitution + simple transposition")
	ui_listbox_addstring(list_main,"Substitution + sparse polyalphabetism")
	ui_listbox_addstring(list_main,"Substitution + units")
	ui_listbox_addstring(list_main,"Substitution + vigen�re")
	ui_listbox_addstring(list_main,"Substitution + vigen�re word list")
	'ui_listbox_addstring(list_main,"Substitution + word cribs")
	ui_listbox_addstring(list_main,"Bigram substitution")
	ui_listbox_addstring(list_main,"Higher-order homophonic")
	ui_listbox_addstring(list_main,"Merge sequential homophones")
	
	'ui_listbox_setcursel(list_main,0)
	set_solverhighlight("substitution")
	solver_text=ui_editor_new(600,20,570,120,solver_status,,window_main)
	ui_control_setfont(solver_text,"Consolas")
	label=ui_label_new(20,145,110,20,"Input window",,window_main)
	input_text=ui_editor_new(20,170,570,519,"",WS_TABSTOP,window_main)
	ui_control_setfont(input_text,"Consolas")
	label=ui_label_new(600,145,110,20,"Output window",,window_main)
	output_text=ui_editor_new(600,170,570,519,"",WS_TABSTOP,window_main)
	ui_control_setfont(output_text,"Consolas")
	
	#ifdef __fb_linux__
	#else
		dim hicon as hicon=loadimage(getmodulehandle(null),"fb_program_icon",image_icon,24,24,lr_shared) '24 by 24 for Windows Taskbar
		sendmessage(window_main,wm_seticon,cast(wparam,icon_big),cast(lparam,hicon))
	#endif

end sub

sub create_window_dimension
	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)
	
	ui_destroywindow(window_dimension)
	window_dimension=ui_window_new(x0,y0,390,750,"Dimension",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	button_dimension_update=ui_button_new(20,20,340,30,"Refresh dimensions list",,window_dimension)
	list_dimension=ui_listbox_new(20,70,340,550,,window_dimension)
	editbox_dimension_custom=ui_editbox_new(20,630,340,25,"",,window_dimension)
	button_dimension_custom=ui_button_new(20,670,340,30,"Set custom dimensions",,window_dimension)
	ui_seticon(window_dimension)

end sub

sub create_window_transposition
	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)
	
	ui_destroywindow(window_transposition)
	window_transposition=ui_window_new(x0,y0,390,750,"Transposition",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	list_transposition=ui_listbox_new(20,20,340,320,,window_transposition)
	ui_listbox_addstring(list_transposition,"Use transposition matrix")
	ui_listbox_addstring(list_transposition,"Mirror")
	ui_listbox_addstring(list_transposition,"Flip")
	ui_listbox_addstring(list_transposition,"Reverse")
	ui_listbox_addstring(list_transposition,"Columnar 1")
	ui_listbox_addstring(list_transposition,"Columnar 2")
	ui_listbox_addstring(list_transposition,"Columnar 3")
	ui_listbox_addstring(list_transposition,"Columnar 4")
	ui_listbox_addstring(list_transposition,"Diagonal 1")
	ui_listbox_addstring(list_transposition,"Diagonal 2")
	ui_listbox_addstring(list_transposition,"Diagonal 3")
	ui_listbox_addstring(list_transposition,"Diagonal 4")
	ui_listbox_addstring(list_transposition,"Diagonal 5")
	ui_listbox_addstring(list_transposition,"Diagonal 6")
	ui_listbox_addstring(list_transposition,"Diagonal 7")
	ui_listbox_addstring(list_transposition,"Diagonal 8")
	ui_listbox_addstring(list_transposition,"Period")
	ui_listbox_addstring(list_transposition,"Period XY")
	ui_listbox_addstring(list_transposition,"Period row order")
	ui_listbox_addstring(list_transposition,"Period column order")
	ui_listbox_addstring(list_transposition,"Skytale")
	ui_listbox_addstring(list_transposition,"Offset")
	ui_listbox_addstring(list_transposition,"Offset row order")
	ui_listbox_addstring(list_transposition,"Offset column order")
	ui_listbox_addstring(list_transposition,"Offset row")
	ui_listbox_addstring(list_transposition,"Offset column")
	ui_listbox_addstring(list_transposition,"Offset rectangular chain")
	ui_listbox_addstring(list_transposition,"Railfence")
	ui_listbox_addstring(list_transposition,"Rearrange rows")
	ui_listbox_addstring(list_transposition,"Rearrange columns")
	ui_listbox_addstring(list_transposition,"Reverse row")
	ui_listbox_addstring(list_transposition,"Reverse column")
	ui_listbox_addstring(list_transposition,"Route")
	ui_listbox_addstring(list_transposition,"Swap row")
	ui_listbox_addstring(list_transposition,"Swap column")
	'ui_listbox_setcursel(list_transposition,0)
	label=ui_label_new(20,350,50,25,"A1:",,window_transposition)
	label=ui_label_new(20,390,50,25,"A2:",,window_transposition)
	label=ui_label_new(20,430,50,25,"A3:",,window_transposition)
	label=ui_label_new(20,470,50,25,"A4:",,window_transposition)
	label=ui_label_new(20,510,50,25,"A5:",,window_transposition)
	label=ui_label_new(20,550,50,25,"A6:",,window_transposition)
	label=ui_label_new(20,590,50,25,"A7:",,window_transposition)
	label=ui_label_new(20,630,50,25,"A8:",,window_transposition)
	editbox_transposition_a1=ui_editbox_new(50,350,310,25,"",,window_transposition)
	editbox_transposition_a2=ui_editbox_new(50,390,310,25,"",,window_transposition)
	editbox_transposition_a3=ui_editbox_new(50,430,310,25,"",,window_transposition)
	editbox_transposition_a4=ui_editbox_new(50,470,310,25,"",,window_transposition)
	editbox_transposition_a5=ui_editbox_new(50,510,310,25,"",,window_transposition)
	editbox_transposition_a6=ui_editbox_new(50,550,310,25,"",,window_transposition)
	editbox_transposition_a7=ui_editbox_new(50,590,310,25,"",,window_transposition)
	editbox_transposition_a8=ui_editbox_new(50,630,310,25,"",,window_transposition) 
	button_transposition_transpose=ui_button_new(20,670,160,30,"Transpose",,window_transposition)
	button_transposition_untranspose=ui_button_new(200,670,160,30,"Untranspose",,window_transposition)
	'checkbox_transposition_keepnulls=ui_checkbox_new(20,667,360,20,"Keep nulls as spaces",,window_transposition)
	ui_seticon(window_transposition)
		
end sub

sub create_window_optionssolver
	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)

	ui_destroywindow(window_optionssolver)	
	window_optionssolver=ui_window_new(x0,y0,600,750,"Solvers options",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	list_optionssolver=ui_listbox_new(20,20,550,600,,window_optionssolver)
	
	ui_listbox_addstring(list_optionssolver,"(General) CPU threads: "+str(solvesub_cputhreads))
	ui_listbox_addstring(list_optionssolver,"(General) Thread wait: "+str(twait))
	ui_listbox_addstring(list_optionssolver,"(General) Entropy weight: "+str(solvesub_entweight))
	ui_listbox_addstring(list_optionssolver,"(General) Iterations: "+str(solvesub_iterations))
	ui_listbox_addstring(list_optionssolver,"(General) Iterations factor: "+str(solvesub_iterationsfactor))
	ui_listbox_addstring(list_optionssolver,"(General) Hill climber iterations: "+str(solvesub_hciterations))
	ui_listbox_addstring(list_optionssolver,"(General) Hill climber iterations factor: "+str(solvesub_hciterationsfactor))
	ui_listbox_addstring(list_optionssolver,"(General) N-gram factor: "+format(solvesub_ngramfactor,"0.00000"))
	ui_listbox_addstring(list_optionssolver,"(General) W-gram factor: "+format(solvesub_wgramfactor,"0.00000"))
	ui_listbox_addstring(list_optionssolver,"(General) Multiplicity weight: "+str(solvesub_multiplicityweight))
	ui_listbox_addstring(list_optionssolver,"(General) Output to file: "+yesno(solvesub_outputdir))
	ui_listbox_addstring(list_optionssolver,"(General) Output to batch: "+yesno(solvesub_outputbatch))
	ui_listbox_addstring(list_optionssolver,"(General) Output scores over: "+str(solvesub_scoreover))
	ui_listbox_addstring(list_optionssolver,"(General) Output improvements only: "+yesno(solvesub_outputimp))
	ui_listbox_addstring(list_optionssolver,"(General) Output additional stats: "+yesno(solvesub_advstats))
	ui_listbox_addstring(list_optionssolver,"(General) Overwrite existing solver output: "+yesno(solvesub_overwriteoutput))
	ui_listbox_addstring(list_optionssolver,"(General) Add PC-cycles to file output format: "+yesno(solvesub_pccyclesformat))
	ui_listbox_addstring(list_optionssolver,"(General) Restarts: "+str(solvesub_restarts)) 
	ui_listbox_addstring(list_optionssolver,"(General) Temperature: "+str(solvesub_temperature))
	ui_listbox_addstring(list_optionssolver,"(General) Enable memory checks: "+yesno(memcheck))
	ui_listbox_addstring(list_optionssolver,"(General) Enable screen size checks: "+yesno(screensizecheck))
	ui_listbox_addstring(list_optionssolver,"(General) 8-gram memory limit: "+str(solvesub_bhmaxgb)+" GB RAM")
	ui_listbox_addstring(list_optionssolver,"(General) 8-gram caching: "+yesno(solvesub_ngramcaching))
	ui_listbox_addstring(list_optionssolver,"(General) Add spaces to output: "+yesno(solvesub_addspaces))
	ui_listbox_addstring(list_optionssolver,"(General) Add spaces to output iterations: "+str(solvesub_addspacesquality))
	ui_listbox_addstring(list_optionssolver,"(General) N-gram log value cut-off: "+str(solvesub_ngramlogcutoff))
	
	ui_listbox_addstring(list_optionssolver,"(Batch n-grams) Iterations: "+str(solvesub_batchngramsrestarts))
	ui_listbox_addstring(list_optionssolver,"(Batch ciphers) Only process ciphers with bigram repeats over: "+str(solvesub_batchciphersbigrams))
	
	ui_listbox_addstring(list_optionssolver,"(Batch ciphers & n-grams) Shutdown computer after task completion: "+yesno(solvesub_batchshutdown))
	
	ui_listbox_addstring(list_optionssolver,"(Batch ciphers & settings) Accuracy short circuit: "+yesno(solvesub_accshortcircuit))
	
'	if bigramsolver=1 then 'remove ??? ' beijinghouse - no readd
		ui_listbox_addstring(list_optionssolver,"(Bigram substitution) Output heatmap: "+yesno(solvesub_bigramheatmap))
		ui_listbox_addstring(list_optionssolver,"(Bigram substitution) Reuse best solution ratio: "+str(solvesub_bigrambestsol))
		ui_listbox_addstring(list_optionssolver,"(Bigram substitution) Auto-crib restarts: "+str(solvesub_bigramautocrib))
		ui_listbox_addstring(list_optionssolver,"(Bigram substitution) Bigram homophone weight divider: "+str(solvesub_bigramhomwdiv))
'	end if
	
	ui_listbox_addstring(list_optionssolver,"(Higher-order homophonic) N-order: "+str(solvesub_higherorderhomophonic))
	ui_listbox_addstring(list_optionssolver,"(Higher-order homophonic) Separation weight divider: "+str(solvesub_higherorderhomophonicweight))
	
	'ui_listbox_addstring(list_optionssolver,"(Substitution + columnar transposition & rearrangement) Columns: "+str(solvesub_ctcolumns))
	ui_listbox_addstring(list_optionssolver,"(Substitution + columnar transposition & rearrangement) Search depth: "+str(solvesub_ctdepth))
	ui_listbox_addstring(list_optionssolver,"(Substitution + columnar transposition & rearrangement) Use bigrams: "+yesno(solvesub_ctmode))
	ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) Period: "+str(solvesub_pnperiod))
	ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) Nulls and skips: "+str(solvesub_pnnulls))
	ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) Manual nulls: "+str(solvesub_pnmannulls))
	ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) Manual skips: "+str(solvesub_pnmanskips))
	
	if development=1 then
		ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) *** Temp ***: "+str(solvesub_nshctemp))
		ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) *** Shift % ***: "+str(solvesub_nshcshift))
		ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) *** Shift div ***: "+str(solvesub_nshcshiftdiv))
		ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) *** Restarts ***: "+str(solvesub_nshcrestartsmax))
		ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) *** Over ***: "+str(solvesub_pnover))
		ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) *** Over skip ***: "+yesno(solvesub_pnoverskip))
		'ui_listbox_addstring(list_optionssolver,"(Substitution + nulls and skips) Search depth: "+str(solvesub_pndepth))
	end if
	
	'ui_listbox_addstring(list_optionssolver,"(Substitution + polyphones [auto]) Extra letters: "+str(solvesub_polyphones))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + polyphones [auto]) Increment extra letters: "+yesno(solvesub_incpolyphones))
	
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Temperature: "+str(solvesub_rowboundtemp))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Sub restarts: "+str(solvesub_rowboundsubrestarts))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Over: "+str(solvesub_rowboundover))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Fine tune: "+str(solvesub_rowboundfine))
	ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Fragments: "+str(solvesub_rowboundfragments))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Crib fragments: "+str(solvesub_rowboundcribfragments))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Crib history table size: "+str(solvesub_rowboundhkeys))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Crib history table mode: "+str(solvesub_rowbounddistinctmode))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Crib distinctiveness: "+str(solvesub_rowbounddistinct))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Crib HC iteration factor: "+str(solvesub_rowboundhcmode1itfact))
	'ui_listbox_addstring(list_optionssolver,"(Substitution + row bound fragments) Crib output history table: "+yesno(solvesub_rowboundcheckhistory))
	
	ui_listbox_addstring(list_optionssolver,"(Substitution + sequential homophones) Sequential weight: "+str(solvesub_seqweight))
	
	ui_listbox_addstring(list_optionssolver,"(Substitution + sparse polyalphabetism) Polyalphabetism weight: "+str(solvesub_matchweight))
	
	ui_listbox_addstring(list_optionssolver,"(Substitution + simple transposition) Use sequential homophones: "+yesno(solvesub_tpseqhom))
	ui_listbox_addstring(list_optionssolver,"(Substitution + simple transposition) PC-cycles, use untransposed texts: "+yesno(solvesub_pcmode))
	
	ui_listbox_addstring(list_optionssolver,"(Substitution + vigen�re) Keyword length: "+str(solvesub_vigenerekeylength))
	ui_listbox_addstring(list_optionssolver,"(Substitution + vigen�re) By columns: "+yesno(solvesub_vigenerebycolumns))
	ui_listbox_addstring(list_optionssolver,"(Substitution + vigen�re) Use subtraction: "+yesno(solvesub_vigeneresubtract))
	
	ui_listbox_addstring(list_optionssolver,"(Merge sequential homophones) Cycle length weight: "+str(solvesub_cyclelengthweight))
	ui_listbox_addstring(list_optionssolver,"(Merge sequential homophones) Target alphabet size: "+str(solvesub_cyclealphabetsize))
	ui_listbox_addstring(list_optionssolver,"(Merge sequential homophones) Use random arguments: "+yesno(solvesub_rndcyclearg))
	
	ui_listbox_setcursel(list_optionssolver,0)
	editbox_optionssolver_a1=ui_editbox_new(20,630,550,25,"",,window_optionssolver)
	
	button_optionssolver_change=ui_button_new(20,670,305,30,"Change value",,window_optionssolver)
	button_optionssolver_normalize=ui_button_new(345,670,225,30,"Normalize n-gram factor",,window_optionssolver)
	
	ui_seticon(window_optionssolver)
	
end sub

sub create_window_optionsstats
	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)
	
	ui_destroywindow(window_optionsstats)
	window_optionsstats=ui_window_new(x0,y0,600,470,"Statistics options",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	list_optionsstats=ui_listbox_new(20,20,550,320,,window_optionsstats)
	ui_listbox_addstring(list_optionsstats,"(Find plaintext and encoding direction) Randomization trials: "+str(stats_dirrndtrials))
	ui_listbox_addstring(list_optionsstats,"(Find encoding randomization) Randomization trials: "+str(stats_encrndtrials))
	ui_listbox_addstring(list_optionsstats,"(Symbol cycle patterns & types) Randomization trials: "+str(stats_symbolcyclepatternsrndtrials))
	ui_listbox_addstring(list_optionsstats,"(N-symbol cycles) Weight: "+str(stats_nsymbolcyclesweight))
	'ui_listbox_addstring(list_optionsstats,"(Plaintext direction) Bigrams alphabet: "+str(stats_bigramsmod))
	ui_listbox_setcursel(list_optionsstats,0)
	editbox_optionsstats_a1=ui_editbox_new(20,350,550,25,"",,window_optionsstats)
	button_optionsstats_change=ui_button_new(20,390,550,30,"Change value",,window_optionsstats)
	ui_seticon(window_optionsstats)
	
end sub

sub create_window_polyphones
	
	pp_windowup=1	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)
	
	ui_destroywindow(window_polyphones)
	window_polyphones=ui_window_new(x0,y0,750,750,"Polyphones",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	
	label=ui_label_new(20,20,250,25,"Solver mode:",,window_polyphones)
	
	radiobutton_polyphones_user=ui_radiobutton_new(20,60,300,25,"User defined polyphones",WS_GROUP,window_polyphones)
	button_polyphones_setall=ui_button_new(40,100,250,30,"Set letters# for all symbols to:",,window_polyphones)
	editbox_polyphones_setall=ui_editbox_new(310,100+3,80,25,"2",,window_polyphones)
	button_polyphones_setsel=ui_button_new(40,140,250,30,"Set letters# for selected symbol to:",,window_polyphones)
	editbox_polyphones_setsel=ui_editbox_new(310,140+3,80,25,"2",,window_polyphones)
	
	radiobutton_polyphones_auto=ui_radiobutton_new(20,200,350,25,"User defined polyphones + extra letters",,window_polyphones)
	label=ui_label_new(40,240,100,25,"- Extra letters:",,window_polyphones)
	editbox_polyphones_extraletters=ui_editbox_new(310,240-3,80,25,"5",,window_polyphones)
	checkbox_polyphones_increment=ui_checkbox_new(40,280-6,330,25,"Increment extra letters automatically",,window_polyphones)
	
	radiobutton_polyphones_hafer1=ui_radiobutton_new(20,340,300,25,"Hafer shifts: -N/+N",,window_polyphones)
	
	radiobutton_polyphones_hafer2=ui_radiobutton_new(20,400,300,25,"Hafer shifts: any 2 shifts",,window_polyphones)
	'label=ui_label_new(40,440,100,25,"- Shifts:",,window_polyphones)
	'editbox_polyphones_shifts=ui_editbox_new(310,440-3,80,25,"2",,window_polyphones)
	
	'label=ui_label_new(20,560,150,25,"Other settings:",,window_polyphones)
	'label=ui_label_new(20,600,200,25,"Distribution/symbol bias:",,window_polyphones)
	'editbox_polyphones_sdbias=ui_editbox_new(310,600-3,80,25,str(solvesub_sdbias),,window_polyphones)
	label=ui_label_new(20,640,150,25,"Multiplicity weight:",,window_polyphones)
	editbox_polyphones_mulweight=ui_editbox_new(310,640-3,80,25,"2",,window_polyphones)
	checkbox_polyphones_override=ui_checkbox_new(20,680-6,350,25,"Override hill-climber settings",,window_polyphones)
	
	label=ui_label_new(420,20,300,25,"User defined polyphones:",,window_polyphones)
	button_polyphones_reload=ui_button_new(420,50,300,30,"Refresh list",,window_polyphones)	
	list_polyphones_stl=ui_listbox_new(420,100,300,610,,window_polyphones)
	
	ui_checkbox_setcheck(checkbox_polyphones_increment,1)
	ui_checkbox_setcheck(checkbox_polyphones_override,1)
	
	ui_radiobutton_setcheck(radiobutton_polyphones_user,1)
	ui_seticon(window_polyphones)
	
end sub

sub create_window_units
	
	un_windowup=1
	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)
	
	ui_destroywindow(window_units)
	window_units=ui_window_new(x0,y0,500,750,"Units",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	
	label=ui_label_new(20,20,100,25,"Unit:",,window_units)
	label=ui_label_new(260,20,100,25,"Mode:",,window_units)
	
	radiobutton_units_symbol=ui_radiobutton_new(20,60,200,25,"Symbol",WS_GROUP,window_units)
	radiobutton_units_row=ui_radiobutton_new(20,85,200,25,"Row",,window_units)
	radiobutton_units_column=ui_radiobutton_new(20,110,200,25,"Column",,window_units)
	radiobutton_units_hseq=ui_radiobutton_new(20,135,200,25,"Horizontal sequence",,window_units)
	
	radiobutton_units_remove=ui_radiobutton_new(260,60,200,25,"Remove",WS_GROUP,window_units)
	radiobutton_units_expand=ui_radiobutton_new(260,85,200,25,"Expand",,window_units)
	radiobutton_units_separate=ui_radiobutton_new(260,110,200,25,"Separate into 2nd key",,window_units)
	radiobutton_units_replace=ui_radiobutton_new(260,135,200,25,"Replace with new symbols",,window_units)
	radiobutton_units_reverse=ui_radiobutton_new(260,160,200,25,"Reverse",,window_units)
	
	label=ui_label_new(20,420,300,25,"Multiplicity weight:",,window_units)
	editbox_units_mulweight=ui_editbox_new(360,420-3,100,25,"0",,window_units)
	
	label=ui_label_new(20,450,300,25,"Key length start:",,window_units)
	editbox_units_klstart=ui_editbox_new(360,450-3,100,25,"1",,window_units)
	
	label=ui_label_new(20,480,300,25,"Key length stop:",,window_units)
	editbox_units_klend=ui_editbox_new(360,480-3,100,25,"0",,window_units)
	
	label=ui_label_new(20,510,300,25,"Horizontal sequence length:",,window_units)
	editbox_units_hseqsize=ui_editbox_new(360,510-3,100,25,"3",,window_units)
	
	label=ui_label_new(20,540,300,25,"Replace, # of symbols per instance:",,window_units)
	editbox_units_replacesymbols=ui_editbox_new(360,540-3,100,25,"1",,window_units)
	
	label=ui_label_new(20,570,300,25,"Replace, # of extra characters to the left:",,window_units)
	editbox_units_replaceleft=ui_editbox_new(360,570-3,100,25,"0",,window_units)
	
	label=ui_label_new(20,600,300,25,"Replace, # of extra characters to the right:",,window_units)
	editbox_units_replaceright=ui_editbox_new(360,600-3,100,25,"0",,window_units)
	
	label=ui_label_new(20,630,60,25,"Period:",,window_units)
	editbox_units_period=ui_editbox_new(360,630-3,100,25,"1",,window_units)
	
	radiobutton_units_period_tp=ui_radiobutton_new(90,630-4,120,25,"Transposed",WS_GROUP,window_units)
	radiobutton_units_period_utp=ui_radiobutton_new(210,630-4,120,25,"Untransposed",,window_units)
	
	button_units_start=ui_button_new(20,670,455,30,"Start solver",,window_units)
	
	ui_radiobutton_setcheck(radiobutton_units_symbol,1)
	ui_radiobutton_setcheck(radiobutton_units_remove,1)
	ui_radiobutton_setcheck(radiobutton_units_period_utp,1)
	
	ui_seticon(window_units)
	
end sub

sub create_window_cribgrid(byval x0 as long,byval y0 as long,byval fresh as byte)
	
	if info_x>100 or info_y>100 then 'crash fix
		ui_editbox_settext(output_text,"Error: input dimensions cannot exceed 100 by 100")
		exit sub	
	end if
	
	dim as integer i,j,k,x,y,e
	dim as string wc_symbol1
	dim as string wc_symbol2
	dim as short wx=40
	dim as short wy2=20
	dim as short wy=1+wy2*2
	dim as short wymin=3
	dim as short wyo=32
	dim as short wxo=-20
	wc_prevcheck_editcipher=0
	
	if fresh=1 then
		wc_l=info_length
		wc_s=info_symbols
		wc_dx=info_x
		wc_dy=info_y
		wc_num=info_numerical
	end if
	
	erase info2
	for i=1 to wc_l
		info2(i)=info(i)
	next i
	
	if wc_num=0 then 
		wx=30
		wxo=-10
	else 
		wx=40
		wxo=-20
	end if
	
	dim as long cip(wc_l)
	dim as long nba(wc_l)
	
	if wc_prevcheck_showcipher=0 then
		wy=wy2
		wymin=1
	end if
	
	if fresh=1 then
		
		'if wc_dx>100 or wc_dy>100 then
		'	ui_editbox_settext(output_text,"Error: input dimensions cannot exceed 100 by 100")
		'	exit sub	
		'end if
		
		if solvesub_cribgridinstance=1 then
			dim as double arg(10)
			arg(1)=wc_l
			arg(2)=wc_s
			for i=1 to wc_l
				cstate(21,i)=nuba(i)
			next i
			cstate_operation(21,22,"Raise unique bigrams",arg())
			wc_s=cstate_symbolcount(22,wc_l,wc_l)
		end if
	
		for i=1 to wc_l
			
			select case solvesub_cribgridinstance
				case 1
					nba(i)=cstate(22,i)
					cip(i)=info(i)
				case else
					nba(i)=nuba(i)
					cip(i)=info(i)
			end select
			
			if wc_num=1 andalso cip(i)>9999 then
				ui_editbox_settext(output_text,"Error: numbers cannot exceed 9999")
				exit sub
			end if
		next i
		redim wc_pgrid(1,wc_dx,wc_dy)
		redim wc_nuba(wc_dx,wc_dy)
		
	end if
	
	dim as string allowedalpha
	for i=0 to ngram_alphabet_size-1
		allowedalpha+=chr(alphabet(i))
	next i
	
	dim as long min_window_width=685 '+200
	dim as long min_window_length
	
	if 45+wc_dx*(wx-1)>min_window_width then min_window_width=45+wc_dx*(wx-1)
	if 120+wc_dy*(wy-wymin)>min_window_length then min_window_length=120+wc_dy*(wy-wymin)
	
	'dim as long desktopx=getsystemmetrics(SM_CXSCREEN) 'ui specific ???
	'dim as long desktopy=getsystemmetrics(SM_CYSCREEN) 'ui specific ???
	
	#ifndef __FB_LINUX__
		dim as long desktopx=getsystemmetrics(SM_CXSCREEN)
		dim as long desktopy=getsystemmetrics(SM_CYSCREEN)
	#else
		dim as long desktopx=0
		dim as long desktopy=0
	#endif
	
	if min_window_width>(desktopx-50) then e=1
	if min_window_length>(desktopy-50) then e=1
	
	if screensizecheck=0 then e=0
	
	if e=0 then
		ui_destroywindow(window_cribs)
		window_cribs=ui_window_new(x0,y0,min_window_width,min_window_length,"Crib grid",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	else
		wc_windowup=0
		ui_destroywindow(window_cribs)
		ui_editbox_settext(output_text,"Error: cipher dimensions exceed screen size, try another "+lb+"set of dimensions")
		exit sub
	end if
	label=ui_label_new(20,30+wyo+(wc_dy*(wy-wymin)),min_window_width,50,"Allowed crib alphabet (case sensitive): "+str(allowedalpha),,window_cribs)
	
	button_cribs_solve=ui_button_new(260,10,120,30,"Solve with cribs",,window_cribs)
	button_cribs_clear=ui_button_new(400,10,120,30,"Clear cribs",,window_cribs)
	button_cribs_reload=ui_button_new(540,10,120,30,"Reload cipher",,window_cribs)
	
	checkbox_cribs_editcipher=ui_checkbox_new(140,12,100,25,"Edit cipher",,window_cribs)
	checkbox_cribs_showcipher=ui_checkbox_new(20,12,100,25,"Show cipher",,window_cribs)
	ui_checkbox_setcheck(checkbox_cribs_showcipher,wc_prevcheck_showcipher)
	
	if ui_checkbox_getcheck(checkbox_cribs_showcipher)=0 then
		ui_checkbox_setcheck(checkbox_cribs_editcipher,0)
		wc_prevcheck_editcipher=0
	end if
	
	ui_seticon(window_cribs)
	
	j=0
	for y=1 to wc_dy
		for x=1 to wc_dx
			j+=1
			if fresh=0 then
				wc_symbol2=wc_pgrid(0,x,y)
			else
				wc_nuba(x,y)=nba(j)
				if wc_num=0 then
					wc_pgrid(1,x,y)=chr(cip(j))
				else
					wc_pgrid(1,x,y)=str(cip(j))
				end if
				wc_symbol2=""
			end if
			wc_symbol1=wc_pgrid(1,x,y)
			if wc_prevcheck_showcipher=0 then
				wc_cribs(x,y)=ui_editbox_new(wxo+x*(wx-1),wyo+y*(wy-wymin),wx,wy,wc_symbol2,ES_CENTER or WS_TABSTOP,window_cribs)
			else
				wc_cribs(x,y)=ui_editbox_new(wxo+x*(wx-1),1+-wy2+wyo+y*(wy-wymin),wx,wy2,wc_symbol2,ES_CENTER or WS_TABSTOP,window_cribs)
				if wc_num=0 then
					wc_cipher(x,y)=ui_editbox_new(wxo+x*(wx-1),wyo+y*(wy-wymin),wx,wy2,wc_symbol1,ES_CENTER or WS_DISABLED,window_cribs)
				else
					wc_cipher(x,y)=ui_editbox_new(wxo+x*(wx-1),wyo+y*(wy-wymin),wx,wy2,wc_symbol1,ES_CENTER or WS_DISABLED,window_cribs)
				end if
			end if
			if j=wc_l then exit for,for
		next x
	next y
	
	wc_windowup=1

end sub

sub create_window_combine
	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)
	
	ui_destroywindow(window_combine)
	window_combine=ui_window_new(x0,y0,780,880,"Combine",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)	
	label=ui_label_new(20,20,150,25,"Operations",,window_combine)
	list_combine_operations=ui_listbox_new(20,45,340,320,,window_combine)
	ui_listbox_addstring(list_combine_operations,"Noop")
	ui_listbox_addstring(list_combine_operations,"Dimension")
	ui_listbox_addstring(list_combine_operations,"Directions")
	ui_listbox_addstring(list_combine_operations,"Period")
	ui_listbox_addstring(list_combine_operations,"Period row order")
	ui_listbox_addstring(list_combine_operations,"Period column order")
	ui_listbox_addstring(list_combine_operations,"Skytale")
	ui_listbox_addstring(list_combine_operations,"Offset")
	ui_listbox_addstring(list_combine_operations,"Offset row order")
	ui_listbox_addstring(list_combine_operations,"Offset column order")
	ui_listbox_addstring(list_combine_operations,"Expand symbol")
	ui_listbox_addstring(list_combine_operations,"Add character")
	ui_listbox_addstring(list_combine_operations,"Add characters")
	ui_listbox_addstring(list_combine_operations,"Add nulls and skips")
	ui_listbox_addstring(list_combine_operations,"Add random character")
	ui_listbox_addstring(list_combine_operations,"Add row")
	ui_listbox_addstring(list_combine_operations,"Add row (using random symbols)")
	ui_listbox_addstring(list_combine_operations,"Add column")
	ui_listbox_addstring(list_combine_operations,"Add column (using random symbols)")
	ui_listbox_addstring(list_combine_operations,"Remove character")
	ui_listbox_addstring(list_combine_operations,"Remove characters")
	ui_listbox_addstring(list_combine_operations,"Remove column")
	ui_listbox_addstring(list_combine_operations,"Remove row")
	ui_listbox_addstring(list_combine_operations,"Random nulls")
	ui_listbox_addstring(list_combine_operations,"Random skips")
	ui_listbox_addstring(list_combine_operations,"Randomize")
	ui_listbox_addstring(list_combine_operations,"Randomize and bigrams")
	ui_listbox_addstring(list_combine_operations,"Randomize row")
	ui_listbox_addstring(list_combine_operations,"Randomize row order")
	ui_listbox_addstring(list_combine_operations,"Randomize column")
	ui_listbox_addstring(list_combine_operations,"Randomize column order")
	ui_listbox_addstring(list_combine_operations,"Plaintext")
	ui_listbox_addstring(list_combine_operations,"Encode: caesar shift")
	ui_listbox_addstring(list_combine_operations,"Encode: homophonic substitution")
	ui_listbox_addstring(list_combine_operations,"Encode: homophonic substitution 2")
	ui_listbox_addstring(list_combine_operations,"Encode: homophonic substitution 1-170")
	ui_listbox_addstring(list_combine_operations,"Encode: homophonic substitution 171-340")
	label=ui_label_new(20,365,50,25,"A1:",,window_combine)
	label=ui_label_new(20,405,50,25,"A2:",,window_combine)
	label=ui_label_new(20,445,50,25,"A3:",,window_combine)
	label=ui_label_new(20,485,50,25,"A4:",,window_combine)
	label=ui_label_new(20,525,50,25,"A5:",,window_combine)
	label=ui_label_new(20,565,50,25,"A6:",,window_combine)
	label=ui_label_new(20,605,50,25,"A7:",,window_combine)
	label=ui_label_new(20,645,50,25,"A8:",,window_combine)
	editbox_combine_a1=ui_editbox_new(50,365,310,25,"",,window_combine)
	editbox_combine_a2=ui_editbox_new(50,405,310,25,"",,window_combine)
	editbox_combine_a3=ui_editbox_new(50,445,310,25,"",,window_combine)
	editbox_combine_a4=ui_editbox_new(50,485,310,25,"",,window_combine)
	editbox_combine_a5=ui_editbox_new(50,525,310,25,"",,window_combine)
	editbox_combine_a6=ui_editbox_new(50,565,310,25,"",,window_combine)
	editbox_combine_a7=ui_editbox_new(50,605,310,25,"",,window_combine)
	editbox_combine_a8=ui_editbox_new(50,645,310,25,"",,window_combine)
	
	radiobutton_combine_transposed=ui_radiobutton_new(20,705,150,25,"Transposed",WS_GROUP,window_combine)
	radiobutton_combine_untransposed=ui_radiobutton_new(200,705,150,25,"Untransposed",,window_combine)
	radiobutton_combine_additive=ui_radiobutton_new(20,750,150,25,"Additive",WS_GROUP,window_combine)
	radiobutton_combine_multiplicative=ui_radiobutton_new(200,750,150,25,"Multiplicative",,window_combine)
	button_combine_add=ui_button_new(20,790,340,30,"Add operation to stack",,window_combine)
	
	label=ui_label_new(410,20,150,25,"Stack",,window_combine)
	list_combine_stack=ui_listbox_new(410,45,340,320,,window_combine)
	button_combine_remove=ui_button_new(410,365,340,30,"Remove operation from stack",,window_combine)
	list_combine_measurements=ui_listbox_new(410,415,340,100,,window_combine)
	
	'0123456789
	ui_listbox_addstring(list_combine_measurements,"2-symbol cycles")
	ui_listbox_addstring(list_combine_measurements,"3-symbol cycles")
	ui_listbox_addstring(list_combine_measurements,"5-gram fragments")
	
	'a
	ui_listbox_addstring(list_combine_measurements,"Adjacency")
	ui_listbox_addstring(list_combine_measurements,"Appearance")
	ui_listbox_addstring(list_combine_measurements,"Asymmetry")
	
	'b
	ui_listbox_addstring(list_combine_measurements,"Bigrams")
	ui_listbox_addstring(list_combine_measurements,"Bigrams alphabet A1")
	
	'c
	ui_listbox_addstring(list_combine_measurements,"Cycle spectrum")
	
	'd
	ui_listbox_addstring(list_combine_measurements,"Deep bigrams")
	ui_listbox_addstring(list_combine_measurements,"Doublets")
	
	'i
	'ui_listbox_addstring(list_combine_measurements,"IOC period A1 by columns")
	
	'k
	ui_listbox_addstring(list_combine_measurements,"Keyword length A1")
	
	'm
	ui_listbox_addstring(list_combine_measurements,"Midpoint shift")
	
	'n
	ui_listbox_addstring(list_combine_measurements,"N-grams")

	'p
	ui_listbox_addstring(list_combine_measurements,"Periods")
	ui_listbox_addstring(list_combine_measurements,"Periods + offset column order")
	ui_listbox_addstring(list_combine_measurements,"Perfect 2-symbol cycles")
	ui_listbox_addstring(list_combine_measurements,"Perfect 3-symbol cycles")
	ui_listbox_addstring(list_combine_measurements,"Prime phobia")
	ui_listbox_addstring(list_combine_measurements,"Pivots")
	
	'r
	ui_listbox_addstring(list_combine_measurements,"Raw IOC")
	
	's
	ui_listbox_addstring(list_combine_measurements,"Sequential A1")
	ui_listbox_addstring(list_combine_measurements,"Sequential length A1")
	ui_listbox_addstring(list_combine_measurements,"Sliding unigram repeats")
	ui_listbox_addstring(list_combine_measurements,"Slope")
	ui_listbox_addstring(list_combine_measurements,"Solve substitution")
	ui_listbox_addstring(list_combine_measurements,"SSS position")
	ui_listbox_addstring(list_combine_measurements,"Symbols")
	ui_listbox_addstring(list_combine_measurements,"Symbols A1-middle-A1")
	
	't
	ui_listbox_addstring(list_combine_measurements,"Trigrams")
	ui_listbox_addstring(list_combine_measurements,"Triplets")
	
	'u
	ui_listbox_addstring(list_combine_measurements,"Unigram A1-middle-A1")
	ui_listbox_addstring(list_combine_measurements,"Unigram distance")
	ui_listbox_addstring(list_combine_measurements,"Unigram distance, log under A1")
	ui_listbox_addstring(list_combine_measurements,"Unigram distance, log over A1")
	ui_listbox_addstring(list_combine_measurements,"Unigram non repeating rows")
	ui_listbox_addstring(list_combine_measurements,"Unigram non repeating columns")
	ui_listbox_addstring(list_combine_measurements,"Unigram repeats")
	ui_listbox_addstring(list_combine_measurements,"Unique unigrams period A1 by columns")
	ui_listbox_addstring(list_combine_measurements,"Unique unigrams period versus A1 by rows")
	
	ui_listbox_setcursel(list_combine_measurements,0)
	checkbox_combine_normalized=ui_checkbox_new(410,515,150,25,"Normalized",,window_combine)
	checkbox_combine_omitlist=ui_checkbox_new(410,545,150,25,"Omit list",,window_combine)
	checkbox_combine_hypergraph=ui_checkbox_new(570,515,200,25,"Output hypergraph",,window_combine)
	checkbox_combine_forcelinear=ui_checkbox_new(570,545,200,25,"Force linear graph",,window_combine)
	label=ui_label_new(410,580,150,25,"A1:",,window_combine)
	editbox_combine_ma1=ui_editbox_new(440,580,310,25,"",,window_combine)
	label=ui_label_new(410,620,150,25,"Get sigma of:",,window_combine)
	editbox_combine_getsigma=ui_editbox_new(510,620,240,25,"",,window_combine)
	
	label=ui_label_new(410,660,190,25,"Length range:",,window_combine)
	editbox_combine_fromlen=ui_editbox_new(520,660,110,25,"",,window_combine)
	editbox_combine_tolen=ui_editbox_new(640,660,110,25,"",,window_combine)
	
	label=ui_label_new(410,700,190,25,"Raw IOC range:",,window_combine)
	editbox_combine_minioc=ui_editbox_new(520,700,110,25,"",,window_combine)
	editbox_combine_maxioc=ui_editbox_new(640,700,110,25,"",,window_combine)
	
	editbox_combine_combinations=ui_editbox_new(410,750,340,25,"Combinations: 0",ES_READONLY,window_combine)
	button_combine_process=ui_button_new(410,790,340,30,"Start task",,window_combine)
	
	ui_radiobutton_setcheck(radiobutton_combine_additive,1)
	ui_radiobutton_setcheck(radiobutton_combine_untransposed,1)
	
	ui_seticon(window_combine)

end sub

sub create_window_transpositionsolver
	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)
	
	ui_destroywindow(window_transpositionsolver)
	window_transpositionsolver=ui_window_new(x0,y0,755,750,"Simple transposition solver",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	
	label=ui_label_new(20,20,340,25,"Available operations",,window_transpositionsolver)
	list_transpositionsolver_operations=ui_listbox_new(20,45,340,460,,window_transpositionsolver)
	label=ui_label_new(385,20,340,25,"Operations to be used by solver",,window_transpositionsolver)
	list_transpositionsolver_stack=ui_listbox_new(385,45,340,460,,window_transpositionsolver)
	
	'A
	'B
	'C
	ui_listbox_addstring(list_transpositionsolver_operations,"Columnar")
	'D
	ui_listbox_addstring(list_transpositionsolver_operations,"Diagonal")
	'E
	'F
	ui_listbox_addstring(list_transpositionsolver_operations,"Flip")
	'G
	'H
	'I
	'J
	'K
	'L
	ui_listbox_addstring(list_transpositionsolver_operations,"L-route")
	'M
	ui_listbox_addstring(list_transpositionsolver_operations,"Mirror")
	'ui_listbox_addstring(list_transpositionsolver_operations,"Move")
	'N
	ui_listbox_addstring(list_transpositionsolver_operations,"None")
	'O
	ui_listbox_addstring(list_transpositionsolver_operations,"Offset")
	ui_listbox_addstring(list_transpositionsolver_operations,"Offset row order")
	ui_listbox_addstring(list_transpositionsolver_operations,"Offset column order")
	'P
	ui_listbox_addstring(list_transpositionsolver_operations,"Period")
	ui_listbox_addstring(list_transpositionsolver_operations,"Period from-to")
	ui_listbox_addstring(list_transpositionsolver_operations,"Period row order")
	ui_listbox_addstring(list_transpositionsolver_operations,"Period column order")
	'Q
	'R
	ui_listbox_addstring(list_transpositionsolver_operations,"Reverse")
	ui_listbox_addstring(list_transpositionsolver_operations,"Railfence")
	'S
	ui_listbox_addstring(list_transpositionsolver_operations,"Skytale")
	ui_listbox_addstring(list_transpositionsolver_operations,"Snake")
	ui_listbox_addstring(list_transpositionsolver_operations,"Spiral")
	ui_listbox_addstring(list_transpositionsolver_operations,"Split")
	'ui_listbox_addstring(list_transpositionsolver_operations,"Swap row")
	'ui_listbox_addstring(list_transpositionsolver_operations,"Swap column")
	'T
	'U
	'V
	'W
	'X
	'Y
	'Z
	
	button_transpostionsolver_add=ui_button_new(20,510,340,30,"Add selected operation",,window_transpositionsolver)
	button_transpostionsolver_addall=ui_button_new(20,550,340,30,"Add all operations",,window_transpositionsolver)
	button_transpostionsolver_remove=ui_button_new(385,510,340,30,"Remove selected operation",,window_transpositionsolver)
	button_transpostionsolver_removeall=ui_button_new(385,550,340,30,"Remove all operations",,window_transpositionsolver)
	
	label=ui_label_new(20,595,150,25,"Batch ciphers restarts:",,window_transpositionsolver)
	editbox_transpositionsolver_batchciphersrestarts=ui_editbox_new(180,592,100,25,str(solvesub_tsbatchciphersrestarts),,window_transpositionsolver)
	
	label=ui_label_new(20,635,150,25,"Operation stack size:",,window_transpositionsolver)
	editbox_transpositionsolver_stacksize=ui_editbox_new(180,632,100,25,str(solvesub_transstack),,window_transpositionsolver)
	
	label=ui_label_new(20,675,150,25,"Bigram search states:",,window_transpositionsolver)
	editbox_transpositionsolver_searchstates=ui_editbox_new(180,672,100,25,str(solvesub_bigramdepth),,window_transpositionsolver)
	
	'(Substitution + simple transposition) Use sequential homophones: 0
	'(Substitution + simple transposition) PC-cycles, use untransposed texts: 1
	
	button_transpostionsolver_batchciphers=ui_button_new(385,590,340,30,"Batch ciphers",,window_transpositionsolver)
	button_transpostionsolver_start=ui_button_new(385,670,340,30,"Start solver",,window_transpositionsolver)

	ui_seticon(window_transpositionsolver)
	
	ts_windowup=1

end sub

sub create_window_manipulation
	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)
	
	ui_destroywindow(window_manipulation)
	window_manipulation=ui_window_new(x0,y0,430,750,"Manipulation",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	list_manipulation_operations=ui_listbox_new(20,20,380,400,,window_manipulation)
	ui_listbox_addstring(list_manipulation_operations,"Add character")
	ui_listbox_addstring(list_manipulation_operations,"Add character periodic")
	ui_listbox_addstring(list_manipulation_operations,"Add column")
	ui_listbox_addstring(list_manipulation_operations,"Add column (using random symbols)")
	ui_listbox_addstring(list_manipulation_operations,"Add row")
	ui_listbox_addstring(list_manipulation_operations,"Add row (using random symbols)")
	ui_listbox_addstring(list_manipulation_operations,"Add null characters")
	ui_listbox_addstring(list_manipulation_operations,"Add null symbol")
	ui_listbox_addstring(list_manipulation_operations,"Add nulls and skips")
	ui_listbox_addstring(list_manipulation_operations,"Assign homophones")
	ui_listbox_addstring(list_manipulation_operations,"Disperse symbol")
	ui_listbox_addstring(list_manipulation_operations,"Encode: caesar shift")
	ui_listbox_addstring(list_manipulation_operations,"Encode: digraph substitution")
	'ui_listbox_addstring(list_manipulation_operations,"Encode: fractioned morse")
	ui_listbox_addstring(list_manipulation_operations,"Encode: homophonic substitution")
	ui_listbox_addstring(list_manipulation_operations,"Encode: 2nd order homophonic substitution")
	'ui_listbox_addstring(list_manipulation_operations,"Encode: homophonic substitution 2")
	'ui_listbox_addstring(list_manipulation_operations,"Encode: homophonic substitution (no repeat window)")
	ui_listbox_addstring(list_manipulation_operations,"Encode: vigen�re")
	ui_listbox_addstring(list_manipulation_operations,"Expand character")
	ui_listbox_addstring(list_manipulation_operations,"Expand periodic")
	ui_listbox_addstring(list_manipulation_operations,"Generate numbers")
	ui_listbox_addstring(list_manipulation_operations,"Generate random numbers")
	ui_listbox_addstring(list_manipulation_operations,"Merge random characters")
	ui_listbox_addstring(list_manipulation_operations,"Merge random symbols")
	ui_listbox_addstring(list_manipulation_operations,"Math")
	ui_listbox_addstring(list_manipulation_operations,"Raise periodic")
	ui_listbox_addstring(list_manipulation_operations,"Raise unique bigrams")
	ui_listbox_addstring(list_manipulation_operations,"Randomize and bigrams")
	ui_listbox_addstring(list_manipulation_operations,"Randomize characters")
	ui_listbox_addstring(list_manipulation_operations,"Randomize positions periodic")
	ui_listbox_addstring(list_manipulation_operations,"Replace periodic with random filler")
	ui_listbox_addstring(list_manipulation_operations,"Remove character")
	ui_listbox_addstring(list_manipulation_operations,"Remove character periodic")
	ui_listbox_addstring(list_manipulation_operations,"Remove column")
	ui_listbox_addstring(list_manipulation_operations,"Remove row")
	label=ui_label_new(20,430,50,25,"A1:",,window_manipulation)
	label=ui_label_new(20,470,50,25,"A2:",,window_manipulation)
	label=ui_label_new(20,510,50,25,"A3:",,window_manipulation)
	label=ui_label_new(20,550,50,25,"A4:",,window_manipulation)
	label=ui_label_new(20,590,50,25,"A5:",,window_manipulation)
	label=ui_label_new(20,630,50,25,"A6:",,window_manipulation)
	editbox_manipulation_a1=ui_editbox_new(50,430,350,25,"",,window_manipulation)
	editbox_manipulation_a2=ui_editbox_new(50,470,350,25,"",,window_manipulation)
	editbox_manipulation_a3=ui_editbox_new(50,510,350,25,"",,window_manipulation)
	editbox_manipulation_a4=ui_editbox_new(50,550,350,25,"",,window_manipulation)
	editbox_manipulation_a5=ui_editbox_new(50,590,350,25,"",,window_manipulation)
	editbox_manipulation_a6=ui_editbox_new(50,630,350,25,"",,window_manipulation)
	button_manipulation_process=ui_button_new(20,670,380,30,"Apply manipulation",,window_manipulation)
	ui_seticon(window_manipulation)

end sub

sub create_window_symbols
	
	dim as long x0,y0,x1,y1
	ui_window_getposition(window_main,x0,y0,x1,y1)

	ui_destroywindow(window_symbols)
	window_symbols=ui_window_new(x0,y0,500,750,"Symbols",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	button_symbols_update=ui_button_new(20,20,450,30,"Refresh symbols list",,window_symbols)
	list_symbols_ngrams=ui_listbox_new(20,70,450,390,,window_symbols)
	list_symbols_operations=ui_listbox_new(20,470,450,150,,window_symbols)
	ui_listbox_addstring(list_symbols_operations,"Expand selected symbol")
	ui_listbox_addstring(list_symbols_operations,"Remove selected symbol")
	ui_listbox_addstring(list_symbols_operations,"Replace selected symbol with")
	ui_listbox_addstring(list_symbols_operations,"Set symbol n-gram size")
	ui_listbox_addstring(list_symbols_operations,"Set plaintext letters for selected symbol")
	ui_listbox_addstring(list_symbols_operations,"Set plaintext letters for all symbols")
	label=ui_label_new(20,630,50,25,"A1:",,window_symbols)
	editbox_symbols_a1=ui_editbox_new(50,630,420,25,"",,window_symbols)
	button_symbols_process=ui_button_new(20,670,450,30,"Apply manipulation",,window_symbols)
	ui_seticon(window_symbols)

end sub

sub create_window_creatematrix(byval x0 as long,byval y0 as long)
	
	dim as long x1,y1
	if x0=0 andalso y0=0 then ui_window_getposition(window_main,x0,y0,x1,y1)
	
	dim as integer i,j,k,x,y,xx,yy,l
	dim as integer tma_scx=715
	dim as integer tma_scy=750
	dim as integer tma_xu=40
	dim as integer tma_yu=30
	dim as integer ok=1
	dim as byte oversized
	
	string_to_info(ui_editbox_gettext(input_text))
	
	tma_dx=info_x
	tma_dy=info_y
	l=info_length
	tma_lpx=0
	tma_lpy=0
	tma_c2=0
	tma_ok=1
	
	if (20+25+tma_dx*tma_xu)>tma_scx then tma_scx=(20+25+tma_dx*tma_xu)
	if (70+50+tma_dy*tma_yu)>tma_scy then tma_scy=(70+50+tma_dy*tma_yu)
	
	'add check to see if window will not be larger than screen
	
	if screensizecheck=1 andalso tma_dx*tma_dy>constcip then 'avoid oversized window
		tma_scx=815
		tma_scy=750
		oversized=1
	end if
	
	ui_destroywindow(window_creatematrix)
	window_creatematrix=ui_window_new(x0,y0,tma_scx,tma_scy,"Create transposition matrix",WS_VISIBLE or WS_SYSMENU or WS_MINIMIZEBOX)
	button_creatematrix_clear=ui_button_new(20,20,110,30,"Reload matrix",,window_creatematrix)
	'button_creatematrix_saveas=ui_button_new(150,20,80,30,"Save as",,window_creatematrix)
	button_creatematrix_undo=ui_button_new(150,20,80,30,"Undo",,window_creatematrix)
	button_creatematrix_transpose=ui_button_new(250,20,100,30,"Transpose",,window_creatematrix)
	button_creatematrix_untranspose=ui_button_new(370,20,100,30,"Untranspose",,window_creatematrix)
	button_creatematrix_exportoutput=ui_button_new(490,20,200,30,"Export to output window",,window_creatematrix)
	
	if tma_dx=0 andalso tma_dy=0 then
		label=ui_label_new(20,70,700,25,"Open a cipher in the input window and click reload matrix.",,window_creatematrix)
		tma_ok=0
	end if
	
	if oversized=1 then
		label=ui_label_new(20,70,700,25,"Dimensions total cannot exceed "+str(constcip)+" entries.",,window_creatematrix)
		tma_ok=0
	end if
	
	if tma_ok=1 then
	
		tma_c=1
		redim tma(tma_dx,tma_dy)
		redim tma_his1(tma_dx*tma_dy,tma_dx,tma_dy)
		redim tma_his2(tma_dx*tma_dy,2)
		redim buttons_creatematrix(tma_dx,tma_dy)
		tma_his2(0,0)=0
		tma_his2(0,1)=0
		tma_his2(0,2)=1
		
		i=0
		for yy=0 to tma_dy-1
			for xx=0 to tma_dx-1
				i+=1
				x=xx+1
				y=yy+1
				tma(x,y)=""
				buttons_creatematrix(x,y)=ui_button_new(20+xx*tma_xu,70+yy*tma_yu,tma_xu-1,tma_yu-1,"",,window_creatematrix)
				if i=l then exit for,for
			next xx
		next yy
	
	end if
	
	cm_windowup=1
	
	ui_seticon(window_creatematrix)

end sub

sub toggle_solverthreads(array()as integer,byval length as integer,byval symbols as integer,byval dim_x as integer,byval dim_y as integer,byval outputdir as string,byval toggle as integer,byval thread_from as integer,byval thread_to as integer)
	
	'change i to t
	
	dim as integer i,j,t
	dim as string errornosize="Error: solver does not work with current n-gram size"
	dim as string errorno2="Error: solver only works with 8-grams or 9-grams"
	
	select case toggle
		case 1 'start threads
			solverexist=1	
			for i=thread_from to thread_to 'threads
				sleep twait
				select case ui_listbox_gettext(list_main,ui_listbox_getcursel(list_main)) 'solver
					case "Substitution","Substitution + word cribs","Substitution + crib grid","Substitution + crib list"
						if fileformat>2 then
							thread_ptr(i)=threadcreate(@bhdecrypt_allg_bhgov,cptr(any ptr,i))
						else
							select case ngram_size
								case 2 to 7
									thread_ptr(i)=threadcreate(@bhdecrypt_234567810g,cptr(any ptr,i))
								case 8,9,10
									thread_ptr(i)=threadcreate(@bhdecrypt_810g,cptr(any ptr,i))
							end select
						end if
					case "Substitution + simple transposition","Substitution + nulls and skips","Substitution + columnar rearrangement","Substitution + columnar transposition","Substitution + units","Substitution + rectangles"
						if solvesub_tpseqhom=0 then
							if fileformat>2 then
								thread_ptr(i)=threadcreate(@bhdecrypt_allg_bhgov,cptr(any ptr,i))
							else
								select case ngram_size
									case 2 to 7
										thread_ptr(i)=threadcreate(@bhdecrypt_234567810g,cptr(any ptr,i))
									case 8,9,10
										thread_ptr(i)=threadcreate(@bhdecrypt_810g,cptr(any ptr,i))
								end select
							end if
						else 'use sequential homophones
							thread_ptr(i)=threadcreate(@bhdecrypt_seqhom_234567810g,cptr(any ptr,i))
						end if
					case "Substitution + monoalphabetic groups","Substitution + mono groups + crib list"
						select case ngram_size
							case 8,9,10:thread_ptr(i)=threadcreate(@bhdecrypt_groups_810g,cptr(any ptr,i))
							case else:ui_editbox_settext(output_text,errorno2):solverexist=0
						end select
					case "Higher-order homophonic"
						select case ngram_size
							case 8,9,10:thread_ptr(i)=threadcreate(@bhdecrypt_higherorder_810g,cptr(any ptr,i))
							case else:ui_editbox_settext(output_text,errorno2):solverexist=0
						end select
					case "Bigram substitution"
						select case ngram_size
							case 8,9,10:thread_ptr(i)=threadcreate(@bhdecrypt_bigram_810g,cptr(any ptr,i))
							case else:ui_editbox_settext(output_text,errorno2):solverexist=0
						end select
					case "Substitution + sequential homophones"
						thread_ptr(i)=threadcreate(@bhdecrypt_seqhom_234567810g,cptr(any ptr,i))
					case "Substitution + row bound","Substitution + row bound fragments" 
						select case ngram_size
							case 3 to 7:thread_ptr(i)=threadcreate(@bhdecrypt_rowbound_34567g,cptr(any ptr,i))
							case else:ui_editbox_settext(output_text,errornosize):solverexist=0
						end select
					case "Substitution + sparse polyalphabetism"
						select case ngram_size
							case 2 to 4:ui_editbox_settext(output_text,errornosize):solverexist=0
							case 5 to 8,10:thread_ptr(i)=threadcreate(@bhdecrypt_sparsepoly_567810g,cptr(any ptr,i))
						end select			
					case "Substitution + polyphones"
						select case ngram_size
							case 2 to 4:ui_editbox_settext(output_text,errornosize):solverexist=0
							case 5 to 8,10
								if ui_radiobutton_getcheck(radiobutton_polyphones_user)=1 then thread_ptr(i)=threadcreate(@bhdecrypt_poly_567810g,cptr(any ptr,i))
								if ui_radiobutton_getcheck(radiobutton_polyphones_auto)=1 then thread_ptr(i)=threadcreate(@bhdecrypt_poly_567810g,cptr(any ptr,i))
								if ui_radiobutton_getcheck(radiobutton_polyphones_hafer1)=1 then thread_ptr(i)=threadcreate(@bhdecrypt_poly_hafer_567810g,cptr(any ptr,i))
								if ui_radiobutton_getcheck(radiobutton_polyphones_hafer2)=1 then thread_ptr(i)=threadcreate(@bhdecrypt_poly_hafer_567810g,cptr(any ptr,i))
						end select			
					case "Substitution + vigen�re","Substitution + vigen�re word list"
						select case ngram_size
							case 2:ui_editbox_settext(output_text,errornosize):solverexist=0
							case 3 to 8,10:thread_ptr(i)=threadcreate(@bhdecrypt_vigenere_34567810g,cptr(any ptr,i))
						end select	
					case "Merge sequential homophones"
						thread_ptr(i)=threadcreate(@bhdecrypt_mergeseqhom,cptr(any ptr,i))
				end select
			next i
			if solverexist=1 then erase csol
			
		case 2 'stop threads
			for i=thread_from to thread_to
				thread(i).thread_stop=1
				do
					sleep 0.001
				loop until thread(i).thread_active=0
			next i
			sleep twait
		
		case 3 'start solving
			solver_status_processing=1
			for i=thread_from to thread_to
				thread(i).outputdir=outputdir
				thread(i).l=length
				thread(i).s=symbols
				thread(i).dim_x=dim_x
				thread(i).dim_y=dim_y
				thread(i).score=0
				thread(i).temperature=solvesub_temperature
				thread(i).restarts=solvesub_restarts
				thread(i).subrestartlevels=solvesub_subrestartlevels
				thread(i).ngramfactor=solvesub_ngramfactor
				thread(i).matchweight=solvesub_matchweight
				thread(i).multiplicityweight=solvesub_multiplicityweight
				thread(i).entweight=solvesub_entweight
				thread(i).solver_stop=0
				for j=1 to length
					thread(i).cip(j)=array(j)
				next j
				thread(i).solver_waiting=0
				thread(i).update=0
				sleep twait
			next i
		
		case 4 'stop solving
				
			for i=thread_from to thread_to
				thread(i).solver_stop=1
				sleep twait
				'do
				'	sleep 0.001
				'loop until thread(i).solver_waiting=1
				thread(i).iterations_completed=0
				thread(i).restarts_completed=0
			next i
			task_active="none"
			solver_status_processing=0
			update_solver_status
			
	end select

end sub

sub mainloop
	
	dim as string s,t,soi
	dim as integer i,j,k,a,x,y,e,h,x2,y2
	dim as integer mop1,prev_mop1
	dim as integer mop2,prev_mop2
	dim as integer mop3,prev_mop3
	dim as double local_score
	
	'internal plaintext for normalization and chi-square test purposes
	
	intext="SIRSIWOULDLIKETOEXPRESSMYCONSTANTCONSTERNATIONCONCERNINGYOURPOORTASTEANDLACKOFSYMPATHYFROMTHEPUBLICASEVIDENCEDBYYOURRUNNINGOFTHEADSFORTHEMOVI"
	intext+="EBADLANDSFEATURINGTHEBLURBINNINETEENFIFTYNINEMOSTPEOPLEWEREKILLINGTIMEKITANDHOLLYWEREKILLINGPEOPLEINLIGHTOFRECENTEVENTSTHISKINDOFMURDERGLORIF"
	intext+="ICATIONCANONLYBEDEPLORABLEATBESTNOTTHATTHEGLORIFICATIONOFVIOLENCEWASEVERJUSTIFIABLEWHYDONTYOUSHOWSOMECONCERNFORPUBLICSENSIBILITIESANDCUTTHEAD"
	
	intext+="OLIVARUTIGLIANOWRITESTHATCHARLESDICKENSDESPITEHAVINGLITTLEREGARDFORAUTHORITYORSOCIALELITESFELLINTOTHENARRATIVETRAPCOMMONINALLSORTSOFMEDIAFORD"
	intext+="ECADESTHATTRANSFORMSFASCINATIONWITHPOLICEDETECTIVESANDUNDERCOVERCOPSINTOADMIRATIONRUTIGLIANOCALLSDICKENSSTRANGELYGIDDYACCOUNTOFAPOLICERIDEALO"
	intext+="NGCALLEDONDUTYWITHINSPECTORFIELDSHOCKINGLYHYPOCRITICALBECAUSEBYHISOWNACCOUNTMOSTOFWHATHEWITNESSEDWASTHEINTIMIDATIONOFTHEPOORRUTIGLIANOISECHOI"
	intext+="NGGEORGEORWELLWHOWROTETHATTHEONLYOFFICIALSWHOMDICKENSHANDLESWITHANYKINDOFFRIENDLINESSARESIGNIFICANTLYENOUGHPOLICEMENASRUTIGLIANOPUTSITDICKENS"
	intext+="RUNSINTOWHATMAYBETHEBIGGESTRECURRINGHYPOCRISYINHISCAREERASWELLASTHEHISTORYOFPOPULARENTERTAINMENTTHEINSISTENCETHATPOLICEOFFICERSFIGHTINGCRIMEP"
	intext+="ROVIDESEXCITINGCONTENTWHILEAVOIDINGTHATTHEVASTMAJORITYOFCRIMEFIGHTINGISULTIMATELYTHECONTINUEDOPPRESSIONANDCONVENIENTSCAPEGOATINGOFSOCIETYSMOS"
	intext+="TVULNERABLEPEOPLERUTIGLIANOSHOWHOWTHEMULTILAYEREDFORMALLYCOMPLEXBOOKBLEAKHOUSEFINALLYALLOWSDICKENSTOEXCAVATEHISOWNMISPERCEPTIONSMANYOFTHENOVE"
	intext+="LSDIZZYINGNUMBEROFPLOTLINESARETOUCHEDBYTHESAMEUNDERCOVERAGENTANDONLYBYGATHERINGTOGETHERTHETHREADSANDSEEINGTHEWORKOFTHEPOLICEACROSSMANYNARRATI"
	intext+="VESCANONEBEGINTOGLIMPSETHEFAULTYMACHINATIONSOFJUSTICE"
	
	intext+="ONAPRILEIGHTEENEIGHTEENSEVENTYTWOAUSTINBIDWELLWALKEDINTOGREENANDSONTAILORSONLONDONSRENOWNEDSAVILEROWANDORDEREDEIGHTBESPOKESUITSTWOTOPCOATSAND"
	intext+="ALUXURIOUSDRESSINGGOWNBIDWELLWASTWENTYSIXYEARSOLDSIXFOOTTALLANDHANDSOMELYGROOMEDWITHAWAXEDMUSTACHEANDBUSHYSIDEWHISKERSIFTHEACCENTDIDNTGIVEITA"
	intext+="WAYHISEYECATCHINGWESTERNHATMARKEDHIMOUTASANAMERICANARICHAMERICANLONDONTRADESMENCALLEDAMERICANSWITHBULGESOFMONEYINTHEIRPOCKETSSILVERKINGSANDTH"
	intext+="EYWEREMOSTWELCOMEINUPMARKETESTABLISHMENTSLIKEGREENANDSONWHICHCHARGEDASMUCHFORTHESTRENGTHOFTHEIRREPUTATIONSASFORTHEQUALITYOFTHEIRGOODS"
	
	intext+="MENTIONTHEMEDIEVALPERIODANDPEOPLEFREEASSOCIATETHEMSELVESRIGHTINTOVISIONSOFPLAGUEVIOLENCEANDSHITCOVEREDPEASANTSTHETERMRENAISSANCEONTHEOTHERHAN"
	intext+="DCONJURESUPSTUFFLIKEHUMANISMSCIENCEANDPAINTINGSOFPEOPLETHATACTUALLYLOOKLIKEPEOPLEBUTLATEFOURTEENTHFIFTEENTHANDSIXTEENTHCENTURYITALYCONSISTEDO"
	intext+="FMORETHANJUSTPAINTERSWITHNINJATURTLENAMESWANKINGTHEIRWAYFROMONETUSCANVILLATOANOTHERITWASALSOFULLOFINTRIGUEMURDERANDCOMPLEXINTERGENERATIONALFA"
	intext+="MILYDRAMAIFTHEREWASONEFAMILYTHATFEATUREDHEAVILYINSOMEOFTHEMOSTVIOLENTANDLICENTIOUSSTORIESOFTHEPERIODITWASTHEBORGIASEVENTODAYTHEIRNAMEISABYWOR"
	intext+="DFORDEPRAVITYANDATTHECENTEROFMANYOFTHEWILDESTBORGIASTORIESWASTHEBEAUTIFULWILYTHRICEWEDLUCREZIA"
	
	intext+="WHENWEFIRSTMOVEDINTOOURBUILDINGINOUREARLYTHIRTIESTHEREWASAMOTHERANDTWOBOYSONTHETHIRDFLOORONFRIDAYSTHEBOYSFATHERWOULDAPPEARANDTHEMOTHERWOULDBE"
	intext+="FREEUNTILHEDROPPEDTHEMOFFAGAINONSUNDAYTHEBOYSMUSTNOWBETEENAGERSIHAVENOIDEAWHATSHEEVERDIDWITHHERALONETIMETHEREWASALSOAWORLDWEARYWRITERNAMEDTOM"
	intext+="ACROSSTHEHALLWHOKEPTFITATTHENEARBYCITYRECCENTERANDWHOMWELIKEDQUITEALOTTOTALKTOBRIEFLYANDWHOLIKEDOURSONWHENHEWASJUSTLEARNINGTOWALKTOMWASENCOUR"
	intext+="AGEDTOVACATEBYTHELANDLORDSOFFEROFABUYOUTANDWEDONTKNOWWHEREHESGONEBUTWEMISSTOMTHELONGTIMETENANTINTWELVEAWERETRISKAIDEKAPHOBICINOURBUILDINGWASK"
	intext+="EEPINGHISRENTCONTROLLEDPLACEASASECONDHOMELIVINGTHERELESSTHANHALFTHEYEARWHICHTHEEVICTIONNOTICETAPEDFACEDOWNTOHISDOORSAIDWASAGAINSTTHELAWOVERTI"
	intext+="METHELEGALNOTICEGREWWORNANDFOLDEDANDRAGGEDFROMALLUSCURIOUSNEIGHBORSNOWTHETENANTDOESNTLIVETHEREATALLATTHEMOMENTNOONEDOESTHEREWASJOHNAMILITARYV"
	intext+="ETWEDONTKNOWWHATBRANCHHEWASOLDBUTWEDONTKNOWHOWOLDNOFAMILYTHATWECOULDTELLJOHNSEEMEDTOLIKEMECALLEDMEGUYFELLAORAWORDLIKETHATWHENWEMETATTHEFRONTD"
	intext+="OORORMAILBOXES"
	
	intext+="THEINTERNSHIPWASPAIDANDPRESTIGIOUSANDITDIDNTINCLUDEGETTINGANYONECOFFEEMYCOPYWRITINGWASGOODANDMYBOSSWASKINDCONFIDENCEBEGANTOBLOOMINMYSTOMACHIT"
	intext+="SPETALSDELICATEBUTCOLORFULHENRYRETURNEDTOCAMPUSFORAWEEKTOCELEBRATEHISTWENTYFIRSTBIRTHDAYWITHHISFRIENDSBEFOREHELEFTFORASIAANDIDROVEUPTOSEEHIMW"
	intext+="ESATACROSSFROMEACHOTHERATWESLEYANSCAFEBOOKSTOREFLIRTINGWARILYANDUNABLETOSTOPSMILINGHEWASNERVOUSHISEYESSAUCERWIDEANDWETIPREENEDINMYPINKFAUXLEA"
	intext+="THERJACKETANDBRAGGEDABOUTMYNEWJOBIWANTEDHIMTOBEIMPRESSEDTOREALIZEWHATAMISTAKEHEDMADEBUTMOSTLYIWANTEDTOTAKEHISHANDONTHETABLEANDSQUEEZEITMYFLED"
	intext+="GINGLIFEINNEWYORKFADEDINTOIRRELEVANCELOOKINGATTHISBOYILOVEDIWASNTOVERHIMANDIDIDNTBOTHERTOPRETENDTOBEAFTERANHOURORSOOFPOLITESMALLTALKANDCAREFU"
	intext+="LBANTERHENRYOFFEREDTOWALKMEBACKTOMYFRIENDRACHELSDORMWHEREIWASSTAYINGTHENIGHTHEWOULDBECRASHINGONOURFRIENDFELIXSCOUCHWEMENTIONEDTHESEDETAILSFUR"
	intext+="TIVELYDURINGCONVERSATIONLITTLEMORSECODECLUESTOBETRANSLATEDONLYIFWESHAREDTHESAMEMOTIVESIHADNOEXPECTATIONSOFMYVISITOTHERTHANTOSEEHIMAGAINANDIWA"
	intext+="SPETRIFIEDTOASSUMETOOMUCHANDLEAVEMYSELFEXPOSEDONCEAGAINTHEDESPERATEEXGIRLFRIENDONTHEGUESTROOMFLOOR"
	
	intext+="WHENIWASEIGHTINOTICEDANATLASONTHEBOOKSHELFINMYROOMIHADJUSTSTARTEDAMASSINGLARGEARTBOOKSFROMFAMILYMUSEUMTRIPSBUTTHISWASTHEFIRSTABNORMALLYSIZEDB"
	intext+="OOKINMYPOSESSIONITWASSOODDLYSHAPEDITSPAGESSPILLEDOVERTHEEDGEOFTHESHELFONEDAYIUSEDALLMYSTRENGTHTOWIGGLEITDOWNOFFTHEBOOKCASEISPRAWLEDONMYBEDROO"
	intext+="MFLOORANDBEGANSIFTINGTHROUGHTHELONGPAGESITMUSTHAVEBEENFROMTHEFIFTIESORSIXTIESITSMELLEDOLDBUTITWASCLEARLYABOOKTHATHADBEENCAREDFOROVERTHEYEARSI"
	intext+="TSPAGESWEREAMIXOFPASTELSSODIZZYINGANDCOMPLEXINHOWPINKSSEPARATEDFROMLIGHTGREENANDTHESKINNIESTBLUERIVERSCUTACROSSTHEPAGESONCEIWASOLDENOUGHTOREA"
	intext+="DMYGRANDPASTARTEDCEREMONIOUSLYGIFTINGMEBOOKSFROMHISSHELVES"
	
	intext+="HOWDOYOUTAMEAWITCHHISTORICALLYYOUDONTYOUKILLHERBURNHERHANGHERINTALESTHEWITCHISOFTENAHERASHEDEVILIFYOUWILLAWOMANWHOSLEEPSWITHLUCIFERWHOISSATAN"
	intext+="SMISTRESSWHOBEARSADEMONICMARKREADTHEFIFTEENTHCENTURYWITCHHUNTERSMALLEUSMALEFICARUMITLLTELLYOUHERVERYEXISTENCEHERBODYITSELFISAPORTALFROMTHISWO"
	intext+="RLDTOOTHERSANDSHEMUSTBEPUTDOWNLESTSHETEARSARIPINREALITYITSELFWICKEDWITCHESTHESTUFFOFHISTORICALLEGENDANDNIGHTMARISHFAIRYTALESINSPIREATERRORTHA"
	intext+="TVERGESONTHESUBLIMETHATFEELINGEDMUNDBURKEARTICULATEDSOLONGAGOOFSTANDINGONTHEEDGEOFACLIFFWHEREYOUFEELTHESIMULTANEITYOFDANGERANDSPECTACULARAWEM"
	intext+="OUNTAINSARESUBLIMEMILTONSSATANISSUBLIMESUBLIMITYONLYEXISTSINTHINGSTHATCOULDKILLYOUWHICHBRINGYOUTOTHEEDGEOFYOURSELFTHEUNTAMEDFEMININETHENSUREL"
	intext+="YFALLSINTOTHISCATEGORYWITCHESEXISTONTHEMARGINSINTHESHADOWSEVERTHREATENINGTOINVADEANDDISRUPTTHESANCTITYOFTHESOCIALORDERTHESEDAYSDISNEYDOESNTKI"
	intext+="LLWITCHESATLEASTNOTASOFTENASTHEYUSEDTOTHESEDAYSDISNEYISINTERESTEDINTHEULTIMATEREHABILITATIONPROJECTHOWDOYOUMAKETHESEARCHETYPALWONDERSTHISSUBL"
	intext+="IMEFEMININITYLESSFRIGHTENINGLESSPOWERFULPARTICULARLYTOPEOPLEINVESTEDINWOMENANDQUEERSBEHAVINGINNORMATIVELYGENDEREDWAYSYOUMAKETHEWITCHAMOTHER"
	
	intext+="ITWASNINETEENEIGHTYONEINTHEOLDECITYSECTIONOFPHILADELPHIAIWASSIXMYPARENTSWEREARTISTSMYDADACELLISTCOMPOSERARRANGERANDMYMOMAPOTTERANDTEACHERANDO"
	intext+="URTINYBATHROOMSHOWEDITONONEWHOLEWALLMYMOMHUNGAPOSTEROFTHESANFRANCISCOBATHSCIRCAEIGHTEENNINETYWITHLOTSOFGENTSINONEPIECESUITSANDLADIESINFRILLYB"
	intext+="ATHINGBONNETSBYTHETOILETONCINDERBLOCKANDBOARDSHELVESWERESTACKSANDSTACKSOFMAGAZINESNEWYORKERSMOSTLYPOSTCARDSFRAMEDTHEMIRROROVERTHESINKFLESHYVI"
	intext+="NTAGENUDIESWITHBOBBEDHAIRSTANDINGINCHORUSLINESBUTMORETHANTHENAKEDLADIESORTHEBORINGNEWYORKERCARTOONSIDSTAREATAFRAMEDPRINTBYOURFRIENDSTUARTHORN"
	intext+="THEWORDSFINDYOURSELFWEREINBOLDTYPEATTHETOPITLOOKEDLIKEAFAKEHIGHSCHOOLYEARBOOKPAGEFIVEROWSOFHEADSHOTSWITHNAMESTYPEDUNDERTHEMJIMUNDERABLANKLOOK"
	intext+="INGMANCAROLUNDERABLACKLADYWITHANAFROMYPARENTSTOLDMEITWASMAILARTBUTSTUARTDIDNTMAILITTOUSITWASACOLLAGEHEXEROXEDHEMADEAZILLIONCOPIESANDMAILEDTHE"
	intext+="MTOAZILLIONOTHERMAILARTISTSWHOEVERTHEYWEREMAILARTISTSWERELITERALTHEYMAILEDEACHOTHERARTAWHOLENETWORKOFWEIRDOSSOMEPROFESSIONALARTISTSSOMEELEANO"
	intext+="RRIGBYESQUELONELYPEOPLESOMEWHOJUSTLIKEDTHEPOSTALSERVICETHEYHADLISTSOFEACHOTHERSADDRESSESANDEVERYONEEXCHANGEDCOPIESTHEONLYEXPENSEWASXEROXINGAN"
	intext+="DSTAMPSTRULYARTFORTHEPEOPLE"
	
	intext+="THEEQUITABLELIFEBUILDINGATONEHUNDREDMONTGOMERYSTREETSITSINTHEHEARTOFSANFRANCISCOSFINANCIALDISTRICTNAMEDAFTERANINSURANCECOMPANYITWASTHEFIRSTSK"
	intext+="YSCRAPERBUILTINTHECITYAFTERTHEDEPRESSIONASYMBOLOFOPTIMISMRISINGTWENTYFIVESTORIESHIGHWITHMARBLEWALLSTHATSPARKLEDINTHESUNTODAYITISHOMETOALLSORT"
	intext+="SOFBUZZYBAYAREACOMPANIESFROMSPRUCECAPITALPARTNERSINVESTORSANDTHOUGHTLEADERSINTHELIFESCIENCESINDUSTRYTOTHEOUTCASTAGENCYSTRATEGISTSANDCREATIVES"
	intext+="WITHAHYPERGROWTHMINDSETTOGETAWAYFROMTHEHECTICPACEOFINVESTINGSTRATEGIZINGANDCREATINGTENANTSCANBURNOFFCALORIESINSIDETHEBUILDINGSPRIVATEGYMORTAK"
	intext+="ETHEIRLUNCHBREAKATOPALUXURIOUSROOFTOPDECKTHEEQUITABLELIFEBUILDINGISALSOHOMETOTHESANFRANCISCOIMMIGRATIONCOURTTHOUGHITSEASYTOMISSONMYFIRSTVISIT"
	intext+="LASTWINTERTHEONLYHINTTHATACOURTLAYWITHINWASTHESCORESOFFAMILIESINTHELOBBYCLUTCHINGSUMMONSESANDLOOKINGCONFUSEDTHECOURTISABOVEOCCUPYINGTHEFOURTH"
	intext+="EIGHTHANDNINTHFLOORSUPHERETHEELEVATORSOPENEDINTOASLIGHTLYOFFKILTERDIMENSIONASECURITYLINESNAKEDINTOACRAMPEDWAITINGROOMWHICHLEDTOAWINDINGANDWIN"
	intext+="DOWLESSHALLWAYFROMWHICHONEENTEREDIDENTICALWINDOWLESSCOURTROOMSITWASDEEPLYDISORIENTINGIOFTENENCOUNTEREDPEOPLEFUMBLINGAROUNDINTHEHALLWAYNOTSURE"
	intext+="HOWTHEHELLTOGETOUTLASTDECEMBERONATHURSDAYAFTERNOONIMETFRANCISCOUGARTEWHOMANAGESTHEIMMIGRATIONDEFENSEUNITOFTHESANFRANCISCOPUBLICDEFENDERSOFFIC"
	intext+="EUGARTEWHOISFORTYEIGHTWASDRESSEDINADARKGRAYSUITANDHADANEATLYTRIMMEDBEARDANDAYOUTHFULFACEUNLIKEMOSTPEOPLEIENCOUNTEREDHEAPPEAREDATEASEANDWELLRE"
	intext+="STEDHISCLIENTANIRAQIMANNAMEDABBASSATNEARBYBOUNCINGHISRIGHTLEGANDRADIATINGANXIETY"
	
	intext+="FORALONGTIMEITHOUGHTIRANANDCOMPETEDINSPORTASAWAYTOUSETHEMETAPHOROFSPORTTOUNDERSTANDLIFELIFEISAMARATHONIWASOFTENTOLDIREMEMBERWATCHINGANDREWATC"
	intext+="HINGCHARIOTSOFFIREPARTICULARLYTHATMOMENTINTHERAINWHENERICLIDDELLJUSTMINUTESAFTERWINNINGARACESTATESIWANTTOCOMPAREFAITHTORUNNINGINARACEITSHARDI"
	intext+="TREQUIRESENERGYOFWILLILOVEDTHATMOMENTASACHILDESPECIALLYASSOMEONEWHOHADATONEPOINTADEEPAMOUNTOFFAITHBUTIALWAYSPAUSEDTHECLIPBEFOREHESTATEDWHATLA"
	intext+="TERBECAMETOMEMOREOBVIOUSSOWHOAMITOSAYBELIEVEHAVEFAITHINTHEFACEOFLIFESREALITIESIHAVENOFORMULAFORWINNINGARACEEVERYONERUNSINTHEIROWNWAYITSTRUETH"
	intext+="ATEVERYONERUNSINTHEIROWNWAYWHICHISAFACTIVECOMETOAPPRECIATEASIVEGROWNOLDERPATIENCEBOTHWITHMYOWNPECULIARMOVEMENTSTHROUGHLIFEANDWITHTHOSEOFOTHER"
	intext+="SISASKILLIACTIVELYTRYTOCULTIVATEANDMAINTAINANDYETEVENLIDDELLSQUOTEHASTODOWITHWINNINGANDTHATTHEIDEAOFWINNINGORFINISHINGORACCOMPLISHINGHASBECOM"
	intext+="EITSOWNUNIVERSALSIGNIFIERITSNOTABOUTWHATYOUDOITSABOUTWHATYOUHAVEDONE"
	
	'initialization stuff
	'------------------------------------------------------------

	' allocate mem for special use pointers
	filetail = Allocate(4)
	keyguardstr = Allocate(10)
		
	for i=0 to 9
		asc2num(i+48)=i
		asc2num10(i+48)=i*10
		asc2num100(i+48)=i*100
		asc2num1000(i+48)=i*1000
		asc2num10000(i+48)=i*10000
		asc2num100000(i+48)=i*100000
		asc2num1000000(i+48)=i*1000000
	next i
	
	basedir=curdir
	mkdir(curdir+"\Ciphers\")
	mkdir(curdir+"\Output\")
	mkdir(curdir+"\Misc\")
	mkdir(curdir+"\N-grams\")
	
	filter="Text files (*.txt)"+chr(0)+"*.txt*"
	file_load_settings
	task_active="none"
	loadngrams_showmsg=1
	symbols_ngramsize=1
	ngrams_clearprevious=1
	
	solvesub_subrestartlevels=3
	
	for i=1 to solvesub_subrestartlevels
		solvesub_subr(i)=solvesub_subrestartlevels-(i-1) '321
	next i
	
	'solvesub_subr(1)=4
	'solvesub_subr(2)=2
	'solvesub_subr(3)=1
	
	'development=1
	
	for i=1 to constcip
		cpol(i)=1
	next i
	
	for i=2 to constcip
		ioctable(i)=i*(i-1)
		shared_cycle_table(i)=(i*(i-1))^0.73  'solvesub_cyclelengthweight
	next i
	
	for i=1 to 32
		ascii_table(i)=1
	next i
	ascii_table(127)=1
	ascii_table(129)=1
	ascii_table(141)=1
	ascii_table(143)=1
	ascii_table(144)=1
	ascii_table(152)=1
	ascii_table(157)=1
	ascii_table(159)=1
	
	'english 1-gram logs:
	'---------------------------------------
	g1(0)=2428/10
	g1(1)=2272/10
	g1(2)=2317/10
	g1(3)=2347/10
	g1(4)=2467/10
	g1(5)=2288/10
	g1(6)=2303/10
	g1(7)=2383/10
	g1(8)=2421/10
	g1(9)=2079/10
	g1(10)=2231/10
	g1(11)=2366/10
	g1(12)=2316/10
	g1(13)=2410/10
	g1(14)=2430/10
	g1(15)=2290/10
	g1(16)=1981/10
	g1(17)=2390/10
	g1(18)=2407/10
	g1(19)=2447/10
	g1(20)=2340/10
	g1(21)=2225/10
	g1(22)=2291/10
	g1(23)=2053/10
	g1(24)=2313/10
	g1(25)=1981/10
	
	'english chi2:
	'---------------------------------------
	chi2(0)=8.167 'a
	chi2(1)=1.492 'b
	chi2(2)=2.782 'c
	chi2(3)=4.253 'd
	chi2(4)=12.702 'e
	chi2(5)=2.228 'f
	chi2(6)=2.015 'g
	chi2(7)=6.094 'h
	chi2(8)=6.966 'i
	chi2(9)=0.153 'j
	chi2(10)=0.772 'k
	chi2(11)=4.025 'l
	chi2(12)=2.406 'm
	chi2(13)=6.749 'n
	chi2(14)=7.507 'o
	chi2(15)=1.929 'p
	chi2(16)=0.095 'q
	chi2(17)=5.987 'r
	chi2(18)=6.327 's
	chi2(19)=9.056 't
	chi2(20)=2.758 'u
	chi2(21)=0.978 'v
	chi2(22)=2.360 'w
	chi2(23)=0.150 'x
	chi2(24)=1.974 'y
	chi2(25)=0.074 'z

	'english most common letters (ordered)
	'---------------------------------------
	mcl(1)=4 'e
	mcl(2)=19 't
	mcl(3)=0 'a
	mcl(4)=14 'o
	mcl(5)=8 'i
	mcl(6)=13 'n
	mcl(7)=18 's
	mcl(8)=7 'h
	mcl(9)=17 'r
	mcl(10)=3 'd
	mcl(11)=11 'l
	mcl(12)=2 'c
	mcl(13)=20 'u
	mcl(14)=12 'm
	mcl(15)=22 'w
	mcl(16)=5 'f
	mcl(17)=6 'g
	mcl(18)=24 'y
	mcl(19)=15 'p
	mcl(20)=1 'b
	mcl(21)=21 'v
	mcl(22)=10 'k
	mcl(23)=9 'j
	mcl(24)=23 'x
	mcl(25)=16 'q
	mcl(26)=25 'z

	'english chi 2, first-letter of words:
	'---------------------------------------
	chi2fl(0)=11.682 'a
	chi2fl(1)=4.434 'b
	chi2fl(2)=5.238 'c
	chi2fl(3)=3.174 'd
	chi2fl(4)=2.799 'e
	chi2fl(5)=4.027 'f
	chi2fl(6)=1.642 'g
	chi2fl(7)=4.200 'h
	chi2fl(8)=7.294 'i
	chi2fl(9)=0.511 'j
	chi2fl(10)=0.456 'k
	chi2fl(11)=2.415 'l
	chi2fl(12)=3.826 'm
	chi2fl(13)=2.284 'n
	chi2fl(14)=7.631 'o
	chi2fl(15)=4.319 'p
	chi2fl(16)=0.222 'q
	chi2fl(17)=2.826 'r
	chi2fl(18)=6.686 's
	chi2fl(19)=15.978 't
	chi2fl(20)=1.183 'u
	chi2fl(21)=0.824 'v
	chi2fl(22)=5.497 'w
	chi2fl(23)=0.045 'x
	chi2fl(24)=0.763 'y
	chi2fl(25)=0.045 'z
	
	for i=0 to 25
		chi2(i)/=100
		chi2fl(i)/=100
	next i
	
	redim thread(threads)
	
	firststart=1
	create_window_main
	update_solver_status
	solvesub_ngramloctemp=basedir+solvesub_ngramloctemp
	solver_active="Substitution"
	thread_ptr(threadsmax+1)=threadcreate(@thread_load_ngrams,0)
	
	'------------------------------------------------------------

	' beijinghouse load a test cipher to solve
	ui_editbox_settext(input_text,"v\0^/v::^T:C\U^U\=3CC3\X-\+X2o.0:T+03:2")
	
	dim as double timer1

	do
		
		sleep 0.001
		
		#ifdef __fb_linux__
		#else
			
			if getmessage(@msg,0,0,0)<>0 then
				if isdialogmessage(window_combine,@msg)=0 _
					and isdialogmessage(window_dimension,@msg)=0 _
					and isdialogmessage(window_optionssolver,@msg)=0 _
					and isdialogmessage(window_transposition,@msg)=0 _
					and isdialogmessage(window_transpositionsolver,@msg)=0 _
					and isdialogmessage(window_creatematrix,@msg)=0 _
					and isdialogmessage(window_manipulation,@msg)=0 _
					and isdialogmessage(window_cribs,@msg)=0 _
					and isdialogmessage(window_units,@msg)=0 _
					and isdialogmessage(window_symbols,@msg)=0 then
		         	translatemessage(@msg)
		         	dispatchmessage(@msg)
				end if
			end if
		
			if ui_listbox_getcursel(list_dimension)<>prev_index_list_dimension then	
				prev_index_list_dimension=ui_listbox_getcursel(list_dimension)
				s=ui_listbox_gettext(list_dimension,prev_index_list_dimension)
				x=val(left(s,instr(s," ")))
				y=val(right(s,instrrev(s," ")))
				if x>0 andalso x<=info_length then
					soi=string_to_info(ui_editbox_gettext(input_text))
					if soi="Ok" then
						info_x=x
						ui_editbox_settext(input_text,info_to_string(info(),info_length,x,y,info_numerical,0,0))
					else ui_editbox_settext(output_text,soi)
					end if
				end if
			end if
				
			if ui_listbox_getcursel(list_symbols_operations)<>prev_index_list_symbols then	 
				prev_index_list_symbols=ui_listbox_getcursel(list_symbols_operations)
				s=ui_listbox_gettext(list_symbols_operations,prev_index_list_symbols)
				ui_editbox_settext(editbox_symbols_a1,"")
				select case s
					case "Expand selected symbol","Remove selected symbol"
						ui_editbox_settext(editbox_symbols_a1,"select a symbol in the list above")
					case "Replace selected symbol with"
						ui_editbox_settext(editbox_symbols_a1,"character(s) or number(s)#")
					case "Set symbol n-gram size"
						ui_editbox_settext(editbox_symbols_a1,"size#")
					case "Set plaintext letters for selected symbol"
						ui_editbox_settext(editbox_symbols_a1,"amount#")
					case "Set plaintext letters for all symbols"
						ui_editbox_settext(editbox_symbols_a1,"amount#")
				end select	
			end if
			
			if ui_listbox_getcursel(list_transposition)<>prev_index_list_transposition then 'transposition <-----------------------------
				prev_index_list_transposition=ui_listbox_getcursel(list_transposition)
				s=ui_listbox_gettext(list_transposition,prev_index_list_transposition)
				ui_editbox_settext(editbox_transposition_a1,"")
				ui_editbox_settext(editbox_transposition_a2,"")
				ui_editbox_settext(editbox_transposition_a3,"")
				ui_editbox_settext(editbox_transposition_a4,"")
				ui_editbox_settext(editbox_transposition_a5,"")
				ui_editbox_settext(editbox_transposition_a6,"")
				ui_editbox_settext(editbox_transposition_a7,"")
				ui_editbox_settext(editbox_transposition_a8,"")	
				select case s
					case "Normal","Mirror","Flip","Reverse","Columnar 1","Columnar 2","Columnar 3","Columnar 4"
					 	ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")
					case "Diagonal 1","Diagonal 2","Diagonal 3","Diagonal 4","Diagonal 5","Diagonal 6","Diagonal 7","Diagonal 8"
						ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")
					case "Diagonal"
						ui_editbox_settext(editbox_transposition_a1,"shift#")
						ui_editbox_settext(editbox_transposition_a2,"corner# (1 to 8)")
					case "Use transposition matrix"
						ui_editbox_settext(editbox_transposition_a1,"Place list with numbers ranging")
						ui_editbox_settext(editbox_transposition_a2,"from one to the input length in ")
						ui_editbox_settext(editbox_transposition_a3,"the output window.")
					case "Skytale","Period","Period row order","Period column order"
						ui_editbox_settext(editbox_transposition_a1,"period#")
						ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")
					case "Period XY"
						ui_editbox_settext(editbox_transposition_a1,"start x#")
						ui_editbox_settext(editbox_transposition_a2,"start y#")
						ui_editbox_settext(editbox_transposition_a3,"move x#")
						ui_editbox_settext(editbox_transposition_a4,"move y#")
						ui_editbox_settext(editbox_transposition_a5,"occupied offset x#")
						ui_editbox_settext(editbox_transposition_a6,"occupied offset y#")
						ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")	
					case "Offset","Offset column order","Offset row order"
						ui_editbox_settext(editbox_transposition_a1,"offset#")
						ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")
					case "Offset column"
						ui_editbox_settext(editbox_transposition_a1,"column#")
						ui_editbox_settext(editbox_transposition_a2,"offset#")
						ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")
					case "Offset row"
						ui_editbox_settext(editbox_transposition_a1,"row#")
						ui_editbox_settext(editbox_transposition_a2,"offset#")
						ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")
					case "Offset rectangular chain"
						ui_editbox_settext(editbox_transposition_a1,"rectangle x1#")
						ui_editbox_settext(editbox_transposition_a2,"rectangle y1#")
						ui_editbox_settext(editbox_transposition_a3,"rectangle x2#")
						ui_editbox_settext(editbox_transposition_a4,"rectangle y2#")
						ui_editbox_settext(editbox_transposition_a5,"offset#")
					case "Railfence"
						ui_editbox_settext(editbox_transposition_a1,"rails#")
						ui_editbox_settext(editbox_transposition_a2,"start rail# (optional)")
						ui_editbox_settext(editbox_transposition_a3,"initial direction: down=0,up=1 (optional)")
					case "Rearrange rows","Rearrange columns"
						ui_editbox_settext(editbox_transposition_a1,"key#")
					case "Reverse column"
						ui_editbox_settext(editbox_transposition_a1,"column#")
						ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")
					case "Reverse row"
						ui_editbox_settext(editbox_transposition_a1,"row#")
						ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")
					case "Route"
						ui_editbox_settext(editbox_transposition_a1,"direction# (1 to 8)")
						ui_editbox_settext(editbox_transposition_a2,"horizontal shift#")
						ui_editbox_settext(editbox_transposition_a3,"vertical shift#")
						ui_editbox_settext(editbox_transposition_a4,"prefix alternation# (0-1)")
						ui_editbox_settext(editbox_transposition_a5,"suffix alternation# (0-1)")
						ui_editbox_settext(editbox_transposition_a7,"partial rectangle: x1#, y1# (optional)")
						ui_editbox_settext(editbox_transposition_a8,"partial rectangle: x2#, y2# (optional)")
					case "Swap row"
						ui_editbox_settext(editbox_transposition_a1,"swap: row#")
						ui_editbox_settext(editbox_transposition_a2,"with: row#")
					case "Swap column"
						ui_editbox_settext(editbox_transposition_a1,"swap: column#")
						ui_editbox_settext(editbox_transposition_a2,"with: column#")
				end select	
			end if
			
			if ui_listbox_getcursel(list_combine_operations)<>prev_index_list_combine_operations then	'combine <-----------------------------
				prev_index_list_combine_operations=ui_listbox_getcursel(list_combine_operations)
				s=ui_listbox_gettext(list_combine_operations,prev_index_list_combine_operations)
				ui_editbox_settext(editbox_combine_a1,"")
				ui_editbox_settext(editbox_combine_a2,"")
				ui_editbox_settext(editbox_combine_a3,"")
				ui_editbox_settext(editbox_combine_a4,"")
				ui_editbox_settext(editbox_combine_a5,"")
				ui_editbox_settext(editbox_combine_a6,"")
				select case s
					case "Dimension"
						ui_editbox_settext(editbox_combine_a1,"from: x-dimension#")
						ui_editbox_settext(editbox_combine_a2,"to: x-dimension#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)") 
					case "Directions"
						ui_editbox_settext(editbox_combine_a1,"from: direction# (1 to 16)")
						ui_editbox_settext(editbox_combine_a2,"to: direction# (1 to 16)")
					case "Period","Period row order","Period column order"
						ui_editbox_settext(editbox_combine_a1,"from: period#")
						ui_editbox_settext(editbox_combine_a2,"to: period#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
					case "Skytale"
						ui_editbox_settext(editbox_combine_a1,"from: period#")
						ui_editbox_settext(editbox_combine_a2,"to: period#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
					case "Offset","Offset row order","Offset column order"
						ui_editbox_settext(editbox_combine_a1,"from: offset#")
						ui_editbox_settext(editbox_combine_a2,"to: offset#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
					case "Expand symbol"
						ui_editbox_settext(editbox_combine_a1,"from: symbol#")
						ui_editbox_settext(editbox_combine_a2,"to: symbol#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
					case "Add character","Add column","Add row","Add random character"
						ui_editbox_settext(editbox_combine_a1,"from: position#")
						ui_editbox_settext(editbox_combine_a2,"to: position#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
					case "Add column (using random symbols)"
						ui_editbox_settext(editbox_combine_a1,"amount: from#")
						ui_editbox_settext(editbox_combine_a2,"amount: to#")
						ui_editbox_settext(editbox_combine_a4,"column#")
					case "Add row (using random symbols)"
						ui_editbox_settext(editbox_combine_a1,"amount: from#")
						ui_editbox_settext(editbox_combine_a2,"amount: to#")
						ui_editbox_settext(editbox_combine_a4,"row#")
					case "Add characters","Remove characters","Random nulls","Random skips"
						ui_editbox_settext(editbox_combine_a1,"amount: from#")
						ui_editbox_settext(editbox_combine_a2,"amount: to#")
						ui_editbox_settext(editbox_combine_a3,"")
						ui_editbox_settext(editbox_combine_a4,"amount#")
					case "Add nulls and skips"
						ui_editbox_settext(editbox_combine_a1,"amount: from#")
						ui_editbox_settext(editbox_combine_a2,"amount: to#")
						ui_editbox_settext(editbox_combine_a3,"")
						ui_editbox_settext(editbox_combine_a4,"nulls#")
						ui_editbox_settext(editbox_combine_a5,"skips#")
					case "Remove character","Remove column","Remove row","Randomize row","Randomize column"
						ui_editbox_settext(editbox_combine_a1,"from: position#")
						ui_editbox_settext(editbox_combine_a2,"to: position#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
					case "Randomize","Noop","Randomize row order","Randomize column order"
						ui_editbox_settext(editbox_combine_a1,"amount: from#")
						ui_editbox_settext(editbox_combine_a2,"amount: to#")
					case"Randomize and bigrams"
						ui_editbox_settext(editbox_combine_a1,"amount: from#")
						ui_editbox_settext(editbox_combine_a2,"amount: to#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
						ui_editbox_settext(editbox_combine_a4,"bigrams: amount#")
					case "Plaintext"
						ui_editbox_settext(editbox_combine_a1,"from: plaintext#")
						ui_editbox_settext(editbox_combine_a2,"to: plaintext#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
					case "Encode: caesar shift"
						ui_editbox_settext(editbox_combine_a1,"amount: from#")
						ui_editbox_settext(editbox_combine_a2,"amount: to#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
						ui_editbox_settext(editbox_combine_a4,"shift#")
						ui_editbox_settext(editbox_combine_a5,"from: position# ")
						ui_editbox_settext(editbox_combine_a6,"to: position#")
					case "Encode: homophonic substitution","Encode: homophonic substitution 1-170","Encode: homophonic substitution 171-340"_
						,"Encode: homophonic substitution 2"
						ui_editbox_settext(editbox_combine_a1,"amount: from#")
						ui_editbox_settext(editbox_combine_a2,"amount: to#")
						ui_editbox_settext(editbox_combine_a3,"step# (optional)")
						ui_editbox_settext(editbox_combine_a4,"symbols#")
						ui_editbox_settext(editbox_combine_a5,"raw IOC approximation# (optional)")
						ui_editbox_settext(editbox_combine_a6,"sequential randomness% (optional)")
				end select	
			end if
			
			if ui_listbox_getcursel(list_manipulation_operations)<>prev_index_list_manipulation_operations then 'manipulation <------------------------------
				prev_index_list_manipulation_operations=ui_listbox_getcursel(list_manipulation_operations)
				s=ui_listbox_gettext(list_manipulation_operations,prev_index_list_manipulation_operations)
				ui_editbox_settext(editbox_manipulation_a1,"")
				ui_editbox_settext(editbox_manipulation_a2,"")
				ui_editbox_settext(editbox_manipulation_a3,"")
				ui_editbox_settext(editbox_manipulation_a4,"")
				ui_editbox_settext(editbox_manipulation_a5,"")
				ui_editbox_settext(editbox_manipulation_a6,"")
				select case s
					case "Add character","Expand character","Remove character"
						ui_editbox_settext(editbox_manipulation_a1,"position#")
					case "Disperse symbol","Expand symbol","Remove symbol"
						ui_editbox_settext(editbox_manipulation_a1,"symbol or number#")
					case "Add null symbol"
						ui_editbox_settext(editbox_manipulation_a1,"symbol or number#")
						ui_editbox_settext(editbox_manipulation_a2,"amount#")
					case "Add nulls and skips"
						ui_editbox_settext(editbox_manipulation_a1,"nulls#")
						ui_editbox_settext(editbox_manipulation_a2,"skips#")
					case "Merge random characters"
						ui_editbox_settext(editbox_manipulation_a1,"merge to: symbol or number#")
						ui_editbox_settext(editbox_manipulation_a2,"amount#")
					case "Merge random symbols"
						ui_editbox_settext(editbox_manipulation_a1,"merge to: symbol or number#")
						ui_editbox_settext(editbox_manipulation_a2,"amount#")
						ui_editbox_settext(editbox_manipulation_a3,"symbol target frequency#")
					case "Add character periodic","Expand periodic","Raise periodic","Randomize positions periodic",_
						"Replace periodic with random filler","Remove character periodic"
						ui_editbox_settext(editbox_manipulation_a1,"from: position#")
						ui_editbox_settext(editbox_manipulation_a2,"to: position#")
						ui_editbox_settext(editbox_manipulation_a3,"step# (optional)")
					case "Encode: homophonic substitution"
						ui_editbox_settext(editbox_manipulation_a1,"from: position#")
						ui_editbox_settext(editbox_manipulation_a2,"to: position#")
						ui_editbox_settext(editbox_manipulation_a3,"step# (optional)")
						ui_editbox_settext(editbox_manipulation_a4,"symbols#")
						ui_editbox_settext(editbox_manipulation_a5,"raw IOC to approximate# (optional)")
						ui_editbox_settext(editbox_manipulation_a6,"sequential randomness% (optional)")
					case "Encode: 2nd order homophonic substitution"
						ui_editbox_settext(editbox_manipulation_a1,"place two plaintexts of equal length")
						ui_editbox_settext(editbox_manipulation_a2,"underneath each other without line breaks")
					case "Encode: homophonic substitution (no repeat window)"
						ui_editbox_settext(editbox_manipulation_a1,"symbols#")
						ui_editbox_settext(editbox_manipulation_a2,"no repeat window size#")
						ui_editbox_settext(editbox_manipulation_a3,"raw ioc to approximate# (optional)")
					case "Encode: vigen�re"
						ui_editbox_settext(editbox_manipulation_a1,"from: position#")
						ui_editbox_settext(editbox_manipulation_a2,"to: position#")
						ui_editbox_settext(editbox_manipulation_a3,"step# (optional)")
						ui_editbox_settext(editbox_manipulation_a4,"keyword")
					case "Encode: digraph substitution"
						ui_editbox_settext(editbox_manipulation_a1,"first letter keyword")
						ui_editbox_settext(editbox_manipulation_a2,"second letter keyword")
					case "Encode: caesar shift"
						ui_editbox_settext(editbox_manipulation_a1,"from: position#")
						ui_editbox_settext(editbox_manipulation_a2,"to: position#")
						ui_editbox_settext(editbox_manipulation_a3,"step# (optional)")
						ui_editbox_settext(editbox_manipulation_a4,"shift#")
					case "Add column","Remove column","Add column (using random symbols)"
						ui_editbox_settext(editbox_manipulation_a1,"column#")
					case "Add row","Remove row","Add row (using random symbols)"
						ui_editbox_settext(editbox_manipulation_a1,"row#")
					case "Generate numbers"
						ui_editbox_settext(editbox_manipulation_a1,"from: number#")
						ui_editbox_settext(editbox_manipulation_a2,"to: number#")
						ui_editbox_settext(editbox_manipulation_a3,"step# (optional)")
					case "Generate random numbers"
						ui_editbox_settext(editbox_manipulation_a1,"amount#")
						ui_editbox_settext(editbox_manipulation_a2,"range: from#")
						ui_editbox_settext(editbox_manipulation_a3,"range: to#")
					case "Raise periodic"
						ui_editbox_settext(editbox_manipulation_a1,"from: position#")
						ui_editbox_settext(editbox_manipulation_a2,"to: position#")
						ui_editbox_settext(editbox_manipulation_a3,"step# (optional)")
					case "Raise unique bigrams"
					case "Randomize and bigrams"
						ui_editbox_settext(editbox_manipulation_a1,"bigrams: amount#")
					case "Randomize characters","Add null characters"
						ui_editbox_settext(editbox_manipulation_a1,"amount#")
					case "Assign homophones"
						ui_editbox_settext(editbox_manipulation_a1,"symbol or number#")
						ui_editbox_settext(editbox_manipulation_a2,"homophones#")
						ui_editbox_settext(editbox_manipulation_a3,"sequential=1 (optional)")
					case "Encode: fractioned morse"
						ui_editbox_settext(editbox_manipulation_a1,"n-gram size# (optional)")
						'ui_editbox_settext(editbox_manipulation_a2,"random key=1 (optional)")
					case "Math"
						ui_editbox_settext(editbox_manipulation_a1,"add#")
						ui_editbox_settext(editbox_manipulation_a2,"subtract#")
						ui_editbox_settext(editbox_manipulation_a3,"modulo#")
					case "Modulo"
						ui_editbox_settext(editbox_manipulation_a1,"modulo#")
				end select	
			end if
			
			#include "window_logic_cribgrid.bi"
			#include "window_logic_transpositionmatrix.bi"
			
			select case msg.hwnd
				
				case input_text 'input window select all support hack
			      if msg.message=wm_keyup then
			         if msg.wparam=vk_a then
			            if (getkeystate(vk_control) and &h8000) then ui_sendmessage(input_text,em_setsel,0,-1)
			         end if
			      end if
			      
				case output_text 'output window select all support hack
			      if msg.message=wm_keyup then
			         if msg.wparam=vk_a then
			            if (getkeystate(vk_control) and &h8000) then ui_sendmessage(output_text,em_setsel,0,-1)
			         end if
			      end if
					
				case window_main
					
					select case msg.message
						case wm_command 'menu commands
							select case msg.wparam
								case 1
		              			file_load()
								case 2
									file_save()
								case 3  
									file_save_as()
								case 4
									generate_substrings
									'exit_prog=1
								case 5 
									create_window_dimension
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										get_native_dimensions
									'else ui_editbox_settext(output_text,soi)
									end if
								case 6 'symbols
									create_window_symbols
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then 
										get_symbols(0)
										ui_listbox_setcursel(list_symbols_ngrams,0)
									'else ui_editbox_settext(output_text,soi)
									end if			
								case 7 'directions
									create_window_transposition
								case 8 'convert to numbers
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										info_numerical=1
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,info_numerical,0,0))
									else ui_editbox_settext(output_text,soi)
									end if	
								case 9 'convert to symbols
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										for i=1 to info_length
											if info(i)>255 then
												ui_editbox_settext(output_text,"Error: number to ASCII mismatch")
												exit select
											end if
											if info(i)<32 then 
												ui_editbox_settext(output_text,"Error: number to ASCII mismatch")
												exit select
											end if
											if ascii_table(info(i))>0 then
												ui_editbox_settext(output_text,"Error: number to ASCII mismatch")
												exit select
											end if
										next i
										info_numerical=0
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,info_numerical,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 10 'convert to random symbols
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										if info_symbols<215 then '<-------------------------------------------------------- ok???
											dim as integer tmp1(info_symbols)
											j=0
											for i=1 to info_symbols
												do
													j+=1
												loop until ascii_table(j)=0 or j=256
												tmp1(i)=j
												if j=256 then ui_editbox_settext(output_text,"Error: number to ASCII mismatch")	
											next i
											for i=1 to info_symbols*sqr(info_symbols)
												swap tmp1(int(rnd*info_symbols)+1),tmp1(int(rnd*info_symbols)+1)
											next i
											for i=1 to info_length
												nuba(i)=tmp1(nuba(i))
											next i
											info_numerical=0
											ui_editbox_settext(input_text,info_to_string(nuba(),info_length,info_x,info_y,info_numerical,0,0))
										else ui_editbox_settext(output_text,"Error: number to ASCII mismatch")
										end if
									else ui_editbox_settext(output_text,soi)
									end if
								case 11 'number by appearance
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										info_numerical=1
										ui_editbox_settext(input_text,info_to_string(nuba(),info_length,info_x,info_y,info_numerical,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 12 'solver options
									create_window_optionssolver
								case 13 'benchmark
									if task_active<>"none" then stop_current_task
									sleep 10
									if len(solver_file_name_ngrams)>0 then
										if task_active="none" then
											sleep 10
											'thread_ptr(threadsmax+1)=threadcreate(@thread_benchmark_ngrams,0)
											thread_ptr(threadsmax+1)=threadcreate(@thread_benchmark,0)
										end if
									else ui_editbox_settext(output_text,"Error: no n-grams loaded")
									end if
								case 14 'load n-grams
									s=""
									dim as string oldfilter=filter
'									filter="GOV and Text files (*.gov/*.txt)"+chr(0)+"*.gov;*.txt*;*.gz;*.zst"+chr(0)+"GOV files (*.gov)"+chr(0)+"*.gov"+chr(0)+"Text files (*.txt)"+chr(0)+"*.txt*"+chr(0)+"Gz files (*.gz)"+chr(0)+"*.gz"
									filter="GOV and Text files (*.gov/*.txt)"+chr(0)+"*.gov;*.txt*"+chr(0)+"GOV files (*.gov)"+chr(0)+"*.gov"+chr(0)+"Text files (*.txt)"+chr(0)+"*.txt*"+chr(0)+"Gz files (*.gz)"+chr(0)+"*.gz"
									s=ui_loadsavedialog(0,"Open n-grams",filter,1,basedir+"\N-grams\")
									filter=oldfilter
									if len(s)>0 then
										if task_active<>"none" then stop_current_task
										sleep 10
										if task_active="none" then
											toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",4,1,threads)
											toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",2,1,threads)
											solvesub_ngramloctemp=s
											sleep 10
											thread_ptr(threadsmax+1)=threadcreate(@thread_load_ngrams,0)
										end if
									end if
								case 15 'unispace
									dim as integer ml=0
									soi=string_to_info(ui_editbox_gettext(input_text))
									if unispacing=1 then
										unispacing=0
										ui_modifymenu(hedit,15,MF_UNCHECKED,15,"Unispace numerical")
										ui_drawmenubar(window_main)
									else 
										unispacing=1
										ui_modifymenu(hedit,15,MF_CHECKED,15,"Unispace numerical")
										ui_drawmenubar(window_main)
									end if
									if soi="Ok" then
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,info_numerical,0,0))
									'else ui_editbox_settext(output_text,soi)
									end if
								case 16 'batch ciphers (substitution)
									if task_active<>"none" then stop_current_task
									sleep 10
									if len(solver_file_name_ngrams)>0 then
										if task_active="none" then
											sleep 10
											thread_ptr(threadsmax+1)=threadcreate(@thread_batch_ciphers_substitution,0)
										end if
									else ui_editbox_settext(output_text,"Error: no n-grams loaded")
									end if
								case 17 'rearrange asequentially
									dim as integer e
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as long seqout(info_length)
										dim as short maprs(info_symbols,1)
										for i=1 to info_length
											maprs(nuba(i),0)+=1
											maprs(nuba(i),1)=info(i)
										next i
										do
											e=0
											for i=1 to info_symbols-1
												if maprs(i,0)<maprs(i+1,0) then
													e=1
													swap maprs(i,0),maprs(i+1,0)
													swap maprs(i,1),maprs(i+1,1)
												end if
											next i
										loop until e=0
										e=0
										for i=1 to info_symbols
											for j=1 to maprs(i,0)
												e+=1
												seqout(e)=maprs(i,1)
											next j
										next i
										ui_editbox_settext(input_text,info_to_string(seqout(),info_length,info_x,info_y,info_numerical,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 18 'combine
									combine_stacksize=0
									erase combine_stack
									create_window_combine
								case 19 'randomize positions
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										for i=1 to info_length*sqr(info_length)
											swap info(int(rnd*info_length)+1),info(int(rnd*info_length)+1)
										next i
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,info_numerical,0,0))
									else ui_editbox_settext(output_text,soi)
									end if				
								case 20 'manipulate
									create_window_manipulation
								case 21 'bias n-grams
									if task_active<>"none" then stop_current_task
									if len(solver_file_name_ngrams)>0 then
										sleep 10
										if task_active="none" then
											thread_ptr(threadsmax+1)=threadcreate(@thread_load_ngrambias,0)
										end if
									else ui_editbox_settext(output_text,"Error: no n-grams loaded")
									end if
								case 22 'batch n-grams
									soi=string_to_info(ui_editbox_gettext(input_text))	
									if soi="Ok" then
										if task_active<>"none" then stop_current_task
										sleep 10 
										if task_active="none" then
											sleep 10
											thread_ptr(threadsmax+1)=threadcreate(@thread_batch_ngrams_substitution,0)
										end if
									else ui_editbox_settext(output_text,soi)
									end if	
								case 23 'convert to lowercase
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										ui_editbox_settext(input_text,lcase(ui_editbox_gettext(input_text)))
									else ui_editbox_settext(output_text,soi)
									end if
								case 24 'convert to lowercase
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										ui_editbox_settext(input_text,ucase(ui_editbox_gettext(input_text)))
									else ui_editbox_settext(output_text,soi)
									end if
								case 25 'remove line breaks
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as string text2=""
										dim as string text1=ui_editbox_gettext(input_text)
										for i=1 to len(text1)
											select case asc(text1,i)
												case 10,13
												case else
													text2+=chr(asc(text1,i))
											end select		
										next i
										ui_editbox_settext(input_text,text2)
									else ui_editbox_settext(output_text,soi)
									end if
								case 26 'remove numbers
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as string text2=""
										dim as string text1=ui_editbox_gettext(input_text)
										for i=1 to len(text1)
											select case asc(text1,i)
												case 48 to 57
												case else
													text2+=chr(asc(text1,i))
											end select		
										next i
										ui_editbox_settext(input_text,text2)
									else ui_editbox_settext(output_text,soi)
									end if
								case 27 'remove punct
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as string text2=""
										dim as string text1=ui_editbox_gettext(input_text)
										for i=1 to len(text1)
											select case asc(text1,i)
												'case 65 to 90,97 to 122
												'	text2+=chr(asc(text1,i))
												case 33 to 47
												case 58 to 63
												case 91 to 96
												case 123 to 126
												case 145 to 148
												case 150,151,173 '\96,\AD,\97
												case 133 '\85		
												case else:text2+=chr(asc(text1,i))
											end select		
										next i
										ui_editbox_settext(input_text,text2)
									else ui_editbox_settext(output_text,soi)
									end if
								case 28 'remove spaces
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as string text2=""
										dim as string text1=ui_editbox_gettext(input_text)
										for i=1 to len(text1)
											select case asc(text1,i)
												case 32
												case else
													text2+=chr(asc(text1,i))
											end select		
										next i
										ui_editbox_settext(input_text,text2)
									else ui_editbox_settext(output_text,soi)
									end if
								case 29 'randomize row order
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as double arg(10)
										for i=1 to info_length
											cstate(1,i)=info(i)
										next i
										arg(1)=info_length
										arg(2)=info_symbols
										arg(3)=info_x
										arg(4)=info_y
										cstate_operation(1,2,"Randomize row order",arg())
										for i=1 to info_length
											info(i)=cstate(2,i)
										next i
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,info_numerical,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 30 'randomize column order
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as double arg(10)
										for i=1 to info_length
											cstate(1,i)=info(i)
										next i
										arg(1)=info_length
										arg(2)=info_symbols
										arg(3)=info_x
										arg(4)=info_y
										cstate_operation(1,2,"Randomize column order",arg())
										for i=1 to info_length
											info(i)=cstate(2,i)
										next i
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,info_numerical,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 31 'add row and column numbers
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as integer sbc=0						 
										dim as string sout=""
										if info_numerical=0 then
											sbc=len(str(info_x))+1
											sout+="    "
											for i=1 to info_x
												sout+=str(i)
												if i<>info_x then sout+=space(sbc-len(str(i)))
											next i
											sout+=lb
											k=0
											for i=1 to info_y
												sout+=lb
												sout+=str(i)
												sout+=space(4-len(str(i)))
												for j=1 to info_x
													k+=1
													sout+=chr(info(k))
													if j<>info_x then sout+=space(sbc-1)
												next j
											next i
										else
											for i=1 to info_length
												if len(str(info(i)))+1>sbc then sbc=len(str(info(i)))+1		
											next i
											sout+="    "
											for i=1 to info_x
												sout+=str(i)
												if i<>info_x then sout+=space(sbc-len(str(i)))
											next i
											sout+=lb
											k=0
											for i=1 to info_y
												sout+=lb
												sout+=str(i)
												sout+=space(4-len(str(i)))
												for j=1 to info_x
													k+=1
													sout+=str(info(k))
													if j<>info_x then sout+=space(sbc-len(str(info(k))))
												next j
											next i
										end if
										ui_editbox_settext(input_text,sout)
									else ui_editbox_settext(output_text,soi)
									end if
								case 32 'convert tabs to spaces
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,info_numerical,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 33 'create transposition matrix
									create_window_creatematrix(0,0)
								case 34 'open file to output window
									file_load2()
								case 35 'offset rows top-to-bottom
									dim as string con=ui_editbox_gettext(input_text)
									dim as string conout=""
									soi=string_to_info(con)
									if info_numerical=1 then exit select
									j=0
									if soi="Ok" then
										for i=1 to len(con)
											conout+=chr(asc(con,i))
											if asc(con,i)=10 andalso asc(con,i-1)=13 then
												j+=1
												conout+=space(j)
											end if
										next i
										ui_editbox_settext(input_text,conout)
									else ui_editbox_settext(output_text,soi)
									end if
								case 36 'offset rows bottom-to-top
									dim as string con=ui_editbox_gettext(input_text)
									dim as string conout=""
									dim as string conpart=""
									soi=string_to_info(con)
									if info_numerical=1 then exit select
									j=0
									k=0
									if soi="Ok" then
										for i=1 to len(con)
											if asc(con,i)<>10 andalso asc(con,i)<>13 then j+=1
											if asc(con,i+1)=13 andalso asc(con,i+2)=10 then
												k+=1
												j=0
											end if
										next i
										if j>0 then k+=1
										j=0
										for i=1 to len(con)
											conpart+=chr(asc(con,i))
											if asc(con,i)=10 or i=len(con) then
												j+=1
												conout+=space(k-j)
												conout+=conpart
												conpart=""
											end if
										next i
										ui_editbox_settext(input_text,conout)
									else ui_editbox_settext(output_text,soi)
									end if
								'case 37 'demote solution
								'	if task_active<>"none" then stop_current_task
								'	if len(solver_file_name_ngrams)>0 then
								'		dim as string con=ui_editbox_gettext(output_text)
								'		i=instrrev(con,chr(13)+chr(10)+chr(13)+chr(10))
								'		if i=0 then i=1
								'		con=right(con,len(con)-(i-1))
								'		ngrambias_text=""
								'		ngrambias_factor=-0.01
								'		ngrambias_showmsg=0
								'		for i=1 to len(con)
								'			select case asc(con,i)
								'				case 10,13
								'				case else:ngrambias_text+=chr(asc(con,i))
								'			end select
								'		next i
								'		'ui_editbox_settext(output_text,ngrambias_text)
								'		thread_load_ngrambias(0)
								'		normalize_ngramfactor
								'		ngrambias_showmsg=1
								'		ui_editbox_settext(output_text,"N-gram bias applied")
								'	else ui_editbox_settext(output_text,"Error: no n-grams loaded")
								'	end if
								case 38
									'-----------------------------------> EMPTY
								case 39 'square with spaces
									dim as string con=ui_editbox_gettext(input_text)
									dim as string conout=""
									soi=string_to_info(con)
									if info_numerical=1 then exit select
									j=0
									k=0
									if soi="Ok" then
										for i=1 to len(con)
											if asc(con,i)<>10 andalso asc(con,i)<>13 then j+=1
											if asc(con,i+1)=13 andalso asc(con,i+2)=10 then
												if j>k then k=j
												j=0
											end if
										next i
										if j>k then k=j
										j=0
										for i=1 to len(con)
											if asc(con,i)<>10 andalso asc(con,i)<>13 then j+=1
											conout+=chr(asc(con,i))
											if asc(con,i+1)=13 andalso asc(con,i+2)=10 then
												if k>j then conout+=space(k-j)
												j=0
											end if
										next i
										if j>0 andalso k>j then conout+=space(k-j)
										ui_editbox_settext(input_text,conout)
									else ui_editbox_settext(output_text,soi)
									end if
								case 40 'convert spaces to rows
									dim as string con=ui_editbox_gettext(input_text)
									dim as string conout=""
									dim as short sp=0,passedfirstchar=0
									soi=string_to_info(con)
									j=0
									if soi="Ok" then
										for i=1 to len(con)
											if asc(con,i)<>32 andalso asc(con,i)<>10 andalso asc(con,i)<>13 then
												if sp=1 then 
													if passedfirstchar=1 then conout+=lb
													sp=0
												end if
												conout+=chr(asc(con,i))
												passedfirstchar=1
											end if
											if asc(con,i)=32 then sp=1
										next i
										ui_editbox_settext(input_text,conout)
									else ui_editbox_settext(output_text,soi)
									end if
								case 41 'convert rows to spaces
									dim as string con=ui_editbox_gettext(input_text)
									dim as string conout=""
									dim as short sp=0
									soi=string_to_info(con)
									j=0
									if soi="Ok" then
										for i=1 to len(con)
											if asc(con,i)<>32 andalso asc(con,i)<>10 andalso asc(con,i)<>13 then
												if sp=1 then 
													conout+=" "
													sp=0
												end if
												conout+=chr(asc(con,i))
											end if
											if asc(con,i)=10 andalso asc(con,i-1)=13 then sp=1
										next i
										ui_editbox_settext(input_text,conout)
									else ui_editbox_settext(output_text,soi)
									end if
								case 42 'stat options
									create_window_optionsstats
								case 43 'rearrange sequentially
									dim as integer e
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as long seqout(info_length)
										dim as short maprs(info_symbols,1)
										for i=1 to info_length
											maprs(nuba(i),0)+=1
											maprs(nuba(i),1)=info(i)
										next i
										do
											e=0
											for i=1 to info_symbols-1
												if maprs(i,0)<maprs(i+1,0) then
													e=1
													swap maprs(i,0),maprs(i+1,0)
													swap maprs(i,1),maprs(i+1,1)
												end if
											next i
										loop until e=0
										j=0
										do
											e=0
											for i=1 to info_symbols
												if maprs(i,0)>0 then
													e=1
													j+=1
													seqout(j)=maprs(i,1)
													maprs(i,0)-=1
												end if
											next i
										loop until e=0
										ui_editbox_settext(input_text,info_to_string(seqout(),info_length,info_x,info_y,info_numerical,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 44 'batch merge sequential homophones
									if task_active<>"none" then stop_current_task
									if len(solver_file_name_ngrams)>0 then
										sleep 100
										if task_active="none" then
											sleep 10
											thread_ptr(threadsmax+1)=threadcreate(@thread_batch_ciphers_mergeseqhom,0)
										end if
									else ui_editbox_settext(output_text,"Error: no n-grams loaded")
									end if
								'case 45 'mark consonants
								'	dim as string mar=ui_editbox_gettext(input_text)
								'	soi=string_to_info(mar)
								'	if soi="Ok" then
								'		for i=1 to info_length
								'			select case info(i)
								'				case 65,69,73,79,85,87,101,105,111,117:mark(i)=1
								'				case else:mark(i)=0
								'			end select
								'		next i
								'	else ui_editbox_settext(output_text,soi)
								'	end if
								'case 46 'mark vowels
								'	dim as string mar=ui_editbox_gettext(input_text)
								'	soi=string_to_info(mar)
								'	if soi="Ok" then
								'		for i=1 to info_length
								'			select case info(i)
								'				case 65,69,73,79,85,87,101,105,111,117:mark(i)=0
								'				case else:mark(i)=1
								'			end select
								'		next i
								'	else ui_editbox_settext(output_text,soi)
								'	end if
								case 47 'batch settings
									'if task_active<>"none" then stop_current_task
									if len(solver_file_name_ngrams)>0 then
										sleep 100
										if task_active="none" then
											sleep 10
											thread_ptr(threadsmax+1)=threadcreate(@thread_batch_settings,0)
										end if
									else ui_editbox_settext(output_text,"Error: no n-grams loaded")
									end if
									'if task_active<>"none" then stop_current_task
									'if len(solver_file_name_ngrams)>0 then
									'	sleep 100
									'	if task_active="none" then
									'		sleep 10
									'		thread_ptr(threadsmax+1)=threadcreate(@thread_batch_settings,0)
									'	end if
									'else ui_editbox_settext(output_text,"Error: no n-grams loaded")
									'end if
								case 48 'n-gram stats
									if len(solver_file_name_ngrams)>0 then
										#include "ngram_stats.bi"
									end if
								case 49 'convert to frequencies
									dim as string cv=ui_editbox_gettext(input_text)
									soi=string_to_info(cv)
									if soi="Ok" then
										dim as short freq01(65536)
										erase freq01
										for i=1 to info_length
											freq01(info(i))+=1
										next i
										for i=1 to info_length
											info(i)=freq01(info(i))
										next i
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,1,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 50 'convert to symbol NBA
									dim as string cv=ui_editbox_gettext(input_text)
									soi=string_to_info(cv)
									if soi="Ok" then
										redim snba(constcip)
										erase snba
										for i=1 to info_length
											snba(nuba(i))+=1
											info(i)=snba(nuba(i))
										next i
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,1,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 51 'convert to distances
									dim as string cv=ui_editbox_gettext(input_text)
									soi=string_to_info(cv)
									if soi="Ok" then
										redim snba(constcip,constcip)
										redim snba1(constcip)
										erase snba,snba1
										for i=1 to info_length
											snba(nuba(i),0)+=1
											snba(nuba(i),snba(nuba(i),0))=i
										next i
										j=0
										erase info
										for i=1 to info_length
											snba1(nuba(i))+=1
											if snba1(nuba(i))<snba(nuba(i),0) then
												j+=1
												info(j)=snba(nuba(i),snba1(nuba(i))+1)-snba(nuba(i),snba1(nuba(i)))
											end if
										next i
										info_length=j
										ui_editbox_settext(input_text,info_to_string(info(),info_length,info_x,info_y,1,0,0))
										redim snba2(0,0)
										redim snba1(0)
									else ui_editbox_settext(output_text,soi)
									end if
								case 52 'convert to incremental map
									dim as string cv=ui_editbox_gettext(input_text)
									soi=string_to_info(cv)
									if soi="Ok" then
										for i=1 to info_length-1
											select case info(i)
												case is<info(i+1):info(i)=asc("*")
												case is>info(i+1):info(i)=asc(".")
												case is=info(i+1):info(i)=asc("=")
											end select
										next i
									else ui_editbox_settext(output_text,soi)	
									end if
									ui_editbox_settext(input_text,info_to_string(info(),info_length-1,info_x,info_y,0,0,0))
								case 53 'generate unique substrings
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as string os
										dim as integer s
										open basedir+"\Output\ciphers.txt" for output as #5
										for i=1 to info_length
											s=nba_to_info_out(info(),i,0)
											'os+="results_sub_directory="+str(int((s/i)*1000))+lb
											os+=info_to_string(info(),i,info_x,info_y,info_numerical,0,0)+lb
											os+=lb
										next i
										print #5,os;
										close #5
									else ui_editbox_settext(output_text,soi)
									end if
								case 54 'output n-grams to binary
									#include "ngrams_output_binary.bi"
								case 55 'convert 2 symbols to 1 number
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as short symn(info_symbols,info_symbols),news=0
										dim as long ncip(info_length)
										dim as string os
										'erase symn
										j=0
										for i=1 to info_length step 2
											j+=1
											if symn(nuba(i),nuba(i+1))=0 then
												news+=1
												symn(nuba(i),nuba(i+1))=news
											end if
											ncip(j)=symn(nuba(i),nuba(i+1))
										next i
										ui_editbox_settext(input_text,info_to_string(ncip(),j,info_x,info_y,1,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 56 'convert 3 symbols to 1 number	
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as short symn(info_symbols,info_symbols,info_symbols),news=0
										dim as long ncip(info_length)
										dim as string os
										'erase symn
										j=0
										for i=1 to info_length step 3
											j+=1
											if symn(nuba(i),nuba(i+1),nuba(i+2))=0 then
												news+=1
												symn(nuba(i),nuba(i+1),nuba(i+2))=news
											end if
											ncip(j)=symn(nuba(i),nuba(i+1),nuba(i+2))
										next i
										ui_editbox_settext(input_text,info_to_string(ncip(),j,info_x,info_y,1,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 57 'convert symbols inbetween spaces to numbers
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										dim as string con=ui_editbox_gettext(input_text)+" "
										dim as short seq(constcip),news=0,nl=0
										dim as long ncip(constcip)
										redim symn(constcip,constcip+1)
										j=0:e=0
										'erase symn,seq,ncip
										for i=1 to len(con)
											select case asc(con,i)
												case 10,13,32 'space or line break
													if j>0 then
														for k=1 to news
															e=1
															if symn(k,1)=j then
																for h=1 to j
																	if symn(k,h+1)<>seq(h) then
																		e=0
																		exit for
																	end if
																next h
															else
																e=0
															end if
															if e=1 then exit for
														next k
														if e=0 then
															news+=1
															symn(news,0)=news 'nba
															symn(news,1)=j 'length
															for h=1 to j
																symn(news,h+1)=seq(h)
															next h
															k=news
														end if
														nl+=1
														ncip(nl)=symn(k,0)
														j=0
													end if
												case else
													j+=1
													seq(j)=asc(con,i)
											end select
										next i
										redim symn(0,0)
										ui_editbox_settext(input_text,info_to_string(ncip(),nl,info_x,info_y,1,0,0))
									else ui_editbox_settext(output_text,soi)
									end if
								case 58 'add spaces to plaintext
									if addspaces_ngrams=1 then
										soi=string_to_info(ui_editbox_gettext(input_text))
										if soi="Ok" then
											dim as long txt(constcip)
											j=0
											for i=1 to info_length
												select case info(i)
													case 65 to 90
														j+=1
														txt(j)=info(i)
													case 97 to 122
														j+=1
														txt(j)=info(i)-32
												end select
											next i
											if j>4 then '5-grams
												ui_editbox_settext(input_text,info_to_string(txt(),j,info_x,info_y,0,2,0))
											else ui_editbox_settext(output_text,"Error: not enough plaintext letters")
											end if
										else ui_editbox_settext(output_text,soi)
										end if
									else ui_editbox_settext(output_text,"Error: internal spacing n-grams not found")
									end if
								case 100 to 300 'statsmenu
									soi=string_to_info(ui_editbox_gettext(input_text))
									if soi="Ok" then
										updatewindow(window_main)
										select case msg.wparam
											case 100:stats_unigrams
											case 101:stats_ngrams
											case 102:stats_encoding
											case 103:stats_periodic(1)
											case 104
												if thread_ptr(threadsmax+2)=0 then
													ui_editbox_settext(output_text,"Please wait..."+lb)
													thread_ptr(threadsmax+2)=threadcreate(@stats_keywordlength,0)
												else stop_measurement=1
												end if
											case 105
												if thread_ptr(threadsmax+2)=0 then
													ui_editbox_settext(output_text,"Please wait..."+lb)
													redim cv1(constcip)
													redim cv2(constcip,constcip)
													thread_ptr(threadsmax+2)=threadcreate(@stats_outputgraphs,0)
													redim cv1(0)
													redim cv2(0,0)
												else stop_measurement=1
												end if
											case 106
												if thread_ptr(threadsmax+2)=0 then
													stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=5
													thread_ptr(threadsmax+2)=threadcreate(@stats_compare_symbolcyclepatterns,0)
												else stop_measurement=1
												end if
											case 107
												if thread_ptr(threadsmax+2)=0 then
													ui_editbox_settext(output_text,"Please wait..."+lb)
													thread_ptr(threadsmax+2)=threadcreate(@stats_findrearrangement,0)
												else stop_measurement=1
												end if
											case 108
												if thread_ptr(threadsmax+2)=0 then
													thread_ptr(threadsmax+2)=threadcreate(@stats_compare_symbolcyclepatterns,0)
												else stop_measurement=1
												end if
											case 109
												if thread_ptr(threadsmax+2)=0 then
													ui_editbox_settext(output_text,"Please wait..."+lb)
													thread_ptr(threadsmax+2)=threadcreate(@stats_observations,0)
												else stop_measurement=1
												end if
											case 110:stats_hafershifts
											case 200 to 250 'plaintext + encoding direction
												if thread_ptr(threadsmax+2)=0 then
													select case msg.wparam
														case 201 to 250:stats_direction_m=msg.wparam-200
													end select
													ui_editbox_settext(output_text,"Please wait..."+lb)
													thread_ptr(threadsmax+2)=threadcreate(@stats_direction,0)
												else stop_measurement=1
												end if
											case 251 to 280
												select case msg.wparam
													case 251:stats_symbolcyclepatternscs=2:stats_symbolcyclepatternsfl=2
													case 252:stats_symbolcyclepatternscs=2:stats_symbolcyclepatternsfl=3
													case 253:stats_symbolcyclepatternscs=2:stats_symbolcyclepatternsfl=4
													case 254:stats_symbolcyclepatternscs=2:stats_symbolcyclepatternsfl=5
													case 255:stats_symbolcyclepatternscs=2:stats_symbolcyclepatternsfl=6
													case 256:stats_symbolcyclepatternscs=2:stats_symbolcyclepatternsfl=7
													case 257:stats_symbolcyclepatternscs=2:stats_symbolcyclepatternsfl=8
													case 258:stats_symbolcyclepatternscs=2:stats_symbolcyclepatternsfl=9
													case 259:stats_symbolcyclepatternscs=2:stats_symbolcyclepatternsfl=10
													case 260:stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=2
													case 261:stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=3
													case 262:stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=4
													case 263:stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=5
													case 264:stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=6
													case 265:stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=7
													case 266:stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=8
													case 267:stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=9
													case 268:stats_symbolcyclepatternscs=3:stats_symbolcyclepatternsfl=10
													case 269:stats_symbolcyclepatternscs=4:stats_symbolcyclepatternsfl=2
													case 270:stats_symbolcyclepatternscs=4:stats_symbolcyclepatternsfl=3
													case 271:stats_symbolcyclepatternscs=4:stats_symbolcyclepatternsfl=4
													case 272:stats_symbolcyclepatternscs=4:stats_symbolcyclepatternsfl=5
													case 273:stats_symbolcyclepatternscs=4:stats_symbolcyclepatternsfl=6
													case 274:stats_symbolcyclepatternscs=4:stats_symbolcyclepatternsfl=7
													case 275:stats_symbolcyclepatternscs=4:stats_symbolcyclepatternsfl=8
													case 276:stats_symbolcyclepatternscs=4:stats_symbolcyclepatternsfl=9
													case 277:stats_symbolcyclepatternscs=4:stats_symbolcyclepatternsfl=10
												end select
												if thread_ptr(threadsmax+2)=0 then
													ui_editbox_settext(output_text,"Please wait..."+lb)
													thread_ptr(threadsmax+2)=threadcreate(@stats_symbolcyclepatterns,0)
												else stop_measurement=1
												end if
											case 160 to 170
												if thread_ptr(threadsmax+2)=0 then
													select case msg.wparam
														case 160:stats_nsymbolcycles=2
														case 161:stats_nsymbolcycles=3
														case 162:stats_nsymbolcycles=4
														case 163:stats_nsymbolcycles=5
														case 164:stats_nsymbolcycles=6
														case 165:stats_nsymbolcycles=7
														case 166:stats_nsymbolcycles=8
													end select
													ui_editbox_settext(output_text,"Please wait..."+lb)
													thread_ptr(threadsmax+2)=threadcreate(@stats_perfectsymbolcycles,0)
												else stop_measurement=1
												end if
											case 171 to 174
												if thread_ptr(threadsmax+2)=0 then
													select case msg.wparam
														case 171:stats_nsymbolcycles=3
														case 172:stats_nsymbolcycles=4
														case 173:stats_nsymbolcycles=5
														case 174:stats_nsymbolcycles=6
													end select
													ui_editbox_settext(output_text,"Please wait..."+lb)
													redim cto(2 to 7,15,10,constcip)
													redim ctmean(20,100000) ' TODO: does this need to me so large??
													thread_ptr(threadsmax+2)=threadcreate(@stats_cycletypes,0)
													redim ctmean(0,0)
													redim cto(0,0,0,0)
												else stop_measurement=1
												end if
											case 120:stats_compare_keymapping
											case 121
'												redim scr1(constcip,constcip)
'												Erase scr1
												stats_compare_kasiskeexamination
'												redim scr1(0,0)
											case 122:stats_compare_equalitytest
											case 123:stats_omnidirectional(2)
											case 124:stats_omnidirectional(3)
											case 125 'free
											case 126:stats_periodic(2)
											case 129:stats_periodic(3)
											case 131:stats_periodic(4)
											case 132:stats_periodic(5)
											case 133:stats_periodic(6)
											case 134:stats_periodic(13)
											case 135:stats_periodic(8)
											case 136:stats_periodic(10)
											case 137:stats_periodic(11)
											case 138:stats_periodic(12)
											case 139:stats_periodic(14)
											case 153:stats_periodic(15)
											case 140 to 152
												if thread_ptr(threadsmax+2)=0 then
													select case msg.wparam
														case 140:encodingnulls_u=1:encodingnulls_t=1:encodingnulls_m=1
														case 141:encodingnulls_u=2:encodingnulls_t=1:encodingnulls_m=2
														case 142:encodingnulls_u=2:encodingnulls_t=1:encodingnulls_m=3
														case 143:encodingnulls_u=3:encodingnulls_t=1:encodingnulls_m=2
														case 144:encodingnulls_u=3:encodingnulls_t=1:encodingnulls_m=3	
														case 145:encodingnulls_u=4:encodingnulls_t=1:encodingnulls_m=2
														case 146:encodingnulls_u=4:encodingnulls_t=1:encodingnulls_m=3
														case 147:encodingnulls_u=5:encodingnulls_t=1:encodingnulls_m=2
														case 148:encodingnulls_u=5:encodingnulls_t=1:encodingnulls_m=3
														case 149:encodingnulls_u=6:encodingnulls_t=1:encodingnulls_m=2
														case 150:encodingnulls_u=6:encodingnulls_t=1:encodingnulls_m=3
														case 151:encodingnulls_u=7:encodingnulls_t=1:encodingnulls_m=2
														case 152:encodingnulls_u=7:encodingnulls_t=1:encodingnulls_m=3
													end select
													thread_ptr(threadsmax+2)=threadcreate(@stats_encodingrandomization,0)
												else stop_measurement=1
												end if
										end select
									else ui_editbox_settext(output_text,soi)
									end if 
							end select
					end select
					
				'window_main
				''------------------------------------------------------------
					
				#include "window_logic_solvers.bi"
				
				case button_main_pauseresume
					if msg.message=wm_lbuttondown then
						if task_active<>"none" then pause_current_task
					end if
					
				case button_main_stoptask
					if msg.message=wm_lbuttondown then
						stop_current_task
					end if
							
				case button_main_openfile
					if msg.message=wm_lbuttondown then file_load()
					
				case button_main_statesave
					if msg.message=wm_lbuttondown then
						text_state=ui_editbox_gettext(input_text)
					end if
				
				case button_main_stateload
					if msg.message=wm_lbuttondown then
						ui_editbox_settext(input_text,text_state)
					end if
				
				case button_main_swapinout
					if msg.message=wm_lbuttondown then
						dim as string tmp_input1=ui_editbox_gettext(input_text)
						dim as string tmp_output1=ui_editbox_gettext(output_text)
						ui_editbox_settext(input_text,tmp_output1)
						ui_editbox_settext(output_text,tmp_input1)
					end if
				
				case button_main_openoutputdir 'by Largo
				   if msg.message=wm_lbuttondown then 'open explorer window
				      dim as string outputdirpath
				      outputdirpath = exepath + "\\output"
				      #ifndef __fb_linux__
				      	shellexecute(null,"explore",outputdirpath,null,null,sw_shownormal)
				      #endif
				   end if
				
				#include "window_controls_polyphones.bi"
				#include "window_controls_units.bi"
				#include "window_controls_cribgrid.bi"
				#include "window_controls_transpositionmatrix.bi"
				#include "window_controls_manipulation.bi"
				#include "window_controls_symbols.bi"
				#include "window_controls_dimension.bi"
				#include "window_controls_transposition.bi"
				#include "window_controls_optionsstats.bi"
				#include "window_controls_optionssolver.bi"
				#include "window_controls_transpositionsolver.bi"
				#include "window_controls_combine.bi"
			
			end select
					
			if ui_window_event_close(window_dimension,msg) then ui_destroywindow(window_dimension)
			if ui_window_event_close(window_creatematrix,msg) then ui_destroywindow(window_creatematrix):cm_windowup=0
			if ui_window_event_close(window_transposition,msg) then ui_destroywindow(window_transposition)
			if ui_window_event_close(window_transpositionsolver,msg) then ui_destroywindow(window_transpositionsolver):ts_windowup=0
			if ui_window_event_close(window_units,msg) then ui_destroywindow(window_units):un_windowup=0
			if ui_window_event_close(window_optionssolver,msg) then ui_destroywindow(window_optionssolver)
			if ui_window_event_close(window_optionsstats,msg) then ui_destroywindow(window_optionsstats)
			if ui_window_event_close(window_combine,msg) then ui_destroywindow(window_combine)
			if ui_window_event_close(window_manipulation,msg) then ui_destroywindow(window_manipulation)
			if ui_window_event_close(window_symbols,msg) then ui_destroywindow(window_symbols)
			if ui_window_event_close(window_cribs,msg) then ui_destroywindow(window_cribs):wc_windowup=0
			if ui_window_event_close(window_polyphones,msg) then ui_destroywindow(window_polyphones):pp_windowup=0
			if ui_window_event_close(window_main,msg) then exit_prog=1		
			
			if exit_prog=1 then
				stop_current_task
				for i=1 to threads
					thread(i).thread_stop=1
					do
						sleep 10
					loop until thread(i).thread_active=0
				next i
				exit do
				' free specially allocated pointers
				DeAllocate(filetail)
				filetail=0
				DeAllocate(keyguardstr)
				keyguardstr=0
			end if
	
		#endif
	
	loop
	
	sleep 100

end sub

sub clean_thread_information
	
	dim as integer ti,tj,tk
	for ti=1 to threads
		thread(ti).itemnumber=0
		thread(ti).itemname=""
		thread(ti).itemname2=""
		thread(ti).l=0
		thread(ti).s=0
		thread(ti).dim_x=0
		thread(ti).dim_y=0
		thread(ti).num=0
		thread(ti).update=0
		thread(ti).score=0
		thread(ti).ngrams=0
		thread(ti).pccycles=0
		thread(ti).chi=0
		thread(ti).ent=0
		thread(ti).ioc=0
		thread(ti).ioc2=0
		thread(ti).iterations=0
		thread(ti).iterationsfactor=0
		thread(ti).temperature=0
		thread(ti).restarts=0
		thread(ti).subrestartlevels=0
		thread(ti).ngramfactor=0
		thread(ti).multiplicity=0
		thread(ti).multiplicityweight=0
		thread(ti).entweight=0
		
		'thread(ti).solver_waiting=0
		'thread(ti).solver_processing=0
		'thread(ti).thread_active=0
		'thread(ti).solver_active=0
		'thread(ti).thread_stop=0
		'thread(ti).solver_stop=0
		
		thread(ti).iterations_completed=0
		thread(ti).restarts_completed=0
		thread(ti).combine_output=0
		thread(ti).outputdir=""
		thread(ti).repeats=""
		thread(ti).effectivesymbols=0
		thread(ti).arg1=0
		thread(ti).tmpd1=0
		thread(ti).match=0
		thread(ti).matchweight=0
		thread(ti).cyclealphabetsize=0
		thread(ti).cyclelengthweight=0
		thread(ti).cyclequalityweight=0
		thread(ti).cyclesizeweight=0
		thread(ti).pcmode=0
		thread(ti).advstats=0
		thread(ti).solkey=0
		thread(ti).solkeyacc=0
		thread(ti).sectime=0
		thread(ti).avgscore=0
		thread(ti).avgioc=0
		thread(ti).avgpccycles=0
		thread(ti).wordflow=0
		for tj=0 to constcip
			thread(ti).cip(tj)=0
			thread(ti).sol(tj)=0
			thread(ti).key(tj)=0
			thread(ti).ckey(tj)=0
			thread(ti).graph(tj)=0
			for tk=0 to 4
				thread(ti).gkey(tj,tk)=0
			next tk
		next tj
		for tj=0 to 100
			for tk=0 to 20
				thread(ti).hkey(100,20)=0
			next tk
		next tj
	next ti
	
end sub

sub update_solver_status
	
	dim as integer i
	dim as integer restarts
	dim as integer iterations
	solver_status=""
	select case solver_status_processing
		case 0	
			solver_status+="Task: "+task_active+lb
			if len(solver_file_name_ngrams)>0 then
				solver_status+=solver_file_name_ngrams+lb
			else
				solver_status+="No n-grams loaded"+lb
			end if	
			solver_status+="---------------------------------------------------------"
		case 1
			if pausetask=0 then
				if threads>1 then
					solver_status+="Task: "+task_active+" (using "+str(threads)+" CPU threads)"+lb
				else
					solver_status+="Task: "+task_active+" (using "+str(threads)+" CPU thread)"+lb
				end if
			else
				solver_status+="Task: "+task_active+" [PAUSED]"+lb
			end if
			if len(solver_file_name_ngrams)>0 then
				solver_status+=solver_file_name_ngrams+lb
			else
				solver_status+="No n-grams loaded"+lb
			end if
			solvesub_avgscore=0
			solvesub_avgioc=0
			solvesub_avgpccycles=0
			for i=1 to threads
				restarts+=thread(i).restarts_completed
				iterations+=thread(i).iterations_completed
				solvesub_avgscore+=thread(i).avgscore
				solvesub_avgioc+=thread(i).avgioc
				solvesub_avgpccycles+=thread(i).avgpccycles
			next i
			if restarts=0 then restarts=1
			dim as integer restartsips=restarts+solvesub_batchciphersbigramsskipped
			solver_status+="---------------------------------------------------------"+lb
			solver_status+="Items: "+str(restartsips-1)
			solver_status+=" Items per second: "+format((restartsips-1)/(timer-ips_timer),"0.00")
			solver_status+=" MIPS: "+format(iterations/(timer-ips_timer)/1000000,"0.00")+lb
			solver_status+="AVG score: "+format(solvesub_avgscore/restarts,"0.00")
			solver_status+=" IOC: "+format(solvesub_avgioc/restarts,"0.00000")
			if solvesub_advstats=1 then solver_status+=" PC-cycles: "+format(solvesub_avgpccycles/restarts,"0.00")
	end select
	ui_editbox_settext(solver_text,solver_status)
	
end sub

function remext(byval s as string)as string
	
	dim as integer a,e,l
	do
		e=0
		a=instr(s,".txt")
		if a=0 then a=instr(s,".gz")
		if a>0 then 
			e=1
			l=len(s)
			s=left(s,a-1)
			's=left(s,a-1)+right(s,l-(a+3))
		end if
	loop until e=0
	return s

end function

sub set_solverhighlight(byval solvername as string)
	
	dim as integer i
	for i=0 to 100
		if lcase(ui_listbox_gettext(list_main,i))=lcase(solvername) then
			ui_listbox_setcursel(list_main,i)
			exit for
		end if
	next i

end sub

sub normalize_ngramfactor
	
	if len(solver_file_name_ngrams)=0 then
		ui_editbox_settext(output_text,"Error: no n-grams loaded")
		exit sub
	end if
	
	if ngram_alphabet_size<>26 then
		ui_editbox_settext(output_text,"Error: alphabet size must be 26")
		exit sub
	end if
	
	dim as byte nnerror
	dim as integer i
	
	for i=0 to ngram_alphabet_size-1 'check alphabet
		select case alphabet(i)
			case 65 to 90
			'case 97 to 122
			case else:nnerror=1
		end select
	next i
	
	if nnerror=1 then
		ui_editbox_settext(output_text,"Error: alphabet must be ABCDEFGHIJKLMNOPQRSTUVWXYZ")
		exit sub
	end if
	
	dim as integer z,z1,z2,j,ioc_int,chi_int,l,s=25,m
	dim as double ngram_score,score,entropy
	dim as string o
	dim as string text=intext
	m = 1000 ' simply set to >0 (and not ngs-1) so gov sections use full guard checking
	in_picker = 1
	
	l=len(text)
	
	dim as byte cip(l),sol(l)
	dim as ushort ngrams(l) ' needed for gov and 9-grams
	dim as integer frq(25)
	
	dim as double enttable(l) 'update with max frequency
	for i=1 to l
		enttable(i)=abs(logbx(i/l,2)*(i/l))
	next i
	
	for i=0 to l-1
		if fileformat>2 then ' gov
			sol(i+1)=text[i] ' regular ascii codes in standard sol array for gov files
		else
			cip(i+1)=text[i]-65  ' old 0-25 range for regular/bh files
			sol(i+1)=text[i]-65  ' needed for 9-gram normalizing
		endif
	'	cip(i+1)=text[i]-65
		frq(text[i]-65)+=1
	next i
	
	for i=0 to 25
		entropy+=enttable(frq(i))
	next i
	
	for i=1 to l-(ngram_size-1)
		if fileformat < 3 then ' not gov
			select case ngram_size
				case 2:ngram_score+=g2(cip(i),cip(i+1))
				case 3:ngram_score+=g3(cip(i),cip(i+1),cip(i+2))	
				case 4:ngram_score+=g4(cip(i),cip(i+1),cip(i+2),cip(i+3))	
				case 5:ngram_score+=g5(cip(i),cip(i+1),cip(i+2),cip(i+3),cip(i+4))	
				case 6:ngram_score+=g6(cip(i),cip(i+1),cip(i+2),cip(i+3),cip(i+4),cip(i+5))
				case 7:ngram_score+=g7(cip(i),cip(i+1),cip(i+2),cip(i+3),cip(i+4),cip(i+5),cip(i+6))
				case 8
					
					ngram_score+=bh8(bh4(cip(i),cip(i+1),cip(i+2),cip(i+3)),bh4(cip(i+4),cip(i+5),cip(i+6),cip(i+7)))

					if ngrams_inmem(9)=1 and i<>l-(ngram_size-1) then  ' beijinghouse combo 8-gram + 9-gram version
						j=i
	
						z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
						If z1 = 0 Then
							ngrams(j) = 0
						Else
							z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
							If z2 = 0 Then
								ngrams(j) = 0
							else
									#include "solver_case9.bi"
							EndIf
						EndIf
					
						ngram_score+=ngrams(j)
					end if
					
'					if i=1 or i=l-(ngram_size-1) then
'						z1 = bh4(cip(i),cip(i+1),cip(i+2),cip(i+3))
'						if z1 <> 0 and z1 < max_allowed_table_index / sqr(2) then
'							z2 = bh4(cip(i+4),cip(i+5),cip(i+6),cip(i+7))
'							if z2 <> 0 and z2 < max_allowed_table_index / sqr(2) then
'								if i=1 then
'									ngram_score+=bh8f(z1,z2)
'								else
'									ngram_score+=bh8l(z1,z2)
'								end if
'							end if
'						end if
'					else	
'						ngram_score+=bh8(bh4(cip(i),cip(i+1),cip(i+2),cip(i+3)),bh4(cip(i+4),cip(i+5),cip(i+6),cip(i+7)))
'					end if
				'case 10:ngram_score+=bh10(bh5(cip(i),cip(i+1),cip(i+2),cip(i+3),cip(i+4)),bh5(cip(i+5),cip(i+6),cip(i+7),cip(i+8),cip(i+9)))

				case 9
					j=i

					z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
					If z1 = 0 Then
						ngrams(j) = 0
					Else
						z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
						If z2 = 0 Then
							ngrams(j) = 0
						else
								#include "solver_case9.bi"
						EndIf
					EndIf
				
					ngram_score+=ngrams(j)

			end select
		else ' gov format
			j=i
			select case ngram_size
				case 2,3,4,5:
					#include "solver_gov_small.bi"
				case 6:
					#include "solver_gov_6.bi"
				case 7:
					#include "solver_gov_7.bi"
				case 8:
					#include "solver_gov_8.bi"
				case 9:
					#include "solver_gov_9.bi"
				case 10:
					#include "solver_gov_10.bi"
			end select
			ngram_score+=z
		end if		
	next i
	
	if ngram_score=0 then
		ui_editbox_settext(output_text,"Error: no corresponding n-grams found")
		exit sub
	end if
	
	dim as integer al=l-(ngram_size-1)
	dim as double ent,scoretohave=24202.69 '25000
'	dim as double ent,scoretohave=(24202.69-4*50)/(7.0/6.0) ' adjust for bonusgrams + wordgrams?
	
	select case solvesub_entweight 'beijinghouse deterministic simplifcation
		case 0.25,0.5,0.75,1,1.5,2:ent=ngram_score*entropy^solvesub_entweight
		case else:ent=ngram_score*fastpow1_single(entropy,solvesub_entweight)
	end select
	solvesub_ngramfactor=(1+((s/l)*solvesub_multiplicityweight))*al*scoretohave/ent
	
	o=ui_listbox_gettext(list_optionssolver,7)
	o=left(o,instr(o,":")-1)
	ui_listbox_replacestring(list_optionssolver,7,o+": "+format(solvesub_ngramfactor,"0.00000")) 'update solver options window
	
end sub

function nba_to_info_out(array()as long,byval l as integer,byval s as integer)as integer
	
	dim as integer i,j
	dim as integer ident(identmax)
	for i=1 to l
		if ident(array(i))=0 then
			j+=1
			info_out(i)=j
			ident(array(i))=j
		else
			info_out(i)=ident(array(i))
		end if
	next i
	return j
	
end function

function nba_to_info_out_short(array()as short,byval l as integer,byval s as integer)as integer
	
	dim as integer i,j
	dim as integer ident(65536)
	for i=1 to l
		if ident(array(i))=0 then
			j+=1
			info_out(i)=j
			ident(array(i))=j
		else
			info_out(i)=ident(array(i))
		end if
	next i
	return j
	
end function

function cstate_operation(byval instate as integer,byval outstate as integer,byval operation as string,arg()as double)as string
	
	dim as integer i,j,k,x,y,e
	
	'arg(1)=info_length
	'arg(2)=info_symbols
	'arg(3)=info_x
	'arg(4)=info_y
	'arg(5)=0
	'arg(6)=ui_checkbox_getcheck(checkbox_transposition_keepnulls)
	'arg(7)=val(ui_editbox_gettext(editbox_transposition_f1))
	'arg(8)=val(ui_editbox_gettext(editbox_transposition_f2))
	'arg(9)=val(ui_editbox_gettext(editbox_transposition_f3))
	'arg(10)=val(ui_editbox_gettext(editbox_transposition_f4))
	
	'for i=1 to arg(1)
	'	cstate(outstate,i)=0
	'next i
	'i=0
	
	select case operation
		
		case "Encode: 2nd order homophonic substitution"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short txt1(l/2)
			dim as short txt2(l/2)
			for i=1 to l/2
				txt1(i)=cstate(instate,i)
				txt2(i)=cstate(instate,i+(l/2))
			next i
			dim as short mat2(s,s)
			'for y=1 to s
			'	for x=1 to s
			'		j+=1
			'		mat2(x,y)=j
			'	next x
			'next y
			s=0
			for i=1 to l/2
				if mat2(txt1(i),txt2(i))=0 then
					s+=1
					mat2(txt1(i),txt2(i))=s
				end if
				cstate(outstate,i)=mat2(txt1(i),txt2(i))
			next i
			return str(l/2)+","+str(s)
				
		case "Add nulls and skips"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short nulls=arg(7)
			dim as short skips=arg(8)
			if nulls<0 then nulls=0
			if skips<0 then skips=0
			if nulls>=l then return "Error: "+lcase(operation)+" (too many nulls)"
			if l+skips>constcip then return "Error: "+lcase(operation)+" (too many skips)"
			dim as short e,r
			dim as short nll(nulls),skp(skips)
			dim as string nslog
			if nulls>0 then nslog+="Nulls("
			for i=1 to nulls 'add char
				nll(i)=int(rnd*(l+1))+1
				nslog+=str(nll(i))
				if i<>nulls then nslog+="," else nslog+=")"
			next i
			if skips>0 then 
				if nulls=0 then 
					nslog+="Skips("
				else
					nslog+=lb+"Skips("
				end if
			end if
			for i=1 to skips 'rem char
				do
					e=0
					r=int(rnd*l)+1
					for j=1 to nulls 'do not rem null
						if r=nll(j) then
							e=1
							exit for
						end if
					next j
					for j=1 to skips 'do not rem same pos
						if r=skp(j) then
							e=1
							exit for
						end if
					next j
				loop until e=0
				skp(i)=r
				nslog+=str(skp(i))
				if i<>skips then nslog+="," else nslog+=")"
			next i
			x=0
			for i=1 to l+1
				for j=1 to nulls
					if nll(j)=i then
						x+=1
						cstate(outstate,x)=cstate(instate,int(rnd*l)+1)
					end if
				next j
				e=0
				for j=1 to skips
					if skp(j)=i then
						e=1
						exit for
					end if
				next j
				if e=0 then
					x+=1
					cstate(outstate,x)=cstate(instate,i)
				end if
			next i
			ui_editbox_settext(output_text,nslog)
			return str(l+(nulls-skips))+","+str(s)
			
		case "Randomize and bigrams"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as long a1=arg(7) 'number of bigrams
			if a1=0 or a1>l then return "Error: "+lcase(operation)+" (bigram repeats amount)"
			dim as integer a,b,c,score
			dim as integer state=rnd*2147483647 or 1
			dim as long cip(l),sym(s)
			dim as uinteger id(s,s)
			s=cstate_nba(instate,2,l,s)
			for i=1 to l
				sym(cstate(2,i))=cstate(instate,i)
				cip(i)=cstate(2,i)
			next i
			do
				for i=1 to l
					state=48271*state and 2147483647
					a=1+l*state shr 31
					state=48271*state and 2147483647
					b=1+l*state shr 31
					swap cip(a),cip(b)
				next i
				c+=1
				score=0
				for i=1 to l-1
					a=cip(i)
					b=cip(i+1)
					if id(a,b)<c then
						id(a,b)=c
					else 
						score+=1
					end if
				next i
			loop until score=a1 'or stoptask=1
			'stoptask=0
			for i=1 to l
				cstate(outstate,i)=sym(cip(i))
			next i
			return str(l)+","+str(s)
			
		case "Randomize and bigrams 2" 'Combine version
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'number of bigrams
			if a1=0 or a1>l then return "Error: "+lcase(operation)+" (bigram repeats amount)"
			dim as integer a,b,c,score
			dim as integer state=rnd*2147483647 or 1
			dim as long cip(l)
			dim as uinteger id(s,s)
			for i=1 to l
				cip(i)=cstate(instate,i)
			next i
			do
				for i=1 to l
					state=48271*state and 2147483647
					a=1+l*state shr 31
					state=48271*state and 2147483647
					b=1+l*state shr 31
					swap cip(a),cip(b)
				next i
				c+=1
				score=0
				for i=1 to l-1
					a=cip(i)
					b=cip(i+1)
					if id(a,b)<c then
						id(a,b)=c
					else 
						score+=1
					end if
				next i
			loop until score=a1
			for i=1 to l
				cstate(outstate,i)=cip(i)
			next i
			
		case "Randomize"
			dim as integer l=arg(1)
			dim as integer tmp(l)
			for i=1 to l
				tmp(i)=i
			next i
			for i=1 to l*5
				swap tmp(int(rnd*l)+1),tmp(int(rnd*l)+1)
			next i
			for i=1 to l
				cstate(outstate,tmp(i))=cstate(instate,i)
			next i
			
		case "Math"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as long a1=arg(7) 'add
			dim as long a2=arg(8) 'sub
			dim as long a3=arg(9) 'mod
			dim as long test(l)
			if a3<2 then return "Error: "+lcase(operation)+" (modulo < 2)"
			for i=1 to l
				test(i)=((cstate(instate,i)mod a3)+a1)-a2
				if test(i)<1 then return "Error: "+lcase(operation)+" negative number(s)"
			next i
			for i=1 to l
				cstate(outstate,i)=test(i)
			next i
			return str(l)+","+str(s)
		
		case "Modulo"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short a1=arg(7) 'modulo
			if a1<2 then return "Error: "+lcase(operation)+" (modulo < 2)"
			for i=1 to l
				cstate(outstate,i)=(cstate(instate,i) mod a1)+1	
			next i
			return str(l)+","+str(s)
		
		case "Encode: fractioned morse"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short a1=arg(7) 'ngram size
			dim as short a2=arg(8) 'random key
			if a1=0 then a1=3
			if a1>4 then return "Error: "+lcase(operation)+" (n-gram size > 4)"
			for i=1 to l
				select case cstate(instate,i)
					case 65 to 90:cstate(instate,i)=cstate(instate,i)-65
					case 97 to 122:cstate(instate,i)=cstate(instate,i)-97
					case else:return "Error: "+lcase(operation)+" (input contains non-letters)"
				end select
			next i	
			dim as byte mor(25,4)
			dim as byte mrc(l*(a1+1))
			mor(0,0)=2:mor(0,1)=0:mor(0,2)=1 'A
			mor(1,0)=4:mor(1,1)=1:mor(1,2)=0:mor(1,3)=0:mor(1,4)=0 'B
			mor(2,0)=4:mor(2,1)=1:mor(2,2)=0:mor(2,3)=1:mor(2,4)=0 'C
			mor(3,0)=3:mor(3,1)=1:mor(3,2)=0:mor(3,3)=0 'D
			mor(4,0)=1:mor(4,1)=0 'E
			mor(5,0)=4:mor(5,1)=0:mor(5,2)=0:mor(5,3)=1:mor(5,4)=0 'F
			mor(6,0)=3:mor(6,1)=1:mor(6,2)=1:mor(6,3)=0 'G
			mor(7,0)=4:mor(7,1)=0:mor(7,2)=0:mor(7,3)=0:mor(7,4)=0 'H
			mor(8,0)=2:mor(8,1)=0:mor(8,2)=0 'I
			mor(9,0)=4:mor(9,1)=0:mor(9,2)=1:mor(9,3)=1:mor(9,4)=1 'J
			mor(10,0)=3:mor(10,1)=1:mor(10,2)=0:mor(10,3)=1 'K
			mor(11,0)=4:mor(11,1)=0:mor(11,2)=1:mor(11,3)=0:mor(11,4)=0 'L
			mor(12,0)=2:mor(12,1)=1:mor(12,2)=1 'M
			mor(13,0)=2:mor(13,1)=1:mor(13,2)=0 'N
			mor(14,0)=3:mor(14,1)=1:mor(14,2)=1:mor(14,3)=1 'O
			mor(15,0)=4:mor(15,1)=0:mor(15,2)=1:mor(15,3)=1:mor(15,4)=0 'P
			mor(16,0)=4:mor(16,1)=1:mor(16,2)=1:mor(16,3)=0:mor(16,4)=1 'Q
			mor(17,0)=3:mor(17,1)=0:mor(17,2)=1:mor(17,3)=0 'R
			mor(18,0)=3:mor(18,1)=0:mor(18,2)=0:mor(18,3)=0 'S
			mor(19,0)=1:mor(19,1)=1 'T
			mor(20,0)=3:mor(20,1)=0:mor(20,2)=0:mor(20,3)=1 'U
			mor(21,0)=4:mor(21,1)=0:mor(21,2)=0:mor(21,3)=0:mor(21,4)=1 'V
			mor(22,0)=3:mor(22,1)=0:mor(22,2)=1:mor(22,3)=1 'W
			mor(23,0)=4:mor(23,1)=1:mor(23,2)=0:mor(23,3)=0:mor(23,4)=1 'X
			mor(24,0)=4:mor(24,1)=1:mor(24,2)=0:mor(24,3)=1:mor(24,4)=1 'Y
			mor(25,0)=4:mor(25,1)=1:mor(25,2)=1:mor(25,3)=0:mor(25,4)=0 'Z
			for i=1 to l
				for j=1 to mor(cstate(instate,i),0)
					k+=1
					mrc(k)=mor(cstate(instate,i),j)
				next j
				if i<l then
					k+=1
					mrc(k)=2
				end if
			next i
			dim as short x1,x2,x3,x4
			dim as short mg1(2):mg1(0)=46:mg1(1)=45:mg1(2)=124
			dim as short mg2(2,2)
			dim as short mg3(2,2,2)
			dim as short mg4(2,2,2,2)
			i=64:j=64:x=32
			for x1=0 to 2
				for x2=0 to 2
					i+=1
					mg2(x1,x2)=i
					for x3=0 to 2
						j+=1
						mg3(x1,x2,x3)=j
						for x4=0 to 2
							x+=1
							mg4(x1,x2,x3,x4)=x
						next x4
					next x3
				next x2
			next x1
			j=0
			for i=1 to k step a1
				j+=1
				select case a1
					case 1:cstate(outstate,j)=mg1(mrc(i))
					case 2:cstate(outstate,j)=mg2(mrc(i),mrc(i+1))
					case 3:cstate(outstate,j)=mg3(mrc(i),mrc(i+1),mrc(i+2))
					case 4:cstate(outstate,j)=mg4(mrc(i),mrc(i+1),mrc(i+2),mrc(i+3))
				end select
			next i
			return str(j)+","+str(s)
		
		case "Assign homophones"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short a1=arg(7) 'symbol#
			dim as short a2=arg(8) 'homophones#
			dim as short a3=arg(9) 'sequential=1
			dim as short h,hp(a2)
			for i=1 to a2
				do
					e=0
					if info_numerical=0 then
						do
							h+=1
							if h>255 then return "Error: "+lcase(operation)+" (not enough ASCII characters)"
						loop until ascii_table(h)=0
					else
						h+=1
					end if
					for j=1 to l
						if cstate(instate,j)=h then
							e=1
							exit for
						end if
					next j
				loop until e=0
				hp(i)=h
			next i
			h=0	
			for i=1 to l
				if cstate(instate,i)=a1 then
					if a3=0 then 'random
						if arg(10)=0 then
							cstate(outstate,i)=hp(int(rnd*a2)+1)
						else
							cstate(outstate,i)=arg(9+int(rnd*a2)+1)
						end if
					else 'sequential
						h+=1
						if h>a2 then h=1
						if arg(10)=0 then
							cstate(outstate,i)=hp(h)
						else
							cstate(outstate,i)=arg(9+h)
						end if
					end if
				else
					cstate(outstate,i)=cstate(instate,i)
				end if	
			next i
			return str(l)+","+str(s+(a2-1))
		
		case "Railfence"
			dim as short l=arg(1)
			dim as short s=arg(2)
			'dim as short dx=arg(3)
			'dim as short dy=arg(4)
			dim as short utp=arg(5)
			dim as short r=arg(7) 'rails
			dim as short o=arg(8) 'start-rail
			dim as short ud=arg(9) 'down=0, up=0
			if r<2 or r>l-1 then return "Error: "+lcase(operation)+" (A1)"
			if o=0 then o=1
			if o<1 or o>r then return "Error: "+lcase(operation)+" (A2)"
			if ud<0 or ud>1 then return "Error: "+lcase(operation)+" (A3)"
			dim as short grid(r,l)	
			if o=1 then ud=0
			if o=r then ud=1
			for i=1 to l
				grid(o,0)+=1
				grid(o,grid(o,0))=i
				if ud=0 then
					o+=1
					if o=r then ud=1
				else
					o-=1
					if o=1 then ud=0
				end if
			next i
			i=0
			for y=1 to r
				for x=1 to grid(y,0)
					i+=1
					if utp=0 then
						cstate(outstate,i)=cstate(instate,grid(y,x))
					else
						cstate(outstate,grid(y,x))=cstate(instate,i)
					end if
				next x
			next y
			
		case "Route"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			dim as short untransposed=arg(5)
			dim as short direction=arg(7) '12345678
			dim as short hshift=arg(8) '0123...
			dim as short vshift=arg(9) '0123...
			dim as short altp=arg(10) '01
			dim as short alts=arg(11) '01
			dim as short dxmax=dx+((dy-1)*abs(hshift))
			dim as short dymax=dy+((dx-1)*abs(vshift))
			dim as short hs,vs,x1,y1
			if dxmax*dymax>32000 then return "Error: "+lcase(operation)+" (A2,A3: arguments out of range)" 
			if dx>l or dx<1 then return "Error: "+lcase(operation)+" (DX: argument out of range)"
			if dy>l or dy<1 then return "Error: "+lcase(operation)+" (DY: argument out of range)"
			if direction<1 or direction>8 then return "Error: "+lcase(operation)+" (A1: argument out of range)"
			'if hshift<0 or hshift>l then return "Error: "+lcase(operation)+" (A2: argument out of range)"
			'if vshift<0 or vshift>l then return "Error: "+lcase(operation)+" (A3: argument out of range)"
			if altp<0 or altp>1 then return "Error: "+lcase(operation)+" (A4: argument out of range)"
			if alts<0 or alts>1 then return "Error: "+lcase(operation)+" (A5: argument out of range)"	 
			dim as short tl(l),tlc
			dim as short grid(dxmax,dymax)
			for y=1 to dy
				if hshift>0 then
					hs=(y-1)*abs(hshift)
				end if
				if hshift<0 then
					hs=(dy-y)*abs(hshift)
				end if
				for x=1 to dx
					i+=1
					grid(x+hs,y+vs)=i 'cstate(instate,i)
					if i=l then exit for,for
					if vshift>0 then
						vs=(x-1)*abs(vshift)
					end if
					if vshift<0 then
						vs=(dx-x)*abs(vshift)
					end if
					'vs+=vshift
				next x
				'hs+=hshift
				'vs=0
			next y
			if direction<5 then 'horizontal
				for y=1 to dymax
					for x=1 to dxmax
						select case direction
							case 1 'LRTB (normal)
								if altp=0 then
									x1=x
								else
									if y mod 2=1 then x1=x else x1=dxmax-(x-1)
								end if
								if alts=0 then
									y1=y
								else
									if y mod 2=1 then y1=(y\2)+1 else y1=dymax-((y\2)-1)
								end if
							case 2 'RLTB (mirror)
								if altp=0 then
									x1=dxmax-(x-1)
								else
									if y mod 2=1 then x1=dxmax-(x-1) else x1=x
								end if
								if alts=0 then
									y1=y
								else
									if y mod 2=1 then y1=(y\2)+1 else y1=dymax-((y\2)-1)
								end if
							case 3 'LRBT (flip)
								if altp=0 then
									x1=x
								else
									if y mod 2=1 then x1=x else x1=dxmax-(x-1)
								end if
								if alts=0 then
									y1=dymax-(y-1)
								else
									if y mod 2=1 then y1=dymax-(y\2) else y1=(y\2)
								end if
							case 4 'RLBT (reverse)
								if altp=0 then
									x1=dxmax-(x-1)
								else
									if y mod 2=1 then x1=dxmax-(x-1) else x1=x
								end if
								if alts=0 then
									y1=dymax-(y-1)
								else
									if y mod 2=1 then y1=dymax-(y\2) else y1=(y\2)
								end if
						end select
						if grid(x1,y1)>0 then
							tlc+=1
							tl(tlc)=grid(x1,y1)
							if tlc=l then exit for,for
						end if
					next x
				next y
			else 'vertical
				for x=1 to dxmax
					for y=1 to dymax
						select case direction
							case 5 'TBLR (columnar 1)
								if altp=0 then
									y1=y	
								else
									if x mod 2=1 then y1=y else y1=dymax-(y-1)
								end if
								if alts=0 then
									x1=x
								else
									if x mod 2=1 then x1=(x\2)+1 else x1=dxmax-((x\2)-1)
								end if
							case 6 'TBRL (columnar 2)
								if altp=0 then
									y1=y
								else
									if x mod 2=1 then y1=y else y1=dymax-(y-1)
								end if
								if alts=0 then
									x1=dxmax-(x-1)
								else
									if x mod 2=1 then x1=dxmax-(x\2) else x1=(x\2)
								end if
							case 7 'BTLR (columnar 3)
								if altp=0 then
									y1=dymax-(y-1)
								else
									if x mod 2=1 then y1=dymax-(y-1) else y1=y
								end if
								if alts=0 then
									x1=x
								else
									if x mod 2=1 then x1=(x\2)+1 else x1=dxmax-((x\2)-1)
								end if
							case 8 'BTRL (columnar 4)
								if altp=0 then
									y1=dymax-(y-1)
								else
									if x mod 2=1 then y1=dymax-(y-1) else y1=y
								end if
								if alts=0 then
									x1=dxmax-(x-1)
								else
									if x mod 2=1 then x1=dxmax-(x\2) else x1=(x\2)
								end if	
						end select
						if grid(x1,y1)>0 then
							tlc+=1
							tl(tlc)=grid(x1,y1)
							if tlc=l then exit for,for
						end if
					next y
				next x
			end if
			for i=1 to l
				if untransposed=0 then
					cstate(outstate,tl(i))=cstate(instate,i)
				else
					cstate(outstate,i)=cstate(instate,tl(i))
				end if
			next i
					
		case "None","Normal"
			dim as short l=arg(1)
			for i=1 to l
				cstate(outstate,i)=cstate(instate,i)
			next i
		
		case "Mirror" '<----------------------------- optimize
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			if dx>l then return "Error: "+lcase(operation)+" (DX > L)"
			if dy>l then return "Error: "+lcase(operation)+" (DY > L)"
			if dx*dy<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short untransposed=arg(5)
			dim as short keepnulls=arg(6)
			dim as short grid(dx,dy)
			for y=1 to dy
				for x=1 to dx
					i+=1
					if i<=l then grid(x,y)=cstate(instate,i)
				next x
			next y
			for y=1 to dy
				for x=1 to dx
					if grid(dx-(x-1),y)>0 then
						j+=1
						cstate(outstate,j)=grid(dx-(x-1),y)	
					end if
				next x						
			next y
		
		case "Mirror rectangular"
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			dim as short untransposed=arg(5)
			dim as short keepnulls=arg(6)
			dim as short sx=arg(7)
			dim as short sy=arg(8)
			dim as short ex=arg(9)
			dim as short ey=arg(10)
			if sx<1 or sx>dx then return "Error: "+lcase(operation)+" (A1)"
			if sy<1 or sy>dy then return "Error: "+lcase(operation)+" (A2)"
			if ex<1 or ex>dx then return "Error: "+lcase(operation)+" (A3)"
			if ey<1 or ey>dy then return "Error: "+lcase(operation)+" (A4)"
			if sx>ex then return "Error: "+lcase(operation)+" (A1 > A3)"
			if sy>ey then return "Error: "+lcase(operation)+" (A2 > A4)"
			dim as short grid(dx,dy)
			dim as short oldline(l)
			for y=1 to dy
				for x=1 to dx
					i+=1
					grid(x,y)=cstate(instate,i)
					if i>l then exit for,for
				next x
			next y
			for y=sy to ey
				i=0
				for x=sx to ex
					i+=1
					oldline(i)=grid(x,y)
				next x
				j=0
				for x=sx to ex
					j+=1
					grid(x,y)=oldline(i-(j-1))
				next x
			next y
			i=0
			for y=1 to dy
				for x=1 to dx
					if grid(x,y)>0 then
						i+=1
						cstate(outstate,i)=grid(x,y)
					end if
				next x
			next y
			
		case "Flip" '<----------------------------- optimize
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			if dx>l then return "Error: "+lcase(operation)+" (DX > L)"
			if dy>l then return "Error: "+lcase(operation)+" (DY > L)"
			if dx*dy<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short untransposed=arg(5)
			dim as short keepnulls=arg(6)
			dim as short grid(dx,dy)
			for y=1 to dy
				for x=1 to dx
					i+=1
					if i<=l then grid(x,y)=cstate(instate,i)
				next x
			next y
			for y=1 to dy
				for x=1 to dx
					if grid(x,dy-(y-1))>0 then
						j+=1
						cstate(outstate,j)=grid(x,dy-(y-1))
					end if
				next x						
			next y
		
		case "Flip rectangular"
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			dim as short untransposed=arg(5)
			dim as short keepnulls=arg(6)
			dim as short sx=arg(7)
			dim as short sy=arg(8)
			dim as short ex=arg(9)
			dim as short ey=arg(10)
			if sx<1 or sx>dx then return "Error: "+lcase(operation)+" (A1)"
			if sy<1 or sy>dy then return "Error: "+lcase(operation)+" (A2)"
			if ex<1 or ex>dx then return "Error: "+lcase(operation)+" (A3)"
			if ey<1 or ey>dy then return "Error: "+lcase(operation)+" (A4)"
			if sx>ex then return "Error: "+lcase(operation)+" (A1 > A3)"
			if sy>ey then return "Error: "+lcase(operation)+" (A2 > A4)"
			dim as short grid(dx,dy)
			dim as short oldline(l)
			for y=1 to dy
				for x=1 to dx
					i+=1
					grid(x,y)=cstate(instate,i)
					if i>l then exit for,for
				next x
			next y
			for x=sx to ex
				i=0
				for y=sy to ey
					i+=1
					oldline(i)=grid(x,y)
				next y
				j=0
				for y=sy to ey
					j+=1
					grid(x,y)=oldline(i-(j-1))
				next y
			next x
			i=0
			for y=1 to dy
				for x=1 to dx
					if grid(x,y)>0 then
						i+=1
						cstate(outstate,i)=grid(x,y)
					end if
				next x
			next y
		
		case "Reverse"
			dim as short l=arg(1)
			for i=1 to l
				cstate(outstate,l-(i-1))=cstate(instate,i)
			next i
		
		case "Columnar 1","Columnar 2","Columnar 3","Columnar 4" '<----------------------------- optimize
			dim as short l=arg(1)
			dim as short ex=arg(3)
			dim as short ey=arg(4)
			if ex>l then return "Error: "+lcase(operation)+" (DX > L)"
			if ey>l then return "Error: "+lcase(operation)+" (DY > L)"
			if ex*ey<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short untransposed=arg(5)
			'dim as short keepnulls=arg(6) 
			dim as short grid(ex,ey)
			dim as short corner
			dim as short xx
			dim as short yy
			select case operation
				case "Columnar 1":corner=1
				case "Columnar 2":corner=2
				case "Columnar 3":corner=3
				case "Columnar 4":corner=4
			end select
			if untransposed=0 then
				for x=1 to ex
					for y=1 to ey
						i+=1
						select case corner
							case 1:xx=x:yy=y
							case 2:xx=ex-(x-1):yy=y
							case 3:xx=x:yy=ey-(y-1)
							case 4:xx=ex-(x-1):yy=ey-(y-1)
						end select
						if i<=l then grid(xx,yy)=cstate(instate,i)
					next y
				next x
				for y=1 to ey
					for x=1 to ex
						if grid(x,y)>0 then
							j+=1
							cstate(outstate,j)=grid(x,y)
						end if					
					next x						
				next y
			else
				for y=1 to ey
					for x=1 to ex
						i+=1
						if i<=l then grid(x,y)=cstate(instate,i)
					next x
				next y		
				for x=1 to ex
					for y=1 to ey
						select case corner
							case 1:xx=x:yy=y
							case 2:xx=ex-(x-1):yy=y
							case 3:xx=x:yy=ey-(y-1)
							case 4:xx=ex-(x-1):yy=ey-(y-1)
						end select
						if grid(xx,yy)>0 then
							j+=1
							cstate(outstate,j)=grid(xx,yy)
						end if					
					next y						
				next x
			end if
			
		case "Diagonal 1","Diagonal 2","Diagonal 3","Diagonal 4","Diagonal 5","Diagonal 6","Diagonal 7","Diagonal 8" '<----------------------------- optimize
			dim as short l=arg(1)
			dim as short ex=arg(3)
			dim as short ey=arg(4)
			if ex>l then return "Error: "+lcase(operation)+" (DX > L)"
			if ey>l then return "Error: "+lcase(operation)+" (DY > L)"
			if ex*ey<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short untransposed=arg(5)
			'dim as short keepnulls=arg(6) 
			dim as short shift=1
			dim as short exs=ex+(shift*(ey-1))
			dim as short grid(exs,ey)
			dim as short xx
			dim as short yy
			dim as short lshift
			dim as short corner
			select case operation
				case "Diagonal 1":lshift=0:corner=1
				case "Diagonal 2":lshift=0:corner=3
				case "Diagonal 3":lshift=1:corner=1
				case "Diagonal 4":lshift=1:corner=3
				case "Diagonal 5":lshift=1:corner=2
				case "Diagonal 6":lshift=1:corner=4
				case "Diagonal 7":lshift=0:corner=2
				case "Diagonal 8":lshift=0:corner=4
			end select	
			if untransposed=0 then
				if lshift=0 then
					for y=1 to ey
						for x=1 to ex 
							i+=1
							if i<=l then grid(x+(shift*(y-1)),y)=1
						next x	
					next y
				else
					for y=1 to ey
						for x=1 to ex 
							i+=1
							if i<=l then grid(x+(shift*(ey-y)),y)=1
						next x	
					next y			
				end if
				for x=1 to exs
					for y=1 to ey
						select case corner
							case 1:xx=x:yy=y
							case 2:xx=exs-(x-1):yy=y 
							case 3:xx=x:yy=ey-(y-1)
							case 4:xx=exs-(x-1):yy=ey-(y-1)
						end select
						if grid(xx,yy)=1 then
							j+=1
							grid(xx,yy)=cstate(instate,j)
						end if
					next y
				next x
				for y=1 to ey
					for x=1 to exs
						if grid(x,y)>0 then
							k+=1
							cstate(outstate,k)=grid(x,y)
						end if
					next x
				next y
			else
				if lshift=0 then
					for y=1 to ey
						for x=1 to ex 
							i+=1
							if i<=l then grid(x+(shift*(y-1)),y)=cstate(instate,i)
						next x	
					next y
				else
					for y=1 to ey
						for x=1 to ex 
							i+=1
							if i<=l then grid(x+(shift*(ey-y)),y)=cstate(instate,i)
						next x	
					next y
				end if
				for x=1 to exs
					for y=1 to ey
						select case corner
							case 1:xx=x:yy=y
							case 2:xx=exs-(x-1):yy=y
							case 3:xx=x:yy=ey-(y-1)
							case 4:xx=exs-(x-1):yy=ey-(y-1)
						end select
						if grid(xx,yy)>0 then
							j+=1
							cstate(outstate,j)=grid(xx,yy)
						end if
					next y
				next x
			end if
		
		case "Diagonal"
			dim as integer l=arg(1)
			dim as integer ex=arg(3)
			dim as integer ey=arg(4)
			dim as integer untransposed=arg(5)
			dim as integer keepnulls=arg(6)
			dim as integer shift=arg(7)
			dim as integer corner=arg(8)
			dim as integer exs=ex+(shift*(ey-1))
			dim as integer grid(exs,ey)
			dim as integer xx
			dim as integer yy
			dim as integer lshift
			select case corner
				case 1:lshift=0:corner=1
				case 2:lshift=0:corner=3
				case 3:lshift=1:corner=1
				case 4:lshift=1:corner=3
				case 5:lshift=1:corner=2
				case 6:lshift=1:corner=4
				case 7:lshift=0:corner=2
				case 8:lshift=0:corner=4
			end select	
			if untransposed=0 then
				if lshift=0 then
					for y=1 to ey
						for x=1 to ex 
							i+=1
							if i<=l then grid(x+(shift*(y-1)),y)=1
						next x	
					next y
				else
					for y=1 to ey
						for x=1 to ex 
							i+=1
							if i<=l then grid(x+(shift*(ey-y)),y)=1
						next x	
					next y			
				end if
				for x=1 to exs step shift
					for y=1 to ey
						for i=0 to shift-1
							select case corner
								case 1:xx=(x+i):yy=y
								case 2:xx=exs-((x+i)-1):yy=y 
								case 3:xx=(x+i):yy=ey-(y-1)
								case 4:xx=exs-((x+i)-1):yy=ey-(y-1)
							end select
							if grid(xx,yy)=1 then
								j+=1
								grid(xx,yy)=cstate(instate,j)
							end if
						next i
					next y
				next x
				for y=1 to ey
					for x=1 to exs
						if grid(x,y)>0 then
							k+=1
							cstate(outstate,k)=grid(x,y)
						end if
					next x
				next y
			else
				if lshift=0 then
					for y=1 to ey
						for x=1 to ex 
							i+=1
							if i<=l then grid(x+(shift*(y-1)),y)=cstate(instate,i)
						next x	
					next y
				else
					for y=1 to ey
						for x=1 to ex 
							i+=1
							if i<=l then grid(x+(shift*(ey-y)),y)=cstate(instate,i)
						next x	
					next y
				end if
				for x=1 to exs step shift
					for y=1 to ey 
						for i=0 to shift-1
							select case corner
								case 1:xx=(x+i):yy=y
								case 2:xx=exs-((x+i)-1):yy=y
								case 3:xx=(x+i):yy=ey-(y-1)
								case 4:xx=exs-((x+i)-1):yy=ey-(y-1)
							end select
							if grid(xx,yy)>0 then
								j+=1
								cstate(outstate,j)=grid(xx,yy)
							end if
						next i
					next y
				next x
			end if
		
		case "Disperse symbol"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7)
			dim as integer r1
			for i=1 to l
				if cstate(instate,i)=a1 then
					do
						r1=cstate(instate,int(rnd*l)+1)
					loop until r1<>a1
					cstate(outstate,i)=r1
				else
					cstate(outstate,i)=cstate(instate,i)
				end if
			next i
			return str(l)+","+str(s) 'nba incorrect
						
		case "Period"
			dim as short l=arg(1)
			dim as short untransposed=arg(5)
			dim as short a=arg(7) 'period
			if a<1 or a>l then return "Error: "+lcase(operation)+" (A1)"
			for i=1 to a
				for j=i to l step a
					k+=1
					if untransposed=0 then 'transpose
						cstate(outstate,j)=cstate(instate,k)
					else 'untranspose
						cstate(outstate,k)=cstate(instate,j)
					end if
				next j
			next i
			
		case "Period partial"
			dim as short l=arg(1)
			dim as short untransposed=arg(5)
			dim as short a1=arg(7) 'period
			dim as short a2=arg(8) 'partial from
			dim as short a3=arg(9) 'partial to
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l then return "Error: "+lcase(operation)+" (A2)"
			if a3<1 or a3>l then return "Error: "+lcase(operation)+" (A3)"
			if a2>a3 then return "Error: "+lcase(operation)+" (A2 > A3)"
			if a1>((a3-a2)+1) then return "Error: "+lcase(operation)+" (A1 > A2+A3)"
			k=(a2-1)
			for i=1 to a1
				for j=(i+(a2-1)) to a3 step a1
					k+=1
					if untransposed=0 then 'transpose
						cstate(outstate,j)=cstate(instate,k)
					else 'untranspose
						cstate(outstate,k)=cstate(instate,j)
					end if							
				next j
			next i
			for i=1 to l
				if cstate(outstate,i)=0 then cstate(outstate,i)=cstate(instate,i)
			next i
		
		case "Period row order" '<----------------------------- optimize
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			if dx>l then return "Error: "+lcase(operation)+" (DX > L)"
			if dy>l then return "Error: "+lcase(operation)+" (DY > L)"
			if dx*dy<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short untransposed=arg(5)
			dim as short a1=arg(7) 'period
			dim as short a2 '=arg(8) 'down/up shift
			dim as short shift '=arg(9) 'shift value
			dim as short newdy=dy+(shift*(dx-1))
			if a1<1 or a1>newdy then return "Error: "+lcase(operation)+" (A1)"
			if a2<0 or a2>1 then return "Error: "+lcase(operation)+" (A2)"
			if shift<0 or shift>dx then return "Error: "+lcase(operation)+" (A3)"
			dim as short row1(newdy)
			dim as short row2(newdy)
			dim as short grid(dx,newdy)
			for i=1 to newdy
				row1(i)=i
			next i
			for i=1 to a1
				for j=i to newdy step a1
					k+=1
					if untransposed=0 then 'transpose
						row2(j)=row1(k)
					else 'untranspose
						row2(k)=row1(j)
					end if							
				next j
			next i
			'i=0
			'for y=1 to dy
			'	for x=1 to dx
			'		i+=1
			'		grid(x,y)=cstate(instate,i)
			'		if i=l then exit for,for
			'	next x
			'next y
			i=0
			if a2=0 then 'down shift
				for y=1 to dy
					for x=1 to dx
						i+=1
						'if i<=l then grid(x+(shift*(y-1)),y)=cstate(instate,i)
						if i<=l then grid(x,y+(shift*(x-1)))=cstate(instate,i)
					next x
				next y
			else 'up shift
				for y=1 to dy
					for x=1 to dx 
						i+=1
						'if i<=l then grid(x+(shift*(dy-y)),y)=cstate(instate,i)
						if i<=l then grid(x,y+(shift*(dx-1)))=cstate(instate,i)
					next x
				next y
			end if
			i=0
			for y=1 to newdy
				for x=1 to dx
					if grid(x,row2(y))>0 then
						i+=1
						cstate(outstate,i)=grid(x,row2(y))
					end if
				next x
			next y
					
		case "Period column order" '<----------------------------- optimize
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			if dx>l then return "Error: "+lcase(operation)+" (DX > L)"
			if dy>l then return "Error: "+lcase(operation)+" (DY > L)"
			if dx*dy<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short untransposed=arg(5)
			dim as short a1=arg(7) 'period
			dim as short a2 '=arg(8) 'left/right shift
			dim as short shift '=arg(9) 'shift value
			dim as short newdx=dx+(shift*(dy-1))
			if a1<1 or a1>newdx then return "Error: "+lcase(operation)+" (A1)"
			if a2<0 or a2>1 then return "Error: "+lcase(operation)+" (A2)"
			if shift<0 or shift>dy then return "Error: "+lcase(operation)+" (A3)"
			dim as short col1(newdx)
			dim as short col2(newdx)
			dim as short grid(newdx,dy)
			for i=1 to newdx
				col1(i)=i
			next i
			for i=1 to a1
				for j=i to newdx step a1
					k+=1
					if untransposed=0 then 'transpose
						col2(j)=col1(k)
					else 'untranspose
						col2(k)=col1(j)
					end if							
				next j
			next i
			'i=0
			'for y=1 to dy
			'	for x=1 to dx
			'		i+=1
			'		grid(x,y)=cstate(instate,i)
			'		if i=l then exit for,for
			'	next x
			'next y
			i=0
			if a2=0 then 'left shift
				for y=1 to dy
					for x=1 to dx 
						i+=1
						if i<=l then grid(x+(shift*(y-1)),y)=cstate(instate,i)
					next x	
				next y
			else 'right shift
				for y=1 to dy
					for x=1 to dx 
						i+=1
						if i<=l then grid(x+(shift*(dy-y)),y)=cstate(instate,i)
					next x	
				next y			
			end if	
			i=0
			for y=1 to dy
				for x=1 to newdx		
					if grid(col2(x),y)>0 then
						i+=1
						cstate(outstate,i)=grid(col2(x),y)
					end if			
				next x
			next y
			
		case "Period XY"
			dim as integer l=arg(1)
			dim as integer dx=arg(3)
			dim as integer dy=arg(4)
			dim as integer untransposed=arg(5)
			dim as integer sx=arg(7)
			dim as integer sy=arg(8)
			dim as integer mx=arg(9)
			dim as integer my=arg(10)
			dim as integer ox=arg(11)
			dim as integer oy=arg(12)
			if sx<1 or sx>dx then return "Error: "+lcase(operation)+" (A1)"
			if sy<1 or sy>dy then return "Error: "+lcase(operation)+" (A2)"
			'if mx<1 then return "Error: "+lcase(operation)+" (A3)"
			'if my<1 then return "Error: "+lcase(operation)+" (A4)"
			dim as integer grid1(dx,dy)
			dim as integer grid2(dx,dy)
			dim as integer x1,y1,x2,y2,px2,py2,e,retries
			for y=1 to dy
				for x=1 to dx
					i+=1
					grid1(x,y)=i
					if i=l then exit for,for
				next x
			next y	
			x1=1
			y1=1
			x2=sx
			y2=sy	
			do
				grid2(x2,y2)=grid1(x1,y1)
				j+=1
				if j=l then exit do
				px2=x2
				py2=y2
				x2+=mx
				if x2>dx then x2=(x2-dx)
				if x2<1 then x2=(dx+x2)
				y2+=my
				if y2>dy then y2=(y2-dy)
				if y2<1 then y2=(dy+y2)
				retries=0
				do
					e=0
					if grid2(x2,y2)>0 then
						retries+=1
						e=1
						x2=px2
						y2=py2
						x2+=ox
						y2+=oy
						if x2>dx then x2=(dx-x2)
						if x2<1 then x2=(dx+x2)
						if y2>dy then y2=(dy-y2)
						if y2<1 then y2=(dy+y2)
					end if
					if retries=l then return "Error: "+lcase(operation)+" (non-unique path)"
				loop until e=0
				x1+=1
				if x1>dx then
					x1=1
					y1+=1
					if y1>dy then y1=1
				end if
			loop
			i=0
			for y=1 to dy
				for x=1 to dx
					if grid2(x,y)>0 then
						i+=1
						if untransposed=0 then
							cstate(outstate,i)=cstate(instate,grid2(x,y))
						else
							cstate(outstate,grid2(x,y))=cstate(instate,i)
						end if
						if i=l then exit for,for
					end if
				next x
			next y		
				
		case "Skytale"
			dim as short l=arg(1)
			dim as short untransposed=arg(5)
			dim as short a=arg(7) 'period
			if a<1 or a>l then return "Error: "+lcase(operation)+" (A1)"
			if gcd(l,a)>1 then return "Error: "+lcase(operation)+" (A1: GCD of cipher length and period > 1)"
			j=1
			for i=1 to l			
				if untransposed=0 then 'transpose
					cstate(outstate,i)=cstate(instate,j)
				else 'untranspose
					cstate(outstate,j)=cstate(instate,i)
				end if		
				j+=a
				if j>l then j-=l		
			next i
			
		case "Nulls"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short a1=arg(7) 'from
			dim as short a2=arg(8) 'to
			dim as short a3=arg(9) 'step
			dim as short a4=arg(5) '0=omit/1=add
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l then return "Error: "+lcase(operation)+" (A2)"
			if a1>a2 then return "Error: "+lcase(operation)+" (A1 > A2)"
			if a3<1 or a3>l then return "Error: "+lcase(operation)+" (A3)"
			if a3>(a2+1)-a1 then return "Error: "+lcase(operation)+" (A3)"
			if a4<0 or a4>3 then return "Error: "+lcase(operation)+" (A4)"
			'dim as short spotlength
			'if l>a2 then spotlength=l else spotlength=a2
			dim as short spot(constcip)
			for i=a1 to a2 step a3
				j+=1
				spot(i)=s+j
			next i
			select case a4
				case 0 'remove
					for i=1 to l
						if spot(i)=0 then
							k+=1
							cstate(outstate,k)=cstate(instate,i)
						end if
					next i
					l-=j
				case 1 'add
					l+=j
					for i=1 to l
						if spot(i)=0 then
							k+=1
							cstate(outstate,i)=cstate(instate,k)
						else
							cstate(outstate,i)=spot(i)
						end if	
					next i
				case 2 'move-to-end
					dim as short nulls(l),nc
					for i=1 to l
						if spot(i)=0 then
							k+=1
							cstate(outstate,k)=cstate(instate,i)
						else
							nc+=1
							nulls(nc)=cstate(instate,i)
						end if
					next i
					for i=1 to nc
						cstate(outstate,k+i)=nulls(i)
					next i
				case 3 'add-and-truncate
					for i=1 to l
						if spot(i)=0 then
							k+=1
							cstate(outstate,i)=cstate(instate,k)
						else
							cstate(outstate,i)=spot(i)
						end if
					next i
			end select
			return str(l)+","+str(s) 'nba incorrect
			
		case "Offset"
			dim as short l=arg(1)
			dim as short untransposed=arg(5)
			dim as short a=arg(7) 'offset
			if a<1 or a>l then return "Error: "+lcase(operation)+" (A1)"
			j=a
			for i=1 to l
				j+=1
				if j>l then j-=l
				if untransposed=0 then 'transpose
					cstate(outstate,j)=cstate(instate,i)
				else 'untranspose	 
					cstate(outstate,i)=cstate(instate,j)
				end if
			next i
			
		case "Offset row order v2"
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			if dx>l then return "Error: "+lcase(operation)+" (DX > L)"
			if dy>l then return "Error: "+lcase(operation)+" (DY > L)"
			if dx*dy<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short a1=arg(7) 'offset
			dim as short a2=arg(8) 'column from
			dim as short a3=arg(9) 'column to
			if a2<1 or a2>dx then return "Error: "+lcase(operation)+" (A2)"
			if a3<1 or a3>dx then return "Error: "+lcase(operation)+" (A3)"
			if a2>a3 then return "Error: "+lcase(operation)+" (A2 > A3)"
			'if a1<1 or a1>(a3+1)-a2 then return "Error: "+lcase(operation)+" (A1)"
			if a1<1 or a1>dy then return "Error: "+lcase(operation)+" (A1)"
			dim as short grid(dx,dy)
			dim as short tmpline(dy)
			for y=1 to dy
				for x=1 to dx
					i+=1
					grid(x,y)=cstate(instate,i)
					if i=l then exit for,for
				next x
			next y
			for x=a2 to a3	
				i=0
				for y=1 to dy
					if grid(x,y)>0 then
						i+=1
						tmpline(i)=grid(x,y)
					end if
				next y
				j=a1
				for k=1 to i
					j+=1
					if j>i then j=1
					grid(x,k)=tmpline(j)
				next k
			next x
			i=0
			for y=1 to dy
				for x=1 to dx
					if grid(x,y)>0 then
						i+=1 
						cstate(outstate,i)=grid(x,y)
						'if i=l then exit for,for
					end if
				next x
			next y
		
		case "Offset row order" '<----------------------------- optimize
			dim as short l=arg(1)
			dim as short x=arg(3)
			dim as short y=arg(4)
			if x>l then return "Error: "+lcase(operation)+" (DX > L)"
			if y>l then return "Error: "+lcase(operation)+" (DY > L)"
			if x*y<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short untransposed=arg(5)
			dim as short keepnulls=0 'arg(6)
			dim as short a=arg(7) 'offset
			if a<0 or a>y then return "Error: "+lcase(operation)+" (A1)"
			dim as integer grid1(x,y)
			dim as integer grid2(x,y)
			dim as short xa,ya,xb,yb
			for ya=1 to y
				for xa=1 to x
					i+=1
					if i>l then 
						grid1(xa,ya)=123456789
					else
						grid1(xa,ya)=cstate(instate,i)
					end if
				next xa
			next ya
			for ya=1 to y
				yb=ya+a
				if yb>y then yb-=y
				for xa=1 to x
					if untransposed=0 then
						grid2(xa,yb)=grid1(xa,ya)
					else
						grid2(xa,ya)=grid1(xa,yb)
					end if
				next xa
			next ya
			i=0
			for ya=1 to y
				for xa=1 to x
					if keepnulls=0 then
						if grid2(xa,ya)=123456789 then 
						else
							i+=1
							if i>l then exit for,for
							cstate(outstate,i)=grid2(xa,ya)
						end if	
					else
						i+=1
						if grid2(xa,ya)=123456789 then
							cstate(outstate,i)=32
						else
							cstate(outstate,i)=grid2(xa,ya)
						end if	
					end if
				next xa
			next ya
			
		case "Offset column order" '<----------------------------- optimize
			dim as short l=arg(1)
			dim as short x=arg(3)
			dim as short y=arg(4)
			if x>l then return "Error: "+lcase(operation)+" (DX > L)"
			if y>l then return "Error: "+lcase(operation)+" (DY > L)"
			if x*y<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short untransposed=arg(5)
			dim as short keepnulls=0 'arg(6)
			dim as short a=arg(7) 'offset
			if a<0 or a>x then return "Error: "+lcase(operation)+" (A1)"
			dim as integer grid1(x,y)
			dim as integer grid2(x,y)
			dim as short xa,ya,xb,yb
			for ya=1 to y
				for xa=1 to x
					i+=1
					if i>l then 
						grid1(xa,ya)=123456789
					else
						grid1(xa,ya)=cstate(instate,i)
					end if
				next xa
			next ya
			for xa=1 to x
				xb=xa+a
				if xb>x then xb-=x
				for ya=1 to y
					if untransposed=0 then
						grid2(xb,ya)=grid1(xa,ya)
					else
						grid2(xa,ya)=grid1(xb,ya)
					end if			
				next ya
			next xa
			i=0
			for ya=1 to y
				for xa=1 to x
					if keepnulls=0 then
						if grid2(xa,ya)=123456789 then 
						else
							i+=1
							if i>l then exit for,for
							cstate(outstate,i)=grid2(xa,ya)
						end if	
					else
						i+=1
						if grid2(xa,ya)=123456789 then
							cstate(outstate,i)=32
						else
							cstate(outstate,i)=grid2(xa,ya)
						end if	
					end if
				next xa
			next ya
		
		case "Offset column order v2"
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			if dx>l then return "Error: "+lcase(operation)+" (DX > L)"
			if dy>l then return "Error: "+lcase(operation)+" (DY > L)"
			if dx*dy<l then return "Error: "+lcase(operation)+" (DX*DY < L)"
			dim as short a1=arg(7) 'offset
			dim as short a2=arg(8) 'column from
			dim as short a3=arg(9) 'column to
			if a2<1 or a2>dy then return "Error: "+lcase(operation)+" (A2)"
			if a3<1 or a3>dy then return "Error: "+lcase(operation)+" (A3)"
			if a2>a3 then return "Error: "+lcase(operation)+" (A2 > A3)"
			'if a1<1 or a1>(a3+1)-a2 then return "Error: "+lcase(operation)+" (A1)"
			if a1<1 or a1>dx then return "Error: "+lcase(operation)+" (A1)"	
			dim as short grid(dx,dy)
			dim as short tmpline(dx)
			for y=1 to dy
				for x=1 to dx
					i+=1
					grid(x,y)=cstate(instate,i)
					if i=l then exit for,for
				next x
			next y
			for y=a2 to a3	
				i=0
				for x=1 to dx
					if grid(x,y)>0 then
						i+=1
						tmpline(i)=grid(x,y)
					end if
				next x
				j=a1
				for k=1 to i
					j+=1
					if j>i then j=1
					grid(k,y)=tmpline(j)
				next k
			next y
			i=0
			for y=1 to dy
				for x=1 to dx
					if grid(x,y)>0 then
						i+=1 
						cstate(outstate,i)=grid(x,y)
						'if i=l then exit for,for
					end if
				next x
			next y
	
		case "Offset column"
			dim as integer l=arg(1)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer untransposed=arg(5)
			dim as integer a1=arg(7) 'column
			dim as integer a2=arg(8) 'offset
			if a1<-tx or a1>tx then return "Error: "+lcase(operation)+" (A1)"
			if a2<-ty or a2>ty then return "Error: "+lcase(operation)+" (A2)"
			dim as integer grid(tx,ty)
			dim as integer col(ty)
			dim as integer my
			for y=1 to ty
				for x=1 to tx
					i+=1
					grid(x,y)=cstate(instate,i)
					if i=l then exit for,for
				next x
			next y
			for y=1 to ty
				if grid(a1,y)>0 then
					my+=1
					col(my)=grid(a1,y)
				end if
			next y	
			if a2<0 then 
				i=ty+a2
			else
				i=a2
			end if
			for y=1 to my
				i+=1
				if i>my then i=1
				if untransposed=0 then
					col(i)=grid(a1,y)
				else
					col(y)=grid(a1,i)
				end if
			next y		
			for y=1 to ty
				grid(a1,y)=col(y)
			next y
			for y=1 to ty
				for x=1 to tx
					if grid(x,y)>0 then
						j+=1
						cstate(outstate,j)=grid(x,y)
						if j=l then exit for,for
					end if
				next x
			next y
			
		case "Offset row"
			dim as integer l=arg(1)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer untransposed=arg(5)
			dim as integer a1=arg(7) 'row
			dim as integer a2=arg(8) 'offset
			if a1<-tx or a1>ty then return "Error: "+lcase(operation)+" (A1)"
			if a2<-ty or a2>tx then return "Error: "+lcase(operation)+" (A2)"
			dim as integer grid(tx,ty)
			dim as integer row(tx)
			dim as integer mx
			for y=1 to ty
				for x=1 to tx
					i+=1
					grid(x,y)=cstate(instate,i)
					if i=l then exit for,for
				next x
			next y	
			for x=1 to tx
				if grid(x,a1)>0 then
					mx+=1
					row(mx)=grid(x,a1)
				end if
			next x	
			if a2<0 then 
				i=tx+a2
			else
				i=a2
			end if
			for x=1 to mx
				i+=1
				if i>mx then i=1
				if untransposed=0 then
					row(i)=grid(x,a1)
				else
					row(x)=grid(i,a1)
				end if
			next x
			for x=1 to tx
				grid(x,a1)=row(x)
			next x
			for y=1 to ty
				for x=1 to tx
					if grid(x,y)>0 then
						j+=1
						cstate(outstate,j)=grid(x,y)
						if j=l then exit for,for
					end if
				next x
			next y
			
		case "Offset rectangular chain"
			dim as integer l=arg(1)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer untransposed=arg(5)
			dim as integer x1=arg(7)
			dim as integer y1=arg(8)
			dim as integer x2=arg(9)
			dim as integer y2=arg(10)
			dim as integer o=arg(11)
			if x1<1 or x1>tx then return "Error: "+lcase(operation)+" (A1)"
			if y1<1 or y1>ty then return "Error: "+lcase(operation)+" (A2)"
			if x2<1 or x2>tx then return "Error: "+lcase(operation)+" (A3)"
			if y2<1 or y2>ty then return "Error: "+lcase(operation)+" (A4)"
			dim as integer grid(tx,ty)
			for y=1 to ty
				for x=1 to tx
					i+=1
					grid(x,y)=cstate(instate,i)
					if i=l then exit for
				next x
			next y
			dim as integer cl
			cl+=(x2-x1)*2
			cl+=(y2-y1)*2
			if cl=0 then cl=1
			if o<-cl or o>cl then return "Error: "+lcase(operation)+" (A5)"
			dim as integer rchain(cl)
			for x=x1 to x2 'grid to chain
				j+=1
				rchain(j)=grid(x,y1)
			next x
			for y=y1+1 to y2
				j+=1
				rchain(j)=grid(x2,y)
			next y
			for x=1 to x2-x1
				j+=1
				i=x2-x
				rchain(j)=grid(i,y2)	
			next x
			for y=1 to (y2-y1)-1
				j+=1
				i=y2-y
				rchain(j)=grid(x1,i)
			next y	
			if untransposed=0 then 
				o=(-o)
			end if	
			if o>0 then 
				j=o
			else
				j=cl+o
			end if	
			for x=x1 to x2 'chain to grid
				j+=1
				if j>cl then j=1	
				grid(x,y1)=rchain(j)
			next x
			for y=y1+1 to y2
				j+=1
				if j>cl then j=1
				grid(x2,y)=rchain(j)
			next y
			for x=1 to x2-x1
				j+=1
				if j>cl then j=1
				i=x2-x
				grid(i,y2)=rchain(j)
			next x
			for y=1 to (y2-y1)-1
				j+=1
				if j>cl then j=1
				i=y2-y
				grid(x1,i)=rchain(j)
			next y
			i=0
			for y=1 to ty
				for x=1 to tx
					if grid(x,y)>0 then
						i+=1
						cstate(outstate,i)=grid(x,y)
						if i=l then exit for
					end if
				next x
			next y
		
		case "Merge random characters"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'symbol
			dim as integer a2=arg(8) 'amount
			dim as short used(l)
			dim as short r1
			for i=1 to a2
				do
					r1=int(rnd*l)+1
				loop until used(r1)=0
				used(r1)=1
			next i
			for i=1 to l
				if used(i)=0 then
					cstate(outstate,i)=cstate(instate,i)
				else
					cstate(outstate,i)=a1
				end if
			next i
			return str(l)+","+str(s) 'nba incorrect
		
		case "Merge random symbols"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'symbol
			dim as integer a2=arg(8) 'amount
			dim as integer a3=arg(9) 'target freq
			if a1>identmax then return "Error: "+lcase(operation)+" (A1)"
			if a2<2 then return "Error: "+lcase(operation)+" (A2)"
			dim as short used(a2),best(a2)
			dim as integer frq1(identmax)
			dim as short r1
			dim as short e,t
			dim as integer b=999999999
			dim as integer loopmax
			for i=1 to l
				frq1(cstate(instate,i))+=1
			next i	
			for i=1 to 10000
				t=0
				for j=1 to a2
					e=0
					loopmax=0
					do
						loopmax+=1
						r1=cstate(instate,int(rnd*l)+1)
						for k=1 to j-1
							if used(r1)=used(r1) then
								e=1
								exit for
							end if
						next k
					loop until e=0 or loopmax=10000
					used(j)=r1
					t+=frq1(r1)
				next j
				if abs(t-a3)<b then
					b=abs(t-a3)
					for j=1 to a2
						best(j)=used(j)
					next j
				end if
				if t=a3 then exit for
			next i
			for i=1 to l
				e=0
				for j=1 to a2
					if cstate(instate,i)=best(j) then
						cstate(outstate,i)=a1
						e=1
						exit for
					end if
				next j
				if e=0 then cstate(outstate,i)=cstate(instate,i)
			next i
			return str(l)+","+str(s) 'nba incorrect
					
		case "Reverse column"
			dim as integer l=arg(1)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer untransposed=arg(5)
			dim as integer a1=arg(7) 'column
			if a1<1 or a1>tx then return "Error: "+lcase(operation)+" (A1)"
			dim as integer grid(tx,ty)
			for y=1 to ty
				for x=1 to tx
					i+=1
					grid(x,y)=cstate(instate,i)
					if i=l then exit for,for
				next x
			next y
			for y=1 to ty\2
				swap grid(a1,y),grid(a1,ty-(y-1))
			next y
			for y=1 to ty
				for x=1 to tx
					if grid(x,y)>0 then
						j+=1
						cstate(outstate,j)=grid(x,y)
						if j=l then exit for,for
					end if
				next x
			next y
		
		case "Reverse row"
			dim as integer l=arg(1)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer untransposed=arg(5)
			dim as integer a1=arg(7) 'row
			if a1<1 or a1>ty then return "Error: "+lcase(operation)+" (A1)"
			dim as integer grid(tx,ty)
			for y=1 to ty
				for x=1 to tx
					i+=1
					grid(x,y)=cstate(instate,i)
					if i=l then exit for,for
				next x
			next y
			for x=1 to tx\2
				swap grid(x,a1),grid(tx-(x-1),a1)
			next x
			for y=1 to ty
				for x=1 to tx
					if grid(x,y)>0 then
						j+=1
						cstate(outstate,j)=grid(x,y)
						if j=l then exit for,for
					end if
				next x
			next y
		
		case "Random nulls"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short a=arg(7) 'amount
			dim as short ps(a),e
			for i=1 to a
				do
					e=0
					ps(i)=int(rnd*l)+1
					for j=1 to i-1
						if ps(i)=ps(j) then
							e=1
							exit for
						end if
					next j
				loop until e=0
			next i
			k=0
			for i=1 to l
				e=0
				for j=1 to a
					if ps(j)=i then
						e=1
						exit for
					end if
				next j
				if e=0 then
					k+=1
					cstate(outstate,k)=cstate(instate,i)
				end if
			next i
		
		case "Random skips"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short a=arg(7) 'amount
			dim as short ps(a),e,sp1=s
			for i=1 to a
				ps(i)=int(rnd*(l+1))+1	
			next i
			k=0
			for i=1 to l+1
				e=0
				for j=1 to a
					if ps(j)=i then
						e=1
						exit for
					end if
				next j
				if e=1 then
					k+=1
					sp1+=1
					cstate(outstate,k)=sp1
				end if
				k+=1
				cstate(outstate,k)=cstate(instate,i)		
			next i
		
		case "Add character"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a=arg(7)
			if a<1 or a>l+1 then return "Error: "+lcase(operation)+" (A1)"
			for i=1 to l+1
				if i=a then 
					cstate(outstate,i)=s+1
				else
					j+=1
					cstate(outstate,i)=cstate(instate,j)
				end if
			next i
			return str(l+1)+","+str(s+1)
		
		case "Add characters"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7)
			dim as integer high=l
			dim as integer cip(constcip)
			for i=1 to a1
				cip(arg(7+i))=s+i
				if arg(7+i)>high then high=arg(7+i)
			next i
			for i=1 to high
				if cip(i)=0 andalso j<l then
					j+=1
					cip(i)=cstate(instate,j)
				end if
				if cip(i)>0 then
					k+=1
					cstate(outstate,k)=cip(i)
				end if
			next i
			return str(l+a1)+","+str(s+a1)
			
		case "Add random character"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer r1=int(rnd*(l+1))+1 
			for i=1 to l+1
				if i<>r1 then
					j+=1
					cstate(outstate,i)=cstate(instate,j)	
				else
					cstate(outstate,i)=s+1
				end if
			next i
			return str(l+1)+","+str(s+1)
		
		case "Add character periodic"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'from
			dim as integer a2=arg(8) 'to
			dim as integer a3=arg(9) 'step
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l+1 then return "Error: "+lcase(operation)+" (A2)"
			if a3>l then return "Error: "+lcase(operation)+" (A3)"
			if a3<1 then a3=1
			dim as integer posadd(l+1)
			dim as integer c
			for i=a1 to a2 step a3
				c+=1
				posadd(i)=1
			next i
			l+=c
			c=0
			for i=1 to l
				if posadd(i)=1 then
					c+=1			 
					cstate(outstate,i)=s+c
				else
					j+=1
					cstate(outstate,i)=cstate(instate,j)
				end if	
			next i
			return str(l)+","+str(s+c)
		
		case "Add row"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer a1=arg(7)
			if a1<1 or a1>ty+1 then return "Error: "+lcase(operation)+" (A1)"
			for y=1 to ty+1
				if a1=y then
					for x=1 to tx
						j+=1
				 		cstate(outstate,i+j)=s+j
					next x
				else
					for x=1 to tx
						i+=1
				 		cstate(outstate,i+j)=cstate(instate,i)
					next x
				end if
			next y
			return str(l+j)+","+str(s+j)
			
		case "Add row (using random symbols)"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer a1=arg(7)
			if a1<1 or a1>ty+1 then return "Error: "+lcase(operation)+" (A1)"
			dim as long cip(l)
			for i=1 to l
				cip(i)=cstate(instate,i)
			next i
			i=0
			for y=1 to ty+1
				if a1=y then 'random row
					for x=1 to tx
						j+=1
				 		'cstate(outstate,i+j)=s+j
				 		cstate(outstate,i+j)=cip(int(rnd*l)+1)
					next x
				else
					for x=1 to tx
						i+=1
				 		cstate(outstate,i+j)=cstate(instate,i)
					next x
				end if
			next y
			return str(l+j)+","+str(s)
		
		case "Add column"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer a1=arg(7)
			if a1<1 or a1>tx+1 then return "Error: "+lcase(operation)+" (A1)"
			for y=1 to ty
				for x=1 to tx+1
					if a1=x then
						j+=1
			 			cstate(outstate,i+j)=s+j
					else
			 			i+=1
			 			cstate(outstate,i+j)=cstate(instate,i)
					end if
				next x
			next y
			return str(l+j)+","+str(s+j)
			
		case "Add column (using random symbols)"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer a1=arg(7)
			if a1<1 or a1>tx+1 then return "Error: "+lcase(operation)+" (A1)"
			dim as long cip(l)
			for i=1 to l
				cip(i)=cstate(instate,i)
			next i
			i=0
			for y=1 to ty
				for x=1 to tx+1
					if a1=x then 'random column
						j+=1
						cstate(outstate,i+j)=cip(int(rnd*l)+1)
			 			'cstate(outstate,i+j)=s+j
					else
			 			i+=1
			 			cstate(outstate,i+j)=cstate(instate,i)
					end if
				next x
			next y
			return str(l+j)+","+str(s)
			
		case "Add null characters"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'amount
			if l+a1>constcip then return "Error: "+lcase(operation)+" (A1)"
			dim as integer used(l+a1),r1,c
			for i=1 to a1
				do
					r1=int(rnd*(l+a1))+1
				loop until used(r1)=0
				used(r1)=1
			next i
			for i=1 to l+a1
				if used(i)=0 then
					c+=1
					cstate(outstate,i)=cstate(instate,c)
				else
					cstate(outstate,i)=cstate(instate,int(rnd*l)+1)
				end if
			next i
			return str(l+a1)+","+str(s) 'nba incorrect
		
		case "Add null symbol"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'symbol
			dim as integer a2=arg(8) 'amount
			if a1>identmax then return "Error: "+lcase(operation)+" (A1)"
			if l+a2>constcip then return "Error: "+lcase(operation)+" (A2)"
			dim as integer used(l+a2),r1,c
			for i=1 to a2
				do
					r1=int(rnd*(l+a2))+1
				loop until used(r1)=0
				used(r1)=1
			next i
			for i=1 to l+a2
				if used(i)=0 then
					c+=1
					cstate(outstate,i)=cstate(instate,c)
				else
					cstate(outstate,i)=a1
				end if
			next i
			return str(l+a2)+","+str(s) 'nba incorrect
				
		case "Remove character"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7)
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			for i=1 to l
				if i=a1 then 
				else
					j+=1
					cstate(outstate,j)=cstate(instate,i)
				end if
			next i
			return str(l-1)+","+str(s) 'nba incorrect
		
		case "Remove characters"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7)
			dim as integer cip(constcip)
			for i=1 to a1
				cip(arg(7+i))=1
			next i
			for i=1 to l
				if cip(i)=0 then
					j+=1
					cstate(outstate,j)=cstate(instate,i)
				end if	
			next i		
			return str(l-a1)+","+str(s) 'nba incorrect
		
		case "Remove character periodic"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'from
			dim as integer a2=arg(8) 'to
			dim as integer a3=arg(9) 'step
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l then return "Error: "+lcase(operation)+" (A2)"
			if a3>l then return "Error: "+lcase(operation)+" (A3)"
			if a3<1 then a3=1
			dim as integer posrem(l)
			dim as integer c
			for i=a1 to a2 step a3
				c+=1
				posrem(i)=1
			next i
			for i=1 to l
				if posrem(i)=1 then
				else
					j+=1
					cstate(outstate,j)=cstate(instate,i)
				end if
			next i
			return str(l-c)+","+str(s) 'nba incorrect
	
		case "Remove symbol"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'symbol
			dim as integer c
			if a1<1 or a1>s then return "Error: "+lcase(operation)+" (A1)"
			for i=1 to l
				if cstate(instate,i)=a1 then
					c+=1
				else
					j+=1
					cstate(outstate,j)=cstate(instate,i)
				end if		
			next i
			l-=c
			return str(l)+","+str(s)
		
		case "Remove row"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer a1=arg(7)
			if a1<1 or a1>ty then return "Error: "+lcase(operation)+" (A1)"
			for y=1 to ty
				if a1=y then
					for x=1 to tx
						j+=1
					next x
				else
					for x=1 to tx
						i+=1
				 		cstate(outstate,i)=cstate(instate,i+j)
					next x
				end if
			next y
			return str(l-j)+","+str(s) 'nba incorrect
			
		case "Remove column"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer tx=arg(3)
			dim as integer ty=arg(4)
			dim as integer a1=arg(7)
			if a1<1 or a1>tx then return "Error: "+lcase(operation)+" (A1)"
			for y=1 to ty
				for x=1 to tx
					if a1=x then
						j+=1
					else
			 			i+=1
			 			cstate(outstate,i)=cstate(instate,i+j)
					end if
				next x			 
			next y
			return str(l-j)+","+str(s) 'nba incorrect
		
		case "Expand symbol"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a=arg(7)
			if a<1 or a>s then return "Error: "+lcase(operation)+" (A1)"
			dim as integer count
			for i=1 to l
				if cstate(instate,i)=a then
					if count=0 then
						cstate(outstate,i)=cstate(instate,i)
					else
						cstate(outstate,i)=s+count
					end if
					count+=1
				else
					cstate(outstate,i)=cstate(instate,i)
				end if
			next i
			return str(l)+","+str(s+(count-1))
						
		case "Expand character"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'position
			dim as integer frq(s)
			dim as integer e=s
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			for i=1 to l
				frq(cstate(instate,i))+=1
				cstate(outstate,i)=cstate(instate,i)
			next i
			i=a1
			if frq(cstate(instate,i))>1 then 'else keep number
				e+=1
				cstate(outstate,i)=e
				frq(cstate(instate,i))-=1
			end if
			return str(l)+","+str(s) 'nba incorrect
			
		case "Expand periodic"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'from
			dim as integer a2=arg(8) 'to
			dim as integer a3=arg(9) 'step
			dim as integer frq(s)
			dim as integer e=s
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l then return "Error: "+lcase(operation)+" (A2)"
			if a3>l then return "Error: "+lcase(operation)+" (A3)"
			if a3<1 then a3=1
			for i=1 to l
				frq(cstate(instate,i))+=1
				cstate(outstate,i)=cstate(instate,i)
			next i
			for i=a1 to a2 step a3
				if frq(cstate(instate,i))>1 then 'else keep number
					e+=1
					cstate(outstate,i)=e
					frq(cstate(instate,i))-=1
				end if
			next i
			return str(l)+","+str(s) 'nba incorrect
		
		case "Raise periodic" 'does not output nba
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'from
			dim as integer a2=arg(8) 'to
			dim as integer a3=arg(9) 'step
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l then return "Error: "+lcase(operation)+" (A2)"
			if a3>l then return "Error: "+lcase(operation)+" (A3)"
			if a3<1 then a3=1
			'dim as short acu
			for i=1 to l
				cstate(outstate,i)=cstate(instate,i)
			next i
			for i=a1 to a2 step a3
				'acu+=1
				cstate(outstate,i)+=s
			next i
			return str(l)+","+str(s) 'nba incorrect
			
		case "Raise unique bigrams"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as short id2(s,s,1)
			for i=1 to l step 2
				k+=2
				if i=l then exit for
				if id2(cstate(instate,i),cstate(instate,i+1),0)=0 then
					j+=1:id2(cstate(instate,i),cstate(instate,i+1),0)=j
					j+=1:id2(cstate(instate,i),cstate(instate,i+1),1)=j
				end if
				cstate(outstate,i)=id2(cstate(instate,i),cstate(instate,i+1),0)
				cstate(outstate,i+1)=id2(cstate(instate,i),cstate(instate,i+1),1)
			next i
			return str(k)+","+str(j)
				
		case "Replace periodic with random filler"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'from
			dim as integer a2=arg(8) 'to
			dim as integer a3=arg(9) 'step
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l then return "Error: "+lcase(operation)+" (A2)"
			if a3>l then return "Error: "+lcase(operation)+" (A3)"
			if a3<1 then a3=1
			for i=1 to l
				cstate(outstate,i)=cstate(instate,i)
			next i
			for i=a1 to a2 step a3
				cstate(outstate,i)=cstate(instate,int(rnd*l)+1)
			next i
			return str(l)+","+str(s) 'nba incorrect
			
		case "Randomize positions periodic"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'from
			dim as integer a2=arg(8) 'to
			dim as integer a3=arg(9) 'step
			dim as integer mem(l)
			dim as integer c
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l then return "Error: "+lcase(operation)+" (A2)"
			if a3>l then return "Error: "+lcase(operation)+" (A3)"
			if a3<1 then a3=1
			for i=1 to l
				cstate(outstate,i)=cstate(instate,i)
			next i
			for i=a1 to a2 step a3
				c+=1
				mem(c)=cstate(instate,i)
			next i
			for i=1 to c*c
				swap mem(int(rnd*c)+1),mem(int(rnd*c)+1)
			next i
			c=0
			for i=a1 to a2 step a3
				c+=1
				cstate(outstate,i)=mem(c)
			next i
			return str(l)+","+str(s) 'nba incorrect		
				
		case "Encode: homophonic substitution"
			
			'340 frequencies:
			'----------------
			'dim as double f340(340)
			'f340(1)=1
			'f340(2)=16
			'f340(3)=27
			'f340(4)=44
			'f340(5)=50
			'f340(6)=54
			'f340(7)=35
			'f340(8)=8
			'f340(9)=18
			'f340(10)=40
			'f340(11)=11
			'f340(12)=12
			'f340(24)=24
			
			'408 frequencies:
			'----------------
			'1: 1
			'3: 6
			'4: 8
			'5: 25
			'6: 66
			'7: 42
			'8: 88
			'9: 45
			'10: 40
			'11: 33
			'12: 24
			'14: 14
			'16: 16
			
			dim as integer l=arg(1)
			dim as integer ol=l
			dim as integer s=arg(2)
			dim as integer a1=arg(7) 'from
			dim as integer a2=arg(8) 'to
			dim as integer a3=arg(9) 'step
			if a3<1 then a3=1
			dim as integer ns=arg(10) 'to symbols
			dim as double frq_rnd=arg(11) '/100 'frequency randomnness
			dim as double seq_rnd=arg(12)/100 'sequential randomness
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l or a2<a1 then return "Error: "+lcase(operation)+" (A2)"
			if a3>(a2-a1)+1 then return "Error: "+lcase(operation)+" (A3)"
			if ns<1 or ns>=l then return "Error: "+lcase(operation)+" (A4)"
			dim as integer cip1(l),cip2(l),c,it
			dim as short nbaid(65536),idc
			for i=1 to l
				cstate(outstate,i)=cstate(instate,i)
			next i
			for i=a1 to a2 step a3
				if mark(i)=0 then
					c+=1
					if nbaid(cstate(instate,i))=0 then
						idc+=1
						cip1(c)=idc
						nbaid(cstate(instate,i))=idc
					else
						cip1(c)=nbaid(cstate(instate,i))
					end if
				end if
			next i
			s=idc
			if ns<=s then return "Error: "+lcase(operation)+" (A4)"
			l=c
			dim as long frq1(constcip)
			dim as long frq2(constcip)
			'dim as double ff(constcip) 'chi
			dim as double best_rawioc=999999999
			'dim as double seq_rnd2=seq_rnd
			for i=1 to l
				frq1(cip1(i))+=1
			next i
			'dim as short cyca(constcip) 'periodic
			'dim as short cycb(constcip) 'periodic
			'dim as short cycc(constcip) 'anti
			dim as integer cycle(s),pm(s),slf
			dim as integer smap(s,l)
			dim as integer r1,r2,r3,r4
			dim as integer a,io
			for i=1 to s
				smap(i,0)=1
				smap(i,1)=i
			next i
			for i=s+1 to ns
				do
					a=int(rnd*s)+1
				loop until (smap(a,0)+1)<=frq1(a)
				smap(a,0)+=1
				smap(a,smap(a,0))=i
			next i
			
			dim as integer rh=l*seq_rnd 'random homophones
			dim as byte rnd_map(l)
			if rh>=l then 
				rh=l
				for i=1 to l
					rnd_map(i)=1
				next i
			else
				for i=1 to rh
					do
						j=1+(rnd*l)
					loop until rnd_map(j)=0
					rnd_map(j)=1
				next i
			end if
			
			'dim as double temp=0.2
			'dim as double tempm=temp/20000
			
			for it=1 to 20000 'make change '20000
				do
					r1=int(rnd*s)+1
				loop until smap(r1,0)>1
				r2=int(rnd*smap(r1,0))+1
				do
					r3=int(rnd*s)+1
				loop until smap(r3,0)<frq1(r3)
				smap(r3,0)+=1
				smap(r3,smap(r3,0))=smap(r1,smap(r1,0))
				smap(r1,smap(r1,0))=0
				smap(r1,0)-=1
				erase frq2
				for i=1 to s
					'cyca(i)=0 'periodic
					'cycb(i)=1 'periodic
					cycle(i)=0
					pm(i)=0
				next i
				
				'for i=1 to l 'anti-cycles
				'	cycc(i)=0
				'next i
				
				'for i=1 to s
				'	smap(i,0)=1
				'	smap(i,1)=i
				'next i
				dim as double sr,opti,ratio=l/ns
				
				'for i=s+1 to ns
				'	do
				'		a=int(rnd*s)+1
				'	loop until (smap(a,0)+1)<=frq1(a)
				'	smap(a,0)+=1
				'	smap(a,smap(a,0))=i
				'next i	
				'dim as short cp=2 'periodic
				'seq_rnd=seq_rnd2
				
				for i=1 to l
					io=cip1(i)
					'seq_rnd+=(25/l)/100
					
					'if rnd>seq_rnd then 'old
					
					if rnd_map(i)=0 then 'sequential
					
					'if rnd>seq_rnd*i/l then '(0 to xx%)
						
						'periodic cycles
						'---------------
						'if cyca(io)>smap(io,0) then 
						'	cyca(io)=0
						'	cycb(io)+=1
						'	if cycb(io)>smap(io,0) then cycb(io)=1
						'	cycle(io)=cycb(io)
						'end if
						'cycle(io)+=cp
						'do
						'	if cycle(io)>smap(io,0) then cycle(io)=cycle(io)-smap(io,0)
						'loop until cycle(io)<=smap(io,0)
						'cyca(io)+=1
						
						'normal cycles
						'-------------
						cycle(io)+=1
						if cycle(io)>smap(io,0) then cycle(io)=1
						
						'homophone group l-to-r, r-to-l shift cycles
						'-------------
						'if smap(io,0)>1 then
						'	if pm(io)=0 then cycle(io)+=1
						'	if pm(io)=1 then cycle(io)-=1
						'	if cycle(io)=smap(io,0) then pm(io)=1
						'	if pm(io)=1 andalso cycle(io)=1 then pm(io)=0
						'else
						'	cycle(io)=1
						'end if
						
						'homophone group random shift cycles
						'-------------
						'if smap(io,0)>1 then
						'	'do
						'		'e=0
						'		slf=int(rnd*2)
						'		'if slf=0 andalso cycle(io)+1>smap(io,0) then e=1
						'		'if slf=1 andalso cycle(io)-1<1 then e=1
						'	'loop until e=0
						'	if slf=0 then cycle(io)+=1
						'	if slf=1 then cycle(io)-=1
						'	if cycle(io)<1 then cycle(io)=smap(io,0)
						'	if cycle(io)>smap(io,0) then cycle(io)=1
						'else
						'	cycle(io)=1
						'end if
						
						'anti cycles
						'-----------
						'cycc(io)+=1
						'if frac((smap(io,0)/frq1(io))*cycc(io))>0 then
						'	cycle(io)=((smap(io,0)/frq1(io))*cycc(io))+1
						'	if cycle(io)>smap(io,0) then cycle(io)=smap(io,0)
						'else
						'	cycle(io)=(smap(io,0)/frq1(io))*cycc(io)
						'	if cycle(io)>smap(io,0) then cycle(io)=smap(io,0)
						'	if cycle(io)>1 then cycle(io)=1
						'end if
						
					else 'random
						
						cycle(io)=int(rnd*smap(io,0))+1 'random homophone
						
						'homophone group random shift cycles
						'-------------
						'if smap(io,0)>1 then
						'	do
						'		e=0
						'		slf=int(rnd*2)
						'		if slf=0 andalso cycle(io)+1>smap(io,0) then e=1
						'		if slf=1 andalso cycle(io)-1<1 then e=1
						'	loop until e=0
						'	if slf=0 then cycle(io)+=1
						'	if slf=1 then cycle(io)-=1
						'	if cycle(io)=smap(io,0) then pm(io)=1
						'	if pm(io)=1 andalso cycle(io)=1 then pm(io)=0
						'else
						'	cycle(io)=1
						'end if
						
					end if
					cip2(i)=smap(io,cycle(io))
				next i
				
				dim as integer rawioc=0
				for i=1 to l
					frq2(cip2(i))+=1
				next i
				for i=1 to ns
					if frq2(i)>1 then
						rawioc+=frq2(i)*(frq2(i)-1)
					end if
				next i
				
				'erase frq2,ff
				'dim as double diff=0
				'for i=1 to l
				'	frq2(cip2(i))+=1
				'next i
				'for i=1 to l
				'	ff(frq2(cip2(i)))+=1
				'next i
				'for i=1 to l
				'	diff+=abs(f340(i)-ff(i))
				'next i
				
				'if abs(frq_rnd-rawioc)+diff<best_rawioc then 'keep change
				if abs(frq_rnd-rawioc)<best_rawioc then 'keep change
					'best_rawioc=abs(frq_rnd-rawioc)+diff
					best_rawioc=abs(frq_rnd-rawioc)
					c=0
					for i=a1 to a2 step a3
						if mark(i)=0 then
							c+=1
							cstate(outstate,i)=cip2(c)
						end if
					next i	
				else 'reverse change
					'best_rawioc-=temp
					smap(r1,0)+=1
					smap(r1,smap(r1,0))=smap(r3,smap(r3,0))
					smap(r3,smap(r3,0))=0
					smap(r3,0)-=1		
				end if
				
				'temp-=tempm
				
				if frq_rnd=rawioc then exit for
				'if diff=0 then exit for
				
			next it
			return str(ol)+","+str(ns) 'nba incorrect
			
		case "Encode: homophonic substitution (no repeat window)"
			dim as short l=arg(1)
			dim as short s=arg(2)
			'dim as integer a1=arg(7) 'from
			'dim as integer a2=arg(8) 'to
			'dim as integer a3=arg(9) 'step
			dim as short ns=arg(7) 'symbols
			dim as short nrws=arg(8) 'no repeat window size
			dim as integer rawioctarget=arg(9) 'raw ioc target 
			dim as short frq0(s),frq1(ns)
			dim as short smap(s,l),mem(l)
			dim as short out0(l),out1(l)
			dim as short it,e,r,rmax=100
			dim as integer rawioc,best=999999999
			for i=1 to l
				frq0(cstate(instate,i))+=1
			next i
			for it=1 to 1000
				for i=0 to nrws-1
					mem(i)=0
				next i
				for i=1 to s
					smap(i,0)=0
				next i
				for i=1 to s
					smap(i,0)+=1
					smap(i,1)=i
				next i
				for i=s+1 to ns
					do
						j=int(rnd*s)+1
					loop until smap(j,0)<frq0(j)
					smap(j,0)+=1
					smap(j,smap(j,0))=i
				next i
				for i=1 to l
					r=0
					do
						e=0
						r+=1
						j=smap(cstate(instate,i),int(rnd*smap(cstate(instate,i),0))+1)
						for k=0 to nrws-1
							if mem(k)=j then 
								e=1
								exit for
							end if
						next k
					loop until e=0 or r=rmax
					out0(i)=j
					mem(i mod nrws)=j
				next i
				for i=1 to ns
					frq1(i)=0
				next i
				for i=1 to l
					frq1(out0(i))+=1
				next i
				rawioc=0
				for i=1 to ns
					if frq1(i)>1 then
						rawioc+=frq1(i)*(frq1(i)-1)
					end if
				next i
				if abs(rawioc-rawioctarget)<best then
					best=abs(rawioc-rawioctarget)
					for i=1 to l
						out1(i)=out0(i)
					next i
				end if
			next it
			for i=1 to l
				cstate(outstate,i)=out1(i)
			next i
			return str(l)+","+str(ns) 'nba incorrect
		
		case "Encode: vigen�re"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short a1=arg(7) 'from
			dim as short a2=arg(8) 'to
			dim as short a3=arg(9) 'step
			if a3<1 then a3=1
			if a1<1 or a1>l then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>l or a2<a1 then return "Error: "+lcase(operation)+" (A2)"
			if a3>(a2-a1)+1 then return "Error: "+lcase(operation)+" (A3)"
			dim as short kl=arg(10) 'keyword length
			dim as short kw(kl),cl
			for i=1 to kl
				kw(i)=arg(10+i)
			next i
			for i=1 to l
				cstate(outstate,i)=cstate(instate,i)
			next i
			for i=a1 to a2 step a3
				select case cstate(instate,i)		
					case 65 to 90 'ucase
						cl+=1
						if cl>kl then cl=1
						if kw(cl)>64 andalso kw(cl)<91 then
							cstate(outstate,i)=(((cstate(instate,i)-65)+(kw(cl)-65))mod 26)+65
						else
							cstate(outstate,i)=cstate(instate,i)
						end if
					case 97 to 122 'lcase
						cl+=1
						if cl>kl then cl=1
						if kw(cl)>96 andalso kw(cl)<123 then
							cstate(outstate,i)=(((cstate(instate,i)-97)+(kw(cl)-97))mod 26)+97
						else
							cstate(outstate,i)=cstate(instate,i)
						end if
					case else
						cstate(outstate,i)=cstate(instate,i)
				end select
			next i
			return str(l)+","+str(s) 'nba incorrect
		
		case "Encode: caesar shift"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short fi1=arg(7)	'from
			dim as short fi2=arg(8) 'to
			dim as short stp=arg(9) 'step
			dim as short csh=arg(10) 'caesar shift
			if fi1<1 or fi1>l then return "Error: "+lcase(operation)+" (A1)"
			if fi2<1 or fi2>l or fi2<fi1 then return "Error: "+lcase(operation)+" (A2)"
			if stp>(fi2-fi1)-1 then return "Error: "+lcase(operation)+" (A3)"
			if stp=0 then stp=1
			if csh>26 then return "Error: "+lcase(operation)+" (A4)"
			for i=1 to l
				if cstate(instate,i)<65 or cstate(instate,i)>90 then
					return "Error: "+lcase(operation)+" (input contains non-standard english letters)"
				end if
			next i
			dim as short a
			for i=1 to l
				cstate(outstate,i)=cstate(instate,i)
			next i
			for i=fi1 to fi2 step stp
				a=cstate(instate,i)+csh
				if a>90 then a=64+(a-90)
				cstate(outstate,i)=a	
			next i
			return str(l)+","+str(s) 'nba incorrect
			
		case "Encode: digraph substitution"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short kl1=arg(7) 'first letter keyword length
			dim as short kl2=arg(8) 'second letter keyword length
			if kl1>26 then return "Error: "+lcase(operation)+" (A1: keyword length > alphabet size)"
			if kl2>26 then return "Error: "+lcase(operation)+" (A2: keyword length > alphabet size)"
			dim as short kw1(kl1)
			dim as short kw2(kl2)
			dim as short kwx(25)
			dim as short kwy(25)
			dim as short memx(25)
			dim as short memy(25)
			dim as short a,b
			for i=1 to l
				if cstate(instate,i)<65 or cstate(instate,i)>90 then
					return "Error: "+lcase(operation)+" (input contains non-standard english letters)"
				end if
			next i
			for i=1 to kl1 'x
				kw1(i)=arg(8+i)-65
			next i
			for i=1 to kl2 'y
				kw2(i)=arg(8+kl1+i)-65
			next i
			for i=0 to 25
				if i+1<=kl1 andalso kw1(i+1)>0 then 
					kwx(i)=kw1(i+1)
					memx(kwx(i))=1
				else
					for j=0 to 25 'next memx
						if memx(j)=0 then
							kwx(i)=j
							memx(j)=1
							exit for
						end if
					next j
				end if
				if i+1<=kl2 andalso kw2(i+1)>0 then 
					kwy(i)=kw2(i+1)
					memy(kwy(i))=1
				else
					for j=0 to 25 'next memy
						if memy(j)=0 then
							kwy(i)=j
							memy(j)=1
							exit for
						end if
					next j
				end if			
			next i
			dim as short dg(25,25,1)
			dim as string dgs="    "
			for x=0 to 25
				dgs+=chr(x+97)
				if x<>25 then dgs+="  "
			next x
			dgs+=lb
			dgs+=lb
			for y=0 to 25
				for x=0 to 25
					dg(x,y,0)=kwx((x+y)mod 26)
					dg(x,y,1)=kwy(x)
					'---------------------------------------------
					if x=0 then dgs+=chr(y+97)+"   "
					dgs+=chr(dg(x,y,0)+65)+chr(dg(x,y,1)+65)
					if x<>25 then
						dgs+=" "
					else
						if y<>25 then dgs+=lb
					end if
				next x
			next y	
			for i=1 to l step 2
				if i+1>l then
					cstate(outstate,i)=0
					cstate(outstate,i+1)=0
					exit for
				end if
				a=cstate(instate,i)-65
				b=cstate(instate,i+1)-65
				cstate(outstate,i)=dg(a,b,0)+65
				cstate(outstate,i+1)=dg(a,b,1)+65	
			next i	
			ui_editbox_settext(output_text,dgs)
			return str(l)+","+str(s) 'nba incorrect
			
		case "Generate numbers"
			dim as integer a1=arg(7) 'from
			dim as integer a2=arg(8) 'to
			dim as integer a3=arg(9) 'step
			if a1<1 or a1>65536 then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>65536 then return "Error: "+lcase(operation)+" (A2)"
			if a3<0 or a3>65536 then return "Error: "+lcase(operation)+" (A3)"
			if a3=0 then a3=1
			if (a2-a1)+1>constcip then return "Error: "+lcase(operation)+" (A1 to A2)"
			for i=a1 to a2 step a3
				j+=1
				cstate(outstate,j)=i
			next i
			return str(j)+","+str(j)
		
		case "Generate random numbers"
			dim as integer a1=arg(7) 'amount
			dim as integer a2=arg(8) 'range: from
			dim as integer a3=arg(9) 'range: to
			if a1>constcip then return "Error: "+lcase(operation)+" (A1)"
			if a3<=a2 then return "Error: "+lcase(operation)+" (A3) >= (A2)"
			for i=1 to a1
				cstate(outstate,i)=int(rnd*(a3-(a2-1)))+a2
			next i
			return str(a1)+","+str(a1) 'nba incorrect
			
		case "Randomize"
			dim as integer l=arg(1)
			dim as integer tmp(l)
			for i=1 to l
				tmp(i)=i
			next i
			for i=1 to l*5
				swap tmp(int(rnd*l)+1),tmp(int(rnd*l)+1)
			next i
			for i=1 to l
				cstate(outstate,tmp(i))=cstate(instate,i)
			next i
			
		case "Randomize row"
			dim as integer l=arg(1)
			dim as integer dx=arg(3)
			dim as integer dy=arg(4)
			dim as integer a1=arg(7)
			if a1<1 or a1>dy then return "Error: "+lcase(operation)+" (A1)"
			dim as integer grid(dx,dy)
			for y=1 to dy
				for x=1 to dx
					i+=1
					grid(x,y)=cstate(instate,i)			
				next x
			next y
			for x=1 to dx^3
				swap grid(int(rnd*dx)+1,a1),grid(int(rnd*dx)+1,a1)
			next x
			i=0
			for y=1 to dy
				for x=1 to dx
					i+=1
					cstate(outstate,i)=grid(x,y)
				next x
			next y			
			
		case "Randomize column"
			dim as integer l=arg(1)
			dim as integer dx=arg(3)
			dim as integer dy=arg(4)
			dim as integer a1=arg(7)
			if a1<1 or a1>dx then return "Error: "+lcase(operation)+" (A1)"
			dim as integer grid(dx,dy)
			for y=1 to dy
				for x=1 to dx
					i+=1
					grid(x,y)=cstate(instate,i)
				next x
			next y	 
			for y=1 to dy^3
				swap grid(a1,int(rnd*dy)+1),grid(a1,int(rnd*dy)+1)
			next y
			i=0
			for y=1 to dy
				for x=1 to dx
					i+=1
					cstate(outstate,i)=grid(x,y)			
				next x
			next y	 
			
		case "Randomize column order"
			dim as integer l=arg(1)
			dim as integer dx=arg(3)
			dim as integer dy=arg(4)
			dim as integer grid1(dx,dy)
			dim as integer grid2(dx,dy)
			dim as integer rndcol(dx)	
			for i=1 to dx
				rndcol(i)=i
			next i
			for i=1 to dx*dx
				swap rndcol(int(rnd*dx)+1),rndcol(int(rnd*dx)+1)
			next i
			i=0
			for y=1 to dy
				for x=1 to dx
					i+=1
					grid1(x,y)=cstate(instate,i)
					if i=l then exit for,for	
				next x
			next y
			for x=1 to dx
				for y=1 to dy
					grid2(x,y)=grid1(rndcol(x),y)
				next y
			next x
			for y=1 to dy
				for x=1 to dx
					if grid2(x,y)>0 then
						j+=1
						cstate(outstate,j)=grid2(x,y)
					end if
				next x
			next y
		
		case "Randomize row order"
			dim as integer l=arg(1)
			dim as integer dx=arg(3)
			dim as integer dy=arg(4)
			dim as integer grid1(dx,dy)
			dim as integer grid2(dx,dy)
			dim as integer rndrow(dy)	
			for i=1 to dy
				rndrow(i)=i
			next i
			for i=1 to dy*dy
				swap rndrow(int(rnd*dy)+1),rndrow(int(rnd*dy)+1)
			next i
			i=0
			for y=1 to dy
				for x=1 to dx
					i+=1
					grid1(x,y)=cstate(instate,i)
					if i=l then exit for,for	
				next x
			next y
			for y=1 to dy
				for x=1 to dx
					grid2(x,y)=grid1(x,rndrow(y))
				next x
			next y
			for y=1 to dy
				for x=1 to dx
					if grid2(x,y)>0 then
						j+=1
						cstate(outstate,j)=grid2(x,y)
					end if
				next x
			next y
			
		case "Randomize characters"
			dim as integer l=arg(1)
			dim as integer s=arg(2)
			dim as integer a1=arg(7)
			dim as integer cip(l)
			dim as integer id(l)
			dim as integer r1,r2
			for i=1 to l
				cip(i)=cstate(instate,i)
			next i
			for i=1 to a1
				do
					r1=int(rnd*l)+1
				loop until id(r1)=0
				id(r1)=1
				do
					r2=int(rnd*l)+1
				loop until cip(r2)<>cip(r1)
				cip(r1)=cip(r2)
			next i
			for i=1 to l
				cstate(outstate,i)=cip(i)
			next i
			return str(l)+","+str(s)
			
		case "Rearrange columns"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			dim as short utp=arg(5)
			for i=1 to dx
				if extarg(i)=0 or extarg(i)>dx then return "Error: "+lcase(operation)+" (Invalid arguments)"
			next i
			dim as short grid(dx,dy)
			i=0
			for y=1 to dy
				for x=1 to dx
					i+=1
				 	grid(x,y)=cstate(instate,i)
				 	if i=l then exit for,for
				next x
			next y
			dim as short xx
			dim as short grid2(dx,dy)
			for x=1 to dx
				xx=extarg(x)
				for y=1 to dy
					if utp=0 then 'transpose
						grid2(xx,y)=grid(x,y)
					else 'untranspose
						grid2(x,y)=grid(xx,y)
					end if
				next y
			next x
			i=0
			for y=1 to dy
				for x=1 to dx
					if grid2(x,y)>0 then
						i+=1
						cstate(outstate,i)=grid2(x,y)
						if i=l then exit for,for
					end if
				next x
			next y
			return str(l)+","+str(s)
		
		case "Rearrange rows"
			dim as short l=arg(1)
			dim as short s=arg(2)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			dim as short utp=arg(5)
			for i=1 to dy
				if extarg(i)=0 or extarg(i)>dy then return "Error: "+lcase(operation)+" (Invalid arguments)"
			next i
			dim as short grid(dx,dy)
			i=0
			for y=1 to dy
				for x=1 to dx
					i+=1
				 	grid(x,y)=cstate(instate,i)
				 	if i=l then exit for,for
				next x
			next y
			dim as short yy
			dim as short grid2(dx,dy)
			for y=1 to dy
				yy=extarg(y)
				for x=1 to dx
					if utp=0 then 'transpose
						grid2(x,yy)=grid(x,y)
					else 'untranspose
						grid2(x,y)=grid(x,yy)
					end if
				next x
			next y
			i=0
			for y=1 to dy
				for x=1 to dx
					if grid2(x,y)>0 then
						i+=1
						cstate(outstate,i)=grid2(x,y)
						if i=l then exit for,for
					end if
				next x
			next y
			return str(l)+","+str(s)
		
		case "Swap row"
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			dim as short a1=arg(7)
			dim as short a2=arg(8)
			if a1<1 or a1>dy then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>dy then return "Error: "+lcase(operation)+" (A2)"
			if a1=a2 then return "Error: "+lcase(operation)+" (A1 = A2)"
			dim as short grid(dx,dy)
			i=0:j=0
			for y=1 to dy
				for x=1 to dx
					i+=1
					if i>l then grid(x,y)=0 else grid(x,y)=cstate(instate,i)
				next x
			next y
			for x=1 to dx
				swap grid(x,a1),grid(x,a2)
			next x
			for y=1 to dy
				for x=1 to dx
					if grid(x,y)>0 then
						j+=1
						cstate(outstate,j)=grid(x,y)
					end if
				next x
			next y
			
		case "Swap column"
			dim as short l=arg(1)
			dim as short dx=arg(3)
			dim as short dy=arg(4)
			dim as short a1=arg(7)
			dim as short a2=arg(8)
			if a1<1 or a1>dy then return "Error: "+lcase(operation)+" (A1)"
			if a2<1 or a2>dy then return "Error: "+lcase(operation)+" (A2)"
			if a1=a2 then return "Error: "+lcase(operation)+" (A1 = A2)"
			dim as short grid(dx,dy)
			i=0:j=0
			for y=1 to dy
				for x=1 to dx
					i+=1
					if i>l then grid(x,y)=0 else grid(x,y)=cstate(instate,i)
				next x
			next y
			for y=1 to dy
				swap grid(a1,y),grid(a2,y)
			next y
			for y=1 to dy
				for x=1 to dx
					if grid(x,y)>0 then
						j+=1
						cstate(outstate,j)=grid(x,y)
					end if
				next x
			next y
				
		case else
			return "Error: operation does not exist"
		
	end select
	
	return "Ok"

end function

sub thread_batch_ngrams_substitution(byval none as any ptr)
	
	dim as integer e,i,j,k,lang,item_i,a,b
	dim as string os,os2
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer cip(l),ciprnd(l)
	dim as double avg1,avg2
	dim as string langfiles(10000),li
	dim as integer languages,old_solvesub_outputdir
	dim as integer rand_i,rand_i_max
	dim as integer norm_i,norm_i_max
	dim as string correl(2,10000)
	dim as string old_solvesub_ngramloc=solvesub_ngramloc
	
	for i=1 to threads
		thread(i).combine_output=1
	next i
	
	task_active="batch n-grams (substitution)"
	update_solver_status
	
	dim as string oldfilter=filter
	filter="Text files (*.bhd)"+chr(0)+"*.bhd*"
	dim as string filename=ui_loadsavedialog(0,"Open n-gram batch list",filter,1,basedir+"\N-grams\Languages\")
	filter=oldfilter
	
	if fileexists(filename)=0 then
		if filename<>"" then ui_editbox_settext(output_text,"Error: file not found")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	dim as string basedir2=left(filename,instrrev(filename,"\"))
	
	for i=1 to l
		cip(i)=nuba(i)
		ciprnd(i)=nuba(i)
	next i
	
	redim combine_score(solvesub_batchngramsrestarts*2)

	open filename for binary as #1
	do
		line input #1,li
		if fileexists(li)=0 then
			ui_editbox_settext(output_text,"N-gram file not found: "+str(li))
			close #1
			task_active="none"
			update_solver_status
			redim combine(0)
			exit sub
		end if
		languages+=1
		langfiles(languages)=li
	loop until eof(1)
	close #1
	
	loadngrams_showmsg=0
	old_solvesub_outputdir=solvesub_outputdir
	solvesub_outputdir=0 'do not output ciphers
	ips_timer=timer
	sectimer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	solvesub_temperature=400
	
	rand_i_max=solvesub_batchngramsrestarts
	norm_i_max=solvesub_batchngramsrestarts
	
	os+="BHdecrypt batch n-grams for: "+file_name+lb
	os+="---------------------------------------------------------"+lb
	os+="N-gram files with larger (alphabets) may exhibit "+lb
	os+="inflated correlation percentages."+lb
	ui_editbox_settext(output_text,os)
	
	for lang=1 to languages 'languages
	
		'ui_listbox_setcursel(list_main,0)
		set_solverhighlight("substitution")
		toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",4,1,threads) 'stop solver
		toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",2,1,threads) 'stop thread
		
		for i=1 to threads
			thread(i).avgscore=0
			thread(i).avgioc=0
			thread(i).avgpccycles=0
		next i
		
		task_active="loading n-grams"
		update_solver_status
		solvesub_ngramloctemp=basedir2+langfiles(lang)
		thread_ptr(threadsmax+3)=threadcreate(@thread_load_ngrams,0)
		
		do
			sleep 0.001
		loop until task_active="none"
		
		'#include "ngrams_output_binary.bi" 'batch output to binary (keep)
		
		'ui_listbox_setcursel(list_main,0)
		set_solverhighlight("substitution")
		toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",1,1,threads) 'start thread	
		
		task_active="batch n-grams"
		solver_status_processing=1
		ips_timer=timer
		
		for i=1 to rand_i_max+norm_i_max
			combine_score(i)=0
		next i
		
		item_i=0
		rand_i=0
		norm_i=0
		
		do 'do randomizations
			sleep twait
			for i=1 to threads
				
				if pausetask=1 then 'pause task
					update_solver_status
					do
						sleep 10
					loop until pausetask=0
					update_solver_status
				end if
				
				if thread(i).solver_waiting=1 then
					item_i+=1
					rand_i+=1
					for j=1 to l
						swap ciprnd(int(rnd*l)+1),ciprnd(int(rnd*l)+1)
					next j
					thread(i).outputdir=basedir+"\Output\"
					thread(i).itemnumber=item_i
					thread(i).l=l
					thread(i).s=s
					thread(i).dim_x=dx
					thread(i).dim_y=dy
					thread(i).score=0
					thread(i).advstats=0
					'thread(i).combine_output=1
					thread(i).iterations=solvesub_iterations
					thread(i).temperature=solvesub_temperature
					thread(i).restarts=solvesub_restarts
					thread(i).subrestartlevels=solvesub_subrestartlevels
					thread(i).ngramfactor=solvesub_ngramfactor
					thread(i).multiplicityweight=solvesub_multiplicityweight
					thread(i).entweight=solvesub_entweight
					thread(i).solver_stop=0
					for j=1 to l
						thread(i).cip(j)=ciprnd(j)
					next j
					thread(i).update=0
					thread(i).solver_waiting=0 'engage thread
					if timer-statustimer>1.1 then
						statustimer=timer
						update_solver_status
					end if
					if rand_i=rand_i_max then exit do
				end if
			next i
			if stoptask=1 then exit for
		loop
		
		do 'do normals
			sleep twait
			for i=1 to threads
				
				if pausetask=1 then 'pause task
					update_solver_status
					do
						sleep 10
					loop until pausetask=0
					update_solver_status
				end if
				
				if thread(i).solver_waiting=1 then
					item_i+=1
					norm_i+=1
					thread(i).outputdir=basedir+"\Output\"
					thread(i).itemnumber=item_i
					thread(i).l=l
					thread(i).s=s
					thread(i).dim_x=dx
					thread(i).dim_y=dy
					thread(i).score=0
					thread(i).advstats=0
					'thread(i).combine_output=1
					thread(i).iterations=solvesub_iterations
					thread(i).temperature=solvesub_temperature
					thread(i).restarts=solvesub_restarts
					thread(i).subrestartlevels=solvesub_subrestartlevels
					thread(i).ngramfactor=solvesub_ngramfactor
					thread(i).multiplicityweight=solvesub_multiplicityweight
					thread(i).entweight=solvesub_entweight
					thread(i).solver_stop=0
					for j=1 to l
						thread(i).cip(j)=cip(j)
					next j
					thread(i).update=0
					thread(i).solver_waiting=0 'engage thread
					if timer-statustimer>1.1 then
						statustimer=timer
						update_solver_status
					end if
					if norm_i=norm_i_max then exit do
				end if
			next i
			if stoptask=1 then exit for
		loop
		
		for i=1 to threads
			do
				sleep 0.001
			loop until thread(i).solver_waiting=1
		next i
		
		avg1=0 'calculate and output difference
		dim as integer avg1_cnt=0
		for i=1 to rand_i_max
			if combine_score(i)>0 then
				avg1_cnt+=1
				avg1+=combine_score(i)
			end if
		next i
		avg2=0
		for i=rand_i_max+1 to rand_i_max+norm_i_max
			if combine_score(i)>avg2 then avg2=combine_score(i)
		next i
		for i=1 to rand_i_max+norm_i_max
			combine_score(i)=0
		next i
		
		a=instr(langfiles(lang),"_")
		b=instr(a+1,langfiles(lang),"_")
		correl(0,lang)=mid(langfiles(lang),a+1,(b-a)-1)
		correl(1,lang)=format((avg2/(avg1/avg1_cnt))*100,"0.00")
		correl(2,lang)=str(ngram_alphabet_size)
		
		do
			e=0
			for i=1 to lang-1
				if val(correl(1,i))<val(correl(1,i+1)) then
					e=1
					swap correl(0,i),correl(0,i+1)
					swap correl(1,i),correl(1,i+1)
					swap correl(2,i),correl(2,i+1)
				end if	
			next i
		loop until e=0
		os2=""
		for i=1 to lang
			os2+=lb+correl(0,i)+": "+correl(1,i)+"% ("+correl(2,i)+")"
		next i
		
		ui_editbox_settext(output_text,os+os2)
	
		'if stoptask=1 then exit for
		
	next lang
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1 'commented previously
		do
			sleep 10
		loop until thread(i).solver_waiting=1 'or timer-stucktimer>2
		thread(i).combine_output=0
	next i
	
	sleep 10
	
	solvesub_ngramloctemp=old_solvesub_ngramloc
	thread_ptr(threadsmax+3)=threadcreate(@thread_load_ngrams,0)
	do
		sleep 10
	loop until task_active="none"
	
	solvesub_outputdir=old_solvesub_outputdir
	loadngrams_showmsg=1
	solver_status_processing=0
	clean_thread_information
	task_active="none"
	update_solver_status
	
	if stoptask=0 andalso solvesub_batchshutdown=1 then shell ("SHUTDOWN /s /t 10 ")
	stoptask=0
	close #1 'test
	
end sub

sub thread_solve_simpletransposition(byval none as any ptr)
	
	dim as integer ss1=val(ui_editbox_gettext(editbox_transpositionsolver_stacksize))
	dim as integer bd1=val(ui_editbox_gettext(editbox_transpositionsolver_searchstates))
	dim as integer br1=val(ui_editbox_gettext(editbox_transpositionsolver_batchciphersrestarts))
	
	if ss1<1 then ss1=1
	if ss1>5 then ss1=5
	
	if bd1<1 then bd1=1
	if bd1>1000000000 then bd1=1000000000
	
	if br1<1 then br1=1
	if br1>1000000000 then br1=1000000000
	
	dim as integer g,h,i,j,k,x,y,e,r,f,t,m,d
	dim as integer iterations=solvesub_iterations
	dim as integer bigramdepth=bd1 'solvesub_bigramdepth
	dim as integer stacksize=ss1 'solvesub_transstack
	dim as double iterationsfactor=solvesub_iterationsfactor
	dim as double hciterationsfactor=solvesub_hciterationsfactor
	dim as double best_score,best_score2
	dim as string os,hc
	dim as short l=info_length
	dim as short s=info_symbols
	dim as short dx0=info_x
	dim as short dy0=info_y
	dim as integer cip0(constcip)
	dim as integer nba0(constcip)
	dim as integer ngram_score
	dim as integer its,br1_curr,br1_restarts
	dim as integer itsmax=solvesub_hciterations
	dim as double tempstart=300/threads '10
	dim as double temp=tempstart
	dim as double temp_min=tempstart/itsmax
	dim as integer rc,r1,r2,r3,r4,ml,bs,restarts,improved
	dim as double change_operation
	dim as integer argmax=6
	dim as string op,re,item
	dim as double arg(100)
	dim as short ccs=21
	dim as short ccs2=ccs+10
	dim as short ccs3=ccs2+10
	dim as short msys=0
	dim as short first,instate,outstate,utp
	dim as double mv,mv_best,avgmv
	dim as double new_score,old_score,avg_score,avg_score2
	dim as byte hor,ver
	
	'---------------------------------------------------- batch ciphers stuff
	
	dim as integer batchciphers=solvesub_transpositionbatchciphers
	dim as integer metainfo,lmax,indacci
	dim as integer cipherend,itemnamenumber
	dim as integer numeric,ignored
	dim as integer cipherline,solution
	dim as integer contain_numbers
	dim as integer contain_spaces
	dim as integer contain_symbols
	dim as string num,filename,ln,ot,char
	dim as string outputdir=basedir+"\Output\"
	dim as string itemname
	dim as integer batchinfo(constcip)
	dim as integer batchsol(constcip)
	dim as integer batchnba(constcip)
	dim as integer ident(65536)
	dim as integer count,count2
	dim as integer batch_x
	dim as integer batch_y
	dim as integer items,items2,acctest,meta
	dim as double batchtimer
	dim as double batchtime,avgacc,acc,avgmp
	dim as string outeditbox
	
	if batchciphers=1 then
		
		task_active="batch ciphers (simple transposition)"
		update_solver_status
		dim as string filename=ui_loadsavedialog(0,"Open cipher batch file (simple transposition)",filter,1,basedir+"\Ciphers\")
		if fileexists(filename)=0 then
			if filename<>"" then ui_editbox_settext(output_text,"Error: file not found")
			task_active="none"
			update_solver_status
			exit sub
	   end if
		open filename for binary as #1
		lmax=constcip
		
	end if
	
	'---------------------------------------------------- batch ciphers stuff
	
	'dim as long dbc(l)
	
	dim as integer dx,dy,a,b,c,p,opn,ox,oy,o,xx,yy,x1,x2,y1,y2,dx1,dy1
	
	dim as string opname(100)
	opname(0)="None"
	opname(1)="Reverse"
	opname(2)="Period"
	opname(3)="Mirror"
	opname(4)="Flip"
	opname(5)="Snake"
	opname(6)="Offset"
	opname(7)="Offset from-to"
	opname(8)="Offset row order"
	opname(9)="Offset column order"
	opname(10)="Columnar"
	opname(11)="Diagonal"
	opname(12)="Period row order"
	opname(13)="Period column order"
	opname(14)="Skytale"
	opname(15)="Railfence"
	opname(16)="Swap row"
	opname(17)="Swap column"
	opname(18)="Split"
	opname(19)="L-route"
	opname(20)="Spiral"
	opname(21)="Period from-to"
	'opname(22)="Move"
	
	dim as integer operations=22 'max +1
	
	dim as short operation(operations-1)
	for i=0 to operations-1
		operation(i)=-1
	next i
	
	for i=0 to 100
		for j=0 to operations-1
			if ui_listbox_gettext(list_transpositionsolver_stack,i)=opname(j) then
				operation(i)=j
			end if
		next j
	next i
	
	dim as integer cb
	'dim as double ptcp(constcip,1)
	dim id() As ulong
	redim id(constcip,constcip)
	dim gr0() As short
	redim gr0(constcip,constcip)
	dim gr1() As short
	redim gr1(constcip,constcip)
	dim gr2() As short
	redim gr2(constcip,constcip)
	dim as short li0(constcip)
	dim as short li1(constcip)
	dim as integer beamdepth
	dim as integer beamsize
	
	dim as integer currl(stacksize)
	dim as integer prevl(stacksize)
	dim as short key0(stacksize,argmax)
	dim as short key1(stacksize,argmax)
	dim as short key2(stacksize,argmax)
	dim as short slock(stacksize,argmax)
	
	'dim as integer tabuits
	'dim as integer tabumax=100000
	'dim as integer taburej
	'dim as short tabu(100000,maxstack,4)
	
	for i=1 to l 'get cipher from input window
		cip0(i)=info(i)
		nba0(i)=nuba(i)
		cstate(ccs,i)=nba0(i)
		cstate(ccs3,i)=i
		for j=0 to stacksize
			cstate(ccs2+j,i)=nba0(i)
		next j
	next i
	
	ips_timer=timer
	dim as double statustimer=timer
	dim as double soltimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	task_active="substitution + simple transposition"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	'redim crc(2^32) '4 GB
	
	sectimer=timer
	
	do 'main loop
		
		'meta=0
		'itemname=""
		
		if batchciphers=1 andalso cipherend=0 then
			
			do 'read batch file, get new cipher
			
				line input #1,ln
				metainfo=0
				
				if instr(ln,"output_sub_directory=")>0 or instr(ln,"results_sub_directory=")>0 then
					'meta=1
					metainfo=1
					if solvesub_outputdir=1 then
						outputdir=basedir+"\Output\"+right(ln,len(ln)-instr(ln,"="))+"\"
						mkdir outputdir
					end if
					itemname=right(ln,len(ln)-instr(ln,"=")) 'if no name set by cipher_information, use sub_directory as name
				end if
				
				if instr(ln,"cipher_information=")>0 then
					meta=1
					metainfo=1
					itemname=right(ln,len(ln)-instr(ln,"="))
				end if
				
				'if instr(ln,"iterations_factor=")>0 then
				'	metainfo=1
				'	iterations*=val(right(ln,len(ln)-instr(ln,"=")))
				'	if solvesub_reversesolve=0 andalso iterations<100000 then iterations=100000
				'	'if iterations<100000 then iterations=100000
				'end if
				
				if instr(ln,"iterations=")>0 then
					metainfo=1
					iterations=val(right(ln,len(ln)-instr(ln,"=")))
					if solvesub_reversesolve=0 andalso iterations<100000 then iterations=100000
				end if
				
				if ln="" or eof(1) then cipherend=1
				
				if metainfo=0 then 
					if solution=0 then 'ciphertext
						cipherline+=1
						contain_spaces=0
						contain_symbols=0
						contain_numbers=0	
						if cipherline=1 then
							count=0
							count2=0
							for i=1 to len(ln) 'check if numeric
								select case asc(ln,i)
									case 32 'space
										contain_spaces=1
									case 48 to 57 'numbers
										contain_numbers=1
									case else 'symbols
										contain_symbols=1
								end select
							next i
							if contain_spaces=1 andalso contain_numbers=1 andalso contain_symbols=0 then
								numeric=1
							else
								numeric=0
							end if
						end if
						if numeric=1 then
							for i=1 to len(ln)
								select case asc(ln,i)
									case 32 'space
										if num<>"" then
											count+=1
											if count<=lmax then
												batchinfo(count)=val(num)
											end if
											num=""
										end if
									case 48 to 57 'numbers
										num+=chr(asc(ln,i))
									case else 'symbols
										'error
								end select
							next i
							if num<>"" then
								count+=1
								if count<=lmax then
									batchinfo(count)=val(num)
								end if
								num=""
							end if
						else
							for i=1 to len(ln)
								select case asc(ln,i)
									case 32 'space
										'error
									case else 'symbols
										count+=1
										if count<=lmax then
											batchinfo(count)=asc(ln,i)
										end if
								end select
							next i
						end if
					end if
				end if
				
				if cipherline=1 then batch_x=count
				
				if cipherend=1 then
					if count>ngram_size then
						batch_y=cipherline
						l=count
						if l>lmax then l=lmax
						s=0
						erase ident
						for i=1 to l
							if ident(batchinfo(i))=0 then
								s+=1
								ident(batchinfo(i))=s
								batchnba(i)=s
							else
								batchnba(i)=ident(batchinfo(i))
							end if
						next i
						
						'process cipher and leave line input loop
						'----------------------------------------
						if itemname="" then
							itemnamenumber+=1
							if meta=0 then itemname=str(itemnamenumber)
						'	itemname=str(itemnamenumber)
						'	if solvesub_outputdir=1 then
						'		outputdir=basedir+"\Output\"+itemname+"\"
						'		mkdir outputdir
						'	end if
						end if
						dx0=batch_x
						dy0=batch_y
						'dim as integer cip0(l)
						'dim as integer nba0(l)
						for i=1 to l
							cip0(i)=batchinfo(i)
							nba0(i)=batchnba(i)
							cstate(ccs,i)=nba0(i)
							cstate(ccs3,i)=i
							for j=0 to stacksize
								cstate(ccs2+j,i)=nba0(i)
							next j
						next i
						cipherline=0
						'cipherend=0
						solution=0
						itsmax=solvesub_hciterations
						iterations=solvesub_iterations
						temp=tempstart
	 					temp_min=tempstart/itsmax
	 					if eof(1) then cipherend=2
						exit do
						
					end if
					'cipherline=0
					'cipherend=0
					'solution=0
				end if
				
			loop until eof(1)
			
		end if
		
		do
		
			rc=int(rnd*stacksize)+1 'random changes
			beamsize=cint((bigramdepth*((itsmax-its)/itsmax))^(1/rc))
			if beamsize<1 then beamsize=1
			
			for i=1 to rc 'init generic loop
				currl(i)=1
				prevl(i)=0
			next i
			
			for i=0 to stacksize 'get key0
				for j=0 to argmax
					key1(i,j)=key0(i,j)
					key2(i,j)=0
					slock(i,j)=0 'erase slock
				next j
			next i
			
			mv_best=0
				
			do 'beam loop
				
				'score output modes: bhdscore/ngrams/pccycles ???
				
				for i=1 to rc 'change key
					if prevl(i)<>currl(i) then
						do
							e=0
							r=int(rnd*stacksize)+1 'random stack
							for j=1 to i-1
								if slock(j,0)=r then
									e=1
									exit for
								end if
							next j
						loop until e=0
						if rnd>0.1 then
							change_operation=1
						else
							select case key0(r,0)
								case 0,1,2,14:change_operation=1
								case else:change_operation=0
							end select
						end if
						for j=0 to argmax 'clean args (needed ???)
							key1(r,j)=0
						next j
						if change_operation=1 then 'change operation (instead of arguments)
							slock(i,0)=r
							if slock(i,0)<>r then 'restore prev. occupied key1 stack
								for j=0 to argmax
									key1(slock(i,0),j)=key0(slock(i,0),j)
								next j
							end if
							do
								key1(r,0)=operation(int(rnd*operations))
							loop until key1(r,0)>-1
							select case key1(r,0)
								case 0 'none
								case 1 'reverse
								case 2 'period
									key1(r,1)=int(rnd*2) 'transpose/untranspose
									key1(r,2)=int(rnd*(l-2))+2 'period 2 to l-1
								case 3 'mirror
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
								case 4 'flip
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
								case 5 'snake
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*2) 'even/uneven
								case 6 'offset
									key1(r,1)=int(rnd*(l-1))+1
								case 7 'offset from-to
									key1(r,1)=int(rnd*(l-1))+1 'from
									key1(r,2)=int(rnd*(l-key1(r,1)))+key1(r,1)+1 'to
									key1(r,3)=int(rnd*(key1(r,2)-key1(r,1)))+1 'offset
								case 8 'offset row order
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*(key1(r,2)-1))+1 'offset
								case 9 'offset column order
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*(key1(r,1)-1))+1 'offset 
								case 10 'columnar
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*2) 'transpose/untranspose
									key1(r,4)=int(rnd*4) 'corner
								case 11 'diagonal
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*2) 'transpose/untranspose
									key1(r,4)=int(rnd*8) 'corner
								case 12 'period row order
									key1(r,1)=int(rnd*((l/2)-2))+2 'dx '2 to (length/2)-1
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*2) 'transpose/untranspose
									key1(r,4)=int(rnd*(key1(r,2)-2))+2 'period 2 to dy-1
								case 13 'period column order
									key1(r,1)=int(rnd*(l-3))+3 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*2) 'transpose/untranspose
									key1(r,4)=int(rnd*(key1(r,1)-2))+2 'period 2 to dx-1
								case 14 'skytale
									key1(r,1)=int(rnd*2) 'transpose/untranspose
									do
										key1(r,2)=int(rnd*(l-2))+2 'period 2 to l-1
									loop until gcd(l,key1(r,2))=1
								case 15 'railfence
									key1(r,1)=int(rnd*2) 'transpose/untranspose
									do
										key1(r,2)=int(rnd*(l-2))+2 'rail 2 to l-1
										'key1(r,2)=int(rnd*((l/2)-1))+2 'rail 2 to l-1
									loop until gcd(l,key1(r,2))=1
									key1(r,3)=int(rnd*key1(r,2))+1 'offset
								case 16 'swap row
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									do
										r1=int(rnd*key1(r,2))+1
										r2=int(rnd*key1(r,2))+1
									loop until r1<>r2
									key1(r,3)=r1
									key1(r,4)=r2
								case 17 'swap column
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									do
										r1=int(rnd*key1(r,1))+1
										r2=int(rnd*key1(r,1))+1
									loop until r1<>r2
									key1(r,3)=r1
									key1(r,4)=r2
								case 18 'split
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*2) 'horizontal or vertical split
									if key1(r,3)=0 then
										key1(r,4)=int(rnd*(key1(r,2)-1))+1 'horizontal split offset
									else
										key1(r,4)=int(rnd*(key1(r,1)-1))+1 'vertical split offset
									end if
									key1(r,5)=int(rnd*32)+1 'transposition 1
									key1(r,6)=int(rnd*32)+1 'transposition 2
								case 19 'l-route
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*2) 'transpose/untranspose
									key1(r,4)=int(rnd*8)+1 'corner
								case 20 'spiral
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									key1(r,3)=int(rnd*2) 'transpose/untranspose
									key1(r,4)=int(rnd*2) 'inward/outward
									key1(r,5)=int(rnd*8)+1 'corner
								case 21 'period from-to
									key1(r,1)=int(rnd*(l-1))+1 'from
									key1(r,2)=int(rnd*(l-key1(r,1)))+key1(r,1)+1 'to
									key1(r,3)=int(rnd*2) 'transpose/untranspose
									key1(r,4)=int(rnd*((1+key1(r,2)-key1(r,1))-2))+2 'period 2 to from-to
								'case 22 'move
								'	key1(r,1)=int(rnd*(l-1))+1 'from
								'	key1(r,2)=int(rnd*(l-key1(r,1)))+key1(r,1) 'to (max: l-1)
								'	key1(r,3)=int(rnd*((1+key1(r,2)-key1(r,1))-1))+1 'period 1 to from-to
							end select
						else 'change argument
							slock(i,0)=r
							key1(r,0)=key0(r,0)
							select case key0(r,0)
								case 2 'period
									select case int(rnd*2)
										case 0 'change transpose/untranspose
											key1(r,1)=int(rnd*2) 'transpose/untranspose
											key1(r,2)=key0(r,2)
										case 1 'change period
											key1(r,1)=key0(r,1)
											key1(r,2)=int(rnd*(l-2))+2 'period 2 to l-1
									end select
								case 3 'mirror
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
								case 4 'flip
									key1(r,1)=int(rnd*(l-2))+2 'dx
									if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
								case 5 'snake
									select case int(rnd*2)
										case 0 'change dx
											key1(r,1)=int(rnd*(l-2))+2 'dx
											if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
											key1(r,3)=key0(r,3)
										case 1 'change even/uneven
											key1(r,1)=key0(r,1)
											key1(r,2)=key0(r,2)
											key1(r,3)=int(rnd*2) 'even/uneven
									end select
								case 6 'offset
									key1(r,1)=int(rnd*(l-1))+1
								case 7 'offset from-to
									select case int(rnd*3)
										case 0 'from
											key1(r,2)=key0(r,2)
											key1(r,3)=key0(r,3)
											key1(r,1)=int(rnd*(key1(r,2)-1))+1
										case 1 'to
											key1(r,1)=key0(r,1)
											key1(r,3)=key0(r,3)
											key1(r,2)=int(rnd*(l-key1(r,1)))+key1(r,1)+1
										case 2 'offset
											key1(r,1)=key0(r,1)
											key1(r,2)=key0(r,2)
											key1(r,3)=int(rnd*(key1(r,2)-key1(r,1)))+1
									end select
								case 8 'offset row order
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									key1(r,3)=int(rnd*(key1(r,2)-1))+1 'offset
								case 9 'offset column order
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									key1(r,3)=int(rnd*(key1(r,1)-1))+1 'offset
								case 10 'columnar
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									key1(r,3)=key0(r,3)
									do
										key1(r,4)=int(rnd*4) 'corner
									loop until key1(r,4)<>key0(r,4)
								case 11 'diagonal
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									key1(r,3)=key0(r,3)
									do
										key1(r,4)=int(rnd*8) 'corner
									loop until key1(r,4)<>key0(r,4)
								case 12 'period row order
									key1(r,3)=key0(r,3)
									key1(r,4)=key0(r,4)
									do
										key1(r,1)=int(rnd*(l-2))+2 'dx
										if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									loop until key1(r,2)>key1(r,4)
								case 13 'period column order
									key1(r,3)=key0(r,3)
									key1(r,4)=key0(r,4)
									do
										key1(r,1)=int(rnd*(l-2))+2 'dx
										if frac(l/key1(r,1))>0 then key1(r,2)=1+l\key1(r,1) else key1(r,2)=l\key1(r,1) 'dy
									loop until key1(r,1)>key1(r,4)
								case 14 'skytale
									select case int(rnd*2)
										case 0 'change transpose/untranspose
											key1(r,1)=int(rnd*2) 'transpose/untranspose
											key1(r,2)=key0(r,2)
										case 1 'change period
											key1(r,1)=key0(r,1)
											do
												key1(r,2)=int(rnd*(l-2))+2 'period 2 to l-1
											loop until gcd(l,key1(r,2))=1
									end select
								case 15 'railfence
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									key1(r,3)=int(rnd*key0(r,2))+1 'offset
								case 16 'swap row
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									do
										r1=int(rnd*key1(r,2))+1
										r2=int(rnd*key1(r,2))+1
									loop until r1<>r2
									key1(r,3)=r1
									key1(r,4)=r2
								case 17 'swap column
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									do
										r1=int(rnd*key1(r,1))+1
										r2=int(rnd*key1(r,1))+1
									loop until r1<>r2
									key1(r,3)=r1
									key1(r,4)=r2
								case 18 'split
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									key1(r,3)=key0(r,3)
									select case int(rnd*3)
										case 0
											if key1(r,3)=0 then
												key1(r,4)=int(rnd*(key1(r,2)-1))+1 'horizontal split offset
											else
												key1(r,4)=int(rnd*(key1(r,1)-1))+1 'vertical split offset
											end if
											key1(r,5)=key0(r,5)
											key1(r,6)=key0(r,6)
										case 1
											key1(r,4)=key0(r,4)
											key1(r,5)=int(rnd*32)+1 'change transposition 1
											key1(r,6)=key0(r,6)
										case 2
											key1(r,4)=key0(r,4)
											key1(r,5)=key0(r,5)
											key1(r,6)=int(rnd*32)+1 'change transposition 2
									end select
								case 19 'l-route
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									select case int(rnd*2)
										case 0
											if key0(r,3)=0 then key1(r,3)=1 else key1(r,3)=0 'change transpose/untranspose
											key1(r,4)=key0(r,4)
										case 1
											key1(r,3)=key0(r,3)
											key1(r,4)=int(rnd*8)+1 'change corner
									end select
								case 20 'spiral
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									key1(r,3)=key0(r,3)
									select case int(rnd*2)
										case 0
											key1(r,4)=int(rnd*2) 'change inward/outward
											key1(r,5)=key0(r,5)
										case 1
											key1(r,4)=key0(r,4)
											key1(r,5)=int(rnd*8)+1 'change corner
									end select
								case 21 'period from-to
									key1(r,1)=key0(r,1)
									key1(r,2)=key0(r,2)
									key1(r,3)=key0(r,3)
									key1(r,4)=key0(r,4)
									select case int(rnd*2)
										case 0 'from
											do
												key1(r,1)=int(rnd*(key1(r,2)-1))+1
											loop until 1+key1(r,2)-key1(r,1)>=key1(r,4) 'from-to >= period
										case 1 'to
											do
												key1(r,2)=int(rnd*(l-key1(r,1)))+key1(r,1)+1
											loop until 1+key1(r,2)-key1(r,1)>=key1(r,4) 'from-to >= period
									end select
								'case 22 'move
								'	key1(r,1)=key0(r,1)
								'	key1(r,2)=key0(r,2)
								'	key1(r,3)=key0(r,3)
								'	select case int(rnd*2)
								'		case 0 'from
								'			do
								'				key1(r,1)=int(rnd*(key1(r,2)-1))+1
								'			loop until 1+key1(r,2)-key1(r,1)>=key1(r,3) 'from-to >= period
								'		case 1 'to
								'			do
								'				key1(r,2)=int(rnd*(l-key1(r,1)))+key1(r,1)+1
								'			loop until 1+key1(r,2)-key1(r,1)>=key1(r,3) 'from-to >= period
								'	end select
							end select
						end if
					end if
				next i
				
				first=0
				for g=1 to stacksize 'apply key to cipher
					if prevl(g)<>currl(g) then
						for h=g to stacksize
							prevl(h)=currl(h)
							if first=0 then
								first=1
								instate=ccs2+(h-1)
							else
								instate=ccs+(h-1)
							end if
							outstate=ccs+h
							select case key1(h,0)
								case 0 'none
									for i=1 to l
										cstate(outstate,i)=cstate(instate,i)
									next i
								case 1 'reverse
									for i=1 to l
										cstate(outstate,i)=cstate(instate,l-(i-1))
									next i
								case 2 'period
									k=0
									utp=key1(h,1)
									p=key1(h,2)
									for i=1 to p
										for j=i to l step p
											k+=1
											if utp=0 then
												cstate(outstate,j)=cstate(instate,k)
											else
												cstate(outstate,k)=cstate(instate,j)
											end if
										next j
									next i
								case 3 'mirror
									dx=key1(h,1)
									dy=key1(h,2)
									i=0:j=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
										next x
									next y
									for y=1 to dy
										for x=1 to dx
											xx=dx-(x-1)
											if gr0(xx,y)>0 then
												j+=1
												cstate(outstate,j)=gr0(xx,y)
											end if
										next x
									next y
								case 4 'flip
									dx=key1(h,1)
									dy=key1(h,2)
									i=0:k=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
											gr1(x,y)=0
										next x
									next y
									for x=1 to dx
										j=0
										for y=1 to dy
											yy=dy-(y-1)
											if gr0(x,yy)>0 then
												j+=1	
												gr1(x,j)=gr0(x,yy)
											end if
										next y
									next x
									for y=1 to dy
										for x=1 to dx
											if gr1(x,y)>0 then
												k+=1
												cstate(outstate,k)=gr1(x,y)
											end if
										next x
									next y
								case 5 'snake
									dx=key1(h,1)
									dy=key1(h,2)
									o=key1(h,3)
									i=0:j=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
										next x
									next y
									for y=1 to dy
										for x=1 to dx
											if y mod 2=o then xx=dx-(x-1) else xx=x
											if gr0(xx,y)>0 then
												j+=1
												cstate(outstate,j)=gr0(xx,y)
											end if
										next x
									next y
								case 6 'offset
									o=key1(h,1)
									for i=1 to l
										a=i+o
										if a>l then a-=l
										cstate(outstate,i)=cstate(instate,a)
									next i
								case 7 'offset from-to
									f=key1(h,1)
									t=key1(h,2)
									o=key1(h,3)
									for i=1 to f-1
										cstate(outstate,i)=cstate(instate,i)
									next i
									for i=f to t
										j=i+o
										if j>t then j=(j-t)+(f-1)
										cstate(outstate,i)=cstate(instate,j)
									next i
									for i=t+1 to l
										cstate(outstate,i)=cstate(instate,i)
									next i
								case 8 'offset row order
									dx=key1(h,1)
									dy=key1(h,2)
									o=key1(h,3)
									i=0:j=0
									for y=1 to dy
										for x=1 to dx
											yy=y+o
											if yy>dy then yy=yy-dy
											i+=1
											if i>l then gr0(x,yy)=0 else gr0(x,yy)=cstate(instate,i)
										next x
									next y
									for y=1 to dy
										for x=1 to dx
											if gr0(x,y)>0 then
												j+=1
												cstate(outstate,j)=gr0(x,y)
											end if
										next x
									next y
								case 9 'offset column order
									dx=key1(h,1)
									dy=key1(h,2)
									o=key1(h,3)
									i=0:j=0
									for y=1 to dy
										for x=1 to dx
											xx=x+o
											if xx>dx then xx=xx-dx
											i+=1
											if i>l then gr0(xx,y)=0 else gr0(xx,y)=cstate(instate,i)
										next x
									next y
									for y=1 to dy
										for x=1 to dx
											if gr0(x,y)>0 then
												j+=1
												cstate(outstate,j)=gr0(x,y)
											end if
										next x
									next y
								case 10 'columnar
									dx=key1(h,1)
									dy=key1(h,2)
									utp=key1(h,3)
									c=key1(h,4)
									i=0:j=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=i
										next x
									next y
									for x=1 to dx
										for y=1 to dy
											select case c
												case 0:xx=x:yy=y
												case 1:xx=dx-(x-1):yy=y
												case 2:xx=x:yy=dy-(y-1)
												case 3:xx=dx-(x-1):yy=dy-(y-1)
											end select
											if gr0(xx,yy)>0 then
												j+=1
												li0(j)=gr0(xx,yy)
											end if
										next y
									next x
									for i=1 to l
										if utp=0 then
											cstate(outstate,i)=cstate(instate,li0(i))
										else
											cstate(outstate,li0(i))=cstate(instate,i)
										end if
									next i
								case 11 'diagonal
									dx=key1(h,1)
									dy=key1(h,2)
									utp=key1(h,3)
									c=key1(h,4)
									b=dx+(dy-1)
									i=0:j=0
									for y=1 to dy
										for x=1 to b
											gr0(x,y)=0
										next x
									next y
									for y=1 to dy
										if c>3 then a=dy-y else a=y-1
										for x=1+a to dx+a
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=i
										next x
									next y
									for x=1 to b
										for y=1 to dy
											select case c
												case 0,4:xx=x:yy=y
												case 1,5:xx=b-(x-1):yy=y
												case 2,6:xx=x:yy=dy-(y-1)
												case 3,7:xx=b-(x-1):yy=dy-(y-1)
											end select
											if gr0(xx,yy)>0 then
												j+=1
												li0(j)=gr0(xx,yy)
											end if
										next y
									next x
									for i=1 to l
										if utp=0 then
											cstate(outstate,i)=cstate(instate,li0(i))
										else
											cstate(outstate,li0(i))=cstate(instate,i)
										end if
									next i
								case 12 'period row order
									dx=key1(h,1)
									dy=key1(h,2)
									utp=key1(h,3)
									p=key1(h,4)
									i=0:k=0:a=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
											gr1(x,y)=0
										next x
									next y
									for i=1 to p
										for j=i to dy step p
											k+=1
											for x=1 to dx
												if utp=0 then 
													gr1(x,j)=gr0(x,k)
												else
													gr1(x,k)=gr0(x,j)
												end if
											next x
										next j
									next i
									for y=1 to dy
										for x=1 to dx
											if gr1(x,y)>0 then
												a+=1
												cstate(outstate,a)=gr1(x,y)
											end if
										next x
									next y
								case 13 'period column order
									dx=key1(h,1)
									dy=key1(h,2)
									utp=key1(h,3)
									p=key1(h,4)
									i=0:k=0:a=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
											gr1(x,y)=0
										next x
									next y
									for i=1 to p
										for j=i to dx step p
											k+=1
											for y=1 to dy
												if utp=0 then 
													gr1(j,y)=gr0(k,y)
												else
													gr1(k,y)=gr0(j,y)
												end if
											next y
										next j
									next i
									for y=1 to dy
										for x=1 to dx
											if gr1(x,y)>0 then
												a+=1
												cstate(outstate,a)=gr1(x,y)
											end if
										next x
									next y
								case 14 'skytale
									utp=key1(h,1)
									p=key1(h,2)
									j=1
									for i=1 to l
										if utp=0 then
											cstate(outstate,i)=cstate(instate,j)
										else
											cstate(outstate,j)=cstate(instate,i)
										end if
										j+=p
										if j>l then j-=l
									next i
								case 15 'railfence
									utp=key1(h,1)
									p=key1(h,2)
									j=key1(h,3)
									for i=1 to p
										gr0(i,0)=0
									next i
									k=int(rnd*2)
									if j=1 then k=0
									if j=p then k=1
									for i=1 to l
										gr0(j,0)+=1
										gr0(j,gr0(j,0))=i
										if k=0 then
											j+=1
											if j=p then k=1
										else
											j-=1
											if j=1 then k=0
										end if
									next i
									i=0
									for y=1 to p
										for x=1 to gr0(y,0)
											i+=1
											if utp=0 then
												cstate(outstate,i)=cstate(instate,gr0(y,x))
											else
												cstate(outstate,gr0(y,x))=cstate(instate,i)
											end if
										next x
									next y
								case 16 'swap row
									dx=key1(h,1)
									dy=key1(h,2)
									r1=key1(h,3)
									r2=key1(h,4)
									i=0:j=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
										next x
									next y
									for x=1 to dx
										swap gr0(x,r1),gr0(x,r2)
									next x
									for y=1 to dy
										for x=1 to dx
											if gr0(x,y)>0 then
												j+=1
												cstate(outstate,j)=gr0(x,y)
											end if
										next x
									next y
								case 17 'swap column
									dx=key1(h,1)
									dy=key1(h,2)
									r1=key1(h,3)
									r2=key1(h,4)
									i=0:j=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
										next x
									next y
									for y=1 to dy
										swap gr0(r1,y),gr0(r2,y)
									next y
									for y=1 to dy
										for x=1 to dx
											if gr0(x,y)>0 then
												j+=1
												cstate(outstate,j)=gr0(x,y)
											end if
										next x
									next y
								case 18 'split
									dx=key1(h,1)
									dy=key1(h,2)
									i=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
											gr1(x,y)=0 'clean gr1
										next x
									next y
									for a=1 to 2 'define rects
										if a=1 then
											if key1(h,3)=0 then 'hor
												x1=1:y1=1:x2=dx:y2=key1(h,4)
											else 'ver
												x1=1:y1=1:x2=key1(h,4):y2=dy
											end if
										else
											if key1(h,3)=0 then 'hor
												x1=1:y1=key1(h,4)+1:x2=dx:y2=dy
											else 'ver
												x1=key1(h,4)+1:y1=1:x2=dx:y2=dy
											end if
										end if
										select case key1(h,4+a)
											case 1 to 16
												x=x1:y=y1
												select case key1(h,4+a)
													case 1,9:xx=x1:yy=y1 'none
													case 2,10:xx=x2:yy=y1 'mirror
													case 3,11:xx=x1:yy=y2 'flip
													case 4,12:xx=x2:yy=y2 'reverse
													case 5,13:xx=x1:yy=y1 'columnar 1
													case 6,14:xx=x2:yy=y1 'columnar 2
													case 7,15:xx=x1:yy=y2 'columnar 3
													case 8,16:xx=x2:yy=y2 'columnar 4
												end select
												for i=1 to ((x2-x1)+1)*((y2-y1)+1)
													if gr0(xx,yy)>0 then 'input direction
														if key1(h,4+a)<9 then gr1(xx,yy)=gr0(x,y) else gr1(x,y)=gr0(xx,yy)
														x+=1
														if x>x2 then
															x=x1
															y+=1
														end if
													else
														if key1(h,4+a)<9 then gr1(xx,yy)=0 else gr1(x,y)=0
													end if
													select case key1(h,4+a) 'output direction
														case 1,9:xx+=1:if xx>x2 then xx=x1:yy+=1 'none
														case 2,10:xx-=1:if xx<x1 then xx=x2:yy+=1 'mirror
														case 3,11:xx+=1:if xx>x2 then xx=x1:yy-=1 'flip
														case 4,12:xx-=1:if xx<x1 then xx=x2:yy-=1 'reverse
														case 5,13:yy+=1:if yy>y2 then xx+=1:yy=y1 'columnar 1
														case 6,14:yy+=1:if yy>y2 then xx-=1:yy=y1 'columnar 2
														case 7,15:yy-=1:if yy<y1 then xx+=1:yy=y2 'columnar 3
														case 8,16:yy-=1:if yy<y1 then xx-=1:yy=y2 'columnar 4
													end select
												next i
											case 17 to 32 'diagonal transpose
												x=x1:y=y1
												select case key1(h,4+a)
													case 17,25:xx=x1:yy=y1 'diag 1u
													case 18,26:xx=x1:yy=y1 'diag 1d
													case 19,27:xx=x2:yy=y1 'diag 2u
													case 20,28:xx=x2:yy=y1 'diag 2d
													case 21,29:xx=x1:yy=y2 'diag 3u
													case 22,30:xx=x1:yy=y2 'diag 3d
													case 23,31:xx=x2:yy=y2 'diag 4u
													case 24,32:xx=x2:yy=y2 'diag 4d
												end select
												r1=xx:r2=yy
												for i=1 to ((x2-x1)+1)*((y2-y1)+1)
													if gr0(xx,yy)>0 then
														if key1(h,4+a)<25 then gr1(xx,yy)=gr0(x,y) else gr1(x,y)=gr0(xx,yy)
														x+=1
														if x>x2 then
															x=x1
															y+=1
														end if
													else
														if key1(h,4+a)<25 then gr1(xx,yy)=0 else gr1(x,y)=0
													end if
													select case key1(h,4+a)
														case 17,25:xx+=1:yy-=1 'diag 1u
															if xx>x2 or yy<y1 then
																r2+=1:if r2>y2 then r2=y2:r1+=1
																xx=r1:yy=r2
															end if
														case 18,26:xx-=1:yy+=1 'diag 1d
															if xx<x1 or yy>y2 then
																r1+=1:if r1>x2 then r1=x2:r2+=1
																xx=r1:yy=r2
															end if
														case 19,27:xx-=1:yy-=1 'diag 2u
															if xx<x1 or yy<y1 then
																r2+=1:if r2>y2 then r2=y2:r1-=1
																xx=r1:yy=r2
															end if
														case 20,28:xx+=1:yy+=1 'diag 2d
															if xx>x2 or yy>y2 then
																r1-=1:if r1<x1 then r1=x1:r2+=1
																xx=r1:yy=r2
															end if
														case 21,29:xx-=1:yy-=1 'diag 3u
															if xx<x1 or yy<y1 then
																r1+=1:if r1>x2 then r1=x2:r2-=1
																xx=r1:yy=r2
															end if
														case 22,30:xx+=1:yy+=1 'diag 3d
															if xx>x2 or yy>y2 then
																r2-=1:if r2<y1 then r2=y1:r1+=1
																xx=r1:yy=r2
															end if
														case 23,31:xx+=1:yy-=1 'diag 4u
															if xx>x2 or yy<y1 then
																r1-=1:if r1<x1 then r1=x1:r2-=1
																xx=r1:yy=r2
															end if
														case 24,32:xx-=1:yy+=1 'diag 4d
															if xx<x1 or yy>y2 then
																r2-=1:if r2<y1 then r2=y1:r1-=1
																xx=r1:yy=r2
															end if
													end select
												next i
										end select
									next a
									i=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											cstate(outstate,i)=gr1(x,y)
										next x
									next y
								case 19 'l-route
									dx=key1(h,1)
									dy=key1(h,2)
									utp=key1(h,3)
									c=key1(h,4)
									i=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=1 'cstate(instate,i)
											gr1(x,y)=0 'clean gr1
										next x
									next y
									select case c
										case 1:x1=dx:y1=1:x2=0:y2=0:hor=1 'corner 1 hor
										case 2:x1=1:y1=dy:x2=0:y2=0:hor=0 'corner 1 ver
										case 3:x1=1:y1=1:x2=1:y2=0:hor=1 'corner 2 hor
										case 4:x1=dx:y1=dy:x2=1:y2=0:hor=0 'corner 2 ver
										case 5:x1=dx:y1=dy:x2=0:y2=1:hor=1 'corner 3 hor
										case 6:x1=1:y1=1:x2=0:y2=1:hor=0 'corner 3 ver
										case 7:x1=1:y1=dy:x2=1:y2=1:hor=1 'corner 4 hor
										case 8:x1=dx:y1=1:x2=1:y2=1:hor=0 'corner 4 ver
									end select
									i=0
									do
										if x2=0 then xx=1 else xx=dx
										if y2=0 then yy=1 else yy=dy
										if hor=1 then
											for x=1 to dx 'hor
												if gr0(xx,y1)>0 andalso gr1(xx,y1)=0 then i+=1:gr1(xx,y1)=i
												if x2=0 then xx+=1 else xx-=1
											next x
											for y=1 to dy 'ver
												if gr0(x1,yy)>0 andalso gr1(x1,yy)=0 then i+=1:gr1(x1,yy)=i
												if y2=0 then yy+=1 else yy-=1
											next y
										else
											for y=1 to dy 'ver
												if gr0(x1,yy)>0 andalso gr1(x1,yy)=0 then i+=1:gr1(x1,yy)=i
												if y2=0 then yy+=1 else yy-=1
											next y
											for x=1 to dx 'hor
												if gr0(xx,y1)>0 andalso gr1(xx,y1)=0 then i+=1:gr1(xx,y1)=i
												if x2=0 then xx+=1 else xx-=1
											next x
										end if
										select case c
											case 1,8:x1-=1:y1+=1:if x1=0 then x1=1:if y1>dy then y1=dy
											case 2,7:x1+=1:y1-=1:if x1>dx then x1=dx:if y1=0 then y1=1
											case 3,6:x1+=1:y1+=1:if x1>dx then x1=dx:if y1>dy then y1=dy
											case 4,5:x1-=1:y1-=1:if x1=0 then x1=1:if y1=0 then y1=1
										end select
									loop until i=l
									i=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if utp=0 then
												cstate(outstate,i)=cstate(instate,gr1(x,y))
											else
												cstate(outstate,gr1(x,y))=cstate(instate,i)
											end if
											if i=l then exit for,for
										next x
									next y
								case 20 'spiral
									dx=key1(h,1)
									dy=key1(h,2)
									utp=key1(h,3)
									j=key1(h,4)
									c=key1(h,5)
									i=0
									for y=1 to dy
										for x=1 to dx
											i+=1
											if i>l then gr0(x,y)=0 else gr0(x,y)=1 'cstate(instate,i)
											gr1(x,y)=0
											gr2(x,y)=0
										next x
									next y
									select case c
										case 1:x1=1:y1=1:d=1
										case 2:x1=1:y1=1:d=3
										case 3:x1=dx:y1=1:d=2
										case 4:x1=dx:y1=1:d=3
										case 5:x1=1:y1=dy:d=1
										case 6:x1=1:y1=dy:d=4
										case 7:x1=dx:y1=dy:d=2
										case 8:x1=dx:y1=dy:d=4
									end select
									x=x1:y=y1:i=0
									do
										gr2(x,y)=1
										if gr0(x,y)>0 andalso gr1(x,y)=0 then i+=1:gr1(x,y)=i
										select case d
											case 1:x+=1
												if x=dx or gr2(x+1,y)>0 then
													if y+1<=dy andalso gr2(x,y+1)=0 then d=3
													if y-1>0 andalso gr2(x,y-1)=0 then d=4
												end if
											case 2:x-=1
												if x=1 or gr2(x-1,y)>0 then
													if y+1<=dy andalso gr2(x,y+1)=0 then d=3
													if y-1>0 andalso gr2(x,y-1)=0 then d=4
												end if
											case 3:y+=1
												if y=dy or gr2(x,y+1)>0 then
													if x+1<=dx andalso gr2(x+1,y)=0 then d=1
													if x-1>0 andalso gr2(x-1,y)=0 then d=2
												end if
											case 4:y-=1
												if y=1 or gr2(x,y-1)>0 then
													if x+1<=dx andalso gr2(x+1,y)=0 then d=1
													if x-1>0 andalso gr2(x-1,y)=0 then d=2
												end if
										end select	
									loop until i=l
									if j=0 then i=0 else i=l 'else i=l
									for y=1 to dy
										for x=1 to dx
											if j=0 then i+=1 else i-=1
											if utp=0 then
												cstate(outstate,i)=cstate(instate,gr1(x,y))
											else
												cstate(outstate,gr1(x,y))=cstate(instate,i)
											end if
											if i=l or i=0 then exit for,for
										next x
									next y
								case 21 'period from-to
									k=0:c=0
									utp=key1(h,3)
									p=key1(h,4)
									for i=key1(h,1) to key1(h,2)
										c+=1
										li0(c)=cstate(instate,i)
									next i
									for i=1 to p
										for j=i to c step p
											k+=1
											if utp=0 then
												li1(j)=li0(k)
											else
												li1(k)=li0(j)
											end if
										next j
									next i
									c=0
									for i=1 to l
										if i>=key1(h,1) andalso i<=key1(h,2) then
											c+=1
											cstate(outstate,i)=li1(c)
										else
											cstate(outstate,i)=cstate(instate,i)
										end if
									next i
								'case 22 'move
								'	j=0
								'	for i=1 to l
								'		li0(i)=0
								'	next i
								'	for i=key1(h,1) to key1(h,2) step key1(h,3)
								'		li0(i)=cstate(instate,i)
								'	next i
								'	for i=1 to l
								'		if li0(i)=0 then
								'			j+=1
								'			cstate(outstate,j)=cstate(instate,i)
								'		end if
								'	next i
								'	for i=key1(h,1) to key1(h,2) step key1(h,3)
								'		j+=1
								'		cstate(outstate,j)=cstate(instate,i)
								'	next i
							end select
						next h
						exit for
					end if
				next g
				
				a=1
				
				'for i=1 to tabuits 'check tabu list
				'	b=tabuits-(i-1) 'top-down
				'	e=1
				'	for j=1 to stacksize
				'		for k=0 to argmax
				'			if key1(j,k)<>tabu(b,j,k) then
				'				e=0
				'				exit for,for
				'			end if
				'		next k
				'	next j
				'	if e=1 then
				'		a=0
				'		taburej+=1
				'		exit for
				'	end if
				'next i
				
				''dim as integer test(l)
				
				mv=0
				if a=1 then
					select case msys 'measurement filter and save best key
						case 0 'bigrams
							
							instate=ccs+stacksize
							
							'bigrams
							'--------------------------------
							cb+=1
							if cb=4294967296 then 'save memory
								cb=1
								erase id
							end if
							for i=1 to l-1
								a=cstate(instate,i)
								b=cstate(instate,i+1)
								if id(a,b)<cb then
									id(a,b)=cb
								else
									mv+=1
								end if
							next i
							
							'cycles (test)
							'--------------------------------
							'for i=1 to l 
							'	testm(i)=cstate(instate,i)
							'next i
							'mv=m_2cycles(testm(),l,s,5)
							
							if mv>mv_best then
								'c=crc_32(@cstate(instate,1),l)
								'if crc(c)=0 then
									mv_best=mv
									for i=0 to stacksize
										for j=0 to argmax
											key2(i,j)=key1(i,j)
										next j
									next i
								'end if
							end if
							
					end select
				end if
				
				currl(rc)+=1 'iterate beam
				for i=1 to rc
					j=rc-(i-1)
					if currl(j)>beamsize then
						if j=1 then exit do
						currl(j)=1
						currl(j-1)+=1
					end if
				next i
				
			loop
		
		loop until mv_best>0
		
		item=""
		for i=1 to stacksize 'generate item name
			select case key2(i,0)
				case 0
					item+="None"
				case 1
					item+="Reverse"
				case 2
					item+="Period("
					if key2(i,1)=0 then item+="TP," else item+="UTP,"
					item+="P:"+str(key2(i,2))+")"
				case 3
					item+="Mirror("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))+")"
				case 4
					item+="Flip("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))+")"
				case 5
					item+="Snake("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))+")"
					'even/uneven (output information)
				case 6
					item+="Offset("
					item+="X:"+str(key2(i,1))+")"
				case 7
					item+="Offset from-to("
					item+="F:"+str(key2(i,1))
					item+=",T:"+str(key2(i,2))
					item+=",X:"+str(key2(i,3))+")"
				case 8
					item+="Offset row order("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					item+=",Y:"+str(key2(i,3))+")"
				case 9
					item+="Offset column order("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					item+=",X:"+str(key2(i,3))+")"		
				case 10
					item+="Columnar("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					if key2(i,3)=0 then item+=",TP" else item+=",UTP"
					item+=",C:"+str(key2(i,4)+1)+")"
				case 11
					item+="Diagonal("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					if key2(i,3)=0 then item+=",TP" else item+=",UTP"
					item+=",C:"+str(key2(i,4)+1)+")"
				case 12
					item+="Period row order("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					if key2(i,3)=0 then item+=",TP" else item+=",UTP"
					item+=",P:"+str(key2(i,4))+")"
				case 13
					item+="Period column order("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					if key2(i,3)=0 then item+=",TP" else item+=",UTP"
					item+=",P:"+str(key2(i,4))+")"
				case 14
					item+="Skytale("
					if key2(i,1)=0 then item+="TP," else item+="UTP,"
					item+="P:"+str(key2(i,2))+")"
				case 15
					item+="Railfence("
					if key2(i,1)=0 then item+="TP," else item+="UTP,"
					item+="R:"+str(key2(i,2))+","
					item+="S:"+str(key2(i,3))+")"
				case 16
					item+="Swap row("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					item+=","+str(key2(i,3))
					item+=","+str(key2(i,4))+")"
				case 17
					item+="Swap column("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					item+=","+str(key2(i,3))
					item+=","+str(key2(i,4))+")"
				case 18
					item+="Split("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					if key2(i,3)=0 then item+=",H," else item+=",V,"
					item+=str(key2(i,4))+","
					for j=1 to 2
						select case key2(i,4+j)
							case 1,9:item+="None"
							case 2,10:item+="Mirror"
							case 3,11:item+="Flip"
							case 4,12:item+="Reverse"
							case 5:item+="Columnar(TP,C:1)"
							case 6:item+="Columnar(TP,C:2)"
							case 7:item+="Columnar(TP,C:3)"
							case 8:item+="Columnar(TP,C:4)"
							case 13:item+="Columnar(UTP,C:1)"
							case 14:item+="Columnar(UTP,C:2)"
							case 15:item+="Columnar(UTP,C:3)"
							case 16:item+="Columnar(UTP,C:4)"
							case 17:item+="Diagonal(TP,C:1A)"
							case 18:item+="Diagonal(TP,C:1B)"
							case 19:item+="Diagonal(TP,C:2A)"
							case 20:item+="Diagonal(TP,C:2B)"
							case 21:item+="Diagonal(TP,C:3A)"
							case 22:item+="Diagonal(TP,C:3B)"
							case 23:item+="Diagonal(TP,C:4A)"
							case 24:item+="Diagonal(TP,C:4B)"
							case 25:item+="Diagonal(UTP,C:1A)"
							case 26:item+="Diagonal(UTP,C:1B)"
							case 27:item+="Diagonal(UTP,C:2A)"
							case 28:item+="Diagonal(UTP,C:2B)"
							case 29:item+="Diagonal(UTP,C:3A)"
							case 30:item+="Diagonal(UTP,C:3B)"
							case 31:item+="Diagonal(UTP,C:4A)"
							case 32:item+="Diagonal(UTP,C:4B)"
						end select
						if j=1 then item+=","
					next j
					item+=")"
				case 19
					item+="L-route("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					if key2(i,3)=0 then item+=",TP" else item+=",UTP"
					item+=",C:"+str(key2(i,4))+")"
				case 20
					item+="Spiral("
					item+=str(key2(i,1))+"*"
					item+=str(key2(i,2))
					if key2(i,3)=0 then item+=",TP" else item+=",UTP"
					if key2(i,4)=0 then item+=",IN," else item+=",OUT,"
					select case key2(i,5)
						case 1:item+="C:1A)"
						case 2:item+="C:1B)"
						case 3:item+="C:2A)"
						case 4:item+="C:2B)"
						case 5:item+="C:3A)"
						case 6:item+="C:3B)"
						case 7:item+="C:4A)"
						case 8:item+="C:4B)"
					end select
				case 21
					item+="Period from-to("
					item+=str(key2(i,1))+"-"
					item+=str(key2(i,2))+","
					if key2(i,3)=0 then item+="TP," else item+="UTP,"
					item+="P:"+str(key2(i,4))+")"
				'case 22
				'	item+="Move("
				'	item+=str(key2(i,1))+"-"
				'	item+=str(key2(i,2))+","
				'	item+="P:"+str(key2(i,3))+")"
				case else
					item+="Error"
			end select
			if solvesub_outputbatch=1 then
				if i<>stacksize then item+=" "
			else
				if i<>stacksize then item+=lb
			end if
		next i
		
		for h=1 to stacksize 'apply key to cipher
			for m=0 to solvesub_pcmode
				if m=0 then
					instate=ccs+(h-1)
					outstate=ccs+h
				else
					instate=ccs3+(h-1)
					outstate=ccs3+h
				end if
				select case key2(h,0)
					case 0 'none
						for i=1 to l
							cstate(outstate,i)=cstate(instate,i)
						next i
					case 1 'reverse
						for i=1 to l
							cstate(outstate,i)=cstate(instate,l-(i-1))
						next i
					case 2 'period
						k=0
						utp=key2(h,1)
						p=key2(h,2)
						for i=1 to p
							for j=i to l step p
								k+=1
								if utp=0 then
									cstate(outstate,j)=cstate(instate,k)
								else
									cstate(outstate,k)=cstate(instate,j)
								end if
							next j
						next i
					case 3 'mirror
						dx=key2(h,1)
						dy=key2(h,2)
						i=0:j=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
							next x
						next y
						for y=1 to dy
							for x=1 to dx
								xx=dx-(x-1)
								if gr0(xx,y)>0 then
									j+=1
									cstate(outstate,j)=gr0(xx,y)
								end if
							next x
						next y
					case 4 'flip
						dx=key2(h,1)
						dy=key2(h,2)
						i=0:k=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
								gr1(x,y)=0
							next x
						next y
						for x=1 to dx
							j=0
							for y=1 to dy
								yy=dy-(y-1)
								if gr0(x,yy)>0 then
									j+=1	
									gr1(x,j)=gr0(x,yy)
								end if
							next y
						next x
						for y=1 to dy
							for x=1 to dx
								if gr1(x,y)>0 then
									k+=1
									cstate(outstate,k)=gr1(x,y)
								end if
							next x
						next y
					case 5 'snake
						dx=key2(h,1)
						dy=key2(h,2)
						o=key2(h,3)
						i=0:j=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
							next x
						next y
						for y=1 to dy
							for x=1 to dx
								if y mod 2=o then xx=dx-(x-1) else xx=x
								if gr0(xx,y)>0 then
									j+=1
									cstate(outstate,j)=gr0(xx,y)
								end if
							next x
						next y
					case 6 'offset
						o=key2(h,1)
						for i=1 to l
							a=i+o
							if a>l then a-=l
							cstate(outstate,i)=cstate(instate,a)
						next i
					case 7 'offset from-to
						f=key2(h,1)
						t=key2(h,2)
						o=key2(h,3)
						for i=1 to f-1
							cstate(outstate,i)=cstate(instate,i)
						next i
						for i=f to t
							j=i+o
							if j>t then j=(j-t)+(f-1)
							cstate(outstate,i)=cstate(instate,j)
						next i
						for i=t+1 to l
							cstate(outstate,i)=cstate(instate,i)
						next i
					case 8 'offset row order
						dx=key2(h,1)
						dy=key2(h,2)
						o=key2(h,3)
						i=0:j=0
						for y=1 to dy
							for x=1 to dx
								yy=y+o
								if yy>dy then yy=yy-dy
								i+=1
								if i>l then gr0(x,yy)=0 else gr0(x,yy)=cstate(instate,i)
							next x
						next y
						for y=1 to dy
							for x=1 to dx
								if gr0(x,y)>0 then
									j+=1
									cstate(outstate,j)=gr0(x,y)
								end if
							next x
						next y
					case 9 'offset column order
						dx=key2(h,1)
						dy=key2(h,2)
						o=key2(h,3)
						i=0:j=0
						for y=1 to dy
							for x=1 to dx
								xx=x+o
								if xx>dx then xx=xx-dx
								i+=1
								if i>l then gr0(xx,y)=0 else gr0(xx,y)=cstate(instate,i)
							next x
						next y
						for y=1 to dy
							for x=1 to dx
								if gr0(x,y)>0 then
									j+=1
									cstate(outstate,j)=gr0(x,y)
								end if
							next x
						next y
					case 10 'columnar
						dx=key2(h,1)
						dy=key2(h,2)
						utp=key2(h,3)
						c=key2(h,4)
						i=0:j=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=i
							next x
						next y
						for x=1 to dx
							for y=1 to dy
								select case c
									case 0:xx=x:yy=y
									case 1:xx=dx-(x-1):yy=y
									case 2:xx=x:yy=dy-(y-1)
									case 3:xx=dx-(x-1):yy=dy-(y-1)
								end select
								if gr0(xx,yy)>0 then
									j+=1
									li0(j)=gr0(xx,yy)
								end if
							next y
						next x
						for i=1 to l
							if utp=0 then
								cstate(outstate,i)=cstate(instate,li0(i))
							else
								cstate(outstate,li0(i))=cstate(instate,i)
							end if
						next i
					case 11 'diagonal
						dx=key2(h,1)
						dy=key2(h,2)
						utp=key2(h,3)
						c=key2(h,4)
						b=dx+(dy-1)
						i=0:j=0
						for y=1 to dy
							for x=1 to b
								gr0(x,y)=0
							next x
						next y
						for y=1 to dy
							if c>3 then a=dy-y else a=y-1
							for x=1+a to dx+a
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=i
							next x
						next y
						for x=1 to b
							for y=1 to dy
								select case c
									case 0,4:xx=x:yy=y
									case 1,5:xx=b-(x-1):yy=y
									case 2,6:xx=x:yy=dy-(y-1)
									case 3,7:xx=b-(x-1):yy=dy-(y-1)
								end select
								if gr0(xx,yy)>0 then
									j+=1
									li0(j)=gr0(xx,yy)
								end if
							next y
						next x
						for i=1 to l
							if utp=0 then
								cstate(outstate,i)=cstate(instate,li0(i))
							else
								cstate(outstate,li0(i))=cstate(instate,i)
							end if
						next i
					case 12 'period row order
						dx=key2(h,1)
						dy=key2(h,2)
						utp=key2(h,3)
						p=key2(h,4)
						i=0:k=0:a=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
								gr1(x,y)=0
							next x
						next y
						for i=1 to p
							for j=i to dy step p
								k+=1
								for x=1 to dx
									if utp=0 then 
										gr1(x,j)=gr0(x,k)
									else
										gr1(x,k)=gr0(x,j)
									end if
								next x
							next j
						next i
						for y=1 to dy
							for x=1 to dx
								if gr1(x,y)>0 then
									a+=1
									cstate(outstate,a)=gr1(x,y)
								end if
							next x
						next y
					case 13 'period column order
						dx=key2(h,1)
						dy=key2(h,2)
						utp=key2(h,3)
						p=key2(h,4)
						i=0:k=0:a=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
								gr1(x,y)=0
							next x
						next y
						for i=1 to p
							for j=i to dx step p
								k+=1
								for y=1 to dy
									if utp=0 then 
										gr1(j,y)=gr0(k,y)
									else
										gr1(k,y)=gr0(j,y)
									end if
								next y
							next j
						next i
						for y=1 to dy
							for x=1 to dx
								if gr1(x,y)>0 then
									a+=1
									cstate(outstate,a)=gr1(x,y)
								end if
							next x
						next y
					case 14 'skytale
						utp=key2(h,1)
						p=key2(h,2)
						j=1
						for i=1 to l
							if utp=0 then
								cstate(outstate,i)=cstate(instate,j)
							else
								cstate(outstate,j)=cstate(instate,i)
							end if
							j+=p
							if j>l then j-=l
						next i
					case 15 'railfence
						utp=key2(h,1)
						p=key2(h,2)
						j=key2(h,3)
						for i=1 to p
							gr0(i,0)=0
						next i
						k=int(rnd*2)
						if j=1 then k=0
						if j=p then k=1
						for i=1 to l
							gr0(j,0)+=1
							gr0(j,gr0(j,0))=i
							if k=0 then
								j+=1
								if j=p then k=1
							else
								j-=1
								if j=1 then k=0
							end if
						next i
						i=0
						for y=1 to p
							for x=1 to gr0(y,0)
								i+=1
								if utp=0 then
									cstate(outstate,i)=cstate(instate,gr0(y,x))
								else
									cstate(outstate,gr0(y,x))=cstate(instate,i)
								end if
							next x
						next y
					case 16 'swap row
						dx=key2(h,1)
						dy=key2(h,2)
						r1=key2(h,3)
						r2=key2(h,4)
						i=0:j=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
							next x
						next y
						for x=1 to dx
							swap gr0(x,r1),gr0(x,r2)
						next x
						for y=1 to dy
							for x=1 to dx
								if gr0(x,y)>0 then
									j+=1
									cstate(outstate,j)=gr0(x,y)
								end if
							next x
						next y
					case 17 'swap column
						dx=key2(h,1)
						dy=key2(h,2)
						r1=key2(h,3)
						r2=key2(h,4)
						i=0:j=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
							next x
						next y
						for y=1 to dy
							swap gr0(r1,y),gr0(r2,y)
						next y
						for y=1 to dy
							for x=1 to dx
								if gr0(x,y)>0 then
									j+=1
									cstate(outstate,j)=gr0(x,y)
								end if
							next x
						next y
					case 18 'split
						dx=key2(h,1)
						dy=key2(h,2)
						i=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=cstate(instate,i)
								gr1(x,y)=0 'clean gr1
							next x
						next y
						for a=1 to 2 'define rects
							if a=1 then
								if key2(h,3)=0 then 'hor
									x1=1:y1=1:x2=dx:y2=key2(h,4)
								else 'ver
									x1=1:y1=1:x2=key2(h,4):y2=dy
								end if
							else
								if key2(h,3)=0 then 'hor
									x1=1:y1=key2(h,4)+1:x2=dx:y2=dy
								else 'ver
									x1=key2(h,4)+1:y1=1:x2=dx:y2=dy
								end if
							end if
							select case key2(h,4+a)
								case 1 to 16
									x=x1:y=y1
									select case key2(h,4+a)
										case 1,9:xx=x1:yy=y1 'none
										case 2,10:xx=x2:yy=y1 'mirror
										case 3,11:xx=x1:yy=y2 'flip
										case 4,12:xx=x2:yy=y2 'reverse
										case 5,13:xx=x1:yy=y1 'columnar 1
										case 6,14:xx=x2:yy=y1 'columnar 2
										case 7,15:xx=x1:yy=y2 'columnar 3
										case 8,16:xx=x2:yy=y2 'columnar 4
									end select
									for i=1 to ((x2-x1)+1)*((y2-y1)+1)
										if gr0(xx,yy)>0 then 'input direction
											if key2(h,4+a)<9 then gr1(xx,yy)=gr0(x,y) else gr1(x,y)=gr0(xx,yy)
											x+=1
											if x>x2 then
												x=x1
												y+=1
											end if
										else
											if key2(h,4+a)<9 then gr1(xx,yy)=0 else gr1(x,y)=0
										end if
										select case key2(h,4+a) 'output direction
											case 1,9:xx+=1:if xx>x2 then xx=x1:yy+=1 'none
											case 2,10:xx-=1:if xx<x1 then xx=x2:yy+=1 'mirror
											case 3,11:xx+=1:if xx>x2 then xx=x1:yy-=1 'flip
											case 4,12:xx-=1:if xx<x1 then xx=x2:yy-=1 'reverse
											case 5,13:yy+=1:if yy>y2 then xx+=1:yy=y1 'columnar 1
											case 6,14:yy+=1:if yy>y2 then xx-=1:yy=y1 'columnar 2
											case 7,15:yy-=1:if yy<y1 then xx+=1:yy=y2 'columnar 3
											case 8,16:yy-=1:if yy<y1 then xx-=1:yy=y2 'columnar 4
										end select
									next i
								case 17 to 32 'diagonal transpose
									x=x1:y=y1
									select case key2(h,4+a)
										case 17,25:xx=x1:yy=y1 'diag 1u
										case 18,26:xx=x1:yy=y1 'diag 1d
										case 19,27:xx=x2:yy=y1 'diag 2u
										case 20,28:xx=x2:yy=y1 'diag 2d
										case 21,29:xx=x1:yy=y2 'diag 3u
										case 22,30:xx=x1:yy=y2 'diag 3d
										case 23,31:xx=x2:yy=y2 'diag 4u
										case 24,32:xx=x2:yy=y2 'diag 4d
									end select
									r1=xx:r2=yy
									for i=1 to ((x2-x1)+1)*((y2-y1)+1)
										if gr0(xx,yy)>0 then
											if key2(h,4+a)<25 then gr1(xx,yy)=gr0(x,y) else gr1(x,y)=gr0(xx,yy)
											x+=1
											if x>x2 then
												x=x1
												y+=1
											end if
										else
											if key2(h,4+a)<25 then gr1(xx,yy)=0 else gr1(x,y)=0
										end if
										select case key2(h,4+a)
											case 17,25:xx+=1:yy-=1 'diag 1u
												if xx>x2 or yy<y1 then
													r2+=1:if r2>y2 then r2=y2:r1+=1
													xx=r1:yy=r2
												end if
											case 18,26:xx-=1:yy+=1 'diag 1d
												if xx<x1 or yy>y2 then
													r1+=1:if r1>x2 then r1=x2:r2+=1
													xx=r1:yy=r2
												end if
											case 19,27:xx-=1:yy-=1 'diag 2u
												if xx<x1 or yy<y1 then
													r2+=1:if r2>y2 then r2=y2:r1-=1
													xx=r1:yy=r2
												end if
											case 20,28:xx+=1:yy+=1 'diag 2d
												if xx>x2 or yy>y2 then
													r1-=1:if r1<x1 then r1=x1:r2+=1
													xx=r1:yy=r2
												end if
											case 21,29:xx-=1:yy-=1 'diag 3u
												if xx<x1 or yy<y1 then
													r1+=1:if r1>x2 then r1=x2:r2-=1
													xx=r1:yy=r2
												end if
											case 22,30:xx+=1:yy+=1 'diag 3d
												if xx>x2 or yy>y2 then
													r2-=1:if r2<y1 then r2=y1:r1+=1
													xx=r1:yy=r2
												end if
											case 23,31:xx+=1:yy-=1 'diag 4u
												if xx>x2 or yy<y1 then
													r1-=1:if r1<x1 then r1=x1:r2-=1
													xx=r1:yy=r2
												end if
											case 24,32:xx-=1:yy+=1 'diag 4d
												if xx<x1 or yy>y2 then
													r2-=1:if r2<y1 then r2=y1:r1-=1
													xx=r1:yy=r2
												end if
										end select
									next i
							end select
						next a
						i=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								cstate(outstate,i)=gr1(x,y)
							next x
						next y
					case 19 'l-route
						dx=key2(h,1)
						dy=key2(h,2)
						utp=key2(h,3)
						c=key2(h,4)
						i=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=1 'cstate(instate,i)
								gr1(x,y)=0 'clean gr1
							next x
						next y
						select case c
							case 1:x1=dx:y1=1:x2=0:y2=0:hor=1 'corner 1 hor
							case 2:x1=1:y1=dy:x2=0:y2=0:hor=0 'corner 1 ver
							case 3:x1=1:y1=1:x2=1:y2=0:hor=1 'corner 2 hor
							case 4:x1=dx:y1=dy:x2=1:y2=0:hor=0'corner 2 ver
							case 5:x1=dx:y1=dy:x2=0:y2=1:hor=1 'corner 3 hor
							case 6:x1=1:y1=1:x2=0:y2=1:hor=0 'corner 3 ver
							case 7:x1=1:y1=dy:x2=1:y2=1:hor=1 'corner 4 hor
							case 8:x1=dx:y1=1:x2=1:y2=1:hor=0 'corner 4 ver
						end select
						i=0
						do
							if x2=0 then xx=1 else xx=dx
							if y2=0 then yy=1 else yy=dy
							if hor=1 then
								for x=1 to dx 'hor
									if gr0(xx,y1)>0 andalso gr1(xx,y1)=0 then i+=1:gr1(xx,y1)=i
									if x2=0 then xx+=1 else xx-=1
								next x
								for y=1 to dy 'ver
									if gr0(x1,yy)>0 andalso gr1(x1,yy)=0 then i+=1:gr1(x1,yy)=i
									if y2=0 then yy+=1 else yy-=1
								next y
							else
								for y=1 to dy 'ver
									if gr0(x1,yy)>0 andalso gr1(x1,yy)=0 then i+=1:gr1(x1,yy)=i
									if y2=0 then yy+=1 else yy-=1
								next y
								for x=1 to dx 'hor
									if gr0(xx,y1)>0 andalso gr1(xx,y1)=0 then i+=1:gr1(xx,y1)=i
									if x2=0 then xx+=1 else xx-=1
								next x
							end if
							select case c
								case 1,8:x1-=1:y1+=1:if x1=0 then x1=1:if y1>dy then y1=dy
								case 2,7:x1+=1:y1-=1:if x1>dx then x1=dx:if y1=0 then y1=1
								case 3,6:x1+=1:y1+=1:if x1>dx then x1=dx:if y1>dy then y1=dy
								case 4,5:x1-=1:y1-=1:if x1=0 then x1=1:if y1=0 then y1=1
							end select
						loop until i=l
						i=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if utp=0 then
									cstate(outstate,i)=cstate(instate,gr1(x,y))
								else
									cstate(outstate,gr1(x,y))=cstate(instate,i)
								end if
								if i=l then exit for,for
							next x
						next y
					case 20 'spiral
						dx=key2(h,1)
						dy=key2(h,2)
						utp=key2(h,3)
						j=key2(h,4)
						c=key2(h,5)
						i=0
						for y=1 to dy
							for x=1 to dx
								i+=1
								if i>l then gr0(x,y)=0 else gr0(x,y)=1 'cstate(instate,i)
								gr1(x,y)=0
								gr2(x,y)=0
							next x
						next y
						select case c
							case 1:x1=1:y1=1:d=1
							case 2:x1=1:y1=1:d=3
							case 3:x1=dx:y1=1:d=2
							case 4:x1=dx:y1=1:d=3
							case 5:x1=1:y1=dy:d=1
							case 6:x1=1:y1=dy:d=4
							case 7:x1=dx:y1=dy:d=2
							case 8:x1=dx:y1=dy:d=4
						end select
						x=x1:y=y1:i=0
						do
							gr2(x,y)=1
							if gr0(x,y)>0 andalso gr1(x,y)=0 then i+=1:gr1(x,y)=i
							select case d
								case 1:x+=1
									if x=dx or gr2(x+1,y)>0 then
										if y+1<=dy andalso gr2(x,y+1)=0 then d=3
										if y-1>0 andalso gr2(x,y-1)=0 then d=4
									end if
								case 2:x-=1
									if x=1 or gr2(x-1,y)>0 then
										if y+1<=dy andalso gr2(x,y+1)=0 then d=3
										if y-1>0 andalso gr2(x,y-1)=0 then d=4
									end if
								case 3:y+=1
									if y=dy or gr2(x,y+1)>0 then
										if x+1<=dx andalso gr2(x+1,y)=0 then d=1
										if x-1>0 andalso gr2(x-1,y)=0 then d=2
									end if
								case 4:y-=1
									if y=1 or gr2(x,y-1)>0 then
										if x+1<=dx andalso gr2(x+1,y)=0 then d=1
										if x-1>0 andalso gr2(x-1,y)=0 then d=2
									end if
							end select
						loop until i=l
						if j=0 then i=0 else i=l
						for y=1 to dy
							for x=1 to dx
								if j=0 then i+=1 else i-=1
								if utp=0 then
									cstate(outstate,i)=cstate(instate,gr1(x,y))
								else
									cstate(outstate,gr1(x,y))=cstate(instate,i)
								end if
								if i=l or i=0 then exit for,for
							next x
						next y
					case 21 'period from-to
						k=0:c=0
						utp=key2(h,3)
						p=key2(h,4)
						for i=key2(h,1) to key2(h,2)
							c+=1
							li0(c)=cstate(instate,i)
						next i
						for i=1 to p
							for j=i to c step p
								k+=1
								if utp=0 then
									li1(j)=li0(k)
								else
									li1(k)=li0(j)
								end if
							next j
						next i
						c=0
						for i=1 to l
							if i>=key2(h,1) andalso i<=key2(h,2) then
								c+=1
								cstate(outstate,i)=li1(c)
							else
								cstate(outstate,i)=cstate(instate,i)
							end if
						next i
					'case 22 'move
					'	j=0
					'	for i=1 to l
					'		li0(i)=0
					'	next i
					'	for i=key2(h,1) to key2(h,2) step key2(h,3)
					'		li0(i)=cstate(instate,i)
					'	next i
					'	for i=1 to l
					'		if li0(i)=0 then
					'			j+=1
					'			cstate(outstate,j)=cstate(instate,i)
					'		end if
					'	next i
					'	for i=key2(h,1) to key2(h,2) step key2(h,3)
					'		j+=1
					'		cstate(outstate,j)=cstate(instate,i)
					'	next i
				end select
				if m=0 then
					for i=1 to l
						cstate(ccs2+h,i)=cstate(ccs+h,i)
					next i
				end if
			next m
		next h
			
		sleep 0.001 'twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if timer-soltimer>1 andalso improved=1 then
				soltimer=timer
				if solvesub_transpositionbatchciphers=1 then hc="Batch cipher: "+itemname+lb else hc=""
				hc+="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+lb
				ui_editbox_settext(output_text,hc+os)
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>0 then
					its+=1
					
					'ptcp(thread(t).ioc2,0)+=thread(t).pccycles
					'ptcp(thread(t).ioc2,1)+=1
					
					new_score=thread(t).score
					'newscore=thread(t).ngrams/thread(t).ioc
					'newscore=thread(t).pccycles
					if new_score>old_score then
						'tabuits+=1
						'if tabuits>tabumax then
						'	tabuits=1
						'	taburej=0
						'end if
						old_score=new_score
						for i=0 to stacksize
							for j=0 to argmax
								key0(i,j)=thread(t).hkey(i,j)
								'tabu(tabuits,i,j)=key0(i,j)
							next j
						next i
						if new_score>best_score then
							improved=1
							best_score=new_score+0.00001
							
							'if solvesub_transpositionbatchciphers=1 then os="Batch cipher: "+itemname+lb else os=""
							'os+="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+lb
							'os+="*** DEV *** Avg score: "+rdc(avgscore2/br1_restarts,2)+lb
							'os+="Avg score: "+rdc(avgscore/restarts,2)+" Tabuits: "+str(tabuits)+" Taburej: "+str(taburej)+lb
							
							os=""
							#include "thread_solve_output.bi"
							os+=lb+thread(t).itemname+lb+lb
							os+=info_to_string(thread(t).sol(),l,dx0,dy0,0,solvesub_addspaces,0)
							'os+=info_to_string(thread(t).cip(),l,dx0,dy0,0)
							
							'os+=lb+lb
							'for i=0 to l
							'	if ptcp(i,1)>0 then
							'		os+=str(i)+": "+format(ptcp(i,0)/ptcp(i,1),"0.00")+" ("+str(ptcp(i,1))+")"+lb
							'	end if
							'next i
							
							ui_editbox_settext(output_text,os)
						end if
					else
						old_score-=temp
					end if
					if its=itsmax then
						its=0
						'itsmax+=itsplus
						iterations*=iterationsfactor
						itsmax*=hciterationsfactor
						temp=tempstart
	 					temp_min=tempstart/itsmax
	 					avg_score+=best_score
	 					if best_score>best_score2 then best_score2=best_score
	 					restarts+=1
	 					old_score=0
	 					if solvesub_transpositionbatchciphers=1 then
	 						best_score=0
	 						improved=0
	 					end if
	 					'tabuits=0
	 					'taburej=0
	 					global_best_score=0	
	 					'erase key0,key1,key2			
	 					for i=0 to stacksize 'erase key0,key1,key2 ',tabu
	 						for j=0 to argmax
	 							key0(i,j)=0
	 							key1(i,j)=0
	 							key2(i,j)=0
	 						next j
	 					next i
	 					for i=1 to threads
					 		thread(i).solver_stop=1
					 		do
								sleep 0.001
					 		loop until thread(i).solver_waiting=1
					 		thread(i).score=0
	 					next i
	 					'redim crc(0)
	 					'redim crc(2^32)
	 					if solvesub_transpositionbatchciphers=1 then
		 					br1_curr+=1
		 					if br1_curr=br1 then
		 						avg_score2+=best_score2
		 						best_score2=0
		 						br1_restarts+=1
		 						restarts=0
		 						br1_curr=0
		 						if cipherend=2 then exit do
		 						cipherend=0
		 						meta=0
								itemname=""
		 					end if
	 					end if
	 					
	 					exit for
					end if
					temp-=temp_min
				end if				
				
				thread(t).outputdir=outputdir 'basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx0
				thread(t).dim_y=dy0
				thread(t).score=0
				if meta=1 then item=itemname+lb+lb+item
				thread(t).itemname=item	
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				thread(t).pcmode=solvesub_pcmode
				thread(t).advstats=solvesub_advstats
				thread(t).ioc2=mv_best
				
				if solvesub_pcmode=1 then
					for i=1 to l
						thread(t).key(i)=cstate(ccs3+stacksize,i)
					next i
				end if
				for i=1 to l
					thread(t).cip(i)=cstate(ccs+stacksize,i)
				next i
				'crc(crc_32(@cstate(ccs+stacksize,1),l))=1
				for i=0 to stacksize
					for j=0 to argmax
						thread(t).hkey(i,j)=key2(i,j)
					next j
				next i		
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				
				'iterations*=solvesub_iterationsfactor
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
				exit for
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	close #1
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	'redim crc(0)
	
	clean_thread_information
	solvesub_transpositionbatchciphers=0
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_substitution(byval none as any ptr)
	
	dim as integer i,j,k,a,p,t,its
	dim as integer iterations=solvesub_iterations
	dim as double best_score
	dim as string os
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as short cip(l)
	dim as short cip2(l)
	dim as short key(l)
	dim as long sol(l)
	dim as integer ngram_score
	dim as byte use_tm
	
	for i=1 to l
		cip(i)=nuba(i)
	next i
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	task_active=lcase(ui_listbox_gettext(list_main,ui_listbox_getcursel(list_main)))
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	'string_to_info(ui_editbox_gettext(output_text)) 'allow transposition matrix
	'if info_length=l andalso info_symbols=l then
	'	use_tm=1
	'	for i=1 to l
	'		key(i)=info(i)
	'		cip2(info(i))=cip(i)
	'	next i
	'end if
	
	dim as integer restarts,avgits,cc,timertest=1 'solve time test
	dim as double avgsolvetime,avgrestarts
	dim as short sol2(l)
	if timertest=1 then
		dim as string comp2=string_to_info(ui_editbox_gettext(output_text))
		for i=1 to l
			sol2(i)=info(i)
		next i
	end if
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).score>best_score then
				
				best_score=thread(t).score+0.00001
				
				os=""
				#include "thread_solve_output.bi"
				if timertest=1 andalso avgits>0 then os+="Its: "+str(avgits)+" AVG time:"+str(stt(avgsolvetime/avgits))+" AVG items: "+format(avgrestarts/avgits,"0.00")+lb
				os+=lb
				
				select case task_active
					
					case "higher-order homophonic"
						
						for i=0 to solvesub_higherorderhomophonic-1
							for j=1 to thread(t).l
								sol(j)=thread(t).gkey(j,i)
							next j
							os+=info_to_string(sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
							if i<>(solvesub_higherorderhomophonic-1) then os+=lb+lb
						next i
						
					case "substitution + sparse polyalphabetism"
						
						os+="Polyalphabetism: "+format((1-thread(t).match)*100,"0.00")+"%"+lb+lb
						os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
						
					case else
						
						os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
						'os+=lb+info_to_string(thread(t).key(),thread(t).l,dx,dy,1)
						
				end select
				
				ui_editbox_settext(output_text,os)
				
				if timertest=1 then
					cc=0
					for i=1 to l
						if sol2(i)=thread(t).sol(i) then cc+=1
					next i
					if cc/l>=0.75 then '75% accurate
						for i=1 to threads
							thread(i).solver_stop=1
							do
								sleep 10
							loop until thread(i).solver_waiting=1
							thread(i).score=0
							thread(i).restarts_completed=0
							thread(i).avgscore=0
							thread(i).avgioc=0
							thread(i).avgpccycles=0
						next i
						sleep 100
						avgits+=1
						avgsolvetime+=thread(t).sectime
						avgrestarts+=restarts
						iterations=solvesub_iterations
						restarts=0
						best_score=0
						global_best_score=0
						sectimer=timer
						exit for
					end if
				end if
				
			end if
			
			if thread(t).solver_waiting=1 andalso best_score>=thread(t).score then
				
				if thread(t).score>0 then restarts+=1
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0	
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).matchweight=solvesub_matchweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				
				'if use_tm=0 then
					thread(t).pcmode=0
					for j=1 to l
						thread(t).cip(j)=cip(j)
					next j
				'else
				'	thread(t).pcmode=solvesub_pcmode
				'	for j=1 to l
				'		thread(t).cip(j)=cip2(j)
				'		thread(t).key(j)=key(j)
				'	next j
				'end if
				
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				iterations*=solvesub_iterationsfactor
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if	
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for t=1 to threads
		thread(t).solver_stop=1
		do
			sleep 10
		loop until thread(t).solver_waiting=1 or timer-stucktimer>2
	next t
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_units(byval none as any ptr)
	
	dim as integer h,i,j,k,a,p,t,r,e,l2,s2,x,y,kl
	dim as integer iterations=solvesub_iterations
	dim as double new_score,best_score
	dim as string os,hc,key,modestring
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer num=info_numerical
	dim as integer improved=0
	dim as short cip(constcip)
	dim as short nba(constcip*2)
	dim as short sym(s)
	dim as short tkey(l)
	dim as short tmpline(constcip)
	
	dim as integer restarts,its,cits
	dim as integer subrestarts=3
	dim as double itsmax=solvesub_hciterations
	dim as double temp_start=3000/threads
	dim as double temp '=temp_start
	dim as double temp_min '=temp_start/itsmax
	
	dim as integer sri=1,srj=1
	dim as integer srits(10,10)
	dim as integer itsmaxpsr=itsmax/subrestarts
	dim as short bkey(subrestarts,l)
	
	dim as short klstart=1
	dim as short klend=0
	dim as short unittype=0
	dim as short unitmode=0
	dim as short unitperiod=1
	dim as short unitperiodutp=1
	dim as short hseqsize=3
	dim as short replaceleft=0
	dim as short replaceright=0
	dim as short replacesymbols=1
	
	'type 0: symbol
	'type 1: row
	'type 2: column
	'type 3: group
	
	'mode 0: remove unit
	'mode 1: expand unit
	'mode 2: separate unit
	'mode 3: replace unit
	'mode 4: reverse unit
	'mode 5: offset unit
		
	if ui_radiobutton_getcheck(radiobutton_units_symbol)=1 then unittype=0
	if ui_radiobutton_getcheck(radiobutton_units_row)=1 then unittype=1
	if ui_radiobutton_getcheck(radiobutton_units_column)=1 then unittype=2
	if ui_radiobutton_getcheck(radiobutton_units_hseq)=1 then unittype=3
	
	select case unittype
		case 0 'symbol
			if ui_radiobutton_getcheck(radiobutton_units_remove)=1 then unitmode=0
			if ui_radiobutton_getcheck(radiobutton_units_expand)=1 then unitmode=1
			if ui_radiobutton_getcheck(radiobutton_units_separate)=1 then
				ui_editbox_settext(output_text,"Error: separate not supported with symbol")
				exit sub
			end if
			if ui_radiobutton_getcheck(radiobutton_units_replace)=1 then unitmode=3
			if ui_radiobutton_getcheck(radiobutton_units_reverse)=1 then
				ui_editbox_settext(output_text,"Error: reverse not supported with symbol")
				exit sub
			end if
		case 1 'row
			if ui_radiobutton_getcheck(radiobutton_units_remove)=1 then unitmode=0
			if ui_radiobutton_getcheck(radiobutton_units_expand)=1 then unitmode=1
			if ui_radiobutton_getcheck(radiobutton_units_separate)=1 then unitmode=2
			if ui_radiobutton_getcheck(radiobutton_units_replace)=1 then
				ui_editbox_settext(output_text,"Error: replace not supported with row")
				exit sub
			end if
			if ui_radiobutton_getcheck(radiobutton_units_reverse)=1 then unitmode=4
		case 2 'column
			if ui_radiobutton_getcheck(radiobutton_units_remove)=1 then unitmode=0
			if ui_radiobutton_getcheck(radiobutton_units_expand)=1 then unitmode=1
			if ui_radiobutton_getcheck(radiobutton_units_separate)=1 then unitmode=2
			if ui_radiobutton_getcheck(radiobutton_units_replace)=1 then
				ui_editbox_settext(output_text,"Error: replace not supported with column")
				exit sub
			end if
			if ui_radiobutton_getcheck(radiobutton_units_reverse)=1 then unitmode=4
		case 3 'group
			if ui_radiobutton_getcheck(radiobutton_units_remove)=1 then unitmode=0
			if ui_radiobutton_getcheck(radiobutton_units_expand)=1 then unitmode=1
			if ui_radiobutton_getcheck(radiobutton_units_separate)=1 then unitmode=2
			if ui_radiobutton_getcheck(radiobutton_units_replace)=1 then unitmode=3
			if ui_radiobutton_getcheck(radiobutton_units_reverse)=1 then unitmode=4
	end select
	
	dim as double old_solvesub_multiplicityweight=solvesub_multiplicityweight
	solvesub_multiplicityweight=val(ui_editbox_gettext(editbox_units_mulweight))
	unitperiod=val(ui_editbox_gettext(editbox_units_period))
	hseqsize=val(ui_editbox_gettext(editbox_units_hseqsize))
	replaceleft=val(ui_editbox_gettext(editbox_units_replaceleft))
	replaceright=val(ui_editbox_gettext(editbox_units_replaceright))
	replacesymbols=val(ui_editbox_gettext(editbox_units_replacesymbols))
	klstart=val(ui_editbox_gettext(editbox_units_klstart))
	klend=val(ui_editbox_gettext(editbox_units_klend))
	
	if ui_radiobutton_getcheck(radiobutton_units_period_tp)=1 then unitperiodutp=0
	if ui_radiobutton_getcheck(radiobutton_units_period_utp)=1 then unitperiodutp=1
	if unitperiod<1 or unitperiod>=l then unitperiod=1
	
	for i=1 to l
		nba(i)=nuba(i)
		sym(nba(i))=info(i)
	next i
	
	ips_timer=timer
	dim as double statustimer=timer
	dim as double soltimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	task_active="substitution + units"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	kl=klstart

	h=0 'init sub restarts
	for i=1 to subrestarts
		srits(i,0)=subrestarts-(i-1)
		k=0
		do
			for j=1 to subrestarts-(i-1)
				h+=1
				k+=1
				srits(i,j)+=1
				if k=int(itsmax/subrestarts) then exit do
			next j
		loop
	next i
	if h<itsmax then srits(subrestarts,1)+=itsmax-h
	
	temp=temp_start-((temp_start/subrestarts)*(sri-1))
	temp_min=temp/srits(sri,srj)
	
	select case unittype 'init new random key
		case 0 to 2 'symbol, row, column
			for i=1 to kl
				do
					e=0
					select case unittype
						case 0:r=int(rnd*s)+1 'symbol
						case 1:r=int(rnd*dy)+1 'row
						case 2:r=int(rnd*dx)+1 'column
					end select
					for j=1 to i-1
						if r=bkey(1,j) then
							e=1
							exit for
						end if
					next j
				loop until e=0
				bkey(1,i)=r
			next i
		case 3 'horizontal sequence
			for i=1 to kl
				do
					e=0
					r=int(rnd*(l-(hseqsize-1)))+1
					for j=1 to i-1
						if abs(r-bkey(1,j))<hseqsize then
							e=1
							exit for
						end if
					next j
				loop until e=0
				bkey(1,i)=r
			next i
	end select
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if timer-soltimer>1 andalso improved=1 then
				soltimer=timer
				hc="Restart: "+str(restarts+1)+" Hill climber: "+str(cits)+"/"+str(int(itsmax))+" @ "+str(int(iterations))+lb
				ui_editbox_settext(output_text,hc+os)
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>new_score then
					
					for i=1 to kl
						bkey(sri,i)=thread(t).key(i)
					next i
					
					new_score=thread(t).score
					
					if new_score>best_score then
						improved=1
						best_score=new_score+0.00001
						'os="Restart: "+str(restarts+1)+" Hill climber: "+str(cits)+"/"+str(int(itsmax))+" @ "+str(int(iterations))+lb
						os=""
						#include "thread_solve_output.bi"
						os+=lb+thread(t).itemname+lb+lb
						os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
						'ui_editbox_settext(output_text,os)
					end if
					
				else
					
					if thread(t).score>0 then new_score-=temp
					
				end if
				
				if thread(t).score>0 then
				
					its+=1
					cits+=1
					temp-=temp_min
				
				end if
				
				'-------------------------------------------------------------------
				
				if its>srits(sri,srj) then
					
					its=0
					srj+=1
					new_score=0
					
					for i=1 to threads
				 		thread(i).solver_stop=1
				 		do
							sleep 0.001
				 		loop until thread(i).solver_waiting=1
				 		thread(i).score=0
					next i
					
					if srj>srits(sri,0) then
						
						srj=1
						sri+=1
						
						if sri>subrestarts then 'restart solver
						
							sri=1
							srj=1
							cits=0
							'bestscore=0
							restarts+=1
							kl+=1
							if klend>0 then
								if kl>klend then kl=klstart
							else			
								select case unittype 'check for max keylength
									case 0 'symbol
										if kl=s then kl=klstart
									case 1 'row
										if kl=dy then kl=klstart
									case 2 'column
										if kl=dx then kl=klstart
									case 3 'horizontal sequence
										if kl=l/hseqsize then kl=klstart
								end select
							end if
							
							'global_best_score=0
							itsmax*=solvesub_hciterationsfactor
							iterations*=solvesub_iterationsfactor
							
							h=0 'init sub restarts
							erase srits
							for i=1 to subrestarts
								srits(i,0)=subrestarts-(i-1)
								k=0
								do
									for j=1 to subrestarts-(i-1)
										h+=1
										k+=1
										srits(i,j)+=1
										if k=int(itsmax/subrestarts) then exit do
									next j
								loop
							next i
							if h<itsmax then srits(subrestarts,1)+=itsmax-h
						
						end if
						
					end if
					
					temp=temp_start-((temp_start/subrestarts)*(sri-1))
					temp_min=temp/srits(sri,srj)
					
					if sri=1 then 'init new random key
					
						select case unittype
							case 0 to 2 'symbol, row, column
								for i=1 to kl
									do
										e=0
										select case unittype
											case 0:r=int(rnd*s)+1 'symbol
											case 1:r=int(rnd*dy)+1 'row
											case 2:r=int(rnd*dx)+1 'column
										end select
										for j=1 to i-1
											if r=bkey(1,j) then
												e=1
												exit for
											end if
										next j
									loop until e=0
									bkey(1,i)=r
								next i
							case 3 'horizontal sequence
								for i=1 to kl
									do
										e=0
										r=int(rnd*(l-(hseqsize-1)))+1
										for j=1 to i-1
											if abs(r-bkey(1,j))<hseqsize then
												e=1
												exit for
											end if
										next j
									loop until e=0
									bkey(1,i)=r
								next i
						end select
						
					else 'take best key from previous sub restart
					
						for i=1 to kl
							bkey(sri,i)=bkey(sri-1,i)
						next i
					
					end if	
				
				end if
				
				for i=1 to kl
					tkey(i)=bkey(sri,i)
				next i
				
				select case unittype 'make random change to key
					case 0 to 2 'symbol, row, column
						k=int(rnd*kl)+1
						do
							e=0
							select case unittype
								case 0:r=int(rnd*s)+1 'symbol
								case 1:r=int(rnd*dy)+1 'row
								case 2:r=int(rnd*dx)+1 'column
							end select
							for j=1 to kl
								if r=tkey(j) then
									e=1
									exit for
								end if
							next j
						loop until e=0
						tkey(k)=r
					case 3 'horizontal sequence
						k=int(rnd*kl)+1
						do
							e=0
							do
								r=int(rnd*(l-(hseqsize-1)))+1
							loop until r<>tkey(k)
							for j=1 to kl
								if j<>k then
									if abs(r-tkey(j))<hseqsize then
										e=1
										exit for
									end if
								end if
							next j
						loop until e=0
						tkey(k)=r
				end select
				
				thread(t).itemname=""
				
				l2=0
				s2=s
				
				quicksort_short(tkey(),1,kl) 'sort key
				
				select case unittype 'apply key
					case 0 'symbol
						thread(t).itemname+="Unit: symbol"+lb
						key="Key("
						for j=1 to kl
							if num=0 then key+=chr(sym(tkey(j))) else key+=str(tkey(j))
							if j<>kl then key+=","
							thread(t).key(j)=tkey(j)
						next j
						select case unitmode
							case 0 'remove
								thread(t).itemname+="Mode: remove"+lb+key+")"
								for i=1 to l
									e=0
									for j=1 to kl
										if nba(i)=tkey(j) then
											e=1
											exit for
										end if
									next j
									if e=0 then
										l2+=1
										cip(l2)=nba(i)
									end if
								next i
							case 1 'expand
								thread(t).itemname+="Mode: expand"+lb+key+")"
								for i=1 to l
									e=0
									for j=1 to kl
										if nba(i)=tkey(j) then
											e=1
											exit for
										end if
									next j
									l2+=1
									if e=0 then
										cip(l2)=nba(i)
									else
										s2+=1
										cip(l2)=s2
									end if
								next i
							case 3 'replace
								thread(t).itemname+="Mode: replace("+str(replaceleft)+","+str(replaceright)+","+str(replacesymbols)+")"+lb+key+")"
								for i=1 to l
									e=0
									for j=1 to kl
										if nba(i)=tkey(j) then
											e=1
											exit for
										end if
									next j
									if e=0 then
										l2+=1
										cip(l2)=nba(i)
									else
										i+=replaceright
										for j=1 to replaceleft
											if l2-1>0 then
												l2-=1
												cip(l2)=0
											end if
										next j
										for j=1 to replacesymbols
											if l2+1<=constcip then
												l2+=1
												s2+=1
												cip(l2)=s2
											end if
										next j
									end if
								next i
						end select
					case 1 'row
						thread(t).itemname+="Unit: row("+str(dx)+","+str(dy)+")"+lb
						key="Key("
						for j=1 to kl
							key+=str(tkey(j))
							if j<>kl then key+=","
							thread(t).key(j)=tkey(j)
						next j
						k=0
						select case unitmode
							case 0:thread(t).itemname+="Mode: remove"+lb+key+")"
							case 1:thread(t).itemname+="Mode: expand"+lb+key+")"
							case 2:thread(t).itemname+="Mode: separate"+lb+key+")"
							case 4:thread(t).itemname+="Mode: reverse"+lb+key+")"
						end select
						for i=1 to l step dx
							k+=1
							e=0
							for j=1 to kl
								if k=tkey(j) then
									e=1
									exit for
								end if
							next j
							if e=0 then
								for j=0 to dx-1
									l2+=1
									cip(l2)=nba(i+j)
								next j
							else
								select case unitmode
									case 0 'remove
									case 1 'expand
										for j=0 to dx-1
											l2+=1
											s2+=1
											cip(l2)=s2
										next j
									case 2 'separate
										for j=0 to dx-1
											l2+=1
											cip(l2)=nba(i+j)+s
										next j
									case 4 'reverse
										for j=0 to dx-1
											tmpline(j)=nba(i+j)
										next j
										for j=0 to dx-1
											if tmpline((dx-1)-j)>0 then
												l2+=1
												cip(l2)=tmpline((dx-1)-j)
											end if
										next j
								end select
							end if
						next i
					case 2 'column
						thread(t).itemname+="Unit: column("+str(dx)+","+str(dy)+")"+lb
						key="Key("
						for j=1 to kl
							key+=str(tkey(j))
							if j<>kl then key+=","
							thread(t).key(j)=tkey(j)
						next j
						select case unitmode
							case 0:thread(t).itemname+="Mode: remove"+lb+key+")"
							case 1:thread(t).itemname+="Mode: expand"+lb+key+")"
							case 2:thread(t).itemname+="Mode: separate"+lb+key+")"
							case 4:thread(t).itemname+="Mode: reverse"+lb+key+")"
						end select
						for i=1 to dx
							e=0
							for j=1 to kl
								if i=tkey(j) then
									e=1
									exit for
								end if
							next j
							if e=0 then
								for j=0 to dy-1
									cip(i+(j*dx))=nba(i+(j*dx))
								next j
							else
								select case unitmode
									case 0 'remove
										for j=0 to dy-1
											cip(i+(j*dx))=0
										next j
									case 1 'expand
										for j=0 to dy-1
											s2+=1
											cip(i+(j*dx))=s2
										next j
									case 2 'separate
										for j=0 to dy-1
											cip(i+(j*dx))=nba(i+(j*dx))+s
										next j
									case 4 'reverse
										for j=0 to dy-1
											tmpline(j)=nba(i+(j*dx))
										next j
										for j=0 to dy-1
											if tmpline((dy-1)-j)>0 then
												cip(i+(j*dx))=tmpline((dy-1)-j)
											end if
										next j
								end select
							end if
						next i
						for i=1 to l
							if cip(i)>0 then
								l2+=1
								cip(l2)=cip(i)
							end if
						next i	
					case 3 'horizontal sequence
						thread(t).itemname+="Unit: horizontal sequence("+str(hseqsize)+")"+lb
						key="Key("
						for j=1 to kl
							key+=str(tkey(j))
							if j<>kl then key+=","
							thread(t).key(j)=tkey(j)
						next j
						select case unitmode
							case 0:thread(t).itemname+="Mode: remove"+lb+key+")"
							case 1:thread(t).itemname+="Mode: expand"+lb+key+")"
							case 2:thread(t).itemname+="Mode: separate"+lb+key+")"
							case 3:thread(t).itemname+="Mode: replace"+lb+key+")"
							case 4:thread(t).itemname+="Mode: reverse"+lb+key+")"
						end select
						for i=1 to l
							e=0
							for j=1 to kl
								if i=tkey(j) then
									e=1
									exit for
								end if
							next j
							if e=0 then
								l2+=1
								cip(l2)=nba(i)
							else
								select case unitmode
									case 0 'remove
										i+=hseqsize-1
									case 1 'expand
										i+=hseqsize-1
										for j=0 to hseqsize-1
											l2+=1
											s2+=1
											cip(l2)=s2
										next j
									case 2 'separate
										i+=hseqsize-1
										for j=0 to hseqsize-1
											l2+=1
											cip(l2)=nba(i+j)+s
										next j
									case 3 'replace
										i+=(hseqsize-1)+replaceright
										for j=1 to replaceleft
											if l2-1>0 then
												l2-=1
												cip(l2)=0
											end if
										next j
										for j=1 to replacesymbols
											if l2+1<=constcip then
												l2+=1
												s2+=1
												cip(l2)=s2
											end if
										next j
									case 4 'reverse ??? bugged ???
										for j=0 to hseqsize-1
											tmpline(j)=nba(i+j)
										next j
										for j=0 to hseqsize-1
											if tmpline((hseqsize-1)-j)>0 then
												l2+=1
												cip(l2)=tmpline((hseqsize-1)-j)
											end if
										next j
										i+=hseqsize-1
								end select
							end if
						next i
				end select
				
				s2=nba_to_info_out_short(cip(),l2,l2) 're-nba cipher
				
				k=0
				for i=1 to unitperiod
					for j=i to l2 step unitperiod
						k+=1
						if unitperiodutp=0 then
							cip(j)=info_out(k) 'transpose
						else
							cip(k)=info_out(j) 'untranspose
						end if
					next j
				next i	
				
				'-------------------------------------------------------------------
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l2
				thread(t).s=s2
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0	
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0	
				thread(t).pcmode=0
				
				for j=1 to l2
					thread(t).cip(j)=cip(j)
				next j
				
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if	
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for t=1 to threads
		thread(t).solver_stop=1
		do
			sleep 10
		loop until thread(t).solver_waiting=1 or timer-stucktimer>2
	next t
	
	solvesub_multiplicityweight=old_solvesub_multiplicityweight
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_cribgrid(byval none as any ptr)
	
	dim as integer h,i,j,k,a,b,p,t,x,y,e,r,bi,bl,acits,avgits,restarts
	dim as integer iterations=solvesub_iterations
	dim as double best_score,avgtime
	dim as string os
	dim as integer l=wc_l 'info_length
	dim as integer s=wc_s 'info_symbols
	dim as integer dx=wc_dx 'info_x
	dim as integer dy=wc_dy 'info_y
	dim as short cip(l)
	dim as short nba(l)
	dim as short key(l)
	dim as double dbl(l)
	dim as long sol(l)
	dim as integer ngram_score,timertest,cc
	dim as double avgsolvetime,avgrestarts
	
	'for i=1 to l
	'	cip(i)=info(i)
	'	nba(i)=wc_nuba(i)
	'next i
	
	i=0
	for y=1 to dy
		for x=1 to dx
			i+=1
			nba(i)=wc_nuba(x,y)
			if i=l then exit for,for
		next x
	next y
	
	i=0
	for y=1 to dy
		for x=1 to dx
			i+=1
			if wc_pgrid(0,x,y)<>"" then
				key(nba(i))=alpharev(asc(wc_pgrid(0,x,y)))+1
			end if
			if i=l then exit for,for
		next x
	next y
	
	e=0
	for i=1 to s
		if key(i)=0 then e=1
	next i
	if e=0 then
		ui_editbox_settext(output_text,"Error: the entire cipher is cribbed")
		exit sub
	end if
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	task_active=lcase(ui_listbox_gettext(list_main,ui_listbox_getcursel(list_main)))
	
	dim as double old_solvesub_entweight=solvesub_entweight
	dim as double old_solvesub_ngramfactor=solvesub_ngramfactor
	dim as double old_solvesub_fastent=solvesub_fastent
	
	if task_active="bigram substitution" then
		select case ngram_size
			case 8:solvesub_entweight=0.65:solvesub_fastent=0
			case 9:solvesub_entweight=0.65:solvesub_fastent=0
			case 10:solvesub_entweight=0.5:solvesub_fastent=2
		end select
		normalize_ngramfactor
	end if
	
	if task_active="substitution + monoalphabetic groups" then
		select case ngram_size
			case 8:solvesub_entweight=0.1:solvesub_fastent=0
			case 9:solvesub_entweight=0.1:solvesub_fastent=0
			case 10:solvesub_entweight=0.075:solvesub_fastent=0
		end select
		normalize_ngramfactor
	end if
	
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	timertest=1 'solve time test
	dim as short sol2(l)
	if timertest=1 then
		dim as string comp2=string_to_info(ui_editbox_gettext(output_text))
		for i=1 to l
			sol2(i)=info(i)
		next i
	end if
	
	use_cribs=1
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).score>best_score then
				
				best_score=thread(t).score+0.00001
				os=""
				#include "thread_solve_output.bi"
				if solvesub_bigramautocrib>0 then os+="Auto-crib iterations: "+str(acits)+lb
				if timertest=1 andalso avgits>0 then os+="Its: "+str(avgits)+" AVG time:"+str(stt(avgsolvetime/avgits))+" AVG items: "+format(avgrestarts/avgits,"0.00")+lb
				if task_active="substitution + monoalphabetic groups" or task_active="substitution + mono groups + crib list"  or task_active="bigram substitution" then os+=lb+"Collisions: "+str(thread(t).ioc2)+lb
				os+=lb
				os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
				
				'select case task_active
				'	case "substitution + crib grid","substitution + monoalphabetic groups"
				'		mutexlock csolmutex
				'		for i=1 to thread(t).l
				'			csol(i,100)=alpharev(thread(t).sol(i))
				'		next i
				'		csol(0,100)=1
				'		mutexunlock csolmutex
				'end select
				
				if solvesub_bigrambestsol=1 andalso task_active="bigram substitution" then
					mutexlock csolmutex
					for i=1 to thread(t).l
						csol(i,100)=alpharev(thread(t).sol(i))
					next i
					csol(0,100)=1
					mutexunlock csolmutex
				end if
				
				ui_editbox_settext(output_text,os)
				
				if timertest=1 then 
					cc=0
					for i=1 to l
						if sol2(i)=thread(t).sol(i) then cc+=1
					next i
					if cc/l>=0.75 then '>75% accurate
						avgits+=1
						avgsolvetime+=thread(t).sectime
						avgrestarts+=restarts
						restarts=0
						for i=1 to threads 'stop solving/threads
							thread(i).solver_stop=1
							do
								sleep 10
							loop until thread(i).solver_waiting=1
							thread(i).score=0
							thread(i).restarts_completed=0
							thread(i).avgscore=0
							thread(i).avgioc=0
							thread(i).avgpccycles=0
						next i
						sleep 10
						'clean_thread_information
						mutexlock csolmutex
						erase csol
						mutexunlock csolmutex
						iterations=solvesub_iterations
						best_score=0
						global_best_score=0
						sectimer=timer
						'ips_timer=timer
						exit for
					end if
				end if
				
			end if
			
			if thread(t).solver_waiting=1 andalso best_score>=thread(t).score then
				
				if thread(t).score>0 then restarts+=1
				
				if solvesub_bigramheatmap=1 andalso task_active="bigram substitution" then 'output MCL heatmap
					if thread(t).score>solvesub_scoreover then
						mutexlock csolmutex
						dim as integer csn=csol(0,0)
						for i=1 to l
							h=0
							for j=0 to ngram_alphabet_size-1
								if csol(i,j)>h then
									h=csol(i,j)
									e=j 'letter
								end if
								dbl(i)=h
								sol(i)=alphabet(e) 'sol(i)=e+65
							next j
						next i
						mutexunlock csolmutex
						dim as byte old_info_numerical=info_numerical
						info_numerical=0
						output_colormap(dbl(),sol(),l,dx,dy,0,"MCL heatmap "+str(csn))
						info_numerical=old_info_numerical
					end if
				end if
				
				if solvesub_bigramautocrib>0 andalso task_active="bigram substitution" then 'auto-crib
					r=0
					for i=1 to threads
						r+=thread(i).restarts_completed
					next i
					if r>=solvesub_bigramautocrib then
						for i=1 to threads 'stop solving/threads
							thread(i).solver_stop=1
							do
								sleep 10
							loop until thread(i).solver_waiting=1
							thread(i).score=0
							thread(i).restarts_completed=0
							thread(i).avgscore=0
							thread(i).avgioc=0
							thread(i).avgpccycles=0
						next i
						sleep 10
						acits+=1
						iterations=solvesub_iterations
						best_score=0
						global_best_score=0
						sectimer=timer
						'ips_timer=timer
						h=0
						for i=1 to l 'get best letter @ position to auto-crib
							for j=0 to ngram_alphabet_size-1
								if csol(i,j)>h then
									if key(nba(i))=0 then 'check if unused crib
										h=csol(i,j)
										bi=i 'position
										bl=j 'letter	
									end if
								end if
							next j
						next i
						key(nba(bi))=bl+1
						i=0
						for y=1 to wc_dy 'update crib grid (not working atm)
							for x=1 to wc_dx
								i+=1
								if nba(i)=nba(bi) then
									wc_pgrid(0,x,y)=chr(alpharev(bl))
									ui_editbox_settext(wc_cribs(x,y),wc_pgrid(0,x,y))
								end if
							next x
						next y
						mutexlock csolmutex
						erase csol
						mutexunlock csolmutex
					end if
				end if
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0	
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				
				for i=1 to s
					thread(t).ckey(i)=key(i)
				next i
				
				thread(t).pcmode=0
				for i=1 to l
					thread(t).cip(i)=nba(i)
				next i
				
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				iterations*=solvesub_iterationsfactor
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
		
	stoptask=0
	
	dim as double stucktimer=timer
	for t=1 to threads
		thread(t).solver_stop=1
		do
			sleep 10
		loop until thread(t).solver_waiting=1 or timer-stucktimer>2
	next t
	
	solvesub_entweight=old_solvesub_entweight
	solvesub_ngramfactor=old_solvesub_ngramfactor
	solvesub_fastent=old_solvesub_fastent
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	use_cribs=0
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_criblist(byval none as any ptr)
	
	dim as integer i,j,k,a,p,t,x,y,e
	dim as integer iterations=solvesub_iterations
	dim as double best_score
	dim as string os,cw,all
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as short cip(l)
	dim as short nba(l)
	dim as short key(constcip)
	dim as integer ngram_score
	dim as byte use_tm

	task_active=lcase(ui_listbox_gettext(list_main,ui_listbox_getcursel(list_main)))

	if task_active="substitution + mono groups + crib list" then
		select case ngram_size
			case 8:solvesub_entweight=0.1:solvesub_fastent=0
			case 9:solvesub_entweight=0.1:solvesub_fastent=0
			case 10:solvesub_entweight=0.075:solvesub_fastent=0
		end select
		normalize_ngramfactor
	else
		task_active="substitution + crib list"	
	end if

	update_solver_status


	dim as string filename=ui_loadsavedialog(0,"Open crib list",filter,1,basedir+"\Misc\")
	
	if fileexists(filename)=0 then
		if filename<>"" then ui_editbox_settext(output_text,"Error: file not found")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	for i=1 to l
		cip(i)=info(i)
		nba(i)=nuba(i)
	next i
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	open filename for input as #1
	
	use_cribs=1
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>best_score then
					best_score=thread(t).score+0.00001
					
					os=""
					#include "thread_solve_output.bi"
					
					os+=lb
					os+=thread(t).itemname+lb
					os+=lb
					os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
					ui_editbox_settext(output_text,os)
				end if	
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0	
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				
				'crib stuff
				'------------------------------------------------------------------------
				
				e=0
				dim as string crib=""
				dim as short cribpos=0
				
				do
					if eof(1) then exit do,do
					line input #1,cw
					crib=right(cw,len(cw)-5)
					cribpos=val(left(cw,5))
					if cribpos>0 andalso cribpos+len(crib)<=l andalso len(crib)>0 then e=1
				loop until e=1
				
				erase key
				
				for i=0 to len(crib)-1
					key(nba(cribpos+i))=alpharev(asc(crib,i+1))+1
				next i
				
				thread(t).itemname="Position: "+str(cribpos)+" Crib: "+crib
				
				for i=1 to s
					thread(t).ckey(i)=key(i) 'crib key
				next i
				
				'------------------------------------------------------------------------
				
				thread(t).pcmode=0
				for i=1 to l
					thread(t).cip(i)=nba(i)
				next i
			
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if	
				
			end if
			
		next t
		
	loop until stoptask=1
	
	close #1
	
	dim as double stucktimer=timer
	for t=1 to threads
		if stoptask=1 then thread(t).solver_stop=1
		do
			sleep 10
		loop until thread(t).solver_waiting=1 'or timer-stucktimer>2
	next t
	
	stoptask=0
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	use_cribs=0
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_wordcribs(byval none as any ptr)
	
	dim as integer i,j,k,a,p,e,t,c,e1,e2,m,m2,r
	dim as double iterations=solvesub_iterations
	dim as double best_score,new_score,old_score,brun_score
	dim as string os,abc,crib
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as short cip(l)
	dim as short cip2(l)
	dim as short nba(l)
	dim as short key(l),skey(s),bkey(10,l)
	dim as integer ngram_score
	dim as byte use_tm,constraints,currsolved
	
	dim as short tc,cc=1
	dim as short cribs(l,l)
	crib=ui_editbox_gettext(output_text)
	dim as short alphab(ngram_alphabet_size-1)
	
	for i=0 to ngram_alphabet_size-1
		alphab(i)=alphabet(i)
	next i
	ui_editbox_settext(output_text,"Please wait...")
	
	if len(crib)=0 then
		ui_editbox_settext(output_text,"Error: no crib placed in the output window")
		exit sub
	end if
	for i=1 to len(crib)
		e=0
		if tc>l then exit for
		for j=0 to ngram_alphabet_size-1	
			if asc(crib,i)=alphab(j) then
				e=1
				tc+=1
				cribs(cc,0)+=1
				cribs(cc,cribs(cc,0))=j+1
				exit for
			else
				if asc(crib,i)=13 then 'new crib
					e=1
					i+=1
					if cribs(cc,0)>0 then cc+=1
					exit for
				end if
			end if
		next j
		if e=0 then
			for j=0 to ngram_alphabet_size-1
				abc+=chr(alphabet(j))
			next j
			ui_editbox_settext(output_text,"Error: crib letters are limited to "+str(abc))
			exit sub
		end if
	next i
	if tc=0 then
		ui_editbox_settext(output_text,"Error: no crib placed in the output window")
		exit sub
	end if
	if tc=l then
		ui_editbox_settext(output_text,"Error: too many or very long crib(s)") 'can't seem to fit word cribs in the cipher anywhere
		exit sub
	end if
	if cribs(cc,0)=0 then cc-=1
	
	for i=1 to l
		cip(i)=nuba(i)
		nba(i)=nuba(i)
	next i
	
	randomize timer
	
	m=0
	
	use_cribs=1
	
	do 'init random key
	
		e1=0
		m2=0
		
		do 'get random non-overlapping positions	
		
			e2=0
			for i=1 to cc
				key(i)=int(rnd*(l-(cribs(i,0)-1)))+1 'position
			next i
			for i=1 to cc-1
				for j=i+1 to cc
					if key(i)=key(j) then e2=1:exit for,for
					if key(i)<key(j) andalso key(i)+cribs(i,0)>=key(j) then e2=1:exit for,for
					if key(i)>key(j) andalso key(j)+cribs(j,0)>=key(i) then e2=1:exit for,for
				next j
			next i
			
			m2+=1
			if m2=100000000 then '100,000,000
				ui_editbox_settext(output_text,"Error: too many or very long crib(s)") 'can't seem to fit word cribs in the cipher anywhere
				exit sub
			end if	
			
		loop until e2=0
		
		for i=1 to s
			skey(i)=0
		next i
		for i=1 to cc 'check if key fits cipher constraints
			for j=0 to cribs(i,0)-1
				if skey(nba(key(i)+j))=0 then
					skey(nba(key(i)+j))=cribs(i,j+1)
				else
					if skey(nba(key(i)+j))<>cribs(i,j+1) then e1=1:exit for,for
				end if
			next j
		next i
		
		m+=1
		if m=100000000 then '100,000,000
			ui_editbox_settext(output_text,"Error: too many or very long crib(s)") 'can't seem to fit word cribs in the cipher anywhere
			exit sub
		end if
		
	loop until e1=0
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	task_active="substitution + word cribs"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	dim as integer itsmax=solvesub_hciterations
	dim as integer sri,srj,citsmax,itsmax2,restarts,its,solved,cits,oldkey
	dim as integer sr(10,10),srimax=4 'sub restarts levels
	for i=1 to srimax
		sr(i,0)=srimax-(i-1)
	next i
	sri=1
	srj=1
	for i=1 to srimax
		for j=1 to sr(i,0)
			sr(i,j)=(itsmax/srimax)/sr(i,0)
			itsmax2+=sr(i,j)
		next j
	next i
	sr(srimax,1)+=itsmax-itsmax2
	citsmax=sr(sri,srj)
	
	'100=21.95%
	'200=30.68%
	
	dim as double tempstart=(200/30)*threads
	dim as double temp=tempstart-((tempstart/srimax)*(sri-1)) 'try individual values
	dim as double tempmin=temp/citsmax
	dim as double currbest
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>new_score then
					
					new_score=thread(t).score
					
					for i=1 to cc 'get key from thread
						key(i)=thread(t).gkey(i,0)
					next i
				
					if new_score>best_score then
						
						best_score=new_score+0.00001
						if best_score>brun_score then brun_score=best_score
						if best_score>solvesub_pnover andalso currsolved=0 then
							solved+=1
							currsolved=1
						end if
						
						'os="Best score: "+format(brunscore,"0.00")+" Over "+str(solvesub_pnover)+": "+str(solved)+" ("+format((solved/(restarts+1))*100,"0.00")+"%)"+lb
						os="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(int(itsmax))+" @ "+str(int(iterations))+lb
						
						#include "thread_solve_output.bi"
						
						os+=lb+thread(t).itemname+lb+lb
						os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
						ui_editbox_settext(output_text,os)
						
					end if
					
				else
					
					if thread(t).score>0 then new_score-=temp
					
				end if
				
				if thread(t).score>0 then
					
					its+=1
					cits+=1
					temp-=tempmin
					
					if thread(t).score>currbest then 'save best key for sub restarts
						currbest=thread(t).score
						for i=1 to cc
							bkey(sri,i)=key(i)
						next i
					end if
					
					'if solvesub_pnoverskip=1 then
					'	if bestscore>=solvesub_pnover then
					'		cits=citsmax
					'		srj=srimax
					'		sri=srimax
					'	end if
					'end if
					
					if cits=citsmax then
						
						for i=1 to threads
					 		thread(i).solver_stop=1
					 		do
								sleep 0.001
					 		loop until thread(i).solver_waiting=1
					 		thread(i).score=0
						next i
						
						srj+=1
						if srj>sr(sri,0) then
							sri+=1
							srj=1
							currbest=0
							if sri>srimax then
								sri=1
								srj=1
								its=0
								restarts+=1
								currsolved=0
								itsmax*=solvesub_hciterationsfactor
								iterations*=solvesub_iterationsfactor
								global_best_score=0
								best_score=0
								itsmax2=0
								for i=1 to srimax
									for j=1 to sr(i,0)
										sr(i,j)=(itsmax/srimax)/sr(i,0)
										itsmax2+=sr(i,j)
									next j
								next i
								sr(srimax,1)+=itsmax-itsmax2
							end if
						end if
						
						if sri=1 then 'init random key
							'm=0
							do 'init random key
								e1=0
								do 'get random non-overlapping positions
									e2=0
									for i=1 to cc
										key(i)=int(rnd*(l-(cribs(i,0)-1)))+1 'position
									next i
									for i=1 to cc-1
										for j=i+1 to cc
											if key(i)=key(j) then e2=1:exit for,for
											if key(i)<key(j) andalso key(i)+cribs(i,0)>=key(j) then e2=1:exit for,for
											if key(i)>key(j) andalso key(j)+cribs(j,0)>=key(i) then e2=1:exit for,for
										next j
									next i
								loop until e2=0
								for i=1 to s
									skey(i)=0
								next i
								for i=1 to cc 'check if key fits cipher constraints
									for j=0 to cribs(i,0)-1
										if skey(nba(key(i)+j))=0 then
											skey(nba(key(i)+j))=cribs(i,j+1)
										else
											if skey(nba(key(i)+j))<>cribs(i,j+1) then e1=1:exit for,for
										end if
									next j
								next i
								
								'm+=1
								'if m=10000000 then
								'	stoptask=1
								'	constraints=1
								'	exit do,do
								'end if
								
							loop until e1=0
						else	
							for i=1 to cc
								key(i)=bkey(sri-1,i)
							next i
						end if
					
						cits=0
						citsmax=sr(sri,srj)
						temp=tempstart-((tempstart/srimax)*(sri-1))
						tempmin=temp/citsmax
						new_score=0
						
					end if
					
				end if
				
				m2=0
				do 'find and make valid random change to key
					m=0
					e1=0
					m2+=1
					r=int(rnd*cc)+1 'crib
					do
						m+=1
						if m=l*10 then e1=1:exit do
						e2=0
						'do
							p=int(rnd*(l-(cribs(r,0)-1)))+1 'position
						'loop until p<>key(r)
						if p<>key(r) then
							for i=1 to cc
								if i<>r then
									if key(i)=p then e2=1:exit for
									if key(i)<p andalso key(i)+cribs(i,0)>=p then e2=1:exit for
									if key(i)>p andalso p+cribs(r,0)>=key(i) then e2=1:exit for
								end if
							next i
						else
							e2=1
						end if
					loop until e2=0
					oldkey=key(r)
					key(r)=p
					for i=1 to s
						skey(i)=0
					next i
					for i=1 to cc 'check if key fits cipher constraints
						for j=0 to cribs(i,0)-1
							if skey(nba(key(i)+j))=0 then
								skey(nba(key(i)+j))=cribs(i,j+1)
							else
								if skey(nba(key(i)+j))<>cribs(i,j+1) then e1=1:exit for,for
							end if
						next j
					next i
					if e1=1 then key(r)=oldkey
					
					if m2=100000000 then '100,000,000
						stoptask=1
						constraints=1
						exit do
					end if
					
				loop until e1=0
				
				crib=""
				for i=1 to s
					skey(i)=0
				next i
				for i=1 to cc 'export key to thread
					thread(t).gkey(i,0)=key(i)
					for j=0 to cribs(i,0)-1
						skey(nba(key(i)+j))=cribs(i,j+1)
						crib+=chr(alphab(cribs(i,j+1)-1))
					next j
					crib+=" cribbed at position "+str(key(i))
					if i<>cc then crib+=lb
				next i
				thread(t).itemname=crib
				
				e=0
				for i=1 to s
					if skey(i)=0 then e=1
					thread(t).key(i)=skey(i)
				next i
				if e=0 then
					stoptask=1
					constraints=1
					exit do
				end if
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0	
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				
				'if use_tm=0 then 'use transposition matrix
					thread(t).pcmode=0
					for i=1 to l
						thread(t).cip(i)=cip(i)
					next i
				'else
				'	thread(t).pcmode=solvesub_pcmode
				'	for j=1 to l
				'		thread(t).cip(j)=cip2(j)
				'		thread(t).key(j)=key(j)
				'	next j
				'end if
				
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for t=1 to threads
		thread(t).solver_stop=1
		do
			sleep 10
		loop until thread(t).solver_waiting=1 or timer-stucktimer>2
	next t
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	if constraints=1 then ui_editbox_settext(output_text,"Error: too many or very long crib(s)") 'can't seem to fit word cribs in the cipher anywhere
	
	clean_thread_information
	use_cribs=1
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_benchmark(byval none as any ptr)
	
	dim as integer i,j
	dim as integer iterations=solvesub_iterations
	dim as double bestscore,mips
	dim as string os
	dim as integer ngram_score,its,itsmax=10000
	dim as string cp="HER>pl^VPk|1LTG2dNp+B(#O%DWY.<*Kf)By:cM+UZGW()L#zHJSpp7^l8*V3pO++RK2_9M+ztjd|5FP+&4k/p8R^FlO-*dCkF>2D(#5+Kq%;2UcXGV.zL|(G2Jfj#O+_NYz+@L9d<M+b+ZR2FBcyA64K-zlUV+^J+Op7<FBy-U+R/5tE|DYBpbTMKO2<clRJ|*5T4M.+&BFz69Sy#+N|5FBc(;8RlGFN^f524b.cV4t++yBX1*:49CE>VUZ5-+|c.3zBK(Op^.fMqG2RcT+L16C<+FlWB|)L++)WCzWcPOSHT/()p|FkdW<7tB_YOB*-Cc>MDHNpkSzZO8A|K;+"
	string_to_info(cp)
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer cip(l)
	dim as integer state=12345
	dim as short r1,r2
	
	for i=1 to l
		cip(i)=nuba(i)
	next i
	
	'ui_listbox_setcursel(list_main,0)
	set_solverhighlight("substitution")
	toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",4,1,threads) 'stop solver
	toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",2,1,threads) 'stop thread
	toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",1,1,threads) 'start thread
	
	dim as double statustimer=timer
	'dim as double statustimer2=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	task_active="benchmark"
	dim as short old_solvesub_outputdir=solvesub_outputdir
	solvesub_outputdir=0
	
	update_solver_status
	
	ips_timer=timer
	
	do
		
		sleep twait
		
		for i=1 to threads
			
			if thread(i).solver_waiting=1 then
				
				if thread(i).score>0 then its+=1
			
				if timer-statustimer>1 then
					os="BHdecrypt benchmark status: "+str(its)+"/"+str(itsmax)
					ui_editbox_settext(output_text,os)
					statustimer=timer
					update_solver_status
				end if
				
				if its=itsmax then
					for i=1 to threads
						iterations+=thread(i).iterations_completed
					next i
					mips=iterations/(timer-ips_timer)/1000000
					exit do
				end if
				
				thread(i).outputdir=basedir+"\Output\"
				thread(i).l=l
				thread(i).s=s
				thread(i).dim_x=dx
				thread(i).dim_y=dy
				thread(i).score=0
				thread(i).pcmode=0
				thread(i).advstats=solvesub_advstats
				thread(i).iterations=iterations
				thread(i).temperature=300 'solvesub_temperature
				thread(i).restarts=1 'solvesub_restarts
				thread(i).subrestartlevels=solvesub_subrestartlevels
				thread(i).ngramfactor=solvesub_ngramfactor
				thread(i).multiplicityweight=0 'solvesub_multiplicityweight
				thread(i).entweight=solvesub_entweight
				thread(i).solver_stop=0
				
				for j=1 to l 'randomize 340
					state=(22695477*state+1)and 2147483647
					r1=int(state/2147483648*26)
					state=(22695477*state+1)and 2147483647
					r2=int(state/2147483648*26)
					swap cip(r1),cip(r2)
				next j
				
				for j=1 to l
					thread(i).cip(j)=cip(j)
				next j
				thread(i).update=0
				thread(i).solver_waiting=0 'engage thread
				'iterations*=solvesub_iterationsfactor
				
			end if
			
		next i
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	if its=itsmax then
		os="BHdecrypt benchmark completed:"+lb
		os+="---------------------------------------------------------"+lb
		os+="MIPS: "+format(mips,"0.00000")
		ui_editbox_settext(output_text,os)
	else
		ui_editbox_settext(output_text,"")
	end if
	
	solvesub_outputdir=old_solvesub_outputdir
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_rectangles(byval none as any ptr)
	
	'vertical rectangles
	'variable period per rectangle
	'2d period
	
	dim as integer i,j,k,t,r,e,a,x,y,x1,x2,y1,y2,sx1,xy,cl,p,utp,ki,ci
	dim as integer iterations=solvesub_iterations
	dim as double d,best_score,old_score,new_score
	dim as string os
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long nba(l)
	dim as long cip(l)
	dim as long cip2(l)
	dim as long cip3(l)
	dim as long mat1(l)
	dim as long mat2(l)
	dim as long grid(dx,dy)
	dim as long grid2(dx,dy)
	dim as integer ngram_score
	
	for i=1 to l
		nba(i)=nuba(i)
	next i
	
	i=0
	for y=1 to dy
		for x=1 to dx
			i+=1
			grid(x,y)=nuba(i)
			if i=l then exit for,for
		next x
	next y
	
	if dy=1 then
		ui_editbox_settext(output_text,"Error: rows < 2")
		return
	end if
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	task_active="substitution + rectangles"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	dim as integer bkey(l,2),key(l,2)
	dim as integer kl=1 'divisions
	dim as integer restarts,its
	dim as double itsmax=solvesub_hciterations
	dim as double temp_start=3000/threads
	dim as double temp=temp_start
	dim as double temp_min=temp_start/itsmax
	
	'op0=period
	'op1=scytale
	
	'area0=characters
	'area1=by rows
	
	dim as integer op=0 'operation
	dim as integer area=0 'area mode
	dim as integer varperiod=0 'variable period
	
	select case area
		case 0 'horizontal (by characters)
			for i=1 to kl 'new key
				do
					e=0
					r=int(rnd*(l-1))+1
					for j=1 to i-1
						if bkey(j,2)=r then e=1
					next j
				loop until e=0
				bkey(i,2)=r
			next i
		case 1 'horizontal (by rows)
			for i=1 to kl 'new key
				do
					e=0
					r=int(rnd*(dy-1))+1
					for j=1 to i-1
						if bkey(j,2)=r then e=1
					next j
				loop until e=0
				bkey(i,2)=r
			next i
	end select
	for i=0 to kl
		if varperiod=0 then
			bkey(i,1)=solvesub_pnperiod
		else
			bkey(i,1)=1
		end if
	next i
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>new_score then
					
					new_score=thread(t).score
					
					if new_score>best_score+0.00001 then
						
						best_score=new_score
						
						os="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(int(itsmax))+" @ "+str(int(iterations))+lb
						
						#include "thread_solve_output.bi"
						
						os+=lb
						os+=thread(t).itemname+lb
						os+=lb
						for i=1 to thread(t).l
							thread(t).sol(i)=thread(t).sol(i)
						next i
						os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
						ui_editbox_settext(output_text,os)
					
					end if
					
					for i=0 to kl
						for j=0 to 2
							bkey(i,j)=thread(t).gkey(i,j)
						next j
					next i
					
				else
					
					if thread(t).score>0 then new_score-=temp
					
				end if
				
				if thread(t).score>0 then
				
					its+=1
					temp-=temp_min
				
				end if
				
				if its=int(itsmax) then
					
					kl+=1
					if kl=dy-1 then kl=1
					its=0
					restarts+=1
					best_score=0
					global_best_score=0
					itsmax*=solvesub_hciterationsfactor
					iterations*=solvesub_iterationsfactor
					temp=temp_start
					temp_min=temp_start/itsmax
					
					for i=1 to threads
				 		thread(i).solver_stop=1
				 		do
							sleep 0.001
				 		loop until thread(i).solver_waiting=1
				 		thread(i).score=0
					next i
					
					select case area
						case 0 'horizontal (by characters)
							for i=1 to kl 'new key
								do
									e=0
									r=int(rnd*(l-1))+1
									for j=1 to i-1
										if bkey(j,2)=r then e=1
									next j
								loop until e=0
								bkey(i,2)=r
							next i
						case 1 'horizontal (by rows)
							for i=1 to kl 'new key
								do
									e=0
									r=int(rnd*(dy-1))+1
									for j=1 to i-1
										if bkey(j,2)=r then e=1
									next j
								loop until e=0
								bkey(i,2)=r
							next i
					end select
					for i=0 to kl
						if varperiod=0 then
							bkey(i,1)=solvesub_pnperiod
						else
							bkey(i,1)=1
						end if
					next i
					
				end if
				
				for i=0 to kl
					for j=0 to 2
						key(i,j)=bkey(i,j)
					next j
				next i
				
				if varperiod=1 then d=rnd else d=0
				
				if d<0.5 then
					select case area 'random change key
						case 0 'horizontal (by character)
							a=int(rnd*kl)+1
							do
								e=0
								r=int(rnd*(l-1))+1
								for j=1 to kl
									if key(j,2)=r then e=1
								next j
							loop until e=0
							key(a,2)=r
						case 1 'horizontal (by rows)
							a=int(rnd*kl)+1
							do
								e=0
								r=int(rnd*(dy-1))+1
								for j=1 to kl
									if key(j,2)=r then e=1
								next j
							loop until e=0
							key(a,2)=r
					end select
				else
					select case area
						case 0
							a=int(rnd*(kl+1))
							'do		
								select case a
									case 0:p=(int(rnd*key(1,2))+1)/2
									case is=kl:p=(int(rnd*(1+l-key(kl,2)))+1)/2
									case else:p=(int(rnd*(1+key(a+1,2)-key(a,2)))+1)/2
								end select
								if p<1 then p=1
							'loop until r<>key(a,1)
							key(a,1)=p
						case 1
							'...
							'....
							'.....
							'......
							'.......
					end select
				end if
				
				do 'sort key
					e=0
					for i=1 to kl-1
						if key(i,2)>key(i+1,2) then
							e=1
							for j=0 to 2
								swap key(i,j),key(i+1,j)
							next j
						end if
					next i
				loop until e=0
				
				'p=solvesub_pnperiod 'get period from nulls & skips solver
				
				ci=0
				for ki=0 to kl 'transpose
					
					select case area 'define boundaries
						case 0 'horizontal (by character)
							select case ki
								case 0:x1=1:x2=key(ki+1,2)
								case is=kl:x1=key(ki,2)+1:x2=l
								case else:x1=key(ki,2)+1:x2=key(ki+1,2)
							end select
						case 1 'horizontal (by rows)
							select case ki
								case 0:x1=1:x2=dx:y1=1:y2=key(ki+1,2)
								case is=kl:x1=1:x2=dx:y1=key(ki,2)+1:y2=dy
								case else:x1=1:x2=dx:y1=key(ki,2)+1:y2=key(ki+1,2)
							end select
					end select
					
					select case op
						
						case 0,1 'period
						
							cl=0
							select case area
								case 0 'horizontal (by characters)
									for x=x1 to x2
										cl+=1
										cip2(cl)=nba(x)
									next x
								case 1 'horizontal (by rows)
									for y=y1 to y2
										for x=x1 to x2
											if grid(x,y)>0 then
												cl+=1
												cip2(cl)=grid(x,y)
											end if
										next x
									next y
							end select
							
							k=0
							if op=0 then 
								utp=1 'period
							else 
								utp=0 'scytale
							end if
							
							p=key(ki,1)
							
							for i=1 to p 'period
								for j=i to cl step p
									k+=1
									if utp=0 then 'transpose
										cip3(j)=cip2(k)
										mat1(j)=k
									else 'untranspose
										cip3(k)=cip2(j)
										mat1(k)=j
									end if
								next j
							next i
							dim as long cij=ci
							for i=1 to cl
								ci+=1
								cip(ci)=cip3(i)
								mat2(ci)=mat1(i)+cij
							next i
						
						case 1 'other
						
					end select
					
				next ki
				
				select case area
					case 0:thread(t).itemname="By characters, key("
					case 1:thread(t).itemname="By rows, key("
				end select
				
				for i=1 to kl
					thread(t).itemname+=str(key(i,2))
					if i<>kl then thread(t).itemname+=","
				next i
				thread(t).itemname+=")"
				
				for i=0 to kl
					for j=0 to 2
						thread(t).gkey(i,j)=key(i,j)
					next j
				next i
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0
				thread(t).pcmode=1 'solvesub_pcmode
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).matchweight=solvesub_matchweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				
				for i=1 to l
					thread(t).cip(i)=cip(i)
				next i
				if solvesub_pcmode=1 then
					for i=1 to l
						thread(t).key(i)=mat2(i)
					next i
				end if
				
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_mergeseqhom(byval none as any ptr)
	
	dim as integer t,i,j
	dim as integer iterations=solvesub_iterations
	dim as double best_score
	dim as string os
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer num=info_numerical
	dim as integer cip(l)
	dim as integer cip2(l)
	dim as integer nba(l)
	dim as integer key(l)
	dim as short old_ngram_alphabet_size=ngram_alphabet_size
	dim as short mas=26,use_tm
	
	for i=1 to l
		cip(i)=info(i) 'info(i)
		nba(i)=nuba(i)
	next i
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	ngram_alphabet_size=solvesub_cyclealphabetsize
	if ngram_alphabet_size>s then ngram_alphabet_size=s
	
	task_active="merge sequential homophones"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	'string_to_info(ui_editbox_gettext(output_text)) 'allow tranposition matrix
	'if info_length=l andalso info_symbols=l then
	'	use_tm=1
	'	for i=1 to l
	'		key(i)=info(i)
	'		cip2(info(i))=cip(i)
	'	next i
	'end if
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>best_score then
					best_score=thread(t).score+0.00001
					
					os=""
					#include "thread_solve_output.bi"
					
					os+=lb
					os+="Symbols: "+str(thread(t).effectivesymbols)+ " Cycle IOC: "+format(thread(t).ioc2,"0.00000")+lb
					os+=lb
					os+=info_to_string(thread(t).sol(),l,dx,dy,num,0,0)
					ui_editbox_settext(output_text,os)
				end if
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).num=num
				thread(t).score=0
				thread(t).pcmode=0
				thread(t).advstats=solvesub_advstats
				
				if solvesub_rndcyclearg=0 then
					thread(t).cyclelengthweight=solvesub_cyclelengthweight
					thread(t).cyclealphabetsize=solvesub_cyclealphabetsize
				else
					thread(t).cyclelengthweight=(int(rnd*5001)+5000)/10000
					thread(t).cyclealphabetsize=int(rnd*(s-(mas-1)))+mas
				end if
				
				if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
					thread(t).itemname="Target alphabet size: "+str(thread(t).cyclealphabetsize)+" Cycle length weight: "+format(thread(t).cyclelengthweight,"0.0000")
				end if
				
				thread(t).cyclesizeweight=solvesub_cyclesizeweight
				thread(t).cyclequalityweight=solvesub_cyclequalityweight
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).matchweight=solvesub_matchweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				
				'for j=1 to l
				'	thread(t).cip(j)=cip(j)
				'next j
				
				if use_tm=0 then
					thread(t).pcmode=0
					for j=1 to l
						thread(t).cip(j)=cip(j)
					next j
				else
					thread(t).pcmode=solvesub_pcmode
					for j=1 to l
						thread(t).cip(j)=cip2(j)
						thread(t).key(j)=key(j)
					next j
				end if
				
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread		
				iterations*=solvesub_iterationsfactor
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
		thread(i).num=0
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	ngram_alphabet_size=old_ngram_alphabet_size
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_rowbound(byval none as any ptr)
	
	dim as integer t,h,i,j,k,r,cl
	dim as integer iterations=solvesub_iterations
	dim as double best_score
	dim as string os
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long cip(constcip)
	dim as long sol(constcip*2)
	dim as integer ngram_score
	dim as integer ngramsize2=ngram_size
	dim as double ngramfactor2=solvesub_ngramfactor
	dim as double iocweight2=solvesub_entweight
	dim as integer fastent2=solvesub_fastent
	dim as double temp2=solvesub_temperature
	dim as string oldname=solver_file_name_ngrams
	dim as string old_file_name_ngrams=solvesub_ngramloc
	dim as integer dontkeep,maxk
	
	r=1
	for i=1 to l
		cl+=1
		k+=1
		if k>maxk then maxk=k
		cip(cl)=nuba(i)
		if rlen(r)=k then
			k=0
			r+=1
			cl+=1
			if cl>constcip then
				ui_editbox_settext(output_text,"Error: cipher too long")
				exit sub
			end if
			cip(cl)=12345
		end if
	next i
	
	if maxk<2 then
		ui_editbox_settext(output_text,"Error: rows are too short")
		exit sub
	end if
	
	ngrams_clearprevious=0
	sleep 10
	if ngrams_inmem(ngram_size)=1 then 
		solvesub_ngramentweight(ngram_size)=solvesub_entweight
		solvesub_ngramfactor2(ngram_size)=solvesub_ngramfactor
	end if
	
	dim as string ngram_basedir=left(old_file_name_ngrams,instrrev(old_file_name_ngrams,"\"))
	
	for i=2 to ngramsize2
		if ngrams_inmem(i)=0 then
			solvesub_ngramloctemp=ngram_basedir+str(i)+right(oldname,len(oldname)-1)
			if fileexists(solvesub_ngramloctemp)=0 then
				solvesub_ngramloctemp=ngram_basedir+str(i)+right(oldname,len(oldname)-1)
				if fileexists(solvesub_ngramloctemp)=0 then 
					ui_editbox_settext(output_text,"Error: n-grams not part of a set")
					exit sub
				end if
			end if
			sleep 10
			task_active="loading "+str(i)+"-grams"
			update_solver_status
			thread_ptr(threadsmax+3)=threadcreate(@thread_load_ngrams,0)
			do
				sleep 10
			loop until task_active="none"
			ngrams_inmem(i)=1
			if i=ngramsize2 then dontkeep=1
			solvesub_ngramentweight(i)=solvesub_entweight
			solvesub_ngramfactor2(i)=solvesub_ngramfactor
		end if
	next i
	
	solvesub_ngramloc=old_file_name_ngrams
	
	ngrams_clearprevious=1
	
	if dontkeep=0 then
		solvesub_entweight=iocweight2
		ngram_size=ngramsize2
		solvesub_ngramfactor=ngramfactor2
		solvesub_temperature=temp2
		solvesub_fastent=fastent2
		solver_file_name_ngrams=oldname
	end if
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	task_active="substitution + row bound"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).score>best_score then
				
				best_score=thread(t).score+0.00001
				os=""
				#include "thread_solve_output.bi"
				os+=lb+"Average n-gram size: "+format(thread(t).tmpd1,"0.00000")+lb+lb
				k=0 'output
				r=1
				for j=1 to l
					k+=1
					sol(k)=thread(t).sol(j)
					if rlen(r)=k then
						os+=info_to_string(sol(),k,dx,dy,0,solvesub_addspaces,1)
						os+=" ("+str(int(thread(t).graph(r)))+")"
						if j<>l then
							os+=lb
							k=0
							r+=1
						end if
					end if
				next j
				ui_editbox_settext(output_text,os)
				
			end if
			
			if thread(t).solver_waiting=1 andalso best_score>=thread(t).score then
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0
				thread(t).pcmode=0
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				for j=1 to cl
					thread(t).cip(j)=cip(j)
				next j
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread		
				iterations*=solvesub_iterationsfactor
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	'solvesub_ngramfactor=ngramfactor2
	'solvesub_entweight=iocweight2
	'solvesub_fastent=fastent2
	'solvesub_temperature=temp2
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_rowbound_fragments(byval none as any ptr)
	
	dim as integer e,t,h,i,j,k,r,cl,r1,r2
	dim as integer iterations=solvesub_iterations
	dim as double new_score,old_score,best_score,best_score2,score
	dim as string os,hc
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as long cip(constcip),nba(constcip),frq(constcip),sol(constcip)
	dim as short key(constcip),ckey(constcip)
	dim as integer ngram_score
	dim as integer ngramsize2=ngram_size
	dim as double ngramfactor2=solvesub_ngramfactor
	dim as double iocweight2=solvesub_entweight
	dim as integer fastent2=solvesub_fastent
	dim as double temp2=solvesub_temperature
	dim as string oldname=solver_file_name_ngrams
	dim as string old_file_name_ngrams=solvesub_ngramloc
	dim as integer dontkeep,maxk,restarts
	
	dim as integer fragments1=solvesub_rowboundfragments-1
	'dim as integer fragments2=solvesub_rowboundcribfragments-1
	dim as integer fragments2=fragments1
	dim as integer its,cits,fragments=fragments1
	dim as double itsmax=solvesub_hciterations
	dim as double temp_start=solvesub_rowboundtemp/threads '2000
	dim as double temp '=temp_start
	dim as double temp_min '=temp/itsmax
	dim as integer over=solvesub_rowboundover
	dim as integer solved,hcmode,currsolved,bestki
	dim as double old_itsmax=itsmax
	dim as double old_twait,old_temp_start,old_iterations
	
	dim as integer subrestarts=solvesub_rowboundsubrestarts
	dim as integer sri=1,srj=1
	dim as integer srits(10,10)
	dim as integer itsmaxpsr=itsmax/subrestarts
	if fragments1>fragments2 then i=fragments1 else i=fragments2
	dim as short bkey(subrestarts,i)
	
	dim as integer left1,right1
	dim as double finetune=solvesub_rowboundfine
	dim as double hcmode1itfact=solvesub_rowboundhcmode1itfact
	dim as double distinct=solvesub_rowbounddistinct
	dim as integer hkeyi,maxhkey=solvesub_rowboundhkeys
	dim as integer distinctmode=solvesub_rowbounddistinctmode
	dim as integer outputhistory=solvesub_rowboundcheckhistory
	dim as double minh,hkey(maxhkey,s),bestk,tempcurr
	dim as short hmatchlist(maxhkey)
	
	for i=1 to l
		nba(i)=nuba(i)
	next i
	
	for i=1 to subrestarts 'init sub restarts
		srits(i,0)=subrestarts-(i-1)
		k=0
		do
			for j=1 to subrestarts-(i-1)
				h+=1
				k+=1
				srits(i,j)+=1
				if k=int(itsmax/subrestarts) then exit do
			next j
		loop
	next i
	if h<itsmax then srits(subrestarts,1)+=itsmax-h
	temp=temp_start-((temp_start/subrestarts)*(sri-1))
	temp_min=temp/srits(sri,srj)
	
	'tempcurr=temp_start
	'temp=tempcurr
	'temp_min=temp/srits(sri,srj)
	
	if fragments>l-1 then
		ui_editbox_settext(output_text,"Error: fragments > cipher length")
		exit sub
	end if
	if solvesub_rowboundcribfragments>l-1 then
		ui_editbox_settext(output_text,"Error: crib fragments > cipher length")
		exit sub
	end if
	
	ngrams_clearprevious=0
	sleep 10
	if ngrams_inmem(ngram_size)=1 then 
		solvesub_ngramentweight(ngram_size)=solvesub_entweight
		solvesub_ngramfactor2(ngram_size)=solvesub_ngramfactor
	end if
	
	dim as string ngram_basedir=left(old_file_name_ngrams,instrrev(old_file_name_ngrams,"\"))
	
	for i=2 to ngramsize2
		if ngrams_inmem(i)=0 then
			solvesub_ngramloctemp=ngram_basedir+str(i)+right(oldname,len(oldname)-1)
			if fileexists(solvesub_ngramloctemp)=0 then
				solvesub_ngramloctemp=ngram_basedir+str(i)+right(oldname,len(oldname)-1)
				if fileexists(solvesub_ngramloctemp)=0 then
					ui_editbox_settext(output_text,"Error: n-grams not part of a set")
					exit sub
				end if
			end if
			sleep 10
			task_active="loading "+str(i)+"-grams"
			update_solver_status
			thread_ptr(threadsmax+3)=threadcreate(@thread_load_ngrams,0)
			do
				sleep 10
			loop until task_active="none"
			ngrams_inmem(i)=1
			if i=ngramsize2 then dontkeep=1
			solvesub_ngramentweight(i)=solvesub_entweight
			solvesub_ngramfactor2(i)=solvesub_ngramfactor
		end if
	next i
	
	solvesub_ngramloc=old_file_name_ngrams
	
	ngrams_clearprevious=1
	
	if dontkeep=0 then
		solvesub_entweight=iocweight2
		ngram_size=ngramsize2
		solvesub_ngramfactor=ngramfactor2
		solvesub_temperature=temp2
		solvesub_fastent=fastent2
		solver_file_name_ngrams=oldname
	end if
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	task_active="substitution + row bound fragments"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	sectimer=timer
	old_twait=twait
	
	for i=1 to fragments 'generate random fragment distribution
		do
			e=0
			bkey(1,i)=int(rnd*l)+1
			for j=1 to i-1
				if bkey(1,i)=bkey(1,j) then
					e=1
					exit for
				end if
			next j
		loop until e=0
	next i
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>new_score then
					
					new_score=thread(t).score
					
					if new_score>best_score then
						
						best_score=thread(t).score+0.00001
						if best_score>best_score2 then best_score2=best_score
						if best_score>over and currsolved=0 then 
							solved+=1
							currsolved=1
						end if
						
						'os="Best score: "+format(bestscore2,"0.00")+" Over "+str(over)+": "+str(solved)+" ("+format((solved/restarts)*100,"0.00")+"%)"+lb
						os="Restart: "+str(restarts+1)+" Hill climber: "+str(cits)+"/"+str(int(itsmax))+" @ "+str(int(iterations))+lb
						#include "thread_solve_output.bi"
						os+=lb+"Average n-gram size: "+format(thread(t).tmpd1,"0.00000")+lb+lb
						
						r=1
						i=0
						for j=1 to l
							i+=1
							'os+=chr(thread(t).sol(j))
							sol(i)=thread(t).sol(j)
							e=0
							for k=1 to fragments
								if thread(t).key(k)=j then
									e=1
									exit for
								end if
							next k
							if e=1 or j=l then
								os+=info_to_string(sol(),i,dx,dy,0,solvesub_addspaces,1)
								os+=" ("+str(int(thread(t).graph(r)))+")"
								if j<>l then
									i=0
									os+=lb
									r+=1
								end if
							end if
						next j
						ui_editbox_settext(output_text,os)
					end if
					
					for i=1 to fragments
						bkey(sri,i)=thread(t).key(i)
					next i
					
				else
					
					if thread(t).score>0 then new_score-=temp
					
				end if
				
				if thread(t).score>0 then
					
					its+=1
					cits+=1
					temp-=temp_min
					
					if hcmode=0 then 'keep history of best distinct substitution keys
						if thread(t).score>hkey(0,0) then
							for i=1 to l
								ckey(nba(i))=alpharev(thread(t).sol(i)) 'get substitution key
							next i
							bestk=0
							hmatchlist(0)=0
							for i=1 to maxhkey
								k=0
								for j=1 to s
									if ckey(j)=hkey(i,j) then k+=1
								next j
								if k/s>bestk then 
									hmatchlist(0)+=1
									hmatchlist(hmatchlist(0))=i
									bestk=k/s
									bestki=i
								end if
							next i
							if bestk>distinct then 'key exist, update best match if better
								if thread(t).score>hkey(bestki,0) then 
									hkey(bestki,0)=thread(t).score
									for i=1 to s
										hkey(bestki,i)=ckey(i)
									next i
								end if
								if distinctmode=1 then 'delete other worse matching keys
									for i=1 to hmatchlist(0)
										if i<>bestki then
											hkey(i,0)=0
											for j=1 to s
												hkey(i,j)=0
											next j
										end if
									next i
								end if
							else 'key appears distinct to all other keys, make new entry
								for i=1 to maxhkey
									if hkey(0,0)=hkey(i,0) then
										hkey(i,0)=thread(t).score
										for j=1 to s
											hkey(i,j)=ckey(j)
										next j
										exit for
									end if
								next i
							end if
							minh=999999999
							for i=1 to maxhkey
								if hkey(i,0)<minh then
									minh=hkey(i,0)
									hkey(0,0)=hkey(i,0)
								end if
							next i
						end if
					end if
					
				end if
				
				if its>srits(sri,srj) or best_score>over then
					
					its=0
					srj+=1
					new_score=0
					
					for j=1 to threads 'stop threads
				 		thread(j).solver_stop=1
				 		do
							sleep 0.001
				 		loop until thread(j).solver_waiting=1
				 		thread(j).score=0
					next j
					
					if srj>srits(sri,0) or best_score>over then
						
						srj=1
						sri+=1
						
						if sri>subrestarts or best_score>over then 'restart solver
							
							select case hcmode
								case 0:hcmode=1
								case 1:hcmode=0
							end select
							
							if hcmode=1 then 'sort hkey (needs optimize)
								do
									e=0
									for i=1 to maxhkey-1
										if hkey(i,0)<hkey(i+1,0) then
											e=1
											for j=0 to s
												swap hkey(i,j),hkey(i+1,j)
											next j
										end if
									next i
								loop until e=0
							end if
							
							if best_score>over then hcmode=0
							
							sri=1
							srj=1
							cits=0
							best_score=0
							
							if hcmode=0 then 'normal
								global_best_score=0
								currsolved=0
								use_cribs=0
								fragments=fragments1
								restarts+=1
								itsmax=old_itsmax
								if old_iterations>0 then iterations=old_iterations
								'if old_temp_start>0 then temp_start=old_temp_start
								if old_twait>0 then twait=old_twait
								itsmax*=solvesub_hciterationsfactor
								old_itsmax=itsmax
								iterations*=solvesub_iterationsfactor
								hkeyi=0
								minh=0
								for i=0 to maxhkey
									for j=0 to s
										hkey(i,j)=0
									next j
								next i
							end if
							
							if hcmode>0 then 'assume best solution as crib
								if outputhistory=1 then
									dim as string outputhistory=""
									for i=1 to maxhkey 'check history
										for j=1 to s
											outputhistory+=chr(alphabet(hkey(i,j)))
										next j
										outputhistory+=" "+format(hkey(i,0),"0.00")+lb
									next i
									ui_editbox_settext(output_text,outputhistory)
									sleep 10000 '???
								end if
								hcmode=2
								hkeyi+=1
								for i=1 to s
									ckey(i)=hkey(hkeyi,i)
								next i
								if hkeyi=maxhkey then 
									hcmode=1
								else
									if hkey(hkeyi+1,0)=0 then hcmode=1
								end if
								use_cribs=2
								fragments=fragments2
								itsmax=old_itsmax*hcmode1itfact
								if itsmax=0 then itsmax=1
								'old_iterations=iterations
								'iterations=1
								'old_temp_start=temp_start
								'temp_start*=2
								'old_twait=twait
								twait=0.001
							end if
							
							h=0 'init sub restarts
							erase srits
							for i=1 to subrestarts
								srits(i,0)=subrestarts-(i-1)
								k=0
								do
									for j=1 to subrestarts-(i-1)
										h+=1
										k+=1
										srits(i,j)+=1
										if k=int(itsmax/subrestarts) then exit do
									next j
								loop
							next i
							if h<itsmax then srits(subrestarts,1)+=itsmax-h
							
						end if
						
					end if
					
					temp=temp_start-((temp_start/subrestarts)*(sri-1))
					temp_min=temp/srits(sri,srj)
					
					if sri=1 then 'init new random key
						'tempcurr=temp_start
						'temp=tempcurr
						'temp_min=temp/srits(sri,srj)
						for i=1 to fragments 'generate random key
							do
								e=0
								bkey(1,i)=int(rnd*l)+1
								for j=1 to i-1
									if bkey(1,i)=bkey(1,j) then
										e=1
										exit for
									end if
								next j
							loop until e=0
						next i
					else 'take best key from previous sub restart
						'tempcurr/=3
						'temp=tempcurr
						'temp_min=temp/srits(sri,srj)
						for i=1 to fragments
							bkey(sri,i)=bkey(sri-1,i)
						next i
					end if
					
				end if
				
				for i=1 to fragments 'get bestkey
					key(i)=bkey(sri,i)
				next i
				
				do 'sort
					e=0
					for i=1 to fragments-1
						if key(i)<key(i+1) then
							e=1
							swap key(i),key(i+1)
						end if
					next i
				loop until e=0
				
				'erase frq
				'for i=0 to fragments-1
				'	frq(i+1)=key(i+1)-key(i)
				'next i
				'frq(i+1)=l-key(fragments)
				'thread(t).itemname=format(m_ioc(frq(),l,fragments,"0.00"),2)
				
				if rnd>finetune then 'coarse random key rearrangement
					r1=int(rnd*fragments)+1
					do
						e=0
						r2=int(rnd*l)+1
						for i=1 to fragments
							if i<>r1 then
								if key(i)=r2 then
									e=1
									exit for
								end if
							end if
						next i
					loop until e=0
					key(r1)=r2
				else 'fine random key rearrangement
					do
						r1=int(rnd*fragments)+1
						if r1>1 then left1=key(r1-1) else left1=1
						if r1<fragments then right1=key(r1+1) else right1=l
						if right1-left1>2 then exit do 'add anti-stuck ???
					loop
					do
						r2=int(rnd*((right1-left1)-1))+(right1+1)
					loop until r2<>key(r1)	
				end if
				
				j=1 'push key to solver format
				cl=0
				for i=1 to l
					cl+=1
					cip(cl)=nba(i)
					for h=1 to fragments
						if key(h)=i then
							j+=1
							cl+=1
							cip(cl)=12345
							exit for
						end if
					next h
				next i
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0
				thread(t).pcmode=0
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				
				for j=1 to cl 'cipher
					thread(t).cip(j)=cip(j)
				next j
				for i=1 to fragments 'fragment key
					thread(t).key(i)=key(i)
				next i
				for i=1 to s 'crib key
					thread(t).ckey(i)=ckey(i)+1
				next i
				
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	use_cribs=0
	if old_twait>0 then twait=old_twait
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	'solvesub_ngramfactor=ngramfactor2
	'solvesub_entweight=iocweight2
	'solvesub_fastent=fastent2
	'solvesub_temperature=temp2
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_polyphones_hafer(byval none as any ptr)
	
	dim as integer h,i,j,k,t,r,ok1
	dim as integer iterations=solvesub_iterations
	dim as double best_score
	dim as string os,hc
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer num=info_numerical
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer nba(l)
	dim as integer threadsq
	dim as integer local_cpol(s)
	dim as integer local_multiplicityweight=val(ui_editbox_gettext(editbox_polyphones_mulweight))
	dim as integer its,itsmax=solvesub_hciterations
	dim as double temp_start=3000/threads,temp=temp_start
	dim as double temp_min=temp/itsmax
	dim as double new_score
	dim as integer shifts=2
	dim as integer hafermode=0
	dim as integer key(constfrq)
	dim as integer bkey(constfrq)
	dim as integer improved
	
	'solvesub_sdbias=val(ui_editbox_gettext(editbox_polyphones_sdbias))
	'if solvesub_sdbias<0 or solvesub_sdbias>1 then
	'	ui_editbox_settext(output_text,"Error: distribution/symbol bias must be in 0 to 1 range")
	'	return
	'end if
	
	if ui_radiobutton_getcheck(radiobutton_polyphones_hafer1)=1 then hafermode=0
	if ui_radiobutton_getcheck(radiobutton_polyphones_hafer2)=1 then hafermode=1
	
	for i=1 to l
		nba(i)=nuba(i)
	next i
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	task_active="substitution + polyphones [hafer]"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	for i=1 to s
		if freq(i)>=shifts then local_cpol(i)=shifts else local_cpol(i)=freq(i)
	next i
	
	dim as byte overridehc=ui_checkbox_getcheck(checkbox_polyphones_override)
	dim as integer local_iterations=solvesub_iterations
	dim as double local_iterationsfactor=solvesub_iterationsfactor
	dim as integer local_hciterations=solvesub_hciterations
	dim as double local_hciterationsfactor=solvesub_hciterationsfactor
	dim as double local_ngramfactor=solvesub_ngramfactor
	if overridehc=1 then
		select case hafermode
			case 0
				local_iterations=solvesub_iterations
				local_iterationsfactor=solvesub_iterationsfactor
				'local_hciterations=ngram_alphabet_size*2
			case 1
				local_iterations=500000
				local_iterationsfactor=2
				local_hciterations=ngram_alphabet_size^shifts
		end select
		iterations=local_iterations
		if local_hciterations<threads then local_hciterations=threads
		itsmax=local_hciterations
		local_hciterationsfactor=1
		temp_min=temp_start/itsmax
		local_ngramfactor*=(1+(local_multiplicityweight/10))
	end if
	
	bkey(0)=shifts
	select case hafermode
		case 1
			do
				'generalize to any # shifts
				r=int(rnd*2):if r=0 then bkey(1)=-int(rnd*ngram_alphabet_size) else bkey(1)=int(rnd*ngram_alphabet_size)
				r=int(rnd*2):if r=0 then bkey(2)=-int(rnd*ngram_alphabet_size) else bkey(2)=int(rnd*ngram_alphabet_size)
			loop until bkey(1)<>bkey(2) andalso abs(bkey(1))+abs(bkey(2))>0 andalso abs(bkey(1))+abs(bkey(2))<>ngram_alphabet_size
	end select
	
	sectimer=timer
	dim as double soltimer
	
	dim as integer restarts,avgits,cc,timertest=1 'solve time test
	dim as double avgsolvetime,avgrestarts
	dim as short sol2(l)
	if timertest=1 then
		dim as string comp2=string_to_info(ui_editbox_gettext(output_text))
		for i=1 to l
			sol2(i)=info(i)
		next i
	end if
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if timer-soltimer>1 andalso improved=1 then
				soltimer=timer
				if hafermode=1 then hc="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+lb
				ui_editbox_settext(output_text,hc+os)
			end if
			
			if thread(t).score>best_score then
				
				improved=1
				best_score=thread(t).score+0.00001
				'if hafermode=1 then hc="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+lb
				os=""
				#include "thread_solve_output.bi"
				if timertest=1 andalso avgits>0 then os+="Its: "+str(avgits)+" AVG time:"+str(stt(avgsolvetime/avgits))+" AVG items: "+format(avgrestarts/avgits,"0.00")+lb
				os+=lb+"Symbols: "+str(thread(t).effectivesymbols)+lb+lb
				os+=str(thread(t).itemname)+lb+lb
				os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
				'ui_editbox_settext(output_text,os)
				
				if timertest=1 then
					cc=0
					for i=1 to l
						if sol2(i)=thread(t).sol(i) then cc+=1
					next i
					if cc/l>=0.75 then '75% accurate
						for i=1 to threads
							thread(i).solver_stop=1
							do
								sleep 10
							loop until thread(i).solver_waiting=1
							thread(i).score=0
							thread(i).restarts_completed=0
							thread(i).avgscore=0
							thread(i).avgioc=0
							thread(i).avgpccycles=0
							for j=1 to s
					 			thread(i).key(j)=0
							next j
						next i
						sleep 100
						avgits+=1
						threadsq=0
						improved=0
						avgsolvetime+=thread(t).sectime
						select case hafermode
							case 0:avgrestarts+=its
							case 1:avgrestarts+=restarts
						end select
						iterations=local_iterations
						restarts=0
						best_score=0
						global_best_score=0
						its=0
						itsmax=local_hciterations
						temp=temp_start
						temp_min=temp_start/itsmax
						bkey(0)=shifts
						select case hafermode
							case 1
								do
									r=int(rnd*2):if r=0 then bkey(1)=-int(rnd*ngram_alphabet_size) else bkey(1)=int(rnd*ngram_alphabet_size)
									r=int(rnd*2):if r=0 then bkey(2)=-int(rnd*ngram_alphabet_size) else bkey(2)=int(rnd*ngram_alphabet_size)
								loop until bkey(1)<>bkey(2) andalso abs(bkey(1))+abs(bkey(2))>0 andalso abs(bkey(1))+abs(bkey(2))<>ngram_alphabet_size
						end select
						sectimer=timer
					end if
				end if
				
			end if
			
			if thread(t).solver_waiting=1 andalso best_score>=thread(t).score then
				
				if thread(t).score>0 then
					its+=1
					temp-=temp_min
				end if
				
				if thread(t).score>new_score then
					
					new_score=thread(t).score
					for i=0 to shifts
						bkey(i)=thread(t).key(s+1+i)
					next i
					
				else
					
					if thread(t).score>0 then new_score-=temp
					
				end if
				
				if its=itsmax andalso hafermode=1 then
					its=0
					threadsq=0
					itsmax*=local_hciterationsfactor
					temp=temp_start
					temp_min=temp/itsmax
					iterations*=local_iterationsfactor
					restarts+=1
					new_score=0
					'best_score=0
					for i=1 to threads
				 		thread(i).solver_stop=1
				 		do
							sleep 0.001
				 		loop until thread(i).solver_waiting=1
				 		thread(i).score=0
				 		for j=1 to s+shifts+1
				 			thread(i).key(j)=0
				 		next j
					next i
					bkey(0)=shifts
					select case hafermode
						case 1
							do
								r=int(rnd*2):if r=0 then bkey(1)=-int(rnd*ngram_alphabet_size) else bkey(1)=int(rnd*ngram_alphabet_size)
								r=int(rnd*2):if r=0 then bkey(2)=-int(rnd*ngram_alphabet_size) else bkey(2)=int(rnd*ngram_alphabet_size)
							loop until bkey(1)<>bkey(2) andalso abs(bkey(1))+abs(bkey(2))>0 andalso abs(bkey(1))+abs(bkey(2))<>ngram_alphabet_size
					end select
				end if
				
				if threadsq<itsmax then 'do not queue aditional cpu threads when not needed
					
					for i=0 to shifts
						key(i)=bkey(i)
					next i
					
					select case hafermode
						case 0
							key(2)=1+(its mod 12)
							key(1)=-key(2)
						case 1
							if rnd>0.5 or thread(t).score=0 then 'retry best key
								i=int(rnd*shifts)+1
								do
									r=int(rnd*2):if r=0 then key(i)=-int(rnd*ngram_alphabet_size) else key(i)=int(rnd*ngram_alphabet_size)
								loop until key(1)<>key(2) andalso abs(key(1))+abs(key(2))>0 andalso abs(key(1))+abs(key(2))<>ngram_alphabet_size
							end if	
					end select
					
					thread(t).itemname="Key: " '+str(key(0))+" "
					for i=1 to shifts
						if key(i)>=0 then thread(t).itemname+="+"+str(key(i))
						if key(i)<0 then thread(t).itemname+=str(key(i))
						if i<>shifts then thread(t).itemname+="/"
					next i
					
					thread(t).outputdir=basedir+"\Output\"
					thread(t).l=l
					thread(t).s=s
					thread(t).dim_x=dx
					thread(t).dim_y=dy
					thread(t).score=0
					thread(t).pcmode=0
					thread(t).advstats=solvesub_advstats
					thread(t).iterations=iterations
					thread(t).temperature=solvesub_temperature
					thread(t).restarts=solvesub_restarts
					thread(t).subrestartlevels=solvesub_subrestartlevels
					thread(t).ngramfactor=local_ngramfactor 'solvesub_ngramfactor
					thread(t).multiplicityweight=local_multiplicityweight 'solvesub_multiplicityweight
					thread(t).entweight=solvesub_entweight
					thread(t).solver_stop=0
					for i=1 to l
						thread(t).cip(i)=nba(i)
					next i
					
					for i=1 to s
						thread(t).key(i)=local_cpol(i)
					next i
					for i=0 to shifts
						thread(t).key(s+1+i)=key(i)
					next i
					
					thread(t).update=0
					thread(t).solver_waiting=0 'engage thread
					if hafermode=1 then threadsq+=1
					
					if hafermode=0 then iterations*=local_iterationsfactor
					
					if timer-statustimer>1 then
						statustimer=timer
						update_solver_status
					end if
					
				else
					
					thread(t).score=0 'needed?
				
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_polyphones_user(byval none as any ptr)
	
	dim as integer i,j,k,t
	dim as integer iterations=solvesub_iterations
	dim as double best_score
	dim as string os
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer num=info_numerical
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer nba(l)
	dim as integer ngram_score
	dim as integer local_cpol(s)
	dim as integer ok1
	dim as integer local_multiplicityweight=val(ui_editbox_gettext(editbox_polyphones_mulweight))
	
	'solvesub_sdbias=val(ui_editbox_gettext(editbox_polyphones_sdbias))
	'if solvesub_sdbias<0 or solvesub_sdbias>1 then
	'	ui_editbox_settext(output_text,"Error: distribution/symbol bias must be in 0 to 1 range")
	'	return
	'end if
	
	for i=1 to l
		nba(i)=nuba(i)
	next i
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	task_active="substitution + polyphones [user]"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	for i=1 to s
		local_cpol(i)=cpol(i)
		if local_cpol(i)>1 andalso freq(i)>1 then ok1=1
	next i
	
	dim as byte overridehc=ui_checkbox_getcheck(checkbox_polyphones_override)
	dim as double local_ngramfactor=solvesub_ngramfactor
	if overridehc=1 then
		local_ngramfactor*=(1+(local_multiplicityweight/10))
	end if
	
	i=0
	if ok1=0 then
		do
			i+=1 'int(rnd*s)+1
			if i>s then
				solver_status_processing=0
				task_active="none"
				update_solver_status
				exit sub
			end if
		loop until freq(i)>1
		local_cpol(i)=2
	end if
	
	sectimer=timer
	
	dim as integer restarts,avgits,cc,timertest=1 'solve time test
	dim as double avgsolvetime,avgrestarts
	dim as short sol2(l)
	if timertest=1 then
		dim as string comp2=string_to_info(ui_editbox_gettext(output_text))
		for i=1 to l
			sol2(i)=info(i)
		next i
	end if
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).score>best_score then
				
				best_score=thread(t).score+0.00001
				os=""
				#include "thread_solve_output.bi"
				if timertest=1 andalso avgits>0 then os+="Its: "+str(avgits)+" AVG time:"+str(stt(avgsolvetime/avgits))+" AVG items: "+format(avgrestarts/avgits,"0.00")+lb
				os+=lb+"Symbols: "+str(thread(t).effectivesymbols)+lb+lb
				os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
				ui_editbox_settext(output_text,os)
				
				if timertest=1 then
					cc=0
					for i=1 to l
						if sol2(i)=thread(t).sol(i) then cc+=1
					next i
					if cc/l>=0.75 then '75% accurate
						for i=1 to threads
							thread(i).solver_stop=1
							do
								sleep 10
							loop until thread(i).solver_waiting=1
							thread(i).score=0
							thread(i).restarts_completed=0
							thread(i).avgscore=0
							thread(i).avgioc=0
							thread(i).avgpccycles=0
						next i
						sleep 100
						avgits+=1
						avgsolvetime+=thread(t).sectime
						avgrestarts+=restarts
						iterations=solvesub_iterations
						restarts=0
						best_score=0
						global_best_score=0
						sectimer=timer
					end if
				end if
				
			end if
			
			if thread(t).solver_waiting=1 andalso best_score>=thread(t).score then
				
				if thread(t).score>0 then restarts+=1
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0
				thread(t).pcmode=0
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=local_ngramfactor 'solvesub_ngramfactor
				thread(t).multiplicityweight=local_multiplicityweight 'solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				for j=1 to l
					thread(t).cip(j)=nba(j)
				next j
				for j=1 to s
					thread(t).key(j)=local_cpol(j)
				next j
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				iterations*=solvesub_iterationsfactor
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_polyphones_auto(byval none as any ptr)
	
	dim as integer i,j,k,x,y,r1,r2,r3,r4,t
	dim as string os,hc
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer num=info_numerical
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as short nba(l)
	dim as short cip(l)
	dim as short sym(s)
	dim as short frq(s)
	dim as short bestp(s)
	dim as short currp(s)
	dim as integer polys=val(ui_editbox_gettext(editbox_polyphones_extraletters)) 'solvesub_polyphones
	dim as integer its
	dim as integer itsmax=solvesub_hciterations
	dim as double temp_start=3000/threads
	dim as double temp=temp_start
	dim as double temp_min=temp_start/itsmax
	dim as double new_score=0
	dim as double avg_score,best_score
	dim as integer restarts
	dim as integer improved
	'dim as integer iterations1=solvesub_iterations
	'dim as double iterations2=iterations1*10
	'dim as double iterations3=(iterations2-iterations1)/itsmax
	dim as double iterations=solvesub_iterations
	dim as integer local_multiplicityweight=val(ui_editbox_gettext(editbox_polyphones_mulweight))
	dim as integer local_cpol(s)
	
	'solvesub_sdbias=val(ui_editbox_gettext(editbox_polyphones_sdbias))
	'if solvesub_sdbias<=0 or solvesub_sdbias>1 then
	'	ui_editbox_settext(output_text,"Error: distribution/symbol bias must be in 0 to 1 range")
	'	return
	'end if
	
	val(ui_editbox_gettext(editbox_polyphones_extraletters))
	if polys>l then
		ui_editbox_settext(output_text,"Error: polyphones > cipher length")
		return
	end if
	
	ips_timer=timer
	dim as double statustimer=timer
	dim as double updatetimer=timer
	
	for i=1 to l
		nba(i)=nuba(i)
		cip(i)=info(i)
		frq(nba(i))+=1
	next i
	
	for i=1 to s
		local_cpol(i)=cpol(i)
	next i
	
	solvesub_incpolyphones=ui_checkbox_getcheck(checkbox_polyphones_increment)
	
	dim as byte overridehc=ui_checkbox_getcheck(checkbox_polyphones_override)
	dim as integer local_iterations=solvesub_iterations
	dim as double local_iterationsfactor=solvesub_iterationsfactor
	dim as integer local_hciterations=solvesub_hciterations
	dim as double local_hciterationsfactor=solvesub_hciterationsfactor
	dim as double local_ngramfactor=solvesub_ngramfactor
	if overridehc=1 then
		local_iterations=500000
		iterations=local_iterations
		local_iterationsfactor=1.2
		local_hciterations=5000
		itsmax=local_hciterations
		local_hciterationsfactor=1
		temp_min=temp_start/itsmax
		local_ngramfactor*=(1+(local_multiplicityweight/10))
	end if
	
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	task_active="substitution + polyphones [auto]"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	for i=1 to s
		bestp(i)=1
	next i
	for i=1 to polys
		do
			r1=int(rnd*s)+1
		loop until frq(r1)>bestp(r1)+(local_cpol(r1)-1)
		bestp(r1)+=1
	next i
	
	dim as integer items,avgits,cc,timertest=1 'solve time test
	dim as double avgsolvetime,avgrestarts
	dim as short sol2(l)
	if timertest=1 then
		dim as string comp2=string_to_info(ui_editbox_gettext(output_text))
		for i=1 to l
			sol2(i)=info(i)
		next i
	end if
	
	sectimer=timer
	dim as double soltimer=timer

	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if timer-soltimer>1 andalso improved=1 then
				soltimer=timer
				hc="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+lb
				ui_editbox_settext(output_text,hc+os)
			end if
			
			if thread(t).score>best_score then
				
				improved=1
				best_score=thread(t).score+0.00001
				'os="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+lb
				os=""
				#include "thread_solve_output.bi"
				if timertest=1 andalso avgits>0 then os+="Its: "+str(avgits)+" AVG time:"+str(stt(avgsolvetime/avgits))+" AVG items: "+format(avgrestarts/avgits,"0.00")+lb
				os+=lb+"Symbols: "+str(thread(t).effectivesymbols)+lb+lb
				os+=info_to_string(thread(t).sol(),thread(t).l,dx,dy,0,solvesub_addspaces,0)
				'ui_editbox_settext(output_text,hc+os)
				
				if timertest=1 then
					cc=0
					for i=1 to l
						if sol2(i)=thread(t).sol(i) then cc+=1
					next i
					if cc/l>=0.75 then '75% accurate
						for i=1 to threads
							thread(i).solver_stop=1
							do
								sleep 10
							loop until thread(i).solver_waiting=1
							thread(i).score=0
							thread(i).restarts_completed=0
							thread(i).avgscore=0
							thread(i).avgioc=0
							thread(i).avgpccycles=0
							for j=1 to s
					 			thread(i).key(j)=0
							next j
						next i
						sleep 100
						avgits+=1
						avgsolvetime+=thread(t).sectime
						avgrestarts+=restarts
						iterations=local_iterations
						restarts=0
						best_score=0
						improved=0
						global_best_score=0
						polys=solvesub_polyphones
						its=0
						items=0
						itsmax=local_hciterations
						temp=temp_start
						temp_min=temp_start/itsmax
						for i=1 to s
							bestp(i)=1
						next i
						for i=1 to polys
							do
								r1=int(rnd*s)+1
							loop until frq(r1)>bestp(r1)+cpol(r1)
							bestp(r1)+=1
						next i
						sectimer=timer
					end if
				end if
				
			end if
			
			if thread(t).solver_waiting=1 andalso best_score>=thread(t).score then
				
				if thread(t).score>0 then
					its+=1
					temp-=temp_min
					items+=1
				end if
				
				if thread(t).score>new_score then
					
					new_score=thread(t).score
					for j=1 to s
						bestp(j)=thread(t).key(j)-(local_cpol(j)-1)
					next j
					
				else
					
					if thread(t).score>0 then new_score-=temp
					
				end if
				
				if its=itsmax then
					its=0
					itsmax*=local_hciterationsfactor
					iterations*=local_iterationsfactor
					restarts+=1
					avg_score+=best_score
					'best_score=0
					temp=temp_start
					temp_min=temp_start/itsmax
					'global_best_score=0
					new_score=0
					if solvesub_incpolyphones=1 then polys+=1
					for j=1 to threads
				 		thread(j).solver_stop=1
				 		do
							sleep 0.001
				 		loop until thread(j).solver_waiting=1
				 		thread(j).score=0
				 		for k=1 to s
				 			thread(j).key(k)=0
				 		next k
					next j
					for j=1 to s
						bestp(j)=1
					next j
					for j=1 to polys
						do
							r1=int(rnd*s)+1
						loop until frq(r1)>bestp(r1)+(local_cpol(r1)-1)
						bestp(r1)+=1
					next j
				end if
				
				for j=1 to s
					currp(j)=bestp(j)
				next j
				 
				for j=1 to 1 'random changes
					do
						r1=int(rnd*s)+1
						r2=int(rnd*s)+1
					loop until r1<>r2 andalso currp(r1)>1 andalso frq(r2)>currp(r2)+(local_cpol(r2)-1)
					currp(r1)-=1
					currp(r2)+=1
				next j
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0
				thread(t).pcmode=0
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=local_ngramfactor 'solvesub_ngramfactor
				thread(t).multiplicityweight=local_multiplicityweight 'solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				
				for j=1 to l
					thread(t).cip(j)=nba(j)
				next j
				for j=1 to s
					thread(t).key(j)=currp(j)+(local_cpol(j)-1)
				next j
				
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_vigenerelist(byval none as any ptr)
	
	dim as integer t,a,b,c,i,j,k
	dim as integer iterations=solvesub_iterations
	dim as double best_score
	dim as string os
	dim as string w
	dim as string item
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer cip(l)
	dim as integer key(constcip)
	dim as integer kl
	dim as integer endfile
	
	task_active="substitution + vigen�re word list"
	update_solver_status
	
	dim as string filename=ui_loadsavedialog(0,"Open vigen�re word list",filter,1,basedir+"\Misc\")
	
	if fileexists(filename)=0 then
		if filename<>"" then ui_editbox_settext(output_text,"Error: file not found")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	for i=1 to l
		cip(i)=nuba(i)
	next i
	
	ips_timer=timer
	dim as double statustimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	open filename for input as #1
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>best_score then
					
					best_score=thread(t).score+0.00001
					
					os=""
					#include "thread_solve_output.bi"
					
					os+=lb
					os+=thread(t).itemname+lb
					os+=lb
					os+=info_to_string(thread(t).sol(),l,dx,dy,0,solvesub_addspaces,0)
					ui_editbox_settext(output_text,os)
					
				end if
				
				if endfile=0 then
				
					thread(t).outputdir=basedir+"\Output\"
					thread(t).l=l
					thread(t).s=s
					thread(t).dim_x=dx
					thread(t).dim_y=dy
					thread(t).score=0
					thread(t).pcmode=0
					thread(t).advstats=solvesub_advstats
					thread(t).iterations=iterations
					thread(t).temperature=solvesub_temperature
					thread(t).restarts=solvesub_restarts
					thread(t).subrestartlevels=solvesub_subrestartlevels
					thread(t).ngramfactor=solvesub_ngramfactor
					thread(t).multiplicityweight=solvesub_multiplicityweight
					thread(t).entweight=solvesub_entweight
					thread(t).solver_stop=0
					
					erase key
					item="Vigen�re keyword: "
					
					do
						kl=0
						line input #1,w
						for j=1 to len(w)
							for k=0 to ngram_alphabet_size-1
								if asc(w,j)=alphabet(k) then
									kl+=1
									key(kl)=(ngram_alphabet_size-1)-alpharev(asc(w,j))
									item+=chr(asc(w,j))
								end if		
							next k
							if j=l then exit for
						next j
						if eof(1) then
							endfile=1
							exit do
						end if
					loop until kl>0
					
					if kl>0 then
					
						thread(t).itemname=item
						
						k=0
						for j=1 to l
							k+=1
							if k>kl then k=1
							thread(t).cip(j)=cip(j)
							thread(t).key(j)=key(k)
						next j
						
						thread(t).update=0
						thread(t).solver_waiting=0 'engage thread
						
					end if
						
				else 'check if all threads finished
					
					stoptask=1
					for j=1 to threads
						if thread(j).solver_waiting=0 then stoptask=0
					next j				 
				
				end if
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	close #1
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_vigenere(byval none as any ptr)
	
	dim as integer h,i,j,k,a,b,p,r1,r2,t
	dim as double iterations=solvesub_iterations
	dim as double iterationsfactor=solvesub_iterationsfactor
	dim as double best_score
	dim as double new_score
	dim as string os,hc
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer cip(l),cip2(l),key2(l)
	dim as integer improved
	
	dim as integer kl=solvesub_vigenerekeylength
	dim as integer bycols=solvesub_vigenerebycolumns
	dim as integer key(kl)
	'dim as integer bestkey(kl)
	
	dim as double itsmax=solvesub_hciterations
	dim as integer its,cits,restarts
	dim as integer vigits,solved,currsolved
	
	dim as double temp_start=3000/threads
	dim as double temp '=temp_start
	dim as double temp_min '=temp/itsmax
	
	dim as integer subrestarts=3
	dim as integer sri=1,srj=1
	dim as integer srits(10,10)
	dim as integer itsmaxpsr=itsmax/subrestarts
	dim as short bkey(subrestarts,kl)
	
	dim as double avgbest,brun_score
	
	if kl>l then
		ui_editbox_settext(output_text,"Error: keyword length > cipher length")
		exit sub
	end if
	
	for i=1 to l
		cip(i)=nuba(i)
	next i
	
	ips_timer=timer
	dim as double statustimer=timer
	dim as double soltimer=timer
	stoptask=0
	solver_status_processing=1
	global_best_score=0
	
	task_active="substitution + vigen�re"
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	h=0 'init sub restarts
	for i=1 to subrestarts
		srits(i,0)=subrestarts-(i-1)
		k=0
		do
			for j=1 to subrestarts-(i-1)
				h+=1
				k+=1
				srits(i,j)+=1
				if k=int(itsmax/subrestarts) then exit do
			next j
		loop
	next i
	if h<itsmax then srits(subrestarts,1)+=itsmax-h
	
	temp=temp_start-((temp_start/subrestarts)*(sri-1))
	temp_min=temp/srits(sri,srj)
	
	for i=1 to kl 'initiate random key
		bkey(1,i)=int(rnd*ngram_alphabet_size)
	next i
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if timer-soltimer>1 andalso improved=1 then
				soltimer=timer
				hc="Restart: "+str(restarts+1)+" Hill climber: "+str(cits)+"/"+str(int(itsmax))+" @ "+str(int(iterations))+lb
				ui_editbox_settext(output_text,hc+os)
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>new_score then
					
					new_score=thread(t).score
					if new_score>brun_score then brun_score=new_score
					
					if new_score>solvesub_pnover andalso currsolved=0 then
						solved+=1
						currsolved=1
					end if
					
					for j=1 to kl 'save bestkey
						bkey(sri,j)=thread(t).graph(j)
					next j
					
					if new_score>best_score then
						improved=1
						best_score=new_score+0.00001
						
						'if development=1 then
							'os="Best score: "+format(brunscore,"0.00")+" Over "+str(solvesub_pnover)+": "+str(solved)+" ("+format((solved/(restarts+1))*100,2)+"%)"+lb
							os="Restart: "+str(restarts+1)+" Hill climber: "+str(cits)+"/"+str(int(itsmax))+" @ "+str(int(iterations))+lb
						
						os=""
						#include "thread_solve_output.bi"
						
						'''	os+="Score: "+format(thread(t).score,"0.00")+" IOC: "+format(thread(t).ioc,"0.0000")+" Multiplicity: "+format(thread(t).multiplicity,4)+stt(thread(t).sectime)+lb
						'else
						'	os="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(int(itsmax))+" @ "+str(int(iterations))+lb
						'	os+="Score: "+format(thread(t).score,"0.00")+" IOC: "+format(thread(t).ioc,"0.00000")+" Multiplicity: "+format(thread(t).s/thread(t).l,5)+lb
						'end if
						
						'''if solvesub_advstats=1 then
						'''	os+=str(thread(t).repeats)+lb+"PT-to-CT cycles: "+str(thread(t).pccycles)+lb
						'''end if
						
						os+=lb+thread(t).itemname+lb+lb
						os+=info_to_string(thread(t).sol(),l,dx,dy,0,solvesub_addspaces,0)
						'ui_editbox_settext(output_text,os)
					end if
						
				else
					
					if thread(t).score>0 then new_score-=temp
					
				end if
				
				if thread(t).score>0 then
				
					its+=1
					cits+=1
					temp-=temp_min
				
				end if
				
				if its>srits(sri,srj) then
					
					its=0
					srj+=1
					new_score=0
					
					for j=1 to threads 'stop threads
				 		thread(j).solver_stop=1
				 		do
							sleep 0.001
				 		loop until thread(j).solver_waiting=1
				 		thread(j).score=0
					next j
					
					if srj>srits(sri,0) then
						
						srj=1
						sri+=1
						
						if sri>subrestarts then 'restart solver
						
							sri=1
							srj=1
							cits=0
							best_score=0
							restarts+=1
							itsmax*=solvesub_hciterationsfactor
							iterations*=solvesub_iterationsfactor
							'temp=temp_start
							'temp_min=temp/itsmax
							currsolved=0
							global_best_score=0
							
							h=0 'init sub restarts
							erase srits
							for i=1 to subrestarts
								srits(i,0)=subrestarts-(i-1)
								k=0
								do
									for j=1 to subrestarts-(i-1)
										h+=1
										k+=1
										srits(i,j)+=1
										if k=int(itsmax/subrestarts) then exit do
									next j
								loop
							next i
							if h<itsmax then srits(subrestarts,1)+=itsmax-h
						
						end if
					
					end if
					
					temp=temp_start-((temp_start/subrestarts)*(sri-1))
					temp_min=temp/srits(sri,srj)
					
					if sri=1 then 'init new random key
						
						for j=1 to kl
							bkey(1,j)=int(rnd*ngram_alphabet_size)
						next j
						
					else 'take best key from previous sub restart
					
						for i=1 to kl
							bkey(sri,i)=bkey(sri-1,i)
						next i
					
					end if
					
				end if
				
				thread(t).outputdir=basedir+"\Output\"
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0
				thread(t).pcmode=0
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				
				for j=1 to kl 'get bestkey
					key(j)=bkey(sri,j)
				next j
				
				for j=1 to 1 'random changes
					key(int(rnd*kl)+1)=int(rnd*ngram_alphabet_size)
				next j
				
				thread(t).itemname="Vigen�re keyword "
				if bycols=0 then 
					thread(t).itemname+="(by rows): "
				else
					thread(t).itemname+="(by columns): "
				end if
				for j=1 to kl
					thread(t).itemname+=chr(alphabet(key(j)))
				next j
				
				if bycols=1 then
					k=0
					for j=1 to kl
						thread(t).graph(j)=key(j)
					next j
					for j=1 to l
						k+=1
						if k>kl then k=1
						cip2(j)=cip(j)
						key2(j)=key(k)
					next j
				else
					a=0
					dim as integer ldkl=l/kl
					if ldkl*kl<l then ldkl+=1
					for j=1 to kl
						thread(t).graph(j)=key(j)
						for k=1 to ldkl
							a+=1
							cip2(a)=cip(a)
							key2(a)=key(j)
							if a=l then exit for,for
						next k
					next j
				end if
				
				a=0:p=1 'period control
				for j=1 to p
					for k=j to l step p
						a+=1
						thread(t).cip(a)=cip2(k)
						thread(t).key(a)=key2(k)
					next k
				next j
				
				thread(t).update=0
				thread(t).solver_waiting=0 'engage thread		
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_genhc(byval none as any ptr)
	
	dim as integer i,j,k,t,e,ac
	dim as integer iterations=solvesub_iterations
	dim as double newscore,bestscore,currbest,brunscore
	dim as string os,his
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer num=info_numerical
	dim as short nba(l)
	dim as integer cip(l)
	
	dim as integer itsmax=solvesub_hciterations 'double
	dim as integer itsmax2 'double
	dim as integer citsmax
	dim as double temp_start
	dim as double temp
	dim as double temp_min
	dim as double sri,srj,srimax
	dim as uinteger nfb
	
	dim as integer solved
	dim as integer its,cits,restarts,kl,kl_nulls
	dim as integer extcip_a1,extcip_a2,extcip_a3
	dim as integer extkey_a1,extkey_a2,extkey_a3,extkey_a4
	dim as integer extinf_a1,extinf_a2,extinf_a3
	dim as integer extpcm_a1,extpcm_a2,extpcm_a3
	
	dim as short akey(2000,1)
	dim as short tkey(2000)
	
	randomize timer 'test
	
	ips_timer=timer
	dim as double statustimer=timer
	dim as double updatetimer=timer
	
	dim as string str_nulls
	dim as string str_skips
	dim as string str_rest
	
	'development=1 '<<<<<<<<<<<<<------------------------------------------------------ switch
	
	stoptask=0
	global_best_score=0
	solvesub_avgscore=0
	solvesub_avgioc=0
	
	select case genhc_mode
		case "columnar rearrangement"
			solvesub_ctcolumns=info_x
			if solvesub_ctmode=1 then 'bigram beam
				ext_hc=1
				extpcm_a1=threads
				extpcm_a2=1
				extpcm_a3=l
			end if
			if solvesub_ctmode=0 then 'azd beam
				ext_hc=2
				extcip_a1=threads
				extcip_a2=solvesub_ctdepth^solvesub_ctdepth
				extcip_a3=l
				extkey_a1=threads
				extkey_a2=solvesub_ctdepth^solvesub_ctdepth
				extkey_a3=solvesub_ctcolumns
				extkey_a4=0
				extinf_a1=0
				extinf_a2=0
				extinf_a3=0
				extpcm_a1=threads
				extpcm_a2=solvesub_ctdepth^solvesub_ctdepth
				extpcm_a3=l
			end if
			srimax=4
			temp_start=(100/30)*threads
			kl=solvesub_ctcolumns
			task_active="substitution + columnar rearrangement"
		case "columnar transposition"
			solvesub_ctcolumns=info_x
			if solvesub_ctmode=1 then 'bigram beam
				ext_hc=3
				extpcm_a1=threads
				extpcm_a2=1
				extpcm_a3=l
			end if
			if solvesub_ctmode=0 then 'azd beam
				ext_hc=4
				extcip_a1=threads
				extcip_a2=solvesub_ctdepth^solvesub_ctdepth
				extcip_a3=l
				extkey_a1=threads
				extkey_a2=solvesub_ctdepth^solvesub_ctdepth
				extkey_a3=solvesub_ctcolumns
				extkey_a4=0
				extinf_a1=0
				extinf_a2=0
				extinf_a3=0
				extpcm_a1=threads
				extpcm_a2=solvesub_ctdepth^solvesub_ctdepth
				extpcm_a3=l
			end if
			srimax=4
			temp_start=(solvesub_nshctemp/30)*threads
			kl=solvesub_ctcolumns
			task_active="substitution + columnar transposition"
		case "nulls and skips"		
			if solvesub_pnmannulls+solvesub_pnmanskips>0 then
				solvesub_pnkl=solvesub_pnmannulls+solvesub_pnmanskips
				kl=solvesub_pnmannulls+solvesub_pnmanskips
				kl_nulls=solvesub_pnmannulls
			else
				solvesub_pnkl=solvesub_pnnulls
				kl=solvesub_pnnulls
				kl_nulls=solvesub_pnnulls
			end if
			if solvesub_pnperiod>l then
				editbox_settext(output_text,"Error: period > cipher length")
				exit sub
			end if
			if kl>l/2 then
				editbox_settext(output_text,"Error: nulls and skips > cipher length / 2")
				exit sub
			end if
			task_active="substitution + nulls and skips"
			ext_hc=5
			extcip_a1=threads
			extcip_a2=solvesub_pndepth^solvesub_pndepth
			extcip_a3=l+kl
			extkey_a1=threads
			extkey_a2=solvesub_pndepth^solvesub_pndepth
			extkey_a3=kl
			extkey_a4=1 'add or remove null
			extinf_a1=threads
			extinf_a2=solvesub_pndepth^solvesub_pndepth
			extinf_a3=1 'length and symbols
			extpcm_a1=threads
			extpcm_a2=solvesub_pndepth^solvesub_pndepth
			extpcm_a3=l+kl
			srimax=4
			temp_start=(100/30)*threads
	end select
	
	solver_status_processing=1	
	temp=temp_start
	temp_min=temp/itsmax
	
	dim as short nulls(2000)
	dim as short skips(2000)
	
	dim as integer sr(10,10) 'double
	dim as short bkey(10,2000,1)
	for i=1 to srimax
		sr(i,0)=srimax-(i-1)
	next i
	sri=1
	srj=1
	for i=1 to srimax
		for j=1 to sr(i,0)
			sr(i,j)=(itsmax/srimax)/sr(i,0)
			itsmax2+=sr(i,j)
		next j
	next i
	sr(srimax,1)+=itsmax-itsmax2
	citsmax=sr(sri,srj)
	
	dim as integer m1=extcip_a1*extcip_a2*extcip_a3*2
	dim as integer m2=extkey_a1*extkey_a2*extkey_a3*extkey_a4*2
	dim as integer m3=extinf_a1*extinf_a2*extinf_a3*2
	dim as integer m4=extpcm_a1*extpcm_a2*extpcm_a3*2
	
	select case ext_hc
		case 1,3 'use bigrams
			if solvesub_pcmode=1 then redim extpcm(extpcm_a1,extpcm_a2,extpcm_a3)
		case 2,4,5,6
			if solvesub_pcmode=1 then
				if m1+m2+m3+m4>fre then
					editbox_settext(output_text,"Error: not enough free RAM to run solver")
					exit sub
				end if
				redim extcip(extcip_a1,extcip_a2,extcip_a3)
				redim extkey(extkey_a1,extkey_a2,extkey_a3,extkey_a4)
				redim extinf(extinf_a1,extinf_a2,extinf_a3)
				redim extpcm(extpcm_a1,extpcm_a2,extpcm_a3)
			else
				if m1+m2+m3+m4>fre then
					editbox_settext(output_text,"Error: not enough free RAM to run solver")
					exit sub
				end if
				redim extcip(extcip_a1,extcip_a2,extcip_a3)
				redim extkey(extkey_a1,extkey_a2,extkey_a3,extkey_a4)
				redim extinf(extinf_a1,extinf_a2,extinf_a3)
				'redim extpcm(extpcm_a1,extpcm_a2,extpcm_a3)
			end if
	end select
	
	update_solver_status
	
	'for i=1 to threads
	'	thread(i).score=0
	'	thread(i).itemname=""
	'	thread(i).itemnumber=0
	'	thread(i).iterations_completed=0
	'	thread(i).restarts_completed=0
	'	for j=0 to kl
	'		for k=0 to 1
	'			thread(i).gkey(j,k)=0
	'		next k
	'	next j
	'	'for j=1 to l+1
	'	'	thread(i).graph(j)=0
	'	'next j
	'next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	for i=1 to l
		nba(i)=nuba(i)
		cip(i)=info(i)
	next i
	
	select case genhc_mode 'init key
		case "columnar rearrangement","columnar transposition"
			for i=1 to kl
				akey(i,0)=i
			next i
		case "nulls and skips"
			for i=1 to kl
				if i<=kl_nulls then 'nulls
					akey(i,1)=0
					do
						e=0
						akey(i,0)=int(rnd*l)+1
						for j=1 to i-1
							if akey(j,0)=akey(i,0) then
								e=1
								exit for
							end if
						next j
					loop until e=0
				else 'skips
					akey(i,1)=1
					do
						e=0
						akey(i,0)=int(rnd*(l+1))+1
						for j=1 to i-1
							if akey(j,1)=0 andalso akey(j,0)=akey(i,0) then
								e=1
								exit for
							end if
						next j
					loop until e=0
				end if
			next i
	end select
	
	dim as integer oldshift=solvesub_nshcshift 'gradual shift
	dim as integer itscount 'gradual shift
	solvesub_nshcshift=0 'gradual shift
	
	dim as double soltimer=timer
	dim as string hc
	dim as integer improved
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if timer-soltimer>1 andalso improved=1 then
				soltimer=timer
				if development=1 then
					hc="Restart: "+str(restarts+1)+"/"+str(solvesub_nshcrestartsmax)+" HC: "+str(int(its/1))+"/"+str(int(itsmax/1))+" Sub: "+str(int(iterations/1))+""+lb
					hc+="Best score: "+format(brunscore,"0.00")+" Over "+str(solvesub_pnover)+": "+str(solved)+" ("+format((solved/(restarts+1))*100,"0.00")+"%)"+lb
				else
					hc="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+lb
				end if
				ui_editbox_settext(output_text,hc+os)
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>newscore then
					
					if development=1 then
						select case genhc_mode
							case "nulls and skips"
								his+="T: "
								for i=1 to 3-len(str(t))
									his+="0"
								next i
								his+=str(t)+" A: 1 Score: "
								for i=1 to 5-len(str(int(thread(t).score)))
									his+="0"
								next i
								his+=str(int(thread(t).score))+" "+thread(t).itemname2+chr(13)+chr(10)
						end select
					end if
					
					newscore=thread(t).score
					
					if newscore>bestscore then
						
						improved=1
						
						if newscore>brunscore then
							brunscore=newscore
							os=""
							#include "thread_solve_output.bi"
							os+=lb+thread(t).itemname+lb+lb
							os+=info_to_string(thread(t).sol(),thread(t).l,thread(t).dim_x,thread(t).dim_y,0,solvesub_addspaces,0)
						end if
						
						bestscore=thread(t).score+0.00001
						if bestscore>=solvesub_pnover andalso ac=0 then
							ac=1
							solved+=1
						end if
						
						'if development=1 then
						'	os="Restart: "+str(restarts+1)+"/"+str(solvesub_nshcrestartsmax)+" HC: "+str(int(its/1))+"/"+str(int(itsmax/1))+" Sub: "+str(int(iterations/1))+""+chr(13)+chr(10)
						'	os+="Best score: "+rdc(brunscore,2)+" Over "+str(solvesub_pnover)+": "+str(solved)+" ("+rdc((solved/(restarts+1))*100,2)+"%)"+chr(13)+chr(10)
						'else
						'	os="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+chr(13)+chr(10)
						'end if
						'
						'os+="Score: "+rdc(thread(t).score,2)+" IOC: "+rdc(thread(t).ioc,5)+" Multiplicity: "+rdc(thread(t).s/thread(t).l,5)+chr(13)+chr(10)
						'if solvesub_advstats=1 then
						'	os+="Repeats: "+str(thread(t).ngrams)+" PC-cycles: "+str(thread(t).pccycles)+chr(13)+chr(10)
						'end if
						'os+=chr(13)+chr(10)
						'os+=thread(t).itemname+chr(13)+chr(10)
						'os+=chr(13)+chr(10)
						'os+=info_to_string(thread(t).sol(),thread(t).l,thread(t).dim_x,thread(t).dim_y,0)
						'editbox_settext(output_text,os)
						
					end if
					
					for j=1 to kl 'save bestkey
						for k=0 to 1
							akey(j,k)=thread(t).gkey(j,k)
						next k
					next j
					
				else
					
					if thread(t).score>0 then 
						newscore-=temp
						if development=1 then
							select case genhc_mode
								case "nulls and skips"
									his+="T: "
									for i=1 to 3-len(str(t))
										his+="0"
									next i
									his+=str(t)+" A: 0 Score: "
									for i=1 to 5-len(str(int(thread(t).score)))
										his+="0"
									next i
									his+=str(int(thread(t).score))+" "+thread(t).itemname2+chr(13)+chr(10)
							end select
						end if		
					end if
					
				end if
				
				if thread(t).score>0 then
					its+=1
					cits+=1
					temp-=temp_min
				end if
				
				solvesub_nshcshift=oldshift*(its/itsmax) 'gradual shift
				
				if newscore>currbest then
					currbest=newscore
					for i=1 to kl
						for j=0 to 1
							bkey(sri,i,j)=akey(i,j)
						next j
					next i
				end if
				
				if solvesub_pnoverskip=1 then
					if bestscore>=solvesub_pnover then
						cits=citsmax
						srj=srimax
						sri=srimax
					end if
				end if
				
				if cits=citsmax then 'restart 'int(citsmax)
				
					for i=1 to threads
				 		thread(i).solver_stop=1
				 		do
							sleep 0.001
				 		loop until thread(i).solver_waiting=1
				 		thread(i).score=0
					next i
					
					srj+=1
					if srj>sr(sri,0) then
						sri+=1
						srj=1
						currbest=0
						if sri>srimax then
							sri=1
							srj=1
							its=0
							select case genhc_mode
								case "nulls and skips"
									if development=1 then
										mkdir basedir+"\Output\"+str_nulls+str_rest+str_skips+"\"
										open basedir+"\Output\"+str_nulls+str_rest+str_skips+"\hc_restart_log_"+str(restarts+1)+".txt" for binary as #4
										print #4,his
										close #4
										his=""
									end if
									if solvesub_pnmannulls+solvesub_pnmanskips=0 then
										kl_nulls-=1
										if kl_nulls<0 then 
											kl_nulls=kl
											itsmax*=solvesub_hciterationsfactor
										end if
									else
										itsmax*=solvesub_hciterationsfactor
									end if
								case else
									itsmax*=solvesub_hciterationsfactor
							end select
							ac=0
							restarts+=1
							iterations*=solvesub_iterationsfactor
							solvesub_nshcshift=0 'gradual shift
							if development=1 andalso restarts=solvesub_nshcrestartsmax then exit do
							global_best_score=0
							bestscore=0
							itsmax2=0
							for i=1 to srimax
								for j=1 to sr(i,0)
									sr(i,j)=(itsmax/srimax)/sr(i,0)
									itsmax2+=sr(i,j)
								next j
							next i
							sr(srimax,1)+=itsmax-itsmax2
						end if
					end if
					
					if sri=1 then
						select case genhc_mode 'init random key
							case "columnar rearrangement","columnar transposition"
								for i=1 to kl
									akey(i,0)=i
								next i
							case "nulls and skips"
								for i=1 to kl
									if i<=kl_nulls then 'nulls
										akey(i,1)=0
										do
											e=0
											akey(i,0)=int(rnd*l)+1
											for j=1 to i-1
												if akey(j,0)=akey(i,0) then
													e=1
													exit for
												end if
											next j
										loop until e=0
									else 'skips
										akey(i,1)=1
										do
											e=0
											akey(i,0)=int(rnd*(l+1))+1
											for j=1 to i-1
												if akey(j,1)=0 andalso akey(j,0)=akey(i,0) then
													e=1
													exit for
												end if
											next j
										loop until e=0
									end if
								next i
						end select
					else	
						for i=1 to kl
							for j=0 to 1
								akey(i,j)=bkey(sri-1,i,j)
							next j
						next i
					end if
					
					cits=0
					citsmax=sr(sri,srj)
					temp=temp_start-((temp_start/srimax)*(sri-1)) 'try individual values
					temp_min=temp/citsmax
					newscore=0
					
				end if
				
				for i=1 to kl
					for j=0 to 1
						thread(t).gkey(i,j)=akey(i,j)
					next j
				next i				
				
				select case genhc_mode
					case "nulls and skips"
						str_nulls=""
						str_skips=""
						str_rest=""
						if kl_nulls=1 then str_nulls=str(kl_nulls)+" null"
						if kl_nulls>1 then str_nulls=str(kl_nulls)+" nulls"
						if kl-kl_nulls=1 then str_skips=str(kl-kl_nulls)+" skip"
						if kl-kl_nulls>1 then str_skips=str(kl-kl_nulls)+" skips"
						if kl_nulls>0 andalso kl-kl_nulls>0 then str_rest=" & "
						mkdir basedir+"\Output\"+str_nulls+str_rest+str_skips+"\"
						thread(t).outputdir=basedir+"\Output\"+str_nulls+str_rest+str_skips+"\"
					case else
						thread(t).outputdir=basedir+"\Output\"
				end select
				
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0
				thread(t).pcmode=solvesub_pcmode
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				thread(t).update=0
				
				for i=1 to l
					thread(t).cip(i)=nba(i)
				next i
				
				thread(t).solver_waiting=0 'engage thread
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	solvesub_nshcshift=oldshift
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	'dim as double vls(l+1),low=999999999
	'for i=1 to threads
	'	for j=1 to l+1
	'		if thread(i).graph(j)<low then low=thread(i).graph(j)
	'		vls(j)+=thread(i).graph(j)
	'	next j
	'next i
	'vls(0)=low
	''if (l+1)/dx>dy then dy+=1
	'output_colormap(vls(),cip(),l,dx,dy,0,"null_heatmap")
	
	redim extcip(0,0,0)
	redim extkey(0,0,0,0)
	redim extinf(0,0,0)
	redim extpcm(0,0,0)
	
	ext_hc=0
	clean_thread_information
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_solve_genhc2(byval none as any ptr)
	
	dim as integer i,j,k,t,e,ac
	dim as integer iterations=solvesub_iterations
	dim as double new_score,best_score,currbest,brun_score
	dim as string os,hc,his
	dim as integer l=info_length
	dim as integer s=info_symbols
	dim as integer dx=info_x
	dim as integer dy=info_y
	dim as integer num=info_numerical
	dim as short nba(l)
	dim as integer cip(l)
	
	dim as integer itsmax=solvesub_hciterations 'double
	dim as integer itsmax2 'double
	dim as integer citsmax
	dim as double temp_start
	dim as double temp
	dim as double temp_min
	dim as double sri,srj,srimax
	dim as uinteger nfb
	
	dim as integer solved,improved
	dim as integer its,cits,restarts,kl,kl_nulls
	dim as integer extcip_a1,extcip_a2,extcip_a3
	dim as integer extkey_a1,extkey_a2,extkey_a3,extkey_a4
	dim as integer extinf_a1,extinf_a2,extinf_a3
	dim as integer extpcm_a1,extpcm_a2,extpcm_a3
	
	dim as short akey(constcip,1)
	dim as short tkey(constcip)
	
	'randomize timer 'test
	
	ips_timer=timer
	dim as double statustimer=timer
	dim as double soltimer=timer
	dim as double updatetimer=timer
	
	dim as string str_nulls
	dim as string str_skips
	dim as string str_rest
	
	'------------------------------- test link poly solver -----------------------------------------------------
	'dim as integer ok1,local_cpol(s)
	'for i=1 to s
	'	local_cpol(i)=cpol(i)
	'	if local_cpol(i)>1 andalso freq(i)>1 then ok1=1
	'next i
	'i=0
	'if ok1=0 then
	'	do
	'		i+=1 'int(rnd*s)+1
	'		if i>s then
	'			exit sub
	'			solver_status_processing=0
	'			task_active="none"
	'			update_solver_status
	'		end if
	'	loop until freq(i)>1
	'	local_cpol(i)=2
	'end if
	'------------------------------- test link poly solver -----------------------------------------------------
	
	'development=1 '<<<<<<<<<<<<<------------------------------------------------------ switch
	
	stoptask=0
	global_best_score=0
	
	select case genhc_mode
		case "columnar rearrangement"
			solvesub_ctcolumns=info_x
			if solvesub_ctmode=1 then 'bigram beam
				ext_hc=1
				extpcm_a1=threads
				extpcm_a2=1
				extpcm_a3=l
			end if
			if solvesub_ctmode=0 then 'bhd beam
				ext_hc=2
				extcip_a1=threads
				extcip_a2=solvesub_ctdepth^solvesub_ctdepth
				extcip_a3=l
				extkey_a1=threads
				extkey_a2=solvesub_ctdepth^solvesub_ctdepth
				extkey_a3=solvesub_ctcolumns
				extkey_a4=0
				extinf_a1=0
				extinf_a2=0
				extinf_a3=0
				extpcm_a1=threads
				extpcm_a2=solvesub_ctdepth^solvesub_ctdepth
				extpcm_a3=l
			end if
			srimax=4
			temp_start=(100/30)*threads
			kl=solvesub_ctcolumns
			task_active="substitution + columnar rearrangement"
		case "columnar transposition"
			solvesub_ctcolumns=info_x
			if solvesub_ctmode=1 then 'bigram beam
				ext_hc=3
				extpcm_a1=threads
				extpcm_a2=1
				extpcm_a3=l
			end if
			if solvesub_ctmode=0 then 'bhd beam
				ext_hc=4
				extcip_a1=threads
				extcip_a2=solvesub_ctdepth^solvesub_ctdepth
				extcip_a3=l
				extkey_a1=threads
				extkey_a2=solvesub_ctdepth^solvesub_ctdepth
				extkey_a3=solvesub_ctcolumns
				extkey_a4=0
				extinf_a1=0
				extinf_a2=0
				extinf_a3=0
				extpcm_a1=threads
				extpcm_a2=solvesub_ctdepth^solvesub_ctdepth
				extpcm_a3=l
			end if
			srimax=4
			temp_start=(solvesub_nshctemp/30)*threads
			kl=solvesub_ctcolumns
			task_active="substitution + columnar transposition"
		case "nulls and skips"
			if solvesub_pnmannulls+solvesub_pnmanskips>0 then
				solvesub_pnkl=solvesub_pnmannulls+solvesub_pnmanskips
				kl=solvesub_pnmannulls+solvesub_pnmanskips
				kl_nulls=solvesub_pnmannulls
			else
				solvesub_pnkl=solvesub_pnnulls
				kl=solvesub_pnnulls
				kl_nulls=solvesub_pnnulls
			end if
			if solvesub_pnperiod>l then
				ui_editbox_settext(output_text,"Error: period > cipher length")
				exit sub
			end if
			if kl>l/2 then
				ui_editbox_settext(output_text,"Error: nulls and skips > cipher length / 2")
				exit sub
			end if
			task_active="substitution + nulls and skips"
			ext_hc=5
			extcip_a1=threads
			extcip_a2=solvesub_pndepth^solvesub_pndepth
			extcip_a3=l+kl
			extkey_a1=threads
			extkey_a2=solvesub_pndepth^solvesub_pndepth
			extkey_a3=kl
			extkey_a4=1 'add or remove null
			extinf_a1=threads
			extinf_a2=solvesub_pndepth^solvesub_pndepth
			extinf_a3=1 'length and symbols
			extpcm_a1=threads
			extpcm_a2=solvesub_pndepth^solvesub_pndepth
			extpcm_a3=l+kl
			srimax=4
			temp_start=(100/30)*threads
	end select
	
	solver_status_processing=1	
	temp=temp_start
	temp_min=temp/itsmax
	
	dim as short nulls(constcip)
	dim as short skips(constcip)
	
	dim as integer sr(10,10) 'double
	dim as short bkey(10,constcip,1)
	for i=1 to srimax
		sr(i,0)=srimax-(i-1)
	next i
	sri=1
	srj=1
	for i=1 to srimax
		for j=1 to sr(i,0)
			sr(i,j)=(itsmax/srimax)/sr(i,0)
			itsmax2+=sr(i,j)
		next j
	next i
	sr(srimax,1)+=itsmax-itsmax2
	citsmax=sr(sri,srj)
	temp=temp_start-((temp_start/srimax)*(sri-1))
	temp_min=temp/citsmax
	
	dim as integer m1=extcip_a1*extcip_a2*extcip_a3*2
	dim as integer m2=extkey_a1*extkey_a2*extkey_a3*extkey_a4*2
	dim as integer m3=extinf_a1*extinf_a2*extinf_a3*2
	dim as integer m4=extpcm_a1*extpcm_a2*extpcm_a3*2
	
	select case ext_hc
		case 1,3 'use bigrams
			if solvesub_pcmode=1 then redim extpcm(extpcm_a1,extpcm_a2,extpcm_a3)
		case 2,4,5,6
			if solvesub_pcmode=1 then
				if m1+m2+m3+m4>fre then
					ui_editbox_settext(output_text,"Error: not enough free RAM to run solver")
					exit sub
				end if
				redim extcip(extcip_a1,extcip_a2,extcip_a3)
				redim extkey(extkey_a1,extkey_a2,extkey_a3,extkey_a4)
				redim extinf(extinf_a1,extinf_a2,extinf_a3)
				redim extpcm(extpcm_a1,extpcm_a2,extpcm_a3)
			else
				if m1+m2+m3+m4>fre then
					ui_editbox_settext(output_text,"Error: not enough free RAM to run solver")
					exit sub
				end if
				redim extcip(extcip_a1,extcip_a2,extcip_a3)
				redim extkey(extkey_a1,extkey_a2,extkey_a3,extkey_a4)
				redim extinf(extinf_a1,extinf_a2,extinf_a3)
				'redim extpcm(extpcm_a1,extpcm_a2,extpcm_a3)
			end if
	end select
	
	update_solver_status
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
		print #3,"output_sub_directory=batch_"+str(batchnr)
	end if
	
	for i=1 to l
		nba(i)=nuba(i)
		cip(i)=info(i)
	next i
	
	select case genhc_mode 'init key
		case "columnar rearrangement","columnar transposition"
			for i=1 to kl
				akey(i,0)=i
			next i
		case "nulls and skips"
			for i=1 to kl
				if i<=kl_nulls then 'nulls
					akey(i,1)=0
					do
						e=0
						akey(i,0)=int(rnd*l)+1
						for j=1 to i-1
							if akey(j,0)=akey(i,0) then
								e=1
								exit for
							end if
						next j
					loop until e=0
				else 'skips
					akey(i,1)=1
					do
						e=0
						akey(i,0)=int(rnd*(l+1))+1
						for j=1 to i-1
							if akey(j,1)=0 andalso akey(j,0)=akey(i,0) then
								e=1
								exit for
							end if
						next j
					loop until e=0
				end if
			next i
	end select
	
	dim as integer oldshift=solvesub_nshcshift 'gradual shift
	dim as integer itscount 'gradual shift
	solvesub_nshcshift=0 'gradual shift
	
	sectimer=timer
	
	do
		
		sleep twait
		
		for t=1 to threads
			
			if pausetask=1 then 'pause task
				update_solver_status
				do
					sleep 10
				loop until pausetask=0
				update_solver_status
			end if
			
			if timer-soltimer>1 andalso improved=1 then
				soltimer=timer
				if development=1 then
					hc="Restart: "+str(restarts+1)+"/"+str(solvesub_nshcrestartsmax)+" HC: "+str(int(its/1))+"/"+str(int(itsmax/1))+" Sub: "+str(int(iterations/1))+""+lb
					hc+="Best score: "+format(brun_score,"0.00")+" Over "+str(solvesub_pnover)+": "+str(solved)+" ("+format((solved/(restarts+1))*100,"0.00")+"%)"+lb
				else
					hc="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+lb
				end if
				ui_editbox_settext(output_text,hc+os)
			end if
			
			if thread(t).solver_waiting=1 then
				
				if thread(t).score>new_score then
					
					if development=1 then
						select case genhc_mode
							case "nulls and skips"
								his+="T: "
								for i=1 to 3-len(str(t))
									his+="0"
								next i
								his+=str(t)+" A: 1 Score: "
								for i=1 to 5-len(str(int(thread(t).score)))
									his+="0"
								next i
								his+=str(int(thread(t).score))+" "+thread(t).itemname2+lb
						end select
					end if
					
					new_score=thread(t).score
					
					if new_score>best_score then
						
						improved=1
						if new_score>brun_score then
							brun_score=new_score
							os=""
							#include "thread_solve_output.bi"
							os+=lb+thread(t).itemname+lb+lb
							os+=info_to_string(thread(t).sol(),thread(t).l,thread(t).dim_x,thread(t).dim_y,0,solvesub_addspaces,0)
							'ui_editbox_settext(output_text,os)
						end if
						best_score=thread(t).score+0.00001
						if best_score>=solvesub_pnover andalso ac=0 then
							ac=1
							solved+=1
						end if
						
						if development=1 then
							os="Restart: "+str(restarts+1)+"/"+str(solvesub_nshcrestartsmax)+" HC: "+str(int(its/1))+"/"+str(int(itsmax/1))+" Sub: "+str(int(iterations/1))+""+lb
							os+="Best score: "+format(brun_score,"0.00")+" Over "+str(solvesub_pnover)+": "+str(solved)+" ("+format((solved/(restarts+1))*100,"0.00")+"%)"+lb
						else
							os="Restart: "+str(restarts+1)+" Hill climber: "+str(its)+"/"+str(itsmax)+" @ "+str(int(iterations))+lb
						end if
						
						'----------------- test link poly solver ---------------------
						'os+=lb
						'os+="Symbols: "+str(thread(t).effectivesymbols)+lb
						'os+=lb
						'----------------- test link poly solver ---------------------
						
					end if
					
					for j=1 to kl 'save bestkey
						for k=0 to 1
							akey(j,k)=thread(t).gkey(j,k)
						next k
					next j
					
				else
					
					if thread(t).score>0 then 
						new_score-=temp
						if development=1 then
							select case genhc_mode
								case "nulls and skips"
									his+="T: "
									for i=1 to 3-len(str(t))
										his+="0"
									next i
									his+=str(t)+" A: 0 Score: "
									for i=1 to 5-len(str(int(thread(t).score)))
										his+="0"
									next i
									his+=str(int(thread(t).score))+" "+thread(t).itemname2+lb
							end select
						end if
					end if
					
				end if
				
				if thread(t).score>0 then
					its+=1
					cits+=1
					temp-=temp_min
				end if
				
				solvesub_nshcshift=oldshift*(its/itsmax) 'gradual shift
				
				if new_score>currbest then
					currbest=new_score
					for i=1 to kl
						for j=0 to 1
							bkey(sri,i,j)=akey(i,j)
						next j
					next i
				end if
				
				if solvesub_pnoverskip=1 then
					if best_score>=solvesub_pnover then
						cits=citsmax
						srj=srimax
						sri=srimax
					end if
				end if
				
				if cits=citsmax then 'restart 'int(citsmax)
				
					for i=1 to threads
				 		thread(i).solver_stop=1
				 		do
							sleep 0.001
				 		loop until thread(i).solver_waiting=1
				 		thread(i).score=0
					next i
					
					srj+=1
					if srj>sr(sri,0) then
						sri+=1
						srj=1
						currbest=0
						if sri>srimax then
							sri=1
							srj=1
							its=0
							select case genhc_mode
								case "nulls and skips"
									if development=1 then
										mkdir basedir+"\Output\"+str_nulls+str_rest+str_skips+"\"
										open basedir+"\Output\"+str_nulls+str_rest+str_skips+"\hc_restart_log_"+str(restarts+1)+".txt" for binary as #4
										print #4,his;
										close #4
										his=""
									end if
									if solvesub_pnmannulls+solvesub_pnmanskips=0 then
										kl_nulls-=1
										if kl_nulls<0 then 
											kl_nulls=kl
											itsmax*=solvesub_hciterationsfactor
										end if
									else
										itsmax*=solvesub_hciterationsfactor
									end if
								case else
									itsmax*=solvesub_hciterationsfactor
							end select
							ac=0
							restarts+=1
							iterations*=solvesub_iterationsfactor
							solvesub_nshcshift=0 'gradual shift
							if development=1 andalso restarts=solvesub_nshcrestartsmax then exit do
							global_best_score=0
							'best_score=0
							itsmax2=0
							for i=1 to srimax
								for j=1 to sr(i,0)
									sr(i,j)=(itsmax/srimax)/sr(i,0)
									itsmax2+=sr(i,j)
								next j
							next i
							sr(srimax,1)+=itsmax-itsmax2
						end if
					end if
					
					if sri=1 then
						select case genhc_mode 'init random key
							case "columnar rearrangement","columnar transposition"
								for i=1 to kl
									akey(i,0)=i
								next i
							case "nulls and skips"
								for i=1 to kl
									if i<=kl_nulls then 'nulls
										akey(i,1)=0
										do
											e=0
											akey(i,0)=int(rnd*l)+1
											for j=1 to i-1
												if akey(j,0)=akey(i,0) then
													e=1
													exit for
												end if
											next j
										loop until e=0
									else 'skips
										akey(i,1)=1
										do
											e=0
											akey(i,0)=int(rnd*(l+1))+1
											for j=1 to i-1
												if akey(j,1)=0 andalso akey(j,0)=akey(i,0) then
													e=1
													exit for
												end if
											next j
										loop until e=0
									end if
								next i
						end select
					else	
						for i=1 to kl
							for j=0 to 1
								akey(i,j)=bkey(sri-1,i,j)
							next j
						next i
					end if
					
					cits=0
					citsmax=sr(sri,srj)
					temp=temp_start-((temp_start/srimax)*(sri-1))
					temp_min=temp/citsmax
					new_score=0
					
				end if
				
				for i=1 to kl
					for j=0 to 1
						thread(t).gkey(i,j)=akey(i,j)
					next j
				next i				
				
				select case genhc_mode
					case "nulls and skips"
						str_nulls=""
						str_skips=""
						str_rest=""
						if kl_nulls=1 then str_nulls=str(kl_nulls)+" null"
						if kl_nulls>1 then str_nulls=str(kl_nulls)+" nulls"
						if kl-kl_nulls=1 then str_skips=str(kl-kl_nulls)+" skip"
						if kl-kl_nulls>1 then str_skips=str(kl-kl_nulls)+" skips"
						if kl_nulls>0 andalso kl-kl_nulls>0 then str_rest=" & "
						mkdir basedir+"\Output\"+str_nulls+str_rest+str_skips+"\"
						thread(t).outputdir=basedir+"\Output\"+str_nulls+str_rest+str_skips+"\"
					case else
						thread(t).outputdir=basedir+"\Output\"
				end select
				
				thread(t).l=l
				thread(t).s=s
				thread(t).dim_x=dx
				thread(t).dim_y=dy
				thread(t).score=0
				thread(t).pcmode=solvesub_pcmode
				thread(t).advstats=solvesub_advstats
				thread(t).iterations=iterations
				thread(t).temperature=solvesub_temperature
				thread(t).restarts=solvesub_restarts
				thread(t).subrestartlevels=solvesub_subrestartlevels
				thread(t).ngramfactor=solvesub_ngramfactor
				thread(t).multiplicityweight=solvesub_multiplicityweight
				thread(t).entweight=solvesub_entweight
				thread(t).solver_stop=0
				thread(t).update=0
				
				for i=1 to l
					thread(t).cip(i)=nba(i)
				next i
				
				'----------------- test link poly solver ---------------------
				'for i=1 to s
				'	thread(t).key(i)=local_cpol(i)
				'next i
				'----------------- test link poly solver ---------------------
				
				thread(t).solver_waiting=0 'engage thread
				
				if timer-statustimer>1 then
					statustimer=timer
					update_solver_status
				end if
				
			end if
			
		next t
		
	loop until stoptask=1
	stoptask=0
	
	solvesub_nshcshift=oldshift
	
	dim as double stucktimer=timer
	for i=1 to threads
		thread(i).solver_stop=1
		do
			sleep 10
		loop until thread(i).solver_waiting=1 or timer-stucktimer>2
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	'dim as double vls(l+1),low=999999999
	'for i=1 to threads
	'	for j=1 to l+1
	'		if thread(i).graph(j)<low then low=thread(i).graph(j)
	'		vls(j)+=thread(i).graph(j)
	'	next j
	'next i
	'vls(0)=low
	''if (l+1)/dx>dy then dy+=1
	'output_colormap(vls(),cip(),l,dx,dy,0,"null_heatmap")
	
	redim extcip(0,0,0)
	redim extkey(0,0,0,0)
	redim extinf(0,0,0)
	redim extpcm(0,0,0)
	
	clean_thread_information
	ext_hc=0
	solver_status_processing=0
	task_active="none"
	update_solver_status

end sub

sub thread_batch_settings(byval none as any ptr)
	
	dim as integer g,h,i,j,k,e,a,b,c,reallystop
	dim as double s1,s2
	dim as double rf,rt,ri
	dim as string o,fl,filename,n
	dim as double range(4)
	
	dim as double old_solvesub_entweight=solvesub_entweight 'save variables
	dim as double old_solvesub_ngramfactor=solvesub_ngramfactor
	dim as double old_solvesub_temperature=solvesub_temperature
	dim as double old_solvesub_ngramlogcutoff=solvesub_ngramlogcutoff
	dim as byte old_solvesub_outputdir=solvesub_outputdir
	dim as byte old_solvesub_fastent=solvesub_fastent
	dim as string old_solvesub_ngramloctemp=solvesub_ngramloctemp
	
	task_active="batch settings"
	update_solver_status
	'stoptask=0
	
	filename=ui_loadsavedialog(0,"Open settings list",filter,1,basedir+"\Misc\") 'get settings list
	if fileexists(filename)=0 then
		if filename<>"" then ui_editbox_settext(output_text,"Error: file not found")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	open filename for binary as #4
	
	ui_editbox_settext(output_text,"Please wait...")
	
	dim as integer batchsize,newbatchsize,curr,maxbatchsize=100000,arg=3
	dim as integer output_history,rangeone,increment,sortmode=0,nn
	dim as double bcsbatch(maxbatchsize,arg)
	'dim as string ngramnames(maxbatchsize)
	
	dim as ubyte local_ngram_values(255),cutoff(255)
	for i=0 to 255
		local_ngram_values(i)=ngram_values(i)
	next i
	j=0
	for i=1 to 255
		if local_ngram_values(i)>0 then
			j+=1
			cutoff(j)=i
		end if
	next i
	
	'todo:
	'-----
	'not bug on 0 values
	'not reload n-grams for cutoff
	
	do
		
		line input #4,fl
		
		fl=lcase(fl)
		fl+=" "
		
		if instr(fl,"sort:")>0 then 'get sort
			sortmode=val(ltrim(rtrim(right(fl,len(fl)-(instr(fl,":")+1)))))
		end if
		
		if instr(fl,"ngrams:")>0 andalso nn=0 then 'load specific n-grams
			loadngrams_showmsg=0
			solvesub_ngramloctemp=basedir+ltrim(rtrim(right(fl,len(fl)-(instr(fl,":")+1))))
			task_active="loading n-grams"
			update_solver_status
			thread_ptr(threadsmax+3)=threadcreate(@thread_load_ngrams,0)
			nn+=1
			sleep 100
			do
				sleep 0.001
			loop until task_active="none"
			sleep 100
			loadngrams_showmsg=1
			for i=0 to 255
				local_ngram_values(i)=ngram_values(i)
			next i
			j=0
			for i=1 to 255
				if local_ngram_values(i)>0 then
					j+=1
					cutoff(j)=i
				end if
			next i
		end if
		
		if instr(fl,"file:")>0 then 'get batch file
			bcsfilename=basedir+ltrim(rtrim(right(fl,len(fl)-(instr(fl,":")+1))))
			if fileexists(bcsfilename)=0 then
				close #1
				if bcsfilename<>"" then ui_editbox_settext(output_text,"Error: batch file not found")
				'if bcsfilename<>"" then ui_editbox_settext(output_text,bcsfilename)
				task_active="none"
				update_solver_status
				exit sub
			end if
		end if
		
		if instr(fl,"range:")>0 then 'get range
			for h=1 to 4
				select case h
					case 1:a=instr(instr(fl,"ent"),fl,any" .,0123456789") ':o+="ent "
					case 2:a=instr(instr(fl,"temp"),fl,any" .,0123456789") ':o+="temp "
					case 3:a=instr(instr(fl,"log"),fl,any" .,0123456789") ':o+="top "
					case 4:a=instr(instr(fl,"top"),fl,any" .,0123456789") ':o+="top "
				end select
				if a>0 then
					c=0
					n=""
					erase range
					for i=a to len(fl)
						select case chr(asc(fl,i))
							case ".",",","0","1","2","3","4","5","6","7","8","9"
								n+=chr(asc(fl,i))
							case " ","-",chr(9)
								if len(n)>0 then
									c+=1
									range(c)=val(n)
									n="" ':o+=n+" "
								end if
								if c=3 then exit for 'from,to,step
							case else
								exit for
						end select
					next i
					e=1
					for i=1 to 3 'from,to,step
						if range(i)=0 then 
							if range(1)>0 then
								e=1
								rangeone=1
							else
								e=0
							end if
							exit for
						end if
					next i
					if e=1 then
						curr=batchsize
						if rangeone=0 then 'inc or decrement range
							if range(2)>range(1) then
								increment=1
							else
								increment=0
							end if
						end if
						k=0
						do
							curr+=1
							if curr>maxbatchsize then
								close #1
								if bcsfilename<>"" then ui_editbox_settext(output_text,"Error: too many setting entries")
								task_active="none"
								update_solver_status
								exit sub
							end if
							select case h
								case 1,2,3 'ent,temp,log
									bcsbatch(curr,h)=range(1)
									if rangeone=1 then
										rangeone=0
										exit do
									end if
									if increment=1 then
										range(1)+=range(3)
										if range(1)>range(2)+0.0000000001 then exit do 'rounding errors band-aid
									else 'decrement
										range(1)-=range(3)
										if range(1)<range(2)-0.0000000001 then exit do 'rounding errors band-aid
									end if
								case 4 'top
									k+=1
									for i=1 to 3
										if bcsstats(k,i)=0 then
											curr-=1
											exit do
										end if
										bcsbatch(curr,i)=bcsstats(k,i+14)
									next i
									if k=range(1) then exit do
							end select	
						loop
						if curr>newbatchsize then newbatchsize=curr
					end if
				end if
			next h
			batchsize=newbatchsize
		end if
		
		if instr(fl,"process")>0 then 'process settings (add multiple times ???)
			if bcsfilename<>"" then
				redim bcsstats(batchsize,17)
				for i=0 to batchsize
					for j=0 to 17
						bcsstats(i,j)=0
					next j
				next i
				for i=1 to batchsize 'check bcsbatch for empty values...
					for h=1 to 3
						if bcsbatch(i,h)=0 then 
							if bcsbatch(i-1,h)>0 then
								bcsbatch(i,h)=bcsbatch(i-1,h) 'use previous value
							else
								select case h
									case 1:bcsbatch(i,h)=old_solvesub_entweight
									case 2:bcsbatch(i,h)=old_solvesub_temperature
									case 3:bcsbatch(i,h)=old_solvesub_ngramlogcutoff
								end select
							end if
						end if
					next h
					'o+=format(bcsbatch(i,1),"0.00")+" "+format(bcsbatch(i,2),"0.00")+lb
				next i	
				solvesub_outputdir=0 'do not output ciphers
				bcsi=0 'reset global counter
				for h=1 to batchsize 'main loop
					sleep 100 'give CPU some time
					'---------------------------------------------------------------------------------
					if bcsbatch(h,3)>255 then bcsbatch(h,3)=0
					solvesub_ngramlogcutoff=cutoff(bcsbatch(h,3)) 'apply n-gram log cutoff
					if solvesub_ngramlogcutoff<>old_solvesub_ngramlogcutoff then 'apply cutoff
						loadngrams_showmsg=0
						task_active="loading n-grams"
						update_solver_status
						thread_ptr(threadsmax+3)=threadcreate(@thread_load_ngrams,0)
						sleep 100
						do
							sleep 0.001
						loop until task_active="none"
						sleep 100
						loadngrams_showmsg=1
					end if
					'solvesub_matchweight=bcsbatch(h,3) 'testing
					'---------------------------------------------------------------------------------
					solvesub_entweight=bcsbatch(h,1) 'apply entropy weight
					select case solvesub_entweight
						case 0.25:solvesub_fastent=1
						case 0.5:solvesub_fastent=2
						case 0.75:solvesub_fastent=3
						case 1:solvesub_fastent=4
						case 1.5:solvesub_fastent=5
						case 2:solvesub_fastent=6
						case else:solvesub_fastent=0
					end select
					'---------------------------------------------------------------------------------
					solvesub_temperature=bcsbatch(h,2) 'apply temperature
					'---------------------------------------------------------------------------------
					normalize_ngramfactor
					bcsbatch(h,0)=solvesub_ngramfactor
					'ngramnames(h)=solver_file_name_ngrams
					batchciphers_showmsg=0 'mute thread_batch_ciphers_substitution
					
					set_solverhighlight("substitution") 'set solver to substitution
					toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",4,1,threads) 'stop solver
					toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",2,1,threads) 'stop thread
					toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",1,1,threads) 'start thread
					task_active="batch ciphers (substitution)"
					thread_ptr(threadsmax+3)=threadcreate(@thread_batch_ciphers_substitution,0)
					
					do 'wait for thread(s) to finish
						sleep 0.001
						if stoptask=1 then
							reallystop=1
							exit do,do 'for
						end if
					loop until task_active="none"
					'update_solver_status
					
					sleep 100 'give CPU some time
					do 'sort
						e=0
						for i=1 to h-1
							select case sortmode
								case 0 'sort by average accuracy 'Avg
									s1=bcsstats(i,3)
									s2=bcsstats(i+1,3)
								case 1 'sort by average accuracy * max accuracy 'Ams (more emphasis on max accuracy)
									s1=(bcsstats(i,3)*100)*(bcsstats(i,14)*100)
									s2=(bcsstats(i+1,3)*100)*(bcsstats(i+1,14)*100)
								case 2 'sort by average accuracy / sqr(time) 'Ats (attempt to factor in time)
									s1=100*(bcsstats(i,3)*100)/sqr(bcsstats(i,2))
									s2=100*(bcsstats(i+1,3)*100)/sqr(bcsstats(i+1,2))
								case 3 'sort by score
									s1=bcsstats(i,0)
									s2=bcsstats(i+1,0)
							end select
							if s1<s2 then
								e=1
								'swap ngramnames(i),ngramnames(i+1)
								for j=0 to 17
									swap bcsstats(i,j),bcsstats(i+1,j)
								next j
								for j=0 to arg
									swap bcsbatch(i,j),bcsbatch(i+1,j)
								next j
							end if
						next i
					loop until e=0
					o="BHdecrypt batch settings: "+right(bcsfilename,len(bcsfilename)-instrrev(bcsfilename,"\"))+lb 'output current
					'if nn=0 then o+=solver_file_name_ngrams+lb
					o+=solver_file_name_ngrams+lb
					o+="---------------------------------------------------------"+lb
					for i=1 to h 'output
						select case sortmode
							case 0:o+="Avg: "+format(bcsstats(i,3)*100,"0.00")+"% Max: "+format(bcsstats(i,14)*100,"0.00")+"% Time: "+format(bcsstats(i,2),"0.0")+" Ent: "+format(bcsbatch(i,1),"0.00")+" Temp: "+format(bcsbatch(i,2),"0.00")+" Log: >"+format(cutoff(bcsbatch(i,3)),"0.00")
							case 1:o+="Ams: "+format((bcsstats(i,3)*100)*(bcsstats(i,14)*100),"0.00")+" Avg: "+format(bcsstats(i,3)*100,"0.00")+"% Max: "+format(bcsstats(i,14)*100,"0.00")+"% Time: "+format(bcsstats(i,2),"0.0")+" Ent: "+format(bcsbatch(i,1),"0.00")+" Temp: "+format(bcsbatch(i,2),"0.00")+" Log: >"+format(cutoff(bcsbatch(i,3)),"0.00")
							case 2:o+="Ats: "+format(100*(bcsstats(i,3)*100)/sqr(bcsstats(i,2)),"0.0")+" Avg: "+format(bcsstats(i,3)*100,"0.00")+"% Max: "+format(bcsstats(i,14)*100,"0.00")+"% Time: "+format(bcsstats(i,2),"0.0")+" Ent: "+format(bcsbatch(i,1),"0.00")+" Temp: "+format(bcsbatch(i,2),"0.00")+" Log: >"+format(cutoff(bcsbatch(i,3)),"0.00")
							case 3:o+="Score: "+str(int(bcsstats(i,0)))+" Avg: "+format(bcsstats(i,3)*100,"0.00")+"% Max: "+format(bcsstats(i,14)*100,"0.00")+"% Time: "+format(bcsstats(i,2),"0.0")+" Ent: "+format(bcsbatch(i,1),"0.00")+" Temp: "+format(bcsbatch(i,2),"0.00")+" Log: >"+format(cutoff(bcsbatch(i,3)),"0.00")
						end select
						'if nn>0 then o+=" ("+str(ngramnames(i))+")"
						if i<>h then o+=lb
					next i
					ui_editbox_settext(output_text,o)
				next h
				output_history+=1
				open basedir+"/output/batch settings "+str(output_history)+".txt" for output as #5 'output history
				print #5,o;
				close #5
				for i=0 to batchsize 'clean stuff
					for j=0 to arg
						bcsbatch(i,j)=0
					next j
				next i
				batchsize=0
				newbatchsize=0
				nn=0
			end if
		end if
		
	loop until eof(4) or stoptask=1 or reallystop=1
	
	close #4
	
	if ui_editbox_gettext(output_text)="Please wait..." then ui_editbox_settext(output_text,"")
	
	'if output_history>0 then
		do
			sleep 10
		loop until task_active="none"
	'else
	'	ui_editbox_settext(output_text,"")
	'end if
	
	redim bcsstats(0,0)
	
	solvesub_temperature=old_solvesub_temperature 'restore variables
	solvesub_entweight=old_solvesub_entweight
	solvesub_ngramfactor=old_solvesub_ngramfactor
	solvesub_outputdir=old_solvesub_outputdir
	solvesub_fastent=old_solvesub_fastent
	if solvesub_ngramlogcutoff<>old_solvesub_ngramlogcutoff then 'restore old n-grams
		solvesub_ngramlogcutoff=old_solvesub_ngramlogcutoff
		loadngrams_showmsg=0
		solvesub_ngramloctemp=old_solvesub_ngramloctemp
		task_active="loading n-grams"
		update_solver_status
		thread_ptr(threadsmax+3)=threadcreate(@thread_load_ngrams,0)
		sleep 100
		do
			sleep 0.001
		loop until task_active="none"
		sleep 100
		loadngrams_showmsg=1
	end if
	clean_thread_information
	batchciphers_showmsg=1
	solver_status_processing=0
	task_active="none"
	update_solver_status
	
	stoptask=0
	reallystop=0
	
end sub

sub thread_batch_ciphers_substitution(byval none as any ptr)
	
	dim as integer h,i,j,k,l,s,e,m,a,b
	dim as integer metainfo,lmax,indacci
	dim as integer cipherend
	dim as integer numeric,ignored
	dim as integer cipherline,solution
	dim as integer contain_numbers
	dim as integer contain_spaces
	dim as integer contain_symbols
	dim as string num,filename
	dim as string char,ln,ot,bestresult
	dim as string outputdir=basedir+"\Output\"
	dim as string itemname
	dim as integer batchinfo(constcip)
	dim as integer batchsol(constcip)
	dim as integer batchnba(constcip)
	dim as integer ident(10000) 'max number size
	dim as integer count,count2
	dim as integer batch_x
	dim as integer batch_y
	dim as uinteger items,items2,acctest,cb,bigrams,items_skipped
	dim as integer acca(100)
	dim as double iterations=solvesub_iterations
	dim as double batchtimer,highscore,lowscore=999999999999999
	dim as double batchtime,avgacc,acc,avgmp
	dim as uinteger avgsymbols,avglength
	dim as string outeditbox
	if solvesub_batchciphersbigrams=0 then e=0 else e=1
	dim as short bgid(constcip*e,constcip*e)
	
	if batchciphers_showmsg=1 then task_active="batch ciphers (substitution)"
	update_solver_status
	
	if batchciphers_showmsg=1 then
		filename=ui_loadsavedialog(0,"Open cipher batch file (substitution)",filter,1,basedir+"\Ciphers\")
	else
		bcsi+=1
		filename=bcsfilename
	end if
	
	if fileexists(filename)=0 then
		if filename<>"" then ui_editbox_settext(output_text,"Error: file not found")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	if batchciphers_showmsg=1 then ui_editbox_settext(output_text,"Please wait, processing batch file...")
	
	ui_setwindowtext(window_main,program_name+" - "+right(filename,len(filename)-instrrev(filename,"\")))
	
	if batchciphers_showmsg=1 then
		set_solverhighlight("substitution")
'		set_solverhighlight("bigram substitution") ' beijinghouse special dev testing
		'ui_listbox_setcursel(list_main,0) 'set solver to Substitution
		toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",4,1,threads) 'stop solver
		toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",2,1,threads) 'stop thread
		toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",1,1,threads) 'start thread
	end if
	
	lmax=constcip
	
	sleep twait
	
	stoptask=0
	global_best_score=0
	solvesub_reversesolve=0
	shared_string=""
	solver_status_processing=1
	task_active="batch ciphers (substitution)"
	update_solver_status
	
	'for i=0 to individual_accuracy
	'	indacc(i).n=""
	'	indacc(i).m=0
	'next i
	'redim indacc(0)
	
	individual_accuracy=0
	
	batchtimer=timer
	ips_timer=timer
	sectimer=timer
	dim as double statustimer=timer
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then 
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
	end if
	
	open filename for binary as #1
	
	do
		
		line input #1,ln	
		metainfo=0
		
		if instr(ln,"output_sub_directory=")>0 or instr(ln,"results_sub_directory=")>0 then
			metainfo=1
			if solvesub_outputdir=1 then
				outputdir=basedir+"\Output\"+right(ln,len(ln)-instr(ln,"="))+"\"
				mkdir outputdir
			end if
			itemname=right(ln,len(ln)-instr(ln,"=")) 'if no name set by cipher_information, use sub_directory as name
		end if
		
		if instr(ln,"cipher_information=")>0 then
			metainfo=1
			itemname=right(ln,len(ln)-instr(ln,"="))		
		end if
		
		if instr(ln,"reverse_solve=")>0 then
			metainfo=1
			solvesub_reversesolve=val(right(ln,len(ln)-instr(ln,"=")))
			if solvesub_reversesolve<>0 andalso solvesub_reversesolve<>1 then solvesub_reversesolve=0
		end if
		
		if instr(ln,"iterations_factor=")>0 then
			metainfo=1
			iterations*=val(right(ln,len(ln)-instr(ln,"=")))
			if solvesub_reversesolve=0 andalso iterations<100000 then iterations=100000
			'if iterations<100000 then iterations=100000
		end if
		
		if instr(ln,"iterations=")>0 then
			metainfo=1
			iterations=val(right(ln,len(ln)-instr(ln,"=")))
			if solvesub_reversesolve=0 andalso iterations<100000 then iterations=100000
			'if iterations<100000 then iterations=100000
		end if
		
		if instr(ln,"solution_plaintext=")>0 then
			metainfo=1
			solution=1
		end if
		
		if ln="" or eof(1) then cipherend=1
		
		if metainfo=0 then 
			if solution=0 then 'ciphertext
				cipherline+=1
				contain_spaces=0
				contain_symbols=0
				contain_numbers=0	
				if cipherline=1 then
					count=0
					count2=0
					for i=1 to len(ln) 'check if numeric
						select case asc(ln,i)
							case 32 'space
								contain_spaces=1
							case 48 to 57 'numbers
								contain_numbers=1
							case else 'symbols
								contain_symbols=1
						end select
					next i
					if contain_spaces=1 andalso contain_numbers=1 andalso contain_symbols=0 then
						numeric=1
					else
						numeric=0
					end if
				end if
				if numeric=1 then
					for i=1 to len(ln)
						select case asc(ln,i)
							case 32 'space
								if num<>"" then
									count+=1
									if count<=lmax then
										batchinfo(count)=val(num)
									end if
									num=""
								end if
							case 48 to 57 'numbers
								num+=chr(asc(ln,i))
							case else 'symbols
								'error
						end select
					next i
					if num<>"" then
						count+=1
						if count<=lmax then
							batchinfo(count)=val(num)
						end if
						num=""
					end if
				else
					for i=1 to len(ln)
						select case asc(ln,i)
							case 32 'space
								'error
							case else 'symbols
								count+=1
								if count<=lmax then
									batchinfo(count)=asc(ln,i)
								end if
						end select
					next i
				end if
			else 'plaintext
				for i=1 to len(ln)
					select case asc(ln,i)
						case 32 'space
							'error
						case else 'symbols
							count2+=1
							if count2<=lmax then
								batchsol(count2)=asc(ln,i)
							end if
					end select
				next i
			end if
		end if
		
		if cipherline=1 then batch_x=count
		
		if cipherend=1 then
			if count>ngram_size then
				batch_y=cipherline
				l=count
				if l>lmax then l=lmax
				s=0
				
				erase ident
				for i=1 to l
					if ident(batchinfo(i))=0 then
						s+=1
						ident(batchinfo(i))=s
						batchnba(i)=s
					else
						batchnba(i)=ident(batchinfo(i))
					end if
				next i
				
				if solvesub_batchciphersbigrams>0 then 'bigram filter
					bigrams=0
					cb+=1
					for i=1 to l-1
						a=batchnba(i)
						b=batchnba(i+1)
						if bgid(a,b)<cb then
							bgid(a,b)=cb
						else 
							bigrams+=1
						end if
					next i
				end if
				
				if solvesub_batchciphersbigrams=0 or bigrams>solvesub_batchciphersbigrams then
					
					do
						for i=1 to threads
							if pausetask=1 then 'pause task
								update_solver_status
								do
									sleep 10
								loop until pausetask=0
							end if
							if timer-statustimer>1.1 then
								statustimer=timer
								update_solver_status
							end if
							if thread(i).solver_waiting=1 then
								items+=1
								
								if thread(i).score>0 then
									items2+=1
									if thread(i).score>highscore then
										highscore=thread(i).score
										'bestresult=thread(info_to_string(thread(i).sol(),thread(i).l,thread(i).dx,thread(i).dy,0)
										'ot="Processing batch file, intermediate highest result:"
										'ot+="---------------------------------------------------------"+lb
										'ot+=bestresult
										'ui_editbox_settext(output_text,ot)
									end if
									if thread(i).score<lowscore then lowscore=thread(i).score
									
									avgmp+=thread(i).s/thread(i).l
									avgsymbols+=thread(i).s
									avglength+=thread(i).l
									
									if thread(i).solkey=1 then 'get accuracy
										m=0
										ignored=0
										for j=1 to thread(i).l
											if thread(i).key(j)=42 then 'ignore for accuracy
												ignored+=1
											else
												if thread(i).sol(j)=thread(i).key(j) then m+=1
											end if
										next j
										acc=m/(thread(i).l-ignored)
										if acc>=0.5 then avgacc+=acc 'avgacc+=acc
										for j=0 to 10
											if acc>=j/10 then acca(j)+=1
										next j
										if batchciphers_showmsg=1 then
											e=0
											for j=1 to individual_accuracy 'get individual accuracies
												if indacc(j).n=thread(i).itemname then
													e=1
													indacc(j).m+=acc
													indacc(j).c+=1
													exit for
												end if
											next j
											if e=0 then
												individual_accuracy+=1
												redim preserve indacc(individual_accuracy)
												indacc(individual_accuracy).n=thread(i).itemname
												indacc(individual_accuracy).m=acc
												indacc(individual_accuracy).c=1
											end if
										end if
									end if	
								end if
								
								thread(i).outputdir=outputdir
								thread(i).l=l
								thread(i).s=s
								thread(i).dim_x=batch_x
								thread(i).dim_y=batch_y
								thread(i).score=0
								thread(i).pcmode=0
								thread(i).itemnumber=items
								thread(i).itemname=itemname
								thread(i).advstats=solvesub_advstats
								thread(i).iterations=iterations
								thread(i).temperature=solvesub_temperature
								thread(i).restarts=solvesub_restarts
								thread(i).subrestartlevels=solvesub_subrestartlevels
								thread(i).ngramfactor=solvesub_ngramfactor
								thread(i).multiplicityweight=solvesub_multiplicityweight
								thread(i).entweight=solvesub_entweight
								thread(i).solver_stop=0
								for j=1 to l
									thread(i).cip(j)=batchnba(j)
								next j
								if count2=l then
									acctest=1
									thread(i).solkey=1
									for j=1 to l
										thread(i).key(j)=batchsol(j)
									next j
								end if
								thread(i).update=0
								thread(i).solver_waiting=0 'engage thread	
								exit do
							end if
						next i
						sleep 0.1
					loop until stoptask=1
				
				else
					items_skipped+=1
					solvesub_batchciphersbigramsskipped+=1 'added to items in status window
				end if
				
			end if
			cipherline=0
			cipherend=0
			solution=0
		end if
		
	loop until eof(1) or stoptask=1
	
	close #1
	
	if stoptask=1 then
		for i=1 to threads
			thread(i).solver_stop=1
		next i
	else
		do
			for i=1 to threads
				if thread(i).solver_waiting=1 then
					if thread(i).score>0 then
						items2+=1
						if thread(i).score>highscore then highscore=thread(i).score
						if thread(i).score<lowscore then lowscore=thread(i).score
						thread(i).score=0
						
						avgmp+=thread(i).s/thread(i).l
						avgsymbols+=thread(i).s
						avglength+=thread(i).l
						
						if thread(i).solkey=1 then 'get accuracy
							m=0
							ignored=0
							for j=1 to thread(i).l
								if thread(i).key(j)=42 then 'ignore for accuracy
									ignored+=1
								else
									if thread(i).sol(j)=thread(i).key(j) then m+=1
								end if
							next j
							acc=m/(thread(i).l-ignored)
							if acc>=0.5 then avgacc+=acc 'avgacc+=acc
							for j=0 to 10
								if acc>=j/10 then acca(j)+=1
							next j
							
							if batchciphers_showmsg=1 then
								e=0
								for j=1 to individual_accuracy 'get individual accuracies
									if indacc(j).n=thread(i).itemname then
										e=1
										indacc(j).m+=acc
										indacc(j).c+=1
										exit for
									end if
								next j
								if e=0 then
									individual_accuracy+=1
									redim preserve indacc(individual_accuracy)
									indacc(individual_accuracy).n=thread(i).itemname
									indacc(individual_accuracy).m=acc
									indacc(individual_accuracy).c=1
								end if
							end if
						end if
					end if
				end if
			next i
			sleep 0.1
		loop until items2=items
	end if
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3	
	batchtime=timer-batchtimer
	if file_name<>"" then
		ui_setwindowtext(window_main,program_name+" - "+file_name)
	else
		ui_setwindowtext(window_main,program_name)
	end if
	
	update_solver_status
	
	if batchciphers_showmsg=1 then
		ot="BHdecrypt batch ciphers (substitution) for: "+right(filename,len(filename)-instrrev(filename,"\"))+lb
		if acctest=1 then ot+=solver_file_name_ngrams+lb
		ot+="---------------------------------------------------------"+lb
		ot+="Items processed: "+str(items2)+lb
		if items_skipped>0 then ot+="Items skipped: "+str(items_skipped)+" ("+format(items_skipped/(items_skipped+items2)*100,"0.00")+"%)"+lb
		ot+="Iterations: "+str(solvesub_iterations)+" Restarts: "+str(solvesub_restarts)+lb
		ot+="Entropy weight: "+str(solvesub_entweight)+" Temperature: "+str(solvesub_temperature)+lb
		ot+="Average score: "+format(solvesub_avgscore/items2,"0.00000")+" Highest: "+format(highscore,"0.00")+ " Lowest: "+format(lowscore,"0.00")+lb
		ot+="Average IOC: "+format(solvesub_avgioc/items2,"0.00000")+lb
		ot+="Average PC-cycles: "+format(solvesub_avgpccycles/items2,"0.00")+lb
		ot+="Average multiplicity: "+format((avgmp/items2),"0.00000")+lb
		ot+="Processing time: "+format(batchtime,"0.00")+" seconds"+lb
		ot+="- Per cipher: "+format(batchtime/items2,"0.00000")+ " seconds"
		if acctest=1 then	
			ot+=lb+lb
			ot+="Accuracy/time score: "+format(100*(avgacc/items2*100)/sqr(batchtime),"0.00")+lb
			'ot+="Accuracy/time score: "+format(10*(avgacc/items2*100)*(1+acca(10)/items2*100)/sqr(batchtime),"0.00")+lb
			ot+=lb
			ot+="Average accuracy: "+format((avgacc/items2)*100,"0.00")+"%"+lb
			ot+="-------------------------------"+lb
			for i=0 to 10
				ot+="Accuracy equal/over "+str(i*10)+"%: "+str(format((acca(i)/items2)*100,"0.00"))+"%"
				if i<>10 then ot+=lb
			next i	
			do 'sort
				e=0
				for i=1 to individual_accuracy-1
					if indacc(i).m/indacc(i).c>indacc(i+1).m/indacc(i).c then
						e=1
						swap indacc(i).n,indacc(i+1).n
						swap indacc(i).m,indacc(i+1).m
						swap indacc(i).c,indacc(i+1).c
					end if
				next i
			loop until e=0
			ot+=lb+lb
			'ot+="Highest result:"+lb
			'ot+="-------------------------------"+lb
			'ot+=bestresult+lb+lb
			ot+="Individual accuracies:"+lb
			ot+="-------------------------------"+lb
			for i=1 to individual_accuracy
				ot+=indacc(i).n+": "+format((indacc(i).m/indacc(i).c)*100,"0.00")+"%"
				if i<>individual_accuracy then ot+=lb
			next i
		else
			'ot+=lb+lb
			'ot+="Highest result:"+lb
			'ot+="-------------------------------"+lb
			'ot+=bestresult
		end if
		ui_editbox_settext(output_text,ot)
	else
		'update_solver_status
		bcsstats(bcsi,0)=solvesub_avgscore/items2
		bcsstats(bcsi,1)=solvesub_avgioc/items2
		bcsstats(bcsi,2)=batchtime
		bcsstats(bcsi,3)=avgacc/items2 'avg acc
		for i=0 to 10
			bcsstats(bcsi,4+i)=acca(i)/items2
		next i
		bcsstats(bcsi,15)=solvesub_entweight
		bcsstats(bcsi,16)=solvesub_temperature
		bcsstats(bcsi,17)=solvesub_ngramlogcutoff
	end if
	
	redim indacc(0)
	
	solvesub_batchciphersbigramsskipped=0
	solver_status_processing=0
	clean_thread_information
	task_active="none"
	if batchciphers_showmsg=1 then update_solver_status
	
	if stoptask=0 andalso solvesub_batchshutdown=1 then shell ("SHUTDOWN /s /t 10 ")
	stoptask=0

end sub

sub ext_bigram_beam_columnartransposition(byval t as short,byval l as short,byval s as short,byval kl as short)
	
	dim as short i,j,x,y,bg,bbg,a,b,lpi,r1,r2
	dim as short rbw=solvesub_ctdepth
	dim as short rbd=solvesub_ctdepth
	dim as short bs(rbd,1)
	dim as short lp(rbd)
	dim as short ch(rbd)
	dim as short cl(kl)
	dim as short bkey(kl)
	dim as short ukey(kl)
	dim as short cip(l)
	dim as short cip2(l)
	dim as uinteger state=1+(rnd*1073741824)*2 '((int(rnd*1073741824)+1)*2)-1
	dim as short cdx=kl
	dim as short cdy=l\kl
	if cdy*kl<l then cdy+=1
	dim as short grid(cdx,cdy)
	dim as integer id(s,s)
	dim as integer cb
	
	dim as short pcmg(cdx,cdy)
	dim as short pcm(l),pcm2(l)
	
	for i=1 to rbd
		lp(i)=1
		ch(i)=1
	next i
	for i=1 to kl
		bkey(i)=thread(t).gkey(i,0)
		if l mod kl>0 andalso i>l mod kl then cl(i)=cdy-1 else cl(i)=cdy
	next i
	for i=1 to l
		cip(i)=thread(t).cip(i)
		if solvesub_pcmode=1 then pcm(i)=i
	next i
	
	bbg=0
	do
		for i=1 to rbd
			if ch(i)=1 then
				'ms_state=(214013*ms_state+2531011)mod 2147483648
				'r1=int(((ms_state shr 16)/32768)*kl)+1
				state=48271*state and 2147483647
				r1=1+kl*state shr 31
				bs(i,0)=r1
				'ms_state=(214013*ms_state+2531011)mod 2147483648
				'r2=int(((ms_state shr 16)/32768)*kl)+1
				state=48271*state and 2147483647
				r2=1+kl*state shr 31
				bs(i,1)=r2
			end if
		next i
		for i=1 to kl
			ukey(i)=bkey(i)
		next i
		for i=1 to rbd
			swap ukey(bs(i,0)),ukey(bs(i,1))
		next i
		
		j=0
		for i=1 to kl 'apply key to cipher
			for y=1 to cl(ukey(i))
				j+=1
				grid(ukey(i),y)=cip(j)
				if solvesub_pcmode=1 then pcmg(ukey(i),y)=pcm(j)
			next y
		next i
		i=0
		for y=1 to cdy
			for x=1 to cdx
				i+=1
				cip2(i)=grid(x,y)
				if solvesub_pcmode=1 then pcm2(i)=pcmg(x,y)
				if i=l then exit for,for
			next x
		next y
		
		cb+=1
		bg=0
		for i=1 to l-1 'count bigrams
			a=cip2(i)
			b=cip2(i+1)
			if id(a,b)<cb then
				id(a,b)=cb
			else 
				bg+=1
			end if
		next i
		
		if bg>bbg then
			bbg=bg
			for i=1 to kl
				thread(t).gkey(i,0)=ukey(i)
			next i
			for i=1 to l
				thread(t).cip(i)=cip2(i)
				if solvesub_pcmode=1 then extpcm(t,1,i)=pcm2(i)
			next i
		end if
		for i=1 to rbd
			ch(i)=0
		next i
		lpi=rbd
		do
			if lp(lpi)<rbw then
				lp(lpi)+=1
				ch(lpi)=1
				exit do
			else
				if lpi=1 then exit do,do
				lp(lpi)=1
				ch(lpi)=1
			end if
			lpi-=1
		loop
	loop
	thread(t).itemname="Column order("
	for i=1 to kl
		thread(t).itemname+=str(thread(t).gkey(i,0))
		if i<>kl then thread(t).itemname+=","
	next i
	thread(t).itemname+=")"

end sub

sub ext_bigram_beam_columnarrearrangement(byval t as short,byval l as short,byval s as short,byval kl as short)
	
	dim as short i,j,x,y,bg,bbg,a,b,lpi,r1,r2
	dim as short rbw=solvesub_ctdepth
	dim as short rbd=solvesub_ctdepth
	dim as short bs(rbd,1)
	dim as short lp(rbd)
	dim as short ch(rbd)
	dim as short cl(kl)
	dim as short bkey(kl)
	dim as short ukey(kl)
	dim as short cip2(l)
	dim as uinteger state=1+(rnd*1073741824)*2 '((int(rnd*1073741824)+1)*2)-1
	dim as short cdx=kl
	dim as short cdy=l\kl
	if cdy*kl<l then cdy+=1
	dim as short grid(cdx,cdy)
	dim as short grid2(cdx,cdy)
	dim as integer id(s,s)
	dim as integer cb
	
	dim as short pcmg(cdx,cdy)
	dim as short pcmg2(cdx,cdy)
	dim as short pcm(l)
	
	for i=1 to rbd
		lp(i)=1
		ch(i)=1
	next i
	for i=1 to kl
		bkey(i)=thread(t).gkey(i,0)
	next i
	
	i=0
	for y=1 to cdy
		for x=1 to cdx
			i+=1
			grid(x,y)=thread(t).cip(i)
			if solvesub_pcmode=1 then pcmg(x,y)=i
			if i=l then exit for,for
		next x
	next y
	
	bbg=0
	do
		for i=1 to rbd
			if ch(i)=1 then
				'ms_state=(214013*ms_state+2531011)mod 2147483648
				'r1=int(((ms_state shr 16)/32768)*kl)+1
				state=48271*state and 2147483647
				r1=1+kl*state shr 31
				bs(i,0)=r1
				'ms_state=(214013*ms_state+2531011)mod 2147483648
				'r2=int(((ms_state shr 16)/32768)*kl)+1
				state=48271*state and 2147483647
				r2=1+kl*state shr 31
				bs(i,1)=r2
			end if
		next i
		for i=1 to kl
			ukey(i)=bkey(i)
		next i
		for i=1 to rbd
			swap ukey(bs(i,0)),ukey(bs(i,1))
		next i
		
		j=0
		for i=1 to kl 'apply key to cipher
			for y=1 to cdy
				grid2(i,y)=grid(ukey(i),y)
				if solvesub_pcmode=1 then pcmg2(i,y)=pcmg(ukey(i),y)
			next y
		next i
		i=0
		for y=1 to cdy
			for x=1 to cdx
				if grid2(x,y)>0 then 
					i+=1
					cip2(i)=grid2(x,y)
					if solvesub_pcmode=1 then pcm(i)=pcmg2(x,y)
				end if
				if i=l then exit for,for
			next x
		next y
		
		cb+=1
		bg=0
		for i=1 to l-1
			a=cip2(i)
			b=cip2(i+1)
			if id(a,b)<cb then
				id(a,b)=cb
			else 
				bg+=1
			end if
		next i
		if bg>bbg then
			bbg=bg
			for i=1 to kl
				thread(t).gkey(i,0)=ukey(i)
			next i
			for i=1 to l
				thread(t).cip(i)=cip2(i)
				if solvesub_pcmode=1 then extpcm(t,1,i)=pcm(i)
			next i
		end if
		for i=1 to rbd
			ch(i)=0
		next i
		lpi=rbd
		do
			if lp(lpi)<rbw then
				lp(lpi)+=1
				ch(lpi)=1
				exit do
			else
				if lpi=1 then exit do,do
				lp(lpi)=1
				ch(lpi)=1
			end if
			lpi-=1
		loop
	loop
	thread(t).itemname="Column order("
	for i=1 to kl
		thread(t).itemname+=str(thread(t).gkey(i,0))
		if i<>kl then thread(t).itemname+=","
	next i
	thread(t).itemname+=")"

end sub

sub ext_bhd_beam_columnarrearrangement(byval t as short,byval l as short,byval s as short,byval kl as short)
	
	dim as short i,j,k,x,y,bg,bbg,a,b,lpi,r1,r2
	dim as short rbw=solvesub_ctdepth
	dim as short rbd=solvesub_ctdepth
	dim as short bs(rbd,1)
	dim as short lp(rbd)
	dim as short ch(rbd)
	dim as short cl(kl)
	dim as short op(rbd)
	dim as short bkey(kl)
	dim as short ukey(kl)
	dim as short tkey(kl)
	dim as short cip2(l)
	dim as uinteger state=1+(rnd*1073741824)*2 '((int(rnd*1073741824)+1)*2)-1
	dim as integer c
	dim as short cdx=kl
	dim as short cdy=l\kl
	if cdy*kl<l then cdy+=1
	dim as short grid(cdx,cdy)
	dim as short grid2(cdx,cdy)
	
	dim as short pcmg(cdx,cdy)
	dim as short pcmg2(cdx,cdy)
	dim as short pcm(l)
	
	for i=1 to rbd
		lp(i)=1
		ch(i)=1
	next i
	for i=1 to kl
		bkey(i)=thread(t).gkey(i,0)
	next i
	i=0
	for y=1 to cdy
		for x=1 to cdx
			i+=1
			grid(x,y)=thread(t).cip(i)
			if solvesub_pcmode=1 then pcmg(x,y)=i
			if i=l then exit for,for
		next x
	next y
	do
		for i=1 to rbd
			if ch(i)=1 then
				'ms_state=(214013*ms_state+2531011)mod 2147483648
				'r1=int(((ms_state shr 16)/32768)*100)+1
				state=48271*state and 2147483647
				r1=1+100*state shr 31
				if i=rbd andalso r1<=10 then '10% chance
					op(i)=2 'offset key
					'ms_state=(214013*ms_state+2531011)mod 2147483648
					'r1=int(((ms_state shr 16)/32768)*(kl-1))+1
					state=48271*state and 2147483647
					r1=1+(kl-1)*state shr 31
					bs(i,0)=r1
				else '90% chance
					op(i)=1 'swap
					'ms_state=(214013*ms_state+2531011)mod 2147483648
					'r1=int(((ms_state shr 16)/32768)*kl)+1
					state=48271*state and 2147483647
					r1=1+kl*state shr 31
					bs(i,0)=r1
					'ms_state=(214013*ms_state+2531011)mod 2147483648
					'r2=int(((ms_state shr 16)/32768)*kl)+1
					state=48271*state and 2147483647
					r2=1+kl*state shr 31
					bs(i,1)=r2
				end if
			end if
		next i
		for i=1 to kl
			ukey(i)=bkey(i)
		next i
		for i=1 to rbd
			if op(i)=1 then 'swap
				swap ukey(bs(i,0)),ukey(bs(i,1))
			else 'offset
				for j=1 to kl
					tkey(j)=ukey(j)
				next j
				k=bs(i,0)
				for j=1 to kl
					k+=1
					if k>kl then k=1
					ukey(k)=tkey(j)
				next j
			end if
		next i
		'j=0
		
		for i=1 to kl 'apply key to cipher
			for y=1 to cdy
				grid2(i,y)=grid(ukey(i),y)
				if solvesub_pcmode=1 then pcmg2(i,y)=pcmg(ukey(i),y)
			next y
		next i
		i=0
		for y=1 to cdy
			for x=1 to cdx
				if grid2(x,y)>0 then 
					i+=1
					cip2(i)=grid2(x,y)
					if solvesub_pcmode=1 then pcm(i)=pcmg2(x,y)
				end if
				if i=l then exit for,for
			next x
		next y
		
		c+=1
		for i=1 to kl
			extkey(t,c,i,0)=ukey(i)
		next i
		for i=1 to l
			extcip(t,c,i)=cip2(i)
			if solvesub_pcmode=1 then extpcm(t,c,i)=pcm(i)
		next i
		lpi=rbd
		do
			if lp(lpi)<rbw then
				lp(lpi)+=1
				ch(lpi)=1
				exit do
			else
				if lpi=1 then exit do,do
				lp(lpi)=1
				ch(lpi)=1
			end if
			lpi-=1
		loop
	loop
	extcip(t,1,0)=c
	
end sub

sub ext_bhd_beam_columnartransposition(byval t as short,byval l as short,byval s as short,byval kl as short)
	
	dim as short c,i,j,x,y,bg,bbg,a,b,lpi,r1,r2
	dim as short rbw=solvesub_ctdepth
	dim as short rbd=solvesub_ctdepth
	dim as short bs(rbd,1)
	dim as short lp(rbd)
	dim as short ch(rbd)
	dim as short cl(kl)
	dim as short bkey(kl)
	dim as short ukey(kl)
	dim as short cip(l)
	dim as short cip2(l)
	dim as uinteger state=1+(rnd*1073741824)*2 '((int(rnd*1073741824)+1)*2)-1
	dim as short cdx=kl
	dim as short cdy=l\kl
	if cdy*kl<l then cdy+=1
	dim as short grid(cdx,cdy)
	dim as integer id(s,s)
	
	dim as short pcmg(cdx,cdy)
	dim as short pcm(l),pcm2(l)
	
	for i=1 to rbd
		lp(i)=1
		ch(i)=1
	next i
	for i=1 to kl
		bkey(i)=thread(t).gkey(i,0)
		if l mod kl>0 andalso i>l mod kl then cl(i)=cdy-1 else cl(i)=cdy
	next i
	for i=1 to l
		cip(i)=thread(t).cip(i)
		if solvesub_pcmode=1 then pcm(i)=i
	next i
	
	bbg=0
	do
		for i=1 to rbd
			if ch(i)=1 then
				'ms_state=(214013*ms_state+2531011)and 2147483647
				'r1=int(((ms_state shr 16)/32768)*kl)+1
				state=48271*state and 2147483647
				r1=1+kl*state shr 31
				bs(i,0)=r1
				'ms_state=(214013*ms_state+2531011)and 2147483647
				'r2=int(((ms_state shr 16)/32768)*kl)+1
				state=48271*state and 2147483647
				r2=1+kl*state shr 31
				bs(i,1)=r2
			end if
		next i
		for i=1 to kl
			ukey(i)=bkey(i)
		next i
		for i=1 to rbd
			swap ukey(bs(i,0)),ukey(bs(i,1))
		next i
		
		j=0
		for i=1 to kl 'apply key to cipher
			for y=1 to cl(ukey(i))
				j+=1
				if solvesub_pcmode=1 then pcmg(ukey(i),y)=pcm(j)
				grid(ukey(i),y)=cip(j)
			next y
		next i
		i=0
		for y=1 to cdy
			for x=1 to cdx
				i+=1
				cip2(i)=grid(x,y)
				if solvesub_pcmode=1 then pcm2(i)=pcmg(x,y)
				if i=l then exit for,for
			next x
		next y
		
		'cb+=1
		'bg=0
		'for i=1 to l-1
		'	a=cip2(i)
		'	b=cip2(i+1)
		'	if id(a,b)<cb then
		'		id(a,b)=cb
		'	else 
		'		bg+=1
		'	end if
		'next i
		'if bg>bbg then
		'	bbg=bg
		'	for i=1 to kl
		'		thread(t).gkey(i,0)=ukey(i)
		'	next i
		'	for i=1 to l
		'		thread(t).cip(i)=cip2(i)
		'	next i
		'end if
		'for i=1 to rbd
		'	ch(i)=0
		'next i
		
		c+=1
		for i=1 to kl
			extkey(t,c,i,0)=ukey(i)
		next i
		for i=1 to l
			extcip(t,c,i)=cip2(i)
			if solvesub_pcmode=1 then extpcm(t,c,i)=pcm2(i)
		next i
		
		lpi=rbd
		do
			if lp(lpi)<rbw then
				lp(lpi)+=1
				ch(lpi)=1
				exit do
			else
				if lpi=1 then exit do,do
				lp(lpi)=1
				ch(lpi)=1
			end if
			lpi-=1
		loop
	loop
	extcip(t,1,0)=c
	
	'thread(t).itemname="Key: Columns("+str(solvesub_ctcolumns)+") "
	'for i=1 to kl
	'	thread(t).itemname+=str(thread(t).gkey(i,0))
	'	if i<>kl then thread(t).itemname+=", "
	'next i

end sub

sub ext_bhd_beam_nullsandskips(byval t as short,byval l as short,byval s as short,byval kl as short)
	
	dim as short i,j,k,x,y,e,p,a,b,lpi,r0,r1,r2,r3,r4,nll,skp
	dim as short ce,shlen
	dim as short rbw=solvesub_pndepth 'beam width
	dim as short rbd=solvesub_pndepth 'beam depth
	dim as short pp=solvesub_pnperiod
	dim as short bias=solvesub_pnbias
	dim as double shift=solvesub_nshcshift
	dim as short bs(rbd,2)
	dim as short lp(rbd)
	dim as short ch(rbd)
	dim as short bkey(kl,1)
	dim as short ukey(kl,1)
	dim as short cip(l+kl)
	dim as short cip2(l+kl)
	dim as short cip3(l+kl)
	dim as short cdx=pp
	dim as short cdy=l\pp
	if cdy*pp<l then cdy+=1
	dim as uinteger state=1+int(rnd*1073741824)*2
	dim as integer c
	dim as short id(constcip)
	
	for i=1 to rbd
		lp(i)=1
		ch(i)=1
	next i
	for i=1 to kl
		for j=0 to 1
			bkey(i,j)=thread(t).gkey(i,j)
		next j
	next i
	for i=1 to l
		cip(i)=thread(t).cip(i)
	next i
	
	do
		
		for i=1 to rbd 'make changes to beam
			if ch(i)=1 then
				
				do
					e=0
					state=48271*state and 2147483647
					r0=1+kl*state shr 31 'random key position
					for j=1 to i-1
						if bs(j,0)=r0 then
							e=1
							exit for
						end if
					next j
				loop until e=0
				state=48271*state and 2147483647
				r3=100*state shr 31 'normal or shift
				
				if r3<shift then 'shift
					
					ce=0
					r1=bkey(r0,1) 'null/skip
					r2=bkey(r0,0) 'position
					shlen=solvesub_pnperiod/solvesub_nshcshiftdiv
					
					do
						e=1
						do
							state=48271*state and 2147483647
							r3=1+(shlen*2)*state shr 31
							r3-=shlen
						loop until r3<>0
						if r1=0 then 'null
							if r2+r3>0 andalso r2+r3<l+1 then
								e=0
								for j=1 to kl
									if r2+r3=bkey(j,0) then
										e=1
										exit for
									end if
								next j
							end if
						else 'skip
							if r2+r3>0 andalso r2+r3<l+2 then
								e=0
								for j=1 to kl
									if r2+r3=bkey(j,0) andalso bkey(j,1)=0 then
										e=1
										exit for
									end if
								next j
							end if
						end if
						if e=1 then
							ce+=1
							if ce>shlen*shlen then 'expand shift area
								ce=0
								shlen+=1
							end if
						end if
					loop until e=0
					r2+=r3
					
				else 'normal
					
					if bkey(r0,1)=0 then 'null
						r1=0
						do
							e=0
							state=48271*state and 2147483647
							r2=1+l*state shr 31
							for j=1 to kl
								if j<>r0 andalso r2=bkey(j,0) then
									e=1
									exit for
								end if
							next j
						loop until e=0
					else 'skip
						r1=1
						do
							e=0
							state=48271*state and 2147483647
							r2=1+(l+1)*state shr 31
							for j=1 to kl
								if j<>r0 andalso r2=bkey(j,0) andalso bkey(j,1)=0 then
									e=1
									exit for
								end if
							next j
						loop until e=0
					end if
					
				end if
				
				bs(i,0)=r0 'key position
				bs(i,1)=r1 'null/skip
				bs(i,2)=r2 'cipher position
			end if
		next i
		
		for i=1 to kl 'change key according to beam
			for j=0 to 1
				ukey(i,j)=bkey(i,j)
			next j
		next i
		for i=1 to rbd
			ukey(bs(i,0),0)=bs(i,2)
			ukey(bs(i,0),1)=bs(i,1)
		next i
		
		p=0
		r1=0 'apply key to cipher
		for i=1 to l+1
			r0=0
			nll=0
			skp=0
			for j=1 to kl
				if ukey(j,0)=i then
					if ukey(j,1)=0 then 
						nll=1 
					else 
						skp=1
						r0+=1
					end if
				end if
			next j
			if skp=1 then
				for j=1 to r0
					p+=1
					r1+=1
					cip2(p)=s+r1
				next j
			end if
			if nll=0 then
				if cip(i)>0 then
					p+=1
					cip2(p)=cip(i)
				end if
			end if
		next i
		
		j=0 'nba cipher
		erase id
		for i=1 to p
			if id(cip2(i))=0 then
				j+=1
				cip3(i)=j
				id(cip2(i))=j
			else
				cip3(i)=id(cip2(i))
			end if
		next i
		
		c+=1 'externalize to ext arrays
		extinf(t,c,0)=p
		extinf(t,c,1)=j
		for i=1 to kl
			for j=0 to 1
				extkey(t,c,i,j)=ukey(i,j)
			next j
		next i
		k=0
		for i=1 to pp
			for j=i to p step pp
				k+=1
				extcip(t,c,k)=cip3(j) 'utp
				if solvesub_pcmode=1 then extpcm(t,c,k)=j
			next j
		next i
		
		lpi=rbd
		do
			if lp(lpi)<rbw then
				lp(lpi)+=1
				ch(lpi)=1
				exit do
			else
				if lpi=1 then exit do,do
				lp(lpi)=1
				ch(lpi)=1
			end if
			lpi-=1
		loop
		
	loop
	extcip(t,1,0)=c
	
end sub

sub get_cyclepatterns(array()as long,byval l as integer,byval s as integer,byval cs as integer,byval fl as integer,byval slot as integer)
	
	'dim as short ctmax=cta(0)
	dim as short l1,l2,l3,l4,l5,l6,l7
	dim as short t(cs),c(cs),p(cs) ',cf(cs),cscf(cs,cs)
	dim as short i,j,k,d,g,fm,cl 'h,i,j,k,d,g,al,cl,fm,bl,a1,a2,a,b,e,o,al1,al2,p1,p2,ct
	dim as short frq(s)
	'dim as double score,lowscore
	dim as integer n,n1,n2,n3,n4,n5,n6,n7,n8,n9
	'if cta(1)=1 then
		n1=cs
		n2=cs^2
		n3=cs^3
		n4=cs^4
		n5=cs^5
		n6=cs^6
		n7=cs^7
		n8=cs^8
		n9=cs^9
	'end if
	'for i=1 to ctmax
	'	cto(cs,i,0,0)=0
	'next i
   for i=1 to l
  		frq(array(i))+=1
   next i
   for i=1 to s
   	if frq(i)>fm then fm=frq(i)
   next i
	dim as short map(l,fm+1)
	for i=1 to l
		map(array(i),0)+=1
		map(array(i),map(array(i),0))=i
	next i
	for i=1 to s
		map(i,map(i,0)+1)=l+1
	next i
	dim as short z(fm*cs),z2(fm*cs)
	select case cs
		case 2
			for l1=1 to s
				for l2=l1+1 to s
					'if frq(l1)>1 andalso frq(l2)>1 then
						'al=0
						cl=0
						for i=1 to cs
							p(i)=1
							t(i)=i-1
						next i
						do
							c(1)=map(l1,p(1))
							c(2)=map(l2,p(2))
							d=l+1
							for i=1 to cs
								if c(i)<d then
									d=c(i)
									g=i
								end if
							next i
							if d=l+1 then exit do
							p(g)+=1
							cl+=1
							z(cl)=t(g)
						loop
						
						select case fl
							case 2
								for i=1 to cl-1
									n=z(i)+(z(i+1)*n1)
									nscf(slot,n)+=1
								next i
							case 3
								for i=1 to cl-2
									n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)
									nscf(slot,n)+=1
								next i
							case 4
								for i=1 to cl-3
									n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)
									nscf(slot,n)+=1
								next i
							case 5
								for i=1 to cl-4
									n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)
									nscf(slot,n)+=1
								next i
							case 6
								for i=1 to cl-5
									n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)
									nscf(slot,n)+=1
								next i
							case 7
								for i=1 to cl-6
									n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)
									nscf(slot,n)+=1
								next i
							case 8
								for i=1 to cl-7
									n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)+(z(i+7)*n7)
									nscf(slot,n)+=1
								next i
							case 9
								for i=1 to cl-8
									n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)+(z(i+7)*n7)+(z(i+8)*n8)
									nscf(slot,n)+=1
								next i
							case 10
								for i=1 to cl-9
									n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)+(z(i+7)*n7)+(z(i+8)*n8)+(z(i+9)*n9)
									nscf(slot,n)+=1
								next i
						end select
						
						'#include "m_cycletypes.bi"
					'end if
				next l2
			next l1	
		case 3 '3-symbol cycles
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 then		
							'al=0
							cl=0
							for i=1 to cs
								p(i)=1
								t(i)=i-1
							next i
							do
								c(1)=map(l1,p(1))
								c(2)=map(l2,p(2))
								c(3)=map(l3,p(3))
								d=l+1
								for i=1 to cs
									if c(i)<d then
										d=c(i)
										g=i
									end if
								next i
								if d=l+1 then exit do
								p(g)+=1
								cl+=1
								z(cl)=t(g)
							loop
							
							select case fl
								case 2
									for i=1 to cl-1
										n=z(i)+(z(i+1)*n1)
										nscf(slot,n)+=1
									next i
								case 3
									for i=1 to cl-2
										n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)
										nscf(slot,n)+=1
									next i
								case 4
									for i=1 to cl-3
										n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)
										nscf(slot,n)+=1
									next i
								case 5
									for i=1 to cl-4
										n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)
										nscf(slot,n)+=1
									next i
								case 6
									for i=1 to cl-5
										n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)
										nscf(slot,n)+=1
									next i
								case 7
									for i=1 to cl-6
										n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)
										nscf(slot,n)+=1
									next i
								case 8
									for i=1 to cl-7
										n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)+(z(i+7)*n7)
										nscf(slot,n)+=1
									next i
								case 9
									for i=1 to cl-8
										n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)+(z(i+7)*n7)+(z(i+8)*n8)
										nscf(slot,n)+=1
									next i
								case 10
									for i=1 to cl-9
										n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)+(z(i+7)*n7)+(z(i+8)*n8)+(z(i+9)*n9)
										nscf(slot,n)+=1
									next i
							end select
							
							'#include "m_cycletypes.bi"
						'end if
					next l3
				next l2
			next l1
		case 4 '4-symbol cycles	
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 then
								'al=0
								cl=0
								for i=1 to cs
									p(i)=1
									t(i)=i-1
								next i
								do
									c(1)=map(l1,p(1))
									c(2)=map(l2,p(2))
									c(3)=map(l3,p(3))
									c(4)=map(l4,p(4))
									d=l+1
									for i=1 to cs
										if c(i)<d then
											d=c(i)
											g=i
										end if
									next i
									if d=l+1 then exit do
									p(g)+=1
									cl+=1
									z(cl)=t(g)	
								loop
								
								select case fl
									case 2
										for i=1 to cl-1
											n=z(i)+(z(i+1)*n1)
											nscf(slot,n)+=1
										next i
									case 3
										for i=1 to cl-2
											n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)
											nscf(slot,n)+=1
										next i
									case 4
										for i=1 to cl-3
											n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)
											nscf(slot,n)+=1
										next i
									case 5
										for i=1 to cl-4
											n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)
											nscf(slot,n)+=1
										next i
									case 6
										for i=1 to cl-5
											n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)
											nscf(slot,n)+=1
										next i
									case 7
										for i=1 to cl-6
											n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)
											nscf(slot,n)+=1
										next i
									case 8
										for i=1 to cl-7
											n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)+(z(i+7)*n7)
											nscf(slot,n)+=1
										next i
									case 9
										for i=1 to cl-8
											n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)+(z(i+7)*n7)+(z(i+8)*n8)
											nscf(slot,n)+=1
										next i
									case 10
										for i=1 to cl-9
											n=z(i)+(z(i+1)*n1)+(z(i+2)*n2)+(z(i+3)*n3)+(z(i+4)*n4)+(z(i+5)*n5)+(z(i+6)*n6)+(z(i+7)*n7)+(z(i+8)*n8)+(z(i+9)*n9)
											nscf(slot,n)+=1
										next i
								end select
								
								'#include "m_cycletypes.bi"
							'end if
						next l4
					next l3
				next l2
			next l1
		case 5 '5-symbol cycles	
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							for l5=l4+1 to s
								'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 then
									'al=0
									cl=0
									for i=1 to cs
										p(i)=1
										t(i)=i '-1
									next i
									do
										c(1)=map(l1,p(1))
										c(2)=map(l2,p(2))
										c(3)=map(l3,p(3))
										c(4)=map(l4,p(4))
										c(5)=map(l5,p(5))
										d=l+1
										for i=1 to cs
											if c(i)<d then
												d=c(i)
												g=i
											end if
										next i
										if d=l+1 then exit do
										p(g)+=1
										cl+=1
										z(cl)=t(g)	
									loop
									'#include "m_cycletypes.bi"
								'end if
							next l5
						next l4
					next l3
				next l2
			next l1
		case 6 '6-symbol cycles	
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							for l5=l4+1 to s
								for l6=l5+1 to s
									'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 then
										'al=0
										cl=0
										for i=1 to cs
											p(i)=1
											t(i)=i '-1
										next i
										do
											c(1)=map(l1,p(1))
											c(2)=map(l2,p(2))
											c(3)=map(l3,p(3))
											c(4)=map(l4,p(4))
											c(5)=map(l5,p(5))
											c(6)=map(l6,p(6))
											d=l+1
											for i=1 to cs
												if c(i)<d then
													d=c(i)
													g=i
												end if
											next i
											if d=l+1 then exit do
											p(g)+=1
											cl+=1
											z(cl)=t(g)	
										loop
										'#include "m_cycletypes.bi"
									'end if
								next l6
							next l5
						next l4
					next l3
				next l2
			next l1
		case 7 '7-symbol cycles	
			for l1=1 to s
				for l2=l1+1 to s
					for l3=l2+1 to s
						for l4=l3+1 to s
							for l5=l4+1 to s
								for l6=l5+1 to s
									for l7=l6+1 to s
										'if frq(l1)>1 andalso frq(l2)>1 andalso frq(l3)>1 andalso frq(l4)>1 andalso frq(l5)>1 then
											'al=0
											cl=0
											for i=1 to cs
												p(i)=1
												t(i)=i '-1
											next i
											do
												c(1)=map(l1,p(1))
												c(2)=map(l2,p(2))
												c(3)=map(l3,p(3))
												c(4)=map(l4,p(4))
												c(5)=map(l5,p(5))
												c(6)=map(l6,p(6))
												c(7)=map(l7,p(7))
												d=l+1
												for i=1 to cs
													if c(i)<d then
														d=c(i)
														g=i
													end if
												next i
												if d=l+1 then exit do
												p(g)+=1
												cl+=1
												z(cl)=t(g)	
											loop
											'#include "m_cycletypes.bi"
										'end if
									next l7
								next l6
							next l5
						next l4
					next l3
				next l2
			next l1
	end select
	'return score

end sub

sub thread_batch_ciphers_mergeseqhom(byval none as any ptr)
	
	dim as integer i,j,k,l,s,e
	dim as integer metainfo
	dim as integer cipherend
	dim as integer numeric
	dim as integer cipherline
	dim as integer contain_numbers
	dim as integer contain_spaces
	dim as integer contain_symbols
	dim as string num
	dim as string char
	dim as string ln
	dim as string outputdir=basedir+"\Output\"
	dim as string itemname
	dim as integer batchinfo(constcip)
	dim as integer batchnba(constcip)
	dim as integer ident(65536)
	dim as integer count
	dim as integer batch_x
	dim as integer batch_y
	dim as integer items
	dim as double iterations=solvesub_iterations
	dim as double batchtimer
	dim as double batchtime
	dim as string outeditbox
	
	task_active="batch ciphers (merge seq. homophones)"
	update_solver_status
	
	dim as string filename=ui_loadsavedialog(0,"Open cipher batch file (merge sequential homophones)",filter,1,basedir+"\Ciphers\")
	
	if fileexists(filename)=0 then
		if filename<>"" then ui_editbox_settext(output_text,"Error: file not found")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	ui_editbox_settext(output_text,"Please wait, processing batch file...")
	
	'ui_listbox_setcursel(list_main,16) '9
	set_solverhighlight("merge sequential homophones")
	toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",4,1,threads) 'stop solver
	toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",2,1,threads) 'stop thread
	toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",1,1,threads) 'start thread
	
	dim as integer old_nas=ngram_alphabet_size
	ngram_alphabet_size=solvesub_cyclealphabetsize
	
	stoptask=0	
	global_best_score=0
	shared_string=""
	task_active="batch ciphers (merge seq. homophones)"
	update_solver_status
	
	batchtimer=timer
	ips_timer=timer
	sectimer=timer
	dim as double statustimer=timer
	
	open filename for binary as #1
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then
		batchnr+=1
		open basedir+"\Output\batch_"+str(batchnr)+".txt" for output as #3
	end if
	
	do
		
		line input #1,ln
		metainfo=0
		
		if instr(ln,"output_sub_directory=")>0 or instr(ln,"results_sub_directory=")>0 then
			metainfo=1
			if solvesub_outputdir=1 then
				outputdir=basedir+"\Output\"+right(ln,len(ln)-instr(ln,"="))+"\"
				mkdir outputdir
			end if
		end if
		
		if instr(ln,"cipher_information=")>0 then
			metainfo=1
			itemname=right(ln,len(ln)-instr(ln,"="))
		end if
		
		if instr(ln,"iterations_factor=")>0 then
			metainfo=1
			iterations*=val(right(ln,len(ln)-instr(ln,"=")))
			if iterations<100000 then iterations=100000
		end if
		
		if instr(ln,"iterations=")>0 then
			metainfo=1
			iterations=val(right(ln,len(ln)-instr(ln,"=")))
			if iterations<100000 then iterations=100000
		end if
		
		if ln="" or eof(1) then
			cipherend=1
		end if
		
		if metainfo=0 then
			cipherline+=1
			contain_spaces=0
			contain_symbols=0
			contain_numbers=0	
			if cipherline=1 then
				count=0
				for i=1 to len(ln) 'check if numeric
					select case asc(ln,i)
						case 32 'space
							contain_spaces=1
						case 48 to 57 'numbers
							contain_numbers=1
						case else 'symbols
							contain_symbols=1
					end select
				next i
				if contain_spaces=1 andalso contain_numbers=1 andalso contain_symbols=0 then
					numeric=1
				else
					numeric=0
				end if
			end if
			if numeric=1 then
				for i=1 to len(ln)
					select case asc(ln,i)
						case 32 'space
							if num<>"" then
								count+=1
								if count<=constcip then
									batchinfo(count)=val(num)
								end if
								num=""
							end if
						case 48 to 57 'numbers
							num+=chr(asc(ln,i))
						case else 'symbols
							'error
					end select
				next i
				if num<>"" then
					count+=1
					if count<=constcip then
						batchinfo(count)=val(num)
					end if
					num=""
				end if
			else
				for i=1 to len(ln)
					select case asc(ln,i)
						case 32 'space
							'error
						case else 'symbols
							count+=1
							if count<=constcip then
								batchinfo(count)=asc(ln,i)
							end if
					end select	
				next i
			end if
		end if
		if cipherline=1 then batch_x=count
		if cipherend=1 then
			if count>ngram_size then
				batch_y=cipherline
				l=count
				if l>constcip then l=constcip
				s=0
				erase ident
				for i=1 to l
					if ident(batchinfo(i))=0 then
						s+=1
						ident(batchinfo(i))=s
						'batchnba(i)=s
					'else
						'batchnba(i)=ident(batchinfo(i))
					end if
				next i
				do
					sleep twait
					for i=1 to threads
						
						if pausetask=1 then 'pause task
							update_solver_status
							do
								sleep 10
							loop until pausetask=0
							update_solver_status
						end if
						
						if timer-statustimer>1.1 then
							statustimer=timer
							update_solver_status
						end if
						
						if thread(i).solver_waiting=1 then
							solver_status_processing=1
							items+=1
							thread(i).outputdir=outputdir
							thread(i).l=l
							thread(i).s=s
							thread(i).num=numeric
							thread(i).dim_x=batch_x
							thread(i).dim_y=batch_y
							thread(i).score=0
							thread(i).pcmode=0
							thread(i).itemnumber=items
							thread(i).itemname=itemname
							thread(i).advstats=solvesub_advstats
							thread(i).cyclealphabetsize=solvesub_cyclealphabetsize
							thread(i).cyclelengthweight=solvesub_cyclelengthweight
							thread(i).cyclesizeweight=solvesub_cyclesizeweight
							thread(i).cyclequalityweight=solvesub_cyclequalityweight
							thread(i).iterations=iterations
							thread(i).temperature=solvesub_temperature
							thread(i).restarts=solvesub_restarts
							thread(i).subrestartlevels=solvesub_subrestartlevels
							thread(i).ngramfactor=solvesub_ngramfactor
							thread(i).multiplicityweight=solvesub_multiplicityweight
							thread(i).entweight=solvesub_entweight
							thread(i).solver_stop=0
							for j=1 to l
								thread(i).cip(j)=batchinfo(j)
							next j
							thread(i).update=0
							thread(i).solver_waiting=0 'engage thread			
							exit do
						end if
					next i
				loop until stoptask=1
			end if
			cipherline=0
			cipherend=0
		end if
		
	loop until eof(1) or stoptask=1
	
	close #1
 
 	dim as double stucktimer=timer
	for i=1 to threads
		if stoptask=1 andalso thread(i).solver_waiting=0 then
			items-=1
			thread(i).solver_stop=1
		end if
		do
			sleep 10
		loop until thread(i).solver_waiting=1 'or timer-stucktimer>2
		thread(i).itemnumber=0
		thread(i).restarts_completed=0
		thread(i).iterations_completed=0
		thread(i).num=0
	next i
	
	if solvesub_outputbatch=1 andalso solvesub_outputdir=1 then close #3
	
	batchtime=timer-batchtimer
	
	update_solver_status
	
	dim as string ot
	ot+="BHdecrypt batch ciphers (merge seq. homophones) for: "+right(filename,len(filename)-instrrev(filename,"\"))+lb
	ot+="---------------------------------------------------------"+lb
	ot+="Items processed: "+str(items)+lb
	ot+="Average score: "+format(solvesub_avgscore/items,"0.00000")+lb
	ot+="Average IOC: "+format(solvesub_avgioc/items,"0.00000")+lb
	ot+="Average PC-cycles: "+format(solvesub_avgpccycles/items,"0.00")+lb
	ot+="Processing time: "+format(batchtime,"0.00")+" seconds"+lb
	ot+="- Per cipher: "+format(batchtime/items,"0.00000")+ " seconds"
	ui_editbox_settext(output_text,ot)
	
	ngram_alphabet_size=old_nas
	solver_status_processing=0
	clean_thread_information
	task_active="none"
	update_solver_status
	
	if stoptask=0 andalso solvesub_batchshutdown=1 then shell ("SHUTDOWN /s /t 10 ")
	stoptask=0

end sub

sub get_combinations
	
	dim as integer i,j,k
	dim as integer arg1
	dim as integer arg2
	dim as integer arg4
	dim as integer arg5
	dim as integer arg6
	dim as integer step1
	dim as integer len1
	dim as integer combs(combine_stacksize)
	dim as integer combs_i=1
	dim as integer total_combs
	dim as integer max_list_length
	dim as integer group_size
	dim as integer max_group_size=1
	dim as integer group_entries1
	dim as integer group_entries2
	dim as integer max_group_entries
	dim as integer currlen=info_length
	dim as integer tmp_x=info_x
	dim as integer tmp_y=info_y
	for i=1 to combine_stacksize
		'select case combine_stack(i).operation
			'case "Dimension","Directions","Period","Skytale","Offset","Offset row order","Offset column order",_
				'"Expand symbol","Add character","Remove character","Randomize","Add column","Add row",_
				'"Remove column","Remove row","Add random character","Add characters"
				group_entries1=0
				arg1=val(combine_stack(i).arg(1))
				arg2=val(combine_stack(i).arg(2))
				step1=val(combine_stack(i).arg(3))
				arg4=val(combine_stack(i).arg(4))
				arg5=val(combine_stack(i).arg(5))
				arg6=val(combine_stack(i).arg(6))
				if step1=0 then step1=1
				select case combine_stack(i).operation
					case "Dimension"
						len1=arg2
					case "Noop"
						len1=1
					case "Directions"
						len1=16
					case "Period","Skytale","Offset"
						len1=currlen
					case "Offset row order","Period row order","Randomize row" '<-- set to currlen? (dimension)
						len1=tmp_y
					case "Offset column order","Period column order","Randomize column" '<-- set to currlen? (dimension)
						len1=tmp_x
					case "Expand symbol"
						len1=info_symbols
					case "Random nulls"
						currlen-=arg4
						len1=arg2
					case "Random skips"
						currlen+=arg4
						len1=arg2
					case "Add character"
						currlen+=1
						len1=currlen
					case "Add characters"
						currlen+=arg4
						len1=currlen
					case "Add nulls and skips"
						currlen+=arg4-arg5
						len1=arg2
					case "Add random character"
						currlen+=1
						len1=arg2
					case "Add column"
						tmp_x+=1
						len1=tmp_x
						currlen+=tmp_y
					case "Add column (using random symbols)"
						tmp_x+=1
						len1=arg2
						currlen+=tmp_y
					case "Add row"
						tmp_y+=1
						len1=tmp_y
						currlen+=tmp_x
					case "Add row (using random symbols)"
						tmp_y+=1
						len1=arg2
						currlen+=tmp_x
					case "Remove character"
						len1=currlen
						currlen-=1
					case "Remove characters"
						currlen-=arg4
						len1=(currlen+1)
					case "Remove column"
						len1=tmp_x
						currlen-=tmp_x 'could be less if irregular <--- problem
						tmp_x-=1
					case "Remove row"
						len1=tmp_y
						currlen-=tmp_y 'could be less if irregular <--- problem
						tmp_y-=1
					case "Randomize","Randomize column order","Randomize row order","Randomize and bigrams"
						len1=arg2
					case "Plaintext"
						len1=arg2
					case "Encode: homophonic substitution","Encode: caesar shift","Encode: homophonic substitution 1-170",_
						"Encode: homophonic substitution 171-340","Encode: homophonic substitution"
						len1=arg2
				end select
				for j=arg1 to arg2 step step1
					if j<=len1 then
						if combine_stack(i).operation="Skytale" then
							if gcd(len1,j)=1 then group_entries1+=1
						else
							group_entries1+=1
						end if
					end if
					if group_entries1=0 then group_entries1=1 'test
				next j
		'end select
		if combine_stack(i).multiplicative=1 then
			max_list_length+=1
			group_size=1
			group_entries2=group_entries1
			if i>1 then combs_i+=1
		else
			if i=1 then max_list_length+=1
			group_size+=1
			if group_size>max_group_size then max_group_size=group_size
			group_entries2+=group_entries1
		end if
		combs(combs_i)=group_entries2
		if group_entries2>max_group_entries then max_group_entries=group_entries2
	next i
	total_combs=1
	for i=1 to max_list_length
		total_combs*=combs(i)
	next i	
	if combine_stacksize=0 then total_combs=0
	
	combine_combinations=total_combs
	combine_maxlistlength=max_list_length
	combine_maxgroupentries=max_group_entries
	combine_maxgroupsize=max_group_size

end sub

sub thread_combine(byval none as any ptr)
	
	dim as string soi=string_to_info(ui_editbox_gettext(input_text))
	if soi<>"Ok" then
		ui_editbox_settext(output_text,soi)
		exit sub
	end if
	
	'arg(1)=info_length
	'arg(2)=info_symbols
	'arg(3)=info_x
	'arg(4)=info_y
	'arg(5)=0
	'arg(6)=ui_checkbox_getcheck(checkbox_transposition_keepnulls)
	'arg(7)=val(ui_editbox_gettext(editbox_transposition_f1))
	'arg(8)=val(ui_editbox_gettext(editbox_transposition_f2))
	'arg(9)=val(ui_editbox_gettext(editbox_transposition_f3))
	'arg(10)=val(ui_editbox_gettext(editbox_transposition_f4))
	
	dim as integer listlength=combine_listlength
	dim as string measurement=combine_measurement
	dim as integer normalized=combine_normalized
	dim as integer omitlist=combine_omitlist
	dim as integer hypergraph=combine_hypergraph
	dim as integer forcelinear=combine_forcelinear
	dim as double getsigma=combine_getsigma
	dim as double ma1=combine_ma1
	dim as integer h,i,j,k,e,x,y,t
	dim as string n
	dim as integer arg1
	dim as integer arg2
	dim as integer arg4
	dim as integer arg5
	dim as integer arg6
	dim as integer step1
	dim as integer len1
	dim as integer useioc
	dim as integer uselen
	
	if combine_minioc>0 or combine_maxioc>0 then useioc=1
	if combine_fromlen>0 or combine_tolen>0 then 
		uselen=1
		if combine_fromlen<1 then combine_fromlen=1
		if combine_tolen<1 then combine_tolen=1
	end if
	
	get_combinations
	
	'n+=str(combine_maxlistlength)+lb
	'n+=str(combine_maxgroupentries)+lb
	'n+=str(combine_maxgroupsize)+lb
	'n+=str(combine_combinations)
	'ui_editbox_settext(output_text,n)
	'exit sub
	
	dim as integer local_maxlistlength=combine_maxlistlength
	dim as integer local_maxgroupentries=combine_maxgroupentries
	dim as integer local_maxgroupsize=combine_maxgroupsize
	dim as integer local_combinations=combine_combinations
	
	dim as unsigned integer looplist(local_maxlistlength,local_maxgroupentries) 'numbers
	dim as combo2 looptable(local_maxlistlength,local_maxgroupsize) 'information
	dim as integer curr_loop=1
	dim as integer curr_group
	dim as integer curr_entry
	dim as integer currlen=info_length
	
	dim as integer tmp_x=info_x
	dim as integer tmp_y=info_y
	dim as integer plaintext
	
	for i=1 to listlength
		if combine_stack(i).multiplicative=0 then 'additive
			curr_group+=1
		else
			if i>1 then 
				curr_loop+=1
				curr_entry=0
			end if
			curr_group=1
		end if	
		'select case combine_stack(i).operation
			'case "Dimension","Directions","Period","Skytale","Offset","Offset row order","Offset column order",_
				'"Expand symbol","Add character","Remove character","Randomize","Add column","Add row",_
				'"Remove column","Remove row","Add random character","Add characters"
				arg1=val(combine_stack(i).arg(1))
				arg2=val(combine_stack(i).arg(2))
				step1=val(combine_stack(i).arg(3))
				arg4=val(combine_stack(i).arg(4))
				arg5=val(combine_stack(i).arg(5))
				arg6=val(combine_stack(i).arg(6))
				if step1=0 then step1=1
				select case combine_stack(i).operation
					case "Dimension"
						len1=arg2
					case "Noop"
						len1=1
					case "Directions"
						len1=16
					case "Period","Skytale","Offset"
						len1=currlen
					case "Offset row order","Period row order","Randomize row" '<-- set to currlen? (dimension)
						len1=tmp_y
					case "Offset column order","Period column order","Randomize column" '<-- set to currlen? (dimension)
						len1=tmp_x
					case "Expand symbol"
						len1=info_symbols
					case "Random nulls"
						currlen-=arg4
						len1=arg2
					case "Random skips"
						currlen+=arg4
						len1=arg2
					case "Add character"
						currlen+=1
						len1=currlen
					case "Add characters"
						currlen+=arg4
						len1=currlen
					case "Add nulls and skips"
						currlen+=arg4-arg5
						len1=arg2
					case "Add random character"
						currlen+=1
						len1=arg2
					case "Add column"
						tmp_x+=1
						len1=tmp_x
						currlen+=tmp_y
					case "Add column (using random symbols)"
						tmp_x+=1
						len1=arg2
						currlen+=tmp_y
					case "Add row"
						tmp_y+=1
						len1=tmp_y
						currlen+=tmp_x
					case "Add row (using random symbols)"
						tmp_y+=1
						len1=arg2
						currlen+=tmp_x
					case "Remove character"
						len1=currlen
						currlen-=1
					case "Remove characters"
						currlen-=arg4
						len1=(currlen+1)
					case "Remove column"
						len1=tmp_x
						currlen-=tmp_x 'could be less <--- problem
						tmp_x-=1
					case "Remove row"
						len1=tmp_y
						currlen-=tmp_y 'could be less <--- problem
						tmp_y-=1
					case "Randomize","Randomize column order","Randomize row order","Randomize and bigrams"
						len1=arg2
					case "Plaintext"
						len1=arg2
						plaintext=1
					case "Encode: homophonic substitution","Encode: caesar shift","Encode: homophonic substitution 1-170",_
						"Encode: homophonic substitution 170-340","Encode: homophonic substitution 2"
						len1=arg2
				end select
				looptable(curr_loop,curr_group).s=combine_stack(i).operation
				looptable(curr_loop,curr_group).n=curr_entry+1
				looptable(curr_loop,curr_group).a4=arg4
				looptable(curr_loop,curr_group).a5=arg5
				looptable(curr_loop,curr_group).a6=arg6
				looptable(curr_loop,curr_group).u=combine_stack(i).untransposed
				for j=arg1 to arg2 step step1
					if j<=len1 then
						if combine_stack(i).operation="Skytale" then
							if gcd(len1,j)=1 then 
								curr_entry+=1
								looplist(curr_loop,curr_entry)=j
							end if
						else
							curr_entry+=1
							looplist(curr_loop,curr_entry)=j
						end if
					end if
					if curr_entry=0 then curr_entry=1 'test
				next j
				looptable(curr_loop,0).n=curr_entry
		'end select
	next i
	
	dim as integer local_l=info_length
	dim as integer local_s=info_symbols
	dim as integer local_x=info_x
	dim as integer local_y=info_y
	
	dim as double combinetimer=timer
	dim as double statustimer
	
	if measurement="Solve substitution" then
		if task_active<>"none" then stop_current_task
		sleep 100
		if task_active="none" then
			toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",4,1,threads) 'stop solver
			toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",2,1,threads) 'stop thread
			toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",1,1,threads) 'start thread
			sleep 100
		end if
		ips_timer=timer
		statustimer=timer
		solver_status_processing=1
		global_best_score=0
		'task_active="combine"
		'for i=1 to threads
		'	thread(i).score=0
		'	thread(i).ngrams=0
		'	thread(i).pccycles=0
		'	thread(i).itemname=""
		'	thread(i).itemnumber=0
		'	thread(i).combine_output=1
		'	thread(i).iterations_completed=0
		'	thread(i).restarts_completed=0
		'	thread(i).avgscore=0
		'	thread(i).avgioc=0
		'	thread(i).avgpccycles=0
		'next i
		'update_solver_status
	end if
	
	stoptask=0
	task_active="combine"
	update_solver_status	
	shared_string=""
	
	dim as string a,b,opreturn
	dim as double m
	dim as double avg
	dim as combo low
	dim as combo high
	dim as integer ok_count
	dim as double arg(100)
	dim as long part(constcip)
	low.m=999999999
	dim as long cip(info_length*2)
	dim as long cip2(info_length)
	
	for i=1 to info_length
		cip2(i)=info(i)
		cstate(21,i)=nuba(i)
		'select case measurement
		'	'case "Slope","Zodiac 340 By,BY,yB,YB":cstate(21,i)=info(i)
		'	case else:cstate(21,i)=nuba(i)
		'end select
	next i
	dim as integer ci=20
	
	dim as generic_loop gl(64)
	dim as integer curr=1
	dim as integer init_loops=1
	dim as integer depth=local_maxlistlength
	
	for i=1 to depth 
		gl(i).f=1 'from
		gl(i).t=looptable(i,0).n 'to
		gl(i).s=1 'step
		gl(i).c=1 'current
	next i
	
	if init_loops=1 then 'init loops
		for i=1 to depth
			select case gl(i).of
			case is=0 'nothing
				'...
			case is=1 'add
				gl(i).f=gl(gl(i).ofi).c+gl(i).ofm
				gl(i).c=gl(i).f
			case is=2 'sub
				'...
		end select	
		next i
		init_loops=0
	end if
	
	'ips_timer=timer
	dim as double timespent=timer
	dim as string taskstatus
	dim as string op1
	dim as string subop
	dim as integer utp1
	dim as integer count
	redim combine_item(local_combinations+1)
	redim combine_score(local_combinations+1)
	redim combine_dims(local_combinations+1,local_maxlistlength+1)
	
	erase graph
	
	ui_editbox_settext(output_text,"Please wait...")
	
	dim as integer l
	dim as integer s
	dim as integer dim_x
	dim as integer dim_y
	
	dim as string dirs(16)
	dirs(1)="Normal"
	dirs(2)="Mirror"
	dirs(3)="Flip"
	dirs(4)="Reverse"
	dirs(5)="Columnar 1"
	dirs(6)="Columnar 2"
	dirs(7)="Columnar 3"
	dirs(8)="Columnar 4"
	dirs(9)="Diagonal 1"
	dirs(10)="Diagonal 2"
	dirs(11)="Diagonal 3"
	dirs(12)="Diagonal 4"
	dirs(13)="Diagonal 5"
	dirs(14)="Diagonal 6"
	dirs(15)="Diagonal 7"
	dirs(16)="Diagonal 8"
	
	dim as short ptl(200,2000)
	dim as short id1(65536)
	dim as string li
	dim as integer cl,ptn
	e=0
	if plaintext=1 then
		for i=1 to 200
			'open basedir+"\Ciphers\Plaintext\Length 340 plaintexts\p"+str(i)+".txt" for binary as #1
			open basedir+"\Ciphers\Plaintext\Length 1000 plaintexts\p"+str(i)+".txt" for binary as #1
			do
			line input #1,li
			li=ucase(li) 'added for caesar shift
			for j=1 to len(li)
				if id1(asc(li,j))=0 then
					
					e+=1
					cl+=1
					ptl(i,cl)=asc(li,j)
					
					'nba
					'------------------------
					'e+=1
					'cl+=1
					'ptl(i,cl)=e
					'id1(asc(li,j))=e
					
				else
					
					cl+=1
					ptl(i,cl)=asc(li,j)
					
					'nba
					'------------------------
					'cl+=1
					'ptl(i,cl)=id1(asc(li,j))	
						
				end if
			next j
			loop until eof(1)
			ptl(i,0)=e
			close #1
			erase id1
			e=0
			cl=0
		next i
	end if
	
	do 'start generic loop
		
		'------------------------------------------------------------------------------
		
		a=""
		count+=1
		
		if timer-combinetimer>1 then
			combinetimer=timer
			taskstatus="Processing task: "+format((count/combine_combinations)*100,"0.00")+"% complete"+lb
			dim as double trs=((combine_combinations-count)*((timer-timespent)/count))
			dim as double days=trs/86400
			dim as double hours=frac(days)*24
			dim as double mins=frac(hours)*60
			dim as double secs=frac(mins)*60
			taskstatus+="---------------------------------------------------------"+lb
			taskstatus+="Approximate time remaining:"+lb
			taskstatus+="- Days: "+str(int(days))+lb
			taskstatus+="- Hours: "+str(int(hours))+lb
			taskstatus+="- Minutes: "+str(int(mins))
			ui_editbox_settext(output_text,taskstatus)
		end if
		
		for i=1 to depth
			
			combine_dims(count,depth-(i-1))=gl(i).c
			
			if i=1 then
				l=local_l
				s=local_s
				dim_x=local_x
				dim_y=local_y
			end if
			
			op1=""
			utp1=0
			for j=1 to local_maxgroupsize 'local_maxlistlength
				if looptable(i,j).n>0 andalso gl(i).c>=looptable(i,j).n then
					op1=looptable(i,j).s
					utp1=looptable(i,j).u
					arg4=looptable(i,j).a4
					arg5=looptable(i,j).a5
					arg6=looptable(i,j).a6
				end if		
			next j
			
			select case op1 'operation
			
				case "Noop"
					if i>1 then a+=", "
					a+="Noop"
					a+="("+str(looplist(i,(gl(i).c)))+")"
					opreturn="Ok"
					for j=1 to l
						cstate(ci+i+1,j)=cstate(ci+i,j)
					next j
				
				case "Dimension"
					dim_x=looplist(i,(gl(i).c))
					dim_y=l\dim_x
					if frac(l/dim_x)>0 then dim_y+=1
					if i>1 then a+=", "
					a+="Dimension"
					a+="("+str(dim_x)+","+str(dim_y)+")"
					opreturn="Ok"
					for j=1 to l
						cstate(ci+i+1,j)=cstate(ci+i,j)
					next j
			
				case "Directions"
					arg(1)=l
					arg(3)=dim_x
					arg(4)=dim_y
					arg(5)=utp1
					arg(6)=0 'keepnulls
					subop=dirs(looplist(i,(gl(i).c)))
					opreturn=cstate_operation(ci+i,ci+i+1,subop,arg())
					if opreturn="Ok" then
						if i>1 then a+=", "	
						select case looplist(i,(gl(i).c))
							case 1:a+="Normal"
							case 2:a+="Mirror"
							case 3:a+="Flip"
							case 4:a+="Reverse"
							case 5:a+="Columnar":if arg(5)=0 then a+="(TP,1)" else a+="(UTP,1)"
							case 6:a+="Columnar":if arg(5)=0 then a+="(TP,2)" else a+="(UTP,2)"
							case 7:a+="Columnar":if arg(5)=0 then a+="(TP,3)" else a+="(UTP,3)"
							case 8:a+="Columnar":if arg(5)=0 then a+="(TP,4)" else a+="(UTP,4)"
							case 9:a+="Diagonal":if arg(5)=0 then a+="(TP,1)" else a+="(UTP,1)"
							case 10:a+="Diagonal":if arg(5)=0 then a+="(TP,2)" else a+="(UTP,2)"
							case 11:a+="Diagonal":if arg(5)=0 then a+="(TP,3)" else a+="(UTP,3)"
							case 12:a+="Diagonal":if arg(5)=0 then a+="(TP,4)" else a+="(UTP,4)"
							case 13:a+="Diagonal":if arg(5)=0 then a+="(TP,5)" else a+="(UTP,5)"
							case 14:a+="Diagonal":if arg(5)=0 then a+="(TP,6)" else a+="(UTP,6)"
							case 15:a+="Diagonal":if arg(5)=0 then a+="(TP,7)" else a+="(UTP,7)"
							case 16:a+="Diagonal":if arg(5)=0 then a+="(TP,8)" else a+="(UTP,8)"
						end select
					else
						a+=opreturn
						exit for
					end if
				
				case "Period","Skytale","Offset"
					arg(1)=l
					arg(5)=utp1
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if opreturn="Ok" then
						if i>1 then a+=", "
						a+=op1
						if arg(5)=0 then a+="(TP," else a+="(UTP,"
						a+=str(arg(7))+")"
					else
						a+=opreturn
						exit for
					end if
				
				case "Offset row order","Offset column order","Period row order","Period column order"
					arg(1)=l
					arg(3)=dim_x
					arg(4)=dim_y
					arg(5)=utp1
					arg(6)=0 'keepnulls
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if opreturn="Ok" then
						if i>1 then a+=", "
						a+=op1
						if arg(5)=0 then a+="(TP," else a+="(UTP,"
						a+=str(arg(7))+")"
					else
						a+=opreturn
						exit for
					end if
				
				case "Expand symbol"
					arg(1)=l
					arg(2)=s
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						's+=val(opreturn)
						s+=val(right(opreturn,len(opreturn)-instr(opreturn,",")))
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Add character","Add random character"
					arg(1)=l
					arg(2)=s
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l+=1
						s+=1
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Add characters"
					arg(1)=l
					arg(2)=s
					arg(7)=arg4 'characters
					arg(8)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l+=arg4
						s+=arg4
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+","+str(arg(8))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Add nulls and skips"
					arg(1)=l
					arg(2)=s
					arg(7)=arg4 'nulls
					arg(8)=arg5 'skips
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l+=arg4-arg5
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						if i>1 then a+=", "
						a+=op1
						a+="("+str(looplist(i,(gl(i).c)))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Add column"
					arg(1)=l
					arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						s+=dim_y
						l+=dim_y
						dim_x+=1
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Add column (using random symbols)"
					arg(1)=l
					arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=arg4 'looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l+=dim_y	
						cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						dim_x+=1
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
					
				case "Add row"
					arg(1)=l
					arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						s+=dim_x
						l+=dim_x
						dim_y+=1
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Add row (using random symbols)"
					arg(1)=l
					arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=arg4 'looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l+=dim_x
						cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						dim_y+=1
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Random nulls"
					arg(1)=l
					arg(2)=s
					arg(7)=arg4
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l-=arg4
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Random skips"
					arg(1)=l
					arg(2)=s
					arg(7)=arg4
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l+=arg4
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Remove character"
					arg(1)=l
					arg(2)=s
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l-=1 
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Remove characters"
					arg(1)=l
					arg(2)=s
					arg(7)=arg4 'characters
					arg(8)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l-=arg4
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+","+str(arg(8))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Remove column"
					arg(1)=l
					arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l-=dim_y
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						dim_x-=1
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Remove row"
					arg(1)=l
					arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						l-=dim_x
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						dim_y-=1
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Randomize"
					arg(1)=l
					arg(2)=s
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
					
				case "Randomize and bigrams"
					arg(1)=l
					arg(2)=s
					arg(7)=arg4
					opreturn=cstate_operation(ci+i,ci+i+1,"Randomize and bigrams 2",arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Randomize row"
					arg(1)=l
					'arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Randomize row order"
					arg(1)=l
					'arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Randomize column"
					arg(1)=l
					'arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if			
					
				case "Randomize column order"
					arg(1)=l
					'arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					arg(7)=looplist(i,(gl(i).c))
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(arg(7))+")"
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
			 	
				case "Plaintext"
					arg(1)=l
					ptn=looplist(i,(gl(i).c))
					s=ptl(ptn,0)
					arg(2)=s
					arg(3)=dim_x
					arg(4)=dim_y
					if i>1 then a+=", "
					a+="Plaintext"
					a+="("+str(ptn)+")"
					opreturn="Ok"
					for j=1 to l
						cstate(ci+i+2,j)=ptl(ptn,j)
					next j
					s=cstate_nba(ci+i+2,ci+i+1,l,s)
				
				case "Encode: caesar shift"
					arg(1)=l
					arg(2)=s
					arg(7)=arg5 'from
					arg(8)=arg6 'to
					arg(9)=1 'step
					arg(10)=arg4 'shift
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(looplist(i,(gl(i).c)))+")"
						s=cstate_symbolcount(ci+i+1,l,l) 'l or 2000?
						's=cstate_nba(ci+i+1,ci+i+2,l,s)
						'for j=1 to l
						'	cstate(ci+i+1,j)=cstate(ci+i+2,j)
						'next j
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Encode: homophonic substitution"
					arg(1)=l
					arg(2)=s
					'arg(7)=looplist(i,(gl(i).c))
					arg(7)=1
					arg(8)=l
					arg(9)=0
					arg(10)=arg4
					arg(11)=arg5
					arg(12)=arg6
					'nba layer?
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(looplist(i,(gl(i).c)))+")"
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Encode: homophonic substitution 2"
					arg(1)=l
					arg(2)=s
					'arg(7)=looplist(i,(gl(i).c))
					arg(7)=1
					arg(8)=l
					arg(9)=0
					arg(10)=arg4
					arg(11)=arg5
					arg(12)=arg6
					'nba layer?
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(looplist(i,(gl(i).c)))+")"
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
					
				case "Encode: homophonic substitution 1-170"
					arg(1)=l
					arg(2)=s
					'arg(7)=looplist(i,(gl(i).c))
					arg(7)=1
					arg(8)=170
					arg(9)=0 'step
					arg(10)=arg4
					arg(11)=arg5
					arg(12)=arg6
					'nba layer?
					op1="Encode: homophonic substitution"
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(looplist(i,(gl(i).c)))+")"
						's=cstate_nba(ci+i+1,ci+i+2,l,s)
						'for j=1 to l
						'	cstate(ci+i+1,j)=cstate(ci+i+2,j)
						'next j
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
				
				case "Encode: homophonic substitution 171-340"
					arg(1)=l
					arg(2)=s
					'arg(7)=looplist(i,(gl(i).c))
					arg(7)=171
					arg(8)=340
					arg(9)=0 'step
					arg(10)=arg4
					arg(11)=arg5
					arg(12)=arg6
					'nba layer?
					op1="Encode: homophonic substitution"
					opreturn=cstate_operation(ci+i,ci+i+1,op1,arg())
					if left(opreturn,5)<>"Error" then
						if i>1 then a+=", "
						a+=op1
						a+="("+str(looplist(i,(gl(i).c)))+")"
						s=cstate_nba(ci+i+1,ci+i+2,l,s)
						for j=1 to l
							cstate(ci+i+1,j)=cstate(ci+i+2,j)
						next j
						opreturn="Ok"
					else
						a+=opreturn
						exit for
					end if
			 	
				case else
					beep
				
			end select
		
		next i
		
		'raw ioc filter
		'--------------
		if useioc=1 then
			for i=1 to l
				cip(i)=cstate(ci+depth+1,i)
			next i
			dim as integer rit=m_ioc3(cip(),l,s,0)
			if rit<combine_minioc or rit>combine_maxioc then opreturn="Error"
		end if
		
		if opreturn="Ok" then
			
			ok_count+=1
			
			if useioc=0 then
				for i=1 to l
					cip(i)=cstate(ci+depth+1,i)
				next i
			end if
			
			if uselen=1 then
				dim as short tl=combine_tolen
				if tl>l then tl=l
				l=0
				for i=combine_fromlen to tl
					l+=1
					part(l)=cip(i)
				next i
				s=nba_to_info_out(part(),l,s)
				for i=1 to l
					cip(i)=info_out(i)
				next i
			end if
				
			select case measurement
				
				case "Solve substitution"
					do
						sleep twait
						for t=1 to threads
							if pausetask=1 then 'pause task
								update_solver_status
								do
									sleep 10
								loop until pausetask=0
								update_solver_status
							end if
							if thread(t).solver_waiting=1 then	
								thread(t).outputdir=basedir+"\Output\"
								thread(t).l=l
								thread(t).s=s
								thread(t).dim_x=dim_x
								thread(t).dim_y=dim_y
								thread(t).score=0
								thread(t).pcmode=0
								thread(t).itemnumber=ok_count
								thread(t).itemname=a
								thread(t).iterations=solvesub_iterations
								thread(t).advstats=solvesub_advstats
								thread(t).temperature=solvesub_temperature
								thread(t).restarts=solvesub_restarts
								thread(t).subrestartlevels=solvesub_subrestartlevels
								thread(t).ngramfactor=solvesub_ngramfactor
								thread(t).multiplicityweight=solvesub_multiplicityweight
								thread(t).entweight=solvesub_entweight
								thread(t).solver_stop=0
								for i=1 to l
									thread(t).cip(i)=cip(i)
								next i
								thread(t).update=0
								thread(t).solver_waiting=0 'engage thread 
								'toggle_solverthreads(cip(),l,s,dim_x,dim_y,basedir+"\Output\",3,i,i)
								if timer-statustimer>1 then
									statustimer=timer
									update_solver_status
								end if
								exit select
							end if
						next t
					loop until stoptask=1
					
				case "2-symbol cycles"
					combine_score(ok_count)=m_2cycles(cip(),l,s,5)
				
				case "3-symbol cycles"
					combine_score(ok_count)=m_3cycles(cip(),l,s,5)
				
				case "Perfect 2-symbol cycles"
					combine_score(ok_count)=m_2cycles_perfect(cip(),l,s)
				
				case "Perfect 3-symbol cycles"
					combine_score(ok_count)=m_3cycles_perfect(cip(),l,s)
					
				case "Unigram repeats"
					combine_score(ok_count)=m_unigrams(cip(),l,s,dim_x,normalized)
					
				case "Sliding unigram repeats"
					combine_score(ok_count)=m_unigrams(cip(),l,s,dim_x,1)
				
				case "Unigram non repeating rows"
					combine_score(ok_count)=m_unigramunitnorepeats(cip(),l,s,dim_x,dim_y,0,0,normalized)
					
				case "Unigram non repeating columns"
					combine_score(ok_count)=m_unigramunitnorepeats(cip(),l,s,dim_x,dim_y,1,0,normalized)
					
				case "Bigrams"
					combine_score(ok_count)=m_fastbigrams(cip(),l,s)
					'combine_score(ok_count)=m_bigrams(cip(),l,s,normalized)
					
				case "Bigrams alphabet A1"
					combine_score(ok_count)=m_fastbigrams_alphabet(cip(),l,s,ma1)
					
				case "Deep bigrams"
					combine_score(ok_count)=m_deep(cip(),l,s,1,0,0)
				
				case "Periods"
					combine_score(ok_count)=m_periods(cip(),l,s)
				
				case "Periods + offset column order"
					combine_score(ok_count)=m_periodsplusoco(cip(),l,s,dim_x,dim_y)
					
				case "Asymmetry"
					combine_score(ok_count)=m_asymmetry(cip(),l,s,normalized)
					
				case "Sequential A1"
					combine_score(ok_count)=m_sequential(cip(),l,s,ma1,0)
					for i=1 to freq(0)
						graph(1,1,i)+=freq(i)
					next i
				
				case "Sequential length A1"			
					combine_score(ok_count)=m_sequential(cip(),l,s,1,ma1)
					
				case "Appearance"
					combine_score(ok_count)=m_appearance(cip(),l,s,1,0)
				
				case "Midpoint shift"
					combine_score(ok_count)=m_midpointshift(cip(),l,s,0)
				
				case "Prime phobia"
					combine_score(ok_count)=m_primephobia(cip(),l,s,0)
				
				case "Raw IOC"
					combine_score(ok_count)=m_ioc3(cip(),l,s,0)
					
				case "Symbols"
					combine_score(ok_count)=nba_to_info_out(cip(),l,s)
					
				case "Highest frequency"
					combine_score(ok_count)=m_highestfrequency(cip(),l,s)
					
				case "Unigram A1-middle-A1"
					combine_score(ok_count)=m_unigram_tmb(cip(),l,s,ma1,0)
				
				case "Symbols A1-middle-A1"
					combine_score(ok_count)=m_unigram_tmb(cip(),l,s,ma1,1)
				
				case "Unigram distance"
					combine_score(ok_count)=m_unigramdistance(cip(),l,s)
					
				case "Unigram distance, log under A1"
					combine_score(ok_count)=m_unigramdistanceuo(cip(),l,s,0,ma1)
					
				case "Unigram distance, log over A1"
					combine_score(ok_count)=m_unigramdistanceuo(cip(),l,s,1,ma1)
					
				case "Unique unigrams period A1 by columns"
					combine_score(ok_count)=0
					for i=1 to ma1
						combine_score(ok_count)+=m_unigramperiodic(cip(),l,s,1,0,ma1,i)
					next i
				
				case "Unique unigrams period versus A1 by rows"
					combine_score(ok_count)=0
					for i=1 to ma1
						combine_score(ok_count)+=m_unigramperiodicvs(cip(),l,s,0,0,ma1,i)
					next i
				
				case "IOC period A1 by columns"
					combine_score(ok_count)=0
					for i=1 to ma1
						combine_score(ok_count)+=m_iocperiodic(cip(),l,s,1,0,ma1,i)
					next i
				
				case "Slope"
					combine_score(ok_count)=m_slope(cip(),l,s,normalized)
				
				case "Keyword length A1"
					combine_score(ok_count)=m_posshift(cip(),l,s,ma1,1)
				
				case "N-grams"
					combine_score(ok_count)=m_ngrams(cip(),l,normalized)
					
				case "Pivots"
					combine_score(ok_count)=m_pivots(cip(),l,dim_x,dim_y,3)
					
				case "5-gram fragments"
					combine_score(ok_count)=m_ngramfragments(cip(),l,s,5)
					
				case "Doublets"
					combine_score(ok_count)=m_npairs(cip(),l,2)
				
				case "Triplets"
					combine_score(ok_count)=m_npairs(cip(),l,3)
					
				case "SSS position"
					combine_score(ok_count)=m_shortestsubstring(cip(),l,s)
					
				case "Trigrams"
					combine_score(ok_count)=m_fasttrigrams(cip(),l,s)
				
				case "Cycle spectrum"
					combine_score(ok_count)=m_2cyclespectrum(cip(),l,s)*10000
				
				case "Adjacency"
					combine_score(ok_count)=m_adjacency(cip(),l,dim_x,dim_y)
				
			end select
		
			combine_item(ok_count)=a
			
		end if
		
		'------------------------------------------------------------------------------
			
		do
			if curr<depth then 'climb	
				curr+=1
				select case gl(curr).of 'option from
					case is=0 'nothing
						'...
					case is=1 'add
						gl(curr).f=gl(gl(curr).ofi).c+gl(curr).ofm
						gl(curr).c=gl(curr).f
					case is=2 'sub
						'...
				end select		
			else	
				if gl(curr).c+gl(curr).s<=gl(curr).t then 'increment
					gl(curr).c+=gl(curr).s
					exit do
				else			
					do 'descent
						curr-=1
						if curr=0 then exit do,do,do
					loop until gl(curr).c<gl(curr).t
					if gl(curr).c+gl(curr).s<=gl(curr).t then
						gl(curr).c+=gl(curr).s
					else
						exit do,do
					end if			
					for i=curr+1 to depth
						select case gl(i).of 'option from
							case is=0 'nothing
								gl(i).c=gl(i).f
							case is=1 'add
								gl(i).f=gl(gl(i).ofi).c+gl(i).ofm
								gl(i).c=gl(i).f
							case is=2 'sub
								'...
						end select
					next i
					exit do
				end if
			end if
		loop
	
	loop until stoptask=1 'end generic loop
	
	dim as double stucktimer=timer
	if measurement="Solve substitution" then
		for i=1 to threads
			'thread(i).solver_stop=1
			do
				sleep 10
			loop until thread(i).solver_waiting=1 'or timer-stucktimer>2
		next i
		b=shared_string
	end if
	
	'create output string
	'-------------------------------------
	
	ui_editbox_settext(output_text,"Processing table, please wait...")
	
	b+="BHdecrypt combine statistics for: "+file_name+lb
	b+="---------------------------------------------------------"+lb
	
	dim as double avg1,avg2,mean,variance,sd
	dim as uinteger ovc,unc
	
	for i=1 to ok_count 'combine_combinations
		m=combine_score(i)
		if m>=getsigma then ovc+=1
		if m<getsigma then unc+=1
		n=combine_item(i)
		if omitlist=0 then b+=n+": "+str(m)+lb
		'b+=str(m)+lb
		avg1+=m
		if m<low.m then
			low.m=m
			low.n=n
		end if
		if m>high.m then
			high.m=m
			high.n=n
		end if
	next i

	mean=avg1/ok_count
	for i=1 to ok_count
		avg2+=(combine_score(i)-mean)^2
	next i
	variance=avg2/ok_count
	sd=sqr(variance)
	
	if omitlist=1 then b+="List omitted by user."+lb
	b+="---------------------------------------------------------"+lb
	b+="Combinations processed: "+str(ok_count)+"/"+str(local_combinations)+lb
	b+="Measurements:"+lb
	'b+="- Summed: "+str(avg)+lb
	b+="- Mean: "+str(mean)+lb
	b+="- Variance: "+str(variance)+lb
	b+="- Standard deviation: "+str(sd)+lb
	'b+="- Sigma of "+str(getsigma)+": "+str(abs(getsigma-mean)/sd)+lb
	if getsigma>0 then b+="- Count equal/over "+str(getsigma)+": "+str(ovc)+lb
	if getsigma>0 then b+="- Count under "+str(getsigma)+": "+str(unc)+lb
	if getsigma>0 then b+="- Sigma of "+str(getsigma)+": "+str((getsigma-mean)/sd)+lb
	b+="- Sigma of "+str(low.m)+": "+str((low.m-mean)/sd)+lb
	b+="- Sigma of "+str(high.m)+": "+str((high.m-mean)/sd)+lb
	b+="- Lowest: "+str(low.m)+" ("+low.n+")"+lb
	b+="- Highest: "+str(high.m)+" ("+high.n+")"+lb+lb
	
	b+="Standard deviation distribution counts:"+lb
	b+="---------------------------------------------------------"+lb
	dim as double maxsd,cs
	if (low.m-mean)/sd>(high.m-mean)/sd then maxsd=(low.m-mean)/sd else maxsd=(high.m-mean)/sd
	if maxsd>int(maxsd) then maxsd=int(maxsd)+1
	dim as integer disp(maxsd) 'positive
	dim as integer disn(maxsd) 'negative
	dim as integer dism	
	for i=1 to ok_count
		cs=(combine_score(i)-mean)/sd
		if cs>0 then disp(int(cs))+=1 
		if cs<0 then disn(int(abs(cs)))+=1
		if cs=0 then dism+=1
	next i
	for i=0 to maxsd
		j=maxsd-i
		b+="+"+str(j)+": "+str(disp(j))+lb
	next i
	b+="=0: "+str(dism)+lb
	for i=0 to maxsd
		b+="-"+str(i)+": "+str(disn(i))
		if i<>maxsd then b+=lb
	next i
	
	if measurement="Sequential A1" then
		for i=1 to constcip
			graph(1,1,i)=int(graph(1,1,i)/ok_count)
		next i
		output_graph(1,1,left(file_name,instr(file_name,".")-1),left(file_name,instr(file_name,".")-1))
	end if
	
	if hypergraph=1 then
		
		ui_editbox_settext(output_text,"Processing hypergraph, please wait...")	
		if forcelinear=1 then local_maxlistlength=1
		
		if local_maxlistlength=1 then
			
			dim as short color1
			dim as double scorediv
			dim as integer height=100
			dim as double hmul=height/250
			scorediv=(high.m-low.m)/250
			screenres(count,height,32,,gfx_null)
			paint(1,1),rgb(100,100,100)
			
			for h=1 to count
				color1=((combine_score(h)-low.m)/scorediv)
				line(h-1,height)-(h-1,height-(color1*hmul)),rgb(0,0,color1),bf	
			next h
			
		else
		
			dim as integer currdimx
			dim as integer currdimy
			dim as integer dimx(local_maxlistlength)
			dim as integer dimy(local_maxlistlength)
			dim as integer dimxrev(local_maxlistlength)
			dim as integer dimyrev(local_maxlistlength)
			dim as integer unitx=1
			dim as integer unity=1
			dim as integer srx=1
			dim as integer sry=1
			dim as integer xx
			dim as integer yy
			dim as integer x1
			dim as integer y1
			dim as integer x2
			dim as integer y2
			dim as short color1
			
			dim as double scorediv
			scorediv=(high.m-low.m)/250
			
			dim as integer si
			dim as integer sa
			dim as integer sb
			dim as integer dimsrev(64)
			dim as integer spaces(64)
			for i=1 to 64
				spaces(i)=i-1
			next i
				
			'calculate hypergraph dimensions
			'-------------------------------
			
			for i=1 to local_maxlistlength
				j=local_maxlistlength-(i-1)
				if j mod 2=1 then
					currdimx+=1
					dimx(currdimx)=looptable(j,0).n
					srx*=dimx(currdimx)
				else
					currdimy+=1
					dimy(currdimy)=looptable(j,0).n
					sry*=dimy(currdimy)
				end if
			next i
			for i=1 to currdimx
				dimxrev(i)=dimx(currdimx-(i-1))
			next i
			for i=1 to currdimy
				dimyrev(i)=dimy(currdimy-(i-1))
			next i	
			
			sb=0
			for i=1 to currdimx
				if spaces(i)>0 then
					for j=i to currdimx
						sa=spaces(i)
						for k=i to j
							if k=j then
								sa*=dimxrev(k)-1
							else
								if k=i then
									sa*=dimxrev(k)-1
								else
									sa*=dimxrev(k)
								end if
							end if
						next k
						sb+=sa
					next j	
				end if
			next i
			srx+=sb
			sb=0
			for i=1 to currdimy
				if spaces(i)>0 then
					for j=i to currdimy
						sa=spaces(i)
						for k=i to j
							if k=j then
								sa*=dimyrev(k)-1
							else
								if k=i then
									sa*=dimyrev(k)-1
								else
									sa*=dimyrev(k)
								end if
							end if
						next k
						sb+=sa
					next j	
				end if
			next i
			sry+=sb
			
			screenres(srx*unitx,sry*unity,32,,gfx_null)
			'paint(1,1),rgb(100,100,100)
			
			dim as integer o1
			dim as integer o2
			if local_maxlistlength mod 2=0 then
				o1=2
				o2=1
			else
				o1=1
				o2=2
			end if
				
			for h=1 to count
				
				for i=1 to local_maxlistlength
					dimsrev(i)=combine_dims(h,local_maxlistlength-(i-1))
				next i
				 
				x=0
				for j=1 to currdimx-1
					x1=combine_dims(h,o1+((j-1)*2))-1
					for k=j+1 to currdimx
						x1*=dimx(k)
					next k
					x+=x1
				next j
				x+=combine_dims(h,local_maxlistlength)
				
				sb=0
				for i=1 to currdimx
					if spaces(i)>0 then
						for j=i to currdimx
							sa=spaces(i)
							for k=i to j
								if k=j then
									sa*=dimsrev(1+((k-1)*2))-1
								else
									if k=i then
										sa*=dimxrev(k)-1
									else
										sa*=dimxrev(k)
									end if
								end if
							next k
							sb+=sa
						next j	
					end if
				next i
				x+=sb
			 
				y=0
				for j=1 to currdimy-1
					y1=combine_dims(h,o2+((j-1)*2))-1
					for k=j+1 to currdimy
						y1*=dimy(k)
					next k
					y+=y1
				next j
				y+=combine_dims(h,local_maxlistlength-1)
				
				sb=0
				for i=1 to currdimy
					if spaces(i)>0 then
						for j=i to currdimy
							sa=spaces(i)
							for k=i to j
								if k=j then
									sa*=dimsrev(2+((k-1)*2))-1
								else
									if k=i then
										sa*=dimyrev(k)-1
									else
										sa*=dimyrev(k)
									end if
								end if
							next k
							sb+=sa
						next j	
					end if
				next i
				y+=sb
						
				if x=0 then x=1
				if y=0 then y=1			
				
				x=(x*unitx)-1 '-unitx
				y=(y*unity)-1 '-unity
			
				color1=((combine_score(h)-low.m)/scorediv)
				'line(x,y)-(x+(unitx-1),y+(unity-1)),rgb(0,0,color1),bf
				pset(x,y),rgb(0,0,color1)
				
			next h
		
		end if
		
		bsave basedir+"\Output\hypergraph.bmp",0
	
	end if
	
	combine_score(0)=low.m
	output_colormap(combine_score(),cip2(),local_l,local_x,local_y,0,"heatmap")
	'output_colormap(combine_score(),cip2(),local_l,local_x,local_y,1,"heatmap_2")
	
	ui_editbox_settext(output_text,b)
	open basedir+"\Output\list.txt" for output as #1
	print #1,b;
	close #1
	
	if measurement="Solve substitution" then
		'solvesub_progressive=old_solvesub_progressive
		toggle_solverthreads(empty(),0,0,0,0,basedir+"\Output\",4,1,threads)
		for i=1 to threads
			thread(i).combine_output=0
		next i
	end if
	
	clean_thread_information
	redim combine_item(0)
	redim combine_score(0)
	redim combine_dims(0,0)
	task_active="none"
	solver_status_processing=0
	update_solver_status

end sub

sub output_colormap(vls()as double,cip()as long,byval l as integer,byval dx as integer,byval dy as integer,byval norm as double,byval ns as string)
	
	'cip=cipher array (optional) to include symbols
	'vls=values array
	'l=cipher length
	'dx=dimension x
	'dy=dimension y
	'norm=normalization 0/1
	'ns=colormap output name string
	
	dim as integer num=info_numerical
	dim as integer i,j,k,x,y,c
	dim as integer unit=34
	dim as double low,high,divi,cr,rr,gg,bb
	dim as string xy
	dim as integer rs,bs,px1,py1,px2,py2
 	
 	screenres(1+(dx+1)*unit,1+(dy+1)*unit,32,,gfx_null)
	paint(1,1),rgb(255,255,255)
 	
 	low=vls(0)
 	if norm=0 then
		for i=1 to l
			if vls(i)<low then low=vls(i)
			if vls(i)>high then high=vls(i)
		next i
		divi=(high-low)/510
	else
		divi=norm/510
 	end if
	
	i=0
	for y=0 to dy
		for x=0 to dx
			px1=x*unit
			py1=y*unit
			px2=(x+1)*unit
			py2=(y+1)*unit
			if x>0 andalso y>0 then
				i+=1	
				cr=int((vls(i)-low)/divi)
				if cr>510 then cr=510
				if cr<=255 then
					rr=cr mod 256
					gg=255
					bb=255-(cr mod 256)
				end if
				if cr>255 andalso cr<=510 then
					rr=255
					gg=255-(cr mod 256)
					bb=0
				end if
				'line(px1,py1)-(px2,py2),rgb(rr,gg,bb),bf
				'line(px1,py1)-(px2,py2),rgb(125,125,125),b
				if cip(i)>0 andalso i<=l then
					line(px1,py1)-(px2,py2),rgb(rr,gg,bb),bf
					line(px1,py1)-(px2,py2),rgb(125,125,125),b
					if num=0 then xy=chr(cip(i)) else xy=str(cip(i))
					rs=unit/2-len(xy)*3-(len(xy)-1)
					bs=unit/2-3
					draw string (px1+rs,py1+bs),xy,rgb(0,0,0)
				end if
			else
				if x=0 then xy=str(y) else xy=str(x)
				rs=unit/2-len(xy)*3-(len(xy)-1)
				bs=unit/2-3
				line(px1,py1)-(px2,py2),rgb(255,255,255),bf
				line(px1,py1)-(px2,py2),rgb(125,125,125),b
				draw string (px1+rs,py1+bs),xy,rgb(100,100,100)
			end if 
		next x
	next y
	bsave basedir+"\Output\"+ns+".bmp",0

end sub

sub output_graph(byval t as short,byval chs as short,byval hs as string,byval ns as string)
	
	dim as integer h,i,j,k,x,y,v1,v2
	dim as integer dx '=1024
	dim as integer dy '=768
	dim as integer ux=14
	dim as integer uy=14
	dim as long ox=50
	dim as long oy=50
	dim as integer hx,hy
	dim as short r,g,b
	
	'graph(thread,channel,entries)
	'multi-channel color graph
	'clear graph array before use
	
	for i=1 to chs
		for j=1 to constcip
			if graph(t,i,j)>0 then
				if j>hx then hx=j
				if graph(t,i,j)>hy then hy=graph(t,i,j)
			end if
		next j
	next i
	
	dx=100+ux*hx
	dy=100+uy*hy
	
	screenres(dx,dy,32,,gfx_null)
	paint(1,1),rgb(255,255,255)
	line(0,0)-(dx-1,dy-1),rgb(210,210,210),b
	draw string((dx/2)-(len(hs)*4),22),str(hs),rgb(150,150,150)
	
	for i=0 to hy 'horizontal grid lines
		j=hy-i
		line(ox,oy+(ux*i))-(dx-ox,oy+(ux*i)),rgb(210,210,210)
		if i mod 10=0 then r=75 else r=150
		draw string(ox-14,oy+(ux*j)-3),str(i mod 10),rgb(r,r,r)
	next i
	for i=0 to hx 'vertical grid lines
		line(ox+(uy*i),oy)-(ox+(uy*i),dy-oy),rgb(210,210,210)
		if (i+1) mod 10=0 then r=75 else r=150
		draw string(ox+(uy*i)-3,(dy-oy)+8),str((i+1) mod 10),rgb(r,r,r)
	next i
	
	dim as double vd=255/hy

	for h=1 to chs
		i=chs-(h-1)
		r=0:g=0:b=0
		select case i
			case 1:r=255
			case 2:g=200
			case 3:b=255
			case 4:r=255:g=255
			'------------------------
			case 5:r=125:g=125:b=255
			case 6:r=150:g=150:b=255
			case 7:r=175:g=175:b=255
			case 8:r=200:g=200:b=255
			'------------------------
			case 9:r=255:g=200:b=255
			case 10:r=255:g=180:b=255
			case 11:r=255:g=160:b=255
			case 12:r=255:g=140:b=255
			'------------------------
			case 13:r=255:g=120:b=255
			case 14:r=255:g=100:b=255
			case 15:r=255:g=80:b=255
			case 16:r=255:g=60:b=255
		end select
		for j=1 to hx
			v1=graph(t,i,j)
			v2=graph(t,i,j+1)
			'if v1>0 then
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy))-(ox+(ux*j),(dy-oy)-(v2*uy)),rgb(r,g,b)
			'-------------------------------------------------------------------------------
			line(ox+(ux*(j-1))+1,(dy-oy)-(v1*uy))-(ox+(ux*j),(dy-oy)-(v2*uy)),rgb(r,g,b)
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy))-(ox+(ux*j)+1,(dy-oy)-(v2*uy)),rgb(r,g,b)
			'-------------------------------------------------------------------------------
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy)-1)-(ox+(ux*j),(dy-oy)-(v2*uy)),rgb(r,g,b)
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy))-(ox+(ux*j),(dy-oy)-(v2*uy)-1),rgb(r,g,b)
			'-------------------------------------------------------------------------------
			line(ox+(ux*(j-1))+1,(dy-oy)-(v1*uy))-(ox+(ux*j)+1,(dy-oy)-(v2*uy)),rgb(r,g,b)
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy)-1)-(ox+(ux*j),(dy-oy)-(v2*uy)-1),rgb(r,g,b)
			'end if
		next j
	next h
 	
	bsave basedir+"\Output\"+ns+".bmp",0

end sub

sub output_graph2(byval t as short,byval chs as short,byval hs as string,byval ns as string)
	
	dim as integer h,i,j,k,x,y
	dim as integer dx '=1024
	dim as integer dy '=768
	dim as integer ux=14
	dim as integer uy=14
	dim as long ox=50
	dim as long oy=50
	dim as integer hx,hy,ly,uly
	dim as short r,g,b
	dim as double v1,v2
	
	'graph(thread,channel,entries)
	'multi-channel color graph
	'clear graph array before use
	
	hx=graph(t,1,0)-1
	
	for i=1 to chs
		for j=1 to hx+1
			'if graph(t,i,j)>0 then
				'if j>hx then hx=j
				if graph(t,i,j)>hy then hy=graph(t,i,j)
				if graph(t,i,j)<ly then ly=graph(t,i,j)
			'end if
		next j
	next i
	
	dx=100+ux*hx
	dy=100+uy*(hy+abs(ly))
	uly=uy*abs(ly)
	
	screenres(dx,dy,32,,gfx_null)
	paint(1,1),rgb(255,255,255)
	line(0,0)-(dx-1,dy-1),rgb(210,210,210),b
	draw string((dx/2)-(len(hs)*4),22),str(hs),rgb(150,150,150)
	
	for i=0 to hy 'horizontal grid lines
		j=hy-i
		line(ox,oy+(ux*i))-(dx-ox,oy+(ux*i)),rgb(210,210,210)
		if i mod 10=0 then r=75 else r=150
		draw string(ox-14,oy+(ux*j)-3),str(i mod 10),rgb(r,r,r)
	next i
	for i=hy to hy+abs(ly) 'horizontal grid lines
		j=i
		line(ox,oy+(ux*i))-(dx-ox,oy+(ux*i)),rgb(210,210,210)
		if k mod 10=0 then r=75 else r=150
		draw string(ox-14,oy+(ux*j)-3),str(k mod 10),rgb(r,r,r)
		k+=1
	next i
	for i=0 to hx 'vertical grid lines
		line(ox+(uy*i),oy)-(ox+(uy*i),dy-oy),rgb(210,210,210)
		if (i+1) mod 10=0 then r=75 else r=150
		draw string(ox+(uy*i)-3,(dy-oy)+8),str((i+1) mod 10),rgb(r,r,r)
	next i
	
	line(ox,oy+(ux*hy))-(dx-ox,oy+(ux*hy)),rgb(150,150,150)
	
	dim as double vd=255/hy
	dy-=uly

	for h=1 to chs
		i=chs-(h-1)
		r=0:g=0:b=0
		select case i
			case 1:r=255
			case 2:g=200
			case 3:b=255
			case 4:r=255:g=255
			'------------------------
			case 5:r=125:g=125:b=255
			case 6:r=150:g=150:b=255
			case 7:r=175:g=175:b=255
			case 8:r=200:g=200:b=255
			'------------------------
			case 9:r=255:g=200:b=255
			case 10:r=255:g=180:b=255
			case 11:r=255:g=160:b=255
			case 12:r=255:g=140:b=255
			'------------------------
			case 13:r=255:g=120:b=255
			case 14:r=255:g=100:b=255
			case 15:r=255:g=80:b=255
			case 16:r=255:g=60:b=255
		end select
		for j=1 to hx
			v1=graph(t,i,j)
			v2=graph(t,i,j+1)
			'if v1>0 then
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy))-(ox+(ux*j),(dy-oy)-(v2*uy)),rgb(r,g,b)
			'-------------------------------------------------------------------------------
			line(ox+(ux*(j-1))+1,(dy-oy)-(v1*uy))-(ox+(ux*j),(dy-oy)-(v2*uy)),rgb(r,g,b)
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy))-(ox+(ux*j)+1,(dy-oy)-(v2*uy)),rgb(r,g,b)
			'-------------------------------------------------------------------------------
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy)-1)-(ox+(ux*j),(dy-oy)-(v2*uy)),rgb(r,g,b)
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy))-(ox+(ux*j),(dy-oy)-(v2*uy)-1),rgb(r,g,b)
			'-------------------------------------------------------------------------------
			line(ox+(ux*(j-1))+1,(dy-oy)-(v1*uy))-(ox+(ux*j)+1,(dy-oy)-(v2*uy)),rgb(r,g,b)
			line(ox+(ux*(j-1)),(dy-oy)-(v1*uy)-1)-(ox+(ux*j),(dy-oy)-(v2*uy)-1),rgb(r,g,b)
			'end if
		next j
	next h
 	
	bsave basedir+"\Output\"+ns+".bmp",0

end sub


#include "stats_functions.bi"


sub pickletter_caching(byval showmsg as ubyte)
	
	dim as string o
	select case ngram_size
		case 8
			if solvesub_ngramcaching=0 then
				redim cachebh80(0,0,0,0)
				redim cachebh81(0,0,0,0)
				redim cachebh82(0,0,0,0)
				redim cachebh83(0,0,0,0)
				redim cachebh84(0,0,0,0)
				redim cachebh85(0,0,0,0)
				redim cachebh86(0,0,0,0)
				redim cachebh87(0,0,0,0)
				if showmsg=1 then
					o=str(ngram_size)+"-gram caching disabled:"+lb
					o+="---------------------------------------------"+lb
					o+="Free memory: "+format(fre/1073741824,"0.00")+" GB RAM"+lb
					ui_editbox_settext(output_text,o)
				end if
			else
				dim as uinteger cachebh8_nfb=8*ngram_alphabet_size^3*ngram_maxtableindex
				if fre>=cachebh8_nfb then
					redim cachebh80(0,0,0,0)
					redim cachebh81(0,0,0,0)
					redim cachebh82(0,0,0,0)
					redim cachebh83(0,0,0,0)
					redim cachebh84(0,0,0,0)
					redim cachebh85(0,0,0,0)
					redim cachebh86(0,0,0,0)
					redim cachebh87(0,0,0,0)
					redim cachebh80(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_maxtableindex)
					redim cachebh81(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_maxtableindex)
					redim cachebh82(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_maxtableindex)
					redim cachebh83(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_maxtableindex)
					redim cachebh84(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_maxtableindex)
					redim cachebh85(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_maxtableindex)
					redim cachebh86(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_maxtableindex)
					redim cachebh87(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_maxtableindex)
					if showmsg=1 then
						o=str(ngram_size)+"-gram caching enabled:"+lb
						o+="---------------------------------------------"+lb
						o+="Memory usage: "+format(cachebh8_nfb/1073741824,"0.00")+" GB RAM"
						ui_editbox_settext(output_text,o)
					end if
				else
					if showmsg=1 then
						o=str(ngram_size)+"-gram caching disabled: not enough free RAM"+lb
						o+="---------------------------------------------"+lb
						o+="Free memory: "+format(fre/1073741824,"0.00")+" GB RAM"+lb
						o+="Memory needed: "+format(cachebh8_nfb/1073741824,"0.00")+" GB RAM"
						ui_editbox_settext(output_text,o)
					end if
					solvesub_ngramcaching=0
				end if
			end if
	end select
	
end sub

sub stop_current_task
	
	stoptask=1
	if pausetask=1 then pause_current_task
	dim as double stucktimer=timer
	do
		sleep 10
	loop until task_active="none" or timer-stucktimer>2

end sub

sub pause_current_task
	
	if task_active<>"benchmark" then
		if pausetask=0 then
			pausetask=1
			ui_label_settext(button_main_pauseresume,"Resume")
		else
			pausetask=0
			ui_label_settext(button_main_pauseresume,"Pause")
		end if
	end if

end sub

sub get_native_dimensions
	
	dim as integer i,j
	 
	ui_listbox_resetcontent(list_dimension) 
	for i=1 to info_length
		if info_length mod i=0 then
			ui_listbox_addstring(list_dimension,str(i)+" * "+str(info_length/i))
			if i=info_x then ui_listbox_setcursel(list_dimension,j)
			j+=1
		end if
	next i
	
end sub

sub get_symbols(byval instance as byte)
	
	dim as integer i,j,k,e
	dim as integer ngramsize=symbols_ngramsize
	
	select case instance
		case 0 'symbols
			ui_listbox_resetcontent(list_symbols_ngrams)
			ui_control_setfont(list_symbols_ngrams,"courier new",18,9)
		case 1 'polyphones
			ngramsize=1
			ui_listbox_resetcontent(list_polyphones_stl)
			ui_control_setfont(list_polyphones_stl,"courier new",18,9)
	end select	
	
	dim as integer j2,cs=0
	dim as integer frq1(info_length,ngramsize,3)
	dim as integer pos1(info_length)
	'dim as integer gap1(info_length,info_length)
	dim as double tmppos
	
	for i=1 to info_length-(ngramsize-1)
		for j=1 to cs 'check if exist
			e=1
			for k=1 to ngramsize
				if frq1(j,k,1)<>nuba(i+(k-1)) then
					e=0
					exit for
				end if		
			next k
			if e=1 then exit for
		next j
		tmppos=0
		if e=0 then
			cs+=1
			for j=1 to ngramsize
				tmppos+=(i+(j-1))
				frq1(cs,j,1)=nuba(i+(j-1)) 'nuba
				frq1(cs,j,2)=info(i+(j-1)) 'symbols
			next j
			pos1(cs)+=(tmppos/ngramsize)
			frq1(cs,0,0)+=1 'frequency
			frq1(cs,0,3)=cs 'appearance
		else
			frq1(j,0,0)+=1 'frequency
			for k=1 to ngramsize
				tmppos+=(i+(k-1))
			next k
			pos1(j)+=(tmppos/ngramsize)
		end if	
	next i
	
	dim as integer h,m,mark1(info_length)
	dim as string gram
	dim as string sfr1
	dim as string nba1
	dim as string mid1
	
	for i=1 to cs
		
		h=0
		for j=1 to cs
			if mark1(j)=0 andalso frq1(j,0,0)>h then 
				h=frq1(j,0,0)
				m=j
				if ngramsize=1 andalso cpol(m)>freq(m) then cpol(m)=freq(m)
			end if
		next j
		mark1(m)=1
		gram=""
		
		for j=1 to ngramsize
			if info_numerical=0 then
				gram+=chr(frq1(m,j,2))
			else
				gram+=str(frq1(m,j,2))
				if j<>ngramsize then gram+=" "		
			end if
		next j
		
		sfr1=str(frq1(m,0,0))
		nba1=str(frq1(m,0,3))
		mid1=str(int((pos1(m)/frq1(m,0,0))-((info_length+1)/2)))
		
		select case instance
			case 0 'symbols
				gram+=":  Freq: "+sfr1+",  NBA: "+nba1 '+",  mid: "+mid1
				'if ngramsize=1 then gram+=",  letters: "+str(cpol(m)) 
				ui_listbox_addstring(list_symbols_ngrams,gram)
			case 1 'polyphones
				gram+="  ("+sfr1+")  letters: "+str(cpol(m))
				ui_listbox_addstring(list_polyphones_stl,gram)
		end select	
		
	next i
	
	'ui_editbox_settext(output_text,str(cs))

end sub

function string_to_info(byval s as string)as string
	
	'if len(s)=0 then return "Error: minimum "+str(ngram_size)+" characters"
	
	info_x=0
	info_y=0
	info_length=0
	info_numerical=0
	
	if len(s)=0 then return "Error: no input"
	
	dim as string t
	dim as integer i,l,e,c,row,rl,lmax
	dim as integer ident(identmax)
	dim as integer spaces,numbers,linebreaks,others,tabs
	
	lmax=constcip
	
	'info_x=0
	'info_length=0
	
	erase info,nuba,freq,rlen
	
	'detect numerical/ascii
	for i=1 to len(s)
		select case asc(s,i)
			case 10,13
				linebreaks+=1
			case 32
				spaces=1
			case 48 to 57
				numbers+=1
			case 9
				tabs+=1
			case else
				others+=1
		end select
	next i
	
	info_numerical=0
	if others=0 andalso numbers>0 andalso spaces>0 then info_numerical=1
	if others=0 andalso numbers>0 andalso linebreaks>0 then info_numerical=1
	
	'check number length ??
	
	if info_numerical=0 then
		for i=1 to len(s)
			select case asc(s,i)
				case 10,13
					if asc(s,i)=13 and asc(s,i+1)=10 then
						row+=1
						rlen(row)=rl
						rl=0
					end if
					if info_x=0 then info_x=l
				case else
					l+=1
					rl+=1
					if l>lmax then return "Error: maximum "+str(lmax)+" characters"
					info(l)=asc(s,i)
			end select
		next i
		if rl>0 then
			row+=1
			rlen(row)=rl
			rl=0
		end if
	else
		for i=1 to len(s)
			select case asc(s,i)
				case 48 to 57 'ascii numbers
					t+=str(asc(s,i)-48)
					if val(t)>identmax then return "Error: numbers cannot be greater than "+str(identmax)
				case 9,10,13,32 'spaces, commas, tabs, etc...
					if t<>"" then
						l+=1
						rl+=1
						if l>lmax then return "Error: maximum "+str(lmax)+" characters"
						info(l)=val(t)
						t=""
					end if
					if asc(s,i)=13 and asc(s,i+1)=10 then 'line break
						row+=1
						rlen(row)=rl
						rl=0
					end if
					if asc(s,i)=10 or asc(s,i)=13 then	 
						if info_x=0 then info_x=l
					end if
			end select
		next i
		if t<>"" then
			l+=1
			rl+=1
			info(l)=val(t)
		end if
		if rl>0 then
			row+=1
			rlen(row)=rl
			rl=0
		end if
	end if
	
	if l<ngram_size then return "Error: input too short"
	
	for i=1 to l
		if info(i)>identmax then return "Error: numbers cannot be greater than "+str(identmax)
		if ident(info(i))=0 then
			e+=1
			freq(e)=1
			'frid(e)=info(i)
			ident(info(i))=e
			nuba(i)=e
			'cpol(e)=1
		else
			c=ident(info(i))
			nuba(i)=c
			freq(c)+=1
			'cpol(e)=1
		end if
	next i
	
	solvesub_newcipher=0
	for i=1 to l
		if info(i)<>pinfo(i) then solvesub_newcipher=1
		pinfo(i)=info(i)
	next i
	
	'if solvesub_newcipher=1 then
	'	for i=1 to e 'reset poly list
	'		cpol(i)=1
	'	next i
	'end if
	
	if info_x=0 then info_x=l
	info_y=l\info_x
	if info_y<l/info_x then info_y+=1
	'if info_y=0 then info_y=1
	info_length=l
	info_symbols=e
	return "Ok"
	
end function

function info_to_string(array()as long,byval l as integer,byval dx as integer,byval dy as integer,byval numerical as integer,byval addspaces as integer,byval nolinebreaks as integer)as string

	dim as string s,s2
	dim as integer p,f,g,h,i,j,k,e,ml,ez,ez2,tz,new_j,j2
	if unispacing=1 andalso numerical=1 then
		for i=1 to l
			if array(i)<>123456789 then
				if len(str(array(i)))>ml then ml=len(str(array(i)))
			end if
		next i
		ml+=1
	end if
	
	dim as integer spacegram_size=5
	
	if numerical=0 andalso addspaces+ngram_standardalphabet>1 andalso l>=spacegram_size then
			
		dim as ubyte als(255)
		dim as ubyte tmp((spacegram_size-1)+l*2) 'l*2
		dim as ubyte sol((spacegram_size-1)+l*2) 'l*2
		dim as long frq(26)
		dim as integer nl,nl2,new_ngram_score,old_ngram_score,r1,r2,old_frq,ll
		dim as uinteger state=12345 'seed
		dim as double score,old_score,bestscore,spaces,temp,temp_min,entropy
		dim as double log2=log(2),spacegramfactor=50
		for i=65 to 90
			als(j)=i
			j+=1
		next i
		als(26)=32	
		j=0
		for i=1 to l
			tmp(i)=alpharev(array(i))
		next i
		for i=1 to 4 'wrap
			tmp(l+i)=alpharev(array(i))
		next i
		for i=1 to l
			'---------------------------------------------------------------------------------
			if i<>1 andalso g5p(tmp(i),tmp(i+1),tmp(i+2),tmp(i+3),tmp(i+4))=1 then 'add spaces
				j+=1
				sol(j)=26	
				frq(26)+=1
				'select case i
				'	case 1,l:frq(26)+=1
				'	case 2,l-1:frq(26)+=2
				'	case 3,l-2:frq(26)+=3
				'	case 4,l-3:frq(26)+=4
				'	case else:frq(26)+=5
				'end select
			end if
			'---------------------------------------------------------------------------------
			j+=1
			sol(j)=tmp(i)
			frq(sol(j))+=1
			'select case i
			'	case 1,l:frq(sol(j))+=1
			'	case 2,l-1:frq(sol(j))+=2
			'	case 3,l-2:frq(sol(j))+=3
			'	case 4,l-3:frq(sol(j))+=4
			'	case else:frq(sol(j))+=5
			'end select
		next i
		
	'' DEBUG: temporarily disable space hillclimber to test mem/performance difference
	if 1=0 then

		
		nl=j 'new length
		nl2=nl
		for i=1 to nl-4
			new_ngram_score+=g5s(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4))
		next i
		bestscore=new_ngram_score/(nl-4)*spacegramfactor 'ngram factor
		for j=0 to 26
			if frq(j)>0 then entropy+=log(frq(j)/nl)/log2*(frq(j)/nl)
		next j
		score*=abs(entropy)
		
		temp=0.1 'solvesub_matchweight 'good ???
		ll=l*solvesub_addspacesquality
		temp_min=temp/ll
		
		for i=1 to ll 'hill-climber
			k=0
			'r1=int(rnd*2) 'add/remove
			state=48271*state and 2147483647
			r1=2*state shr 31
			if frq(26)=0 then r1=0 'if no spaces exist
			'erase tmp
			old_ngram_score=new_ngram_score
			if r1=0 then 'add space
				do
					'r2=int(rnd*(nl2-2))+2 'random posistion
					state=48271*state and 2147483647
					r2=2+(nl2-2)*state shr 31
				loop until sol(r2)<>26
				for j=1 to nl2 'rewrite array
					if j=r2 then
						k+=1
						tmp(k)=26
					end if
					k+=1
					tmp(k)=sol(j)
				next j
				old_frq=frq(26)
				frq(26)+=1
				'select case r2
				'	case 1,k:frq(26)+=1
				'	case 2,k-1:frq(26)+=2
				'	case 3,k-2:frq(26)+=3
				'	case 4,k-3:frq(26)+=4
				'	case else:frq(26)+=5
				'end select
				f=r2-(spacegram_size-1)
				if f<1 then f=1
				g=r2-1
				if g>nl2-(spacegram_size-1) then g=nl2-(spacegram_size-1)	
				for j=f to g 'remove old n-grams from score
					new_ngram_score-=g5s(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
				next j
				g=r2
				if g>k-(spacegram_size-1) then g=k-(spacegram_size-1)
				for j=f to g 'add new n-grams to score
					new_ngram_score+=g5s(tmp(j),tmp(j+1),tmp(j+2),tmp(j+3),tmp(j+4))
				next j
			else 'remove space	
				do
					'r2=int(rnd*nl2)+1 'random posistion
					state=48271*state and 2147483647
					r2=2+(nl2-2)*state shr 31 'r2=1+nl2*state shr 31
				loop until sol(r2)=26
				for j=1 to nl2 'rewrite array
					if j<>r2 then
						k+=1
						tmp(k)=sol(j)
					end if
				next j
				old_frq=frq(26)
				frq(26)-=1
				'select case r2
				'	case 1,nl2:frq(26)-=1
				'	case 2,nl2-1:frq(26)-=2
				'	case 3,nl2-2:frq(26)-=3
				'	case 4,nl2-3:frq(26)-=4
				'	case else:frq(26)-=5
				'end select
				f=r2-(spacegram_size-1)
				if f<1 then f=1
				g=r2
				if g>nl2-(spacegram_size-1) then g=nl2-(spacegram_size-1)
				for j=f to g 'remove old n-grams from score
					new_ngram_score-=g5s(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
				next j
				g=r2-1
				if g>k-(spacegram_size-1) then g=k-(spacegram_size-1)
				for j=f to g 'add new n-grams to score
					new_ngram_score+=g5s(tmp(j),tmp(j+1),tmp(j+2),tmp(j+3),tmp(j+4))
				next j
			end if
			'new_ngram_score=0
			'for j=1 to k-4 'score array
			'	new_ngram_score+=g5s(tmp(j),tmp(j+1),tmp(j+2),tmp(j+3),tmp(j+4))
			'next j
			score=new_ngram_score/(k-4)*spacegramfactor '57=ngram factor
			entropy=0
			for j=0 to 26
				if frq(j)>0 then entropy+=log(frq(j)/k)/log2*(frq(j)/k)
			next j
			score*=abs(entropy)
			if score>bestscore then 'keep change
				bestscore=score
				nl2=k
				'erase sol
				for j=1 to nl2
					sol(j)=tmp(j)
				next j
			else
				temp-=temp_min
				bestscore-=temp
				frq(26)=old_frq
				new_ngram_score=old_ngram_score
			end if
		next i
		j=0
		for i=1 to nl2
			j+=1
			s+=chr(als(sol(i)))
			if sol(i)=26 andalso j>40 andalso nolinebreaks=0 then
				s+=lb
				j=0
			end if
		next i


		if solvesub_wordgrams_enabled=1 then
			
		'word n-gram score
		'-----------------------------------------------------------	
		s2=""
		for i=1 to nl2
			s2+=chr(als(sol(i)))
		next i
		s2+=" "
		dim as ulong words=0,word_array(nl2),word_score
		j=0
		for i=1 to len(s2)-1 'first char is never a space, start at 1
			if s2[i]=32 then
				words+=1
				word_array(words)=wlptr(crc_32(@s2[j],i-j))
				j=i+1
			end if
		next i
		for i=1 to words-1
			word_score+=wl2(word_array(i),word_array(i+1))
		next i
'		word_score+=wl(word_array(i),word_array(1)) 'wrap around
		s+=" "+format(word_score/(words-1),"0.00")
		'for i=1 to words-2
		'	word_score+=wl3(word_array(i),word_array(i+1),word_array(i+2))
		'next i
		's+=" "+rdc(word_score/(words-2),2)
		'-----------------------------------------------------------

		end if

	'temp disable space maker
	end if
	dim as ulong words=0

		if solvesub_wordgrams_enabled=1 then

			'word gram score addon:
			'----------------------
			's+=str(g6w(asc("B")-65,asc("E")-65,asc("C")-65,asc("A")-65,asc("U")-65,asc("S")-65))+","
			's+=str(g6w(asc("K")-65,asc("I")-65,asc("L")-65,asc("L")-65,asc("I")-65,asc("N")-65))
			
'''			s+=lb+"----------------------------------------"+lb
			dim as ushort wor(l),nwor(l)
			dim as integer nwlen=0,wngs=7,wordlen=0
			dim as ushort wordgrams2(constcip)
			if solvesub_7gwordgrams=0 then
				wngs=6
			end if	
			words=0
			
			for i=1 to l
				sol(i)=alpharev(array(i))
			next i
			words=0
'			for i=l+1 to l+(wngs-1) 'wrap around cipher to catch last word(s)
'				sol(i)=sol(i-l)
'			next i
			for i=1 to l-(wngs-1) 'copy known values 
				if solvesub_7gwordgrams=0 then
					wordgrams2(i)=g6w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
				else
					wordgrams2(i)=g7w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
				end if
			next i
			for i=1 to l 'convert sol array to words
			  if i<=l-(wngs-1) then ' don't go beyond end of array, but process all so last letters get tagged in nwlen for final check
				j=wordgrams2(i)
''				if solvesub_7gwordgrams=0 then
''					j=g6w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
''				else
''					j=g7w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
''				end if
			  else
			   j=0
			  end if
				if j>0 then
					if nwlen>0 then
						k=0
						select case nwlen
							case 1:k=g6w2(nwor(1),26,26,26,26,26):if k>0 then words+=1:wor(words)=k
							case 2:k=g6w2(nwor(1),nwor(2),26,26,26,26):if k>0 then words+=1:wor(words)=k
							case 3:k=g6w2(nwor(1),nwor(2),nwor(3),26,26,26):if k>0 then words+=1:wor(words)=k
							case 4:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),26,26):if k>0 then words+=1:wor(words)=k
							case 5:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),nwor(5),26):if k>0 then words+=1:wor(words)=k
							case 6:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),nwor(5),nwor(6)):if k>0 then words+=1:wor(words)=k
						end select
						if k=0 then
							s+="(" 'write word string
							for k=1 to nwlen
								s+=chr(nwor(k)+65)
							next k
							s+=") ":h+=nwlen+1
							if h>40 then
								h=0
								if i<l then s+=lb
							end if
							'---------------------------------------------
							wordlen+=nwlen
							nwlen=0
							words+=1
							wor(words)=0
						else
							s+="[" 'write word string
							for k=1 to nwlen
								s+=chr(nwor(k)+65)
							next k
							s+="] ":h+=nwlen+1
							if h>40 then
								h=0
								if i<>l then s+=lb
							end if
							'---------------------------------------------
							nwlen=0
						end if
					end if
					if wl(j,0)>wngs then 'if word length >6 then check word vs wordlist entry
						e=1
						if wl(j,0)>l-(i-1) then
						   e=0
						else
							for k=wngs+1 to wl(j,0) 'k=0 to wl(j,0)-1   ' can skip checking of first 6 letters if you trust the ngrams
								if wl(j,k)<>sol(i+k-1)+65 then
									e=0
									exit for
								end if
							next k
						end if
						if e=1 then ' main word works, check for longer ones first though
						   j2=j ' best found so far
						else
							j2=0
						end if
					   new_j=j ' next item being tested
					   ez=1 ' base string still matches
					   while (ez<>0)
					    new_j+=1
						 if wl(new_j,0) > wl(j2,0) then
	                  ez=1
							for tz=1 to wngs 'wl(j,0)
								if wl(new_j,tz) <> wl(j,tz) then
								   ez=0
								   exit for
								end if
							next tz
							ez2=1
							if ez=1 then ' if lower string matches
								if wl(new_j,0)>l-(i-1) then ' and checks out that cipher long enough for bigger string to fit
								   ez2=0
								else
									for tz=wngs+1 to wl(new_j,0) 'wl(j,0) to wl(new_j,0)-1 ' check if new part matches
										if wl(new_j,tz) <> sol(i+tz-1)+65 then
										   ez2=0
										   exit for
										end if
									next tz
								end if
								if ez2=1 then
									if not ( wordgrams2(i+wl(j2,0))>0 and wordgrams2(i+wl(new_j,0))=0 ) then ' don't over-extend words by stealing front of next word
										j2=new_j
									end if
								end if
							end if
						 else
						   ez=0
						 end if
						wend

						if j2 > 0 then
							words+=1
							wor(words)=j2
							i+=wl(j2,0)-1 'skip other letters of the found word
							'---------------------------------------------
							for k=1 to wl(wor(words),0) 'write word string
								s+=chr(wl(wor(words),k))
							next k
							s+=" ":h+=wl(wor(words),0)+1
							if h>40 then
								h=0
								if i<l then s+=lb
							end if
						else
							nwlen+=1
							nwor(nwlen)=sol(i)
						end if
					else
''						words+=1
'						if wl(j,0)>=l-(i-1) then 
						if wl(j,0)>l-(i-1) then 
							nwlen+=1
							nwor(nwlen)=sol(i)
''							wor(words)=0
''							i+=wl(j,0)-1 'skip other letters of the found word
							'---------------------------------------------
''							s+="(" 'write word string
''							for k=1 to wl(wor(words),0)
''								s+=chr(wl(wor(words),k))
''							next k
''							s+=") ":h+=wl(wor(words),0)+1
						else
							
					
						   ' main word works, check for longer ones first though
						   j2=j ' best found so far
						   if wl(j2,0) > 3 then
							   new_j=j ' next item being tested
							   ez=1 ' base string still matches
							   while (ez<>0)
							    new_j+=1
								 if wl(new_j,0) > wl(j2,0) then
			                  ez=1
									for tz=1 to wl(j,0)
										if wl(new_j,tz) <> wl(j,tz) then
										   ez=0
										   exit for
										end if
									next tz
									ez2=1
									if ez=1 then ' if lower string matches
										if wl(new_j,0)>l-(i-1) then ' and checks out that cipher long enough for bigger string to fit
										   ez2=0
										else
											for tz=wl(j,0)+1 to wl(new_j,0) 'wl(j,0) to wl(new_j,0)-1 ' check if new part matches
												if wl(new_j,tz) <> sol(i+tz-1)+65 then
												   ez2=0
												   exit for
												end if
											next tz
										end if
										if ez2=1 then
											if not ( wordgrams2(i+wl(j2,0))>0 and wordgrams2(i+wl(new_j,0))=0 ) then ' don't over-extend words by stealing front of next word
												j2=new_j
											end if
										end if
									end if
								 else
								   ez=0
								 end if
								wend
							end if
							
							words+=1
							wor(words)=j2
							i+=wl(j2,0)-1 'skip other letters of the found word
							'---------------------------------------------
							for k=1 to wl(wor(words),0) 'write word string
								s+=chr(wl(wor(words),k))
							next k
							s+=" ":h+=wl(wor(words),0)+1
						end if
						if h>40 then
							h=0
							if i<l then s+=lb
						end if
					end if
				else
					nwlen+=1
					nwor(nwlen)=sol(i)
				end if
			next i
			if nwlen>0 then
				k=0
				select case nwlen
					case 1:k=g6w2(nwor(1),26,26,26,26,26):if k>0 then words+=1:wor(words)=k
					case 2:k=g6w2(nwor(1),nwor(2),26,26,26,26):if k>0 then words+=1:wor(words)=k
					case 3:k=g6w2(nwor(1),nwor(2),nwor(3),26,26,26):if k>0 then words+=1:wor(words)=k
					case 4:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),26,26):if k>0 then words+=1:wor(words)=k
					case 5:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),nwor(5),26):if k>0 then words+=1:wor(words)=k
					case 6:k=g6w2(nwor(1),nwor(2),nwor(3),nwor(4),nwor(5),nwor(6)):if k>0 then words+=1:wor(words)=k
				end select
				if k=0 then
					s+="(" 'write word string
					for k=1 to nwlen
						s+=chr(nwor(k)+65)
					next k
					s+=") ":h+=nwlen+1
'					if h>40 then
'						h=0
						'if i<>l then s+=lb
'					end if
					'---------------------------------------------
					wordlen+=nwlen
					nwlen=0
					words+=1
					wor(words)=0
				else
'					s+="[" 'write word string '' don't need to add brackets on last word since by design often needs this step
					for k=1 to nwlen
						s+=chr(nwor(k)+65)
					next k
					s+=" "
'					s+="] ":h+=nwlen+1
'					if h>40 then
'						h=0
'						'if i<>l then s+=lb
'					end if
					'---------------------------------------------
					nwlen=0
				end if
			end if
			
			if words>1 then
				dim as double wscore=0
				dim as double wscore_current=0
				dim as double wscore2=0
				dim as double wscore3=0
				dim as ushort w31,w32,w33
				dim as integer valid_triples=0
				
			s+=lb+"----------------------------------------"+lb
			
				for i=1 to words-1 'score words with word 2-grams
					wscore_current=wl2(wor(i),wor(i+1))
					wscore+=wscore_current
					' punish common "stop words" so wordgrams don't select too hard for them
					' since they are usually short and over-selected for by n-grams already
					' similar resistance factor to entropy for 
	'				if wl(wor(i),0) < 3 then
	'					wscore2*=0.9
	'				end if
	'				if wl(wor(i+1),0) < 3 then
	'					wscore2*=0.9
	'				end if
					if (wscore_current>0 and wscore2>0) then
						w31=wl13(wor(i-1))
						s+=str(i-1)+":"+str(wl13(wor(i-1)))+" "
						if (w31 > 0) then
							w32=wl13(wor(i))
							if (w32 > 0) then
								w33=wl13(wor(i+1))
								if (w33 > 0) then
									valid_triples+=1
									wscore3+=wl3(w31,w32,w33)
								end if	
							end if
						end if
					end if	
					wscore2=wscore_current  ' save trailing wordpair score
				next i
				if wscore>0 then
					wscore/=(words-1 + wordlen/6.0)   ' beijinghouse last term punishes unassigned "wordless" letters
					wscore/=4.0   							  ' beijinghouse only score triples in top 2k most common words
					if valid_triples>0 then wscore+=(wscore3/valid_triples)/4.0 ' but don't punish words for not being in this set
					s+=lb+format(wscore,"0.00")			 ' just discouraging repeativite stop-word strings like "IS THIS IS"
				end if
			end if


			s+=lb+"----------------------------------------"+lb

			' DUMB DEBUG CODE to peek at all the ngram word codes
			for i=1 to l 'convert sol array to words
			  if i<l-wngs then ' don't go beyond end of array, but process all so last letters get tagged in nwlen for final check
				if solvesub_7gwordgrams=0 then
					j=g6w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
				else
					j=g7w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
				end if
			  else
			   j=0
			  end if
				if j>0 then
					s+=str(i)+":"+str(j)+" "
				end if
			next i

		end if


	
		return s
		
	end if
	
	for i=1 to l
		if array(i)>0 then
			if numerical=0 then
				'if addspaces>1 andalso ngram_standardalphabet=1 then
				'else
					s+=chr(array(i))
					if i mod dx=0 andalso i<>l andalso nolinebreaks=0 then s+=lb	
				'end if
			else
				if array(i)=123456789 then
					's+=space(ml)
				else
					s+=str(array(i))
				end if
				if i mod dx=0 then
					if i<>l andalso nolinebreaks=0 then s+=lb
				else
					if unispacing=0 then
						if i<>l then s+=" "
					else
						if array(i)=123456789 then
							if i<>l then s+=space(ml)
						else				
							if i<>l then s+=space(ml-len(str(array(i))))
						end if
					end if
				end if
			end if
		end if
	next i
	return s
	
end function

sub quicksort_sort2(byval low as integer,byval high as integer)
	
	dim as integer i,j,k,pivot
	pivot=sort2((low+high)/2,0)
	i=low
	j=high
	do while i<=j
		do while i<high and sort2(i,0)<pivot
			i+=1
		loop
		do while j>low and pivot<sort2(j,0)
			j-=1
		loop
		if i<=j then 
			for k=0 to 4
				swap sort2(i,k),sort2(j,k)
			next k
			i+=1
			j-=1
		end if
	loop
	if low<j then quicksort_sort2(low,j)
	if i<high then quicksort_sort2(i,high)

end sub

sub quicksort_short(array()as short,byval low as integer,byval high as integer)
	
	dim as integer i,j,k,pivot
	pivot=array((low+high)/2)
	i=low
	j=high
	do while i<=j
		do while i<high and array(i)<pivot
			i+=1
		loop
		do while j>low and pivot<array(j)
			j-=1
		loop
		if i<=j then 
			swap array(i),array(j)
			i+=1
			j-=1
		end if
	loop
	if low<j then quicksort_short(array(),low,j)
	if i<high then quicksort_short(array(),i,high)

end sub

function frequencies(array()as long,byval l as integer,byval s as integer,byval ngs as integer,byval num as integer,byval mf as integer)as string
	
	dim as string rs
	dim as integer i,j,k,n,m,e,f,counted
	dim as integer cip(l,1)
	dim as integer frq(l,ngs)
	
	for i=1 to l
		cip(i,0)=array(i)
	next i
	
	for i=1 to l-(ngs-1)		
		m=0
		for k=0 to (ngs-1) 'check marked
			if cip(i+k,1)=1 then m+=1
		next k	
		if m<ngs then 			
			f+=1
			frq(f,0)=1 'count
			for k=1 to ngs
				frq(f,k)=cip(i+(k-1),0)
			next k		
			for j=i+1 to l-(ngs-1)
				e=1
				for k=0 to (ngs-1) 'check for repeats
					if cip(i+k,0)<>cip(j+k,0) then
						e=0
						exit for	
					end if
				next k		
				if e=1 then
					frq(f,0)+=1 'count
					if frq(f,0)>1 then counted=1
					for k=0 to (ngs-1)
						cip(j+k,1)=1 'mark
					next k
				end if					
			next j				
		end if		
	next i
	
	if counted=1 or ngs=1 then
		rs+=str(ngs)+"-gram frequencies > "+str(mf)+":"+lb
		rs+="---------------------------------------------------------"
		do 'swapsort
			e=0
			for i=1 to f-1
				if frq(i,0)<frq(i+1,0) then
					for j=0 to ngs
						swap frq(i,j),frq(i+1,j)
						e=1
					next j
				end if	
			next i
		loop until e=0
		for i=1 to f
			if frq(i,0)>mf then
				rs+=lb
				if num=0 then
					for j=1 to ngs
						rs+=chr(frq(i,j))
					next j
				else
					for j=1 to ngs
						if j<>ngs then
							rs+=str(frq(i,j))+" "
						else
							rs+=str(frq(i,j))
						end if
					next j	
				end if
				rs+=": "+str(frq(i,0))
				'if i<>f then rs+=lb
			end if
		next i
		return rs
	end if

end function

sub thread_load_ngrams(byval none as any ptr)
	
	dim as integer a,b,e,h,i,j,k,l,m,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,nm1,old
	dim as ubyte ub
	dim as ulongint h0,h1,h2,h3,curr_items,total_items
	dim as integer abortload,thread_loadngrams_showmsg
	dim as integer nfb,nfb2,filelen1,bytecode_size
	dim as double newtemp,loadngramtimer,start_time=timer
	dim as string file_name_ngrams=solvesub_ngramloctemp
	dim as string s,num,ot,file_name_ngramsini,table_file,first_file,last_file,pos_file,pos2_file,space_file
	dim as gzfile gzf
	
	if fileexists(file_name_ngrams)=0 then 'check if file exist
		if file_name_ngrams<>"" then ui_editbox_settext(output_text,"Error: file not found")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	if instr(file_name_ngrams,".txt")>0 then
		file_name_ngramsini=left(file_name_ngrams,instr(file_name_ngrams,".txt"))+"ini"
	else ' handle both .gz and .gov files
		if instr(file_name_ngrams,".g")>0 then file_name_ngramsini=left(file_name_ngrams,instr(file_name_ngrams,".g"))+"ini"
	end if
	
	if fileexists(file_name_ngramsini)=0 then 'check if ini file exist
		ui_editbox_settext(output_text,"Error: .ini file not found")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	dim as integer old_ngram_mem=ngram_mem
	dim as integer old_ngram_size=ngram_size
	dim as double old_ngramfactor=solvesub_ngramfactor
	dim as double old_entweight=solvesub_entweight
	dim as integer old_ngram_alphabet_size=ngram_alphabet_size
	dim as double old_temp=solvesub_temperature
	dim as short old_alphabet(255),old_alpharev(255)
	
	for i=0 to 255
		old_alphabet(i)=alphabet(i)
		old_alpharev(i)=alpharev(i)
	next i
	
	open file_name_ngramsini for binary as #1
	
	line input #1,s 'get ngram_size
	a=instr(s,"=")
	
	ngram_size=0
	fileformat=0
	ngram_format="text"
	s=lcase(s)
	
	'branch off special n-gram systems
	'------------------------------------------------------------------------
	ngram_size=val(right(s,len(s)-a))
	if instr(s,"old")>0 then 'convert 2-byte log to 1-byte log
		old=1
		fileformat=2
		ngram_size=val(right(s,len(s)-(instr(s,"old")+2)))
	end if
	if instr(s,"b")>0 then 'binary format
		fileformat=1
		ngram_format="binary"
		ngram_size=val(right(s,len(s)-(instr(s,"b")+0)))
	end if
	if instr(s,"t")>0 then 'text format
		fileformat=2
		ngram_format="text"
		ngram_size=val(right(s,len(s)-(instr(s,"t")+0)))
	end if
'	if instr(s,"hm")>0 then 'hashmap format
'		fileformat=-2
'		ngram_format="hashmap"
'		ngram_size=val(right(s,len(s)-(instr(s,"hm")+1)))
'	end if
	if instr(s,"bh")>0 then 'beijinghouse
		ngram_size=val(right(s,len(s)-(instr(s,"bh")+1)))
	end if
	if instr(s,"gov")>0 then 'beijinghouse gov format
		select case right(s,4)
			case "usf3"
				fileformat=5
				ngram_format="3-hypergraph static function"
				s=left(s,len(s)-4)
			case "usf4"
				fileformat=6
				ngram_format="4-hypergraph static function"
				s=left(s,len(s)-4)
			case "csf3"
				fileformat=3
				ngram_format="compressed 3-hypergraph static function"
				s=left(s,len(s)-4)
			case "csf4"
				fileformat=4
				ngram_format="compressed 4-hypergraph static function"
				s=left(s,len(s)-4)
			case else:abortload=1
		end select
		ngram_size=val(right(s,len(s)-(instr(s,"gov")+2)))
		select case ngram_size 'allowed n-gram sizes/formats
			case 2,3,4,5,6,7,8,9,10,11,12,13,14
			case else:abortload=1
		end select
	else
		select case ngram_size
			case 2,3,4,5,7,6,9
			case 8 ',10
				fileformat=1
				ngram_format="binary"
				table_file=left(file_name_ngrams,instr(file_name_ngrams,".txt")-1)+"_table.txt"
					if fileexists(table_file)=0 then
						table_file=left(file_name_ngrams,instr(file_name_ngrams,".txt")-1)+"_table.txt.zst"
						if fileexists(table_file)=0 then
							table_file=left(file_name_ngrams,instr(file_name_ngrams,".txt")-1)+"_table.zst"
							if fileexists(table_file)=0 then
								table_file=left(file_name_ngrams,instr(file_name_ngrams,".txt")-1)+"_table.txt.gz"
								if fileexists(table_file)=0 then
									table_file=left(file_name_ngrams,instr(file_name_ngrams,".txt")-1)+"_table.gz"
										if fileexists(table_file)=0 then abortload=2
								end if
							end if
						end if
					end if
'				first_file=left(file_name_ngrams,instr(file_name_ngrams,".txt")-1)+"_first.txt"
'					if fileexists(table_file)=0 then
'						first_file=left(file_name_ngrams,instr(file_name_ngrams,".txt")-1)+"_first.txt.zst"
'						if fileexists(table_file)=0 then
'							first_file="" ' no firstgram file found
'						end if
'					end if
'				last_file=left(file_name_ngrams,instr(file_name_ngrams,".txt")-1)+"_last.txt"
'					if fileexists(table_file)=0 then
'						last_file=left(file_name_ngrams,instr(file_name_ngrams,".txt")-1)+"_last.txt.zst"
'						if fileexists(table_file)=0 then
'							last_file="" ' no lastgram file found
'						end if
'					end if
			case else:abortload=1
		end select
	end if
	
	if abortload>0 then
		close #1
		ngram_size=old_ngram_size
		select case abortload
			case 1:ui_editbox_settext(output_text,"Error: unsupported n-gram size for this n-gram system")
			case 2:ui_editbox_settext(output_text,"Error: beijinghouse n-gram system table file not found")
		end select
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	line input #1,s 'get ngram_factor
	a=instr(s,"=")
	if a=0 then 
		close #1
		ngram_size=old_ngram_size
		solvesub_ngramfactor=old_ngramfactor
		ui_editbox_settext(output_text,"Error: n-gram factor")
		task_active="none"
		update_solver_status
		exit sub
	end if
	solvesub_ngramfactor=val(right(s,len(s)-a))
	if old=1 then solvesub_ngramfactor*=10
	s=ui_listbox_gettext(list_optionssolver,7)
	s=left(s,instr(s,":")-1)
	ui_listbox_replacestring(list_optionssolver,7,s+": "+str(solvesub_ngramfactor)) 'update solver options window
	
	line input #1,s 'get entweight/iocweight
	a=instr(s,"=")
	if a<=0 then 
		close #1
		ngram_size=old_ngram_size
		solvesub_ngramfactor=old_ngramfactor
		solvesub_entweight=old_entweight
		ui_editbox_settext(output_text,"Error: entropy weight")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	if instr(lcase(s),"ioc")>0 then 'convert iocweight to entropy for backwards compatibility
		solvesub_fastent=ngram_size-1
		if solvesub_fastent>4 then solvesub_fastent=4
		select case solvesub_fastent
			case 1:solvesub_entweight=0.25
			case 2:solvesub_entweight=0.5
			case 3:solvesub_entweight=0.75
			case else:solvesub_entweight=1
		end select
		solvesub_ngramfactor/=6.103
		s=ui_listbox_gettext(list_optionssolver,7)
		s=left(s,instr(s,":")-1)
		ui_listbox_replacestring(list_optionssolver,7,s+": "+format(solvesub_ngramfactor,"0.00000")) 'update solver options window
	else
		solvesub_entweight=val(right(s,len(s)-a))
		select case solvesub_entweight
			case 0.25:solvesub_fastent=1
			case 0.5:solvesub_fastent=2
			case 0.75:solvesub_fastent=3
			case 1:solvesub_fastent=4
			case 1.5:solvesub_fastent=5
			case 2:solvesub_fastent=6
			case else:solvesub_fastent=0
		end select
	end if
	s=ui_listbox_gettext(list_optionssolver,2)
	s=left(s,instr(s,":")-1)
	ui_listbox_replacestring(list_optionssolver,2,s+": "+str(solvesub_entweight)) 'update solver options window
	
	line input #1,s 'get ngram_alphabet
	a=instr(s,"=")
	if a=0 then
		close #1
		ngram_size=old_ngram_size
		solvesub_ngramfactor=old_ngramfactor
		solvesub_entweight=old_entweight
		ui_editbox_settext(output_text,"Error: alphabet") 'if loadngrams_showmsg=1 then 
		task_active="none"
		update_solver_status
		exit sub
	end if
	if right(s,len(s)-a)="ABCDEFGHIJKLMNOPQRSTUVWXYZ" then ngram_standardalphabet=1 else ngram_standardalphabet=0
	ngram_alphabet_size=0
	dim as ubyte alpharevp1(255)
	for i=a+1 to len(s)
		ngram_alphabet_size+=1
		alphabet(j)=asc(s,i)
		alpharev(asc(s,i))=j
		alpharevp1(asc(s,i))=j+1 'plus 1
		j+=1
	next i
	nm1=j-1
	
	if fileformat=0 then 'auto detect n-gram format
		fileformat=2
		ngram_format="text"
		gzf=gzopen(file_name_ngrams,"rb")
		dim ubp as ubyte ptr
		ubp=allocate(1)
		for i=1 to ngram_size 'read first n-gram size bytes and check if alphabet letters
			gzread(gzf,ubp,1)
			if alpharevp1(ubp[0])=0 then
				ngram_format="binary"
				fileformat=1
			end if
		next i
		gzclose(gzf)
	end if

	task_active="loading "+str(ngram_size)+"-grams"
	if loadngrams_showmsg=1 then 
		ui_editbox_settext(output_text,"Please wait...")
		update_solver_status
	end if
	
	line input #1,s 'get n-gram temp
	a=instr(s,"=")
	if a>0 then
		solvesub_temperature=val(right(s,len(s)-a))
		newtemp=solvesub_temperature
	end if
	if solvesub_temperature<=0 then solvesub_temperature=700
	s=ui_listbox_gettext(list_optionssolver,17)
	s=left(s,instr(s,":")-1)
	ui_listbox_replacestring(list_optionssolver,17,s+": "+str(solvesub_temperature))
	
	line input #1,s 'get items
	a=instr(s,"=")
	if a>0 then
		total_items=val(right(s,len(s)-a))
	end if
	
	close #1
	
	if ngrams_clearprevious=1 then 'clear all
		'------------------------------------------------------------------------
		for i=2 to 8				' beijinghouse: purposely not clearing 9-grams for testing
			ngrams_inmem(i)=0
		next i
		'------------------------------------------------------------------------
		redim g2(0,0),g2b(0,0) 'default
		redim g3(0,0,0),g3b(0,0,0)
		redim g4(0,0,0,0),g4b(0,0,0,0)
		redim g5(0,0,0,0,0),g5b(0,0,0,0,0)
		redim g6(0,0,0,0,0,0),g6b(0,0,0,0,0,0)
		redim g7(0,0,0,0,0,0,0),g7b(0,0,0,0,0,0,0)
		'------------------------------------------------------------------------
		redim bh8(0,0),bh4(0,0,0,0) 'beijinghouse
		'redim bh8f(0,0),bh8l(0,0) 'beijinghouse
		'redim bh8s(0,0),bh8sl(0,0) 'beijinghouse
		'redim bh10(0,0),bh5(0,0,0,0,0)
		redim cachebh80(0,0,0,0)
		redim cachebh81(0,0,0,0)
		redim cachebh82(0,0,0,0)
		redim cachebh83(0,0,0,0)
		redim cachebh84(0,0,0,0)
		redim cachebh85(0,0,0,0)
		redim cachebh86(0,0,0,0)
		redim cachebh87(0,0,0,0)
		'------------------------------------------------------------------------
'		redim g53(0,0,0,0,0) 'beijinghouse 9-grams
'		redim g54(0,0,0,0,0)			' beijinghouse: purposely not clearing 9-grams for testing
'		redim gxa9(0,0)
'		redim gxb9(0,0)
'		redim gxc9(0,0)
'		redim gxd9(0,0)
'		redim gxe9(0,0)
'		redim gxf9(0,0)
'		redim gxg9(0,0)
'		redim gxh9(0,0)
'		redim gxi9(0,0)
'		redim gxj9(0,0)
'		redim gxk9(0,0)
'		redim gxl9(0,0)
'		redim gxm9(0,0)
'		redim gxn9(0,0)
'		redim gxo9(0,0)
'		redim gxp9(0,0)
'		redim gxq9(0,0)
'		redim gxr9(0,0)
'		redim gxs9(0,0)
'		redim gxt9(0,0)
'		redim gxu9(0,0)
'		redim gxv9(0,0)
'		redim gxw9(0,0)
'		redim gxx9(0,0)
'		redim gxy9(0,0)
'		redim gxz9(0,0)
		'------------------------------------------------------------------------
		if (gov_offset_and_seed<>null) then deallocate(gov_offset_and_seed) ' beijinghouse gov
		if (gov_array<>null) then deallocate(gov_array)

		if (gov_symbol<>null) then deallocate(gov_symbol) ' beijinghouse gov (compressed)
		if (gov_last_codeword_plus_one<>null) then deallocate(gov_last_codeword_plus_one)
		if (gov_how_many_up_to_block<>null) then deallocate(gov_how_many_up_to_block) ' beijinghouse gov
		if (gov_shift<>null) then deallocate(gov_shift)
		
		redim g3keyguard(0,0,0)
		redim g4keyguard(0,0,0,0)
		redim g5keyguard(0,0,0,0,0)
	end if
	
	ngram_file_size = filelen(file_name_ngrams)
	
	if fileformat > 2 then ' gov 
		nfb=filelen(file_name_ngrams)
	else
		select case ngram_size 'calculate required memory
			case 2 to 7 'default	
				nfb=(ngram_alphabet_size^ngram_size)+(ngram_size-1)*(ngram_alphabet_size^(ngram_size-1))
				if total_items=0 andalso fileformat=1 then 
					total_items=ngram_alphabet_size^ngram_size
				else
					total_items=filelen(file_name_ngrams)
				end if
			case 8,10 'beijnghouse
				if memcheck=1 then
					i=fre-(1073741824*2) '2 GB RAM
					j=fre*0.8 'for low memory systems
					max_allowed_table_index=sqr(iif(i>j,i,j))
				else
					max_allowed_table_index=sqr(26^ngram_size)
				end if
				if (max_allowed_table_index*max_allowed_table_index)>(1073741824*solvesub_bhmaxgb) then max_allowed_table_index=sqr(1073741824*solvesub_bhmaxgb)
		end select
	end if

	' correct loading progress by getting true streamed file size of gziped text files
	If instr(file_name_ngrams,".gz") andalso fileformat=2 Then
		filelen1=filelen(file_name_ngrams)

		open file_name_ngrams for binary access read as #1
		get #1, filelen1-4+1, *filetail, 4   ' grab last 4 bytes off gzip file to recover the original file size
		close #1
		bytecode_size = filetail[0] + filetail[1] Shl 8 + filetail[2] Shl 16 + filetail[3] Shl 24	
			' new file size table
			' very accurate now!
			' it's using precise pigz -9 cutoffs
			' for exact cutoff sized beijinghouse encoded files
			'
			' pigz -11 cutoffs in tailing comments
			' may use for higher cutoffs eventually
			' since that's how beijinghouse compresses larger files in practice
			select case filelen1
				case is>20679447534:filelen1 = bytecode_size + 16*2^32 '64GB+ uncompressed (extrapolated)
				case is>19403148052:filelen1 = bytecode_size + 15*2^32 '60GB+ uncompressed (extrapolated)
				case is>18126848570:filelen1 = bytecode_size + 14*2^32 '56GB+ uncompressed (extrapolated)
				case is>16850549088:filelen1 = bytecode_size + 13*2^32 '52GB+ uncompressed (extrapolated)
				case is>15574249606:filelen1 = bytecode_size + 12*2^32 '48GB+ uncompressed (extrapolated)
				case is>14297950124:filelen1 = bytecode_size + 11*2^32 '44GB+ uncompressed ' 13794005440
				case is>13021650642:filelen1 = bytecode_size + 10*2^32 '40GB+ uncompressed ' 12556442790
				case is>11725023762:filelen1 = bytecode_size +  9*2^32 '36GB+ uncompressed ' 11315945185
				case is>10446514566:filelen1 = bytecode_size +  8*2^32 '32GB+ uncompressed ' 10077073168
				case is> 9149966667:filelen1 = bytecode_size +  7*2^32 '28GB+ uncompressed ' 8827460215
				case is> 7823921226:filelen1 = bytecode_size +  6*2^32 '24GB+ uncompressed ' 7550156516
				case is> 6511839228:filelen1 = bytecode_size +  5*2^32 '20GB+ uncompressed ' 6279223958
				case is> 5171187771:filelen1 = bytecode_size +  4*2^32 '16GB+ uncompressed ' 4989043243
				case is> 3725972775:filelen1 = bytecode_size +  3*2^32 '12GB+ uncompressed ' 3602529616
				case is> 2590853796:filelen1 = bytecode_size +  2*2^32 '08GB+ uncompressed ' 2502583929
				case is> 1302625091:filelen1 = bytecode_size +    2^32 '04GB+ uncompressed ' 1255766450
				case else:filelen1 = bytecode_size				
			end select
			total_items=filelen1
	elseif instr(file_name_ngrams,".zst") andalso fileformat=2 then
		filelen1=filelen(file_name_ngrams) / 3.65 ' can only estimate since zstd has no total size info
	Else
		filelen1=filelen(file_name_ngrams)
	End if

	if memcheck=1 andalso nfb>=fre then 'memory check
		ot="Error: not enough free RAM to load n-grams"+lb
		ot+="---------------------------------------------------------"+lb
		ot+="Free memory: "+format(fre/1073741824,"0.00")+" GB RAM"+lb
		ot+="Memory needed: "+format(nfb/1073741824,"0.00")+" GB RAM"
		ui_editbox_settext(output_text,ot)
		solver_file_name_ngrams=""
		task_active="none"
		update_solver_status
		exit sub
		'add rollback to previous n-grams ???
	end if
	
	ngram_mem=nfb
	solvesub_ngramloc=solvesub_ngramloctemp
	
	'gz variables
	'-----------------------------
	dim fd as ubyte ptr 'file data
	dim fs as ushort ptr 'file data
	dim as integer buffer=1050600*4 ' beijinghouse 4MB buffer = 10% faster reading vs 1MB
	dim as integer bytesread=buffer
	dim as integer totalbytes=buffer
	dim as integer bl=buffer
	dim as integer extra_chars,last_read
	fd=allocate(buffer)
 	
	ngram_count=0
	count3=0
	count4=0
	count5=0
	highgram=0
	
	dim as ubyte ptr ld
	dim as integer ic
	dim as ubyte ptr pi
	dim as ushort ptr ps
	ngram_maxtableindex=0
	ngram_lowval=999
	ngram_highval=0
	erase ngram_values

	solvesub_bonusgrams_enabled=1
	solvesub_wordgrams_enabled=1
	solvesub_7gwordgrams=0

	' TODO: eventually use uncompressed binary with mem map for instant load
	if firststart=1 and solvesub_bonusgrams_enabled=1 then

		redim fg6(25,25,25,25,25,25)
		redim sg6(25,25,25,25,25,25)
		redim slg6(25,25,25,25,25,25)
		redim lg6(25,25,25,25,25,25)

		solver_file_name_ngrams="Bonus Grams [1.18GB mem]"
		ot=str(solver_file_name_ngrams)+lb
		ot+="--------------------------------------------------------"+lb
		ot+="Loading progress: 0.0%"
		if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)

		gzf=gzopen(basedir+"\N-grams\Positional\6-firstgramsb40.txt.zst","rb")
		pi=@fg6(0,0,0,0,0,0)
		gzread(gzf, pi, 26^6)
		gzclose(gzf)

		solver_file_name_ngrams="Bonus Grams [1.2GB mem]"
		ot=str(solver_file_name_ngrams)+lb
		ot+="--------------------------------------------------------"+lb
		ot+="Loading progress: 25.0%"
		if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)

		gzf=gzopen(basedir+"\N-grams\Positional\6-secondgramsb40.txt.zst","rb")
		pi=@sg6(0,0,0,0,0,0)
		gzread(gzf, pi, 26^6)
		gzclose(gzf)

		solver_file_name_ngrams="Bonus Grams [1.2GB mem]"
		ot=str(solver_file_name_ngrams)+lb
		ot+="--------------------------------------------------------"+lb
		ot+="Loading progress: 50.0%"
		if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)

		gzf=gzopen(basedir+"\N-grams\Positional\6-secondlastgramsb40.txt.zst","rb")
		pi=@slg6(0,0,0,0,0,0)
		gzread(gzf, pi, 26^6)
		gzclose(gzf)

		solver_file_name_ngrams="Bonus Grams [1.2GB mem]"
		ot=str(solver_file_name_ngrams)+lb
		ot+="--------------------------------------------------------"+lb
		ot+="Loading progress: 75.0%"
		if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)

		gzf=gzopen(basedir+"\N-grams\Positional\6-lastgramsb40.txt.zst","rb")
		pi=@lg6(0,0,0,0,0,0)
		gzread(gzf, pi, 26^6)
		gzclose(gzf)

		solver_file_name_ngrams="Bonus Grams [1.2GB mem]"
		ot=str(solver_file_name_ngrams)+lb
		ot+="--------------------------------------------------------"+lb
		ot+="Loading progress: 100.0%"
		if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)

	end if
	
	'word n-grams 
	'----------------------------------------------------------------------------------
	if firststart=1 and solvesub_wordgrams_enabled=1 then

		dim as integer wmax=65535
		redim wl(wmax,50)
		redim wl13(wmax)
'''		redim wlptr(2^32)

		if solvesub_7gwordgrams=0 then
''			open basedir+"\N-grams\Words\1-word_grams_english_beijinghouse_google_64k.txt" for binary as #1
			open basedir+"\N-grams\Words\1-word_grams_english_beijinghouse_google_doubleclean2_64k.txt" for binary as #1
		else
			open basedir+"\N-grams\Words\1-word_grams_english_beijinghouse_google_clean_64k.txt" for binary as #1
		end if
		i=0
		h=0 ' wordlist 1 to 3 translation counter
		do
			line input #1,s
'			s=ucase(s) 'convert to uppercase
			if s<>"" then
				i+=1
				num=""
				for j=0 to len(s)-1
					select case s[j]
						case 65 to 90 'uppercase letter
							wl(i,j+1)=s[j]
						case 32 'space
							wl(i,0)=j 'word length
							num=right(s,len(s)-j-1)
							exit for
					end select
				next j
				wl(i,50)=log(val(num))*10
'				if (val(num)>15280000.0) then ' for the 2k 3-gram file; tied to 1-word_grams_english_beijinghouse_google_doubleclean2_64k.txt for now; should be variable
				if (val(num)>9846800.0) then ' for the 3k 3-gram file; tied to 1-word_grams_english_beijinghouse_google_doubleclean2_64k.txt for now; should be variable
'				if (val(num)>8355000.0) then ' for the 3.4k 3-gram file; tied to 1-word_grams_english_beijinghouse_google_doubleclean2_64k.txt for now; should be variable
					h+=1
					wl13(i)=h
				else
					wl13(i)=0
				end if
			end if
		loop until eof(1) or i=wmax
		close #1
		
		' add special re-check table for 1-6 letter words
		redim g6w2(26,26,26,26,26,26)
		for i=1 to 65535 'top XXXXX words
			select case wl(i,0)
				case 1:g6w2(wl(i,1)-65,26,26,26,26,26)=i
				case 2:g6w2(wl(i,1)-65,wl(i,2)-65,26,26,26,26)=i
				case 3:g6w2(wl(i,1)-65,wl(i,2)-65,wl(i,3)-65,26,26,26)=i
				case 4:g6w2(wl(i,1)-65,wl(i,2)-65,wl(i,3)-65,wl(i,4)-65,26,26)=i
				case 5:g6w2(wl(i,1)-65,wl(i,2)-65,wl(i,3)-65,wl(i,4)-65,wl(i,5)-65,26)=i
				case 6:g6w2(wl(i,1)-65,wl(i,2)-65,wl(i,3)-65,wl(i,4)-65,wl(i,5)-65,wl(i,6)-65)=i
				case else
			end select
		next i
		
		redim wl2(wmax,wmax)
		pi=@wl2(0,0)
		loadngramtimer=timer
		if solvesub_7gwordgrams=0 then
''			gzf=gzopen(basedir+"\N-grams\Words\2-word_grams_english_beijinghouse_google_64k.txt.zst","rb")
			gzf=gzopen(basedir+"\N-grams\Words\2-word_grams_english_beijinghouse_google_doubleclean2_64k.txt","rb")
		else
			gzf=gzopen(basedir+"\N-grams\Words\2-word_grams_english_beijinghouse_google_clean_64k.txt.zst","rb")
		end if
		solver_file_name_ngrams="2-wordgram File [4GB mem]"

		gzread( gzf, pi, 2 )
		ot=str(solver_file_name_ngrams)+lb
		ot+="--------------------------------------------------------"+lb
		ot+="Loading progress: 33.0%"
		if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)

		pi=(@wl2(0,0))+2		
		gzread( gzf, pi, (wmax+1)^2/2-1 )
		ot=str(solver_file_name_ngrams)+lb
		ot+="--------------------------------------------------------"+lb
		ot+="Loading progress: 66.0%"
		if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)
		
		pi=(@wl2(0,0))+2147483649
		gzread( gzf, pi, (wmax+1)^2/2-1 )
					
''		total_items=((wmax+1)^2)-1
''		for i=0 to ((wmax+1)^2)-1
''			if k=0 then
''				bl=0
''				k=buffer
''				curr_items=i
''				#include "ngram_loading_progress.bi"
''				gzread(gzf,fd,buffer)
''			end if
''			k-=1 'byte
''			j=fd[bl]
''			bl+=1
''			pi[i]=j
''		next i
		gzclose(gzf)

		for i=0 to wmax  ' TODO: shouldn't need eventually but currently storing values in these images sometimes but don't want now
			wl2(0,i)=0
			wl2(i,0)=0
		next i


		loadngramtimer=timer
''		redim wl3(2000,2000,2000) ' 8GB 2k word 3-wordgram 
''		gzf=gzopen(basedir+"\N-grams\Words\3-word_grams_english_beijinghouse_google_doubleclean2_2k.txt","rb")
''		solver_file_name_ngrams="3-wordgram File [7.46GB mem]"

''		for i=0 to 26	' load in 27 chunks since > 2GB loads impossible with gzread
''			pi=@wl3(0,0,0)+i*296740963
''			gzread( gzf, pi, 296740963 )
''			ot=str(solver_file_name_ngrams)+lb
''			ot+="--------------------------------------------------------"+lb
''			ot+="Loading progress: "+format(100*(i+1)/27,"0.00")+"%"
''			if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)
''		next i

		redim wl3(3000,3000,3000) ' 27GB 2k word 3-wordgram 
		gzf=gzopen(basedir+"\N-grams\Words\3-word_grams_english_beijinghouse_google_doubleclean2_3k.txt","rb")
		solver_file_name_ngrams="3-wordgram File [25.2GB mem]"

		for i=0 to 3000	' load in 3000 chunks since > 2GB loads impossible with gzread
			pi=@wl3(0,0,0)+i*9006001
			gzread( gzf, pi, 9006001 )
			ot=str(solver_file_name_ngrams)+lb
			ot+="--------------------------------------------------------"+lb
			ot+="Loading progress: "+format(100*(i+1)/3001,"0.00")+"%"
			if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)
		next i


'		redim wl3(3400,3400,3400) ' 27GB 2k word 3-wordgram 
'		gzf=gzopen(basedir+"\N-grams\Words\3-word_grams_english_beijinghouse_google_doubleclean2_34k.txt","rb")
'		solver_file_name_ngrams="3-wordgram File [36.6GB mem]"
'
'		for i=0 to 18	' load in 19 chunks since > 2GB loads impossible with gzread
'			pi=@wl3(0,0,0)+i*2070457379
'			gzread( gzf, pi, 2070457379 )
'			ot=str(solver_file_name_ngrams)+lb
'			ot+="--------------------------------------------------------"+lb
'			ot+="Loading progress: "+format(100*(i+1)/19,"0.00")+"%"
'			if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)
'		next i

		
		gzclose(gzf)

		if solvesub_7gwordgrams=0 then
			redim g6w(25,25,25,25,25,25)
			ps=@g6w(0,0,0,0,0,0)
'			gzf=gzopen(basedir+"\N-grams\Words\6gram-to-1word-5M-line-reddit2020-beijinghouse1_clean_test1.txt.zst","rb")

''			gzf=gzopen(basedir+"\N-grams\Words\6gram-to-1word-5M-line-reddit2020-beijinghouse1test1.txt.zst","rb")
'			gzf=gzopen(basedir+"\N-grams\Words\6gram-to-1word-160M-line-reddit2010-beijinghouse_doubleclean2_test2.txt","rb")
'			gzf=gzopen(basedir+"\N-grams\Words\6gram-to-1word-64M-line-reddit2009-beijinghouse_doubleclean2_test3.txt","rb")
			gzf=gzopen(basedir+"\N-grams\Words\6gram-to-1word-64M-line-reddit2009-beijinghouse_doubleclean2_test5.txt","rb")

'			gzf=gzopen(basedir+"\N-grams\Words\6gram-to-1word-600M-line-reddit2020-beijinghouse1_clean_test2.txt.zst","rb")
			k=0
			solver_file_name_ngrams="Word Discrimination File [589MB mem]"
			ot=str(solver_file_name_ngrams)+lb
			ot+="--------------------------------------------------------"+lb
			ot+="Loading progress: 50.0%"
			if loadngrams_showmsg=1 then ui_editbox_settext(output_text,ot)
			gzread( gzf, ps, 26^6*2  )

	

'			total_items=(26^6)-1
'			for i=0 to (26^6)-1
'				if k=0 then
'					bl=0
'					k=buffer
'					curr_items=i
'					#include "ngram_loading_progress.bi"
'					gzread(gzf,fs,buffer)
'				end if
'				k-=2 'short
'				j=fs[bl]
'				bl+=1
'				ps[i]=j
'			next i
			gzclose(gzf)
		else
			fs=allocate(buffer)
			redim g7w(25,25,25,25,25,25,25)
			ps=@g7w(0,0,0,0,0,0,0)
			loadngramtimer=timer
			gzf=gzopen(basedir+"\N-grams\Words\7gram-to-1word-600M-line-reddit2020-beijinghouse1_clean_test2.txt","rb")
			k=0
			solver_file_name_ngrams="Word Discrimination File [16GB mem]"
			total_items=(26^7)-1
			for i=0 to (26^7)-1
				if k=0 then
					bl=0
					k=buffer
					curr_items=i
					#include "ngram_loading_progress.bi"
					gzread(gzf,fs,buffer)
				end if
				k-=2 'short
				j=fs[bl]
				bl+=1
				ps[i]=j
			next i
			gzclose(gzf)
			deallocate(fs)
		end if

		
	end if
	'----------------------------------------------------------------------------------
	
	solver_file_name_ngrams=right(file_name_ngrams,len(file_name_ngrams)-instrrev(file_name_ngrams,"\"))

	ngrams_inmem(ngram_size)=1
	loadngramtimer=timer
	gzf=gzopen(file_name_ngrams,"rb")
				
	if fileformat > 2 then ' gov

		' TODO: add code to check that the reads are the correct length (error checking)

		' read in number of ngrams
		bytesRead = gzread( gzf, @ngram_count, 8)
		curr_items += bytesRead
		total_items = ngram_count
		
		' read in width (ONLY for uncompressed static functions)
		if fileformat = 5 orelse fileformat = 6 then
			bytesRead = gzread( gzf, @gov_width, 8)
			curr_items += bytesRead
		endif
		
		' read in multiplier
		bytesRead = gzread( gzf, @gov_multiplier, 8)
		curr_items += bytesRead

		' read in max codeword length (ONLY for compressed static functions)
		if fileformat = 3 orelse fileformat = 4 then
			bytesRead = gzread( gzf, @gov_global_max_codeword_length, 8)
			curr_items += bytesRead
		endif
		
		' read in global seed
		bytesRead = gzread( gzf, @gov_globalseed, 8)
		curr_items += bytesRead
		
		' read in offsetAndSeed.length
		bytesRead = gzread( gzf, @gov_offset_and_seed_length, 8)
		curr_items += bytesRead
		
		' resize offset and seed array and read it in
		gov_offset_and_seed = allocate(gov_offset_and_seed_length * 8)
		bytesRead = gzread( gzf, gov_offset_and_seed, gov_offset_and_seed_length * 8)
		curr_items += bytesRead

		' read in array.length
		bytesRead = gzread( gzf, @gov_array_length, 8)
		curr_items += bytesRead
		
		' resize array and read it in
		gov_array = allocate(gov_array_length * 8)
		
		If ngram_size < 6 Then
			bytesRead = gzread( gzf, gov_array, gov_array_length * 8)
			curr_items += bytesRead

		Else

			#include "ngram_loading_progress.bi"

			' split array into medium sized chunks
			' so can track loading + not read in over
			' 2^32 bytes at once (which doesn't work)
			Dim As integer chunksize
			chunksize = 1024
			If ngram_size > 8 Then chunksize *= 1024
			k = int((gov_array_length-1) / chunksize)
			for j = 0 to k-1
				bytesRead = gzread( gzf, gov_array+j*chunksize, chunksize*8)
				curr_items += bytesRead
				#include "ngram_loading_progress.bi"
			next j

			' read in final few bytes that didn't fit in larger chunks
			for j = k*chunksize to gov_array_length-1
				bytesRead = gzread( gzf, gov_array+j, 8)
				curr_items += bytesRead
			next j

			#include "ngram_loading_progress.bi"

		EndIf


		if fileformat = 3 orelse fileformat = 4 then
			bytesRead = gzread( gzf, @gov_escaped_symbol_length, 8) ' read in escaped symbol length (ONLY for compressed static functions)
			curr_items += bytesRead
			bytesRead = gzread( gzf, @gov_escape_length, 8) ' read in escape length (ONLY for compressed static functions)
			curr_items += bytesRead
			bytesRead = gzread( gzf, @gov_decoding_table_length, 8) ' read in decoding table length (ONLY for compressed static functions)
			curr_items += bytesRead
			bytesRead = gzread( gzf, @gov_num_symbols, 8) ' read in num symbols (ONLY for compressed static functions)
			curr_items += bytesRead
			gov_last_codeword_plus_one = allocate(gov_decoding_table_length * 8) ' resize array and read it in (ONLY for compressed static functions)
			bytesRead = gzread( gzf, gov_last_codeword_plus_one, gov_decoding_table_length * 8)
			curr_items += bytesRead
			gov_how_many_up_to_block = allocate(gov_decoding_table_length * 4) ' resize array and read it in (ONLY for compressed static functions)
			bytesRead = gzread( gzf, gov_how_many_up_to_block, gov_decoding_table_length * 4)
			curr_items += bytesRead
			gov_shift = allocate(gov_decoding_table_length) ' resize array and read it in (ONLY for compressed static functions)
			bytesRead = gzread( gzf, gov_shift, gov_decoding_table_length)
			curr_items += bytesRead
			gov_symbol = allocate(gov_num_symbols * 8) ' resize array (above) and read it in (ONLY for compressed static functions)
			bytesRead = gzread( gzf, gov_symbol, gov_num_symbols * 8)
			curr_items += bytesRead
			gov_w = gov_global_max_codeword_length
			gov_end = gov_global_max_codeword_length - gov_escape_length
			gov_start = gov_end - gov_escaped_symbol_length
		endif


		If ngram_size > 5 Then
			
			' grab all 3-gram and 4-gram keyguards from gov
			redim g3keyguard(65 to 90,65 to 90,65 to 90)
			redim g4keyguard(65 to 90,65 to 90,65 to 90,65 to 90)
			for i= 65 to 90
				for j = 65 to 90
					for k = 65 to 90

						*keyguardstr = chr(i,j,k) ' kinda lame way to set string; prob slow but ok for testing

						select case fileformat
							case 5:g3keyguard(i,j,k)=sf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_width, gov_multiplier, gov_globalseed, keyguardstr, 3)
							case 6:g3keyguard(i,j,k)=sf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_width, gov_multiplier, gov_globalseed, keyguardstr, 3)
							case 3:g3keyguard(i,j,k)=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_global_max_codeword_length, gov_escaped_symbol_length, gov_escape_length, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, keyguardstr, 3)
							case 4:g3keyguard(i,j,k)=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_global_max_codeword_length, gov_escaped_symbol_length, gov_escape_length, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, keyguardstr, 3)
						end select

						if g3keyguard(i,j,k) > 0 then
							If g3keyguard(i,j,k) > highgram Then highgram = g3keyguard(i,j,k)
							count3 += 1
						endif


						for l = 65 to 90	

							*keyguardstr = chr(i,j,k,l) ' kinda lame way to set string; prob slow but ok for testing

							select case fileformat
								case 5:g4keyguard(i,j,k,l)=sf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_width, gov_multiplier, gov_globalseed, keyguardstr, 4)
								case 6:g4keyguard(i,j,k,l)=sf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_width, gov_multiplier, gov_globalseed, keyguardstr, 4)
								case 3:g4keyguard(i,j,k,l)=csf3_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, keyguardstr, 4)
								case 4:g4keyguard(i,j,k,l)=csf4_get_byte_array_fb(gov_array, gov_offset_and_seed, gov_multiplier, gov_globalseed, gov_w, gov_end, gov_start, gov_symbol, gov_last_codeword_plus_one, gov_how_many_up_to_block, gov_shift, keyguardstr, 4)
							end Select
																
							if g4keyguard(i,j,k,l) > 0 then
								If g4keyguard(i,j,k,l) > highgram Then highgram = g4keyguard(i,j,k,l)
								count4 += 1
							endif

						next l
					next k
				next j
			next i

			' expand 4-gram guards into 5-gram guards
			redim g5keyguard(65 to 90,65 to 90,65 to 90,65 to 90,65 to 90)				
			for i= 65 to 90
				for j = 65 to 90
					for k = 65 to 90
						for l = 65 to 90	
							for m = 65 to 90	

								if g4keyguard(i,j,k,l) > 0 andalso g4keyguard(j,k,l,m) > 0 then
									g5keyguard(i,j,k,l,m) =  1 ' perhaps take avg in future if used as stronger guard
									count5 += 1	
								EndIf
								
							next m
						next l
					next k
				next j
			next i

		else ' 2-gram - 5-gram gov
			highgram = 63 ' assume 6-bit values for now			
		endif
		
	else

		select case ngram_size
			
			case 2,3,4,5,6,7
				
				select case ngram_size
					case 2
						redim g2(0,0)
						redim g2(nm1,nm1)
						pi=@g2(0,0)
					case 3
						redim g3(0,0,0)
						redim g3(nm1,nm1,nm1)
						pi=@g3(0,0,0)
					case 4
						redim g4(0,0,0,0)
						redim g4(nm1,nm1,nm1,nm1)
						pi=@g4(0,0,0,0)
					case 5
						redim g5(0,0,0,0,0)
						redim g5(nm1,nm1,nm1,nm1,nm1)
						pi=@g5(0,0,0,0,0)
					case 6
						redim g6(0,0,0,0,0,0)
						redim g6(nm1,nm1,nm1,nm1,nm1,nm1)
						pi=@g6(0,0,0,0,0,0)
					case 7
						redim g7(0,0,0,0,0,0,0)
						redim g7(nm1,nm1,nm1,nm1,nm1,nm1,nm1)
						pi=@g7(0,0,0,0,0,0,0)
				end select
	
				if fileformat=2 then '1.17 beijinghouse n-gram load
					
					while totalbytes=buffer
						extra_chars=buffer-bl
						for i=0 to extra_chars-1
							fd[i]=fd[bl+i]
						next i
						bl=0
						bytesread=gzread(gzf,fd+extra_chars,buffer-extra_chars)
						totalbytes=bytesread+extra_chars
						if totalbytes<buffer then last_read=1
						while ((not last_read) and (bl<(totalbytes-ngram_size-4))) or (last_read and (bl<totalbytes))
							'-----------------------------------------------------------------------------------------
							ic=0
							x1=alpharev(fd[bl+ic]):ic+=1
							x2=alpharev(fd[bl+ic]):ic+=1
							if ic<ngram_size then x3=alpharev(fd[bl+ic]):ic+=1
							if ic<ngram_size then x4=alpharev(fd[bl+ic]):ic+=1
							if ic<ngram_size then x5=alpharev(fd[bl+ic]):ic+=1
							if ic<ngram_size then x6=alpharev(fd[bl+ic]):ic+=1
							if ic<ngram_size then x7=alpharev(fd[bl+ic]):ic+=1
							if old=0 then
								h=asc2num100(fd[bl+ic])+asc2num10(fd[bl+ic+1])+asc2num(fd[bl+ic+2])
							else
								h=(asc2num1000(fd[bl+ic])+asc2num100(fd[bl+ic+1])+asc2num10(fd[bl+ic+2])+asc2num(fd[bl+ic+3]))/10
							end if
							if h>solvesub_ngramlogcutoff then
								if h>highgram then highgram=h
								if h<ngram_lowval then ngram_lowval=h
								if h>ngram_highval then ngram_highval=h
								ngram_values(h)+=1
								select case ngram_size
									case 2:g2(x1,x2)=h
									case 3:g3(x1,x2,x3)=h
									case 4:g4(x1,x2,x3,x4)=h
									case 5:g5(x1,x2,x3,x4,x5)=h
									case 6:g6(x1,x2,x3,x4,x5,x6)=h
									case 7:g7(x1,x2,x3,x4,x5,x6,x7)=h
								end select
								ngram_count+=1
							end if
							'-----------------------------------------------------------------------------------------
							bl+=ngram_size+3+old
							curr_items+=ngram_size+3+old
							while bl<totalbytes and fd[bl]<32 'skip new lines
								bl+=1
							wend
							#include "ngram_loading_progress.bi"
						wend
					wend

'				elseif fileformat = -2 then
'					
'					dim xx as string * 6
'					while totalbytes=buffer
'						extra_chars=buffer-bl
'						for i=0 to extra_chars-1
'							fd[i]=fd[bl+i]
'						next i
'						bl=0
'						bytesread=gzread(gzf,fd+extra_chars,buffer-extra_chars)
'						totalbytes=bytesread+extra_chars
'						if totalbytes<buffer then last_read=1
'						while ((not last_read) and (bl<(totalbytes-ngram_size-4))) or (last_read and (bl<totalbytes))
'							'-----------------------------------------------------------------------------------------
'							ic=0
'							while (ic<ngram_size)
'								xx[ic]=fd[bl+ic]:ic+=1
'							wend
'							ub=asc2num100(fd[bl+ic])+asc2num10(fd[bl+ic+1])+asc2num(fd[bl+ic+2])
'							if ub>solvesub_ngramlogcutoff then
'								if ub>highgram then highgram=ub
'								if ub<ngram_lowval then ngram_lowval=ub
'								if ub>ngram_highval then ngram_highval=ub
'								ngram_values(ub)+=1
'								map.put(xx,ub)
'								ngram_count+=1
'							end if
'							'-----------------------------------------------------------------------------------------
'							bl+=ngram_size+3
'							curr_items+=ngram_size+3
'							while bl<totalbytes and fd[bl]<32 'skip new lines
'								bl+=1
'							wend
'							#include "ngram_loading_progress.bi"
'						wend
'					wend
					
				else 'binary
					
					k=0
					for i=0 to ((nm1+1)^ngram_size)-1
						if k=0 then
							bl=0
							k=buffer
							gzread(gzf,fd,buffer)
							#include "ngram_loading_progress.bi"
						end if
						k-=1 'byte
						j=fd[bl]
						bl+=1
						curr_items+=1
						if j>solvesub_ngramlogcutoff then
							pi[i]=j
							ngram_count+=1
							if j>highgram then highgram=j
							if j<ngram_lowval then ngram_lowval=j
							if j>ngram_highval then ngram_highval=j
							ngram_values(j)+=1
						end if
					next i
					
				end if
				
				#include "thread_load_best_ngram.bi" 'find best n-gram for letter position
							
			case 8,10 'beijinghouse 8-gram system
 
				' close already openned file from above first
				gzclose(gzf)
				
				gzf=gzopen(table_file,"rb")
				dim fd2 as ulong ptr 'file data
				fd2=allocate(buffer)
				dim pi2 as ulong ptr
			
				select case ngram_size
					case 8
						redim bh4(0,0,0,0)
						redim bh4(nm1,nm1,nm1,nm1)
						pi2=@bh4(0,0,0,0)

					'case 10
					'	redim bh5(0,0,0,0,0)
					'	redim bh5(nm1,nm1,nm1,nm1,nm1)
					'	pi2=@bh5(0,0,0,0,0)
				end select
				
			 	k=0
				for i=0 to ((nm1+1)^(ngram_size\2))-1
					if k=0 then
						bl=0
						k=buffer
						gzread(gzf,fd2,buffer)
					end if
					k-=4 'long
					j=fd2[bl]
					bl+=1
					if j>ngram_maxtableindex then ngram_maxtableindex=j
					if j>max_allowed_table_index then j=0
					if j>0 then pi2[i]=j
				next i
				
				gzclose(gzf)
				deallocate(fd2)
				
				bl=buffer
				fd=allocate(buffer)
				gzf=gzopen(file_name_ngrams,"rb")
				gzbuffer(gzf,8192*16) ' beijinghouse speed up reading with bigger internal zlib buffer
				if ngram_maxtableindex>max_allowed_table_index then
					ngram_mem=max_allowed_table_index*max_allowed_table_index
					select case ngram_size
						case 8:redim bh8(max_allowed_table_index,max_allowed_table_index)
						'case 10:redim bh10(max_allowed_table_index,max_allowed_table_index)
					end select
				else
					ngram_mem=ngram_maxtableindex*ngram_maxtableindex
					select case ngram_size
						case 8:redim bh8(ngram_maxtableindex,ngram_maxtableindex)
						'case 10:redim bh10(ngram_maxtableindex,ngram_maxtableindex)
					end select
				end if
				
				select case ngram_size
					case 8:pi=@bh8(0,0)
					'case 10:pi=@bh10(0,0)
				end select
				
				loadngramtimer=timer
				dim as integer ind1,ind2
				total_items=ngram_maxtableindex*ngram_maxtableindex
				if max_allowed_table_index<ngram_maxtableindex then	' beijinghouse optimization
					total_items=ngram_maxtableindex*max_allowed_table_index
				end if
				
				k=0
				h=0
				for curr_items=0 to total_items-1
					if k=0 then
						bl=0
						k=buffer
						gzread(gzf,fd,buffer)
						#include "ngram_loading_progress.bi"
					end if
					k-=1 'byte
					j=fd[bl]
					bl+=1
					if ind1<max_allowed_table_index+1 andalso ind2<max_allowed_table_index+1 then e=1 else e=0
					if j>solvesub_ngramlogcutoff andalso e=1 then
						pi[h]=j
						ngram_count+=1
						if j>highgram then highgram=j:ngram_highval=j
						if j<ngram_lowval then ngram_lowval=j
						ngram_values(j)+=1
					end if
					if e=1 then h+=1
					ind2+=1
'					if ind2>max_allowed_table_index then		' beijinghouse optimization (skip parts where e=0)
'						gzseek(gzf,ngram_maxtableindex-max_allowed_table_index,SEEK_CUR)
'						if ngram_maxtableindex-max_allowed_table_index >= k then
'							k = 0
'							ind2 += ngram_maxtableindex-max_allowed_table_index
'						else
'							k -= ngram_maxtableindex-max_allowed_table_index
'							bl += ngram_maxtableindex-max_allowed_table_index
'							ind2 += ngram_maxtableindex-max_allowed_table_index
'						end if							
'					end if
					if ind2=ngram_maxtableindex+1 then
						ind2=0
						ind1+=1
					end if
				next curr_items
				
				if ngram_maxtableindex>max_allowed_table_index then
					trimmed_table_ratio=max_allowed_table_index/ngram_maxtableindex
					ngram_maxtableindex=max_allowed_table_index
				else
					trimmed_table_ratio=1
				end if
				

			case 9 'beijinghouse crazy 9-gram systems


				ngram_mem=0
'				redim g3keyguard(nm1,nm1,nm1)

				dim fileData3 As UByte Ptr			 ' temp storage for encoder table read-in
				Dim tablecount(4,256) As UInteger ' temp storage for encoder scoring table sizes
				Dim bigtablecount(26) As UInteger
				Dim bigtablejumps(26) As UInteger
				Dim jump_location As Integer
				Dim max_bigcount As UInteger
				Dim buf_offset As Integer
				Dim spin As UInteger
				ReDim g53(nm1,nm1,nm1,nm1,nm1)
				ReDim g54(nm1,nm1,nm1,nm1,nm1)
				ngram_mem += 2*4*26^5  ' only count 2x 32bit arrays because 2 will be redimed to 0 depending on format
				Dim as uinteger top_pivot, pivot, d1,d2,d3,d4,d5,d6,j2,max_count, buf_len_s, acu_len_s, hop_len_s
				pivot = 65
				highgram=1
				top_pivot = pivot + 26
				Dim encoder_count As uinteger

				dim as uinteger bufferLen = buffer '33554432    ' use 32MB buffer - 8gram headers > 1MB; 9-grams > 2.5MB; 10-grams > 5MB
				dim as uinteger bytesRead = bufferLen  	'init value, reset when reading but needed for loop entry
				dim as uinteger totalBytes = bufferLen 	'init value, reset when reading but needed for loop entry
				buf_len_s = bufferLen	'init value, reset when reading but needed for loop entry

				While pivot < top_pivot
					
					buf_len_s = 0
					gzread( gzf, fd, 4 )
					d1 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
					acu_len_s += 4
					
					If d1 = 0 Then ' if using double-sized indexes
						ninegramformat = 3
						gzread( gzf, fd, 4 )
						d1 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4
					ElseIf fd[1+buf_len_s] = 6 Then ' using folded indexes
						ninegramformat = 8
						ninegram_quartile = fd[2+buf_len_s]
						highgram = ninegram_quartile
						ninegram_offset = fd[3+buf_len_s]
						gzread( gzf, fd, 4 )
						d1 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4
'								ninegram_offset = d1 shr 1 ' reuse this variable since unused in this format and need to know where half point is
						ninegram_half(pivot-65) = d1 shr 1
					ElseIf fd[1+buf_len_s] = 1 Then ' using folded indexes
						ninegramformat = 4
						ninegram_quartile = fd[2+buf_len_s]
						highgram = ninegram_quartile
						ninegram_offset = fd[3+buf_len_s]
						gzread( gzf, fd, 4 )
						d1 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4	
						gzread( gzf, fd, 4 )
						d3 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4
					ElseIf fd[1+buf_len_s] = 3 Then ' using folded indexes with bitshifted keys in 2 low order score bits
						ninegramformat = 6
						ninegram_quartile = fd[2+buf_len_s]
						highgram = ninegram_quartile
						ninegram_offset = fd[3+buf_len_s]
						gzread( gzf, fd, 4 )
						d1 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4	
						gzread( gzf, fd, 4 )
						d3 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4
					ElseIf fd[1+buf_len_s] = 2 Then ' using triple folded indexes
						ninegramformat = 5
						ninegram_quartile = fd[2+buf_len_s]
						highgram = ninegram_quartile
						ninegram_offset = fd[3+buf_len_s]
						gzread( gzf, fd, 4 )
						d1 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4	
						gzread( gzf, fd, 4 )
						d3 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4
						gzread( gzf, fd, 4 )
						d5 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4
					endif
					
					gzread( gzf, fd, d1*4 )								
					
					encoder_count = 1
					While encoder_count <= d1
						g53(alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s]),  alpharev(fd[3+buf_len_s]), alpharev(pivot)) = encoder_count
'						g3keyguard(alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s])) = 1
						acu_len_s += 4
						buf_len_s += 4
						encoder_count += 1
					wend

					' load backup grams for folded format
					If ninegramformat = 4 orelse ninegramformat=5 orelse ninegramformat=6 then		
						buf_len_s = 0

						gzread( gzf, fd, d3*4 )
						
						encoder_count = 1048576 + 1 ' put backup grams arbitrarily higher in encoding count
						While encoder_count <= d3 + 1048576 ' 5000000 1048576 = &H100000
							g53(alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s]),  alpharev(fd[3+buf_len_s]), alpharev(pivot)) = encoder_count
							acu_len_s += 4
							buf_len_s += 4
							encoder_count += 1
'							g3keyguard(alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s])) = 1
						Wend
					endif

					' load backup grams for folded format
					If ninegramformat = 5 Then		
						buf_len_s = 0

						gzread( gzf, fd, d5*4 )
						
						encoder_count = 16777216 + 1 '10000001 ' put backup grams arbitrarily higher in encoding count
						While encoder_count <= d5 + 16777216 '10000000
							g53(alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s]),  alpharev(fd[3+buf_len_s]), alpharev(pivot)) = encoder_count
							acu_len_s += 4
							buf_len_s += 4
							encoder_count += 1
'							g3keyguard(alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s])) = 1
						Wend
					endif
					
					buf_len_s = 0
					gzread( gzf, fd, 4 )
				
					d2 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
					acu_len_s += 4

					If ninegramformat = 4 orelse ninegramformat = 5 orelse ninegramformat=6 then
						buf_len_s = 0
						gzread( gzf, fd, 4 )
					
						d4 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4
					endif

					If ninegramformat = 5 Then
						buf_len_s = 0
						gzread( gzf, fd, 4 )
					
						d6 = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
						acu_len_s += 4
					endif
					
					If ninegramformat = 4 Then
						gzread( gzf, fd, d2*4 + d4*4 + 255*4 + 255*4*3 )	
					elseif ninegramformat = 5 then
						gzread( gzf, fd, d2*4 + d4*4 + d6*4 + 255*4 )
					elseif ninegramformat=6 then
						gzread( gzf, fd, d2*4 + d4*4 + 255*4 )
					elseif ninegramformat=8 then
						gzread( gzf, fd, d2*4 + 255*4*2 )
					else
						gzread( gzf, fd, d2*4 + 255*4 + 255*4*ninegramformat )	
				   endif							
					
					encoder_count = 1
					While encoder_count <= d2
						g54(alpharev(pivot), alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s]), alpharev(fd[3+buf_len_s])) = encoder_count
'						g3keyguard(alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s])) = 1
						acu_len_s += 4
						buf_len_s += 4
						encoder_count += 1
					wend

					' load backup grams for folded format
					If ninegramformat = 4 orelse ninegramformat = 5 orelse ninegramformat = 6 then
						encoder_count = 1048576 + 1 ' put backup grams arbitrarily higher in encoding count
						While encoder_count <= d4 + 1048576 '5000000
							g54(alpharev(pivot), alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s]), alpharev(fd[3+buf_len_s])) = encoder_count
							acu_len_s += 4
							buf_len_s += 4
							encoder_count += 1
'							g3keyguard(alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s])) = 1
						Wend
					endif

					' load double backup grams for folded format
					If ninegramformat = 5 Then
								
						encoder_count = 16777216 + 1 ' put backup grams arbitrarily higher in encoding count
						While encoder_count <= d6 + 16777216
							g54(alpharev(pivot), alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s]), alpharev(fd[3+buf_len_s])) = encoder_count
							acu_len_s += 4
							buf_len_s += 4
							encoder_count += 1
'							g3keyguard(alpharev(fd[0+buf_len_s]), alpharev(fd[1+buf_len_s]), alpharev(fd[2+buf_len_s])) = 1
						Wend
					endif
					
					' add in actual scores
					If ninegramformat = 9 orelse ninegramformat = 5 orelse ninegramformat = 6 then	
						j2 = 0
					elseif ninegramformat = 8 then
						j2 = 1
					Else
						j2 = 3
					endif
					
					max_count = 0	
					For j = 0 To j2
						For i = 255 To 1 Step -1
							tablecount(j,i) = (fd[0+buf_len_s] Shl 24) + (fd[1+buf_len_s] Shl 16) + (fd[2+buf_len_s] Shl 8) + fd[3+buf_len_s]
							If tablecount(j,i) > max_count Then
			               max_count = tablecount(j,i)	
							EndIf
							acu_len_s += 4
							buf_len_s += 4
						Next i
					Next j
					
					if  ninegramformat = 8 then ' only need half the entries for this format since 4-bit
						'd1 = 1 + (d1 shr 1)
						if (d1 mod 2) = 1 then
							d1 = 1 + (d1 shr 1)
						else
							d1 = (d1 shr 1)
						endif
					endif
					
					
					Select case pivot-65
						Case 0: redim gxa9(1 to d1,1 to d2)
						Case 1: redim gxb9(1 to d1,1 to d2)
						Case 2: redim gxc9(1 to d1,1 to d2)
						Case 3: redim gxd9(1 to d1,1 to d2)
						Case 4: redim gxe9(1 to d1,1 to d2)
						Case 5: redim gxf9(1 to d1,1 to d2)
						Case 6: redim gxg9(1 to d1,1 to d2)
						Case 7: redim gxh9(1 to d1,1 to d2)
						Case 8: redim gxi9(1 to d1,1 to d2)
						Case 9: redim gxj9(1 to d1,1 to d2)
						Case 10:redim gxk9(1 to d1,1 to d2)
						Case 11:redim gxl9(1 to d1,1 to d2)
						Case 12:redim gxm9(1 to d1,1 to d2)
						Case 13:redim gxn9(1 to d1,1 to d2)
						Case 14:redim gxo9(1 to d1,1 to d2)
						Case 15:redim gxp9(1 to d1,1 to d2)
						Case 16:redim gxq9(1 to d1,1 to d2)
						Case 17:redim gxr9(1 to d1,1 to d2)
						Case 18:redim gxs9(1 to d1,1 to d2)
						Case 19:redim gxt9(1 to d1,1 to d2)
						Case 20:redim gxu9(1 to d1,1 to d2)
						Case 21:redim gxv9(1 to d1,1 to d2)
						Case 22:redim gxw9(1 to d1,1 to d2)
						Case 23:redim gxx9(1 to d1,1 to d2)
						Case 24:redim gxy9(1 to d1,1 to d2)
						Case 25:redim gxz9(1 to d1,1 to d2)
					End Select

					
					ngram_mem += d1*d2
					
					fileData3 = Allocate(max_count*4)
					
					total_items = filelen1
					
					For j = 0 To j2
						For i = 255 To 1 Step -1
							hop_len_s = tablecount(j,i)
							If hop_len_s > 0 then
								
								If ninegramformat <> 9 Then
									buf_len_s = 0
									bytesRead = gzread( gzf, fileData3, hop_len_s*4 )
								Else
									buf_offset = buf_len_s
								EndIf
								
								if ninegramformat = 3 then
									if i > highgram Then highgram = i
								endif
								
								While (buf_len_s - buf_offset) < hop_len_s
									
									x1 = (fileData3[buf_len_s] Shl 8) + fileData3[hop_len_s+buf_len_s]
									x2 = (fileData3[2*hop_len_s+buf_len_s] Shl 8) + fileData3[3*hop_len_s+buf_len_s]
									
									If j = 1 Then
										x2 += 65536    ' YES THIS IS CORRECT, encoder is built like this
									ElseIf j = 2 Then
										x1 += 65536
									ElseIf j = 3 Then
										x1 += 65536
										x2 += 65536
									EndIf
									
									Select case pivot-65
										Case 0: gxa9(x1,x2) = i
										Case 1: gxb9(x1,x2) = i
										Case 2: gxc9(x1,x2) = i
										Case 3: gxd9(x1,x2) = i
										Case 4: gxe9(x1,x2) = i
										Case 5: gxf9(x1,x2) = i
										Case 6: gxg9(x1,x2) = i
										Case 7: gxh9(x1,x2) = i
										Case 8: gxi9(x1,x2) = i
										Case 9: gxj9(x1,x2) = i
										Case 10:gxk9(x1,x2) = i
										Case 11:gxl9(x1,x2) = i
										Case 12:gxm9(x1,x2) = i
										Case 13:gxn9(x1,x2) = i
										Case 14:gxo9(x1,x2) = i
										Case 15:gxp9(x1,x2) = i
										Case 16:gxq9(x1,x2) = i
										Case 17:gxr9(x1,x2) = i
										Case 18:gxs9(x1,x2) = i
										Case 19:gxt9(x1,x2) = i
										Case 20:gxu9(x1,x2) = i
										Case 21:gxv9(x1,x2) = i
										Case 22:gxw9(x1,x2) = i
										Case 23:gxx9(x1,x2) = i
										Case 24:gxy9(x1,x2) = i
										Case 25:gxz9(x1,x2) = i
									End Select
									
									if i<ngram_lowval then ngram_lowval=i
									if i>ngram_highval then ngram_highval=i
									ngram_values(i)+=1
						
									ngram_count+=1

									if ninegramformat = 8 and (i > 15) and ( (i and &HF) > 0) then ' since 4-bit format, if both halves have data, count twice
										ngram_count+=1
									endif

									buf_len_s+=1
									acu_len_s+=4

									curr_items = acu_len_s
					
									#include "ngram_loading_progress.bi"
									
								wend
								
							endif
							
						Next i 
					Next j
					
					DeAllocate(fileData3)
					
					'-----
					
					pivot += 1
					
				Wend


		end select
	
	end if
	
	gzclose(gzf)
	deallocate(fd)
	
	
''' temp removal for testing
	
	if firststart=1 then 'load n-grams that add spaces to output
'		pos_file=basedir+"\N-grams\Spaces\5-grams_positions"
'		space_file=basedir+"\N-grams\Spaces\5-grams_english+spaces_jarlve_reddit.txt.zst"
'		if fileexists(pos_file)=-1 andalso fileexists(space_file)=-1 then
'			pi=@g5p(0,0,0,0,0)
'			gzf=gzopen(pos_file,"rb")
'			gzread( gzf, pi, (25+1)^5 )
'			gzclose(gzf)
'			pi=@g5s(0,0,0,0,0)
'			gzf=gzopen(space_file,"rb")
'			gzread( gzf, pi, (26+1)^5 )
'			gzclose(gzf)
'			addspaces_ngrams=1
'		end if
		firststart=0
	end if

	
	ngram_avgval=0
	distinct_values=0
	ngram_value_entropy1=0
	ngram_value_entropy2=0

	
	if fileformat < 3 then
	
		dim as double c1,c2
		ngram_values(0)=(ngram_alphabet_size^ngram_size)-ngram_count
		for i=0 to 255
			if ngram_values(i)>0 then
				ngram_avgval+=ngram_values(i)*i
				distinct_values+=1
				c1=ngram_values(i)/(ngram_alphabet_size^ngram_size)
				ngram_value_entropy1+=logbx(c1,2)*c1
				if i>1 then
					c2=ngram_values(i)/ngram_count
					ngram_value_entropy2+=logbx(c2,2)*c2
				end if
			end if
		next i
		if ngram_lowval=999 then ngram_lowval=0
		
		pickletter_caching(0)
	
	endif 
	
	ngram_loading_time=timer-start_time
	#include "ngram_stats.bi"
	
	task_active="none"
	if loadngrams_showmsg=1 then update_solver_status
	
end sub

sub thread_load_ngrambias(byval none as any ptr)
	
	dim as integer i,j,k,l,e,n,m,v,jj,kk,nfb,skip,mb,ul,ol
	dim as integer x1,x2,x3,x4,x5,x6,x7,x8,x9,z1,z2,filelength
	dim as double maxfrq
	dim as string s,o,filename
	dim as ubyte alphar(255)
	dim as double bias=0.1 'default bias
	dim as double t1,t2
	
	if ngrambias_showmsg=1 then
		task_active="loading n-gram bias"
		update_solver_status
		filename=ui_loadsavedialog(0,"Open n-gram bias",filter,1,basedir+"\N-grams\Biases\")
		if fileexists(filename)=0 then
			if filename<>"" then ui_editbox_settext(output_text,"Error: file not found")
			task_active="none"
			update_solver_status
			exit sub
		end if
		filelength=filelen(filename)
		'if filelength>2^20 then
		'	ui_editbox_settext(output_text,"Error: bias file cannot be larger than 1 megabyte") 'due to slow speed of routine
		'	task_active="none"
		'	exit sub
		'end if
	else
		filelength=len(ngrambias_text)
	end if
	
	t2=timer
	
	nfb=filelength*2 'ngbt,ngbd
	nfb+=filelength*4 'ngbf
	nfb+=filelength*ngram_size 'ngbn
	
	if memcheck=1 andalso nfb>=fre then
		ui_editbox_settext(output_text,"Error: not enough free RAM to load n-gram bias")
		task_active="none"
		exit sub
	end if
	
	for i=0 to ngram_alphabet_size-1
		alphar(alphabet(i))=1
	next i
	
	redim ngbt(filelength)
	redim ngbd(filelength)
	'dim as ulong bigfrq(ngram_alphabet_size-1,ngram_alphabet_size-1)
	dim as ulong trifrq(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1)
	
	if ngrambias_showmsg=1 then
		open filename for binary as #1
		line input #1,s
		if left(s,12)="Bias factor=" then
			skip=1
			bias=val(right(s,len(s)-12))
			line input #1,s
		end if
	else
		bias=ngrambias_factor
		s=ngrambias_text
	end if
	
	do 'convert bias file to array
		if ngrambias_showmsg=1 then if skip=0 then line input #1,s else skip=0
		for i=0 to len(s)-1
			e=0
			if alphar(s[i])=1 then 'check if letter exist within current alphabet
				e=1:l+=1
				ngbt(l)=alpharev(s[i])
			else 'else check if letter case variation exist within current alphabet, if yes then convert
				select case s[i]
					case 65 to 90 'ucase
						if alphar(s[i]+32)=1 then
							e=1:l+=1
							ngbt(l)=alpharev(s[i]+32)
						end if
					case 97 to 122 'lcase
						if alphar(s[i]-32)=1 then
							e=1:l+=1
							ngbt(l)=alpharev(s[i]-32)
						end if
				end select
			end if
		next i
		if eof(1)=0 then
			l+=1
			ngbt(l)=0
			for i=0 to ngram_size-1
				if l-i>0 then ngbd(l-i)=1
			next i
		end if
	loop until eof(1)
	close #1
	
	'for i=1 to l-(ngram_size-1)
	'	bigfrq(ngbt(i),ngbt(i+1))+=1
	'	if bigfrq(ngbt(i),ngbt(i+1))>mb then mb=bigfrq(ngbt(i),ngbt(i+1))
	'next i
	
	for i=1 to l-(ngram_size-1)
		trifrq(ngbt(i),ngbt(i+1),ngbt(i+2))+=1
		if trifrq(ngbt(i),ngbt(i+1),ngbt(i+2))>mb then mb=trifrq(ngbt(i),ngbt(i+1),ngbt(i+2))
	next i
	
	nfb+=(ngram_alphabet_size^3)*mb*4 'ngbl
	
	if memcheck=1 andalso nfb>=fre then
		ui_editbox_settext(output_text,"Error: not enough free RAM to load n-gram bias")
		task_active="none"
		update_solver_status
		exit sub
	end if
	
	'redim ngbl(ngram_alphabet_size-1,ngram_alphabet_size-1,mb)
	
	'for i=1 to l-1 'create bigram look up table
	'	ngbl(ngbt(i),ngbt(i+1),0)+=1
	'	ngbl(ngbt(i),ngbt(i+1),ngbl(ngbt(i),ngbt(i+1),0))=i
	'next i
	
	redim ngbl(ngram_alphabet_size-1,ngram_alphabet_size-1,ngram_alphabet_size-1,mb)
	
	for i=1 to l-1 'create quadgram look up table
		ngbl(ngbt(i),ngbt(i+1),ngbt(i+2),0)+=1
		ngbl(ngbt(i),ngbt(i+1),ngbt(i+2),ngbl(ngbt(i),ngbt(i+1),ngbt(i+2),0))=i
	next i
	
	redim ngbn(l,ngram_size-1)
	redim ngbf(l)
	
	t1=timer
	
	for i=1 to l-(ngram_size-1) 'build n-gram list + frequencies (slow routine)
		
		if ngrambias_showmsg=1 then
			if timer-t1>1 then
				t1=timer
				o="Loading n-gram bias: "+format((i/(l-(ngram_size-1)))*100,"0.00")+"%"+lb
				o+="(click stop task to cancel)"
				ui_editbox_settext(output_text,o)
			end if
			if stoptask=1 then
				stoptask=0
				task_active="none"
				update_solver_status
				ui_editbox_settext(output_text,"")
				exit sub
			end if
		end if
		
		'if ngbd(i)=0 then
		'	n+=1
		'	ngbd(i)=1
		'	ngbf(n)=1
		'	for k=0 to ngram_size-1
		'		ngbn(n,k)=ngbt(i+k)
		'	next k
		'	for j=1 to ngbl(ngbt(i),ngbt(i+1),0)
		'		if ngbl(ngbt(i),ngbt(i+1),j)>=i then exit for
		'	next j
		'	for jj=j to ngbl(ngbt(i),ngbt(i+1),0)
		'		kk=ngbl(ngbt(i),ngbt(i+1),jj)
		'		if ngbd(kk)=0 andalso kk<=l-(ngram_size-1) then
		'			e=1
		'			for k=0 to ngram_size-1
		'				if ngbt(i+k)<>ngbt(kk+k) then
		'					e=0
		'					exit for
		'				end if
		'			next k
		'			if e=1 then
		'				ngbf(n)+=1
		'				ngbd(kk)=1
		'			end if
		'		end if
		'	next jj
		'end if
		
		if ngbd(i)=0 then
			n+=1
			ngbd(i)=1
			ngbf(n)=1
			for k=0 to ngram_size-1
				ngbn(n,k)=ngbt(i+k)
			next k
			for j=1 to ngbl(ngbt(i),ngbt(i+1),ngbt(i+2),0)
				if ngbl(ngbt(i),ngbt(i+1),ngbt(i+2),j)>=i then exit for
			next j
			for jj=j to ngbl(ngbt(i),ngbt(i+1),ngbt(i+2),0)
				kk=ngbl(ngbt(i),ngbt(i+1),ngbt(i+2),jj)
				if ngbd(kk)=0 andalso kk<=l-(ngram_size-1) then
					e=1
					for k=0 to ngram_size-1
						if ngbt(i+k)<>ngbt(kk+k) then
							e=0
							exit for
						end if
					next k
					if e=1 then
						ngbf(n)+=1
						ngbd(kk)=1
					end if
				end if
			next jj
		end if
		
	next i
	
	'bias*=(n/(l-(ngram_size-1))) 'attempted normalization
	
	for i=1 to n 'find max frequency for normalization
		if ngbf(i)>0 then ngbf(i)=log(ngbf(i)+1)*100 'use log
		if ngbf(i)>m then m=ngbf(i)
	next i
		
	'if ngram_size<7 then
	'	maxfrq=exp(highgram/100)
	'else
		maxfrq=exp(highgram/10)
	'end if
	
	select case ngram_size 'apply bias to normal n-grams
		case 2
			for i=1 to n
				v=log(exp((g2(ngbn(i,0),ngbn(i,1))/10))+(maxfrq*(ngbf(i)/m)*bias))*10
				if v<1 then v=0:ul+=1
				if v>255 then v=255:ol+=1
				if v>highgram then highgram=v
				g2(ngbn(i,0),ngbn(i,1))=v
			next i
		case 3
			for i=1 to n
				v=log(exp((g3(ngbn(i,0),ngbn(i,1),ngbn(i,2))/10))+(maxfrq*(ngbf(i)/m)*bias))*10
				if v<1 then v=0:ul+=1
				if v>255 then v=255:ol+=1
				if v>highgram then highgram=v
				g3(ngbn(i,0),ngbn(i,1),ngbn(i,2))=v
			next i
		case 4
			for i=1 to n
				v=log(exp((g4(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3))/10))+(maxfrq*(ngbf(i)/m)*bias))*10
				if v<1 then v=0:ul+=1
				if v>255 then v=255:ol+=1
				if v>highgram then highgram=v
				g4(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3))=v
			next i
		case 5
			for i=1 to n
				v=log(exp((g5(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3),ngbn(i,4))/10))+(maxfrq*(ngbf(i)/m)*bias))*10
				if v<1 then v=0:ul+=1
				if v>255 then v=255:ol+=1
				if v>highgram then highgram=v
				g5(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3),ngbn(i,4))=v
			next i
		case 6
			for i=1 to n
				v=log(exp((g6(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3),ngbn(i,4),ngbn(i,5))/10))+(maxfrq*(ngbf(i)/m)*bias))*10
				if v<1 then v=0:ul+=1
				if v>255 then v=255:ol+=1
				if v>highgram then highgram=v
				g6(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3),ngbn(i,4),ngbn(i,5))=v
			next i
		case 7
			for i=1 to n
				v=log(exp((g7(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3),ngbn(i,4),ngbn(i,5),ngbn(i,6))/10))+(maxfrq*(ngbf(i)/m)*bias))*10
				if v<1 then v=0:ul+=1
				if v>255 then v=255:ol+=1
				if v>highgram then highgram=v
				g7(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3),ngbn(i,4),ngbn(i,5),ngbn(i,6))=v
			next i
		case 8
			for i=1 to n
				if bh4(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3))<>0 andalso bh4(ngbn(i,4),ngbn(i,5),ngbn(i,6),ngbn(i,7))<>0 then 'protect 0 values
					v=bh8(bh4(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3)),bh4(ngbn(i,4),ngbn(i,5),ngbn(i,6),ngbn(i,7)))
					v=log(exp(v/10)+(maxfrq*(ngbf(i)/m)*bias))*10
					if v<1 then v=0:ul+=1
					if v>255 then v=255:ol+=1
					if v>highgram then highgram=v
					if v>0 then bh8(bh4(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3)),bh4(ngbn(i,4),ngbn(i,5),ngbn(i,6),ngbn(i,7)))=v
				end if
			next i
		'case 10
		'	for i=1 to n
		'		if bh5(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3),ngbn(i,4))<>0 andalso bh5(ngbn(i,5),ngbn(i,6),ngbn(i,7),ngbn(i,8),ngbn(i,9))<>0 then 'protect 0 values
		'			v=bh10(bh5(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3),ngbn(i,4)),bh5(ngbn(i,5),ngbn(i,6),ngbn(i,7),ngbn(i,8),ngbn(i,9)))
		'			v=log(exp(v/10)+(maxfrq*(ngbf(i)/m)*bias))*10
		'			if v<1 then v=0:ul+=1
		'			if v>255 then v=255:ol+=1
		'			if v>highgram then highgram=v
		'			if v>0 then bh10(bh5(ngbn(i,0),ngbn(i,1),ngbn(i,2),ngbn(i,3),ngbn(i,4)),bh5(ngbn(i,5),ngbn(i,6),ngbn(i,7),ngbn(i,8),ngbn(i,9)))=v
		'		end if
		'	next i
	end select
	
	redim ngbt(0)
	redim ngbd(0)
	redim ngbn(0,0)
	redim ngbl(0,0,0,0)
	redim ngbf(0)
	
	pickletter_caching(0)
	#include "thread_load_best_ngram.bi"
	
	if ngrambias_showmsg=1 then
		o=right(filename,len(filename)-instrrev(filename,"\"))+lb '+str(m)+", "+str(mn)+", "+str(bias)
		o+="---------------------------------------------------------"+lb
		o+="Bias "+str(ngram_size)+"-grams: "+str(n)+lb
		o+="Bias factor: "+format(bias,"0.00000")+lb
		if ul+ol>0 then o+="Fixed out of bound values: "+str(ul+ol)+lb
		o+="Loading time: "+format(timer-t2,"0.0")+" seconds"
		ui_editbox_settext(output_text,o)
		task_active="none"
		update_solver_status
	end if
	
end sub

sub bhdecrypt_mergeseqhom(byval tn_ptr as any ptr)
	
	dim as integer tn=cint(tn_ptr)
	thread(tn).thread_active=1
	thread(tn).thread_stop=0
	thread(tn).solver_waiting=1
	
	dim as integer local_outputbatch,local_pcmode,local_alphabet_size
	dim as integer h,i,j,k,l,s,t,e,o,p,new_letter,old_letter,curr_symbol,improved
	dim as integer ioc_int,random_restarts,rr,it,ll,num
	dim as uinteger iterations,iterations_total,iterations_max,old_ioc_int,local_advstats
	dim as double new_score,old_score,best_score,ioc,temp,temp_min,start_temp,prev_temp,ls
	dim as double factor,temp1,entweight,ngramfactor,m,multiplicityweight,curr_temp,ngf,ngfal,iocweight
	dim as integer local_outputdir,local_outputimp,local_over,cioc,abc_sizem1
	dim as string filename,solstring
	dim as double new_cycle_score,old_cycle_score,cycle_old,cycle_new,tcs
	dim as integer cycle_count,low,c1,mi
	dim as short al,cl,cs,s1,es,lnb(0)
	dim as uinteger state,seed=tn
		
	dim as short frq(255)
	dim as short frq2(255)
	dim as short nba(constcip)
	dim as short sol(constcip)
	dim as short map1(constfrq,constfrq)
	dim as short map1r(constcip)
	dim as short stl(constcip)
	dim as short id(constcip)
	dim as short sym(constcip)
	
	dim as short solnba(255,constfrq)
	dim as short m1p(constcip)
	dim as short cyc(constcip)
	dim as double cycles(255)
	dim as double cycle_table(constcip) 'weights
	dim as double tmp1,tmp2,tmp3,tmpd
	dim as integer cl2
	dim as double cyclealphabetsize,cyclelengthweight
	dim as integer solver_output=5
	
	dim as short z2(constcip),p1,p2,al1,al2,al3
	
	do 'wait for input
	
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			cyclealphabetsize=thread(tn).cyclealphabetsize
			cyclelengthweight=thread(tn).cyclelengthweight
			local_advstats=thread(tn).advstats
			local_outputdir=solvesub_outputdir
			local_outputbatch=solvesub_outputbatch
			local_outputimp=solvesub_outputimp
			local_over=solvesub_scoreover
			random_restarts=thread(tn).restarts
			iocweight=thread(tn).entweight
			ngramfactor=thread(tn).ngramfactor	
			iterations_total=thread(tn).iterations
			multiplicityweight=thread(tn).multiplicityweight
			temp1=thread(tn).temperature
			local_alphabet_size=cyclealphabetsize
			l=thread(tn).l
			s=thread(tn).s
			
			if local_alphabet_size>s then local_alphabet_size=s-1 else local_alphabet_size-=1
			
			m=s/l
			ls=l/s
			ll=l*(l-1)	 			
			best_score=0
			al=(l-3)
			
			for i=2 to constcip
				cycle_table(i)=(i*(i-1))^cyclelengthweight
			next i
			
			erase id,map1,map1r,sym
			
			e=0
			for i=1 to l
				if id(thread(tn).cip(i))=0 then
					e+=1
					nba(i)=e
					id(thread(tn).cip(i))=e
					sym(e)=thread(tn).cip(i)
				else
					nba(i)=id(thread(tn).cip(i))
				end if
				map1r(nba(i))+=1
				map1(nba(i),map1r(nba(i)))=i
			next i
			
			for rr=1 to random_restarts
				
				erase frq,solnba,cycles
			
				start_temp=((temp1/30)*ls)
				start_temp/=m_ioc2(nba(),l,s,2)^0.75
				curr_temp=start_temp
				'ngf=ngramfactor
				'ngf/=(1+((s/l)*multiplicityweight))
				'ngfal=ngf/al
				
				for i=1 to s	
					state=48271*state and 2147483647
					new_letter=(local_alphabet_size+1)*state shr 31
					stl(i)=new_letter
					frq(new_letter)+=map1r(i)	
					solnba(new_letter,0)+=1
					solnba(new_letter,solnba(new_letter,0))=i
					for j=1 to map1r(i)
						sol(map1(i,j))=new_letter
					next j
				next i
		 	
				ioc_int=0
				for i=0 to local_alphabet_size
					ioc_int+=ioctable(frq(i))
				next i
				
				old_score=0
				curr_symbol=1
				temp=curr_temp
				temp_min=temp/iterations_total
				
				'cycle_count=s
				new_cycle_score=0
					
				for h=0 to local_alphabet_size 'get initial cycle score
					if frq(h)>0 then
						cl=0
						cs=solnba(h,0) 'cycle length/size
						for i=1 to cs
							m1p(i)=1
						next i
						do
							low=l+1
							for i=1 to cs
								if map1(solnba(h,i),m1p(i))>0 then
									if map1(solnba(h,i),m1p(i))<low then
										mi=i
										low=map1(solnba(h,i),m1p(i))
									end if
								end if
							next i
							if low<>l+1 then
								cl+=1
								cyc(cl)=mi
								m1p(mi)+=1
							end if
						loop until low=l+1
						al=0
						tcs=0
						
						'----------------------------------------------------------------------
						
						if cs=1 then 'score normal cycles
							if cl>1 then 
								tcs+=cycle_table(cl)
								'tcs+=log(cycle_table(cl))
							else 
								tcs+=0.1
							end if
						else
							for i=1 to cl-(cs-1)
								e=1
								for j=i to i+(cs-2)
									for k=j+1 to i+(cs-1)
										if cyc(j)=cyc(k) then
											e=0
											exit for,for
										end if
									next k
								next j
								'z2(i)=e
								if e=1 then al+=1
							next i
							'al3=cl-(cs-1)
							'if al3 mod 2=1 then
						   '   al1=1:al2=(al3-1)/2
							'else
						   '   al1=0:al2=al3/2
						   'end if
						   'p1=0:p2=0
						   'for i=1 to al2
						   '   p1+=z2(i)
						   '   p2+=z2(i+al1+al2)
						   'next i
						   tcs+=cycle_table(al)
							'tcs+=cycle_table(al)*(1+(((al2+(p1-p2))/(al2*2))/1))
						end if
						
						'----------------------------------------------------------------------
						
						cycles(h)=tcs
						new_cycle_score+=tcs	
					end if
				next h
				
				for it=1 to iterations_total
					
					old_letter=stl(curr_symbol)
					
					state=48271*state and 2147483647
					new_letter=local_alphabet_size*state shr 31
					if new_letter=old_letter then new_letter=local_alphabet_size
					
					for i=1 to map1r(curr_symbol)
						sol(map1(curr_symbol,i))=new_letter
					next i
					
					old_ioc_int=ioc_int
					ioc_int+=(map1r(curr_symbol)+frq(new_letter)-frq(old_letter))*map1r(curr_symbol)shl 1
					
					j=0
					solnba(new_letter,0)+=1
					solnba(new_letter,solnba(new_letter,0))=curr_symbol
					solnba(old_letter,0)-=1
					for i=1 to solnba(old_letter,0)
						if solnba(old_letter,i)=curr_symbol then j+=1
						solnba(old_letter,i)=solnba(old_letter,i+j)
					next i
					
					old_cycle_score=new_cycle_score
					
					'-------------------------------
					
					if frq(old_letter)-map1r(curr_symbol)>0 then
						
						cl=0
						cs=solnba(old_letter,0)
						for i=1 to cs
							m1p(i)=1
						next i
						do
							low=l+1
							for i=1 to cs
								if map1(solnba(old_letter,i),m1p(i))>0 then
									if map1(solnba(old_letter,i),m1p(i))<low then
										mi=i
										low=map1(solnba(old_letter,i),m1p(i))
									end if
								end if
							next i
							if low<>l+1 then
								cl+=1
								cyc(cl)=mi
								m1p(mi)+=1
							end if
						loop until low=l+1
						al=0
						tcs=0
						
						'----------------------------------------------------------------------
						
						if cs=1 then 'score normal cycles
							if cl>1 then
								tcs+=cycle_table(cl)
								'tcs+=log(cycle_table(cl))
							else
								tcs+=0.1
							end if
						else
							for i=1 to cl-(cs-1)
								e=1
								for j=i to i+(cs-2)
									for k=j+1 to i+(cs-1)
										if cyc(j)=cyc(k) then
											e=0
											exit for,for
										end if
									next k
								next j
								'z2(i)=e
								if e=1 then al+=1
							next i
							'al3=cl-(cs-1)
							'if al3 mod 2=1 then
						   '   al1=1:al2=(al3-1)/2
						   'else 
						   '   al1=0:al2=al3/2
						   'end if
						   'p1=0:p2=0
						   'for i=1 to al2
						   '   p1+=z2(i)
						   '   p2+=z2(i+al1+al2)
						   'next i
						   tcs+=cycle_table(al)
							'tcs+=cycle_table(al)*(1+(((al2+(p1-p2))/(al2*2))/1)) 'uses matchweight ???
						end if
						
						'----------------------------------------------------------------------
						
						cycle_old=cycles(old_letter)
						cycles(old_letter)=tcs
					else
						cycle_old=cycles(old_letter)
						cycles(old_letter)=0
					end if
					new_cycle_score+=cycles(old_letter)-cycle_old
					
					'-------------------------------
					
					cl=0
					cs=solnba(new_letter,0)
					for i=1 to cs
						m1p(i)=1
					next i
					do
						low=l+1
						for i=1 to cs
							if map1(solnba(new_letter,i),m1p(i))>0 then
								if map1(solnba(new_letter,i),m1p(i))<low then
									mi=i
									low=map1(solnba(new_letter,i),m1p(i))
								end if
							end if
						next i
						if low<>l+1 then
							cl+=1
							cyc(cl)=mi
							m1p(mi)+=1
						end if
					loop until low=l+1
					al=0
					tcs=0
					
					'----------------------------------------------------------------------
					
					if cs=1 then 'score normal cycles
						if cl>1 then
							tcs+=cycle_table(cl)
							'tcs+=log(cycle_table(cl))
						else 
							tcs+=0.1
						end if
					else
						for i=1 to cl-(cs-1)
							e=1
							for j=i to i+(cs-2)
								for k=j+1 to i+(cs-1)
									if cyc(j)=cyc(k) then
										e=0
										exit for,for
									end if
								next k
							next j
							'z2(i)=e
							if e=1 then al+=1
						next i
						'al3=cl-(cs-1)
						'if al3 mod 2=1 then
					   '   al1=1:al2=(al3-1)/2
					   'else 
					   '   al1=0:al2=al3/2
					   'end if
					   'p1=0:p2=0
					   'for i=1 to al2
					   '   p1+=z2(i)
					   '   p2+=z2(i+al1+al2)
					   'next i
					   tcs+=cycle_table(al)
						'tcs+=cycle_table(al)*(1+(((al2+(p1-p2))/(al2*2))/1)) 'uses matchweight ???
					end if
					
					'----------------------------------------------------------------------
					
					cycle_new=cycles(new_letter)
					cycles(new_letter)=tcs
					new_cycle_score+=cycles(new_letter)-cycle_new
					new_score=(new_cycle_score/(((l-1)*(l-2))^cyclelengthweight))*100000
					
					'cioc=0
					'for i=0 to local_alphabet_size
					'	if solnba(i,0)>1 then cioc+=ioctable(solnba(i,0))
					'next i
					'new_score*=1+((cioc/(s*(s-1)))*solvesub_matchweight)
					
					'new_score*=entropy
					'new_score/=4
					
					if new_score>old_score then
						
						frq(old_letter)-=map1r(curr_symbol)
						frq(new_letter)+=map1r(curr_symbol)
						stl(curr_symbol)=new_letter
						old_score=new_score
						
						state=48271*state and 2147483647
						curr_symbol=1+s*state shr 31													 								 
						
						if new_score>best_score then
							
							thread(tn).sectime=timer-sectimer
							thread(tn).ioc=ioc_int/ll
							es=0:cioc=0
							for i=0 to local_alphabet_size
								if frq(i)>0 then es+=1
								if solnba(i,0)>1 then cioc+=ioctable(solnba(i,0))
							next i
							thread(tn).ioc2=cioc/(s*(s-1))
							thread(tn).effectivesymbols=es
							thread(tn).multiplicity=s/l
							for i=1 to l
								thread(tn).sol(i)=sym(solnba(sol(i),1))
								thread(tn).key(i)=sol(i)+1
							next i
							best_score=new_score+0.00001
							thread(tn).score=best_score
							
						end if
						
					else
						
						j=0
						solnba(old_letter,0)+=1
						solnba(old_letter,solnba(old_letter,0))=curr_symbol
						solnba(new_letter,0)-=1
						for i=1 to solnba(new_letter,0)
							if solnba(new_letter,i)=curr_symbol then j+=1
							solnba(new_letter,i)=solnba(new_letter,i+j) 
						next i
						
						cycles(old_letter)=cycle_old
						cycles(new_letter)=cycle_new	
						
						new_cycle_score=old_cycle_score
						ioc_int=old_ioc_int
						for i=1 to map1r(curr_symbol)
							sol(map1(curr_symbol,i))=old_letter
						next i
						
						'if curr_symbol=s then curr_symbol=1 else curr_symbol+=1
						
						state=48271*state and 2147483647
						curr_symbol=1+s*state shr 31
						
						old_score-=temp*(map1r(curr_symbol)+1)/l
					
					end if
					
					temp-=temp_min
					
					thread(tn).iterations_completed+=1
					if thread(tn).solver_stop=1 then exit for,for
					if pausetask=1 then do:sleep 10:loop until pausetask=0
					
				next it
			
			next rr
				
			if thread(tn).solver_stop=0 then
				
				#include "solver_advstats.bi"
				#include "solver_output.bi"
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub

sub bhdecrypt_seqhom_234567810g(byval tn_ptr as any ptr)
	
	#include "solver_variables.bi"
	
	dim as integer solver_output=0 'solver id
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short bnba(constcip)
	dim as ubyte sol(constcip)
	dim as ubyte key1(constcip)
	dim as ubyte key2(constcip)
	dim as ubyte stl(constcip)
	dim as short utp_nba(constcip)
	dim as short utp_sol(constcip)
	dim as ubyte ngrams(constcip)
	dim as short m1p(constcip)
	dim as short cyc(constcip)
	dim as short mape1(constcip)
	dim as short mape2(constcip)
	dim as double cycles(constfrq)
	dim as double cycle_table(constcip)
	dim as short frc(constcip)
	dim as double enttable(constent)
	dim as byte sr(10),lnb(0)
	dim as byte cribkey(0)
	dim as integer rl(40)
	
	cycle_table(1)=1
	for i=2 to constcip
		cycle_table(i)=(i*(i-1))^solvesub_cyclelengthweight '0.73
	next i
	
	dim as short maps(constcip)
	dim as short maps2(constcip)
	dim as integer mi,mj
	
	do 'wait for input
		
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 2,3,4,5:tempdiv=3
				case 6:tempdiv=2.75
				case 7:tempdiv=2.5
				case 8:tempdiv=2.25
				case 10:tempdiv=2
			end select
			
			#include "solver_settings.bi"
			
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			#include "ext_hc1.bi"
			
			for b=1 to bm
				
				#include "ext_hc2.bi"
				
				for i=1 to l*ngram_size
					enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
				next i
				
				frcmax=0
				for i=1 to l
					nba(i)=thread(tn).cip(i)
					frc(nba(i))+=1
					if frc(nba(i))>frcmax then frcmax=frc(nba(i))
				next i
				
				dim as short map1(s,frcmax)
				dim as short map2(s,frcmax*ngram_size)
				dim as ubyte sna(abc_sizem1,l)
				dim as short map1b(s,l) 'can be smaller ???
				dim as short map2b(s,frcmax*ngram_size)
				
				for i=1 to s
					frc(i)=0
					map1(i,0)=0
					map2(i,0)=0
					for j=0 to l
						map1b(i,j)=0
					next j
					maps(i)=i
					mape2(i)=0
				next i
				
				for i=1 to l
					map1(nba(i),0)+=1
					map1(nba(i),map1(nba(i),0))=i
					if local_pcmode=0 then
						map1b(nba(i),0)+=1
						map1b(nba(i),map1b(nba(i),0))=i
					else
						utp_nba(thread(tn).key(i))=nba(i)
						map1b(utp_nba(i),0)+=1
						map1b(utp_nba(i),map1b(utp_nba(i),0))=i
					end if
					mape1(i)=0
				next i
				
				for i=1 to l-(ngram_size-1) 'generalized entropy reduction for first/last ngs letters
					for j=0 to ngram_size-1
						mape1(i+j)+=1
					next j
				next i
				j=0
				for i=1 to l
					mape2(nba(i))+=mape1(i)
					j+=mape1(i)
				next i
				ent_score_norm=(l*ngram_size)/j
				
				for i=1 to l
					for j=0 to ngram_size-1
						h=i-(ngram_size-1)
						if h+j>0 andalso h+j<l-(ngram_size-2) then
							e=0
							for k=1 to map2(nba(i),0)
								if map2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								map2(nba(i),0)+=1
								map2(nba(i),map2(nba(i),0))=h+j
								map2b(nba(i),map2(nba(i),0))=j
							end if
						end if
					next j
				next i
				
				start_temp=(temp1/4.61538)/((s/l)/log(l))
				start_temp/=m_ioc2(nba(),l,s,2)^0.75
				curr_temp=start_temp
				ngf=(ngramfactor/1.4)*ent_score_norm
				ngf/=(1+((s/l)*multiplicityweight))
				ngfal=ngf/al
				best_score=0
				onesixl=1.7/l
				
				solution_timer=timer
				
				for lv=1 to lvmax
					
					for lr=1 to sr(lv)
						
						iterations=(iterations_total/sr(lv))/lvmax
				
						for rr=1 to random_restarts
							
							erase frq
							erase cycles
							
							for i=0 to abc_sizem1
								sna(i,0)=0
							next i
							
							if lv=1 then
								for i=1 to s
									state=48271*state and 2147483647
									new_letter=abc_size*state shr 31
									stl(i)=new_letter
									frq(new_letter)+=mape2(i) 'map1(i,0)
									sna(new_letter,0)+=1
									sna(new_letter,sna(new_letter,0))=i
									for j=1 to map1(i,0)
										sol(map1(i,j))=new_letter
									next j
								next i
							else
								for i=1 to s
									stl(i)=key2(i)
									frq(key2(i))+=mape2(i) 'map1(i,0)
									sna(key2(i),0)+=1
									sna(key2(i),sna(key2(i),0))=i
									for j=1 to map1(i,0)
										sol(map1(i,j))=key2(i)
									next j
								next i
							end if
							
							entropy=0
							for i=0 to abc_sizem1
								entropy+=enttable(frq(i))
							next i
							
							mi=0
							mj=0
							#include "solver_picksymbol.bi"
							
							old_score=0
							'curr_symbol=1
							temp=curr_temp
							temp_min=temp/iterations
							new_ngram_score=0
							new_cycle_score=0
							
							#include "solver_ngram_init.bi"
							
							for h=0 to abc_sizem1
								if frq(h)>0 then
									cl=0
									cs=sna(h,0)
									for i=1 to cs
										m1p(i)=1
									next i
									do
										low=l+1
										for i=1 to cs
											if map1b(sna(h,i),m1p(i))>0 then
												if map1b(sna(h,i),m1p(i))<low then
													mii=i
													low=map1b(sna(h,i),m1p(i))
												end if
											end if
										next i
										if low<>l+1 then
											cl+=1
											cyc(cl)=mii
											m1p(mii)+=1
										end if
									loop until low=l+1
									cal=0
									tcs=0
									if cs=1 then
										tcs+=cycle_table(cl)
									else
										for i=1 to cl-(cs-1)
											e=1
											for j=i to i+(cs-2)
												for k=j+1 to i+(cs-1)
													if cyc(j)=cyc(k) then
														e=0
														exit for,for
													end if
												next k
											next j
											if e=1 then cal+=1
										next i
										tcs+=cycle_table(cal)
									end if
									cycles(h)=tcs
									new_cycle_score+=tcs	
								end if	
							next h
							
							mc=5
							mc_minus=(mc-1)/iterations
							
							for it=1 to iterations
								
								'---------------------------------------------------------------------
								
								old_letter=stl(curr_symbol)
								
								if lv=lvmax then mc-=mc_minus
								
								state=48271*state and 2147483647
								d=4*state shr 31
								
								#include "solver_pickletter.bi"
								
								for i=1 to map1(curr_symbol,0)
									sol(map1(curr_symbol,i))=new_letter
								next i
								
								old_entropy=entropy
								entropy+=enttable(frq(old_letter)-mape2(curr_symbol))-enttable(frq(old_letter))
								entropy+=enttable(frq(new_letter)+mape2(curr_symbol))-enttable(frq(new_letter))
								
								j=0
								sna(new_letter,0)+=1
								sna(new_letter,sna(new_letter,0))=curr_symbol
								sna(old_letter,0)-=1
								for i=1 to sna(old_letter,0)
									if sna(old_letter,i)=curr_symbol then j+=1
									sna(old_letter,i)=sna(old_letter,i+j) 
								next i
								
								old_ngram_score=new_ngram_score
								
								#include "solver_ngram_main.bi"
										
								'-------------------- get old_letter cycle score --------------------
								
								old_cycle_score=new_cycle_score
								
								if frq(old_letter)-map1(curr_symbol,0)>0 then
									cl=0
									cs=sna(old_letter,0)
									for i=1 to cs
										m1p(i)=1
									next i
									do
										low=l+1
										for i=1 to cs
											if map1b(sna(old_letter,i),m1p(i))>0 then
												if map1b(sna(old_letter,i),m1p(i))<low then
													mii=i
													low=map1b(sna(old_letter,i),m1p(i))
												end if
											end if
										next i
										if low<>l+1 then
											cl+=1
											cyc(cl)=mii
											m1p(mii)+=1
										end if
									loop until low=l+1
									cal=0
									tcs=0
									if cs=1 then
										tcs+=cycle_table(cl)
									else
										for i=1 to cl-(cs-1)
											e=1
											for j=i to i+(cs-2)
												for k=j+1 to i+(cs-1)
													if cyc(j)=cyc(k) then
														e=0
														exit for,for
													end if
												next k
											next j
											if e=1 then cal+=1
										next i
										tcs+=cycle_table(cal)
									end if
									cycle_old=cycles(old_letter)
									cycles(old_letter)=tcs
								else
									cycle_old=cycles(old_letter)
									cycles(old_letter)=0
								end if
								new_cycle_score+=cycles(old_letter)-cycle_old
								
								'-------------------- get new_letter cycle score --------------------
								
								cl=0
								cs=sna(new_letter,0)
								for i=1 to cs
									m1p(i)=1
								next i
								do
									low=l+1
									for i=1 to cs
										if map1b(sna(new_letter,i),m1p(i))>0 then
											if map1b(sna(new_letter,i),m1p(i))<low then
												mii=i
												low=map1b(sna(new_letter,i),m1p(i))
											end if
										end if
									next i
									if low<>l+1 then
										cl+=1
										cyc(cl)=mii
										m1p(mii)+=1
									end if
								loop until low=l+1
								cal=0
								tcs=0
								if cs=1 then
									tcs+=cycle_table(cl)
								else
									for i=1 to cl-(cs-1)
										e=1
										for j=i to i+(cs-2)
											for k=j+1 to i+(cs-1)
												if cyc(j)=cyc(k) then
													e=0
													exit for,for
												end if
											next k
										next j
										if e=1 then cal+=1
									next i
									tcs+=cycle_table(cal)
								end if
								cycle_new=cycles(new_letter)
								cycles(new_letter)=tcs
								new_cycle_score+=cycles(new_letter)-cycle_new
								
								#include "solver_fastent.bi"
								
								new_score+=(10000*solvesub_seqweight)*(new_cycle_score/cycle_table(l-1))
								'new_score/=1.574
								
								'---------------------------------------------------------------------
								
								if new_score>old_score then	
									
									frq(old_letter)-=mape2(curr_symbol)
									frq(new_letter)+=mape2(curr_symbol)
									stl(curr_symbol)=new_letter
									
									#include "solver_ngram_tail.bi"
									
									old_score=new_score
									
									#include "solver_picksymbol.bi"
									
									if new_score>best_score then
										
										solution_improved=1
										thread(tn).sectime=timer-sectimer
										for i=1 to l
											thread(tn).sol(i)=alphabet(sol(i))
										next i
										thread(tn).ent=entropy
										thread(tn).multiplicity=s/l
										best_score=new_score+0.00001
										for i=1 to s
											key1(i)=stl(i)
										next i
										
									end if
									
									if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
										solution_improved=0 'consume
										solution_timer=timer
										#include "solver_ioc.bi"
										#include "solver_advstats.bi"
										thread(tn).score=best_score
										#include "solver_output.bi"
										#include "ext_hc3.bi"
									end if
									
								else	
									
									j=0
									sna(old_letter,0)+=1
									sna(old_letter,sna(old_letter,0))=curr_symbol
									sna(new_letter,0)-=1
									for i=1 to sna(new_letter,0)
										if sna(new_letter,i)=curr_symbol then j+=1
										sna(new_letter,i)=sna(new_letter,i+j) 
									next i
						
									cycles(old_letter)=cycle_old
									cycles(new_letter)=cycle_new	
									new_cycle_score=old_cycle_score
									new_ngram_score=old_ngram_score
									entropy=old_entropy
									
									for i=1 to map1(curr_symbol,0)
										sol(map1(curr_symbol,i))=old_letter
									next i
									
									#include "solver_picksymbol.bi"
									
									old_score-=temp*map1(curr_symbol,0)*onesixl*old_score/new_score
								
								end if
								
								temp-=temp_min
								
								thread(tn).iterations_completed+=1
								if thread(tn).solver_stop=1 then exit for,for,for,for,for
								if pausetask=1 then do:sleep 10:loop until pausetask=0
								
							next it
							
						next rr
						
				   next lr
				
					for i=1 to s
						key2(i)=key1(i)
					next i
					
					curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
				
				next lv
			
			next b
			
			if thread(tn).solver_stop=0 then
				
				#include "ext_hc4.bi"
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub

sub bhdecrypt_rowbound_34567g(byval tn_ptr as any ptr)
	
	#include "solver_variables.bi"
	
	dim as integer solver_output=2
	
	ngrf=2 'n-gram range from
	ngrt=ngram_size 'n-gram range to
	
	dim as integer ngrams0(10)
	dim as ubyte ngrams(ngrf to ngrt,constcip)
	dim as ubyte key1(constcip)
	dim as ubyte key2(constcip)
	dim as ubyte sol(constcip)
	dim as ubyte stl(constcip)
	dim as ubyte stlp(constcip)
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short lnb(constcip)
	'dim as short bnba(constcip) 'for ext hill climber
	dim as ubyte row(constcip)
	dim as short frc(constcip)
	dim as double enttable(constcip)
	dim as double ngramentweight(10)
	dim as uinteger new_ngramscore(10)
	dim as uinteger old_ngramscore(10)
	dim as double ngfa(10)
	dim as short pow2(10)
	dim as byte sr(10)
	dim as byte cribkey(constcip)

	for i=0 to ngrt
		pow2(i)=2^i '*(ngs/ngrt)
	next i
	
	dim as short maps(constcip)
	dim as short maps2(constcip)
	dim as integer mi,mj
	
	do 'wait for input
		
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 2,3,4,5,6,7,8:tempdiv=3
			end select
			
			for i=ngrf to ngrt
				ngramentweight(i)=solvesub_ngramentweight(i)
			next i
			
			#include "solver_settings.bi"
			
			if use_cribs>0 then
				for i=1 to s-1
					cribkey(i)=thread(tn).ckey(i)
				next i
			end if
			
			ll=l*(l-1)
			bm=1
			
			'#include "ext_hc1.bi"
			
			for b=1 to bm	
				
				'#include "ext_hc2.bi"
				
				for i=1 to l
					enttable(i)=abs(logbx(i/l,2)*(i/l))
				next i
				
				j=0
				frcmax=0
				erase frc
				for i=1 to constcip
					if thread(tn).cip(i)=0 then exit for
					if thread(tn).cip(i)=12345 then
						lnb(j)=1
					else
						j+=1
						lnb(j)=0
						nba(j)=thread(tn).cip(i)
						frc(nba(j))+=1
						if frc(nba(j))>frcmax then frcmax=frc(nba(j))
					end if
				next i
				
				dim as short map1(s,frcmax)
				dim as short map2(ngrf to ngrt,s,frcmax*ngrt)
				dim as uinteger fm3(abc_sizem1,abc_sizem1,abc_sizem1)
				
				for i=0 to abc_sizem1
					for j=0 to abc_sizem1
						for k=0 to abc_sizem1
							fm3(i,j,k)=0
						next k
					next j
				next i
				
				for i=1 to s
					map1(i,0)=0
					for ngs=ngrf to ngrt 'n-gram range
						map2(ngs,i,0)=0
					next ngs
					maps(i)=i
				next i
				
				for i=1 to l
					map1(nba(i),0)+=1
					map1(nba(i),map1(nba(i),0))=i
				next i
				
				for i=1 to l
					for ngs=ngrf to ngrt
						for j=0 to ngs-1
							e=0
							h=i-(ngs-1)
							if ngs=ngrt then
								for k=0 to ngs-2
									if lnb(h+j+k)=1 then 'skip line break
										e=1
										exit for
									end if
								next k
							else
								if lnb(h+j+(ngs-1))=1 or h+j+(ngs-1)=l then
									for k=0 to ngs-2
										if lnb(h+j+k)=1 then 'skip line break
											e=1
											exit for
										end if
									next k
								else 
									e=1
								end if
							end if
							if e=0 andalso h+j>0 andalso h+j<l-(ngs-2) then
								for k=1 to map2(ngs,nba(i),0)
									if map2(ngs,nba(i),k)=h+j then
										e=1
										exit for
									end if
								next k
								if e=0 then
									map2(ngs,nba(i),0)+=1
									map2(ngs,nba(i),map2(ngs,nba(i),0))=h+j
								end if
							end if
						next j
					next ngs
				next i
				
				'select case ngram_size
				'	case 2:temp1=25
				'	case 3:temp1=150
				'	case 4:temp1=400
				'	case 5:temp1=625
				'	case 6,7,8:temp1=975
				'end select
				
				start_temp=((temp1/4.61538)/2)/((s/l)/log(l)) '/2
				start_temp/=m_ioc2(nba(),l,s,2)^0.75
				curr_temp=start_temp
				best_score=0
				onesixl=1.6/l
				
				solution_timer=timer
				
				for ngs=ngrf to ngrt
					ngfa(ngs)=solvesub_ngramfactor2(ngs)/(1+((s/l)*multiplicityweight))
				next ngs
				
				for lv=1 to lvmax
					
					for lr=1 to sr(lv)
						
						iterations=(iterations_total/sr(lv))/lvmax
						'if iterations=0 then iterations=1
						
						for rr=1 to random_restarts
							
							erase frq
							
							if lv=1 then
								for i=1 to s
									
									if use_cribs=0 then	
										state=48271*state and 2147483647
										new_letter=abc_size*state shr 31
									else
										if cribkey(i)=0 then
											state=48271*state and 2147483647
											new_letter=abc_size*state shr 31
										else
											new_letter=cribkey(i)-1
										end if
									end if
									stl(i)=new_letter
									frq(new_letter)+=map1(i,0)
									for j=1 to map1(i,0)
										sol(map1(i,j))=new_letter
									next j
									
									'state=48271*state and 2147483647
									'new_letter=abc_size*state shr 31
									'stl(i)=new_letter
									'frq(new_letter)+=map1(i,0)
									'for j=1 to map1(i,0)
									'	sol(map1(i,j))=new_letter
									'next j
									
								next i
							else
								for i=1 to s
									stl(i)=key2(i)
									frq(key2(i))+=map1(i,0)
									for j=1 to map1(i,0)
										sol(map1(i,j))=key2(i)
									next j
								next i
							end if
							
							entropy=0
							for i=0 to abc_sizem1
								entropy+=enttable(frq(i))
							next i
							
							mi=0
							mj=0
							#include "solver_picksymbol.bi"
							
							old_score=0
							temp=curr_temp
							temp_min=temp/iterations
							
							for ngs=ngrf to ngrt
								ngrams0(ngs)=0
								new_ngramscore(ngs)=0
							next ngs
							
							for i=1 to l
								for ngs=ngrf to ngrt
									if i<l-(ngs-2) then
										e=0
										if ngs=ngrt then
											for j=0 to ngs-2
												if lnb(i+j)=1 then
													e=1
													exit for
												end if
											next j
										else
											if i+(ngs-1)=l then
												for j=0 to ngs-2
													if lnb(i+j)=1 then
														e=1
														exit for
													end if
												next j
											else
												if lnb(i+(ngs-1))=1 then
													for j=0 to ngs-2
														if lnb(i+j)=1 then
															e=1
															exit for
														end if
													next j
												else
													e=1
												end if
											end if
										end if
										if e=0 then
											ngrams0(ngs)+=1
											select case ngs
												case 2:ngrams(ngs,i)=g2(sol(i),sol(i+1))
												case 3:ngrams(ngs,i)=g3(sol(i),sol(i+1),sol(i+2))
												case 4:ngrams(ngs,i)=g4(sol(i),sol(i+1),sol(i+2),sol(i+3))
												case 5:ngrams(ngs,i)=g5(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4))
												case 6:ngrams(ngs,i)=g6(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
												case 7:ngrams(ngs,i)=g7(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
											end select
											new_ngramscore(ngs)+=ngrams(ngs,i)
										end if
									end if
								next ngs
							next i
							
							avgngs=0
							total_ngrams=0
							total_ngrams2=0
							for ngs=ngrf to ngrt
								avgngs+=ngrams0(ngs)*ngs
								total_ngrams+=ngrams0(ngs)/pow2(ngrt-ngs)
								total_ngrams2+=ngrams0(ngs)
							next ngs
							avgngs/=total_ngrams2
							
							for it=1 to iterations
								
								'---------------------------------------------------------------------
								
								if use_cribs<2 then 'short-circuit
								
									old_letter=stl(curr_symbol)
									older_letter=stlp(curr_symbol)
									
									state=48271*state and 2147483647
									new_letter=abc_sizem1*state shr 31
									if new_letter=old_letter then new_letter=abc_sizem1
									state=48271*state and 2147483647
									nl2=abc_sizem1*state shr 31
									if nl2=old_letter then nl2=abc_sizem1
									new_letter=iif(fm3(older_letter,old_letter,new_letter)>=fm3(older_letter,old_letter,nl2),new_letter,nl2)
									
									for i=1 to map1(curr_symbol,0)
										sol(map1(curr_symbol,i))=new_letter
									next i
									
									old_entropy=entropy
									entropy+=enttable(frq(old_letter)-map1(curr_symbol,0))-enttable(frq(old_letter))
									entropy+=enttable(frq(new_letter)+map1(curr_symbol,0))-enttable(frq(new_letter))
								
								end if
								
								new_score=0
								for ngs=ngrf to ngrt
									old_ngramscore(ngs)=new_ngramscore(ngs)
									for i=1 to map2(ngs,curr_symbol,0)
										j=map2(ngs,curr_symbol,i)
										select case ngs
											case 2:new_ngramscore(ngs)+=g2(sol(j),sol(j+1))-ngrams(ngs,j)
											case 3:new_ngramscore(ngs)+=g3(sol(j),sol(j+1),sol(j+2))-ngrams(ngs,j)
											case 4:new_ngramscore(ngs)+=g4(sol(j),sol(j+1),sol(j+2),sol(j+3))-ngrams(ngs,j)
											case 5:new_ngramscore(ngs)+=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))-ngrams(ngs,j)
											case 6:new_ngramscore(ngs)+=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))-ngrams(ngs,j)
											case 7:new_ngramscore(ngs)+=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))-ngrams(ngs,j)
										end select
									next i
									select case ngramentweight(ngs)
										case 0.25:new_score+=(new_ngramscore(ngs)*(ngfa(ngs))*entropy^0.25)/pow2(ngrt-ngs)
										case 0.5:new_score+=(new_ngramscore(ngs)*(ngfa(ngs))*entropy^0.5)/pow2(ngrt-ngs)
										case 0.75:new_score+=(new_ngramscore(ngs)*(ngfa(ngs))*entropy^0.75)/pow2(ngrt-ngs)
										case 1:new_score+=(new_ngramscore(ngs)*(ngfa(ngs))*entropy)/pow2(ngrt-ngs)
										case 1.5:new_score+=(new_ngramscore(ngs)*(ngfa(ngs))*entropy^1.5)/pow2(ngrt-ngs)
										case 2:new_score+=(new_ngramscore(ngs)*(ngfa(ngs))*entropy*entropy)/pow2(ngrt-ngs)
										case else:new_score+=(new_ngramscore(ngs)*(ngfa(ngs))*fastpow1_single(entropy,ngramentweight(ngs)))/pow2(ngrt-ngs)
									end select
								next ngs
								new_score/=total_ngrams
								'new_score/=(avgngs/solvesub_matchweight) 'test for fragments hill-climber
								
								'---------------------------------------------------------------------
								
								if new_score>old_score then
									
									fm3(older_letter,old_letter,new_letter)+=1
									frq(old_letter)-=map1(curr_symbol,0)
									frq(new_letter)+=map1(curr_symbol,0)	
									stl(curr_symbol)=new_letter
									stlp(curr_symbol)=old_letter
									
									for ngs=ngrf to ngrt
										for i=1 to map2(ngs,curr_symbol,0)
											j=map2(ngs,curr_symbol,i)
											select case ngs
												case 2:ngrams(ngs,j)=g2(sol(j),sol(j+1))
												case 3:ngrams(ngs,j)=g3(sol(j),sol(j+1),sol(j+2))
												case 4:ngrams(ngs,j)=g4(sol(j),sol(j+1),sol(j+2),sol(j+3))
												case 5:ngrams(ngs,j)=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
												case 6:ngrams(ngs,j)=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))
												case 7:ngrams(ngs,j)=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))
											end select
										next i
									next ngs
									
									old_score=new_score
									
									#include "solver_picksymbol.bi"
									
									if new_score>best_score then
										
										solution_improved=1
										thread(tn).sectime=timer-sectimer
										thread(tn).ent=entropy
										thread(tn).tmpd1=avgngs
										thread(tn).multiplicity=s/l
										
										j=0:h=0
										for i=1 to l 'calculate individual row scores
											j+=1
											row(j)=sol(i)
											if lnb(i)=1 or i=l then
												rowscore=0
												select case ngrt
													case 3
														for k=1 to j-2
															rowscore+=g3(row(k),row(k+1),row(k+2))/total_ngrams*ngfa(3)*entropy^ngramentweight(3)
														next k
														if j>1 then rowscore+=g2(row(j-1),row(j))/total_ngrams*ngfa(2)*entropy^ngramentweight(2)/pow2(1)
													case 4
														for k=1 to j-3
															rowscore+=g4(row(k),row(k+1),row(k+2),row(k+3))/total_ngrams*ngfa(4)*entropy^ngramentweight(4)
														next k
														if j>1 then rowscore+=g2(row(j-1),row(j))/total_ngrams*ngfa(2)*entropy^ngramentweight(2)/pow2(2)
														if j>2 then rowscore+=g3(row(j-2),row(j-1),row(j))/total_ngrams*ngfa(3)*entropy^ngramentweight(3)/pow2(1)
													case 5
														for k=1 to j-4
															rowscore+=g5(row(k),row(k+1),row(k+2),row(k+3),row(k+4))/total_ngrams*ngfa(5)*entropy^ngramentweight(5)
														next k
														if j>1 then rowscore+=g2(row(j-1),row(j))/total_ngrams*ngfa(2)*entropy^ngramentweight(2)/pow2(3)
														if j>2 then rowscore+=g3(row(j-2),row(j-1),row(j))/total_ngrams*ngfa(3)*entropy^ngramentweight(3)/pow2(2)
														if j>3 then rowscore+=g4(row(j-3),row(j-2),row(j-1),row(j))/total_ngrams*ngfa(4)*entropy^ngramentweight(4)/pow2(1)
													case 6
														for k=1 to j-5
															rowscore+=g6(row(k),row(k+1),row(k+2),row(k+3),row(k+4),row(k+5))/total_ngrams*ngfa(6)*entropy^ngramentweight(6)
														next k
														if j>1 then rowscore+=g2(row(j-1),row(j))/total_ngrams*ngfa(2)*entropy^ngramentweight(2)/pow2(4)
														if j>2 then rowscore+=g3(row(j-2),row(j-1),row(j))/total_ngrams*ngfa(3)*entropy^ngramentweight(3)/pow2(3)
														if j>3 then rowscore+=g4(row(j-3),row(j-2),row(j-1),row(j))/total_ngrams*ngfa(4)*entropy^ngramentweight(4)/pow2(2)
														if j>4 then rowscore+=g5(row(j-4),row(j-3),row(j-2),row(j-1),row(j))/total_ngrams*ngfa(5)*entropy^ngramentweight(5)/pow2(1)
													case 7
														for k=1 to j-6
															rowscore+=g7(row(k),row(k+1),row(k+2),row(k+3),row(k+4),row(k+5),row(k+6))/total_ngrams*ngfa(7)*entropy^ngramentweight(7)
														next k
														if j>1 then rowscore+=g2(row(j-1),row(j))/total_ngrams*ngfa(2)*entropy^ngramentweight(2)/pow2(5)
														if j>2 then rowscore+=g3(row(j-2),row(j-1),row(j))/total_ngrams*ngfa(3)*entropy^ngramentweight(3)/pow2(4)
														if j>3 then rowscore+=g4(row(j-3),row(j-2),row(j-1),row(j))/total_ngrams*ngfa(4)*entropy^ngramentweight(4)/pow2(3)
														if j>4 then rowscore+=g5(row(j-4),row(j-3),row(j-2),row(j-1),row(j))/total_ngrams*ngfa(5)*entropy^ngramentweight(5)/pow2(2)
														if j>5 then rowscore+=g6(row(j-5),row(j-4),row(j-3),row(j-2),row(j-1),row(j))/total_ngrams*ngfa(6)*entropy^ngramentweight(6)/pow2(1)
												end select
												j=0:h+=1
												thread(tn).graph(h)=rowscore
											end if
										next i
										
										for i=1 to l
											thread(tn).sol(i)=alphabet(sol(i)) 'use alphabet for normal mode
										next i
										best_score=new_score+0.00001
										
										for i=1 to s
											key1(i)=stl(i)
										next i
										
									end if
									
									if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
										solution_improved=0 'consume
										solution_timer=timer
										#include "solver_ioc.bi"
										#include "solver_advstats.bi"
										thread(tn).score=best_score
										#include "solver_output.bi"
										'#include "ext_hc3.bi"
									end if
									
								else
									
									for i=ngrf to ngrt
										new_ngramscore(i)=old_ngramscore(i)
									next i
									
									entropy=old_entropy
									
									for i=1 to map1(curr_symbol,0)
										sol(map1(curr_symbol,i))=old_letter
									next i
									
									#include "solver_picksymbol.bi"
									
									old_score-=temp*map1(curr_symbol,0)*onesixl*old_score/new_score
									
								end if
								
								temp-=temp_min
								
								if use_cribs=2 then exit for,for,for,for,for 'short-circuit
								thread(tn).iterations_completed+=1
								if thread(tn).solver_stop=1 then exit for,for,for,for,for
								if pausetask=1 then do:sleep 10:loop until pausetask=0
								
							next it
							
						next rr
					
					next lr
					
					for i=1 to s
						key2(i)=key1(i)
					next i
					
					curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
					
				next lv
				
			next b
			
			if thread(tn).solver_stop=0 then
				
				'#include "ext_hc4.bi"
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				if use_cribs<2 then 
					thread(tn).avgscore+=best_score
					thread(tn).avgioc+=thread(tn).ioc
					thread(tn).avgpccycles+=thread(tn).pccycles
					thread(tn).restarts_completed+=1
				end if
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub

sub bhdecrypt_234567810g(byval tn_ptr as any ptr)
	
	#include "solver_variables.bi"
	
	dim as integer solver_output=0 'solver id
	dim as ubyte key1(constcip)
	dim as ubyte key2(constcip)
	dim as ubyte sol(constcip)
	dim as ubyte stl(constcip)
	dim as ubyte ngrams(constcip)
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short bnba(constcip)
	dim as short frc(constcip)
	dim as short mape1(constcip)
	dim as short mape2(constcip)
	dim as byte cribkey(constcip)
	dim as double enttable(constent)
	dim as byte sr(10),lnb(0)
	dim as ushort wordgrams(constcip)
	dim as ushort wordgrams2(constcip)
				
	dim as short maps(constcip)
	dim as short maps2(constcip)
	dim as integer mi,mj
		
	do 'wait for input
		
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 2,3,4,5:tempdiv=3
				case 6:tempdiv=2.75
				case 7:tempdiv=2.5
				case 8:tempdiv=2.25
				case 10:tempdiv=2
			end select
			
			#include "solver_settings.bi"
			
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			if use_cribs=1 then
				for i=1 to s
					cribkey(i)=thread(tn).ckey(i)
				next i
			end if
			
			#include "ext_hc1.bi"
			
			for b=1 to bm
				
				#include "ext_hc2.bi" 'may reinitialize l and s for hill climbers
				
				for i=1 to l*ngram_size
					enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
				next i
				
				frcmax=0
				for i=1 to l
					nba(i)=thread(tn).cip(i)
					frc(nba(i))+=1
					if frc(nba(i))>frcmax then frcmax=frc(nba(i))
				next i
				
				dim as short map1(s,frcmax)
				dim as short map2(s,frcmax*ngram_size)
				dim as short wordmap2(s,frcmax*wngs)
				dim as short map2b(s,frcmax*ngram_size)
				
				for i=1 to s
					frc(i)=0
					map1(i,0)=0
					map2(i,0)=0
					wordmap2(i,0)=0
					maps(i)=i
					mape2(i)=0
				next i
				
				for i=1 to l
					map1(nba(i),0)+=1
					map1(nba(i),map1(nba(i),0))=i
					mape1(i)=0
				next i
				
				for i=1 to l-(ngram_size-1) 'entropy reduction
					for j=0 to ngram_size-1
						mape1(i+j)+=1
					next j
				next i
				
				for i=1 to l 'entropy non-reduction (temp lazy hack to test)
					mape1(i)=ngram_size
				next i
				
				j=0
				for i=1 to l
					mape2(nba(i))+=mape1(i)
					j+=mape1(i)
				next i
				ent_score_norm=(l*ngram_size)/j
				
				for i=1 to l
					for j=0 to ngram_size-1
						h=i-(ngram_size-1)
						if h+j>0 andalso h+j<=l then '-(ngram_size-2) then 'h+j<l-(ngram_size-2) then '' beijinghouse add wrap
							e=0
							for k=1 to map2(nba(i),0)
								if map2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								map2(nba(i),0)+=1
								map2(nba(i),map2(nba(i),0))=h+j
								map2b(nba(i),map2(nba(i),0))=j
							end if
						end if
					next j
				next i

				for i=1 to l
					for j=0 to wngs-1
						h=i-(wngs-1)
						if h+j>0 andalso h+j<l-(wngs-2) then
							e=0
							for k=1 to wordmap2(nba(i),0)
								if wordmap2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								wordmap2(nba(i),0)+=1
								wordmap2(nba(i),wordmap2(nba(i),0))=h+j
							end if
						end if
					next j
				next i

				start_temp=(temp1/4.61538)/((s/l)/log(l)) 'temp1/(s/l)
				start_temp/=m_ioc2(nba(),l,s,2)^0.75
				curr_temp=start_temp
				ngf=ngramfactor*ent_score_norm
				ngf/=1+((s/l)*multiplicityweight)
				ngfal=ngf/al
				onesixl=1.7/l
				best_score=0
				
				' beijinghouse: allow nearly instant output of first pass
		'		solution_timer=timer-solvesub_solutionreleasetimer
				
				for lv=1 to lvmax
					
					for lr=1 to sr(lv)
						
						iterations=(iterations_total/sr(lv))/lvmax
						
						for rr=1 to random_restarts
							
							erase frq
							
							if lv=1 then
								for i=1 to s
									if use_cribs=0 then
										state=48271*state and 2147483647
										new_letter=abc_size*state shr 31
									else
										if cribkey(i)=0 then
											state=48271*state and 2147483647
											new_letter=abc_size*state shr 31
										else
											new_letter=cribkey(i)-1
										end if
									end if
									stl(i)=new_letter
									frq(new_letter)+=mape2(i)
									for j=1 to map1(i,0)
										sol(map1(i,j))=new_letter
									next j
								next i
							else
								for i=1 to s
									stl(i)=key2(i)
									frq(key2(i))+=mape2(i)
									for j=1 to map1(i,0)
										sol(map1(i,j))=key2(i)
									next j
								next i
							end if
							
							entropy=0
							for i=0 to abc_sizem1
								entropy+=enttable(frq(i))
							next i
							
							mi=0
							mj=0
							
							old_score=0
							temp=curr_temp
							temp_min=temp/iterations
							new_ngram_score=0

							' lazy version, add better one later if this code helps/works
							for i=l+1 to l+ngram_size-1
								sol(i)=sol(i-1) ' add copy of first few letters past end of cipher so can get full ngram scores
							next i

							#include "solver_ngram_init.bi"

							for i=1 to l-(wngs-1)
									if solvesub_7gwordgrams=0 then
										wordgrams(i)=g6w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
									else
										wordgrams(i)=g7w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
									end if
							next i

							#include "solver_bonusgram_init.bi"
							
							mc=5
							mc_minus=(mc-1)/iterations
							
							accept=1
							
							for it=1 to iterations
								
								'---------------------------------------------------------------------
								
								#include "solver_picksymbol.bi"
								
								if accept=0 then old_score-=temp*map1(curr_symbol,0)*onesixl*old_score/new_score
								'if accept=0 then old_score-=temp*map2(curr_symbol,0)/al*(old_score/new_score)
								
								temp-=temp_min
								
								old_letter=stl(curr_symbol)
								
								if lv=lvmax then mc-=mc_minus
								state=48271*state and 2147483647
								d=4*state shr 31
								
								#include "solver_pickletter.bi"
																
								for i=1 to map1(curr_symbol,0)
									sol(map1(curr_symbol,i))=new_letter
								next i
								
								' lazy version, add better one later if this code helps/works
								for i=l+1 to l+ngram_size-1
									sol(i)=sol(i-1) ' add copy of first few letters past end of cipher so can get full ngram scores
								next i

								
								old_entropy=entropy
								entropy+=enttable(frq(old_letter)-mape2(curr_symbol))-enttable(frq(old_letter))
								entropy+=enttable(frq(new_letter)+mape2(curr_symbol))-enttable(frq(new_letter))
								
								old_ngram_score=new_ngram_score

								#include "solver_bonusgram_main.bi"

								#include "solver_ngram_main.bi"
								#include "solver_fastent.bi"

								#include "solver_wordscore.bi"
								
								new_score*=1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size) 'multiplicative


								if new_score>old_score then
									
									accept=1
									
									#include "solver_bonusgram_tail.bi"

									frq(old_letter)-=mape2(curr_symbol)
									frq(new_letter)+=mape2(curr_symbol)
									stl(curr_symbol)=new_letter

									for i=1 to l-(wngs-1) 'save the new values since they worked
										wordgrams(i)=wordgrams2(i)
									next i

									#include "solver_ngram_tail.bi"
									
									old_score=new_score
									
									if new_score>best_score then
										
										solution_improved=1
										thread(tn).sectime=timer-sectimer
										for i=1 to l
											thread(tn).sol(i)=alphabet(sol(i))
										next i
										thread(tn).ent=entropy
										thread(tn).multiplicity=s/l
										best_score=new_score+0.00001
										
										#include "solver_accuracy_shortcircuit.bi" 'inflates 100% accuracy
										
										for i=1 to s
											key1(i)=stl(i)
										next i
										
									end if
									
									if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
										solution_improved=0
										solution_timer=timer
										#include "solver_ioc.bi"
										#include "solver_advstats.bi"
										thread(tn).score=best_score
										#include "solver_output.bi"
										#include "ext_hc3.bi"
									end if
									
								else
									
									accept=0
									new_ngram_score=old_ngram_score
									entropy=old_entropy
									
									for i=1 to map1(curr_symbol,0)
										sol(map1(curr_symbol,i))=old_letter
									next i

									' lazy version, add better one later if this code helps/works
									for i=l+1 to l+ngram_size-1
										sol(i)=sol(i-1) ' add copy of first few letters past end of cipher so can get full ngram scores
									next i

								end if
								
								thread(tn).iterations_completed+=1
								if thread(tn).solver_stop=1 then exit for,for,for,for,for
								if pausetask=1 then do:sleep 10:loop until pausetask=0
								
							next it
						
						next rr
					
					next lr
					
					for i=1 to s
						key2(i)=key1(i)
					next i
					
					curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
					
				next lv
				
			next b
			
			if thread(tn).solver_stop=0 then
				
				#include "ext_hc4.bi"
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub

sub bhdecrypt_higherorder_810g(byval tn_ptr as any ptr)
	
	dim as short tn=cint(tn_ptr)
	
	thread(tn).thread_active=1
	thread(tn).thread_stop=0
	thread(tn).solver_waiting=1
	
	dim as integer local_outputbatch,local_pcmode,local_outputdir,local_outputimp
	dim as integer h,i,j,k,l,s,e,nl2,ngs,al,lv,lr,lvmax,frcmax,improved,local_advstats
	dim as integer abc_size,abc_sizem1,new_letter(4),old_letter(4),curr_symbol(4),older_letter(4)
	dim as integer ll,b,bm,new_ngram_score(4),old_ngram_score(4),bl,ioc_int,n,ni,nm,solution_improved
	dim as uinteger state(4),it,iterations,iterations_total,rr,random_restarts,seed=tn
	dim as double local_over,new_score(4),new_score_hss(4),old_score(4),best_score,ioc,temp(4),temp_min(4),start_temp,temp1
	dim as double bbest,bioc,entweight,ngramfactor,multiplicityweight,curr_temp,ngf,ngfal,solution_timer
	dim as double entropy(4),old_entropy(4),onesixl,best_keyscore(4),d,ent_score_norm,tempdiv
	dim as string filename,solstring
	
	dim as ubyte key1(4,constcip)
	dim as ubyte key2(4,constcip)
	dim as ubyte sol(4,constcip)
	dim as ubyte stl(4,constcip)
	dim as ubyte ngrams(4,constcip)
	dim as long frq(4,constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short bnba(constcip)
	dim as short frc(constcip)
	dim as short mape1(constcip)
	dim as short mape2(constcip)
	dim as byte cribkey(constcip)
	dim as double enttable(constent)
	dim as byte sr(10),lnb(0)
	
	ngs=ngram_size
	
	dim as integer solver_output=4
	dim as integer blt,bls
	dim as double mc,mc_minus
	
	dim as short maps(4,constcip)
	dim as short maps2(4,constcip)
	dim as integer mi(4),mj(4)
	
	dim as integer z,z1,z2
	
	do 'wait for input
		
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			ni=solvesub_higherorderhomophonic-1
			
			for n=0 to ni
				seed+=threads
				if (seed*2)-1>2147483647 then seed=tn
				state(n)=(seed*2)-1
			next n
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 8:tempdiv=2
				'case 10:tempdiv=2
			end select
			
			#include "solver_settings.bi"
			
			ll=l*(l-1)
			al=l-(ngram_size-1)
			nm=((ni+1)*ni)/2
			
			'if use_cribs=1 then
			'	for i=1 to s
			'		cribkey(i)=thread(tn).ckey(i)
			'	next i
			'end if
			
			'#include "ext_hc1.bi"
			
			bm=1
			
			for b=1 to bm
				
				'#include "ext_hc2.bi" 'may reinitialize l and s for hill climbers
				
				frcmax=0
				for i=1 to l
					nba(i)=thread(tn).cip(i)
					frc(nba(i))+=1
					if frc(nba(i))>frcmax then frcmax=frc(nba(i))
				next i
				
				dim as short map1(s,frcmax)
				dim as short map2(s,frcmax*ngram_size)
				dim as short map2b(s,frcmax*ngram_size)
				
				for i=1 to s
					frc(i)=0
					map1(i,0)=0
					map2(i,0)=0
					for n=0 to ni
						maps(n,i)=i
					next n
					mape2(i)=0
				next i
				
				for i=1 to l
					map1(nba(i),0)+=1
					map1(nba(i),map1(nba(i),0))=i
					mape1(i)=0
				next i
				
				for i=1 to l-(ngram_size-1) 'generalized entropy reduction for first/last ngs letters
					for j=0 to ngram_size-1
						mape1(i+j)+=1
					next j
				next i
				j=0
				for i=1 to l
					mape2(nba(i))+=mape1(i)
					j+=mape1(i)
				next i
				ent_score_norm=(l*ngram_size)/j
				
				for i=1 to l*ngram_size
					enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
				next i
				
				for i=1 to l
					for j=0 to ngram_size-1
						h=i-(ngram_size-1)
						if h+j>0 andalso h+j<l-(ngram_size-2) then
							e=0
							for k=1 to map2(nba(i),0)
								if map2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								map2(nba(i),0)+=1
								map2(nba(i),map2(nba(i),0))=h+j
								map2b(nba(i),map2(nba(i),0))=j
							end if
						end if
					next j
				next i
				
				start_temp=(temp1/4.61538)/((s/l)/log(l)) 'temp1/(s/l)
				start_temp/=m_ioc2(nba(),l,s,2)^0.75
				curr_temp=start_temp
				ngf=ngramfactor*ent_score_norm
				ngf/=1+((s/l)*multiplicityweight)
				ngfal=ngf/al
				onesixl=1.7/l
				
				' beijinghouse: allow nearly instant output of first pass
		'		solution_timer=timer-solvesub_solutionreleasetimer
				
				best_score=0
				for n=0 to ni	
					best_keyscore(n)=0	
				next n
				
				for lv=1 to lvmax
					
					for lr=1 to sr(lv)
						
						iterations=(iterations_total/sr(lv))/lvmax
						
						for rr=1 to random_restarts
							
							for n=0 to ni
								
								for i=0 to abc_sizem1 'erase frq
									frq(n,i)=0
								next i
								
								if lv=1 then
									for i=1 to s
										'if use_cribs=0 then	
											state(n)=48271*state(n) and 2147483647
											new_letter(n)=abc_size*state(n) shr 31
										'else
										'	if cribkey(i)=0 then
										'		state=48271*state and 2147483647
										'		new_letter=abc_size*state shr 31
										'	else
										'		new_letter=cribkey(i)-1
										'	end if
										'end if
										stl(n,i)=new_letter(n)
										frq(n,new_letter(n))+=mape2(i)
										for j=1 to map1(i,0)
											sol(n,map1(i,j))=new_letter(n)
										next j
									next i
								else
									for i=1 to s
										stl(n,i)=key2(n,i)
										frq(n,key2(n,i))+=mape2(i)
										for j=1 to map1(i,0)
											sol(n,map1(i,j))=key2(n,i)
										next j
									next i
								end if
								
								entropy(n)=0
								for i=0 to abc_sizem1
									entropy(n)+=enttable(frq(n,i))
								next i
								
								mi(n)=0
								mj(n)=0
								#include "solver_picksymbol_2ndorder.bi"
								
								'old_score(n)=0
								'new_score(n)=0
								'new_score_hss(n)=0
								
								temp(n)=curr_temp
								temp_min(n)=temp(n)/iterations
							
								new_ngram_score(n)=0
								'select case ngram_size
								'	case 8
										for i=1 to al
											ngrams(n,i)=bh8(bh4(sol(n,i),sol(n,i+1),sol(n,i+2),sol(n,i+3)),bh4(sol(n,i+4),sol(n,i+5),sol(n,i+6),sol(n,i+7)))
											new_ngram_score(n)+=ngrams(n,i)
										next i
								'	case 10
								'		for i=1 to al
								'			ngrams(n,i)=bh10(bh5(sol(n,i),sol(n,i+1),sol(n,i+2),sol(n,i+3),sol(n,i+4)),bh5(sol(n,i+5),sol(n,i+6),sol(n,i+7),sol(n,i+8),sol(n,i+9)))
								'			new_ngram_score(n)+=ngrams(n,i)
								'		next i
								'end select
								
								#include "solver_fastent_2ndorder.bi"
							
							next n
							
							mc=5
							mc_minus=(mc-1)/iterations
							
							n=ni
							
							dim as uinteger iterationsni=iterations*(ni+1)
							
							for it=1 to iterationsni
								
								'---------------------------------------------------------------------
								
								n+=1
								if n=ni+1 then n=0
								
								old_letter(n)=stl(n,curr_symbol(n))
								
								if lv=lvmax then mc-=mc_minus
								
								state(n)=48271*state(n) and 2147483647
								d=4*state(n) shr 31
								
								bls=0
								if d>mc then
									#include "solver_randomnewletter_2ndorder.bi"
								else
									state(n)=48271*state(n) and 2147483647
									k=1+map2(curr_symbol(n),0)*state(n) shr 31
									j=map2(curr_symbol(n),k)
									new_letter(n)=abc_size
									'select case ngram_size
									'	case 8
											#include "solver_pickletter_bh8_2ndorder.bi"
									'	case 10
									'		#include "solver_pickletter_bh10_2ndorder.bi"
									'end select
									if new_letter(n)=old_letter(n) or new_letter(n)=abc_size then
										#include "solver_randomnewletter_2ndorder.bi"
									end if
								end if
								
								for i=1 to map1(curr_symbol(n),0)
									sol(n,map1(curr_symbol(n),i))=new_letter(n)
								next i
								
								old_entropy(n)=entropy(n)
								entropy(n)+=enttable(frq(n,old_letter(n))-mape2(curr_symbol(n)))-enttable(frq(n,old_letter(n)))
								entropy(n)+=enttable(frq(n,new_letter(n))+mape2(curr_symbol(n)))-enttable(frq(n,new_letter(n)))
								
								old_ngram_score(n)=new_ngram_score(n)
								
								'select case ngram_size
								'	case 8
										for i=1 to map2(curr_symbol(n),0)
											z=0
											j=map2(curr_symbol(n),i)
											z1=bh4(sol(n,j),sol(n,j+1),sol(n,j+2),sol(n,j+3))
											if z1<>0 then
												z2=bh4(sol(n,j+4),sol(n,j+5),sol(n,j+6),sol(n,j+7))
												if z2<>0 then z=bh8(z1,z2)
											end if
											new_ngram_score(n)+=z-ngrams(n,j)
										next i
								'	case 10
								'		for i=1 to map2(curr_symbol(n),0)
								'			z=0
								'			j=map2(curr_symbol(n),i)
								'			z1=bh5(sol(n,j),sol(n,j+1),sol(n,j+2),sol(n,j+3),sol(n,j+4))
								'			if z1<>0 then
								'				z2=bh5(sol(n,j+5),sol(n,j+6),sol(n,j+7),sol(n,j+8),sol(n,j+9))
								'				if z2<>0 then z=bh10(z1,z2)
								'			end if
								'			new_ngram_score(n)+=z-ngrams(n,j)
								'		next i
								'end select
								
								#include "solver_fastent_2ndorder.bi"
								
								stl(n,curr_symbol(n))=new_letter(n)
								
								dim as integer ss,hss=0
								
								for i=0 to ni-1
									for j=i+1 to ni
										ss=0
										for k=1 to s
											if stl(i,k)=stl(j,k) then ss+=1
										next k
										if ss>hss then hss=ss
									next j
								next i
								
								dim as double hssdiv=1+(hss/(s*solvesub_higherorderhomophonicweight))
								
								for i=0 to ni
									new_score_hss(i)=new_score(i)/hssdiv 'penalty
								next i
								
								'---------------------------------------------------------------------
								
								if new_score_hss(n)>old_score(n) then
									
									frq(n,old_letter(n))-=mape2(curr_symbol(n))
									frq(n,new_letter(n))+=mape2(curr_symbol(n))
									
									'select case ngram_size
									'	case 8
											for i=1 to map2(curr_symbol(n),0)
												j=map2(curr_symbol(n),i)
												ngrams(n,j)=bh8(bh4(sol(n,j),sol(n,j+1),sol(n,j+2),sol(n,j+3)),bh4(sol(n,j+4),sol(n,j+5),sol(n,j+6),sol(n,j+7)))
											next i
									'	case 10
									'		for i=1 to map2(curr_symbol(n),0)
									'			j=map2(curr_symbol(n),i)
									'			ngrams(n,j)=bh10(bh5(sol(n,j),sol(n,j+1),sol(n,j+2),sol(n,j+3),sol(n,j+4)),bh5(sol(n,j+5),sol(n,j+6),sol(n,j+7),sol(n,j+8),sol(n,j+9)))
									'		next i
									'end select
									
									old_score(n)=new_score_hss(n)
									
									if new_score_hss(n)>best_keyscore(n) then
										best_keyscore(n)=new_score_hss(n)
										for i=1 to s
											key1(n,i)=stl(n,i)
										next i
									end if
									
									#include "solver_picksymbol_2ndorder.bi"
									
									dim as double new_score_comb=0
									for i=0 to ni
										new_score_comb+=new_score_hss(i)
									next i
									new_score_comb/=(ni+1)
									'new_score_comb*=1+(1/abc_size) 'generalize for n-order ???
									
									if new_score_comb>best_score then
										
										solution_improved=1
										thread(tn).sectime=timer-sectimer
										dim as double new_score_copy(ni),hscore=0
										dim as integer cn=0,firstn=123
										for i=0 to ni
											new_score_copy(i)=new_score_hss(i)
										next i
										for i=0 to ni
											cn=0
											hscore=0
											for j=0 to ni
												if new_score_copy(j)>hscore then
													hscore=new_score_copy(j)
													cn=j
												end if
											next j
											if firstn=123 then firstn=cn
											new_score_copy(cn)=0
											for j=1 to l
												thread(tn).gkey(j,i)=alphabet(sol(cn,j))
											next j
										next i
										erase frq2
										for i=1 to l
											thread(tn).sol(i)=alphabet(sol(firstn,i))
											frq2(sol(firstn,i))+=1
										next i
										ioc_int=0
										for i=0 to abc_sizem1
											ioc_int+=ioctable(frq2(i))
										next i
										thread(tn).ioc=ioc_int/ll
										thread(tn).ent=entropy(firstn)
										thread(tn).multiplicity=s/l
										best_score=new_score_comb+0.00001
										'thread(tn).score=best_score
										
										'#include "ext_hc3.bi"
										
									end if
									
									if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
										solution_improved=0 'consume
										solution_timer=timer
										'#include "solver_ioc.bi"
										#include "solver_advstats.bi"
										thread(tn).score=best_score
										#include "solver_output.bi"
										'#include "ext_hc3.bi"
									end if
									
								else
									
									stl(n,curr_symbol(n))=old_letter(n)
									
									new_ngram_score(n)=old_ngram_score(n)
									entropy(n)=old_entropy(n)
									
									for i=1 to map1(curr_symbol(n),0)
										sol(n,map1(curr_symbol(n),i))=old_letter(n)
									next i
									
									#include "solver_picksymbol_2ndorder.bi"
									
									old_score(n)-=temp(n)*map1(curr_symbol(n),0)*onesixl*old_score(n)/new_score_hss(n)
									
								end if
								
								temp(n)-=temp_min(n)
								
								thread(tn).iterations_completed+=1
								if thread(tn).solver_stop=1 then exit for,for,for,for,for
								if pausetask=1 then do:sleep 10:loop until pausetask=0
								
							next it
						
						next rr
					
					next lr
					
					for n=0 to ni
						for i=1 to s
							key2(n,i)=key1(n,i)
						next i
					next n
					
					curr_temp/=tempdiv
					
				next lv
				
			next b
			
			if thread(tn).solver_stop=0 then
				
				'#include "ext_hc4.bi"
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					'#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub

sub bhdecrypt_bigram_810g(byval tn_ptr as any ptr)
	
	#include "solver_variables.bi"
	
	dim as integer solver_output=0 'solver id
	dim as ubyte key1(constcip)
	dim as ubyte key2(constcip)
	dim as ubyte sol(constcip)
	dim as ubyte stl(constcip)
	dim as ubyte ngrams(constcip)
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short bnba(constcip)
	dim as short frc(constcip)
	dim as short mape1(constcip)
	dim as short mape2(constcip)
	dim as byte cribkey(constcip)
	dim as double enttable(constent)
	dim as byte sr(10),lnb(0)
	dim as integer rl(40)
	dim as ushort wordgrams(constcip)
	dim as ushort wordgrams2(constcip)
	
	dim as short maps(constcip)
	dim as short maps2(constcip)
	dim as integer mi,mj
		
	do 'wait for input
		
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 8:tempdiv=2.25
				case 9:tempdiv=2.25 'TODO: test if this is best once working
'				case 8:tempdiv=3
				'case 10:tempdiv=3
			end select
			
			#include "solver_settings.bi"
			
			'bigram substitution stuff
			'-----------------------------------------
			'8-grams: entweight=0.75
			'10-grams+: entweight=0.75+
			
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			if use_cribs=1 then
				for i=1 to s
					cribkey(i)=thread(tn).ckey(i)
				next i
			end if
			
			#include "ext_hc1.bi"
			
			for b=1 to bm
				
				#include "ext_hc2.bi" 'may change l and s for hill climbers
				
				frcmax=0
				for i=1 to l
					nba(i)=thread(tn).cip(i)
					frc(nba(i))+=1
					if frc(nba(i))>frcmax then frcmax=frc(nba(i))
				next i
				
				dim as short map1(s,frcmax)
				dim as short map2(s,frcmax*ngram_size)
				dim as short wordmap2(s,frcmax*wngs)
				dim as short map2b(s,frcmax*ngram_size)
				
				'bigram substitution stuff
				'-----------------------------------------	
				dim as short s2=0 'unique bigrams
				dim as short ps2(l,1) 'can be smaller
				dim as short frq3(s,s) 'frq2 not used past this block
				dim as short hp2(constfrq,constfrq)
				dim as uinteger hp=0,old_hp
				for i=1 to s
					for j=1 to s
						frq3(i,j)=0
					next j
				next i
				for i=1 to l-1 step 2
					if frq3(nba(i),nba(i+1))=0 then
						s2+=1
						ps2(s2,0)=i
						ps2(s2,1)=i+1
					end if
					frq3(nba(i),nba(i+1))+=1
				next i
				redim frq3(0,0)
				'-----------------------------------------
				
				for i=1 to l*ngram_size
					enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
				next i
				
				for i=1 to s
					frc(i)=0
					map1(i,0)=0
					map2(i,0)=0
					wordmap2(i,0)=0
					maps(i)=i
					mape2(i)=0
				next i
				
				for i=1 to l
					map1(nba(i),0)+=1
					map1(nba(i),map1(nba(i),0))=i
					mape1(i)=0
				next i
				
				for i=1 to l-(ngram_size-1) 'entropy reduction
					for j=0 to ngram_size-1
						mape1(i+j)+=1
					next j
				next i
				j=0
				for i=1 to l
					mape2(nba(i))+=mape1(i)
					j+=mape1(i)
				next i
				ent_score_norm=(l*ngram_size)/j
				
				for i=1 to l
					for j=0 to ngram_size-1
						h=i-(ngram_size-1)
						if h+j>0 andalso h+j<l-(ngram_size-2) then
							e=0
							for k=1 to map2(nba(i),0)
								if map2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								map2(nba(i),0)+=1
								map2(nba(i),map2(nba(i),0))=h+j
								map2b(nba(i),map2(nba(i),0))=j
							end if
						end if
					next j
				next i

				for i=1 to l
					for j=0 to wngs-1
						h=i-(wngs-1)
						if h+j>0 andalso h+j<l-(wngs-2) then
							e=0
							for k=1 to wordmap2(nba(i),0)
								if wordmap2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								wordmap2(nba(i),0)+=1
								wordmap2(nba(i),wordmap2(nba(i),0))=h+j
							end if
						end if
					next j
				next i

				start_temp=(temp1/4.61538)/((s/l)/log(l))
				start_temp/=m_ioc2(nba(),l,s,2)^0.75
				curr_temp=start_temp
				ngf=ngramfactor*ent_score_norm
				ngf/=1+(s/l)*multiplicityweight
				ngfal=ngf/al
				onesixl=1.7/l
				hi=1/highgram
				best_score=0
				
				' beijinghouse: allow nearly instant output of first pass
			'	solution_timer=timer-solvesub_solutionreleasetimer
				
				for lv=1 to lvmax
					
					for lr=1 to sr(lv)
						
						iterations=(iterations_total/sr(lv))/lvmax
				
						for rr=1 to random_restarts
							
							erase frq
							erase hp2 'bigram substitution stuff
							
							if lv=1 then
								
								mutexlock csolmutex
								for i=1 to s
									if use_cribs=0 then
										state=48271*state and 2147483647
										new_letter=abc_size*state shr 31
									else
										if cribkey(i)=0 then
											
											'bigram substitution stuff
											'-----------------------------------------
											
											'e=0 'use heatmap (experimental)
											'h=0
											'k=0
											'for j=0 to abc_size
											'	if csol(map1(i,1),j)>h then
											'		h=csol(map1(i,1),j)
											'		e=j
											'		k=1
											'	end if
											'next j
											'if k=1 then
											'	new_letter=e
											'else
											'	state=48271*state and 2147483647
											'	new_letter=abc_size*state shr 31
											'end if
											
											state=48271*state and 2147483647
											dim as double rndroll=state/2147483648
												
											if rndroll>solvesub_bigrambestsol then
												state=48271*state and 2147483647 'use random (good for unbiased heatmap)
												new_letter=abc_size*state shr 31
											else
												if csol(0,100)=1 then 'use best solution (good for solving)
													new_letter=csol(map1(i,1),100)
												else
													state=48271*state and 2147483647
													new_letter=abc_size*state shr 31
												end if
											end if
											
											'-----------------------------------------
											
										else
											new_letter=cribkey(i)-1
										end if
									end if
									stl(i)=new_letter
									frq(new_letter)+=mape2(i) 'map15(i,0)
									for j=1 to map1(i,0)
										sol(map1(i,j))=new_letter
									next j
								next i
								mutexunlock csolmutex
								
							else
								for i=1 to s
									stl(i)=key2(i)
									frq(key2(i))+=mape2(i) 'map15(i,0)
									for j=1 to map1(i,0)
										sol(map1(i,j))=key2(i)
									next j
								next i
							end if
							
							'bigram substitution stuff
							'-----------------------------------------
							hp=0
							for i=1 to s2
								hp2(sol(ps2(i,0)),sol(ps2(i,1)))+=1
							next i
							for i=1 to s2
								hp+=ioctable(hp2(sol(ps2(i,0)),sol(ps2(i,1))))
							next i
							'-----------------------------------------
							
							entropy=0
							for i=0 to abc_sizem1
								entropy+=enttable(frq(i))
							next i
							
							mi=0
							mj=0
							#include "solver_picksymbol.bi"
							
							old_score=0
							temp=curr_temp
							temp_min=temp/iterations
							new_ngram_score=0
							
							select case ngram_size
								case 8
									for i=1 to al
										ngrams(i)=bh8(bh4(sol(i),sol(i+1),sol(i+2),sol(i+3)),bh4(sol(i+4),sol(i+5),sol(i+6),sol(i+7)))
										new_ngram_score+=ngrams(i)
									next i
								case 9
									for j=1 to al

										z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
										If z1 = 0 Then
											ngrams(j) = 0
										Else
											z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
											If z2 = 0 Then
												ngrams(j) = 0
											else
													#include "solver_case9.bi"
											EndIf
										EndIf
									
										new_ngram_score+=ngrams(j)
									next j
								'case 10
								'	for i=1 to al
								'		ngrams(i)=bh10(bh5(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4)),bh5(sol(i+5),sol(i+6),sol(i+7),sol(i+8),sol(i+9)))
								'		new_ngram_score+=ngrams(i)
								'	next i
							end select

							for i=1 to l-(wngs-1)
									if solvesub_7gwordgrams=0 then
										wordgrams(i)=g6w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
									else
										wordgrams(i)=g7w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
									end if
							next i

							#include "solver_bonusgram_init.bi"

							mc=5
							mc_minus=(mc-1)/iterations
							
							for it=1 to iterations
								
								'---------------------------------------------------------------------
								
								old_letter=stl(curr_symbol)
								
								if lv=lvmax then mc-=mc_minus
								
								state=48271*state and 2147483647
								d=4*state shr 31
								
								bls=0
								if d>mc then
									#include "solver_randomnewletter.bi"
								else
									state=48271*state and 2147483647
									k=1+map2(curr_symbol,0)*state shr 31
									j=map2(curr_symbol,k)
									new_letter=abc_size
									select case ngram_size
										case 8
											#include "solver_pickletter_bh8.bi"
										case 9
											#include "solver_pickletter_bh9.bi"
									'	case 10
									'		#include "solver_pickletter_bh10.bi"
									end select
									if new_letter=old_letter or new_letter=abc_size then
										#include "solver_randomnewletter.bi"
									end if
								end if
								
								'bigram substitution stuff
								'-----------------------------------------
								old_hp=hp
								if map1(curr_symbol,1) mod 2=0 then 'even
									hp+=ioctable(hp2(sol(map1(curr_symbol,1)-1),new_letter)+1)-ioctable(hp2(sol(map1(curr_symbol,1)-1),new_letter))
									hp+=ioctable(hp2(sol(map1(curr_symbol,1)-1),old_letter)-1)-ioctable(hp2(sol(map1(curr_symbol,1)-1),old_letter))
								else 'uneven
									hp+=ioctable(hp2(new_letter,sol(map1(curr_symbol,1)+1))+1)-ioctable(hp2(new_letter,sol(map1(curr_symbol,1)+1)))
									hp+=ioctable(hp2(old_letter,sol(map1(curr_symbol,1)+1))-1)-ioctable(hp2(old_letter,sol(map1(curr_symbol,1)+1)))
								end if
								'-----------------------------------------
								
								old_entropy=entropy
								entropy+=enttable(frq(old_letter)-mape2(curr_symbol))-enttable(frq(old_letter))
								entropy+=enttable(frq(new_letter)+mape2(curr_symbol))-enttable(frq(new_letter))
								
								old_ngram_score=new_ngram_score
								num_ngrams=map2(curr_symbol,0)
								
								for i=1 to num_ngrams
									j=map2(curr_symbol,i)
									new_ngram_score-=ngrams(j)
								next i

								#include "solver_bonusgram_main.bi"

								#include "solver_fastent_bh.bi"

								#include "solver_wordscore.bi"


								dim as double hw=(1+hp/solvesub_bigramhomwdiv) 'bigram substitution stuff
								if solvesub_bigramhomwdiv=0 then hw=1

								' works by adding in upcoming wordscore to test for score needed
								score_needed=(old_score*hw/ent2/ngfal/(1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size))-new_ngram_score)*hi

								if num_ngrams>score_needed then 'beijinghouse score_needed optimization
									for i=1 to map1(curr_symbol,0)
										sol(map1(curr_symbol,i))=new_letter
									next i
									select case ngram_size 
										case 8
											for i=1 to num_ngrams
												z=0
												j=map2(curr_symbol,i)
												z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
												if z1<>0 then
													z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
													if z2<>0 then z=bh8(z1,z2)
												end if
												score_needed-=z*hi
												if (num_ngrams-i)<score_needed then exit for
												new_ngram_score+=z
												if score_needed<0 then exit for
											next i
										case 9
											for i=1 to num_ngrams
												j=map2(curr_symbol,i)
												z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
												If z1 = 0 Then
													z = 0
												Else
													z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
													If z2 = 0 Then
														z = 0
													else
															#include "solver_case9z.bi"
													EndIf
												endif
												score_needed-=z*hi
												if num_ngrams-i<score_needed then exit for
												new_ngram_score+=z
												if score_needed<0 then exit for
											next i
										'case 10
										'	for i=1 to num_ngrams
										'		z=0
										'		j=map2(curr_symbol,i)
										'		z1=bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
										'		if z1<>0 then
										'			z2=bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9))
										'			if z2<>0 then z=bh10(z1,z2)
										'		end if
										'		score_needed-=z*hi
										'		if (num_ngrams-i)<score_needed then exit for
										'		new_ngram_score+=z
										'		if score_needed<0 then exit for
										'	next i
									end select
									if score_needed>=0 then
										for i=1 to map1(curr_symbol,0)
											sol(map1(curr_symbol,i))=old_letter
										next i
									end if
								end if
								
								'---------------------------------------------------------------------
								
								if score_needed<0 then

									#include "solver_bonusgram_tail.bi"
									
									'bigram substitution stuff
									'-----------------------------------------
									if map1(curr_symbol,1) mod 2=0 then 'even
										hp2(sol(map1(curr_symbol,1)-1),new_letter)+=1
										hp2(sol(map1(curr_symbol,1)-1),old_letter)-=1
									else 'uneven
										hp2(new_letter,sol(map1(curr_symbol,1)+1))+=1
										hp2(old_letter,sol(map1(curr_symbol,1)+1))-=1
									end if
									'-----------------------------------------
									
									frq(old_letter)-=mape2(curr_symbol)
									frq(new_letter)+=mape2(curr_symbol)
									stl(curr_symbol)=new_letter
									
									select case ngram_size
										case 8
											for h=1 to num_ngrams
												j=map2(curr_symbol,h)
												ngrams(j)=bh8(bh4(sol(j),sol(j+1),sol(j+2),sol(j+3)),bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7)))
												if h>i then new_ngram_score+=ngrams(j)
											next h
										case 9
											for k=1 to num_ngrams
												j=map2(curr_symbol,k)
												z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
												If z1 = 0 Then
													ngrams(j) = 0
												Else
													z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
													If z2 = 0 Then
														ngrams(j) = 0
													else
															#include "solver_case9.bi"
													EndIf
												endif
												if k>i then new_ngram_score+=ngrams(j)
											next k
									'	case 10
									'		for h=1 to num_ngrams
									'			j=map2(curr_symbol,h)
									'			ngrams(j)=bh10(bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)),bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9)))
									'			if h>i then new_ngram_score+=ngrams(j)
									'		next h
									end select

									for i=1 to l-(wngs-1) 'save the new values since they worked
										wordgrams(i)=wordgrams2(i)
									next i
									
									new_score=new_ngram_score*ngfal*ent2
									
									' add in w_score if it passes
									new_score*=1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size) 'multiplicative
									
									new_score/=hw 'bigram substitution stuff
									
									old_score=new_score
									
									#include "solver_picksymbol.bi"
									
									if new_score>best_score then
										
										solution_improved=1
										thread(tn).sectime=timer-sectimer
										for i=1 to l
											thread(tn).sol(i)=alphabet(sol(i))
										next i
										thread(tn).ent=entropy
										thread(tn).multiplicity=s/l	
										thread(tn).ioc2=hp
										best_score=new_score+0.00001				
										for i=1 to s
											key1(i)=stl(i)
										next i
										
										if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
											solution_improved=0 'consume
											solution_timer=timer
											#include "solver_ioc.bi"
											#include "solver_advstats.bi"
											thread(tn).score=best_score
											#include "solver_output.bi"
											#include "ext_hc3.bi"
										end if
										
									end if
									
								else
									
									hp=old_hp 'bigram substitution stuff
									
									new_ngram_score=old_ngram_score
									entropy=old_entropy
									
									#include "solver_picksymbol.bi"
									
									old_score-=temp*map1(curr_symbol,0)*onesixl
									
								end if
								
								temp-=temp_min
								
								thread(tn).iterations_completed+=1
								if thread(tn).solver_stop=1 then exit for,for,for,for,for
								if pausetask=1 then do:sleep 10:loop until pausetask=0
								
							next it
							
						next rr
					
					next lr
					
					for i=1 to s
						key2(i)=key1(i)
					next i
					
					curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
				
				next lv
			
			next b
			
			if thread(tn).solver_stop=0 then
				
				#include "ext_hc4.bi"
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub


sub bhdecrypt_allg_bhgov(byval tn_ptr as any ptr)
	
	#include "solver_variables.bi"

	dim as integer solver_output=0 'solver id 
	dim as ubyte key1(constcip)
	dim as ubyte key2(constcip)
	dim as ubyte sol(constcip)
	dim as ubyte stl(constcip)
	dim as ubyte ngrams(constcip)
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short bnba(constcip)
	dim as short frc(constcip)
	dim as short mape1(constcip)
	dim as short mape2(constcip)
	dim as byte cribkey(constcip)
	dim as double enttable(constent)
	dim as byte sr(10),lnb(0)

	dim as integer x
	
	dim as short maps(constcip)
	dim as short maps2(constcip)
	dim as integer mi,mj
	
	do 'wait for input
		
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 2,3,4,5:tempdiv=3
				case 6:tempdiv=2.75
				case 7:tempdiv=2.5
				case 8:tempdiv=2.25
				case 10:tempdiv=2
				case else:tempdiv=2
			end select
			
			#include "solver_settings.bi"
			
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			if use_cribs=1 then
				for i=1 to s
					cribkey(i)=thread(tn).ckey(i)
				next i
			end if
			
			#include "ext_hc1.bi"
			
			for b=1 to bm
				
				#include "ext_hc2.bi" 'may change l and s for hill climbers
				
				frcmax=0
				for i=1 to l
					nba(i)=thread(tn).cip(i)
					frc(nba(i))+=1
					if frc(nba(i))>frcmax then frcmax=frc(nba(i))
				next i
				
				dim as short map1(s,frcmax)
				dim as short map2(s,frcmax*ngram_size)
				dim as short map2b(s,frcmax*ngram_size)
				
				for i=1 to l*ngram_size
					enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
				next i
				
				for i=1 to s
					frc(i)=0
					maps(i)=i
					mape2(i)=0
					map1(i,0)=0
					map2(i,0)=0
				next i
				
				for i=1 to l
					map1(nba(i),0)+=1
					map1(nba(i),map1(nba(i),0))=i
					mape1(i)=0
				next i
				
				for i=1 to l-(ngram_size-1) 'entropy reduction
					for j=0 to ngram_size-1
						mape1(i+j)+=1
					next j
				next i
				j=0
				for i=1 to l
					mape2(nba(i))+=mape1(i)
					j+=mape1(i)
				next i
				ent_score_norm=(l*ngram_size)/j
				
				for i=1 to l
					for j=0 to ngram_size-1
						h=i-(ngram_size-1)
						if h+j>0 andalso h+j<l-(ngram_size-2) then
							e=0
							for k=1 to map2(nba(i),0)
								if map2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								map2(nba(i),0)+=1
								map2(nba(i),map2(nba(i),0))=h+j
								map2b(nba(i),map2(nba(i),0))=j
							end if
						end if
					next j
				next i
				
				start_temp=(temp1/4.61538)/((s/l)/log(l))
				start_temp/=m_ioc2(nba(),l,s,2)^0.75
				curr_temp=start_temp
				ngf=ngramfactor*ent_score_norm
				ngf/=1+(s/l)*multiplicityweight
				ngfal=ngf/al
				onesixl=1.7/l
				hi=1.0/highgram
				best_score=0

				' beijinghouse: allow nearly instant output of first pass
			'	solution_timer=timer-solvesub_solutionreleasetimer
				
				for lv=1 to lvmax
					
					for lr=1 to sr(lv)
						
						iterations=(iterations_total/sr(lv))/lvmax
						
						for rr=1 to random_restarts
							
							erase frq
							
							if lv=1 then
								for i=1 to s
									if use_cribs=0 then
										state=48271*state and 2147483647
										new_letter=65+(abc_size*state shr 31)
									else
										if cribkey(i)=0 then
											state=48271*state and 2147483647
											new_letter=65+(abc_size*state shr 31)
										else
											new_letter=65+cribkey(i)-1
										end if
									end if
									stl(i)=new_letter
									frq(new_letter)+=mape2(i)
									for j=1 to map1(i,0)
										sol(map1(i,j))=new_letter
									next j
								next i
							else
								for i=1 to s
									stl(i)=key2(i)
									frq(key2(i))+=mape2(i)
									for j=1 to map1(i,0)
										sol(map1(i,j))=key2(i)
									next j
								next i
							end if
							
							entropy=0
							for i=65 to 90 '0 to abc_sizem1
								entropy+=enttable(frq(i))
							next i
							
							mi=0
							mj=0
							#include "solver_picksymbol.bi"
							
							old_score=0
							temp=curr_temp
							temp_min=temp/iterations
							new_ngram_score=0
							
							for j=0 to al    ' zero out ngrams so keyguard optimization isn't fooled
								ngrams(j) = 0 ' into thinking there's a valid ngram preceeding it
							next j
							
							in_picker=1
							for j=1 to al
								Select Case ngram_size
									Case 6
										#include "solver_gov_6.bi"
									Case 7
										#include "solver_gov_7.bi"
									Case 8
										#include "solver_gov_8.bi"
									Case 9
										#include "solver_gov_9.bi"
									Case 10
										#include "solver_gov_10.bi"
									Case Else ' 2-5
										#include "solver_gov_small.bi"
								End select
								ngrams(j)=z
								new_ngram_score+=ngrams(j)
							next j
							in_picker=0


							' give initial score so can see what trash we have here
							#include "solver_fastent_bh.bi"
							new_score=new_ngram_score*ngfal*ent2
							thread(tn).sectime=timer-sectimer
							ioc_int=0
							for i=65 to 90
								ioc_int+=ioctable(frq(i))
							next i
							thread(tn).ioc=ioc_int/ll
							thread(tn).ent=entropy
							thread(tn).multiplicity=s/l
							for i=1 to l
								thread(tn).sol(i)=sol(i)
							next i
							best_score=new_score+0.00001
							thread(tn).score=best_score
							
							for i=1 to s
								key1(i)=stl(i)
							next i
							
							' ugly hack -- not sure how to make batches and interactive work well
							' probably only accuracy batches will work in this mode now
							If thread(tn).solkey = 0 Then
								thread(tn).solver_waiting=1 ' trick GUI into being responsive when not in batch accuracy mode
							endif
							
							mc=5
							mc_minus=(mc-1)/iterations

							accept=1
							
							for it=1 to iterations
								
								'---------------------------------------------------------------------

								#include "solver_picksymbol.bi"
								
								if accept=0 then old_score-=temp*map1(curr_symbol,0)*onesixl*old_score/new_score
								'if accept=0 then old_score-=temp*map2(curr_symbol,0)/al*(old_score/new_score)
								
								temp-=temp_min
								
								old_letter=stl(curr_symbol)
								old_ngram_score=new_ngram_score
								num_ngrams=map2(curr_symbol,0)

								if lv=lvmax then mc-=mc_minus
								state=48271*state and 2147483647
								d=4*state shr 31

								k=1+num_ngrams*(state shr 31)
								j=map2(curr_symbol,k)
								m=ngram_size-1-map2b(curr_symbol,k)

								bls=0
								if d>mc then
									#include "solver_randomnewletter_gov.bi"
								else
									state=48271*state and 2147483647
								'	new_letter=abc_size

									in_picker=1
									#include "solver_pickletter_gov.bi"
									in_picker=0

									if new_letter=old_letter then ' or new_letter=abc_size then
										#include "solver_randomnewletter_gov.bi"
									end if
								end if
								
								old_entropy=entropy
								entropy+=enttable(frq(old_letter)-mape2(curr_symbol))-enttable(frq(old_letter))
								entropy+=enttable(frq(new_letter)+mape2(curr_symbol))-enttable(frq(new_letter))
																
								for i=1 to num_ngrams
									j=map2(curr_symbol,i)
									new_ngram_score-=ngrams(j)
								next i
								
								#include "solver_fastent_bh.bi"
								
								score_needed=(old_score/ent2/ngfal-new_ngram_score)*hi
								
								if num_ngrams>score_needed then 'beijinghouse score_needed optimization
									for i=1 to map1(curr_symbol,0)
										sol(map1(curr_symbol,i))=new_letter
									next i
									for i=1 to num_ngrams
										z=0
										j=map2(curr_symbol,i)
										select case ngram_size
											case 6
												#include "solver_gov_6.bi"
											case 7
												#include "solver_gov_7.bi"
											case 8
												#include "solver_gov_8.bi"
											case 9
												#include "solver_gov_9.bi"
											case 10
												#include "solver_gov_10.bi"
											case else '2-5
												#include "solver_gov_small.bi"
										end select
										score_needed-=z*hi
										if (num_ngrams-i)<score_needed then exit for
										new_ngram_score+=z
										if score_needed<0 then exit for
									next i
									if score_needed>=0 then
										for i=1 to map1(curr_symbol,0)
											sol(map1(curr_symbol,i))=old_letter
										next i
									end if
								end if
								
								'---------------------------------------------------------------------
								
								if score_needed<0 then

									accept=1
									frq(old_letter)-=mape2(curr_symbol)
									frq(new_letter)+=mape2(curr_symbol)
									stl(curr_symbol)=new_letter
									
									for h=1 to num_ngrams
										j=map2(curr_symbol,h)
										select case ngram_size
											case 6
												#include "solver_gov_6.bi"
											case 7
												#include "solver_gov_7.bi"
											case 8
												#include "solver_gov_8.bi"
											case 9
												#include "solver_gov_9.bi"
											case 10
												#include "solver_gov_10.bi"
											case else '2-5
												#include "solver_gov_small.bi"
										end select
										ngrams(j)=z
										if h>i then new_ngram_score+=ngrams(j)
									next h
									
									new_score=new_ngram_score*ngfal*ent2
									
									old_score=new_score
									
							'		#include "solver_picksymbol.bi"
									
									if new_score>best_score then

										solution_improved=1										
										best_score=new_score+0.00001
										thread(tn).sectime=timer-sectimer
										for i=1 to l
											thread(tn).sol(i)=sol(i)
										next i
										thread(tn).ent=entropy
										thread(tn).multiplicity=s/l
										m = 1000 ' scramble this variable just so inner solver fragments inside shortcircuit can't
													 ' accidentally use the m=ngs-1 optimization incorrectly if last change happened to be a
													 ' lone m=ngs-1 in the jth n-gram
										#include "solver_accuracy_shortcircuit_gov.bi" 'inflates 100% accuracy

										for i=1 to s
											key1(i)=stl(i)
										next i
										
									end if

									if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
										solution_improved=0
										solution_timer=timer
										#include "solver_ioc_gov.bi"
										#include "solver_advstats.bi"
										thread(tn).score=best_score
										#include "solver_output.bi"
										#include "ext_hc3.bi"
									end if

								else

									accept=0
									new_ngram_score=old_ngram_score
									entropy=old_entropy
									
								end if
								
								thread(tn).iterations_completed+=1
								if thread(tn).solver_stop=1 then exit for,for,for,for,for
								if pausetask=1 then do:sleep 10:loop until pausetask=0
								
							next it
							
						next rr
						
					next lr
					
					for i=1 to s
						key2(i)=key1(i)
					next i
					
					curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
					
				next lv
			
			next b
			
			if thread(tn).solver_stop=0 then
				
				#include "ext_hc4.bi"
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub


sub bhdecrypt_810g(byval tn_ptr as any ptr)
	
	#include "solver_variables.bi"
	
	dim as integer solver_output=0 'solver id
	dim as ubyte key1(constcip)
	dim as ubyte key2(constcip)
	dim as ubyte sol(constcip)
	dim as ubyte stl(constcip)
'	dim as ubyte ngrams(constcip)
	dim as ushort ngrams(constcip)
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short bnba(constcip)
	dim as short frc(constcip)
	dim as short mape1(constcip)
	dim as short mape2(constcip)
	dim as byte cribkey(constcip)
	dim as double enttable(constent)
	dim as byte sr(10),lnb(0)
'	dim as integer rl(40)
	dim as ushort wordgrams(constcip)
	dim as ushort wordgrams2(constcip)
	
	dim as short maps(constcip)
	dim as short maps2(constcip)
	dim as integer mi,mj

	do 'wait for input
		
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 8:tempdiv=2.25
				case 10:tempdiv=2
			end select
			
			#include "solver_settings.bi"
			
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			if use_cribs=1 then
				for i=1 to s
					cribkey(i)=thread(tn).ckey(i)
				next i
			end if
			
			#include "ext_hc1.bi"
			
			for b=1 to bm
				
				#include "ext_hc2.bi" 'may change l and s for hill climbers
				
				frcmax=0
				for i=1 to l
					nba(i)=thread(tn).cip(i)
					frc(nba(i))+=1
					if frc(nba(i))>frcmax then frcmax=frc(nba(i)) ' TODO : could make this bound tighter to save tiny amount of stack memory
				next i														 ' just need to calculate distance of symbols
				
				dim as short map1(s,frcmax)
				dim as short map2(s,frcmax*ngram_size)
				dim as short wordmap2(s,frcmax*wngs)
				dim as short map2b(s,frcmax*ngram_size)
				
				for i=1 to l*ngram_size
					enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
				next i
				
				for i=1 to s
					frc(i)=0
					maps(i)=i
					mape2(i)=0
					map1(i,0)=0
					wordmap2(i,0)=0
					map2(i,0)=0
				next i
				
				for i=1 to l
					map1(nba(i),0)+=1
					map1(nba(i),map1(nba(i),0))=i
					mape1(i)=0
				next i
				
				for i=1 to l-(ngram_size-1) 'entropy reduction
					for j=0 to ngram_size-1
						mape1(i+j)+=1
					next j
				next i

				for i=1 to l 'entropy non-reduction (temp lazy hack to test)
					mape1(i)=ngram_size
				next i
				
				j=0
				for i=1 to l
					mape2(nba(i))+=mape1(i)
					j+=mape1(i)
				next i
				ent_score_norm=(l*ngram_size)/j
				
				for i=1 to l
					for j=0 to ngram_size-1
						h=i-(ngram_size-1)
						if h+j>0 andalso h+j<=l then  'h+j<l-(ngram_size-2) then
							e=0
							for k=1 to map2(nba(i),0)
								if map2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								map2(nba(i),0)+=1
								map2(nba(i),map2(nba(i),0))=h+j
								map2b(nba(i),map2(nba(i),0))=j
							end if
						end if
					next j
				next i

				' beijinghouse 8-gram + 25% 9-gram combo mode
				if ngrams_inmem(9)=1 and ngram_size=8 then
					for i=1 to s
						h = map2(s,0)
						for k=1 to h	'add all trailing 8-grams to each symbol set to be careful and make stable
							if map2(s,k)+1<> al and map2(s,k)+1 <> map2(s,k+1) then
								map2(s,map2(s,0)+1) = map2(s,k)+1
								map2(s,0)+=1
							end if
						next k
					next i
				end if

				for i=1 to l
					for j=0 to wngs-1
						h=i-(wngs-1)
						if h+j>0 andalso h+j<l-(wngs-2) then
							e=0
							for k=1 to wordmap2(nba(i),0)
								if wordmap2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								wordmap2(nba(i),0)+=1
								wordmap2(nba(i),wordmap2(nba(i),0))=h+j
							end if
						end if
					next j
				next i
				
				start_temp=(temp1/4.61538)/((s/l)/log(l))
				start_temp/=m_ioc2(nba(),l,s,2)^0.75
				curr_temp=start_temp
				ngf=ngramfactor*ent_score_norm
				ngf/=1+(s/l)*multiplicityweight
				ngfal=ngf/al
				onesixl=1.7/l
				' beijinghouse 8-gram + 25% 9-gram combo mode
				if ngrams_inmem(9)=1 and ngram_size=8 then
					highgram=255+63
				end if
				hi=1.0/highgram
				best_score=0
				
				' beijinghouse: allow nearly instant output of first pass
			'	solution_timer=timer-solvesub_solutionreleasetimer
				
				for lv=1 to lvmax
					
					for lr=1 to sr(lv)
						
						iterations=(iterations_total/sr(lv))/lvmax
						
						for rr=1 to random_restarts
							
							erase frq
							
							if lv=1 then
								for i=1 to s
									if use_cribs=0 then
										state=48271*state and 2147483647
										new_letter=abc_size*state shr 31
									else
										if cribkey(i)=0 then
											state=48271*state and 2147483647
											new_letter=abc_size*state shr 31
										else
											new_letter=cribkey(i)-1
										end if
									end if
									if solvesub_reversesolve=1 then  ' re-enable reverse solve mode
										new_letter=alpharev(thread(tn).key(map1(i,1)))
									end if
									stl(i)=new_letter
									frq(new_letter)+=mape2(i)
									for j=1 to map1(i,0)
										sol(map1(i,j))=new_letter
									next j
								next i
							else
								for i=1 to s
									stl(i)=key2(i)
									frq(key2(i))+=mape2(i)
									for j=1 to map1(i,0)
										sol(map1(i,j))=key2(i)
									next j
								next i
							end if
							
							entropy=0
							for i=0 to abc_sizem1
								entropy+=enttable(frq(i))
							next i
							
							mi=0
							mj=0
							#include "solver_picksymbol.bi"
							
							old_score=0
							temp=curr_temp
							temp_min=temp/iterations
							new_ngram_score=0

							' lazy version, add better one later if this code helps/works
							for i=l+1 to l+ngram_size-1
								sol(i)=sol(i-1) ' add copy of first few letters past end of cipher so can get full ngram scores
							next i
							
							select case ngram_size
								case 8
									' firstgrams
'									i = 1
'									z1 = bh4(sol(i),sol(i+1),sol(i+2),sol(i+3))
'									if z1 <> 0 and z1 < max_allowed_table_index / sqr(2) then
'										z2 = bh4(sol(i+4),sol(i+5),sol(i+6),sol(i+7))
'										if z2 <> 0 and z2 < max_allowed_table_index / sqr(2) then
'											ngrams(i)=bh8f(z1,z2)
'											new_ngram_score+=ngrams(i)
'										else
'											ngrams(i)=0
'										end if
'									else
'										ngrams(i)=0
'									end if
									
'									for i=2 to al-1
									for i=1 to l 'al
										ngrams(i)=bh8(bh4(sol(i),sol(i+1),sol(i+2),sol(i+3)),bh4(sol(i+4),sol(i+5),sol(i+6),sol(i+7)))
										new_ngram_score+=ngrams(i)
									next i

									' beijinghouse 8-gram + 25% 9-gram combo mode
									if ngrams_inmem(9)=1 then
										for j=1 to al-1
											z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
											If z1 <> 0 Then
												z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
												If z2 <> 0 Then
													#include "solver_case9z.bi"
													ngrams(j)+=z
													new_ngram_score+=z
												EndIf
											EndIf
										next j
									end if

'									' lastgrams
'									i = al
'									z1 = bh4(sol(i),sol(i+1),sol(i+2),sol(i+3))
'									if z1 <> 0 and z1 < max_allowed_table_index / sqr(2) then
'										z2 = bh4(sol(i+4),sol(i+5),sol(i+6),sol(i+7))
'										if z2 <> 0 and z2 < max_allowed_table_index / sqr(2) then
'											ngrams(i)=bh8l(z1,z2)
'											new_ngram_score+=ngrams(i)
'										else
'											ngrams(i)=0
'										end if
'									else
'										ngrams(i)=0
'									end if

								case 9
									for j=1 to al

										z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
										If z1 = 0 Then
											ngrams(j) = 0
										Else
											z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
											If z2 = 0 Then
												ngrams(j) = 0
											else
													#include "solver_case9.bi"
											EndIf
										EndIf
									
										new_ngram_score+=ngrams(j)
									next j


							'	case 10
							'		for i=1 to al
							'			ngrams(i)=bh10(bh5(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4)),bh5(sol(i+5),sol(i+6),sol(i+7),sol(i+8),sol(i+9)))
							'			new_ngram_score+=ngrams(i)
							'		next i
							end select

							for i=1 to l-(wngs-1)
									if solvesub_7gwordgrams=0 then
										wordgrams(i)=g6w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
									else
										wordgrams(i)=g7w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
									end if
							next i

							#include "solver_bonusgram_init.bi"
							
							mc=5
							mc_minus=(mc-1)/iterations

							accept=1

							if solvesub_reversesolve=1 then
								#include "solver_fastent_bh.bi"

								#include "solver_wordscore.bi"

								new_score=new_ngram_score*ngfal*ent2

								' add in w_score if it passes
								new_score*=1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size) 'multiplicative

								old_score=new_score
								if new_score>best_score then
									thread(tn).sectime=timer-sectimer
									thread(tn).ent=entropy
									thread(tn).multiplicity=s/l
									#include "solver_ioc.bi"
									for i=1 to l
										thread(tn).sol(i)=alphabet(sol(i))
									next i
									best_score=new_score+0.00001
									thread(tn).score=best_score
								end if
							end if
							
							for it=1 to iterations
								
								'---------------------------------------------------------------------
								
								old_letter=stl(curr_symbol)
								
								if lv=lvmax then mc-=mc_minus
								
								state=48271*state and 2147483647
								d=4*state shr 31
								
								bls=0
								if d>mc then
									#include "solver_randomnewletter.bi"
								else
									state=48271*state and 2147483647
									k=1+map2(curr_symbol,0)*state shr 31
									j=map2(curr_symbol,k)
									new_letter=abc_size
									select case ngram_size
										case 8
											#include "solver_pickletter_bh8.bi"
										case 9
											#include "solver_pickletter_bh9.bi"
									'	case 10
									'		#include "solver_pickletter_bh10.bi"
									end select
									if new_letter=old_letter or new_letter=abc_size then
										#include "solver_randomnewletter.bi"
									end if
								end if
								
								old_entropy=entropy
								entropy+=enttable(frq(old_letter)-mape2(curr_symbol))-enttable(frq(old_letter))
								entropy+=enttable(frq(new_letter)+mape2(curr_symbol))-enttable(frq(new_letter))
								
								old_ngram_score=new_ngram_score
								num_ngrams=map2(curr_symbol,0)
								
								for i=1 to num_ngrams
									j=map2(curr_symbol,i)
									new_ngram_score-=ngrams(j)
								next i

									for i=1 to map1(curr_symbol,0)
										sol(map1(curr_symbol,i))=new_letter
									next i
									
									' lazy version, add better one later if this code helps/works
									for i=l+1 to l+ngram_size-1
										sol(i)=sol(i-1) ' add copy of first few letters past end of cipher so can get full ngram scores
									next i
									
									
								#include "solver_bonusgram_main.bi"
								
								#include "solver_fastent_bh.bi"
								
								#include "solver_wordscore.bi"

								' works by adding in upcoming wordscore to test for score needed
								score_needed=(old_score/ent2/ngfal/(1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size))-new_ngram_score)*hi

'								score_needed=(old_score/ent2/ngfal-new_ngram_score)*hi
								
								if num_ngrams>score_needed then 'beijinghouse score_needed optimization

									select case ngram_size 
										case 8
											for i=1 to num_ngrams
												z=0
												j=map2(curr_symbol,i)
												z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
												if z1<>0 then
													z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
													if z2<>0 then
'														if j=1 then
'															if z1 < max_allowed_table_index / sqr(2) and  z2 < max_allowed_table_index / sqr(2) then
'																z=bh8f(z1,z2)
'															end if
'														elseif j=al then
'															if z1 < max_allowed_table_index / sqr(2) and  z2 < max_allowed_table_index / sqr(2) then
'																z=bh8l(z1,z2)
'															end if
'														else
															z=bh8(z1,z2)
'														end if
													end if
												end if
												score_needed-=z*hi
				''temp								if (num_ngrams-i)<score_needed then exit for
												new_ngram_score+=z
				''temp								if score_needed<0 then exit for
												
												
												' beijinghouse 8-gram + 25% 9-gram combo mode
												if ngrams_inmem(9)=1 and j<>al then
													z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
													If z1 <> 0 Then
														z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
														If z2 <> 0 Then
															#include "solver_case9z.bi"
														EndIf
													EndIf

													score_needed-=z*hi
													if (num_ngrams-i)<score_needed then exit for
													new_ngram_score+=z
													if score_needed<0 then exit for
												end if

											next i
										case 9
											for i=1 to num_ngrams
												j=map2(curr_symbol,i)
												z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
												If z1 = 0 Then
													z = 0
												Else
													z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
													If z2 = 0 Then
														z = 0
													else
															#include "solver_case9z.bi"
													EndIf
												endif
												score_needed-=z*hi
												if num_ngrams-i<score_needed then exit for
												new_ngram_score+=z
												if score_needed<0 then exit for
											next i
									'	case 10
									'		for i=1 to num_ngrams
									'			z=0
									'			j=map2(curr_symbol,i)
									'			z1=bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
									'			if z1<>0 then
									'				z2=bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9))
									'				if z2<>0 then z=bh10(z1,z2)
									'			end if
									'			score_needed-=z*hi
									'			if (num_ngrams-i)<score_needed then exit for
									'			new_ngram_score+=z
									'			if score_needed<0 then exit for
									'		next i
									end select

								end if

									if score_needed>=0 then
										for i=1 to map1(curr_symbol,0)
											sol(map1(curr_symbol,i))=old_letter
										next i
										
										' lazy version, add better one later if this code helps/works
										for i=l+1 to l+ngram_size-1
											sol(i)=sol(i-1) ' add copy of first few letters past end of cipher so can get full ngram scores
										next i

										
									end if
								
								'---------------------------------------------------------------------
								
								if score_needed<0 then

									accept=1

									#include "solver_bonusgram_tail.bi"

									frq(old_letter)-=mape2(curr_symbol)
									frq(new_letter)+=mape2(curr_symbol)
									stl(curr_symbol)=new_letter

									select case ngram_size
										case 8
											for h=1 to num_ngrams
												j=map2(curr_symbol,h)
												z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
												if z1<>0 then
													z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
													if z2<>0 then
'														if j=1 then
'															if z1 < max_allowed_table_index / sqr(2) and  z2 < max_allowed_table_index / sqr(2) then
'																ngrams(j)=bh8f(z1,z2)
'															else
'																ngrams(j)=0
'															end if
'														elseif j=al then
'															if z1 < max_allowed_table_index / sqr(2) and  z2 < max_allowed_table_index / sqr(2) then
'																ngrams(j)=bh8l(z1,z2)
'															else
'																ngrams(j)=0
'															end if
'														else
															ngrams(j)=bh8(z1,z2)
'														end if
													else
														ngrams(j)=0
													end if
												else
													ngrams(j)=0
												end if
												' beijinghouse 8-gram + 25% 9-gram combo mode
												if ngrams_inmem(9)=1 and j<>al then
													z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
													If z1 <> 0 Then
														z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
														If z2 <> 0 Then
															#include "solver_case9z.bi"
															ngrams(j)+=z
														EndIf
													EndIf
												end if
												
												if h>i then new_ngram_score+=ngrams(j)
											next h
										case 9
											for k=1 to num_ngrams
												j=map2(curr_symbol,k)
												z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
												If z1 = 0 Then
													ngrams(j) = 0
												Else
													z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
													If z2 = 0 Then
														ngrams(j) = 0
													else
															#include "solver_case9.bi"
													EndIf
												endif
												if k>i then new_ngram_score+=ngrams(j)
											next k

									'	case 10
									'		for h=1 to num_ngrams
									'			j=map2(curr_symbol,h)
									'			ngrams(j)=bh10(bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)),bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9)))
									'			if h>i then new_ngram_score+=ngrams(j)
									'		next h
									end select

									for i=1 to l-(wngs-1) 'save the new values since they worked
										wordgrams(i)=wordgrams2(i)
									next i
									
									new_score=new_ngram_score*ngfal*ent2

									' add in w_score if it passes
									new_score*=1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size) 'multiplicative

									old_score=new_score
									
									#include "solver_picksymbol.bi"
									
									if new_score>best_score then
										
										solution_improved=1
										best_score=new_score+0.00001
										thread(tn).sectime=timer-sectimer
										for i=1 to l
											thread(tn).sol(i)=alphabet(sol(i))
										next i
										thread(tn).ent=entropy
										thread(tn).multiplicity=s/l
										for i=1 to s
											key1(i)=stl(i)
										next i

										#include "solver_accuracy_shortcircuit_bh.bi" 'inflates 100% accuracy
									end if
									
									if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
										solution_improved=0 'consume
										solution_timer=timer
										#include "solver_ioc.bi"
										#include "solver_advstats.bi"
										thread(tn).score=best_score
										#include "solver_output.bi"
										#include "ext_hc3.bi"
									end if
									
								else

									accept=0
									new_ngram_score=old_ngram_score
									entropy=old_entropy
									
									#include "solver_picksymbol.bi"

									' TODO: find way to do this with optimization above
'									old_score-=temp*map1(curr_symbol,0)*onesixl*old_score/new_score
'									old_score-=temp*map1(curr_symbol,0)*onesixl*old_score/(new_ngram_score*ngfal*ent2*(1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size)))
									old_score-=temp*map1(curr_symbol,0)*onesixl
									
								end if
								
								temp-=temp_min
								
								thread(tn).iterations_completed+=1
								if thread(tn).solver_stop=1 then exit for,for,for,for,for
								if pausetask=1 then do:sleep 10:loop until pausetask=0
								
							next it
							
						next rr
						
					next lr
					
					for i=1 to s
						key2(i)=key1(i)
					next i
					
					curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
					
				next lv
			
			next b
			
			if thread(tn).solver_stop=0 then
				
				#include "ext_hc4.bi"
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub


sub bhdecrypt_groups_810g(byval tn_ptr as any ptr)
	
	#include "solver_variables.bi"
	
	dim as integer solver_output=6
	dim as ubyte key1(constcip)
	dim as ubyte key2(constcip)
	dim as ubyte sol(constcip)
	dim as ubyte stl(constcip)
	dim as ubyte oos(constcip)
	dim as ubyte ngrams(constcip)
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short bnba(constcip)
	dim as short frc(constcip)
	dim as short mape1(constcip)
	dim as short mape2(constcip)
	dim as byte cribkey(constcip)
	dim as double enttable(constent)
	dim as byte sr(10),lnb(0)
	dim as integer rl(40)
	dim as ushort wordgrams(constcip)
	dim as ushort wordgrams2(constcip)
	
	dim as short maps(constcip)
	dim as short maps2(constcip)
	dim as integer mi,mj

	do 'wait for input
		
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 8:tempdiv=3.0
				case 9:tempdiv=2.5
'				case 8:tempdiv=2.5
				case 10:tempdiv=2
			end select
			
			#include "solver_settings.bi"
			
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			if use_cribs=1 then
				for i=1 to s
					cribkey(i)=thread(tn).ckey(i)
				next i
			end if
			
			#include "ext_hc1.bi"
			
			for b=1 to bm
				
				#include "ext_hc2.bi" 'may change l and s for hill climbers
				
				frcmax=0
				for i=1 to l
					nba(i)=thread(tn).cip(i)
					frc(nba(i))+=1
					if frc(nba(i))>frcmax then frcmax=frc(nba(i))
				next i
				
				dim as short map1(s,frcmax)
				dim as short map2(s,frcmax*ngram_size)
				dim as short wordmap2(s,frcmax*wngs)
				dim as short map2b(s,frcmax*ngram_size)
				
				'------------------------ monoalphabetic groups ------------------------
				dim as integer ghp=0,curr_testg=1
				dim as integer dx=thread(tn).dim_x
				'dim as integer dy=thread(tn).dim_y
				dim as short mapg(l) 'cipher position to group
				dim as short mapgh(dx,constfrq) 'group, homophones
				dim as uinteger testg(dx)
				dim as double lsdx=l/s*dx
				if lsdx<1 then lsdx=1
				lsdx*=4  ' beijinghouse note: why *4? just magic number? let's try others
				
				'j=0
				'for i=1 to ngram_size-1
				'	j+=1
				'	if j>dx then j=1
				'	mapg(i)=j
				'next i
				'j=0
				'for i=ngram_size to al
				'	j+=1
				'	if j>dx then j=1
				'	mapg(i)=j
				'next i
				'j=0
				'for i=al+1 to l
				'	j+=1
				'	if j>dx then j=1
				'	mapg(i)=j
				'next i
				
				for i=1 to l
					j+=1
					if j>dx then j=1
					mapg(i)=j
				next i
				
				for i=1 to dx
					for j=0 to constfrq
						mapgh(i,j)=0
					next j
				next i
				'------------------------------------------------------------------
				
				for i=1 to l*ngram_size
					enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
				next i
				
				for i=1 to s
					frc(i)=0
					map1(i,0)=0
					map2(i,0)=0
					wordmap2(i,0)=0
					maps(i)=i
					mape2(i)=0
				next i
				
				for i=1 to l
					map1(nba(i),0)+=1
					map1(nba(i),map1(nba(i),0))=i
					mape1(i)=0
				next i

				h=1 ' create translation array that orders symbols from 1 to s by frequency
				for i=l to 1 step -1
					for j=1 to s
						if map1(j,0) = i then
							oos(h) = j
							h+=1
						end if
					next j
				next i

				
				for i=1 to l-(ngram_size-1) 'entropy reduction
					for j=0 to ngram_size-1
						mape1(i+j)+=1
					next j
				next i
				
				for i=1 to l 'entropy non-reduction (temp lazy hack to test)
					mape1(i)=ngram_size
				next i
				
				j=0
				for i=1 to l
					mape2(nba(i))+=mape1(i)
					j+=mape1(i)
				next i
				ent_score_norm=(l*ngram_size)/j
				
				for i=1 to l
					for j=0 to ngram_size-1
						h=i-(ngram_size-1)
						if h+j>0 andalso h+j<=l then  'h+j<l-(ngram_size-2) then
							e=0
							for k=1 to map2(nba(i),0)
								if map2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								map2(nba(i),0)+=1
								map2(nba(i),map2(nba(i),0))=h+j
								map2b(nba(i),map2(nba(i),0))=j
							end if
						end if
					next j
				next i

				for i=1 to l
					for j=0 to wngs-1
						h=i-(wngs-1)
						if h+j>0 andalso h+j<l-(wngs-2) then
							e=0
							for k=1 to wordmap2(nba(i),0)
								if wordmap2(nba(i),k)=h+j then
									e=1
									exit for
								end if
							next k
							if e=0 then
								wordmap2(nba(i),0)+=1
								wordmap2(nba(i),wordmap2(nba(i),0))=h+j
							end if
						end if
					next j
				next i

				start_temp=(temp1/4.61538)/((s/l)/log(l))
				start_temp/=m_ioc2(nba(),l,s,2)^0.75
				curr_temp=start_temp
				ngf=(ngramfactor/1.04577677)*ent_score_norm
				ngf/=1+(s/l)*multiplicityweight
				ngfal=ngf/al
				onesixl=1.7/l
				hi=1.0/highgram
				best_score=0
				
				' beijinghouse: allow nearly instant output of first pass
	'			solution_timer=timer-solvesub_solutionreleasetimer
				
				for lv=1 to lvmax
					
					for lr=1 to sr(lv)
						
						iterations=(iterations_total/sr(lv))/lvmax
				
						for rr=1 to random_restarts
							
							erase frq

							' initialization for experimental letter picker after this section (to track group letter assignments)
	'						ghp=0
	'						curr_testg=1
	'						for i=1 to dx
	'							for j=0 to constfrq
	'								mapgh(i,j)=0
	'							next j
	'							testg(i)=0
	'						next i
							
							if lv=1 then
								'mutexlock csolmutex
								for i=1 to s
									if use_cribs=0 then
										state=48271*state and 2147483647
										new_letter=abc_size*state shr 31
									else
										if cribkey(i)=0 then
											state=48271*state and 2147483647
											new_letter=abc_size*state shr 31
											'------------------------ monoalphabetic groups ------------------------
											'state=48271*state and 2147483647
											'dim as double rndroll=state/2147483648
											'if rndroll>solvesub_bigrambestsol then 'solvesub_bigrambestsol
											'	state=48271*state and 2147483647 'use random
											'	new_letter=abc_size*state shr 31
											'else
											'	if csol(0,100)=1 then 'use best solution
											'		new_letter=csol(map1(i,1),100)
											'	else
											'		state=48271*state and 2147483647
											'		new_letter=abc_size*state shr 31
											'	end if
											'end if
											'------------------------------------------------------------------
										else
											new_letter=cribkey(i)-1
'											k=mapg(map1(i,1)) 'find group (assume well formed so first instance = actual group)
'											mapgh(k,new_letter)=1 'mark cribed letters as off-limits for experimental picker below
										end if
									end if
									stl(i)=new_letter
									frq(new_letter)+=mape2(i) ' remove if retesting experimental section below
									for j=1 to map1(i,0)
										sol(map1(i,j))=new_letter
									next j
								next i
								'mutexunlock csolmutex
							else
								for i=1 to s
									stl(i)=key2(i)
										frq(key2(i))+=mape2(i) ' remove if retesting experimental section below
									for j=1 to map1(i,0)
										sol(map1(i,j))=key2(i)
									next j
								next i
							end if

							' experimental reinitialization routine that assigns letters to symbols
							' in a way that biases more frequent symbols to be more common letters

							if 1=0 then 'lv=1 then ''' section blocked out for now since doesn't work well yet
								'mutexlock csolmutex
								for i=1 to s
									if use_cribs=0 or cribkey(oos(i))=0 then
										
										k=mapg(map1(oos(i),1)) 'find group (assume well formed so first instance = actual group)
										for j=1 to 26
											if mapgh(k,mcl(j))=0 then ' if no letter in group has this letter yet
												state=48271*state and 2147483647
												h=100*state shr 31						' pick number from 0 to 100
												if h > 50 then								' half the time TODO: find proper fraction to assign
													new_letter=mcl(j)
													mapgh(k,new_letter)=1 'add homophone to group
													exit for	' stop going through list once one is picked
												end if
											end if	
										next j

									else
										new_letter=cribkey(oos(i))-1
									end if
									stl(oos(i))=new_letter
									frq(new_letter)+=mape2(oos(i))
									for j=1 to map1(oos(i),0)
										sol(map1(oos(i),j))=new_letter
									next j
								next i
								'mutexunlock csolmutex
	'						else
	'							for i=1 to s
	'								stl(i)=key2(i)
	'								frq(key2(i))+=mape2(i)
	'								for j=1 to map1(i,0)
	'									sol(map1(i,j))=key2(i)
	'								next j
	'							next i
							end if
										
							'------------------------ monoalphabetic groups ------------------------
							ghp=0
							curr_testg=1
							for i=1 to dx
								for j=0 to constfrq
									mapgh(i,j)=0
								next j
								testg(i)=0
							next i
							for i=1 to s
								for j=1 to map1(i,0)
									k=mapg(map1(i,j)) 'group
									if testg(k)<curr_testg then 'check if already added to this group within current symbol
										testg(k)=curr_testg
										mapgh(k,stl(i))+=1 'add homophone to group
										if mapgh(k,stl(i))>1 then ghp+=1
									end if
								next j
								curr_testg+=1
							next i
							'------------------------------------------------------------------
							
							entropy=0
							for i=0 to abc_sizem1
								entropy+=enttable(frq(i))
							next i
							
							norm=1+(ghp/lsdx)
							
							mi=0
							mj=0
							#include "solver_picksymbol.bi"
							
							old_score=0
							temp=curr_temp
							temp_min=temp/iterations
							new_ngram_score=0

							' lazy version, add better one later if this code helps/works
							for i=l+1 to l+ngram_size-1
								sol(i)=sol(i-1) ' add copy of first few letters past end of cipher so can get full ngram scores
							next i

							select case ngram_size
								case 8
									for i=1 to l 'al
										ngrams(i)=bh8(bh4(sol(i),sol(i+1),sol(i+2),sol(i+3)),bh4(sol(i+4),sol(i+5),sol(i+6),sol(i+7)))
										new_ngram_score+=ngrams(i)
									next i
								case 9
									for j=1 to l 'al
										z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
										If z1 = 0 Then
											ngrams(j) = 0
										Else
											z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
											If z2 = 0 Then
												ngrams(j) = 0
											else
													#include "solver_case9.bi"
											EndIf
										EndIf
									
										new_ngram_score+=ngrams(j)
									next j
								'case 10
								'	for i=1 to al
								'		ngrams(i)=bh10(bh5(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4)),bh5(sol(i+5),sol(i+6),sol(i+7),sol(i+8),sol(i+9)))
								'		new_ngram_score+=ngrams(i)
								'	next i
							end select

							for i=1 to l-(wngs-1)
									if solvesub_7gwordgrams=0 then
										wordgrams(i)=g6w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5))
									else
										wordgrams(i)=g7w(sol(i),sol(i+1),sol(i+2),sol(i+3),sol(i+4),sol(i+5),sol(i+6))
									end if
							next i

							#include "solver_bonusgram_init.bi"
							
							accept=1
														
							mc=5
							mc_minus=(mc-1)/iterations
							
							for it=1 to iterations
								
								'---------------------------------------------------------------------
								
								old_letter=stl(curr_symbol)
								
								if lv=lvmax then mc-=mc_minus
								
								state=48271*state and 2147483647
								d=4*state shr 31
								
								bls=0
								if d>mc then
									#include "solver_randomnewletter.bi"
								else
									state=48271*state and 2147483647
									k=1+map2(curr_symbol,0)*state shr 31
									j=map2(curr_symbol,k)
									new_letter=abc_size
									select case ngram_size
										case 8
											#include "solver_pickletter_bh8.bi"
										case 9
											#include "solver_pickletter_bh9.bi"
									'	case 10
									'		#include "solver_pickletter_bh10.bi"
									end select
									if new_letter=old_letter or new_letter=ngram_alphabet_size then
										#include "solver_randomnewletter.bi"
									end if
								end if
								
								old_norm=norm
								
								old_entropy=entropy
								
								entropy+=enttable(frq(old_letter)-mape2(curr_symbol))-enttable(frq(old_letter))
								entropy+=enttable(frq(new_letter)+mape2(curr_symbol))-enttable(frq(new_letter))
								
								old_ngram_score=new_ngram_score
								num_ngrams=map2(curr_symbol,0)

								
								for i=1 to num_ngrams
									j=map2(curr_symbol,i)
									new_ngram_score-=ngrams(j)
								next i

									for i=1 to map1(curr_symbol,0)
										sol(map1(curr_symbol,i))=new_letter
									next i

									' lazy version, add better one later if this code helps/works
									for i=l+1 to l+ngram_size-1
										sol(i)=sol(i-1) ' add copy of first few letters past end of cipher so can get full ngram scores
									next i


								#include "solver_bonusgram_main.bi"
								
								#include "solver_fastent_bh.bi"
								
								'------------------------ monoalphabetic groups ------------------------
								for i=1 to map1(curr_symbol,0)
									j=mapg(map1(curr_symbol,i)) 'group
									if testg(j)<curr_testg then
										testg(j)=curr_testg
										if mapgh(j,old_letter)>1 then ghp-=1
										mapgh(j,old_letter)-=1
										mapgh(j,new_letter)+=1
										if mapgh(j,new_letter)>1 then ghp+=1
									end if
								next i
								curr_testg+=1
								'------------------------------------------------------------------
								
								norm=1+(ghp/lsdx)
								
								
								' beijinghouse hurts score on mono tests maybe? should it be disabled?	
								#include "solver_wordscore.bi"

								' works by adding in upcoming wordscore to test for score needed
								score_needed=(old_score*norm/ent2/ngfal/(1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size))-new_ngram_score)*hi
																
								if num_ngrams>score_needed then 'beijinghouse score_needed optimization
									select case ngram_size 
										case 8
											for i=1 to num_ngrams
												z=0
												j=map2(curr_symbol,i)
												z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
												if z1<>0 then
													z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
													if z2<>0 then z=bh8(z1,z2)
												end if
												score_needed-=z*hi
												if (num_ngrams-i)<score_needed then exit for
												new_ngram_score+=z
												if score_needed<0 then exit for
											next i
										case 9
											for i=1 to num_ngrams
												j=map2(curr_symbol,i)
												z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
												If z1 = 0 Then
													z = 0
												Else
													z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
													If z2 = 0 Then
														z = 0
													else
															#include "solver_case9z.bi"
													EndIf
												endif
												score_needed-=z*hi
												if num_ngrams-i<score_needed then exit for
												new_ngram_score+=z
												if score_needed<0 then exit for
											next i
										'case 10
										'	for i=1 to num_ngrams
										'		z=0
										'		j=map2(curr_symbol,i)
										'		z1=bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
										'		if z1<>0 then
										'			z2=bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9))
										'			if z2<>0 then z=bh10(z1,z2)
										'		end if
										'		score_needed-=z*hi
										'		if (num_ngrams-i)<score_needed then exit for
										'		new_ngram_score+=z
										'		if score_needed<0 then exit for
										'	next i
									end select
								end if

									if score_needed>=0 then
										for i=1 to map1(curr_symbol,0)
											sol(map1(curr_symbol,i))=old_letter
											'------------------------ monoalphabetic groups ------------------------
											j=mapg(map1(curr_symbol,i)) 'group
											if testg(j)<curr_testg then
												testg(j)=curr_testg
												if mapgh(j,new_letter)>1 then ghp-=1
												mapgh(j,new_letter)-=1
												mapgh(j,old_letter)+=1
												if mapgh(j,old_letter)>1 then ghp+=1
											end if
											'------------------------------------------------------------------
										next i
										curr_testg+=1
																				
										' lazy version, add better one later if this code helps/works
										for i=l+1 to l+ngram_size-1
											sol(i)=sol(i-1) ' add copy of first few letters past end of cipher so can get full ngram scores
										next i

									end if
								
								'---------------------------------------------------------------------
								
								if score_needed<0 then

									accept = 1
									#include "solver_bonusgram_tail.bi"

									frq(old_letter)-=mape2(curr_symbol)
									frq(new_letter)+=mape2(curr_symbol)
									stl(curr_symbol)=new_letter
									
									select case ngram_size
										case 8
											for h=1 to num_ngrams
												j=map2(curr_symbol,h)
												ngrams(j)=bh8(bh4(sol(j),sol(j+1),sol(j+2),sol(j+3)),bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7)))
												if h>i then new_ngram_score+=ngrams(j)
											next h
										case 9
											for k=1 to num_ngrams
												j=map2(curr_symbol,k)
												z1 = g53(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
												If z1 = 0 Then
													ngrams(j) = 0
												Else
													z2 = g54(sol(j+4),sol(j+5),sol(j+6),sol(j+7),sol(j+8))
													If z2 = 0 Then
														ngrams(j) = 0
													else
															#include "solver_case9.bi"
													EndIf
												endif
												if k>i then new_ngram_score+=ngrams(j)
											next k
									'	case 10
									'		for h=1 to num_ngrams
									'			j=map2(curr_symbol,h)
									'			ngrams(j)=bh10(bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)),bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9)))
									'			if h>i then new_ngram_score+=ngrams(j)
									'		next h
									end select

									for i=1 to l-(wngs-1) 'save the new values since they worked
										wordgrams(i)=wordgrams2(i)
									next i
									
									new_score=new_ngram_score*ngfal*ent2
									
									' add in w_score if it passes
									new_score*=1+solvesub_wgramfactor*(wscore/255.0)/(14.0-ngram_size) 'multiplicative
									
									new_score/=norm 'monoalphabetic groups
									
									old_score=new_score
									
									#include "solver_picksymbol.bi"
									
									if new_score>best_score then
										
										solution_improved=1
										best_score=new_score+0.00001
										
										'if ghp=0 or lv=lvmax then
											thread(tn).sectime=timer-sectimer
											for i=1 to l
												thread(tn).sol(i)=alphabet(sol(i))
											next i
											thread(tn).ent=entropy
											thread(tn).multiplicity=s/l
											thread(tn).ioc2=ghp 'collisions
										'end if
										
										for i=1 to s
											key1(i)=stl(i)
										next i
										
										if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
											solution_improved=0 'consume
											solution_timer=timer
											#include "solver_ioc.bi"
											#include "solver_advstats.bi"
											thread(tn).score=best_score
											#include "solver_output.bi"
											#include "ext_hc3.bi"
										end if
										
									end if
									
								else
									
									accept = 0
									
									norm=old_norm 'monoalphabetic groups
									
									new_ngram_score=old_ngram_score
									entropy=old_entropy
									
									#include "solver_picksymbol.bi"

									' TODO: find way to do this with optimization above
'									old_score-=temp*map1(curr_symbol,0)*onesixl*old_score/new_score
									old_score-=temp*map1(curr_symbol,0)*onesixl
									
								end if
								
								temp-=temp_min
								
								thread(tn).iterations_completed+=1
								if thread(tn).solver_stop=1 then exit for,for,for,for,for
								if pausetask=1 then do:sleep 10:loop until pausetask=0
								
							next it
							
						next rr
						
					next lr
					
					for i=1 to s
						key2(i)=key1(i)
					next i
					
					curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
					
				next lv
			
			next b
			
			if thread(tn).solver_stop=0 then
				
				#include "ext_hc4.bi"
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub


sub bhdecrypt_poly_hafer_567810g(byval tn_ptr as any ptr)
	
	#include "solver_variables.bi"
	
	dim as integer solver_output=1
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short sol(constcip)
	dim as double es(constcip)
	dim as short pps(constcip)
	dim as short frc(constcip)
	dim as short mape1(constcip)
	dim as ubyte ngrams(constcip)
	dim as double enttable(constent)
	dim as short abcshift(constfrq,constfrq)
	dim as integer old_letters(constfrq)
	dim as integer new_letters(constfrq)
	dim as integer sr(10),lnb(0)
	
	dim as short maps(1,constcip)
	dim as short maps2(1,constcip)
	dim as integer mi(1),mj(1)
	
	do 'wait for input
	
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 5:tempdiv=3
				case 6:tempdiv=2.75
				case 7:tempdiv=2.5
				case 8:tempdiv=2.25
				case 10:tempdiv=2
			end select
			
			#include "solver_settings.bi"
			
			m=s/l
			ls=l/s
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			shifts=thread(tn).key(s+1)
			for i=0 to abc_sizem1
				for j=0 to shifts-1
					abcshift(i,j)=i+thread(tn).key(s+2+j)
					if abcshift(i,j)<0 then abcshift(i,j)+=abc_size
					if abcshift(i,j)>abc_sizem1 then abcshift(i,j)-=abc_size
				next j
			next i
			
			for i=1 to l*ngram_size
				enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
			next i
			
			frcmax=0
			for i=1 to l
				nba(i)=thread(tn).cip(i)
				frc(nba(i))+=1
				if frc(nba(i))>frcmax then frcmax=frc(nba(i))
			next i
			
			'erase frq
			
			dim as short map1(s,frcmax)
			dim as short map2(s,frcmax*ngram_size)
			dim as short map2b(s,frcmax*ngram_size)
			dim as short map3(l,ngram_size)
			dim as short poly_letter(s,abc_size)
			dim as short poly_count(s,abc_size)
			
			dim as short sol_sr1(l)
			dim as short poly_letter_sr1(s,abc_size)
			dim as short poly_count_sr1(s,abc_size)
			dim as short sol_sr2(l)
			dim as short poly_letter_sr2(s,abc_size)
			dim as short poly_count_sr2(s,abc_size)
			
			for i=1 to s
				frc(i)=0
				map1(i,0)=0
				map2(i,0)=0
				for j=0 to abc_size
					poly_count(i,j)=0
					poly_letter(i,j)=0
				next j
				for j=0 to 1
					maps(j,i)=i
				next j
			next i
			
			for i=1 to l
				map3(i,0)=0
				map1(nba(i),0)+=1
				map1(nba(i),map1(nba(i),0))=i
				mape1(i)=0
			next i
			
			k=0
			for i=1 to l-(ngram_size-1) 'entropy reduction
				for j=0 to ngram_size-1
					mape1(i+j)+=1
				next j
				k+=mape1(i)
			next i
			ent_score_norm=(l*ngram_size)/k
			
			for i=1 to l
				for j=0 to (ngram_size-1)
					h=i-(ngram_size-1)
					if h+j>0 andalso h+j<l-(ngram_size-2) then
						e=0
						for k=1 to map2(nba(i),0)
							if map2(nba(i),k)=h+j then
								e=1
								exit for
							end if
						next k
						if e=0 then
							map2(nba(i),0)+=1
							map2(nba(i),map2(nba(i),0))=h+j
							map2b(nba(i),map2(nba(i),0))=j
						end if
						e=0
						for k=1 to map3(i,0) 'check needed ???
							if map3(i,k)=h+j then
								e=1
								exit for
							end if
						next k
						if e=0 then
							map3(i,0)+=1
							map3(i,map3(i,0))=h+j
						end if
					end if
				next j
			next i
			
		 	ns=s
			for i=1 to s
				pps(i)=thread(tn).key(i)
				if pps(i)>map1(i,0) then pps(i)=map1(i,0)
				if pps(i)>abc_sizem1 then pps(i)=abc_sizem1
				ns+=pps(i)
			next i
			
			'start_temp=(temp1/1.3)*(l/ns)
			start_temp=((temp1/4.61538)/8)/((ns/l)/log(l)) '2.7
			start_temp/=m_ioc2(nba(),l,s,2)^0.75
			best_score=0
			onesixl=1.7/l
			ngfent=ngramfactor*ent_score_norm/al
			
			solution_timer=timer
			
			for lv=1 to lvmax
				
				for lr=1 to sr(lv)
					
					iterations=(iterations_total/sr(lv))/lvmax
					
					for rr=1 to random_restarts
						
						tes=0
						erase frq
						
						for i=1 to s
							for j=0 to abc_size
								poly_count(i,j)=0
								poly_letter(i,j)=0
							next j
						next i
						
						if lv=1 then
							for i=1 to s
								if pps(i)=1 then
									state=48271*state and 2147483647
									poly_letter(i,0)=abc_size*state shr 31
								else
									state=48271*state and 2147483647
									r=abc_size*state shr 31
									for j=0 to shifts-1 'pps(i)-1
										poly_letter(i,j)=abcshift(r,j)
									next j
								end if
								for j=1 to map1(i,0) 'assign initial letter distribution
									state=48271*state and 2147483647
									rp=pps(i)*state shr 31
									poly_count(i,rp)+=1
									sol(map1(i,j))=poly_letter(i,rp)
									frq(poly_letter(i,rp))+=mape1(map1(i,j))
								next j
								acu=0
								for j=0 to pps(i)-1
									if poly_count(i,j)>0 then acu+=1
								next j
								es(i)=acu
								tes+=acu
							next i
						else
							for i=1 to l
								sol(i)=sol_sr2(i)
								frq(sol_sr2(i))+=mape1(i)
							next i
							for i=1 to s
								for j=0 to pps(i)-1
									poly_letter(i,j)=poly_letter_sr2(i,j)
									poly_count(i,j)=poly_count_sr2(i,j)
								next j
								acu=0
								for j=0 to pps(i)-1
									if poly_count(i,j)>0 then acu+=1
								next j
								es(i)=acu
								tes+=acu
							next i
						end if
						
						'iterations=iterations_total
						
						old_score=0
						temp=start_temp
						temp_min=temp/iterations
						
						rc0_symbol=0
						rc0_number=0
						do
							rc0_symbol+=1
							if rc0_symbol>s then rc0_symbol=1
						loop until pps(rc0_symbol)>1
						rc1_poly=1
						rc1_symbol=1
						
						for i=0 to 1
							mi(i)=0
							mj(i)=0
							#include "solver_picksymbol_hafer.bi"
						next i
						
						mc=5
						mc_minus=(mc-1)/iterations
						
						new_ngram_score=0
						accept=1
						
						entropy=0
						for i=0 to abc_sizem1
							entropy+=enttable(frq(i))
						next i
						
						#include "solver_ngram_init.bi"
						
						for it=1 to iterations
							
							if lv=lvmax then mc-=mc_minus
							
							state=48271*state and 2147483647
							rndroll=state/2147483648
							
							if rndroll>solvesub_sdbias then
								rchange=0
								rc0_number+=1
								if rc0_number>map1(rc0_symbol,0) then
									rc0_number=1
									do
										#include "solver_picksymbol_hafer.bi"
									loop until pps(curr_symbol)>1
									rc0_symbol=curr_symbol
								end if
								if accept=0 then old_score-=temp*map3(map1(rc0_symbol,rc0_number),0)/al*(old_score/new_score)
							else 'letter
								rchange=1
								#include "solver_picksymbol_hafer.bi"
								rc1_symbol=curr_symbol
								if accept=0 then old_score-=temp*map2(rc1_symbol,0)/al*(old_score/new_score)
							end if
							
							old_entropy=entropy
							
							if rchange=0 then 'change distribution
								
								pos1=map1(rc0_symbol,rc0_number)
								old_letter=sol(pos1)
								for i=0 to pps(rc0_symbol)-1 'find old letter
									if old_letter=poly_letter(rc0_symbol,i) then
										old_poly=i
										poly_count(rc0_symbol,i)-=1
										exit for
									end if
								next i
								do
									state=48271*state and 2147483647
									rp=pps(rc0_symbol)*state shr 31
									new_letter=poly_letter(rc0_symbol,rp)
								loop until new_letter<>old_letter
								poly_count(rc0_symbol,rp)+=1
								prev_es=es(rc0_symbol)
								tes-=prev_es
								acu=0
								for i=0 to pps(rc0_symbol)-1
									if poly_count(rc0_symbol,i)>0 then acu+=1
								next i
								es(rc0_symbol)=acu
								tes+=acu
								sol(pos1)=new_letter
								entropy+=enttable(frq(old_letter)-mape1(pos1))-enttable(frq(old_letter))
								entropy+=enttable(frq(new_letter)+mape1(pos1))-enttable(frq(new_letter))
								
								old_ngram_score=new_ngram_score
								
								select case ngram_size
									case 5
										for i=1 to map3(pos1,0)
											j=map3(pos1,i)
											new_ngram_score+=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))-ngrams(j)
										next i
									case 6
										for i=1 to map3(pos1,0)
											j=map3(pos1,i)
											new_ngram_score+=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))-ngrams(j)
										next i
									case 7
										for i=1 to map3(pos1,0)
											j=map3(pos1,i)
											new_ngram_score+=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))-ngrams(j)
										next i
									case 8
										for i=1 to map3(pos1,0)
											z=0
											j=map3(pos1,i)
											z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
											if z1<>0 then
												z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
												if z2<>0 then z=bh8(z1,z2)
											end if
											new_ngram_score+=z-ngrams(j)
										next i
									'case 10
									'	for i=1 to map3(pos1,0)
									'		z=0
									'		j=map3(pos1,i)
									'		z1=bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
									'		if z1<>0 then
									'			z2=bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9))
									'			if z2<>0 then z=bh10(z1,z2)
									'		end if
									'		new_ngram_score+=z-ngrams(j)
									'	next i
								end select
								
							else 'change letters
								
								state=48271*state and 2147483647
								d=4*state shr 31
								
								for i=0 to shifts-1
									old_letters(i)=poly_letter(rc1_symbol,i)
								next i
								
								#include "solver_pickletter_hafer.bi"
								
								old_ngram_score=new_ngram_score
								
								entropy=0
								for i=0 to abc_sizem1
									entropy+=enttable(frq(i))
								next i
								
								select case ngram_size
									case 5
										for i=1 to map2(rc1_symbol,0)
											j=map2(rc1_symbol,i)
											new_ngram_score+=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))-ngrams(j)
										next i
									case 6
										for i=1 to map2(rc1_symbol,0)
											j=map2(rc1_symbol,i)
											new_ngram_score+=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))-ngrams(j)
										next i
									case 7
										for i=1 to map2(rc1_symbol,0)
											j=map2(rc1_symbol,i)
											new_ngram_score+=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))-ngrams(j)
										next i
									case 8
										for i=1 to map2(rc1_symbol,0)
											j=map2(rc1_symbol,i)
											z=0
											z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
											if z1<>0 then
												z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
												if z2<>0 then z=bh8(z1,z2)
											end if
											new_ngram_score+=z-ngrams(j)
										next i
									'case 10
									'	for i=1 to map2(rc1_symbol,0)
									'		j=map2(rc1_symbol,i)
									'		z=0
									'		z1=bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
									'		if z1<>0 then
									'			z2=bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9))
									'			if z2<>0 then z=bh10(z1,z2)
									'		end if
									'		new_ngram_score+=z-ngrams(j)
									'	next i
								end select
								
							end if
							
							new_score=new_ngram_score*ngfent/(1+(((tes-s)/l)*multiplicityweight))
							
							select case solvesub_fastent
								case 0:new_score*=fastpow1_single(entropy,entweight)
								case 1:new_score*=entropy^0.25
								case 2:new_score*=entropy^0.5
								case 3:new_score*=entropy^0.75
								case 4:new_score*=entropy
								case 5:new_score*=entropy^1.5
								case 6:new_score*=entropy*entropy
							end select
							
							if new_score>old_score then
								
								if rchange=0 then
									
									frq(old_letter)-=mape1(pos1)
									frq(new_letter)+=mape1(pos1)
									
									select case ngram_size
										case 5
											for i=1 to map3(pos1,0)
												j=map3(pos1,i)
												ngrams(j)=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
											next i
										case 6
											for i=1 to map3(pos1,0)
												j=map3(pos1,i)
												ngrams(j)=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))
											next i
										case 7
											for i=1 to map3(pos1,0)
												j=map3(pos1,i)
												ngrams(j)=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))
											next i
										case 8
											for i=1 to map3(pos1,0)
												j=map3(pos1,i)
												ngrams(j)=bh8(bh4(sol(j),sol(j+1),sol(j+2),sol(j+3)),bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7)))
											next i
										'case 10
										'	for i=1 to map3(pos1,0)
										'		j=map3(pos1,i)
										'		ngrams(j)=bh10(bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)),bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9)))
										'	next i
									end select
									
								else 'if rchange=1 then
									
									select case ngram_size
										case 5
											for i=1 to map2(rc1_symbol,0)
												j=map2(rc1_symbol,i)
												ngrams(j)=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
											next i
										case 6
											for i=1 to map2(rc1_symbol,0)
												j=map2(rc1_symbol,i)
												ngrams(j)=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))
											next i
										case 7
											for i=1 to map2(rc1_symbol,0)
												j=map2(rc1_symbol,i)
												ngrams(j)=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))
											next i
										case 8
											for i=1 to map2(rc1_symbol,0)
												j=map2(rc1_symbol,i)
												ngrams(j)=bh8(bh4(sol(j),sol(j+1),sol(j+2),sol(j+3)),bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7)))
											next i
										'case 10
										'	for i=1 to map2(rc1_symbol,0)
										'		j=map2(rc1_symbol,i)
										'		ngrams(j)=bh10(bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)),bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9)))
										'	next i
									end select
									
								end if
								
								old_score=new_score
								
								if new_score>best_score then
									
									solution_improved=1
									best_score=new_score+0.00001
									thread(tn).sectime=timer-sectimer
									thread(tn).ent=entropy
									thread(tn).effectivesymbols=tes
									thread(tn).multiplicity=tes/l
									for i=1 to l
										thread(tn).sol(i)=alphabet(sol(i))
									next i
									for i=1 to l
										sol_sr1(i)=sol(i)
									next i
									for i=1 to s
										for j=0 to pps(i)-1
											poly_letter_sr1(i,j)=poly_letter(i,j)
											poly_count_sr1(i,j)=poly_count(i,j)
										next j
									next i
									
								end if
								
								if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
									solution_improved=0 'consume
									solution_timer=timer
									#include "solver_ioc.bi"
									#include "solver_advstats.bi"
									thread(tn).score=best_score
									#include "solver_output.bi"
								end if
								
								accept=1
								
							else
								
								new_ngram_score=old_ngram_score
								
								if rchange=0 then 'undo distribution
									sol(map1(rc0_symbol,rc0_number))=old_letter
									poly_count(rc0_symbol,rp)-=1
									poly_count(rc0_symbol,old_poly)+=1
									tes-=es(rc0_symbol)
									tes+=prev_es
									es(rc0_symbol)=prev_es
								else 'undo letter
									for i=0 to shifts-1
										poly_letter(rc1_symbol,i)=old_letters(i)
									next i
									for i=1 to map1(rc1_symbol,0)
										for j=0 to shifts-1
											if new_letters(j)=sol(map1(rc1_symbol,i)) then
												sol(map1(rc1_symbol,i))=old_letters(j)
												frq(new_letters(j))-=mape1(map1(rc1_symbol,i))
												frq(old_letters(j))+=mape1(map1(rc1_symbol,i))
												exit for
											end if
										next j
									next i
								end if
								
								entropy=old_entropy
								accept=0
								
							end if
							
							temp-=temp_min
							
							thread(tn).iterations_completed+=1
							if thread(tn).solver_stop=1 then exit for
							if pausetask=1 then do:sleep 10:loop until pausetask=0
							
						next it
						
					next rr
					
				next lr
				
				for i=1 to l
					sol_sr2(i)=sol_sr1(i)
				next i
				for i=1 to s
					for j=0 to pps(i)-1
						poly_letter_sr2(i,j)=poly_letter_sr1(i,j)
						poly_count_sr2(i,j)=poly_count_sr1(i,j)
					next j
				next i
				
				curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
				
			next lv
			
			if thread(tn).solver_stop=0 then
				
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
			
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub


sub bhdecrypt_poly_567810g(byval tn_ptr as any ptr)

	#include "solver_variables.bi"
	
	dim as integer solver_output=1
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short sol(constcip)
	dim as double es(constcip)
	dim as short pps(constcip)
	dim as short frc(constcip)
	dim as short mape1(constcip)
	dim as ubyte ngrams(constcip)
	dim as double enttable(constent)
	
	dim as uinteger pcc,poscheck(constcip)
	dim as integer sr(10),lnb(0)
	
	do 'wait for input
	
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			'lvmax=1 'solvesub_subrestartlevels
			'for i=1 to lvmax
			'	sr(i)=solvesub_subr(i)
			'next i
			
			'select case ngram_size
			'	case 5:tempdiv=3
			'	case 6:tempdiv=2.75
			'	case 7:tempdiv=2.5
			'	case 8:tempdiv=2.25
			'	case 10:tempdiv=2
			'end select
			
			#include "solver_settings.bi"
			
			m=s/l
			ls=l/s
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			for i=1 to l*ngram_size
				enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
			next i
			
			frcmax=0
			for i=1 to l
				nba(i)=thread(tn).cip(i)
				frc(nba(i))+=1
				if frc(nba(i))>frcmax then frcmax=frc(nba(i))
			next i
			
			dim as short map1(s,frcmax)
			dim as short map2(s,frcmax*ngram_size)
			'dim as short map2b(s,frcmax*ngram_size)
			dim as short map3(l,ngram_size)
			'dim as short map3b(l,ngram_size)
			dim as short poly_letter(s,abc_size)
			dim as short poly_count(s,abc_size)
			
			'dim as short sol_sr1(l)
			'dim as short poly_letter_sr1(s,abc_size)
			'dim as short poly_count_sr1(s,abc_size)
			'dim as short sol_sr2(l)
			'dim as short poly_letter_sr2(s,abc_size)
			'dim as short poly_count_sr2(s,abc_size)
			
			for i=1 to s
				frc(i)=0
				map1(i,0)=0
				map2(i,0)=0
				for j=0 to abc_size
					poly_count(i,j)=0
					poly_letter(i,j)=0
				next j
			next i
			
			for i=1 to l
				map3(i,0)=0
				map1(nba(i),0)+=1
				map1(nba(i),map1(nba(i),0))=i
				mape1(i)=0
			next i
			
			k=0
			for i=1 to l-(ngram_size-1) 'entropy reduction
				for j=0 to ngram_size-1
					mape1(i+j)+=1
				next j
				k+=mape1(i)
			next i
			ent_score_norm=(l*ngram_size)/k
			
			for i=1 to l
				for j=0 to (ngram_size-1)
					h=i-(ngram_size-1)
					if h+j>0 andalso h+j<l-(ngram_size-2) then
						e=0
						for k=1 to map2(nba(i),0)
							if map2(nba(i),k)=h+j then
								e=1
								exit for
							end if
						next k
						if e=0 then
							map2(nba(i),0)+=1
							map2(nba(i),map2(nba(i),0))=h+j
							'map2b(nba(i),map2(nba(i),0))=j
						end if
						e=0
						for k=1 to map3(i,0) 'check needed ???
							if map3(i,k)=h+j then
								e=1
								exit for
							end if
						next k
						if e=0 then
							map3(i,0)+=1
							map3(i,map3(i,0))=h+j
							'map3b(i,map3(i,0))=j
						end if
					end if
				next j
			next i
			
		 	ns=s
			for i=1 to s
				pps(i)=thread(tn).key(i)
				if pps(i)>map1(i,0) then pps(i)=map1(i,0)
				if pps(i)>abc_sizem1 then pps(i)=abc_sizem1
				ns+=pps(i)
			next i
			
			'start_temp=(temp1/1.3)*(l/ns)
			start_temp=((temp1/4.61538)/3)/((ns/l)/log(l))
			start_temp/=m_ioc2(nba(),l,s,2)^0.75
			curr_temp=start_temp
			best_score=0
			onesixl=1.7/l
			ngfent=ngramfactor*ent_score_norm/al
			solution_timer=timer
			
			'for lv=1 to lvmax
				
				'for lr=1 to sr(lv)
					
					'iterations=(iterations_total/sr(lv))/lvmax
					
					'for rr=1 to random_restarts
						
						erase frq
						erase poscheck
						pcc=0
						tes=0
						
						'for i=1 to s
						'	for j=0 to abc_size
						'		poly_count(i,j)=0
						'		poly_letter(i,j)=0
						'	next j
						'next i
						
						'if lv=1 then
							for i=1 to s
								for j=0 to pps(i)-1 'assign initial letters to polyphones
									do
										e=0
										state=48271*state and 2147483647
										new_letter=abc_size*state shr 31
										for k=0 to j-1
											if new_letter=poly_letter(i,k) then
												e=1
												exit for
											end if
										next k
									loop until e=0
									poly_letter(i,j)=new_letter
								next j
								for j=1 to map1(i,0) 'assign initial letter distribution
									state=48271*state and 2147483647
									rp=pps(i)*state shr 31
									poly_count(i,rp)+=1
									sol(map1(i,j))=poly_letter(i,rp)
									frq(poly_letter(i,rp))+=mape1(map1(i,j))
								next j
								acu=0
								for j=0 to pps(i)-1
									if poly_count(i,j)>0 then acu+=1
								next j
								es(i)=acu
								tes+=acu
							next i
						'else
						'	for i=1 to l
						'		sol(i)=sol_sr2(i)
						'		frq(sol_sr2(i))+=mape1(i)
						'	next i
						'	for i=1 to s
						'		for j=0 to pps(i)-1
						'			poly_letter(i,j)=poly_letter_sr2(i,j)
						'			poly_count(i,j)=poly_count_sr2(i,j)
						'		next j
						'		acu=0
						'		for j=0 to pps(i)-1
						'			if poly_count(i,j)>0 then acu+=1
						'		next j
						'		es(i)=acu
						'		tes+=acu
						'	next i
						'end if
						
						iterations=iterations_total
						
						old_score=0
						temp=curr_temp
						temp_min=temp/iterations
						
						rc0_symbol=0
						rc0_number=0
						do
							rc0_symbol+=1
							if rc0_symbol>s then rc0_symbol=1
						loop until pps(rc0_symbol)>1
						
						rc1_symbol=1
						rc1_poly=1
						
						new_ngram_score=0
						
						'mc=5
						'mc_minus=(mc-1)/iterations
						
						entropy=0
						for i=0 to abc_sizem1
							entropy+=enttable(frq(i))
						next i
						
						#include "solver_ngram_init.bi"
						
						for it=1 to iterations
							
							'if lv=lvmax then mc-=mc_minus
							
							state=48271*state and 2147483647
							rndroll=state/2147483648
							
							if rndroll>solvesub_sdbias then
								rchange=0
								rc0_number+=1
								if rc0_number>map1(rc0_symbol,0) then
									rc0_number=1
									do
										rc0_symbol+=1
										if rc0_symbol>s then rc0_symbol=1
									loop until pps(rc0_symbol)>1
								end if
								'if accept=0 then old_score-=temp*(1/l)
								if accept=0 then old_score-=temp*1*onesixl*old_score/new_score
								'if accept=0 then old_score-=temp*map3(map1(rc0_symbol,rc0_number),0)/al*(old_score/new_score)  '<--- NEW
							else 'letter
								rchange=1
								do
									rc1_poly+=1
									if rc1_poly>pps(rc1_symbol) then
										rc1_poly=1
										rc1_symbol+=1
										if rc1_symbol>s then rc1_symbol=1
									end if
								loop until poly_count(rc1_symbol,rc1_poly-1)>0
								'if accept=0 then old_score-=temp*((poly_count(rc1_symbol,rc1_poly-1))/l)
								if accept=0 then old_score-=temp*poly_count(rc1_symbol,rc1_poly-1)*onesixl*old_score/new_score
								'if accept=0 then old_score-=temp*poly_count(rc1_symbol,rc1_poly-1)/al*(old_score/new_score) '<--- NEW
							end if
							
							if rchange=0 then 'change letter distribution
								
								pos1=map1(rc0_symbol,rc0_number)
								old_letter=sol(pos1)	
								for i=0 to pps(rc0_symbol)-1 'find old letter
									if old_letter=poly_letter(rc0_symbol,i) then
										old_poly=i
										poly_count(rc0_symbol,i)-=1
										exit for
									end if
								next i						
								do 'roll new letter
									state=48271*state and 2147483647
									rp=pps(rc0_symbol)*state shr 31
									new_letter=poly_letter(rc0_symbol,rp)
								loop until new_letter<>old_letter		
								poly_count(rc0_symbol,rp)+=1
								prev_es=es(rc0_symbol)
								tes-=prev_es
								acu=0
								for i=0 to pps(rc0_symbol)-1
									if poly_count(rc0_symbol,i)>0 then acu+=1
								next i
								es(rc0_symbol)=acu
								tes+=acu
								sol(pos1)=new_letter
								
								frq(old_letter)-=mape1(pos1)
								frq(new_letter)+=mape1(pos1)
								
								'entropy+=enttable(frq(old_letter)-mape1(pos1))-enttable(frq(old_letter))
								'entropy+=enttable(frq(new_letter)+mape1(pos1))-enttable(frq(new_letter))
								
								old_ngram_score=new_ngram_score
								
								select case ngram_size
									case 5
										for i=1 to map3(pos1,0)
											j=map3(pos1,i)
											new_ngram_score+=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))-ngrams(j)
										next i
									case 6
										for i=1 to map3(pos1,0)
											j=map3(pos1,i)
											new_ngram_score+=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))-ngrams(j)
										next i
									case 7
										for i=1 to map3(pos1,0)
											j=map3(pos1,i)
											new_ngram_score+=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))-ngrams(j)
										next i
									case 8
										for i=1 to map3(pos1,0)
											z=0
											j=map3(pos1,i)
											z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
											if z1<>0 then
												z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
												if z2<>0 then z=bh8(z1,z2)
											end if
											new_ngram_score+=z-ngrams(j)
										next i
									'case 10
									'	for i=1 to map3(pos1,0)
									'		z=0
									'		j=map3(pos1,i)
									'		z1=bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
									'		if z1<>0 then
									'			z2=bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9))
									'			if z2<>0 then z=bh10(z1,z2)
									'		end if
									'		new_ngram_score+=z-ngrams(j)
									'	next i
								end select
								
							else 'change letter
								
								old_poly=rc1_poly-1
								old_letter=poly_letter(rc1_symbol,old_poly)
								
								'state=48271*state and 2147483647
								'd=4*state shr 31
								'
								'if d>mc then
									do
										e=0
										state=48271*state and 2147483647
										new_letter=abc_size*state shr 31
										for i=0 to pps(rc1_symbol)-1
											if new_letter=poly_letter(rc1_symbol,i) then
												e=1
												exit for
											end if
										next i
									loop until e=0
								'else
								'	do
								'		state=48271*state and 2147483647
								'		i=1+map1(rc1_symbol,0)*state shr 31
								'	loop until old_letter=sol(map1(rc1_symbol,i))
								'	state=48271*state and 2147483647
								'	k=1+map3(map1(rc1_symbol,i),0)*state shr 31
								'	j=map3(rc1_symbol,k)
								'	
								'	select case ngram_size
								'		case 8
								'			new_letter=abc_size
								'			curr_symbol=rc1_symbol
								'			#include "solver_pickletter_bh8_poly.bi"
								'	end select
								'	e=0
								'	for i=0 to pps(rc1_symbol)-1
								'		if new_letter=poly_letter(rc1_symbol,i) then
								'			e=1
								'			exit for
								'		end if
								'	next i
								'	if e=1 or new_letter=old_letter or new_letter=abc_size then
								'		do
								'			e=0
								'			state=48271*state and 2147483647
								'			new_letter=abc_size*state shr 31
								'			for i=0 to pps(rc1_symbol)-1
								'				if new_letter=poly_letter(rc1_symbol,i) then
								'					e=1
								'					exit for
								'				end if
								'			next i
								'		loop until e=0
								'	end if
								'end if
								
								poly_letter(rc1_symbol,old_poly)=new_letter
								for i=1 to map1(rc1_symbol,0)
									if old_letter=sol(map1(rc1_symbol,i)) then
										sol(map1(rc1_symbol,i))=new_letter
										frq(old_letter)-=mape1(map1(rc1_symbol,i))
										frq(new_letter)+=mape1(map1(rc1_symbol,i))
									end if
								next i
								
								'entropy=0
								'for i=0 to abc_sizem1
								'	entropy+=enttable(frq(i))
								'next i
								
								old_ngram_score=new_ngram_score
								
								pcc+=1
								
								for i=1 to map1(rc1_symbol,0)
									h=map1(rc1_symbol,i)
									if new_letter=sol(h) then
										select case ngram_size
											case 5
												for k=1 to map3(h,0)
													j=map3(h,k)
													if poscheck(j)<pcc then
														poscheck(j)=pcc
														new_ngram_score+=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))-ngrams(j)
													end if
												next k
											case 6
												for k=1 to map3(h,0)
													j=map3(h,k)
													if poscheck(j)<pcc then
														poscheck(j)=pcc
														new_ngram_score+=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))-ngrams(j)
													end if
												next k
											case 7
												for k=1 to map3(h,0)
													j=map3(h,k)
													if poscheck(j)<pcc then
														poscheck(j)=pcc
														new_ngram_score+=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))-ngrams(j)
													end if
												next k
											case 8
												for k=1 to map3(h,0)
													j=map3(h,k)
													if poscheck(j)<pcc then
														z=0
														poscheck(j)=pcc
														z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
														if z1<>0 then
															z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
															if z2<>0 then z=bh8(z1,z2)
														end if
														new_ngram_score+=z-ngrams(j)
													end if
												next k
											'case 10
											'	for i=k to map3(h,0)
											'		j=map3(h,k)
											'		if poscheck(j)<pcc then
											'			z=0
											'			poscheck(j)=pcc
											'			z1=bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
											'			if z1<>0 then
											'				z2=bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9))
											'				if z2<>0 then z=bh10(z1,z2)
											'			end if
											'			new_ngram_score+=z-ngrams(j)
											'		end if
											'	next k
										end select
									end if
								next i
								
							end if
							
							entropy=0
							for i=0 to abc_sizem1
								entropy+=enttable(frq(i))
							next i
							
							'ngf=ngramfactor*ent_score_norm
							'ngf/=1+(((tes-s)/l)*multiplicityweight)
							'new_score=(new_ngram_score*ngf)/al
							
							new_score=new_ngram_score*ngfent/(1+(((tes-s)/l)*multiplicityweight))
							
							select case solvesub_fastent
								case 0:new_score*=fastpow1_single(entropy,entweight)
								case 1:new_score*=entropy^0.25
								case 2:new_score*=entropy^0.5
								case 3:new_score*=entropy^0.75
								case 4:new_score*=entropy
								case 5:new_score*=entropy^1.5
								case 6:new_score*=entropy*entropy
							end select
							
							if new_score>old_score then
								
								if rchange=0 then
									'frq(old_letter)-=mape1(pos1)
									'frq(new_letter)+=mape1(pos1)
									select case ngram_size
										case 5
											for i=1 to map3(pos1,0)
												j=map3(pos1,i)
												ngrams(j)=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
											next i
										case 6
											for i=1 to map3(pos1,0)
												j=map3(pos1,i)
												ngrams(j)=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))
											next i
										case 7
											for i=1 to map3(pos1,0)
												j=map3(pos1,i)
												ngrams(j)=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))
											next i
										case 8
											for i=1 to map3(pos1,0)
												j=map3(pos1,i)
												ngrams(j)=bh8(bh4(sol(j),sol(j+1),sol(j+2),sol(j+3)),bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7)))
											next i
										'case 10
										'	for i=1 to map3(pos1,0)
										'		j=map3(pos1,i)
										'		ngrams(j)=bh10(bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)),bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9)))
										'	next i
									end select	
								else 'rchange=1
									for i=1 to map1(rc1_symbol,0)
										h=map1(rc1_symbol,i)
										if new_letter=sol(h) then
											select case ngram_size
												case 5
													for k=1 to map3(h,0)
														j=map3(h,k)
														if poscheck(j)=pcc then ngrams(j)=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
													next k
												case 6
													for k=1 to map3(h,0)
														j=map3(h,k)
														if poscheck(j)=pcc then ngrams(j)=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))
													next k
												case 7
													for k=1 to map3(h,0)
														j=map3(h,k)
														if poscheck(j)=pcc then ngrams(j)=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))
													next k
												case 8
													for k=1 to map3(h,0)
														j=map3(h,k)
														if poscheck(j)=pcc then ngrams(j)=bh8(bh4(sol(j),sol(j+1),sol(j+2),sol(j+3)),bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7)))
													next k
												'case 10
												'	for k=1 to map3(h,0)
												'		j=map3(h,k)
												'		if poscheck(j)=pcc then ngrams(j)=bh10(bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)),bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9)))
												'	next k
											end select
										end if
									next i
								end if
								
								old_score=new_score
								
								if new_score>best_score then
									
									solution_improved=1
									best_score=new_score+0.00001
									thread(tn).sectime=timer-sectimer
									for i=1 to l
										thread(tn).sol(i)=alphabet(sol(i))
									next i
									thread(tn).ent=entropy
									thread(tn).effectivesymbols=tes
									thread(tn).multiplicity=tes/l
									
									'for i=1 to l
									'	sol_sr1(i)=sol(i)
									'next i
									'for i=1 to s
									'	for j=0 to pps(i)-1
									'		poly_letter_sr1(i,j)=poly_letter(i,j)
									'		poly_count_sr1(i,j)=poly_count(i,j)
									'	next j
									'next i
									
								end if
								
								if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
									solution_improved=0 'consume
									solution_timer=timer
									#include "solver_ioc.bi"
									#include "solver_advstats.bi"
									thread(tn).score=best_score
									#include "solver_output.bi"
								end if
								
								accept=1
								
							else
								
								new_ngram_score=old_ngram_score
								
								if rchange=0 then 'undo distribution
									sol(map1(rc0_symbol,rc0_number))=old_letter
									
									frq(new_letter)-=mape1(map1(rc0_symbol,rc0_number))
									frq(old_letter)+=mape1(map1(rc0_symbol,rc0_number))
									
									poly_count(rc0_symbol,rp)-=1
									poly_count(rc0_symbol,old_poly)+=1
									tes-=es(rc0_symbol)
									tes+=prev_es
									es(rc0_symbol)=prev_es
								else 'undo letter
									poly_letter(rc1_symbol,old_poly)=old_letter
									for i=1 to map1(rc1_symbol,0)
										if new_letter=sol(map1(rc1_symbol,i)) then
											sol(map1(rc1_symbol,i))=old_letter
											frq(new_letter)-=mape1(map1(rc1_symbol,i))
											frq(old_letter)+=mape1(map1(rc1_symbol,i))
										end if
									next i
								end if
								
								accept=0
								
							end if
							
							temp-=temp_min
							
							thread(tn).iterations_completed+=1
							if thread(tn).solver_stop=1 then exit for
							if pausetask=1 then do:sleep 10:loop until pausetask=0
							
						next it
						
					'next rr
					
				'next lr
				
				'for i=1 to l
				'	sol_sr2(i)=sol_sr1(i)
				'next i
				'for i=1 to s
				'	for j=0 to pps(i)-1
				'		poly_letter_sr2(i,j)=poly_letter_sr1(i,j)
				'		poly_count_sr2(i,j)=poly_count_sr1(i,j)
				'	next j
				'next i
				'
				'curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
				
			'next lv
			
			if thread(tn).solver_stop=0 then
				
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
			
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub

sub bhdecrypt_sparsepoly_567810g(byval tn_ptr as any ptr)
	
	#include "solver_variables.bi"
	
	dim as integer solver_output=3
	dim as long frq(constfrq)
	dim as short frq2(constfrq)
	dim as short nba(constcip)
	dim as short sol(constcip)
	dim as short key1(constcip)
	dim as short key2(constcip)
	dim as short mpp2(constcip)
	dim as short mape1(constcip)
	dim as ubyte ngrams(constcip)
	dim as ubyte stlp(constcip)
	dim as short frc(constcip)
	dim as double enttable(constent)
	dim as byte sr(10),lnb(0)
	
	do 'wait for input
	
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 8:tempdiv=2
				case else:tempdiv=2
			end select
			
			#include "solver_settings.bi"
			
			m=s/l
			ls=l/s
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			for i=1 to l*ngram_size
				enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
			next i
			
			frcmax=0
			for i=1 to l
				nba(i)=thread(tn).cip(i)
				frc(nba(i))+=1
				if frc(nba(i))>frcmax then frcmax=frc(nba(i))
			next i
			
			dim as short map1(s,frcmax)
			dim as short map2s(l,abc_sizem1)
			dim as uinteger fm3(abc_sizem1,abc_sizem1,abc_sizem1)
			
			for i=1 to s
				map1(i,0)=0
			next i
				
			for i=1 to l
				map1(nba(i),0)+=1
				map1(nba(i),map1(nba(i),0))=i
				mape1(i)=0
			next i
			
			k=0
			for i=1 to l-(ngram_size-1) 'entropy reduction
				for j=0 to ngram_size-1
					mape1(i+j)+=1
				next j
				k+=mape1(i)
			next i
			ent_score_norm=(l*ngram_size)/k
			
			for i=0 to abc_sizem1
				for j=0 to abc_sizem1
					for k=0 to abc_sizem1
						fm3(i,j,k)=0
					next k
				next j
			next i
			
			cur_its=0
			start_temp=temp1*ls
			start_temp/=m_ioc2(nba(),l,s,2)^0.75
			curr_temp=start_temp
			ngf=ngramfactor*ent_score_norm
			ngf/=(1+((s/l)*multiplicityweight))
			ngfal=ngf/al
			best_score=0
			
			solution_timer=timer
			
			for lv=1 to lvmax
					
				for lr=1 to sr(lv)
					
					iterations=(iterations_total/sr(lv))/lvmax
				
					for rr=1 to random_restarts
						
						for i=1 to s
							for j=0 to abc_sizem1
								map2s(i,j)=0
							next j
						next i
						
						for i=0 to abc_sizem1
							frq(i)=0
						next i
						
						if lv=1 then
							for i=1 to l
								state=48271*state and 2147483647
								new_letter=abc_size*state shr 31
								sol(i)=new_letter
								frq(new_letter)+=mape1(i)
								map2s(nba(i),new_letter)+=1
							next i
						else	 
							for i=1 to l
								sol(i)=key2(i)
								frq(sol(i))+=mape1(i)
								map2s(nba(i),sol(i))+=1
							next i	
						end if
						
						entropy=0
						for i=0 to abc_sizem1
							entropy+=enttable(frq(i))
						next i
						
						match_int=0
						for i=1 to s
							mpp2(i)=0
							for j=0 to abc_sizem1
								if map2s(i,j)>mpp2(i) then mpp2(i)=map2s(i,j)
							next j
							match_int+=mpp2(i)
						next i
						
						new_ngram_score=0
						
						#include "solver_ngram_init.bi"
						
						'mc=5
						'mc_minus=(mc-1)/iterations
						
						old_score=0
						temp=curr_temp
						temp_min=temp/iterations
						
						for it=1 to iterations 'iterations_total
							
							state=48271*state and 2147483647
							cur_pos=1+l*state shr 31 'change random position		
							
							old_letter=sol(cur_pos)
							older_letter=stlp(cur_pos)
							
							'if lv=lvmax then mc-=mc_minus
							'state=48271*state and 2147483647
							'd=4*state shr 31
							
							'if d>mc then
							'	#include "solver_randomnewletter.bi"
							'else
							'	do
							'		state=48271*state and 2147483647
							'		k=ngram_size*state shr 31
							'		j=cur_pos-k
							'	loop until j<=al andalso j>0
							'	select case k
							'		case 0:new_letter=g5b(4,sol(j+1),sol(j+2),sol(j+3),sol(j+4))
							'		case 1:new_letter=g5b(3,sol(j),sol(j+2),sol(j+3),sol(j+4))
							'		case 2:new_letter=g5b(2,sol(j),sol(j+1),sol(j+3),sol(j+4))
							'		case 3:new_letter=g5b(1,sol(j),sol(j+1),sol(j+2),sol(j+4))
							'		case 4:new_letter=g5b(0,sol(j),sol(j+1),sol(j+2),sol(j+3))
							'	end select
							'	if new_letter=old_letter or new_letter=abc_size then
							'		#include "solver_randomnewletter.bi"
							'	end if
							'end if
							
							state=48271*state and 2147483647
							new_letter=abc_sizem1*state shr 31
							if new_letter=old_letter then new_letter=abc_sizem1
							state=48271*state and 2147483647
							nl2=abc_sizem1*state shr 31
							if nl2=old_letter then nl2=abc_sizem1
							new_letter=iif(fm3(older_letter,old_letter,new_letter)>=fm3(older_letter,old_letter,nl2),new_letter,nl2)
							
							sol(cur_pos)=new_letter
							
							old_entropy=entropy
							entropy+=enttable(frq(old_letter)-mape1(cur_pos))-enttable(frq(old_letter))
							entropy+=enttable(frq(new_letter)+mape1(cur_pos))-enttable(frq(new_letter))
							
							i=nba(cur_pos)
							map2s(i,old_letter)-=1
							map2s(i,new_letter)+=1
							old_match_int=match_int
							old_mpp2=mpp2(i)
							match_int-=old_mpp2
							
							mpp2(i)=0
							for j=0 to abc_sizem1
								if map2s(i,j)>mpp2(i) then mpp2(i)=map2s(i,j)
							next j
							match_int+=mpp2(i)
							
							old_ngram_score=new_ngram_score
							
							select case ngram_size
								case 5
									for i=0 to 4
										j=cur_pos-i
										if j<=al andalso j>0 then
											new_ngram_score+=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))-ngrams(j)
										end if
									next i
								case 6
									for i=0 to 5
										j=cur_pos-i
										if j<=al andalso j>0 then
											new_ngram_score+=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))-ngrams(j)
										end if
									next i
								case 7
									for i=0 to 6
										j=cur_pos-i
										if j<=al andalso j>0 then
											new_ngram_score+=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))-ngrams(j)
										end if
									next i
								case 8
									for i=0 to 7
										j=cur_pos-i
										if j<=al andalso j>0 then
											z=0
											z1=bh4(sol(j),sol(j+1),sol(j+2),sol(j+3))
											if z1<>0 then
												z2=bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7))
												if z2<>0 then z=bh8(z1,z2)
											end if
											new_ngram_score+=z-ngrams(j)
										end if
									next i
								'case 10
								'	for i=0 to 9
								'		j=cur_pos-i
								'		if j<=al andalso j>0 then
								'			z=0
								'			z1=bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
								'			if z1<>0 then
								'				z2=bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9))
								'				if z2<>0 then z=bh10(z1,z2)
								'			end if
								'			new_ngram_score+=z-ngrams(j)
								'		end if
								'	next i
							end select
							
							select case solvesub_fastent
								case 0:new_score=new_ngram_score*ngfal*fastpow1_single(entropy,entweight)*((match_int/l)^solvesub_matchweight)
								case 1:new_score=new_ngram_score*ngfal*(entropy^0.25)*((match_int/l)^solvesub_matchweight)
								case 2:new_score=new_ngram_score*ngfal*(entropy^0.5)*((match_int/l)^solvesub_matchweight)
								case 3:new_score=new_ngram_score*ngfal*(entropy^0.75)*((match_int/l)^solvesub_matchweight)
								case 4:new_score=new_ngram_score*ngfal*entropy*((match_int/l)^solvesub_matchweight)
								case 5:new_score=new_ngram_score*ngfal*(entropy^1.5)*((match_int/l)^solvesub_matchweight)
								case 6:new_score=new_ngram_score*ngfal*entropy*entropy*((match_int/l)^solvesub_matchweight)
							end select
							
							if new_score>old_score then
								
								fm3(older_letter,old_letter,new_letter)+=1
								frq(old_letter)-=mape1(cur_pos) 
								frq(new_letter)+=mape1(cur_pos)						
								old_score=new_score
								stlp(cur_pos)=old_letter
								
								select case ngram_size
									case 5
										for i=0 to 4
											j=cur_pos-i
											if j<=al andalso j>0 then
												ngrams(j)=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
											end if
										next i
									case 6
										for i=0 to 5
											j=cur_pos-i
											if j<=al andalso j>0 then
												ngrams(j)=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))
											end if
										next i
									case 7
										for i=0 to 6
											j=cur_pos-i
											if j<=al andalso j>0 then
												ngrams(j)=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))
											end if
										next i
									case 8
										for i=0 to 7
											j=cur_pos-i
											if j<=al andalso j>0 then
												ngrams(j)=bh8(bh4(sol(j),sol(j+1),sol(j+2),sol(j+3)),bh4(sol(j+4),sol(j+5),sol(j+6),sol(j+7)))
											end if
										next i
									'case 10
									'	for i=0 to 9
									'		j=cur_pos-i
									'		if j<=al andalso j>0 then
									'			ngrams(j)=bh10(bh5(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4)),bh5(sol(j+5),sol(j+6),sol(j+7),sol(j+8),sol(j+9)))
									'		end if
									'	next i
								end select
								
								if new_score>best_score then															
									
									solution_improved=1
									thread(tn).sectime=timer-sectimer
									for i=1 to l
										key1(i)=sol(i)
										thread(tn).sol(i)=alphabet(sol(i))
									next i
									thread(tn).ent=entropy
									thread(tn).match=match_int/l
									thread(tn).multiplicity=s/l
									best_score=new_score+0.00001
									
								end if
								
								if solution_improved=1 andalso timer-solution_timer>solvesub_solutionreleasetimer then
									solution_improved=0 'consume
									solution_timer=timer
									#include "solver_ioc.bi"
									#include "solver_advstats.bi"
									thread(tn).score=best_score
									#include "solver_output.bi"
								end if
								
							else
								
								i=nba(cur_pos)
								mpp2(i)=old_mpp2
								map2s(i,old_letter)+=1
								map2s(i,new_letter)-=1
								match_int=old_match_int
								sol(cur_pos)=old_letter
								entropy=old_entropy
								new_ngram_score=old_ngram_score
								old_score-=temp*(1/l)*(old_score/new_score)
								
							end if
							
							temp-=temp_min
							
							thread(tn).iterations_completed+=1
							if thread(tn).solver_stop=1 then exit for,for,for,for
							if pausetask=1 then do:sleep 10:loop until pausetask=0
						
						next it
					
					next rr
					
				next lr
				
				for i=1 to l
					key2(i)=key1(i)
				next i
				
				curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
				
			next lv
			
			if thread(tn).solver_stop=0 then
				
				if solution_improved=1 then
					solution_improved=0
					solution_timer=timer
					#include "solver_ioc.bi"
					#include "solver_advstats.bi"
					thread(tn).score=best_score
					#include "solver_output.bi"
				end if
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub

sub bhdecrypt_vigenere_34567810g(byval tn_ptr as any ptr)
	
	dim as integer solver_output=7 'id
	
	#include "solver_variables.bi"
		
	dim as long frq1(constfrq)
	dim as long frq2(constfrq)
	dim as long frq3(constfrq)
	dim as ubyte vkey(constcip)
	dim as ubyte sol(constcip)
	dim as ubyte stl(constcip)
	dim as ubyte key1(constcip)
	dim as ubyte key2(constcip)
	dim as short nba(constcip)
	dim as ubyte ngrams(constcip)
	dim as short mape1(constcip)
	dim as short mape2(constcip)
	dim as short frc(constcip)
	dim as double enttable(constent)
	dim as byte sr(10),lnb(0)
	dim as byte cribkey(0)
	
	dim as short maps(constcip)
	dim as short maps2(constcip)
	dim as integer mi,mj
	
	do 'wait for input
	
		sleep twait
		
		if thread(tn).solver_waiting=0 then
			
			seed+=threads
			if (seed*2)-1>2147483647 then seed=tn
			state=(seed*2)-1
			
			lvmax=solvesub_subrestartlevels
			for i=1 to lvmax
				sr(i)=solvesub_subr(i)
			next i
			
			select case ngram_size
				case 2,3,4,5:tempdiv=3
				case 6:tempdiv=2.75
				case 7:tempdiv=2.5
				case 8:tempdiv=2.25
				case 10:tempdiv=2
			end select
			
			#include "solver_settings.bi"
			
			m=s/l
			ls=l/s
			ll=l*(l-1)
			al=l-(ngram_size-1)
			
			for i=1 to l*ngram_size
				enttable(i)=abs(logbx(i/(l*ngram_size),2)*(i/(l*ngram_size)))
			next i
			
			frcmax=0
			for i=1 to l
				nba(i)=thread(tn).cip(i)
				frc(nba(i))+=1
				if frc(nba(i))>frcmax then frcmax=frc(nba(i))
				mape1(i)=0
			next i
			
			dim as short map1(s,frcmax)
			dim as short map2(s,frcmax*ngram_size)
			dim as byte vadd(abc_sizem1,abc_sizem1)
			dim as byte vsub(abc_sizem1,abc_sizem1)
			
			for i=1 to s
				map1(i,0)=0
				map2(i,0)=0
				maps(i)=i
				mape2(i)=0
			next i
			
			for i=1 to l-(ngram_size-1) 'entropy reduction
				for j=0 to ngram_size-1
					mape1(i+j)+=1
				next j
			next i
			j=0
			for i=1 to l
				mape2(nba(i))+=mape1(i)
				j+=mape1(i)
			next i
			ent_score_norm=(l*ngram_size)/j
			
			for i=0 to abc_sizem1
				for j=0 to abc_sizem1
					vadd(i,j)=(i+j) mod abc_size
					vsub(i,j)=i-j
					if vsub(i,j)<0 then vsub(i,j)=abc_size+vsub(i,j)
				next j
			next i
			
			for i=1 to l
				vkey(i)=thread(tn).key(i)
				map1(nba(i),0)+=1
				map1(nba(i),map1(nba(i),0))=i
			next i
			
			for i=1 to l
				for j=0 to ngram_size-1
					if i-j>0 andalso i-j<l-(ngram_size-2) then
						e=0
						for k=1 to map2(nba(i),0)
							if map2(nba(i),k)=i-j then
								e=1
								exit for
							end if
						next k
						if e=0 then 
							map2(nba(i),0)+=1
							map2(nba(i),map2(nba(i),0))=i-j
						end if
					end if
				next j
			next i
			
			'temp1=350 'overwrite temp
			temp1/=1.75
			start_temp=(temp1*ls)
			start_temp/=m_ioc2(nba(),l,s,2)^0.75
			curr_temp=start_temp
			ngf=(ngramfactor/4.6)*ent_score_norm
			ngf/=(1+((s/l)*multiplicityweight))
			ngfal=ngf/al
			best_score=0
			
			for lv=1 to lvmax
					
				for lr=1 to sr(lv)
						
					iterations=(iterations_total/sr(lv))/lvmax
			
					for rr=1 to random_restarts
						
						erase frq1,frq2
						
						if lv=1 then
							for i=1 to s								
								state=48271*state and 2147483647
								new_letter=abc_size*state shr 31
								stl(i)=new_letter
								frq2(new_letter)+=mape2(i)
								for j=1 to map1(i,0)
									if subtract=0 then
										vig_letter=vadd(new_letter,vkey(map1(i,j)))
									else
										vig_letter=vsub(new_letter,vkey(map1(i,j)))
									end if
									sol(map1(i,j))=vig_letter
									frq1(vig_letter)+=mape1(map1(i,j))
								next j
							next i
						else
							for i=1 to s
								stl(i)=key2(i)
								for j=1 to map1(i,0)
									if subtract=0 then
										vig_letter=vadd(key2(i),vkey(map1(i,j)))
									else
										vig_letter=vsub(key2(i),vkey(map1(i,j)))
									end if
									sol(map1(i,j))=vig_letter
									frq1(vig_letter)+=mape1(map1(i,j))
									frq2(key2(i))+=mape1(map1(i,j))
								next j
							next i
						end if
						
						mi=0
						mj=0
						#include "solver_picksymbol.bi"
						
						old_score=0
						temp=curr_temp
						temp_min=temp/iterations
						
						new_ngram_score=0
						
						#include "solver_ngram_init.bi"
						
						for it=1 to iterations
							
							old_letter=stl(curr_symbol)
						
							state=48271*state and 2147483647
							new_letter=abc_sizem1*state shr 31
							if new_letter=old_letter then new_letter=abc_sizem1
							
							frq2(old_letter)-=mape2(curr_symbol)
							frq2(new_letter)+=mape2(curr_symbol)
							
							for i=1 to map1(curr_symbol,0)
								frq1(sol(map1(curr_symbol,i)))-=mape1(map1(curr_symbol,i))
								if subtract=0 then
									vig_letter=vadd(new_letter,vkey(map1(curr_symbol,i)))
								else
									vig_letter=vsub(new_letter,vkey(map1(curr_symbol,i)))
								end if
								sol(map1(curr_symbol,i))=vig_letter
								frq1(vig_letter)+=mape1(map1(curr_symbol,i))
							next i
							
							entropy1=0
							entropy2=0
							for i=0 to abc_sizem1
								entropy1+=enttable(frq1(i))
								entropy2+=enttable(frq2(i))
							next i
							
							old_ngram_score=new_ngram_score
							
							#include "solver_ngram_main.bi"
							
							select case solvesub_fastent
								case 0:new_score=new_ngram_score*ngfal*fastpow1_single(entropy1,entweight)*entropy2
								case 1:new_score=new_ngram_score*ngfal*(entropy1^0.25)*entropy2
								case 2:new_score=new_ngram_score*ngfal*(entropy1^0.5)*entropy2
								case 3:new_score=new_ngram_score*ngfal*(entropy1^0.75)*entropy2
								case 4:new_score=new_ngram_score*ngfal*entropy1*entropy2
								case 5:new_score=new_ngram_score*ngfal*(entropy1^1.5)*entropy2
								case 6:new_score=new_ngram_score*ngfal*entropy1*entropy1*entropy2
							end select
							
							if new_score>old_score then
							
								stl(curr_symbol)=new_letter
								
								#include "solver_ngram_tail.bi"
								
								old_score=new_score
								
								#include "solver_picksymbol.bi"
								
								if new_score>best_score then
									
									thread(tn).sectime=timer-sectimer
									erase frq3
									for i=1 to l
										thread(tn).sol(i)=alphabet(sol(i))
										frq3(sol(i))+=1
									next i
									ioc_int=0
									for i=0 to abc_sizem1
										ioc_int+=ioctable(frq3(i))
									next i
									thread(tn).ioc=ioc_int/ll
									thread(tn).ent=entropy1
									thread(tn).multiplicity=s/l
									'thread(tn).ioc2=ioc_int2/ll
									best_score=new_score+0.00001
									thread(tn).score=best_score
									for i=1 to s
										key1(i)=sol(map1(i,1))
									next i
									
								end if
								
							else
								
								new_ngram_score=old_ngram_score
								
								frq2(new_letter)-=mape2(curr_symbol)
								frq2(old_letter)+=mape2(curr_symbol)
								
								for i=1 to map1(curr_symbol,0)
									frq1(sol(map1(curr_symbol,i)))-=mape1(map1(curr_symbol,i))
									if subtract=0 then
										vig_letter=vadd(old_letter,vkey(map1(curr_symbol,i)))
									else
										vig_letter=vsub(old_letter,vkey(map1(curr_symbol,i)))
									end if
									sol(map1(curr_symbol,i))=vig_letter
									frq1(vig_letter)+=mape1(map1(curr_symbol,i))
								next i
								
								#include "solver_picksymbol.bi"
								
								old_score-=temp*(map1(curr_symbol,0)+1)/l
							
							end if
							
							temp-=temp_min
							
							thread(tn).iterations_completed+=1
							if thread(tn).solver_stop=1 then exit for,for,for,for
							if pausetask=1 then do:sleep 10:loop until pausetask=0
							
						next it
						
					next rr
				
				next lr
				
				for i=1 to s
					key2(i)=key1(i)
				next i
				
				curr_temp/=tempdiv/(1+(s/l)) 'curr_temp/=tempdiv
			
			next lv
			
			if thread(tn).solver_stop=0 then
				
				#include "solver_advstats.bi"
				#include "solver_output.bi"
				
				if thread(tn).combine_output=1 then combine_score(thread(tn).itemnumber)=best_score
				
				thread(tn).avgscore+=best_score
				thread(tn).avgioc+=thread(tn).ioc
				thread(tn).avgpccycles+=thread(tn).pccycles
				thread(tn).restarts_completed+=1
				
			end if
			
			thread(tn).solver_waiting=1
		
		end if
		
	loop until thread(tn).thread_stop=1
	
	thread(tn).thread_active=0
	thread(tn).thread_stop=0
	
end sub


#include "helper_functions.bi"