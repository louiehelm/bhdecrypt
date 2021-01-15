If m = ngram_size-1 Then
	For x = 0 To ngram_size-2
		If nba(j+x) = curr_symbol Then			 ' cheaky code to mangle m value if multiple copies of symbol before final char are in n-gram 
			m = 0								 ' so solver optimization isn't fooled inside inner solver fragments below
		endif
	Next x
EndIf

Select Case ngram_size
	Case 6
		for i=65 to 90
			For x = 0 To ngram_size-1
				If nba(j+x) = curr_symbol Then
					sol(j+x)=i
				EndIf
			Next x
			#include "solver_gov_6.bi"
			if z>bls then bls=z:new_letter=i
		Next i
	Case 7
		for i=65 to 90
			For x = 0 To ngram_size-1
				If nba(j+x) = curr_symbol Then
					sol(j+x)=i
				EndIf
			Next x
			#include "solver_gov_7.bi"
			if z>bls then bls=z:new_letter=i
		Next i
	Case 8
		for i=65 to 90
			For x = 0 To ngram_size-1
				If nba(j+x) = curr_symbol Then
					sol(j+x)=i
				EndIf
			Next x
			#include "solver_gov_8.bi"
			if z>bls then bls=z:new_letter=i
		Next i
	Case 9
		for i=65 to 90
			For x = 0 To ngram_size-1
				If nba(j+x) = curr_symbol Then
					sol(j+x)=i
				EndIf
			Next x
			#Include "solver_gov_9.bi"
			if z>bls then bls=z:new_letter=i
		Next i
	Case 10
		for i=65 to 90
			For x = 0 To ngram_size-1
				If nba(j+x) = curr_symbol Then
					sol(j+x)=i
				EndIf
			Next x
			#include "solver_gov_10.bi"
			if z>bls then bls=z:new_letter=i
		Next i
	Case Else ' 2-5
		for i=65 to 90
			For x = 0 To ngram_size-1
				If nba(j+x) = curr_symbol Then
					sol(j+x)=i
				EndIf
			Next x
			#include "solver_gov_small.bi"
			if z>bls then bls=z:new_letter=i
		Next i
End Select

'For x = 0 To ngram_size-1
'	If nba(j+x) = curr_symbol Then
'		sol(j+x)=old_letter
'	EndIf
'Next


' Think it's OK to leave test n-gram mangled since it will be overwritten
' immediately outside this call by newletter in all locations