" Txtfmt: Set of Vim plugins (syntax, ftplugin, plugin) for creating and
" displaying formatted text with Vim.
" File: This is the txtfmt ftplugin file, which contains mappings and
" functions for working with the txtfmt color/formatting tokens.
" Creation:	2004 Nov 06
" Last Change: 2008 Apr 6
" Maintainer:	Brett Pershing Stahlman <brettstahlman@comcast.net>
" License:	This file is placed in the public domain.

"echoerr "Sourcing ftplugin..."

" Let the common code know whether this is syntax file or ftplugin
let s:script_name = 'ftplugin'
" Function: s:Add_undo() <<<
" Purpose: Add the input string to the b:undo_ftplugin variable. (e.g., unmap
" commands, etc...)
" Return: none
" NOTE: This cannot be in common config, since it is called even when common
" config is being skipped for this script.
fu! s:Add_undo(s)
	if !exists('b:undo_ftplugin')
		let b:undo_ftplugin = ''
		let add_sep = 0
	elseif b:undo_ftplugin !~ '^\s*$'
		let add_sep = 1
	else
		let add_sep = 0
	endif
	let b:undo_ftplugin = b:undo_ftplugin.(add_sep!=0?'|':'').a:s
endfu
" >>>
" plugin load considerations <<<
" Important Note: I intentionally skip the standard check for
" exists("b:did_ftplugin") because I want to make it easy for txtfmt user to
" load txtfmt ftplugin after another ftplugin. The standard check would
" make this more difficult.
" NOTE: Vim bug? in ftplugin.vim will cause error if you set b:undo_ftplugin
" but not b:did_ftplugin.
" Yes. It is a bug. It's been fixed, but don't count on it, since we want it
" to work for version 6.3...
let b:did_ftplugin = 1
" Don't source the ftplugin again for same buffer, as it will cause errors due
" to <unique>. Note that this is important now that I've removed the check on
" b:did_ftplugin.
if exists("b:loaded_txtfmt")
	finish
endif
let b:loaded_txtfmt = 1
" Ensure that b:loaded_txtfmt is unlet whenever an ftplugin is about to be
" loaded.
" Note: Originally, I initialized b:undo_ftplugin to the empty string. The
" problem with doing so, however, is that it clobbers undo actions defined by
" a previously sourced ftplugin. Recall that txtfmt is designed to be used in
" combination with other filetypes (e.g., by using the dot-separated filetype
" name mechanism).
" Note: Don't unlet b:did_ftplugin, as standard ftplugin.vim does this
call s:Add_undo('unlet! b:loaded_txtfmt')
" >>>
" Set compatibility options <<<
" (set to Vim defaults to avoid errors with line continuation)
let s:save_cpo = &cpo
set cpo&vim
" >>>
" Common Configuration <<<
" Note: No point in having the modeline and/or global options processed by
" both the syntax and ftplugin files.
" IMPORTANT: Everything inside the "Common Configuration" fold should be
" identical between the syntax and ftplugin files. Keep in sync as changes are
" made...
if !exists('b:txtfmt_did_common_config')
	" Note: An unlet of b:txtfmt_did_common_config is intentionally NOT added
	" to b:undo_ftplugin. If it were unlet by b:undo_ftplugin, we would
	" generally do common configuration twice, since the standard setup will
	" source b:undo_ftplugin between the loading of syntax and filetype
	" plugins. In order for the mechanism to work properly,
	" b:txtfmt_did_common_config needs to be unlet before either syntax or
	" filetype plugin is loaded. There are currently several ways to get this
	" to happen: :e[dit], :b[delete], :Refresh
	let b:txtfmt_did_common_config = 1
	" Ensure that the code within plugin/txtfmt.vim will be executed when the
	" file is sourced.
	let b:txtfmt_do_common_config = 1
	" TODO - Should we ensure that warning occurs for missing file?
	runtime plugin/txtfmt.vim 
	" Make sure the common config doesn't run again
	unlet b:txtfmt_do_common_config

endif
" >>>
" Note on writing to b:undo_ftplugin <<<
" Regardless of who did the common config, it is ESSENTIAL that only the
" ftplugin write to b:undo_ftplugin, as ftplugin.vim function LoadFTPlugin()
" can run between sourcing of syntax file and sourcing of ftplugin file, and
" when it does, if b:undo_ftplugin exists, it will attempt to unlet both it
" and did_ftplugin. Problem is two-fold. Whatever syntax file has written to
" undo_ftplugin is lost, and worse, did_ftplugin may not exist at that point.
" If it does not, the "unlet" without the ! will generate error! Note: Bram
" said he would fix this in distributed version, but everyone will still have
" the old for awhile...
" >>>
" Script-local functions <<<
" Function: s:SID() <<<
" Purpose: Returns the SID number of this script. (help <SNR>)
" Note: This function is taken directly from the Vim help.
fu! s:SID()
  return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfu
" >>>
" Function: s:Prep_for_single_quotes() <<<
" Description: Double all single-quotes in the input string to prepare it for
" insertion within single quotes in a string that will be exec'ed. (:help
" literal-string)
fu! s:Prep_for_single_quotes(s)
	return substitute(a:s, "'", "''", 'g')
endfu
" >>>
" Function: s:Move_cursor() <<<
" Purpose: Move the cursor right or left (within the current line) by the
" specified number of characters positions. Note that character positions are
" not always the same as screen columns, and are certainly not always the same
" as byte positions.
" IMPORTANT NOTE: This function was designed to effect the cursor position
" offset specified by the '.' in a fmt/clr spec. Because of the way the
" offsets are calculated in the caller (Insert_tokstr), the raw offset may
" need to be adjusted by one character due to (for example) the impossibility
" of positioning cursor before BOL. The logic for doing this should be
" multi-byte aware. Although such logic could be implemented in this
" function's caller, there is no reason to have it in both functions. The
" logic in Insert_tokstr may be simplified if this function is made simply to
" move as far as possible on the current line in the specified direction, in
" cases in which the offset as specified is too large.
" Note: As a general rule, character positions correspond to screen columns;
" in some cases, however, a single character may occupy multiple screen
" columns (e.g. a tab). Also, there may be cases in which multiple characters
" occupy a single screen column (e.g. a base character followed by combining
" characters).
" Method: Begin moving in the specified direction a single byte at a time.
" Each time a byte position change results in a screen position change of *at
" least* one column, consider that we have moved by exactly one character.
" This strategy ensures that a tab will be considered a single character
" (unless 'virtualedit' is set), as will the combination of a base character
" and its associated combining characters. (Note, however, that such special
" cases should not generally be an issue because this function was designed to
" handle offsets in sequences of txtfmt tokens, which should not include tabs
" or combining characters.)
" Inputs:
" off			- Absolute value specifies number of character positions to
" 				move. Sign specifies the direction:
" 				pos. ==> right, neg. ==> left
" Return: The difference between the number of characters positions we were
" requested to move and the number of character positions we actually moved;
" i.e., zero indicates no problem executing the requested move.
" Error: Not possible. Since this function is used only internally,
" questionable inputs are silently converted to safe ones.
fu! s:Move_cursor(off)
	" Convert the signed offset that was input to a direction and magnitude.
	if a:off < 0
		let off = -a:off
		let inc = -1
	else
		let off = a:off
		let inc = 1
	endif
	" Are we moving rightward?
	if inc == 1
		" Determine the current mode, which determines the right-most valid
		" position on the line.
		let mode = mode()
		" Determine the last byte on which cursor may be positioned.
		" (In insert mode, cursor may be positioned just after the last byte.)
		let last_col = mode =~ 'i' ? virtcol('$') : virtcol('$') - 1
		"echomsg 'mode='.mode.' last_col='.last_col
	endif
	" Determine starting byte and screen column location.
	let col = col('.')
	let vcol_prev = virtcol('.')
	" Keep track of how many character positions we've moved.
	let moved = 0
	" Loop until we reach desired location
	while moved < off
		" Determine next byte position. (Next byte may or may not belong to a
		" different character.)
		let col = col + inc
		" Make sure we're not about to move too far.
		if inc == 1
			" Moving rightward
			if col > last_col
				" Can't move any further right!
				break
			endif
		else
			" Moving leftward
			if col < 1
				" Can't move any further left!
				break
			endif
		endif
		" Adjust cursor pos to new byte position
		call cursor(0, col)
		" Determine new virtual col
		let vcol = virtcol('.')
		" Has screen position changed since last time through?
		if vcol_prev != vcol
			let moved = moved + 1
		endif
		" Save the virtual column for next time
		let vcol_prev = vcol
	endwhile
	" Return the number of character positions *not* moved (usually 0)
	return off - moved
endfu
" >>>
" Function: s:Insert_tokstr() <<<
" Called_from: 'insert' or 'normal' mode mapping
" Purpose: Insert the fmt/clr token sequence specified by tokstr at the cursor
" location. Tokstr should be one of the following:
" -a literal (already translated) token sequence with offset prepended (just
"  as it would have been returned by s:Translate_fmt_clr_spec)
" -an untranslated fmt/clr spec.
" -empty string (in which case, user will be prompted to enter a fmt/clr spec)
" After any required translations, this function ensures that the token
" sequence is inserted into the buffer (at a location determined by 'cmd'
" argument). It accomplishes the token insertion in one of two ways, depending
" upon current mode:
" 'insert': Returns the token sequence to be inserted (since the assumption is
"           that this function has been invoked from an expression register
"           (<C-R>=)).
" 'normal': Uses the specified 'cmd' in conjunction with normal!.
" Originally, this function was also responsible for moving the cursor
" to the inter-token position indicated by the (optional) dot replacing one of
" the commas in the fmt/clr spec. This could be done only because all
" insertions (including ones from 'insert' mode) were accomplished within this
" function using normal! That implementation, however, (specifically, the
" use of normal! from within an insert-mode mapping) caused problems with
" respect to an active count applied to the preceding enter-insert command
" (the one that started the insert from which mapping was invoked). Thus, the
" cursor adjustment functionality has been moved to a separate function
" (s:Adjust_cursor), invoked from the mapping after this function has
" returned. Since the offset governing cursor adjustment is determined within
" this function, we use static variables s:Adjust_cursor_inv_off and
" s:Adjust_cursor_modestr to communicate the information to s:Adjust_cursor.
" Inputs:
" tokstr		- raw (un-translated) fmt/clr spec to insert. If empty, user
"				will be prompted for fmt/clr spec list.
" cmd			- [iIaAoOs] - Vim command used (conceptually, at least) to
" 				insert the token string. Note that in some cases, the current
" 				mode will determine how the tokens are actually inserted.
" literal		- nonzero if the input tokstr comprises an offset followed by
" 				the literal fmt/clr tokens; i.e., if it needs no translation.
" 				(This might be the case if this routine is called from a
" 				user-defined mapping.)
" 				Note: When this flag is set, the input tokstr should be in the
" 				form returned by s:Translate_fmt_clr_spec.
" end_in_norm	- nonzero if it is desired that we end up in normal mode after
" 				map completion. (Most 'insert-token' commands have an
" 				alternate form that causes this flag to be set)
" v:count1		If greater than 1 and mode is normal, indicates the count to
" 				be used with the normal mode command used to insert the
" 				tokens.
" 				Note: Used only when a:1 is not supplied.
" a:1			If supplied, represents the count to be used with the normal
" 				mode command that inserts the tokens. (Needed when this
" 				function is called from a user-map)
" Return: Depends upon mode from which we are called:
" 'insert': Returns the inserted string, since we have been called from an
" expression register (<C-R>=).
" 'normal': Returns an empty string.
" Error: Use echoerr with meaningful error message, as this function is
" generally invoked directly from mapping.
" Assumptions: Cursor position and mode unchanged from when map was invoked
" Vim trivia: col('.') and col('$') return 1 for empty line
fu! s:Insert_tokstr(tokstr, cmd, literal, end_in_norm, ...)
	" Declare modifiable version of input parameter
	let tokstr = a:tokstr
	" Ensure that if we return without inserting any tokens (i.e. because user
	" canceled the operation), s:Adjust_cursor will not attempt to do
	" anything.
	unlet! s:Adjust_cursor_inv_off
	unlet! s:Adjust_cursor_modestr
	" Assumption: If a:literal is true, input tokstr has already been
	" translated and validated by s:Translate_fmt_clr_spec, and in fact, is
	" exactly the string returned by that function; hence, validation will not
	" be performed here.
	if !a:literal
		if tokstr == ''
			" Prompt user for special fmt/clr spec string
			let tokstr = s:Prompt_fmt_clr_spec()
			" Strip surrounding whitespace, which is ignored.
			" Note: Only surrounding whitespace is ignored! Whitespace not
			" permitted within fmt/clr spec list.
			let tokstr = substitute(tokstr, '^\s*\(.\{-}\)\s*$', '\1', 'g')
			" Check for Cancel request
			if tokstr == ''
				" Note that nothing about position or mode has changed at this point
				return ''
			endif
		endif
		" Translate and validate fmt/clr spec
		let tokstr = s:Translate_fmt_clr_list(tokstr)
		if tokstr == ''
			" Invalid fmt/clr sequence
			echoerr "Insert_tokstr(): Invalid fmt/clr sequence entered: ".s:err_str
			" Note that nothing about position or mode has changed at this point
			return ''
		endif
	endif
	" At this point, we have a translated fmt/clr spec comprising an offset
	" followed by the actual fmt/clr token sequence. Extract the pieces from
	" the string (internally generated - no need for validation)
	let offset = substitute(tokstr, '\(\-\?[[:digit:]]\+\),\(.*\)', '\1', '')
	let tokstr = substitute(tokstr, '\(\-\?[[:digit:]]\+\),\(.*\)', '\2', '')
	" If user didn't specify offset, default is past inserted chars
	if offset < 0
		" Get length of tokstr, noting that it may contain multi-byte
		" tokens.
		let offset = strlen(substitute(tokstr, '.', 'x', 'g'))
	endif

	" Validate the command
	" Get cmd string in standard form (strip surrounding whitespace)
	" TODO - Perhaps this isn't necessary, since cmd is generated internally.
	let cmd = substitute(a:cmd, '^\s*\(.\{-}\)\s*$', '\1', '')
	if cmd !~ '^[iIaAoOs]$'
		echoerr 'Insert_tokstr(): Invalid insert token cmd string: '.cmd
		return ''
	endif
	" Validate current mode
	let modestr = mode()
	if modestr !~ '^[ni]$'
		echoerr "Insert_tokstr(): May be called only from 'normal' and 'insert' modes."
		return ''
	endif
	" Validation Complete!
	" Build start/end mode string for convenience in testing
	let modestr = modestr.(a:end_in_norm ? 'n' : 'i')
	" Calculate offset from end of tokstr (inverse offset)
	" a b c d e f
	"0 1 2 3 4 5 6	: offset
	"6 5 4 3 2 1 0	: inv_off
	" We'll need to know number of *characters* (not bytes) in tokstr
	let tokstrlen = strlen(substitute(tokstr, '.', 'x', 'g'))
	let inv_off = tokstrlen - offset
	" Make modestr and inv_off available for s:Adjust_cursor, which should run
	" immediately after this function returns.
	let s:Adjust_cursor_inv_off = inv_off
	let s:Adjust_cursor_modestr = modestr
	" Note on cursor positioning: <<<
	" -The normal! insert commands will result in cursor being positioned 'ON'
	"  the last char inserted, regardless of which mode we start in. (For
	"  commands ending in insert mode, this means insert position just before
	"  the last char inserted.)
	"  TODO - Explain why - help on normal explains how the incomplete
	"  command (in this case enter-insert) will be terminated with <Esc>.
	" -The 'stopinsert' used when starting in insert and ending in normal mode
	"  will cause cursor to move back one, if we were in insert mode when
	"  mapping was invoked.
	" -undo undoes everything in a 'normal' command as a unit, so we put the
	"  entire command for changing text into a single normal!. Note that since
	"  cursor movement commands are not part of undo mechanism, any necessary
	"  cursor adjustments may be made after the text change.
	" -When starting in normal and ending in insert mode, there are 2 cases:
	"  1) Last char inserted is end of line, so we must use startinsert! to
	" start insert and end up with cursor after last inserted char. (Can't
	" position cursor beyond it in normal mode.)
	"  2) Last char inserted is not at end of line. Could move right one, then
	"  use 'startinsert'.
	" >>>
	if modestr[0] == 'n'
		" Insert the tokens with the appropriate enter-insert command and
		" count. Originally, I thought that doing it this way, rather than
		" using explicit cursor() and setline() calls, made the insertions
		" repeatable with repeat command (.); unfortunately, however, the
		" repeat command works only for normal mode commands entered ON
		" COMMAND LINE! While I could go back to using setline() and cursor(),
		" the logic for simulating a particular type of enter insert command
		" ([iIaAoOs]) with an optional count argument is a bit simpler this
		" way.
		" Determine the effective count (either from optional input or
		" v:count1).
		if a:0 > 0
			" Note: Counts are generated internally; hence, validation has
			" been performed already.
			let l:count = a:1
		else
			let l:count = v:count1
		endif
		" IMPORTANT NOTE: <C-R><C-R>= works differently in actual normal mode
		" from the way it works when used in a normal command. Due to what I
		" would call a bug, but one which Bram has no intention of fixing due
		" to the complexity of the various segments of code that process the
		" strings, you cannot embed character nr2char(128) in a string literal
		" used in an expression register. This character is used in the gui to
		" introduce a special sequence, which results in termcap expansion of
		" some sort. The point is, since character 128 could be used as txtfmt
		" token, need another way to insert a string that could contain it.
		" Solution: use the actual string variable, rather than a string
		" literal. This is actually more intuitive anyways - I just wasn't
		" sure what the scope requirements were for variables used in
		" expression register - it works.
		exe 'normal! '.l:count.a:cmd."\<C-R>\<C-R>=l:tokstr\<CR>"
		"IMPORTANT: Return empty string so that default return of 0 is not
		"inserted by the "<C-R>=" used to invoke this function from insert
		"mode !
		return ''
	else
		" Return the token string to be inserted via expression register
		" (<C-R>=) in insert-mode mapping.
		return l:tokstr
	endif
endfu
" >>>
" Function: s:Adjust_cursor() <<<
" Purpose:
" Method:
" Inputs: (indirect, via script-local vars)
" s:Adjust_cursor_inv_off
" s:Adjust_cursor_modestr
" Return: Empty string (to permit the function to be called from an expression
" register)
" Error: Not possible. Since this function is used only internally,
" questionable inputs are silently converted to safe ones.
" Note: There is at least one scenario under which we must return empty string
" immediately (i.e., without performing any adjustment): it is the case in
" which user has canceled a token insertion. In this case, neither of the
" script-local input vars will exist.
fu! s:Adjust_cursor()
	if !exists('s:Adjust_cursor_modestr') || !exists('s:Adjust_cursor_inv_off')
		" It appears no adjustment is required
		return ''
	endif
	if s:Adjust_cursor_modestr == 'nn'
		" Cursor is on last inserted char now. Special care must be taken
		" to ensure that we don't attempt to set cursor before beginning of
		" line (inv_off == tokstrlen and first char inserted is first char on
		" line). Note that this insert type works as though the chars had been
		" inserted, offset implemented, then insert mode exited.
		" Extreme points:
		" inv_off==0			--> Position on last char inserted
		" inv_off==tokstrlen	--> Position before first char or on first
		"                           char if beginning of line.
		" Adjust cursor in multi-byte safe manner
		" Note: I am intentionally letting Move_cursor handle the special
		" case of cursor positioned at beginning of line. Move_cursor is
		" multi-byte safe and will not attempt to position the cursor before
		" the beginning of the line, even when inv_off requests it.
		call s:Move_cursor(-s:Adjust_cursor_inv_off)
	elseif s:Adjust_cursor_modestr == 'ni'
		" Cursor is on last inserted char now, but with an inv_off of 0,
		" needs to end up 1 col position right of last inserted char after
		" entering insert mode. There are 2 cases to consider...
		" *** Case 1 ***
		" New cursor position is past the end of the line.
		" In this case, we cannot use Move_cursor to accomplish the shift
		" because we are currently in normal mode, which precludes the setting
		" of cursor position past EOL. (Note: Calling startinsert prior to
		" Move_cursor doesn't work since, according to the Vim docs, "when
		" using this command in a function or script, the insertion only
		" starts after the function or script is finished.") Fortunately, we
		" can call startinsert! to enter insert mode and position the cursor
		" past the end of the line.
		" *** Case 2 ***
		" New cursor position is *NOT* past the end of the line.
		" Accomplish the required right shift simply by adjusting the value of
		" inv_off passed to Move_cursor. There is no way for Move_cursor
		" to fail to reach requested position in this case, since even in the
		" extreme case (inv_off == tokstrlen and tokens inserted at beginning
		" of line), the offset of 1 ensures we won't request a position before
		" BOL.
		if s:Move_cursor(-s:Adjust_cursor_inv_off + 1)
			" Move_cursor was unable to move past EOL.
			startinsert!
		else
			startinsert
		endif
	elseif s:Adjust_cursor_modestr == 'in'
		" Cursor is at col number of last inserted char + 1, which is where it
		" needs to be for inv_off==0. Stopinsert will shift it 1 char left.
		" Note that if inv_off==tokstrlen, cursor will end up to left of
		" inserted chars unless this would put it prior to beginning of line.
		call s:Move_cursor(-s:Adjust_cursor_inv_off)
		exe 'stopinsert'
	elseif s:Adjust_cursor_modestr == 'ii'
		" Cursor is at col number of last inserted char + 1, which is where it
		" needs to be for inv_off==0.
		" one beyond it (for inv_off==0). Note that since we're staying in
		" insert mode, positions before and after inserted chars are legal,
		" even when inserted char(s) are at beginning or end of line.
		call s:Move_cursor(-s:Adjust_cursor_inv_off)
	endif
	return ''
endfu
" >>>
" Function: s:Prompt_fmt_clr_spec() <<<
" Purpose: Prompt user for type of formatting region desired, and return
" the string entered
" How: The user will be prompted to enter a fmt/clr list, consisting of
" fmt/clr atoms separated by commas and/or dots. The format of a fmt/clr atom
" is described in header of Translate_fmt_clr_spec().,
" Return: The entered string
fu! s:Prompt_fmt_clr_spec()
	" Prompt the user for fmt/clr spec string
	call inputsave()
	let str = input('Enter a fmt / clr string. (Enter to cancel): ')
	call inputrestore()
	return str
endfu
" >>>
" Function: s:Lookup_clr_namepat() <<<
" Purpose: Convert the input color name pattern to a color index in range
" 0..7, using the buffer-specific color definition array b:txtfmt_clr_namepat.
" Return: Color index between 0 and 7 inclusive, or -1 if error.
fu! s:Lookup_clr_namepat(namepat)
	" Loop over colors
	let i = 0
	while i < b:txtfmt_num_colors - 1
		if a:namepat =~ b:txtfmt_clr_namepat{i}
			return i
		endif
		let i = i + 1
	endwhile
	" Didn't find it!
	return -1
endfu
" >>>
" Function: s:Translate_fmt_clr_spec() <<<
" Purpose: Convert the input fmt/clr spec string to the corresponding fmt/clr
" token.
" How: The input fmt/clr spec string will be in one of the following formats:
" "f-"
" "c-"
" "f[u][b][i][s][r][c]" if txtfmt 'formats' is set to 'all'
" --or--
" "f[u][b][i][s][r]   " if txtfmt 'formats' is set to 'all_but_undercurl'
" --or--
" "f[u][b][i]"          if txtfmt 'formats' is set to 'basic'
" "c<clr_patt>"
" Note: <clr_patt> must match one of the color definitions specified by user
" (or default if user hasn't overriden).
" Return: One of the following:
" 1) A single fmt token
" 2) A single clr token
" 3) '' - empty string if erroneous user entry
" Error: If error, function will set the script-local s:err_str
" Note: The function logic takes advantage of the fact that both strpart() and
" string offset bracket notation (s[i]) allow indices past end of string, in
" which case, they return empty strings.
fu! s:Translate_fmt_clr_spec(s)
	" Declare modifiable version of input parameter
	let s = a:s
	" Check for empty string (all whitespace considered empty string, as it
	" should have been detected as 'Cancel' request by caller).
	if s =~ '^\s*$'
		" Caller should validate this, but just in case
		let s:err_str = "Empty fmt/clr spec"
		return ''
	endif
	let len = strlen(s)
	let ret_str = ''
	if s[0] ==? 'f'
		" fmt string
		if s[1] == '-'
			if strlen(s) == 2
				" default format
				let ret_str = ret_str.nr2char(b:txtfmt_fmt_first_tok)
			else
				" Shouldn't be anything after f-
				let s:err_str = 'Unexpected chars after "f-"'
				return ''
			endif
		else
			" Not a default fmt request - remainder of string should match
			" [ubi[sr[c]]]
			let s = strpart(s, 1)
			if s =~ '[^'.b:ubisrc_fmt{b:txtfmt_num_formats-1}.']'
				if s !~ '[^ubisrc]'
					" User has mistakenly used s, r or c with 'short' formats
					" and/or c with a version of Vim that doesn't support
					" undercurl. Give an appropriate warning.
					if b:txtfmt_cfg_formats == 'basic'
						let s:err_str = "Only 'u', 'b' and 'i' attributes are permitted when 'short' formats are in effect"
					else " b:txtfmt_cfg_formats == 'all_but_undercurl'
						" Long formats are in use; hence, we can get here only
						" if user attempted to use undercurl in version of Vim
						" that doesn't support it.
						let s:err_str = "Undercurl attribute supported only in Vim 7 or later"
					endif
				else
					let s:err_str = 'Invalid chars in fmt spec after "f"'
				endif
				return ''
			else
				" Convert the entered chars to a binary val used to get token
				let bin_val = 0
				if s=~'u' | let bin_val = bin_val + 1 | endif
				if s=~'b' | let bin_val = bin_val + 2 | endif
				if s=~'i' | let bin_val = bin_val + 4 | endif
				if b:txtfmt_cfg_formats == 'all' || b:txtfmt_cfg_formats == 'all_but_undercurl'
					if s=~'s' | let bin_val = bin_val + 8  | endif
					if s=~'r' | let bin_val = bin_val + 16 | endif
					if b:txtfmt_cfg_formats == 'all'
						if s=~'c' | let bin_val = bin_val + 32 | endif
					endif
				endif
				let ret_str = ret_str.nr2char(b:txtfmt_fmt_first_tok + bin_val)
			endif
		endif
	elseif s[0] ==? 'c'
		" clr string
		if s[1] == '-'
			if strlen(s) == 2
				" default format
				let ret_str = ret_str.nr2char(b:txtfmt_clr_first_tok)
			else
				" Shouldn't be anything after c-
				let s:err_str = 'Unexpected chars after "c-"'
				return ''
			endif
		else
			" Not a default clr request - remainder of string denotes a
			" color
			let s = strpart(s, 1)
			" Determine which color index corresponds to color pattern
			let clr_ind = s:Lookup_clr_namepat(s)
			if clr_ind < 0
				let s:err_str = "Invalid color name pattern: '".s."'"
				return ''
			endif
			" IMPORTANT NOTE: Default clr region is offset zero, which is
			" different from red=0, green=0, blue=0, which is offset 1.
			" Thus, we must add 1 to clr index.
			let ret_str = ret_str.nr2char(b:txtfmt_clr_first_tok + clr_ind + 1)
		endif
	else
		let s:err_str = 'Invalid fmt/clr spec. Must begin with "f" or "c"'
		return ''
	endif
	" Return the token as a string
	return ret_str
endfu
" >>>
" Function: s:Translate_fmt_clr_list() <<<
" Purpose: Translate the input comma/dot-separated list of fmt/clr spec atoms
" into a string of tokens suitable for insertion into the buffer. Validation
" is performed. Also, cursor offset into translated token string is determined
" based upon the presence of a dot (replaces comma when it appears between
" fmt/clr atoms - may also appear as first or last character in fmt/clr spec
" list).
" Input: Comma/Dot-separated list of fmt/clr spec atoms.
" Return: String of the following format:
" <offset>,<tokstr>
" Error: Return empty string and set s:err_str
" Warning: Set s:wrn_str
fu! s:Translate_fmt_clr_list(s)
	" For convenience
	let s = a:s
	let len = strlen(s)
	" Initializations <<<
	let offset = -1			" -1 means not explicitly set by user
	let offset_fixed = 0	" binary flag
	let i = 0
	let sep = ''			"[,.] or '' for end-of string
	let num_fld = 0			" # of atoms encountered
	let tokstr = ''			" built up in loop
	" >>>
	" Process the fmt/clr spec atom(s) in a loop
	while i < len
		" Find end of spec ([,.] or end of string)
		" (Commas and dots not allowed except as field sep)
		" NOTE: Match with '$' returns strlen (even for empty string)
		let ie = match(s, '[,.]\|$', i)
		" Extract field sep and text
		let sep = ie<len ? s[ie] : ''
		" TODO - See about consolidating the if's below...
		if ie>i
			let fld = strpart(s, i, ie-i)
			let num_fld = num_fld+1
			" Translate field if non-empty
			let tok = s:Translate_fmt_clr_spec(fld)
			if tok == ''
				" Must have been error
				let s:err_str = "Invalid fmt/clr spec: '".fld."': ".s:err_str
				return ''
			endif
			let tokstr = tokstr.tok
		" Validate the field in various ways
		elseif i==ie	" check null fields
			let fld = ''
			if ie==0	" at beginning of list ('.' permitted)
				if ie==len-1
					let s:err_str = "Separator with nothing to separate"
					return ''
				elseif len==0
					" Note: This should probably be checked before now, but
					" just to be complete...
					let s:err_str = "Invalid empty fmt/clr spec list"
					return ''
				elseif sep=='.'
					let offset = 0
				else
					let s:err_str = "Invalid leading ',' in fmt/clr spec list"
					return ''
				endif
			else	" not at beginning of list
				let s:err_str = "Empty field encountered at '".strpart(s, i)
				return ''
			endif
		endif
		if ie==len-1	" validate last char in string
			if num_fld==0
				" NOTE: Can't actually get here...
				let s:err_str = "fmt/clr spec list contains no fields"
				return ''
			elseif sep!='.'
				let s:err_str = "Trailing comma not allowed in fmt/spec list"
				return ''
			endif
		endif
		" If here, field is OK unless atom is bad...
		" Do offset logic
		if offset==-1 && sep=='.'
			let offset = num_fld
		endif
		" Update for next iteration
		let i = ie+1
		if i > len
			break
		endif
		" OLD (implicit) logic for determining cursor offset <<<
		"if tok=~b:re_fmt_any_stok || tok=~b:re_fmt_etok
		"	if fmt_begun
		"		" Fix cursor location (if not already fixed)
		"		if offset==0
		"			let offset = num_fld-1
		"		endif
		"	endif
		"	" If fmt start tok, set flag
		"	if tok!~b:re_fmt_etok
		"		let fmt_begun = 1
		"	endif
		"elseif tok=~b:re_clr_any_stok || tok=~b:re_clr_etok
		"	if clr_begun
		"		" Fix cursor location (if not already fixed)
		"		if offset==0
		"			let offset = num_fld-1
		"		endif
		"	endif
		"	" If clr start tok, set flag
		"	if tok!~b:re_clr_etok
		"		let clr_begun = 1
		"	endif
		"endif
		" >>>
	endwhile
	" Return the special format string
	return offset.','.tokstr
endfu
" >>>
" Function: s:Jump_to_tok() <<<
" Purpose: Jumps forward or backwards (as determined by a:dir), to the
" v:count1'th nearest token of type given by a:type ('c'=clr 'f'=fmt 'a'=any
" (clr or fmt)). If 'till' argument is nonzero, jump will position cursor one
" char position closer to starting location than the sought token. (This
" behavior is analogous to t and T normal mode commands.)
" Note: If the map that invokes this function is a visual-mode mapping,
" special logic is required to restore the visual selection prior to
" performing any cursor movement. This is because Vim's default vmap behavior
" is to remove the visual highlighting and position the cursor at the start of
" the visual area as soon as the map is invoked. For the motion mappings that
" call this function, the default behavior is not acceptable.
" Inputs:
" type		1 or 2 chars indicating the type of token sought. Format is as
" 			follows:
" 			[{target-modifier}]{target-type}
"	 			{target-modifier} :=
"	 				b	'begin region' tokens only
"	 				e	'end region' tokens only
"	 			{target-type}
"	 				c = color, f = format, a = color or format
" dir		single char indicating direction for search (f=forward, b=back).
" 			Wrap behavior is determined by the 'wrapscan' option.
" till		If nonzero, jump lands cursor not on the token, but just 'before'
"			it (where before indicates the side closest to starting point).
"			'Till' is used because of analogy with Vim's t and T commands in
"			normal mode.
" v:count1	If greater than 1, indicates the number of jumps to be performed.
" 			Allows a count to be used with the invoking mapping when jumping
" 			to the N'th token of a particular type and in a particular
" 			direction is desired.
" a:1		If supplied, represents the count to be used. (Needed when this
"			function is called from a user-map)
" Return: Always return empty string, in case function is called from an
" expression register in insert mode.
" IMPORTANT NOTE: On the use of \%# atom -- When used in search() (i.e.,
" non-interactively), Vim appears to use lookahead to optimize when using
" \%#\@!; however, a '\%#' by itself, or followed by \@=, is NOT optimized.
" (Vim searches the entire file with wraparound before finding the cursor
" position!)
" NOTE: Ideally, if the 'till' flag is set for a backwards search, I would use
" the /e modifier with a ? search begun from normal mode to find the token and
" position the cursor on the character after it. (If token is last char on
" line, cursor would be positioned in first column of following line.)
" However, this can cause problems when tok range includes char code 128. This
" problem can be avoided if search() is used. Unfortunately, search() does not
" permit the /e modifier to be used (and \zs and/or \ze appear to be a bit
" buggy when used just after a newline - e.g., try /\n\zs/ and see what
" happens!). Thus, my strategy for finding the target location when the 'till'
" flag is set is to use search() to find the sought token, employing patterns
" that will match only if the 'till' destination location actually exists. If
" search() finds a valid destination, I then accomplish the 'till' move with a
" subsequent positioning command.
fu! s:Jump_to_tok(mode, type, dir, till, ...)
	" Get the search pattern
	if a:type == 'c'
		let re = b:txtfmt_re_clr_tok
	elseif a:type == 'bc'
		let re = b:txtfmt_re_clr_stok
	elseif a:type == 'ec'
		let re = b:txtfmt_re_clr_etok
	elseif a:type == 'f'
		let re = b:txtfmt_re_fmt_tok
	elseif a:type == 'bf'
		let re = b:txtfmt_re_fmt_stok
	elseif a:type == 'ef'
		let re = b:txtfmt_re_fmt_etok
	elseif a:type == 'a'
		let re = b:txtfmt_re_any_tok
	elseif a:type == 'ba'
		let re = b:txtfmt_re_any_stok
	elseif a:type == 'ea'
		let re = b:txtfmt_re_any_etok
	else
		" Error - shouldn't ever get here - just return
		return ''
	endif
	" Important Note: If mode is visual, Vim has already removed the visual
	" highlighting and positioned the cursor at the start of the visual
	" region. Since this is a motion mapping, we need to undo this; i.e.,
	" restore the visual highlighting and put the cursor at the correct
	" end/corner of the visual region, allowing for the fact that any number
	" of "o" and or "O" commands may have been executed to bounce the cursor
	" between ends/corners... Normal mode gv fits the bill.
	" Important Note: When a visual mode mapping invokes this function, Vim
	" has already changed mode to normal before we get here. Thus, we must use
	" the mode string passed from the mapping to determine whether we need to
	" restore the visual selection. Since we're using gv, it doesn't matter
	" which visual sub-mode was in effect.
	if a:mode == 'v'
		normal! gv
	endif
	" Get the search options
	if a:dir == 'b'
		" Leave wrap option alone so that 'wrapscan' will be honored
		let opt = 'b'
		if a:till
			" NOTE: The \n\_. handles the following 2 cases:
			" 1) Sought token is at end of line followed by non-empty line
			" 2) Sought token is at end of line followed by empty line
			" NOTE: The \%#\@! ensures that if we're sitting on a character
			" after the the target token type, we don't match the token just
			" before it. (Otherwise we'd get stuck when trying to do multiple
			" successive backwards jumps.)
			let re = re.'\%(\n\%#\@!\_.\|\%#\@!.\)\@='
		endif
	elseif a:dir == 'f'
		" Leave wrap option alone so that 'wrapscan' will be honored
		let opt = ''
		if a:till
			" The following pattern will position us on the buffer position
			" one char prior to the sought token, even in case where the token
			" is at beginning of a line preceded by blank line.
			" NOTE: landing on a \n puts cursor at end of line ended by the
			" newline.
			" NOTE: \@= is necessary when cpo-c is set to avoid skipping every
			" other token when there are multiple consecutive tokens of same
			" type.
			let re = '\_.'.re.'\@='
		endif
	else
		" Error - Should never get here - just return
		return ''
	endif
	" Get the count, which is either supplied explicitly as optional extra
	" arg, or is obtained from v:count1
	if a:0 > 0
		" Note: Counts are generated internally; hence, validation has
		" been performed already.
		let l:count = a:1
	else
		let l:count = v:count1
	endif
	" In a loop count, perform the search()
	let i = 0
	while i < l:count
		" Note: If search fails, cursor will not be moved.
		" IMPORTANT NOTE: Simplest thing would be to use normal search command
		" here, but that gives problems if tok range includes 128!
		let l2 = search(re, opt)
		" Did we find the sought token?
		if l2 > 0
			" We're on found tok
			if a:till
				" NOTE: 2 cases:
				" 1) Backward search - we're on token, but we need to be at
				" position just past it (and search() assures us the position
				" exists.
				" 2) Forward search - search() got us to correct position (for
				" both 'till' and non-'till' cases.
				if a:dir == 'b'
					" Backward search
					" IMPORTANT NOTE: Original implementation used col() and
					" cursor(), which are *NOT* multi-byte safe!
					" Use virtcol and special search instead.
					" Note: Vim documentation implies that the following holds
					" true when cursor is positioned on the last character of
					" the line: virtcol('.') == virtcol('$') - 1
					let c2 = virtcol('.')
					if c2 != virtcol('$') - 1
						" Not last char on line
						call search('\%'.(c2 + 1).'v', '')
					else
						" Last char on line - move to start of next line
						" Note: cursor() can handle col of 1 even for empty
						" line.  Also, it's mult-byte safe.
						call cursor(l2 + 1, 1)
					endif
				endif
			endif
		else
			" No reason to keep looping...
			break
		endif
		let i = i + 1
	endwhile
	return ''
endfu
" >>>
" Function: s:Mapwarn_check() <<<
" Purpose: Determine whether the user has already been warned about the
" mapping ambiguity/conflict indicated by input arguments, and return nonzero
" if so. Additionally, if the 'add' parameter is true, record the input
" conflict/ambiguity in the data structures maintaining such information so
" that the function will return true for it next time.
" Inputs:
" lhs  - lhs of the new mapping
" rhs  - rhs of the old (existing) mapping
" mode - single character indicating the mode of the mapping (e.g. n=normal,
"        v=visual, i=insert, o=operator-pending, etc...)
" add  - flag indicating whether the conflict indicated by lhs, rhs and mode
"        should be added to the data structures searched by this function
fu! s:Mapwarn_check(lhs, rhs, mode, add)
	let found = 0
	let i = 0
	if exists('g:txtfmt_mapwarn_cnt')
		while i < g:txtfmt_mapwarn_cnt
			if a:lhs == g:txtfmt_mapwarn_lhs{i} &&
				\a:rhs == g:txtfmt_mapwarn_rhs{i} &&
				\a:mode == g:txtfmt_mapwarn_mode{i}
				let found = 1
				break
			endif
			let i = i + 1
		endwhile
	endif
	if !found && a:add
		" Make sure g:txtfmt_mapwarn_cnt is self-starting
		if !exists('g:txtfmt_mapwarn_cnt')
			let g:txtfmt_mapwarn_cnt = 0
		endif
		" Add a new conflict/ambiguity to the arrays
		let g:txtfmt_mapwarn_lhs{g:txtfmt_mapwarn_cnt} = a:lhs
		let g:txtfmt_mapwarn_rhs{g:txtfmt_mapwarn_cnt} = a:rhs
		let g:txtfmt_mapwarn_mode{g:txtfmt_mapwarn_cnt} = a:mode
		let g:txtfmt_mapwarn_cnt = g:txtfmt_mapwarn_cnt + 1
	endif
	" Indicate whether input conflict/ambiguity was found
	return found
endfu
" >>>
" Function: s:Undef_map() <<<
" Purpose: Creates an undo action for the map whose lhs, rhs and unmap_cmd are
" input, and adds the undo action to b:undo_ftplugin.
" Inputs:
" mode 	- single char, used as input to maparg, mapcheck, etc...
" lhs   - string representing the lhs of the map to be undone
" rhs   - string representing the rhs of the map to be undone.
" Assumptions:
" -All maps to be undone are buffer-local.
" -All occurrences of '<SID>[_a-zA-Z0-9]' in the rhs of a mapping defined by
"  this plugin represent a call to a script-local function.
" Note: rhs is required so that we can be sure to delete *only* maps created
" by this plugin. (Consider that user could either intentionally or
" inadvertently override one of the txtfmt maps with a completely unrelated
" map after this plugin is loaded. For this reason, we cannot (or should not)
" blindly delete lhs.)
fu! s:Undef_map(lhs, rhs, mode)
	" Determine the unmap command to be used.
	if a:mode=='n'
		let unmap_cmd = 'nunmap'
	elseif a:mode=='i'
		let unmap_cmd = 'iunmap'
	elseif a:mode=='o'
		let unmap_cmd = 'ounmap'
	elseif a:mode=='v'
		let unmap_cmd = 'vunmap'
	else
		echoerr 'Internal error - unsupported mapmode passed to Undef_map()'
		return 1
	endif
	" Create the undo action, taking special care to avoid deleting a map with
	" the same lhs, created by user after the sourcing of this plugin.
	" Note: Prep_for_single_quotes ensures that single quotes contained in lhs
	" or rhs are properly escaped before being wrapped in the single-quoted
	" string that will be parsed when b:undo_ftplugin is exec'ed.
	" Note: Be sure not to add whitespace between the lhs of the map being
	" unmapped and the subsequent '|' as this will result in nonexistent
	" mapping error.
	" Note: When the maparg() is executed, it will return function names of
	" the form '<SNR>{number}_func' rather than '<SID>func'. Thus, to ensure
	" that the delayed comparison works properly, I need to convert a:rhs to
	" the <SNR>{number}_ form now.
	let rhs = substitute(a:rhs, '<SID>\ze[_a-zA-Z0-9]',
		\'\= "<SNR>" . s:SID() . "_"', 'g')
	call s:Add_undo("if maparg('".s:Prep_for_single_quotes(a:lhs)
		\."', '".a:mode."') == '".s:Prep_for_single_quotes(rhs)."' | "
		\.unmap_cmd." <buffer> ".a:lhs."| endif")
endfu
" >>>
" Function: s:Def_map() <<<
" Purpose: Define both the level 1 and level 2 map as appropriate.
" Inputs:
" mode 	- single char, used as input to maparg, mapcheck, etc...
" lhs1	- lhs of first-level map
" lhs2	- rhs of first-level map, lhs of second-level map
" rhs2	- rhs of second-level map
" How: Consider whether user already has a map to level 2 (which should take
" precedence over maplevel 1). Also, make sure the map from level 2, if it
" exists, is not incorrect, etc...
" Note: Cause b:undo_ftplugin to be updated so that whatever mappings are made
" by us will be unmapped when ftplugin is unloaded.
" Return:
" 0			- success
" nonzero	- error
" NOTE: Function will echoerr to user
fu! s:Def_map(mode, lhs1, lhs2, rhs2)
	" TODO - Perhaps eventually support operator mode if needed
	if a:mode=='n'
		let cmd1 = 'nmap'
		let cmd2 = 'nnoremap'
	elseif a:mode=='i'
		let cmd1 = 'imap'
		let cmd2 = 'inoremap'
	elseif a:mode=='o'
		let cmd1 = 'omap'
		let cmd2 = 'onoremap'
	elseif a:mode=='v'
		let cmd1 = 'vmap'
		let cmd2 = 'vnoremap'
	else
		echoerr 'Internal error - unsupported mapmode passed to Def_map()'
		return 1
	endif
	" Do first map level <<<
	if !hasmapto(a:lhs2, a:mode)
		" User hasn't overridden the default level 1 mapping
		" Make sure there's no conflict or ambiguity between an existing map
		" and the default one we plan to add...
		let oldarg = maparg(a:lhs1, a:mode)
		let oldchk = mapcheck(a:lhs1, a:mode)
		" Check for conflicts and ambiguities, decoding applicable portions of
		" mapwarn option character flag string into more immediately useful
		" variables, to avoid messy ternaries in the subsequent logic.
		" Note: Create only the variables that will be used.
		if oldarg != ''
			" Map conflict
			let l:problem = 'c'
			if b:txtfmt_cfg_mapwarn =~ 'M'
				let l:msg_or_err = 'm'
			elseif b:txtfmt_cfg_mapwarn =~ 'E'
				let l:msg_or_err = 'e'
			endif
			if exists('l:msg_or_err')
				let l:once_only = b:txtfmt_cfg_mapwarn =~ 'O'
			endif
			let l:create = b:txtfmt_cfg_mapwarn =~ 'C'
			let l:old_rhs = oldarg
		elseif oldchk != ''
			" Map ambiguity
			let l:problem = 'a'
			if b:txtfmt_cfg_mapwarn =~ 'm'
				let l:msg_or_err = 'm'
			elseif b:txtfmt_cfg_mapwarn =~ 'e'
				let l:msg_or_err = 'e'
			endif
			if exists('l:msg_or_err')
				let l:once_only = b:txtfmt_cfg_mapwarn =~ 'o'
			endif
			let l:create = b:txtfmt_cfg_mapwarn =~ 'c'
			let l:old_rhs = oldchk
		endif
		if exists('l:problem')
			" There's an ambiguity or conflict
			if exists('l:msg_or_err')
				" We need to warn unless warning is precluded by 'once-only'
				" mechanism
				if !l:once_only || !s:Mapwarn_check(a:lhs1, l:old_rhs, a:mode, l:once_only)
					let l:warnstr = 'Level 1 map '
						\.(l:problem == 'a' ? 'ambiguity:' : 'conflict: ')
						\.a:lhs1.' already mapped to '.l:old_rhs
					if l:msg_or_err == 'm'
						echomsg l:warnstr
					else
						echoerr l:warnstr
					endif
				endif
			endif
		endif
		" Do the map for buffer unless map creation is precluded by conflict
		" or ambiguity in absence of the 'create' flag.
		" Note: Do not use <unique> attribute, since that would cause Vim to
		" display error, due to the original mapping.
		if !exists('l:problem') || l:create
			exe cmd1.' <buffer> '.a:lhs1.' '.a:lhs2
			" Create undo action for the map just created
			call s:Undef_map(a:lhs1, a:lhs2, a:mode)
		endif
	else
		"echomsg "Skipping 1st level"
	endif
	" >>>
	" Do second map level <<<
	" Assumption: Second-level mappings have long <Scriptname><...> names,
	" preceded by <Plug>. It is safe to assume user hasn't mapped one to
	" something else...
	exe cmd2.' <buffer> '.a:lhs2.' '.a:rhs2
	" Create undo action for the map just created
	call s:Undef_map(a:lhs2, a:rhs2, a:mode)
	" >>>
	" Success
	return 0
endfu
" >>>
" Function: s:MakeString() <<<
" Purpose: Build and return a string by concatenating a base string some
" number of times to itself.
" Inputs:
" str	-base string, which will be concatenated to itself
" len	-# of occurrences of 'str' to put in the return string
" Return: The generated string
fu! s:MakeString(str, len)
	let s = ''
	let i = 0
	while i < a:len
		let s = s.a:str
		let i = i + 1
	endwhile
	return s
endfu
" >>>
" Function: s:ShowTokenMap() <<<
" Purpose: Echo to user a table showing the current use of all tokens in the
" range of fmt/clr tokens.
" How: Use echo, as this is intended as a temporary showing for informational
" purposes only, and highlighting of the text is not required.
" Format: Should be something like this...
"=== COLORS ===
"char-nr  description  clr-pattern                                  cterm      gui
"180      no color     -
"181      color0        ^\\%(k\\|bla\\%[ck]\\)$,c:Black,g:#000000   Black      #000000
"182      color1        ^blu\\%[e]$,c:DarkBlue,g:#0000FF            DarkBlue   #0000FF
"183      color2        ^g\\%[reen]$,c:DarkGreen,g:#00FF00          DarkGreen  #00FF00
"=== FORMAT ===
"char-nr  description  spec
"189      no format    -
"190      italic       i
"191      bold         b
"192      bold,italic  bi
".
".
".
".
fu! s:ShowTokenMap()
	" Loop 2 times - first time is just to calculate column widths
	let cw1 = 0 | let cw2 = 0 | let cw3 = 0 | let cw4 = 0 | let cw5 = 0
	" Initialize the vars that will accumulate table text
	let fmt_header = '' | let fmt_lines = ''
	let clr_header = '' | let clr_lines = ''
	let i = 0
	while i < 2
		" Loop over all format lines (1 hdr and b:txtfmt_num_formats-1 fmt)
		let iFmt = -1	" start with header line
		while iFmt < b:txtfmt_num_formats
			let line = ''	" Initialize text for current line
			" Column 1
			if iFmt == -1
				let col1_text = 'char-nr'
			else
				let col1_text = ''.(b:txtfmt_fmt_first_tok + iFmt)
			endif
			if i == 0
				" Calculate col width
				if strlen(col1_text) > cw1
					let cw1 = strlen(col1_text)
				endif
			else
				" Output line
				let line = line.(col1_text.s:MakeString(' ', cw1 + 2 - strlen(col1_text)))
			endif
			" Column 2
			if iFmt == -1
				let col2_text = 'description'
			elseif iFmt == 0
				let col2_text = 'no format'
			else
				let col2_text = b:txtfmt_fmt{iFmt}
			endif
			if i == 0
				" Calculate col width
				if strlen(col2_text) > cw2
					let cw2 = strlen(col2_text)
				endif
			else
				" Output line
				let line = line.(col2_text.s:MakeString(' ', cw2 + 2 - strlen(col2_text)))
			endif
			" Column 3
			if iFmt == -1
				let col3_text = 'fmt-spec'
			elseif iFmt == 0
				let col3_text = '-'
			else
				let col3_text = b:ubisrc_fmt{iFmt}
			endif
			if i == 0
				" Calculate col width
				if strlen(col3_text) > cw3
					let cw3 = strlen(col3_text)
				endif
			else
				" Output line
				let line = line.(col3_text.s:MakeString(' ', cw3 + 2 - strlen(col3_text)))
			endif
			" Accumulate line just built into the list of lines
			if i == 1
				if iFmt == -1
					" Store header line separately so that echohl can be used
					let fmt_header = line
				else
					" Regular row in table (non-header)
					let fmt_lines = fmt_lines.(iFmt==0?'':"\<NL>").line
				endif
			endif
			let iFmt = iFmt + 1
		endwhile
		" Loop over all color tokens
		" Index note: In this loop, index 0 refers to 'no color', while index
		" 1 refers to color 0 (rgb=000000).
		let iClr = -1
		while iClr < b:txtfmt_num_colors
			let line = ''	" Initialize text for current line
			" Column 1
			if iClr == -1
				let col1_text = 'char-nr'
			else
				let col1_text = ''.(b:txtfmt_clr_first_tok + iClr)
			endif
			if i == 0
				" Calculate col width
				if strlen(col1_text) > cw1
					let cw1 = strlen(col1_text)
				endif
			else
				" Output line
				let line = line.(col1_text.s:MakeString(' ', cw1 + 2 - strlen(col1_text)))
			endif
			" Column 2
			if iClr == -1
				let col2_text = 'description'
			elseif iClr == 0
				let col2_text = 'no color'
			else
				let col2_text = 'Color'.iClr
			endif
			if i == 0
				" Calculate col width
				if strlen(col2_text) > cw2
					let cw2 = strlen(col2_text)
				endif
			else
				" Output line
				let line = line.(col2_text.s:MakeString(' ', cw2 + 2 - strlen(col2_text)))
			endif
			" Column 3
			if iClr == -1
				let col3_text = 'clr-pattern'
			elseif iClr == 0
				let col3_text = '-'
			else
				let col3_text = b:txtfmt_clr_namepat{iClr-1}
			endif
			if i == 0
				" Calculate col width
				if strlen(col3_text) > cw3
					let cw3 = strlen(col3_text)
				endif
			else
				" Output line
				let line = line.(col3_text.s:MakeString(' ', cw3 + 2 - strlen(col3_text)))
			endif
			" Column 4
			if iClr == -1
				let col4_text = 'ctermfg'
			elseif iClr == 0
				let col4_text = 'N.A.'
			else
				let col4_text = b:txtfmt_clr_ctermfg{iClr-1}
			endif
			if i == 0
				" Calculate col width
				if strlen(col4_text) > cw4
					let cw4 = strlen(col4_text)
				endif
			else
				" Output line
				let line = line.(col4_text.s:MakeString(' ', cw4 + 2 - strlen(col4_text)))
			endif
			" Column 5
			if iClr == -1
				let col5_text = 'guifg'
			elseif iClr == 0
				let col5_text = 'N.A.'
			else
				let col5_text = b:txtfmt_clr_guifg{iClr-1}
			endif
			if i == 0
				" Calculate col width
				if strlen(col5_text) > cw5
					let cw5 = strlen(col5_text)
				endif
			else
				" Output line
				let line = line.(col5_text.s:MakeString(' ', cw5 + 2 - strlen(col5_text)))
			endif
			" Accumulate line just built into the list of lines
			if i == 1
				if iClr == -1
					" Store header line separately so that echohl can be used
					let clr_header = line
				else
					" Regular row in table (non-header)
					let clr_lines = clr_lines.(iClr==0?'':"\<NL>").line
				endif
			endif
			let iClr = iClr + 1
		endwhile
		let i = i + 1
	endwhile
	echohl Title
	echo '=== COLORS ==='
	echo clr_header
	echohl None
	echo clr_lines
	echohl Title
	echo '=== FORMAT ==='
	echo fmt_header
	echohl None
	echo fmt_lines
endfu
" >>>
" Function: s:MoveStartTok() <<<
" IMPORTANT NOTE: Special care must be taken when defining this function, as
" it invokes :Refresh command, which causes the script to be re-sourced. This
" leads to E127 'Cannot redefine function' when fu[!] is encountered, since
" the function is in the process of executing.
if !exists('*s:MoveStartTok')
fu! s:MoveStartTok(moveto, ...)
	if a:0
		" Validate and process optional version value
		if a:0 != 1
			echoerr 'Incorrect # of arguments supplied to :MoveStartTok (1 or 2 expected)'
			return
		elseif (0 + a:1) =~ '^[1-9][0-9]\{2}$'
			" Use version supplied by user
			let old_ver = a:1
		else
			echoerr a:1.' is not a valid Vim version number. Should be same format as v:version'
			return
		endif
	else
		" Assume current version
		let old_ver = v:version
	endif
	" Validate the new starttok
	if a:moveto !~ '^\s*'.b:txtfmt_re_number_atom.'\s*$'
		echoerr "Invalid 'starttok' value supplied: `".a:moveto."'"
		return
	endif
	" Get current settings from buffer
	" Assumption: This function can be invoked only from an active txtfmt
	" buffer
	let old_starttok = b:txtfmt_cfg_starttok
	let old_formats = b:txtfmt_cfg_formats
	" Determine new settings
	let new_starttok = a:moveto
	let new_formats = b:txtfmt_cfg_formats
	" Determine amount of shift (signed value)
	let l:offset = new_starttok - old_starttok
	" Correct old formats based upon Vim version if necessary
	if old_formats == 'all' && old_ver < b:txtfmt_const_vimver_undercurl
		let old_formats = 'all_but_undercurl'
	endif
	" Determine length of new ranges
	let new_rangelen = b:txtfmt_const_tokrange_size_{new_formats}
	" Perform upper-bound check on new range
	let enc_class = TxtfmtCommon_Encoding_get_class(&enc)
	if !(new_starttok + new_rangelen - 1 <= b:txtfmt_const_tokrange_limit_{enc_class})
		" Invalid destination for move!
		echoerr "Starttok value of `".new_starttok."' causes upper bound for encoding `"
			\.&enc."' to be exceeded"
		return
	endif
	" If here, move is legal.
	" STEP 1: Translate only the unescaped tokens
	exe '%s/'.b:txtfmt_re_any_tok.'/\='
		\.'nr2char(char2nr(submatch(0)) + l:offset)'
		\.'/g'
	" STEP 2: Remove escape chars for characters that are txtfmt tokens under
	" old tokrange setting, but not under new. 
	" Design Decision: For backslash escape method, remove only backslashes
	" escaping txtfmt tokens.
	" Design Decision: An escape is an escape if it's escaping any txtfmt
	" token, even a useless 'no-format' or 'no-color' token appearing outside
	" a region. (Recall that I don't highlight these to facilitate removal by
	" user...)
	" Rationale: The goal of this function is not to clean up user's file, but
	" simply to translate tokrange
	if b:txtfmt_cfg_escape != 'none'
		if b:txtfmt_cfg_escape == 'bslash'
			let re_esc_pair = '\%(\%(^\|[^\\]\)\%(\\\\\)*\)\@<=\(\\\)\('
				\.b:txtfmt_re_any_tok_atom.'\)'
		elseif b:txtfmt_cfg_escape == 'self'
			" IMPORTANT NOTE: This pattern is intended to be used only in a global
			" substitution; hence, it is not necessary to use assertions to ensure
			" that the actual escape char ends up in \1. This is because the first
			" token in a sequence of escape/escapee pairs is always an escape, as
			" is every other one after that in the sequence. A '%s///' works this
			" way naturally.
			let re_esc_pair = '\('.b:txtfmt_re_any_tok_atom.'\)\(\1\)'
		endif
		" Perform substitution
		" Note: sub-replace-special mechanism translates backslash escapes;
		" hence the escape() call to get an extra backslash when \1 is
		" backslash.
		exe '%s/'.re_esc_pair.'/\='
			\.'char2nr(submatch(2)) >= l:new_starttok &&'
			\.' char2nr(submatch(2)) <= l:new_starttok + l:new_rangelen - 1'
			\.' ? escape(submatch(1), "\\").submatch(2)'
			\.' : submatch(2)/g'
	endif
	" Cause buffer to be refreshed with the new settings
	" Note: The following events are consumed by modeline processing logic,
	" which may need to alter the starttok value in a modeline
	" Note: <f-args> ensures that new_starttok is a string. This is important
	" because it permits the modeline processing function to respect user's
	" choice of hex or dec when altering the modeline.
	let b:txtfmt_event_new_starttok = new_starttok
	let b:txtfmt_event_new_formats = new_formats
	:Refresh
endfu
endif	" if !exists('*s:MoveStartTok')
" >>>
" >>>
" Configuration <<<
" Needed only for ftplugin
" Note: Performed after the Common Configuration, which sets the 'starttok'
" option, needed when processing user maps

" Function: s:Expand_user_map_macro() <<<
" Purpose: Expand the input string, which is assumed to be the `...' in one of
" the user-map expansion sequences of the form <...>.
" Return: If the macro is valid, return the expanded text, just as it would
" appear in the rhs of the map; otherwise, an empty string.
fu! s:Expand_user_map_macro(s)
	let re_ins_tok_i   = '^i\\:\(.\+\)$'
	let re_ins_tok_n   = '^n\([1-9]\d*\)\?\\\(v\?\)\([iIaAoOs]\):\(.\+\)$'
	let re_jump_to_tok = '^\([nvio]\)\([1-9]\d*\)\?\([][]\)\(t\?\)\([be]\?[fca]\)'
	" Determine which macro type we have
	if a:s =~ re_ins_tok_n . '\|' . re_ins_tok_i
		" Insert-token macro
		if a:s[0] == 'n'
			" Insert-token macro (normal)
			let l:count       = substitute(a:s, re_ins_tok_n, '\1', '')
			let end_in_norm   = substitute(a:s, re_ins_tok_n, '\2', '') == 'v'
			let enter_ins_cmd = substitute(a:s, re_ins_tok_n, '\3', '')
			let fmtclr_list   = substitute(a:s, re_ins_tok_n, '\4', '')
		else
			" Insert-token macro (insert)
			let fmtclr_list = substitute(a:s, re_ins_tok_i, '\1', '')
		endif
		" Validate / Translate the fmt/clr list
		let tokstr = s:Translate_fmt_clr_list(fmtclr_list)
		if tokstr==''
			" Invalid fmt/clr list
			" TODO: Perhaps fix up the error string.
			let s:err_str = "Invalid fmt/clr list in user map rhs: ".s:err_str
			return ''
		endif
		" Create the mode-specific expansion text
		if a:s[0] == 'n'
			" normal mode
			let seq = ":call <SID>Insert_tokstr('"
				\.tokstr."', '".enter_ins_cmd."', 1, ".end_in_norm
				\.(strlen(l:count) ? (", ".l:count) : "")
				\.")<CR>"
				\.":call <SID>Adjust_cursor()<CR>"
		else
			" insert mode
			let seq = "<C-R>=<SID>Insert_tokstr('".tokstr."', 'i', 1, 0)<CR>"
			\."<C-R>=<SID>Adjust_cursor()<CR>"
		endif
	elseif a:s =~ re_jump_to_tok
		" Jump to token macro
		let l:mode   = substitute(a:s, re_jump_to_tok, '\1', '')
		let l:count  = substitute(a:s, re_jump_to_tok, '\2', '')
		let l:dir    = substitute(a:s, re_jump_to_tok, '\3', '') == '[' ? 'b' : 'f'
		let l:till   = substitute(a:s, re_jump_to_tok, '\4', '') == 't' ? 1 : 0
		let l:target = substitute(a:s, re_jump_to_tok, '\5', '')
		if l:mode =~ '[nvo]'
			let l:seq = ":<C-U>call <SID>Jump_to_tok('"
				\.l:mode."', '".l:target."', '".l:dir."', ".l:till
				\.(strlen(l:count) ? (", ".l:count) : "")
				\.")<CR>"
		else
			" TODO - Permit insert-mode?
			let l:seq = "<C-R>=<SID>Jump_to_tok('"
				\.l:mode."', '".l:target."', '".l:dir."', ".l:till
				\.(strlen(l:count) ? (", ".l:count) : "")
				\.")<CR>"
		endif
	else
		let s:err_str = "Invalid user-map expansion sequence: `<".a:s.">'"
		return ''
	endif
	" If here, expansion was successul. Return the expanded text.
	return seq
endfu
" >>>
" Function: s:OldExpand_user_map_macro() <<<
" Purpose: Expand the input string, which is assumed to be the `...' in one of
" the user-map expansion sequences of the form <...>.
" Return: If the macro is valid, return the expanded text, just as it would
" appear in the rhs of the map; otherwise, an empty string.
fu! s:OldExpand_user_map_macro(s)
	let re_ins_tok_i   = '^i:\(.\+\)$'
	let re_ins_tok_n   = '^n\(v\?\)\([iIaAoOs]\):\(.\+\)$'
	let re_jump_to_tok = '^\([1-9]\d*\)\?\([][]\)\(t\?\)\([fFcCaA]\)'
	" Determine which macro type we have
	if a:s =~ re_ins_tok_n . '\|' . re_ins_tok_i
		" Insert-token macro
		if a:s[0] == 'n'
			" Insert-token macro (normal)
			let end_in_norm   = substitute(a:s, re_ins_tok_n, '\1', '') == 'v'
			let enter_ins_cmd = substitute(a:s, re_ins_tok_n, '\2', '')
			let fmtclr_list   = substitute(a:s, re_ins_tok_n, '\3', '')
		else
			" Insert-token macro (insert)
			let fmtclr_list = substitute(a:s, re_ins_tok_i, '\1', '')
		endif
		" Validate / Translate the fmt/clr list
		let tokstr = s:Translate_fmt_clr_list(fmtclr_list)
		if tokstr==''
			" Invalid fmt/clr list
			" TODO: Perhaps fix up the error string.
			let s:err_str = "Invalid fmt/clr list in user map rhs: ".s:err_str
			return ''
		endif
		" Create the mode-specific expansion text
		if a:s[0] == 'n'
			" normal mode
			let seq = ":call <SID>Insert_tokstr('"
				\.tokstr."', '".enter_ins_cmd."', 1, ".end_in_norm.")<CR>"
				\.":call <SID>Adjust_cursor()<CR>"
		else
			" insert mode
			let seq = "<C-R>=<SID>Insert_tokstr('".tokstr."', 'i', 1, 0)<CR>"
			\."<C-R>=<SID>Adjust_cursor()<CR>"
		endif
	elseif a:s =~ re_jump_to_tok
		" Jump to token macro
		let l:count  = substitute(a:s, re_jump_to_tok, '\1', '')
		let l:dir    = substitute(a:s, re_jump_to_tok, '\2', '') == '[' ? 'b' : 'f'
		let l:till   = substitute(a:s, re_jump_to_tok, '\3', '') == 't' ? 1 : 0
		let l:target = substitute(a:s, re_jump_to_tok, '\4', '')
		" TODO - Figure out how to handle the mode argument...
		" Probably make it optional, and if not supplied, get from mode()...
		let l:seq = ":<C-U>call <SID>Jump_to_tok('"
			\.l:target."', '".l:dir."', ".l:till
			\.(strlen(l:count) ? (", ".l:count) : "")
			\.")<CR>"
	else
		let s:err_str = "Invalid user-map expansion sequence: `<".a:s.">'"
		return ''
	endif
	" If here, expansion was successul. Return the expanded text.
	return seq
endfu
" >>>
" Function: s:Translate_user_map_rhs() <<<
" Purpose: Convert the rhs specified in a user map definition string to the
" rhs that will be used in the actual map command. Special <<...>> sequences
" are expanded.
" Input: rhs string as it would appear in a user-map string
" Return: The rhs as it would appear in a map command (with user-map macros
" expanded)
" Error: Set s:err_str and return empty string
fu! s:Translate_user_map_rhs(rhs)
	let s = a:rhs
	" Catch empty (or all ws) strings - shouldn't be input
	if s =~ '^[[:space:]]*$'
		let s:err_str = "f:User map rhs must contain at least 1 non-whitespace char"
		return ''
	endif
	" Loop until special sequences are all expanded
	let ret_str = ''	" build up in loop
	let len = strlen(s)
	let i1 = 0
	let i2 = 0
	while i2 < len
		" Find start of <<...>> sequence - this is safe even if i2 is index of
		" next '<'
		"let i1 = matchend(s, '\%(\\\_.\|[^<]\)*', i2)
		let i1 = matchend(s, '<<', i2)
		if i1 < 0
			" String is exhausted - accumulate up to end
			let ret_str = ret_str.strpart(s, i2)
			break
		else
			" Accumulate, prior to processing <<...>>
			let ret_str = ret_str.strpart(s, i2, i1-i2-2)
		endif
		" Now find closing `>>' (it's not optional at this point)
		let i2 = match(s, '>>', i1)
		if i2 < 0
			let s:err_str = "Unmatched `<<' in user map rhs"
			return ''
		endif
		" Extract stuff inside <<...>>
		" i1 points to 1st char beyond `<<'
		" i2 points to first `>'
		" i1 == i2 implies empty ...
		if i2 > i1
			let seq = strpart(s, i1, i2-i1)
		else
			let s:err_str = "Empty fmt/clr map sequence"
			return ''
		endif
		" We have a non-empty sequence. Convert txtfmt-specific <rt> to `>'
		" before passing to Expand_user_map_macro for expansion.
		"let seq = substitute(seq, '\\\(.\)', '\1', 'g')
		let seq = substitute(seq, '<rt>', '>', 'g')
		" Expand the macro
		let seq = s:Expand_user_map_macro(seq)
		" Append the expanded text to the return string
		let ret_str = ret_str.seq
		" Make i2 point just past `>>' (it's on the 1st `>')
		let i2 = i2+2
	endwhile
	" Return the now completely expanded string
	return ret_str
endfu
" >>>
" Function: s:OldTranslate_user_map_rhs() <<<
" Purpose: Convert the rhs specified in a user map definition string to the
" rhs that will be used in the actual map command. Special <...> sequences are
" expanded.
" Input: rhs string as it would appear in map cmd, but with both < and \
" backslash-escaped (as well as any > that needs to be included literally
" within the <..>).
" Return: The rhs as it would appear in a map command (1 level of
" backslash-escaping removed)
" Error: Set s:err_str and return empty string
fu! s:OldTranslate_user_map_rhs(rhs)
	let s = a:rhs
	" Catch empty (or all ws) strings - shouldn't be input
	if s =~ '^[[:space:]]*$'
		let s:err_str = "f:User map rhs must contain at least 1 non-whitespace char"
		return ''
	endif
	" Loop until special sequences are all expanded
	let ret_str = ''	" build up in loop
	let len = strlen(s)
	let i1 = 0
	let i2 = 0
	while i2 < len
		" Find start of <...> sequence - this is safe even if i2 is index of
		" next '<'
		let i1 = matchend(s, '\%(\\\_.\|[^<]\)*', i2)
		if i1 < 0
			" Shouldn't get in here using matchend with pattern above
			let s:err_str = "f:Internal error from within Translate_user_map_rhs() - report to developer"
			return ''
		elseif i1 >= len
			" String is exhausted - accumulate up to end
			let ret_str = ret_str.strpart(s, i2)
			break
		else
			" Accumulate, prior to processing <...>
			let ret_str = ret_str.strpart(s, i2, i1-i2)
		endif
		" Now find closing '>', skipping over any backslash escaped ones
		let i2 = match(s, '\%(\\.\)*\zs>', i1)
		if i2 < 0
			let s:err_str = "f:Unescaped and unmatched '<' in user map rhs"
			return ''
		endif
		" Extract stuff inside < >
		if i2 > i1+1
			" i1 points to < and i2 points to >
			let seq = strpart(s, i1+1, i2-i1-1)
		else
			let s:err_str = "f:Empty fmt/clr map sequence"
			return ''
		endif
		" We have a non-empty sequence. Unescape it before passing to
		" Expand_user_map_macro for expansion.
		let seq = substitute(seq, '\\\(.\)', '\1', 'g')
		" Expand the macro
		let seq = s:Expand_user_map_macro(seq)
		" Escape the replacement string the same as non-replaced text
		let ret_str = ret_str.escape(seq, '\')
		" Make i2 point just past processed text (past closing '>')
		let i2 = i2+1
	endwhile
	" Unescape expanded rhs to get into form used in map cmd
	" (Remove 1 level of escaping)
	return substitute(ret_str, '\\\(.\)', '\1', 'g')
endfu
" >>>
" Function: s:Do_user_maps() <<<
" Purpose: Process any special global variables set by user, for the purpose
" of allowing him to build his own map sequences from primitives.
" How:
fu! s:Do_user_maps()
	" In the following regex, \1=map command, \2=lhs, \3=rhs
	" RULES:
	" map_cmd must be imap, inoremap, nmap or nnoremap
	" map_lhs is terminated by first unescaped whitespace
	"   -whitespace may appear in lhs if preceded by <C-V>
	" map_rhs is everything else in the string
	" 	-must have extra level of escaping for \ and <
	" 	-may contain special <[in]:...> sequences
	" 	TODO - Fix up the regex...
	let re_usermap = '^\s*\([in]\%(nore\)\?map\)\s\+'
		\.'\(\%('."\<C-V>.".'\|\S\)\+\)\s\+\(.\+\)'
	" Allow up to configurable number of user maps
	" Note: txtfmtUsermaplimit option may be set globally or buflocally, with
	" precedence given to buflocal set.
	let bset = exists('b:txtfmtUsermaplimit')
	let gset = exists('g:txtfmtUsermaplimit')
	if bset || gset
		let user_map_limit = bset ? b:txtfmtUsermaplimit : g:txtfmtUsermaplimit
		" Validate the limit set by user
		if user_map_limit !~ '^\s*\([1-9]\d*\|0x\x\+\)\s*$'
			" Invalid format - Warn and abort user-map processing
			echoerr "Aborting user-defined map processing: "
				\.(bset ? 'b:' : 'g:').'txtfmtUsermaplimit set to invalid value: '
				\."`".user_map_limit."'"
			return
		endif
	else
		" Set default
		let user_map_limit = 25
	endif
	" Loop over all possible maps
	let i = 1
	while i <= user_map_limit
		" Determine whether buflocal or global setting exists for this element
		let bset = exists('b:txtfmtUsermap'.i)
		let gset = exists('g:txtfmtUsermap'.i)
		if bset || gset
			" Obtain the buflocal or global element
			let s = bset ? b:txtfmtUsermap{i} : g:txtfmtUsermap{i}
			" Validate and process the user map string
			if s !~ re_usermap
				echoerr 'Ignoring malformed user-defined map specified by '
					\.(bset ? 'b:' : 'g:').'txtfmtUsermap{'.i.'}: '
					\.'help txtfmt-user-map-fmt'
			else
				" Extract the map command and the map lhs/rhs
				let map_cmd = substitute(s, re_usermap, '\1', '')
				let map_lhs = substitute(s, re_usermap, '\2', '')
				let map_rhs = substitute(s, re_usermap, '\3', '')
				" Process non-empty rhs for special sequences
				" NOTE: rhs has extra level of \ and < escaping because of the
				" special embedded <...> sequences
				let map_rhs = s:Translate_user_map_rhs(map_rhs)
				if map_rhs==''
					echoerr "User-defined map #".i." ignored due to error: ".s:err_str
				else
					" Attempt to define the map
					exe map_cmd.' <buffer> '.map_lhs.' '.map_rhs
					" Add corresponding undo action (n or i unmap)
					" TODO - Figure out how to use s:Undef_map and avoid "no
					" such mapping error.
					call s:Add_undo(map_cmd[0].'unmap <buffer> '.map_lhs)
				endif
			endif
		endif
		" Progress to next possible user map
		let i = i + 1
	endwhile
endfu
" >>>
" Function: s:Set_mapwarn() <<<
" Purpose: Set txtfmt_cfg_mapwarn option either from user-supplied
" g:txtfmtMapwarn or to default value. Global var txtfmtMapwarn is a character
" flag option, which may contain the following flags: mMeEcCoO. Although the
" flags may appear in any combination and in any order, there are certain
" combinations that make no sense and should (arguably) result in a warning:
" -m and e should not be used together
" -M and E should not be used together
" Note: If either of the above 2 rules are violated, the last supplied flag
" takes precedence.
" -o should not be used without either e or m
" -O should not be used without either E or M
fu! s:Set_mapwarn()
	" The following buffer-local config option is the output of this function,
	" and must be set before return.
	unlet! b:txtfmt_cfg_mapwarn
	if exists('g:txtfmtMapwarn')
		" Process value supplied by user, storing to buffer-local config
		" variable a valid and normalized set of character flags.
		" Design Decision: Preserve the order of flags being retained rather
		" than arranging them in fiducial order.
		" Note: Existence of l:mapwarn after the loop implies that no error
		" was found with user-supplied option value. (Note that empty string
		" is a valid setting.)
		let mapwarn = ''
		let i = strlen(g:txtfmtMapwarn) - 1
		while i >= 0
			let ch = g:txtfmtMapwarn[i]
			if ch !~ '[mMeEcCoO]'
				" Invalid flag!
				unlet mapwarn
				break
			endif
			" Make sure flags already in mapwarn don't preclude addition of
			" this one.
			if (
				\-1 == stridx(mapwarn, ch) &&
				\(ch != 'm' || -1 == stridx(mapwarn, 'e')) &&
				\(ch != 'e' || -1 == stridx(mapwarn, 'm')) &&
				\(ch != 'M' || -1 == stridx(mapwarn, 'E')) &&
				\(ch != 'E' || -1 == stridx(mapwarn, 'M'))
			\)
				" Prepend the flag to preserve order. (Recall that loop is in
				" reverse order.)
				let mapwarn = ch . mapwarn
			endif
			" Retreat to preceding character flag
			let i = i - 1
		endwhile
		if exists('l:mapwarn')
			" No errors were encountered in the set of mapwarn.
			let b:txtfmt_cfg_mapwarn = mapwarn
		else
			" Warn user that his setting was not valid
			echomsg "Ignoring invalid setting of txtfmtMapwarn: `".g:txtfmtMapwarn
				\."' (:he txtfmtMapwarn)"
		endif
	endif
	" If option was not set by user to a valid value, set to default
	if !exists('b:txtfmt_cfg_mapwarn')
		" Use default
		let b:txtfmt_cfg_mapwarn = 'mMoOcC'
	endif
endfu
" >>>
" Function: s:Do_config() <<<
" Purpose: Set script local variables, taking into account whether user has
" overriden via txtfmt globals.
fu! s:Do_config()
	" set vim 'iskeyword' option <<<
	" Exclude the special tokens from iskeyword option, so that word movement
	" normal commands will work intuitively. (Recall that the delimiters will
	" appear as space characters.)
	" IMPORTANT NOTE: Ideally, we would be able to have the tokens treated
	" just like whitespace, from the standpoint of word and WORD motions;
	" unfortunately, we can't instruct Vim to do this - the best we can do is
	" make them non-keyword, which means they'll be treated like punctation;
	" i.e., word motions will stop on them and on the beginning of subsequent
	" word.
	" IMPORTANT TODO: Vim doesn't allow multi-byte characters to be excluded!
	" Decide whether there's a workaround. For now, don't do this if we're
	" dealing with tokens above 255.
	if (b:txtfmt_cfg_starttok + b:txtfmt_num_formats + b:txtfmt_num_colors - 1 <= 255) 
		let val = '^'.b:txtfmt_cfg_starttok.'-'.
					\(''.(b:txtfmt_cfg_starttok + b:txtfmt_num_formats + b:txtfmt_num_colors - 1))
		exe 'setlocal iskeyword+='.val
		call s:Add_undo('setlocal iskeyword-='.val)
	endif
	" >>>
	" Process txtfmtMapwarn option <<<
	call s:Set_mapwarn()
	" >>>
	" txtfmtUsermaplimit: Max # of user maps that will be checked <<<
	" Allow nonnegative dec, hex, or oct
	" Cannot set from modeline
	if exists('g:txtfmtUsermaplimit') && g:txtfmtUsermaplimit =~ '^\%(0[xX]\)\?[0-9]\+$'
		let s:txtfmtUsermaplimit = g:txtfmtUsermaplimit
	else
		" Set to reasonable default
		let s:txtfmtUsermaplimit = 25
	endif
	" >>>
	" User map definition examples for test <<<

	" Map CTRL-B in insert mode to start and terminate a 'bold' region,
	" leaving the cursor positioned in the region interior, ready to type bold
	" text.
	" Hint: Similar maps might be created for underline and italic
	let g:txtfmtUsermap1 = 'inoremap <C-B> <<i\:fb.f->>'

	" Map CTRL-\f in insert mode to end current format region.
	let g:txtfmtUsermap2 = 'inoremap <C-\>f <<i\:f->>'

	" Map CTRL-\c in insert mode to end current color region.
	let g:txtfmtUsermap3 = 'inoremap <C-\>c <<i\:c->>'

	" Map \t in normal mode to embolden, underline and center (i.e.
	" 'title-ize') the current line
	let g:txtfmtUsermap4 =
	    \'nnoremap <Bslash>t <<n\vI:fbu>><<n\vA:f->>:ce<CR>'

	" Map \cf in normal mode to change all text within the current format
	" region (without deleting the tokens that begin and end the region).
	" Note: Since the default jump-to-token mappings are used in the rhs
	" (rather than the special expansion macros), nmap must be used (rather
	" than nnoremap).
	" Note: The reason the ]f does not cause the format 'end region' token to
	" be deleted is that the operator-pending jump-to-token maps work
	" 'exclusively' when there is no 'v' between operator and motion.
	let g:txtfmtUsermap5 =
	    \'nmap <Bslash>cf [tbfc]f'

	" Same as preceding map but for current color region.
	" Note: This one demonstrates the use of the 'jump-to-token' expansion
	" macros.
	let g:txtfmtUsermap6 =
		\'nnoremap <Bslash>cc <<n[tbc>>c<<o]c>>'

	" Map <LocalLeader>bw in normal mode to embolden the word under the
	" cursor. (The extra complexity is needed to ensure that you can invoke
	" with cursor anywhere on the word.)
	let g:txtfmtUsermap7 =
	    \'nnoremap <LocalLeader>bw :if col(".")!=1 && '
	    \.'getline(".")[col(".")-2]=~"\\w"<Bar>exe "norm!  b"<Bar>'
	    \.'endif<CR><<n\vi:fb>>e<<n\va:f->>b'

	" Map \vf in normal mode to select all of the current format region
	" visually.
	" Note: Unlike the earlier one for changing the current format region,
	" this one doesn't constrain the backwards jump to a 'begin' region token;
	" hence, it will also highlight the text between regions.
	let g:txtfmtUsermap8 =
	    \'nnoremap <LocalLeader>vf <<n[tf>>v<<v]tf>>'

	" Map <C-\>vf in insert mode to do the same in insert mode
	let g:txtfmtUsermap9 =
	    \'inoremap <C-\>vf <<i[tf>><Esc>lv<<v]tf>>'

	" Map <LocalLeader><Space> in normal mode to jump forward to the 3rd
	" 'begin format region' token. (Not overly practical, but demonstrates the
	" use of whitespace in the lhs, as well as the use of the optional count
	" with the jump-to-token expansion macros.)
	let g:txtfmtUsermap10 =
	    \'nnoremap <LocalLeader><Space> <<n3]bf>>'

	" Map <LocalLeader>_ in normal mode to substitute the next 4 characters
	" with a 'bold' format token followed by a 'no format' token, leaving the
	" cursor positioned between the two.
	" (This map is not intended to be useful, but merely to demonstrate the
	" specification of a count with an insert-token expansion macro.)
	let g:txtfmtUsermap11 =
	    \'nnoremap <LocalLeader>_ <<n4\s:fb.f->>'
	" >>>
	" Process any user-defined maps <<<
	call s:Do_user_maps()
	" >>>
endfu
" >>>
call s:Do_config()
" >>>
" Public-interface functions <<<
" Function: g:Txtfmt_GetTokInfo() <<<
" Purpose: Return a string, which gives information about a token at a
" specific line/col. If optional line/col pair is not supplied, cursor
" location will be assumed.
" Inputs:
" [line]	Optional arg #1. Line number of char for which info is desired. If
" 			present, 2nd optional arg (col) must also be supplied.
" [col]		Optional arg #2. Column number of char for which info is desired.
" NOTE: Currently, even when a multi-byte encoding is used, [col] is used as a
" byte offset rather than a character offset.
" TODO: Decide whether I should stop obtaining the character via
" getline()[pos] in favor of a multi-byte safe way.
" Return: Variable format string as follows:
" *** color token ***
" c<clr_num>
" Note: <clr_num> is 1 based.
" *** format token ***
" f<[u][b][i]>
" i.e., the format descriptor in fiducial form
" *** non-token ***
" <ascii_char_code>
" *** invalid char location or wrong # of inputs ***
" <empty string>
" Note: Will show warning to user if inputs were invalid in a syntactical
" sense. (No error msg for nonexistent char position.)
" Interface note: This function is meant to be used by plugin user; e.g., from
" mappings.
" IMPORTANT NOTE: This function now works for multibyte encodings.
fu! Txtfmt_GetTokInfo(...)
	" The output of the if/else will be a variable (ch) whose first character
	" is the token about which information is requested
	if a:0 == 0
		let ch = strpart(getline('.'), col('.') - 1)
	elseif a:0 == 1
		" Makes no sense to supply line but not column!
		echoerr 'Txtfmt_GetTokInfo(): Attempt to specify line without column'
		return ''
	elseif a:0 == 2
		" Check for nonnegative line number
		if a:1 =~ '^[1-9][0-9]*$'
			let line = a:1
		else
			echoerr 'Txtfmt_GetTokInfo(): '.a:1.' is not a valid line #'
			return ''
		endif
		" Check for nonnegative col number
		if a:2 =~ '^[1-9][0-9]*$'
			" Note: Input col is 1-based character index. Use byteidx to convert
			" to byte index for strpart.
			let col0 = byteidx(getline(line), a:2 - 1)
			if col0 == -1
				" Invalid (too large) col position - not error...
				let ch = ''
			else
				let ch = strpart(getline(line), col0)
			endif
		else
			echoerr 'Txtfmt_GetTokInfo(): '.a:2.' is not a valid col #'
			return ''
		endif
	else
		echoerr 'Txtfmt_GetTokInfo(): Wrong # of args - should be 0 or 2'
		return ''
	endif
	" If here, inputs are syntactically valid and ch holds a string whose
	" first character is the one about which info is requested, or empty
	" string if the requested position is invalid.
	if ch == ''
		" Char pos doesn't exist - not an error
		return ''
	endif
	let char_nr = char2nr(ch)
	" Determine the range within which token lies
	if char_nr >= b:txtfmt_fmt_first_tok && char_nr <= b:txtfmt_fmt_last_tok
		" fmt token
		return 'f'.b:ubisrc_fmt{char_nr - b:txtfmt_fmt_first_tok}
	elseif char_nr >= b:txtfmt_clr_first_tok && char_nr <= b:txtfmt_clr_last_tok
		" clr token
		" offset 0 = 'no color', represented by 'c-'
		" offset i = color{i-1}
		let offset = char_nr - b:txtfmt_clr_first_tok
		return 'c'.(offset == 0 ? '-' : ''.(offset-1).'')
	else
		" Not a txtfmt token - just return ascii value
		return ''.char_nr.''
	endif
endfu
" >>>
" Function: g:Txtfmt_GetTokStr() <<<
" Purpose: Translate the input fmt/clr spec list and return the resulting
" token string.
" Inputs:
" s		fmt/clr spec list to be translated
" Return: If input spec list is valid, the corresponding literal token
" sequence is returned as a string; otherwise, empty string is returned and
" error msg is output.
fu! Txtfmt_GetTokStr(s)
	" Make sure this is a txtfmt buffer
	if !exists('b:loaded_txtfmt')
		echoerr "Function Txtfmt_GetTokStr can be used only within a 'txtfmt' buffer"
		return ''
	endif
	" Call script-local function to perform the translation
	let tokstr = s:Translate_fmt_clr_list(a:s)
	if (tokstr == '')
		echoerr "`".a:s."' is not a valid fmt/clr spec list"
		return ''
	else
		" We have a translated fmt/clr spec comprising an offset followed by
		" the actual fmt/clr token sequence. Extract the literal token string
		" and throw the offset away.
		" TODO - Embed this in a special accessor function that may be used
		" elsewhere...
		let tokstr = substitute(tokstr, '\(\-\?[[:digit:]]\+\),\(.*\)', '\2', '')
		return tokstr
	endif
endfu
" >>>
" >>>
" Public-interface commands <<<
com! -buffer ShowTokenMap call <SID>ShowTokenMap()
com! -buffer -nargs=? MoveStartTok call <SID>MoveStartTok(<f-args>)
" >>>
" MAPS: LEVEL 1 & 2 (reconfig): normal/insert mode --> <Plug>... mappings <<<
" Note: <C-R> used (rather than <C-O>) to prevent side-effect when insert-mode
" mapping invoked past end of line (cursor pos off by 1)
" normal mode jump 'to' token mappings <<<
" Align sequence <<<
" AlignCtrl default
" AlignCtrl w=p0P1 ,
" AlignCtrl g ^call
" '<,'>Align
" >>>
call s:Def_map('n', '[bf', '<Plug>TxtfmtBckToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bf', 'b', 0)<CR>")
call s:Def_map('n', ']bf', '<Plug>TxtfmtFwdToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bf', 'f', 0)<CR>")
call s:Def_map('n', '[bc', '<Plug>TxtfmtBckToClrBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bc', 'b', 0)<CR>")
call s:Def_map('n', ']bc', '<Plug>TxtfmtFwdToClrBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bc', 'f', 0)<CR>")
call s:Def_map('n', '[ba', '<Plug>TxtfmtBckToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'ba', 'b', 0)<CR>")
call s:Def_map('n', ']ba', '<Plug>TxtfmtFwdToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'ba', 'f', 0)<CR>")
call s:Def_map('n', '[f' , '<Plug>TxtfmtBckToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'f' , 'b', 0)<CR>")
call s:Def_map('n', ']f' , '<Plug>TxtfmtFwdToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'f' , 'f', 0)<CR>")
call s:Def_map('n', '[c' , '<Plug>TxtfmtBckToClrTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'c' , 'b', 0)<CR>")
call s:Def_map('n', ']c' , '<Plug>TxtfmtFwdToClrTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'c' , 'f', 0)<CR>")
call s:Def_map('n', '[a' , '<Plug>TxtfmtBckToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'a' , 'b', 0)<CR>")
call s:Def_map('n', ']a' , '<Plug>TxtfmtFwdToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'a' , 'f', 0)<CR>")
call s:Def_map('n', '[ef', '<Plug>TxtfmtBckToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ef', 'b', 0)<CR>")
call s:Def_map('n', ']ef', '<Plug>TxtfmtFwdToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ef', 'f', 0)<CR>")
call s:Def_map('n', '[ec', '<Plug>TxtfmtBckToClrEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ec', 'b', 0)<CR>")
call s:Def_map('n', ']ec', '<Plug>TxtfmtFwdToClrEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ec', 'f', 0)<CR>")
call s:Def_map('n', '[ea', '<Plug>TxtfmtBckToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ea', 'b', 0)<CR>")
call s:Def_map('n', ']ea', '<Plug>TxtfmtFwdToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ea', 'f', 0)<CR>")
" >>>
" visual mode jump 'to' token mappings <<<
call s:Def_map('v', '[bf', '<Plug>TxtfmtBckToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bf', 'b', 0)<CR>")
call s:Def_map('v', ']bf', '<Plug>TxtfmtFwdToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bf', 'f', 0)<CR>")
call s:Def_map('v', '[bc', '<Plug>TxtfmtBckToClrBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bc', 'b', 0)<CR>")
call s:Def_map('v', ']bc', '<Plug>TxtfmtFwdToClrBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bc', 'f', 0)<CR>")
call s:Def_map('v', '[ba', '<Plug>TxtfmtBckToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'ba', 'b', 0)<CR>")
call s:Def_map('v', ']ba', '<Plug>TxtfmtFwdToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'ba', 'f', 0)<CR>")
call s:Def_map('v', '[f' , '<Plug>TxtfmtBckToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'f' , 'b', 0)<CR>")
call s:Def_map('v', ']f' , '<Plug>TxtfmtFwdToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'f' , 'f', 0)<CR>")
call s:Def_map('v', '[c' , '<Plug>TxtfmtBckToClrTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'c' , 'b', 0)<CR>")
call s:Def_map('v', ']c' , '<Plug>TxtfmtFwdToClrTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'c' , 'f', 0)<CR>")
call s:Def_map('v', '[a' , '<Plug>TxtfmtBckToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'a' , 'b', 0)<CR>")
call s:Def_map('v', ']a' , '<Plug>TxtfmtFwdToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'a' , 'f', 0)<CR>")
call s:Def_map('v', '[ef', '<Plug>TxtfmtBckToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ef', 'b', 0)<CR>")
call s:Def_map('v', ']ef', '<Plug>TxtfmtFwdToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ef', 'f', 0)<CR>")
call s:Def_map('v', '[ec', '<Plug>TxtfmtBckToClrEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ec', 'b', 0)<CR>")
call s:Def_map('v', ']ec', '<Plug>TxtfmtFwdToClrEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ec', 'f', 0)<CR>")
call s:Def_map('v', '[ea', '<Plug>TxtfmtBckToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ea', 'b', 0)<CR>")
call s:Def_map('v', ']ea', '<Plug>TxtfmtFwdToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ea', 'f', 0)<CR>")
" >>>
" operator-pending mode jump 'to' token mappings <<<
" Note: 'v' can be used with these to toggle inclusive/exclusive
call s:Def_map('o', '[bf', '<Plug>TxtfmtBckToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bf', 'b', 0)<CR>")
call s:Def_map('o', ']bf', '<Plug>TxtfmtFwdToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bf', 'f', 0)<CR>")
call s:Def_map('o', '[bc', '<Plug>TxtfmtBckToClrBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bc', 'b', 0)<CR>")
call s:Def_map('o', ']bc', '<Plug>TxtfmtFwdToClrBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bc', 'f', 0)<CR>")
call s:Def_map('o', '[ba', '<Plug>TxtfmtBckToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'ba', 'b', 0)<CR>")
call s:Def_map('o', ']ba', '<Plug>TxtfmtFwdToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'ba', 'f', 0)<CR>")
call s:Def_map('o', '[f' , '<Plug>TxtfmtBckToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'f' , 'b', 0)<CR>")
call s:Def_map('o', ']f' , '<Plug>TxtfmtFwdToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'f' , 'f', 0)<CR>")
call s:Def_map('o', '[c' , '<Plug>TxtfmtBckToClrTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'c' , 'b', 0)<CR>")
call s:Def_map('o', ']c' , '<Plug>TxtfmtFwdToClrTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'c' , 'f', 0)<CR>")
call s:Def_map('o', '[a' , '<Plug>TxtfmtBckToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'a' , 'b', 0)<CR>")
call s:Def_map('o', ']a' , '<Plug>TxtfmtFwdToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'a' , 'f', 0)<CR>")
call s:Def_map('o', '[ef', '<Plug>TxtfmtBckToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ef', 'b', 0)<CR>")
call s:Def_map('o', ']ef', '<Plug>TxtfmtFwdToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ef', 'f', 0)<CR>")
call s:Def_map('o', '[ec', '<Plug>TxtfmtBckToClrEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ec', 'b', 0)<CR>")
call s:Def_map('o', ']ec', '<Plug>TxtfmtFwdToClrEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ec', 'f', 0)<CR>")
call s:Def_map('o', '[ea', '<Plug>TxtfmtBckToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ea', 'b', 0)<CR>")
call s:Def_map('o', ']ea', '<Plug>TxtfmtFwdToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ea', 'f', 0)<CR>")
" >>>
" normal mode jump 'till' token mappings <<<
call s:Def_map('n', '[tbf', '<Plug>TxtfmtBckTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bf', 'b', 1)<CR>")
call s:Def_map('n', ']tbf', '<Plug>TxtfmtFwdTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bf', 'f', 1)<CR>")
call s:Def_map('n', '[tbc', '<Plug>TxtfmtBckTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bc', 'b', 1)<CR>")
call s:Def_map('n', ']tbc', '<Plug>TxtfmtFwdTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bc', 'f', 1)<CR>")
call s:Def_map('n', '[tba', '<Plug>TxtfmtBckTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'ba', 'b', 1)<CR>")
call s:Def_map('n', ']tba', '<Plug>TxtfmtFwdTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'ba', 'f', 1)<CR>")
call s:Def_map('n', '[tf' , '<Plug>TxtfmtBckTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'f' , 'b', 1)<CR>")
call s:Def_map('n', ']tf' , '<Plug>TxtfmtFwdTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'f' , 'f', 1)<CR>")
call s:Def_map('n', '[tc' , '<Plug>TxtfmtBckTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'c' , 'b', 1)<CR>")
call s:Def_map('n', ']tc' , '<Plug>TxtfmtFwdTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'c' , 'f', 1)<CR>")
call s:Def_map('n', '[ta' , '<Plug>TxtfmtBckTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'a' , 'b', 1)<CR>")
call s:Def_map('n', ']ta' , '<Plug>TxtfmtFwdTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'a' , 'f', 1)<CR>")
call s:Def_map('n', '[tef', '<Plug>TxtfmtBckTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ef', 'b', 1)<CR>")
call s:Def_map('n', ']tef', '<Plug>TxtfmtFwdTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ef', 'f', 1)<CR>")
call s:Def_map('n', '[tec', '<Plug>TxtfmtBckTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ec', 'b', 1)<CR>")
call s:Def_map('n', ']tec', '<Plug>TxtfmtFwdTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ec', 'f', 1)<CR>")
call s:Def_map('n', '[tea', '<Plug>TxtfmtBckTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ea', 'b', 1)<CR>")
call s:Def_map('n', ']tea', '<Plug>TxtfmtFwdTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ea', 'f', 1)<CR>")
" >>>
" visual mode jump 'till' token mappings <<<
call s:Def_map('v', '[tbf', '<Plug>TxtfmtBckTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bf', 'b', 1)<CR>")
call s:Def_map('v', ']tbf', '<Plug>TxtfmtFwdTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bf', 'f', 1)<CR>")
call s:Def_map('v', '[tbc', '<Plug>TxtfmtBckTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bc', 'b', 1)<CR>")
call s:Def_map('v', ']tbc', '<Plug>TxtfmtFwdTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bc', 'f', 1)<CR>")
call s:Def_map('v', '[tba', '<Plug>TxtfmtBckTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'ba', 'b', 1)<CR>")
call s:Def_map('v', ']tba', '<Plug>TxtfmtFwdTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'ba', 'f', 1)<CR>")
call s:Def_map('v', '[tf' , '<Plug>TxtfmtBckTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'f' , 'b', 1)<CR>")
call s:Def_map('v', ']tf' , '<Plug>TxtfmtFwdTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'f' , 'f', 1)<CR>")
call s:Def_map('v', '[tc' , '<Plug>TxtfmtBckTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'c' , 'b', 1)<CR>")
call s:Def_map('v', ']tc' , '<Plug>TxtfmtFwdTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'c' , 'f', 1)<CR>")
call s:Def_map('v', '[ta' , '<Plug>TxtfmtBckTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'a' , 'b', 1)<CR>")
call s:Def_map('v', ']ta' , '<Plug>TxtfmtFwdTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'a' , 'f', 1)<CR>")
call s:Def_map('v', '[tef', '<Plug>TxtfmtBckTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ef', 'b', 1)<CR>")
call s:Def_map('v', ']tef', '<Plug>TxtfmtFwdTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ef', 'f', 1)<CR>")
call s:Def_map('v', '[tec', '<Plug>TxtfmtBckTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ec', 'b', 1)<CR>")
call s:Def_map('v', ']tec', '<Plug>TxtfmtFwdTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ec', 'f', 1)<CR>")
call s:Def_map('v', '[tea', '<Plug>TxtfmtBckTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ea', 'b', 1)<CR>")
call s:Def_map('v', ']tea', '<Plug>TxtfmtFwdTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ea', 'f', 1)<CR>")
" >>>
" operator-pending mode jump 'till' token mappings <<<
" Note: 'v' can be used with these to toggle inclusive/exclusive
call s:Def_map('o', '[tbf', '<Plug>TxtfmtBckTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bf', 'b', 1)<CR>")
call s:Def_map('o', ']tbf', '<Plug>TxtfmtFwdTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bf', 'f', 1)<CR>")
call s:Def_map('o', '[tbc', '<Plug>TxtfmtBckTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bc', 'b', 1)<CR>")
call s:Def_map('o', ']tbc', '<Plug>TxtfmtFwdTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bc', 'f', 1)<CR>")
call s:Def_map('o', '[tba', '<Plug>TxtfmtBckTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'ba', 'b', 1)<CR>")
call s:Def_map('o', ']tba', '<Plug>TxtfmtFwdTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'ba', 'f', 1)<CR>")
call s:Def_map('o', '[tf' , '<Plug>TxtfmtBckTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'f' , 'b', 1)<CR>")
call s:Def_map('o', ']tf' , '<Plug>TxtfmtFwdTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'f' , 'f', 1)<CR>")
call s:Def_map('o', '[tc' , '<Plug>TxtfmtBckTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'c' , 'b', 1)<CR>")
call s:Def_map('o', ']tc' , '<Plug>TxtfmtFwdTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'c' , 'f', 1)<CR>")
call s:Def_map('o', '[ta' , '<Plug>TxtfmtBckTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'a' , 'b', 1)<CR>")
call s:Def_map('o', ']ta' , '<Plug>TxtfmtFwdTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'a' , 'f', 1)<CR>")
call s:Def_map('o', '[t$f', '<Plug>TxtfmtBckTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('o', '$f', 'b', 1)<CR>")
call s:Def_map('o', ']t$f', '<Plug>TxtfmtFwdTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('o', '$f', 'f', 1)<CR>")
call s:Def_map('o', '[t$c', '<Plug>TxtfmtBckTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('o', '$c', 'b', 1)<CR>")
call s:Def_map('o', ']t$c', '<Plug>TxtfmtFwdTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('o', '$c', 'f', 1)<CR>")
call s:Def_map('o', '[t$a', '<Plug>TxtfmtBckTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('o', '$a', 'b', 1)<CR>")
call s:Def_map('o', ']t$a', '<Plug>TxtfmtFwdTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('o', '$a', 'f', 1)<CR>")
" >>>
" normal mode insert token mappings <<<
" These mappings may be used from normal mode to insert special tokens.
" Note: The first set leaves cursor in insert mode, and is probably the most
" useful. The second set enters insert mode to do the insert and puts cursor
" at correct offset prior to returning to normal mode. Works just like
" inserting the token, then hitting <Esc>.
" TODO - This one is redundant to the \vi one - use the latter instead for
" notational consistency?
call s:Def_map('n', '<C-\><C-\>', '<Plug>TxtfmtInsertTok_n',
			\":<C-U>call <SID>Insert_tokstr('', 'i', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
" Start in normal / End in insert
call s:Def_map('n', '<LocalLeader>i', '<Plug>TxtfmtInsertTok_i',
			\":<C-U>call <SID>Insert_tokstr('', 'i', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>I', '<Plug>TxtfmtInsertTok_I',
			\":<C-U>call <SID>Insert_tokstr('', 'I', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>a', '<Plug>TxtfmtInsertTok_a',
			\":<C-U>call <SID>Insert_tokstr('', 'a', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>A', '<Plug>TxtfmtInsertTok_A',
			\":<C-U>call <SID>Insert_tokstr('', 'A', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>o', '<Plug>TxtfmtInsertTok_o',
			\":<C-U>call <SID>Insert_tokstr('', 'o', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>O', '<Plug>TxtfmtInsertTok_O',
			\":<C-U>call <SID>Insert_tokstr('', 'O', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>s', '<Plug>TxtfmtInsertTok_s',
			\":<C-U>call <SID>Insert_tokstr('', 's', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
" Start in normal / End in normal
call s:Def_map('n', '<LocalLeader>vi', '<Plug>TxtfmtInsertTok_vi',
			\":<C-U>call <SID>Insert_tokstr('', 'i', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>vI', '<Plug>TxtfmtInsertTok_vI',
			\":<C-U>call <SID>Insert_tokstr('', 'I', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>va', '<Plug>TxtfmtInsertTok_va',
			\":<C-U>call <SID>Insert_tokstr('', 'a', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>vA', '<Plug>TxtfmtInsertTok_vA',
			\":<C-U>call <SID>Insert_tokstr('', 'A', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>vo', '<Plug>TxtfmtInsertTok_vo',
			\":<C-U>call <SID>Insert_tokstr('', 'o', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>vO', '<Plug>TxtfmtInsertTok_vO',
			\":<C-U>call <SID>Insert_tokstr('', 'O', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map('n', '<LocalLeader>vs', '<Plug>TxtfmtInsertTok_vs',
			\":<C-U>call <SID>Insert_tokstr('', 's', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
" >>>
" insert mode insert token mappings <<<
" NOTE: Default is to use something that wouldn't be typed as text for the
" insert mode map. User may wish to remap this one to a Function key or
" something else entirely. I find <C-\><C-\> very easy to type...
call s:Def_map('i', '<C-\><C-\>', '<Plug>TxtfmtInsertTok_i',
			\"<C-R>=<SID>Insert_tokstr('', 'i', 0, 0)<CR>"
			\."<C-R>=<SID>Adjust_cursor()<CR>")
" >>>
" NOTES <<<
" -enterinsert default is 'i'
" -mode default is 'ni'
" -<C-0> can't be used in insert-mode mapping for some reason...
" >>>
" TODO <<<
" -Convert ASCII only pattern character classes to ones that will work with
" multi-byte chars
" -Add commands/functions for detecting and altering the range of character
"  codes used for txtfmt tokens.
" -Use syntax clusters instead of the double definition trickery I used when I
"  didn't know about syntax clusters.
" >>>
" >>>
" Restore compatibility options <<<
" Restore compatibility options to what they were
let &cpo = s:save_cpo
" >>>
	" vim: sw=4 ts=4 foldmethod=marker foldmarker=<<<,>>> :
