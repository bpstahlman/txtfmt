" Txtfmt: Set of Vim plugins (syntax, ftplugin, plugin) for creating and
" displaying formatted text with Vim.
" File: This is the txtfmt ftplugin file, which contains mappings and
" functions for working with the txtfmt color/formatting tokens.
" Creation:	2004 Nov 06
" Last Change: 2023 May 06
" Maintainer:	Brett Pershing Stahlman <brettstahlman@gmail.net>
" License:	This file is placed in the public domain.

" Script-level constants <<<
" Let the common code know whether this is syntax file or ftplugin
" TODO: This doesn't appear to be used any longer. Consider removal.
let s:script_name = 'ftplugin'

" Binary progression: ubisrc
let s:ubisrc_mask = {'u': 1, 'b': 2, 'i': 4, 's': 8, 'r': 16, 'c': 32}
let s:cfg_color_name_compat = 0

" Number of bytes backwards to look when attempting sync for vmap operation.
let s:SYNC_DIST_BYTES = 2500

" Prior to v3.0, a color name was permitted to contain whitespace; disallowing
" it permits us to use whitespace, rather than comma, to separate terms in
" fmt/clr spec list. It's unlikely anyone is relying upon this, but provide an
" option just in case.
" TODO: Need to go back and make Translate_fmt_clr_list handle this.
" TODO: Is this the best place for this?
let s:cfg_color_name_compat = 0

" Define the default shortcut maps.
" Note: Would prefer '\', ',' and '_' as leaders, but \i conflicts with a manual
" map.
" TODO: Also considered use of Meta keys, but I think this may be frowned on in Vim...
let s:txtfmtShortcuts = [
			\ '-f f-',
			\ '-c c-',
			\ '-k k-',
			\ '-- f- c- k-',
			\ ',b fb',
			\ ',i fi',
			\ ',u fu',
			\ '_r cr',
			\ '_g cg',
			\ '_b cb',
			\ '_R kr',
			\ '_G kg',
			\ '_B kb',
\]
" >>>
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
" Note: The cpo options generally affect only code outside functions; however,
" the line continuation option takes effect when functions are parsed, not run.
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
" Function: s:Msg() <<<
" Echo the messages provided as args to the user (and the message history), each
" on a separate line, with highlighting appropriate to the message level.
" Note: echomsg doesn't support embedded newlines; hence, the need for a list.
fu! s:Msg(level, ...)
	if a:level == 'error'
		echohl ErrorMsg
	elseif a:level == 'warning'
		echohl WarningMsg
	elseif a:level == 'info'
	else
		echohl None
	endif
	try
		for line in a:000
			echomsg line
		endfor
	finally
		echohl None
	endtry
endfu
" >>>
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
" Function: s:Get_active_rgns() <<<
fu! s:Get_active_rgns()
	let bgc_active = b:txtfmt_cfg_bgcolor && b:txtfmt_cfg_numbgcolors > 0
	let clr_active = b:txtfmt_cfg_numfgcolors > 0
	return extend(extend(['fmt'], clr_active ? ['clr'] : []), bgc_active ? ['bgc'] : [])
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
		let last_col = mode =~ 'i' ? col('$') : col('$') - 1
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
			" AUTO_MAPS TODO: Revisit this: I permit freer format in auto map
			" specs.
			let tokstr = TxtfmtUtil_strip(tokstr)
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
	if modestr !~ '^[niR]$'
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
		"mode!
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
" Function: s:Delete_cur_char() <<<
" Purpose: Delete the character at the cursor position
" Return: Number of bytes deleted
" Assumption: Caller saves and restores @z, which is used by this function
fu! s:Delete_cur_char()
	" Obtain the character about to be deleted so we can determine its length
	normal! "zyl
	" Delete into black hole
	" TODO: Decide whether to use the small delete register, in which case we
	" wouldn't need the step above.
	normal! "_x
	" Return number of bytes removed
	return strlen(@z)
endfu
" >>>
" Function: s:Delete_region_text_old() <<<
" Purpose: Delete all text in specified region.
" Inputs:
" pos_beg: <[ln, col] of pos at beg of region to delete>
" pos_end: <[ln, col] of pos just past end of region to delete>
" inc:  <both-inclusive>|[<beg-inclusive>, <end-inclusive>]
" Return: Number of bytes deleted
" Tested: 22Nov2015
" Rationale: The reason I chose to do this analytically (as opposed to simply
" using a delete motion with \%l and \%c assertions in target pattern, saving
" deleted text to a register, and running strlen on the register) is that the
" analytic approach doesn't require saving the (potentially large) deleted text
" (for calculation of # of bytes deleted). Technically, the delete operator
" approach doesn't require this either, but it's certainly the easiest way, and
" if we're going to calculate the length of deleted text more analytically, the
" delete operator provides no real advantage over the analytic approach...
" TODO: If the length of deleted text isn't required (and it no longer appears
" to be), simplify this function!!!!!
fu! s:Delete_region_text_old(pos_beg, pos_end, inc)
	let inc = type(a:inc) == 3 ? a:inc : [a:inc, a:inc]
	let [line_beg, col_beg, line_end, col_end] =
		\[a:pos_beg[0], a:pos_beg[1], a:pos_end[0], a:pos_end[1]]

	" Begin
	let linetext_beg = getline(line_beg)
	if !inc[0]
		let col_beg += byteidx(strpart(linetext_beg, col_beg - 1), 1)
	endif
	let keeptext_beg = strpart(linetext_beg, 0, col_beg - 1)

	" End
	let keeptext_end = line_beg == line_end
		\? linetext_beg[col_end - 1 : ]
		\: getline(line_end)[col_end - 1 : ]
	if inc[1]
		" Make sure the final char is included in deletion.
		let offset = byteidx(keeptext_end, 1)
		let col_end += offset
		let keeptext_end = keeptext_end[offset : ]
	endif

	" Calculate # of bytes being deleted (before making any buffer mods).
	if line_beg == line_end
		" Recall that col_beg and col_end now point to first char to be deleted
		" and char *beyond* last char to be deleted, respectively.
		let del_bytes = col_end - col_beg
	else
		" Initialize to # of bytes between *start* of line_beg and *start* of
		" line_end (includes newlines).
		let del_bytes = line2byte(line_end) - line2byte(line_beg)
		" Subtract bytes kept at head of beg line
		let del_bytes -= col_beg - 1
		" Add bytes removed at head of end line
		let del_bytes += col_end - 1
	endif

	" Modify the buffer.
	call setline(line_beg, keeptext_beg . keeptext_end)
	if line_end - line_beg
		" Remove lines to bit bucket.
		silent exe (line_beg + 1) . "," . line_end . "d _"
	endif

	" Return number of bytes removed
	return del_bytes
endfu
" >>>
" Function: s:Restore_visual_mode() <<<
" Purpose: Visually select the range indicated by the input positions, using
" the visual mode command returned by visualmode().
" Inputs:
" Return:
" Error:
" Assumptions:
" Vim Idiosyncrasy: The gv command remembers which end of the selection had
" the cursor; however, if you explicitly change one of the vsel endpoints with
" setpos, Vim automatically sets the other (not explicitly set vsel endpoint)
" to the old vsel's cursor pos.
" TODO: I need to restore cursor pos just as Vim does.
fu! s:Restore_visual_mode(pos_beg, pos_end, deactivate)
	let poss = [a:pos_beg, a:pos_end]
	" Start with the old selection...
	exe 'normal! ' . visualmode()
	" Possible TODO: Could make this an Adjust_visual_mode function taking offset, or leave as Restore, but take optional offsets.
	let i = getpos(".") == getpos("'<") ? 1 : 0
	" ...Adjust the opposite end first.
	normal! o
	call cursor(poss[i])
	" ...and finish with the end where cursor should remain.
	normal! o
	call cursor(poss[(i + 1) % 2])
	if a:deactivate
		" TODO: Consider using v or V (as function of visualmode() return) to de-activate.
		exe "normal! \<Esc>"
	endif
endfu
" >>>
" Function: s:Can_delete_tok() <<<
" Purpose: Return true if and only if the token under the cursor can be
" deleted. A return of false indicates that the token should be replaced with
" a single space.
" Logic: If 'keepsep' is false, we can always delete. Otherwise, we can delete
" a token whenever doing so does not result in the joining of 2
" non-whitespace, non-token chars that were previously separated by apparent
" whitespace. As a special case, I allow deletion of a token just prior to
" what appears to be the punctuation at the end of a sentence or phrase.
" Assumption: Cursor is positioned on a token. (We don't check.)
" Assumption: Caller saves and restores @z, which is used by this function
" TODO !!! UNUSED !!! REMOVE !!!
fu! s:Can_delete_tok()
	" TODO: Eventually, make this (or something similar) an option
	let b:txtfmt_cfg_keepsep = 1
	" Initialize return value to false
	let ret_val = 0
	if !b:txtfmt_cfg_keepsep
		" If tokens are not used as space, we can always delete
		let ret_val = 1
	else
		" Cache cursor column position
		let tok_col = col('.')
		if tok_col == 1 || tok_col + strlen(s:Get_char()) >= col('$')
			" We can always delete a token at the beginning or end of the line
			let ret_val = 1
		else
			" Assumption: There's at least one char both before and after
			" token under char. (This simplifies searches.)
			" Design Decision: Inactive color is not treated as token, since
			" it will not appear as whitespace.
			" TODO: Decide whether this is what I want...
			let tok_line = line('.')
			" Create line/col constraint regex that can be used both for
			" forward and backward search
			let re_lcc = '\%' . tok_line . 'l\%' . tok_col . 'c'
			" VIM BUG: If backward search is performed from a multi-byte char
			" just after a space char, the backward search will not find the
			" space char.
			" Workaround: Move forward one char position before performing
			" backward search. (I've verified that this works even when the
			" char just after the multi-byte char is also multi-byte.)
			" Assumption: We've already verified that we're not at the end of
			" the line
			" Note: The subsequent if and else are responsible for restoring
			" cursor position
			normal! l
			if search('\%(' . b:txtfmt_re_any_tok . '\|\s\)' . re_lcc, 'bW', tok_line)
				" There's whitespace or token space to the left of cursor
				" position
				let ret_val = 1
				" Restore cursor pos after successful search
				normal! l
			else
				" Move back to cursor pos
				normal! h
				" Define a regex corresponding *roughly* to Vim's concept of a
				" sentence when cpoptions does not contain 'J'
				" :help sentence
				" Variation: I permit 1 (period) or 3 (ellipsis) dots, and any
				" combination of bangs and question marks.
				" Rationale: A sentence is sometimes ended with ..., !!!, ???
				" !?!?, etc... Note that such sentences would be recognized by
				" Vim, which doesn't care what comes before the final [.!?].
				let re_punct = '\%(\.\|\.\{3}\|[!?]\+\)'
				let re_eos = re_punct . '[])"'']*\%($\|\s\)'
				" Additionally, define regex corresponding to *my* concept of
				" a 'clause' (e.g., something ending with comma, semicolon or
				" colon)
				" Note: The `\%l \%c .' within the lookbehind assertion
				" ensures that the pattern must match just after cursor pos
				let re_eoc = '[,;:]\%($\|\s\)'
				if search(
					\ '\%(' . re_lcc . '.\)\@<=\%(' . b:txtfmt_re_any_tok
					\ . '\|\s\|' . re_eos . '\|' . re_eoc . '\)', 'W', tok_line)
					let ret_val = 1
					" Restore cursor pos after successful search
					normal! h
				endif
			endif
		endif
	endif
	return ret_val
endfu
" >>>
" Shift functions <<<
" Remove tokens within leading indent in each line in input range.
" Return: list indicating what was removed:
" [
"   {'lnum': lnum, 'toks': [{tokstr}...]}]
" Note: Only lines with tokens removed are represented in list.
" Precondition: Can't get here in 'noconceal' case.
fu! s:Remove_toks_in_li(l1, l2)
	let ret = []
	" Short-circuit on li=none (in case caller doesn't check).
	if b:txtfmt_cfg_leadingindent == 'none'
		return ret
	endif
	for lnum in range(a:l1, a:l2)
		" Find extents of leading indent
		" Design Decision: Slurp in any toks after leadingindent.
		" Rationale: 1) Allows them to be repositioned to BOL, and 2) more
		" importantly, allows any whitespace following the tok, which isn't
		" included in leadingindent, to take part in shift.
		" Special Case: Leading indent matches whitespace only, so we make its
		" regex optional to ensure that toks at BOL are removed.
		let re_li = '^\%(' . b:txtfmt_re_leading_indent . '\)\?\%(' . b:txtfmt_re_any_tok . '\)*'
		let line_str = getline(lnum)
		let [li_str, li_strlen, skip_bytes] = ['', 0, 0]
		let li_end = matchend(line_str, re_li)
		if li_end >= 0
			" Grab the leading indent.
			let li_str = strpart(line_str, 0, li_end)
		else
			let li_end = 0
		endif
		" Don't add list element if no leading indent (or toks just past).
		if !li_end
			continue
		endif
		" Process all toks in the leading indent, rebuilding the line without
		" them as we go...
		let [toks, uniq] = [[], {}]
		let [i, s] = [0, '']
		let m = ['', -1, 0]
		" Note: If this point is reached, we're guaranteed to enter the while at
		" least once.
		while m[2] >= 0 && m[2] < li_end
			let m = matchstrpos(li_str, b:txtfmt_re_any_tok, m[2])
			if m[1] >= 0
				" Found a token.
				" Note: Uniquifying the list of tokens ensures that recombining
				" them after the shift can't produce escape-escapee pairs,
				" regardless of the 'escape' setting.
				if !has_key(uniq, m[0])
					let uniq[m[0]] = 1
					" Add the token to the list.
					call add(toks, m[0])
				endif
				" Accumulate up to tok.
				let s .= strpart(li_str, i, m[1] - i)
				" Note: Replacement considered only for toks in true leading
				" indent.
				" TODO: Eventually, remove this if since it will never be
				" entered now that 'li' is forced to 'none' for 'noconceal'
				" case.
				if !b:txtfmt_cfg_conceal && m[2] < li_end
					" Replace tok with space for alignment.
					" Design Decision Needed: Should we use strdisplaywidth() to
					" determine screen width of token, or simply assume all toks
					" are 1 screen char wide?
					let s .= ' '
				endif
				" Skip over the token
				let i = m[2]
			else
				" Accumulate remainder.
				let s .= li_str[i:]
			endif
		endwhile
		if empty(toks)
			" Don't add list element if no toks in leading indent.
			continue
		endif
		" Accumulate object representing this line into return list.
		call add(ret, {'lnum': lnum, 'toks': toks})
		" Modify the line in the buffer.
		call setline(lnum, s . line_str[li_end + skip_bytes:])
	endfor
	return ret
endfu

fu! s:Restore_toks_after_shift(toks)
	" Short-circuit on li=none (in case caller doesn't check).
	if b:txtfmt_cfg_leadingindent == 'none'
		return
	endif
	for t in a:toks
		" Cache line string and tok list.
		let line_str = getline(t['lnum'])
		let toks = t['toks']
		" Set li to 0-based index of first char we're keeping of original line:
		" i.e., first char not replaced by token(s).
		if b:txtfmt_cfg_conceal
			" Since toks are zero-width, there's no reason to replace any
			" whitespace.
			let li = 0
		else " noconceal
			" TODO: Eventually, remove this else since it will never be entered
			" now that 'li' is forced to 'none' for 'noconceal' case.
			" Toks are not zero-width: "Hide" them in existing whitespace if
			" possible.
			" Assumption: Getting here implies li=white
			" Find the end of the run of whitespace that can be replaced with
			" tokens.
			" Note: When the replacement is performed, it must use *all* tokens,
			" even if it means widening the leading indent because the leading
			" whitespace isn't sufficient to absorb all the tokens.
			let li_end = max([matchend(line_str, '^\s*'), 0])
			let ti_end = len(toks)
			let [li, ti] = [0, 0]
			" Loop until we reach end of either whitespace or tokens.
			while li < li_end && ti < ti_end
				if line_str[li] == ' '
					let ti += 1
				else
					" How many tokens can we fit in this TAB?
					" Design Decision Needed: Should we use strdisplaywidth() on
					" individual toks rather than assuming single screen char
					" width? Would need to be done in the tok removal function
					" too.
					" Note: In theory, ti should be incremented by
					" min(ti_end-ti, &ts), but it's safe to increment past
					" ti_end, so don't bother with min().
					" Note: Since we've just performed some sort of shift, tabs
					" are stacked at head of line, which means we don't really
					" need to use strdisplaywidth() to get tab's screen width.
					let ti += &ts
				endif
				" Advance to next tab or char.
				let li += 1
			endwhile
		endif
		" Prepend *all* tokens to the original line, minus any leading
		" whitespace marked for replacement.
		call setline(t['lnum'], join(toks, '') . line_str[li : ])
	endfor
endfu

" Note: Although we're invoked from insert-mode map, we'll be in normal mode by
" the time the function is called. This function is responsible for restoring
" cursor position in a sensible manner, and returning to insert mode.
fu! s:Indent(dedent)
	" Undo the backwards char movement inherent in transition from insert to
	" normal mode.
	" Rationale: Allows us to save/restore this position and have subsequent
	" :startinsert put cursor back where it was.
	" Alternative: Could use CTRL-O prior to the <Esc> in imap.
	normal! `^
	let cur = getpos('.')
	" Note: This regex is not the same as the more specific 'leadingindent'.
	let re_leading_ws_or_tok = '^\%(\s\|' . b:txtfmt_re_any_tok . '\)*'
	" Is portion of line preceding cursor nothing but whitespace and toks?
	let in_ws = strpart(getline('.'), 0, cur[2] - 1) =~ re_leading_ws_or_tok . '$'
	if !in_ws
		" Cache distance from cursor to EOL (which should be unaffected by
		" indent/dedent)
		let edist = col('$') - cur[2]
	endif
	" Remove toks from leading indent.
	let toks = s:Remove_toks_in_li(cur[1], cur[1])
	" Let Vim's native CTRL-T/D perform the requested indent/dedent.
	" Design Decision: CTRL-T and CTRL-D work most predictably (and sanely) when
	" executed from end of line (after any leading whitespace).
	exe 'norm! A' . (a:dedent ? "\<C-D>" : "\<C-T>")
	call s:Restore_toks_after_shift(toks)
	" Restore cursor position, according to following logic:
	" If mapping was executed from (possibly empty) leading whitespace or tokens
	" (not necessarily leading indent), position on first non-whitespace/token;
	" else, attempt to preserve position by positioning cursor at its original
	" distance from EOL.
	if in_ws
		" Note: Regex uses * quantifier; hence, failure (-1) not possible.
		let cur[2] = matchend(getline('.'), re_leading_ws_or_tok) + 1
	else
		" Note: Cursor was positioned past a char that couldn't have been
		" removed by indent/dedent.
		let cur[2] = col('$') - edist
	endif
	call cursor(cur[1], cur[2])
	" Return to insert mode on function return.
	startinsert
endfu

fu! s:Lineshift(mode, dedent)
	if a:mode == 'o'
		let [l1, l2] = [line("'["), line("']")]
	elseif a:mode == 'n'
		" normal
		let [l1, l2] = [line("."), line(".") + v:count1 - 1]
	elseif a:mode[0] == 'c'
		" command
		" TODO: Perhaps implement a Lsh and Rsh command (to replace :< and :>
		" commands, which can't be overridden)?
	elseif a:mode =~ '[vV]'
		" visual
		let [l1, l2] = [line("'<"), line("'>")]
	endif

	" Remove toks from leading indent.
	let toks = s:Remove_toks_in_li(l1, l2)
	" Perform the indent in mode-appropriate manner, to ensure that cursor is
	" left in correct location.
	if a:mode == 'o'
		exe printf("norm! %s%s",
			\ a:dedent ? "<" : ">",
			\ line(".") == l1 ? "']" : "'[")
	elseif a:mode == 'n'
		exe printf("norm! %d%s", v:count1, a:dedent ? "<<" : ">>")
	elseif a:mode[0] == 'c'
		" TODO: See earlier note.
	elseif a:mode =~ '[vV]'
		exe printf("norm! gv%s", a:dedent ? "<" : ">")
	else
		echoerr "Invalid mode for Txtfmt lineshift: " . a:mode
	endif

	" Prepend uniquified, ordered sequences of removed tokens to lines.
	call s:Restore_toks_after_shift(toks)
	" Note: Vim leaves cursor on first non-whitespace char (possibly token).
	" In some cases (e.g., 'noconceal' and li=white), this would leave cursor
	" within what appears to be leading whitespace.
	" Solution: Place cursor just past all leading whitespace/tokens.
	let col = matchend(getline('.'), '^\%(\s\|' . b:txtfmt_re_any_tok . '\)*') + 1
	call cursor(0, col)
endfu

fu! s:Shift_left_operator(mode)
	call s:Lineshift('o', 1)
endfu
fu! s:Shift_right_operator(mode)
	call s:Lineshift('o', 0)
endfu

" Txtfmt "smart" (Txtfmt-aware) :retab command
fu! s:Retab(l1, l2, bang, ts)
	" Remove toks from leading indent.
	let toks = s:Remove_toks_in_li(a:l1, a:l2)
	" Perform the requested :retab.
	exe a:l1 . ',' . a:l2 . 'retab' . a:bang . ' ' . a:ts
	" Prepend uniquified, ordered sequences of removed tokens to lines.
	call s:Restore_toks_after_shift(toks)
endfu

" >>>
" Function: s:Is_cursor_on_char() <<<
" TODO: Perhaps move elsewhere...
fu! s:Is_cursor_on_char()
	" Note: The following test for something under the cursor works well even
	" when 'virtualedit' is active.
	if col('.') == col('$')
		" Nothing under the cursor
		return 0
	else
		return 1
	endif
endfu
" >>>
" Function: s:Is_active_color_index() <<<
" TODO: Perhaps move elsewhere...
fu! s:Is_active_color_index(typ, idx)
	" TODO: Possibly refactor this test into a function that could
	" be called from other places as well?
	let fg_or_bg = a:typ == 'clr' ? 'fg' : 'bg'
	" Is this color active?
	let ip = 1
	let i = -1 " set invalid in case we never enter loop
	" Loop over active colors
	while ip <= b:txtfmt_cfg_num{fg_or_bg}colors
		" Obtain index into the arrays containing both active and
		" inactive colors
		let i = b:txtfmt_cfg_{fg_or_bg}color{ip}
		if a:idx == i
			" Found it - index corresponds to active color
			return 1
		endif
		let ip = ip + 1
	endwhile
	" If we get here, the color isn't active
	return 0
endfu
" >>>
" Function: Search_in_range <<<
" Purpose: Efficiently wraps builtin search() routine, searching only within
" input range, regardless of cursor position
" Return: Value returned by Vim's search() routine. If match occurs, cursor
" will be positioned on matching char; otherwise, it will be left at starting
" location.
" Possible Bug: Discovered 26Dec2014 - In the event that the search ends up
" being backwards, it doesn't appear that I do anything to ensure that the
" first (of potentially multiple) within search region is selected.
" TODO !!! UNUSED !!! REMOVE !!!
fu! Search_in_range(re, l1, c1, l2, c2)
	" Save original position
	let l_orig = line('.')
	let c_orig = col('.')
	" Move to start of region to be searched
	call cursor(a:l1, a:c1)
	" Position ourselves on the char from which search will be attempted, and
	" set search flags and stopline accordingly.
	" Note: The purpose of this logic is to ensure that we search the entire
	" region (including first and last chars), without searching more of the
	" buffer than necessary.
	if a:c1 > 1
		" Move to char just prior to region
		normal! h
		let sflags = 'W'
		let stopline = a:l2
	elseif a:l1 > 1
		" Couldn't move back on starting line, but we can move to end of line
		" prior to region start
		normal! k$
		let sflags = 'W'
		let stopline = a:l2
	else
		" Try backward search from region end
		call cursor(a:l2, a:c2)
		" Attempt to move rightward
		normal! l
		if col('.') > a:c2
			" This is an acceptable starting point for backward search
			let sflags = 'bW'
			let stopline = a:l1
		elseif line('$') > a:l2
			" Couldn't move rightward, but we can move to beginning of first
			" line beyond region
			normal! j0
			let sflags = 'bW'
			let stopline = a:l1
		else
			" Search region encompasses entire file. Move to end and permit
			" wrap
			normal! G$
			let sflags = ''
			let stopline = 0
		endif
	endif
	" Build the regex with appropriate line/col constraints
	" Note: The following constrained regex would work even without the logic
	" above; the logic above is required to ensure that we don't search more
	" of the buffer than necessary.
	let re = '\%('
		\ . '\%(\%>' . a:l1 . 'l\|\%>' . (a:c1 - 1) . 'c\)\&'
		\ . '\%(\%<' . a:l2 . 'l\|\%<' . (a:c2 + 1) . 'c\)'
		\ . '\)\&' . a:re
	" Perform the search
	if stopline
		let ret_val = search(re, sflags, stopline)
	else
		" Note: I believe setting stopline to 0 works just like omitting the
		" argument, but the Vim docs are a bit ambiguous on this point...
		let ret_val = search(re, sflags)
	endif
	if !ret_val
		" Search was unsuccessful. Return to original location
		call cursor(l_orig, c_orig)
	endif
	" Return the value returned by search()
	return ret_val
endfu
" >>>
" Function: s:Get_cur_tok_for_rgn() <<<
" Purpose: If cursor is within the body of a Txtfmt highlighting region (i.e.,
" not on an escape, start or end token) return the applicable token;
" otherwise, return empty string.
" Inputs:
" rgn    One of the following 3 strings: 'clr', 'fmt', 'bgc
"        Indicates the region type of interest
" Return: If the cursor lies within a region of the following form...
" Tf<idx>_<rgn>[<rgn> ...]_<num>[_<num> ...][_rtd]
" ...return the token corresponding to {rgn}, or the corresponding default
" token; otherwise, return the empty string.
" Error: N/A
" Assumptions: In order for this routine to be useful, caller should ensure
" that the character under the cursor falls into one of the following two
" categories:
" 1. Is not within a Txtfmt region
" 2. Is within the non-special portion of a Txtfmt region: i.e., not on an
"    escaping char or a region start/end token
" TESTING: Did 1st level of testing on 23Oct2009. Worked like a charm with no
" need for debugging.
" Update_28Dec2014: This one doesn't use synstack() (as does
" Get_cur_rgn_info), and hence, may be incomplete.
fu! s:Get_cur_tok_for_rgn(rgn)
	let bgc_active = b:txtfmt_cfg_bgcolor && b:txtfmt_cfg_numbgcolors > 0
	let clr_active = b:txtfmt_cfg_numfgcolors > 0
	let rgn_idx_max_fmt = b:txtfmt_num_formats - 1
	" Note: The following provide only upper bounds for color indices; an
	" additional test for color activity will be required.
	" Note: b:txtfmt_num_colors includes the default token; hence, the -1
	let rgn_idx_max_clr = b:txtfmt_num_colors - 1
	let rgn_idx_max_bgc = b:txtfmt_num_colors - 1

	if !s:Is_cursor_on_char()
		" Nothing under cursor - we can't determine anything about current
		" region type
		return ''
	endif
	" Get name of syntax group under cursor
	let s = synIDattr(synID(line("."), col("."), 1), "name")
	" Define regexes used in parsing syntax name
	let re_hdr = '^\%(Tf\%(0\|[1-9][0-9]*\)_\)'
	let re_rgn = '^\%(' . 'fmt' . (clr_active ? '\|\%(clr\)' : '') . (bgc_active ? '\|\%(bgc\)' : '') . '\)'
	let re_num = '^\%(_\([1-9][0-9]*\)\)'
	let re_ftr = '^\%(_rtd\)\?$'
	" Assume highlighted region of the following form:
	" Tf<idx>_<rgn>[<rgn> ...]_<num>[_<num> ...][_rtd]
	if s =~ re_hdr
		let s = substitute(s, re_hdr, '', '')
	else
		return ''
	endif
	" Extract the list of region types involved
	let rgn_idx = 0
	while 1
		if s =~ re_rgn
			let rgn = substitute(s, '\(...\).*', '\1', '')
			let s = substitute(s, '...', '', '')
			" Validate rgn
			" Note: The following 2 tests obviate the need to test for too
			" many rgns
			if rgn == 'clr' && !clr_active || rgn == 'bgc' && !bgc_active
				" Ooops! This region shouldn't exist
				return ''
			endif
			if exists('l:rgn_idxs_' . rgn)
				" Ooops! We've seen this one already
				return ''
			endif
			" Record index at which this rgn type was found
			let rgn_idxs_{rgn} = rgn_idx
			" Record the rgn type seen at this index
			let rgns{rgn_idx} = rgn
			let rgn_idx = rgn_idx + 1
		else
			if !rgn_idx
				" Not a valid Txtfmt region
				return ''
			else
				" Break out of loop to process the indices
				break
			endif
		endif
	endwhile
	" Process the list of _<num>, each of which corresponds to a region type
	" within rgns{} array
	let num_idx = 0
	while 1
		if s =~ re_num
			if num_idx >= rgn_idx
				" Oops! More rgn indices than rgns
				return ''
			endif
			" Extract the region number
			let num = substitute(s, re_num . '.*', '\1', '')
			let s = substitute(s, re_num, '', '')
			" Validate the region number
			" Test for region number too high
			" Note: There will be an additional test for colors to determine
			" whether they're active
			if num > rgn_idx_max_{rgns{num_idx}}
				" Oops! No such region token
				return ''
			endif
			" Additional tests for color regions
			if rgns{num_idx} == 'clr' || rgns{num_idx} == 'bgc'
				if !s:Is_active_color_index(rgns{num_idx}, num)
					" Ooops! num corresponds to inactive color
					return ''
				endif
			endif
			" All validity checks have passed
			" Save the region number and update index
			let rgn_nr{num_idx} = num
			let num_idx = num_idx + 1
		else
			" This is not another _<index>
			if num_idx < rgn_idx
				" Didn't find enough indices
				return ''
			else
				" Break out of loop to process possible _rtd
				break
			endif
		endif
	endwhile
	" Allow both normal and _rtd variants
	if s == '' || s =~ re_ftr
		" Valid region
		" Return applicable token
		if exists('l:rgn_idxs_' . a:rgn)
			" This rgn type was mentioned explicitly in the region name
			return nr2char(b:txtfmt_{a:rgn}_first_tok + rgn_nr{l:rgn_idxs_{a:rgn}})
			"return a:rgn . ": " . rgn_nr{l:rgn_idxs_{a:rgn}}
		else
			" The rgn of interest was not mentioned explicitly; assume default
			return nr2char(b:txtfmt_{a:rgn}_first_tok)
			"return a:rgn . ": default"
		endif
	else
		" Not a valid Txtfmt group
		return ''
	endif
endfu
" >>>
" Function: s:Prompt_fmt_clr_spec() <<<
" Purpose: Prompt user for type of formatting region desired, and return
" the string entered
" How: The user will be prompted to enter a fmt/clr[/bgc] list, consisting of
" fmt/clr[/bgc] atoms separated by commas and/or dots. The format of a
" fmt/clr[/bgc] atom is described in header of Translate_fmt_clr_spec().,
" Return: The entered string
fu! s:Prompt_fmt_clr_spec()
	" Prompt the user for fmt/clr spec string
	" TODO: Decide whether prompt needs to distinguish between bgc and clr
	call inputsave()
	let str = input('Enter a fmt / clr string. (Enter to cancel): ')
	call inputrestore()
	return str
endfu
" >>>
" Function: s:Lookup_clr_namepat() <<<
" Purpose: Convert the input color name pattern to a color index in range
" 1..8, using the buffer-specific color definition array
" b:txtfmt_{clr|bgc}_namepat.
" Use b:txtfmt_cfg_{fg|bg}color{} arrays to determine whether the specified
" color is active.
" Return:
"     Requested color valid and active: Color index between 1 and 8
"     Requested color invalid: 0
"     Requested color valid but inactive: -1 * {color_index}
fu! s:Lookup_clr_namepat(typ, namepat)
	if a:typ == 'c'
		let fg_or_bg = 'fg'
		let clr_or_bgc = 'clr'
	elseif a:typ == 'k'
		let fg_or_bg = 'bg'
		let clr_or_bgc = 'bgc'
	else
		echoerr "Internal error - Unknown color type `".a:typ."' passed to Lookup_clr_namepat()"
		return 0
	endif
	" Loop over all color definitions (active and inactive), looking for one
	" whose regex matches input color name
	let i = 1
	while i < b:txtfmt_num_colors
		if a:namepat =~ b:txtfmt_{clr_or_bgc}_namepat{i}
			" We found a match!
			if b:txtfmt_cfg_{fg_or_bg}colormask[i - 1] != '1'
				" Inactive color
				return -1 * i
			else
				" Active color
				return i
			endif
		endif
		let i = i + 1
	endwhile
	" Didn't find it!
	return 0
endfu
" >>>
" Function: s:Translate_fmt_clr_spec() <<<
" Purpose: Convert the input fmt/clr spec string to the corresponding fmt/clr
" token.
" Syntax: See the examples.
" Pre Condition: No whitespace is permitted anywhere in the input. Caller should
" have stripped any surrounding whitespace.
" Examples: For now, these examples are sufficiently enlightening to obviate
" need for formal grammar.
"
" -- Formats --
" f-      default (no fmt)
" f=      same
" f=-     same
" fubi    underline-bold-italic
" fbiu    same
" f=ubi   same
" Note: s/r/c attributes disallowed for certain permutations of
" b:txtfmt_cfg_longformats and b:txtfmt_cfg_undercurl
"
" -- Colors --
" cblue   fg clr blue
" c=blue  same
" Note: Default (no) clr/bgc forms identical to fmt forms.
" Note: Color names such as 'blue' in the example must match one of the color
" definitions specified by user (or default pattern if user hasn't overriden).
" Note: Specification of an inactive color (or any background color when
" background colors are disabled by 'tokrange') is considered to be an error.
" Return: One of the following:
" 1) A single fmt token
" 2) A single clr token
" 3) A single bgc token
" 4) empty string signals erroneous user entry
" Error: If error, function will set the script-local s:err_str
" Note: The function logic takes advantage of the fact that both strpart() and
" string offset bracket notation (s[i]) allow indices past end of string, in
" which case, they return empty strings.
" TODO: Rework to use exceptions rather than s:err_str.
fu! s:Translate_fmt_clr_spec(spec)
	" Design Decision: No whitespace permitted between type and what follows.
	let [t, s] = [a:spec[0], a:spec[1:]]
	if empty(s)
		let s:err_str = 'Empty fmt/clr spec: ' . t . ' must be followed by something.'
		return ''
	endif
	" If here, we don't have (illegal) bare f/c/k.
	" Permit (and ignore) a single `=' following type.
	if s[0] == '='
		" Note: After this point, empty(s) is valid default indication.
		let s = s[1:]
	endif
	" Ensure valid component type.
	if t !~? '[fc' . (b:txtfmt_cfg_bgcolor ? 'k' : '') . ']'
		if t ==? 'k'
			" Oops! Background colors aren't active.
			let s:err_str = "The current 'tokrange' setting does not support background colors."
				\." (:help txtfmt-formats)"
		else
			let s:err_str = 'Invalid fmt/clr spec. Must begin with '
				\.(b:txtfmt_cfg_bgcolor ? '"f", "c" or "k"' : '"f" or "c"')
		endif
		return ''
	endif
	" Check for default (no fmt/clr)
	" Note: Due to earlier validation, empty s implies bare `='.
	if empty(s) || s[0] == '-'
		" Maybe valid default; make sure nothing follows a `-'.
		if empty(s[1])
			" valid default format
			return nr2char(
				\b:txtfmt_{b:txtfmt_rgn_typ_abbrevs[t]}_first_tok)
		else
			" Shouldn't be anything after [fck]-
			let s:err_str = 'Unexpected chars after "' . t . '-"'
			return ''
		endif
	endif
	" We have a non-default (but not necessarily valid) spec.
	if t ==? 'f'
		" fmt string
		" Since not default fmt request, spec must match [ubi[sr[c]]]
		if s =~ '[^'.b:ubisrc_fmt{b:txtfmt_num_formats-1}.']'
			" s contains illegal (but not necessarily invalid) char
			if s !~ '[^ubisrc]'
				" Illegal (but not invalid) char
				" User has mistakenly used s, r or c with one of the
				" 'short' formats or c with a version of Vim that doesn't
				" support undercurl. Give an appropriate warning.
				if !b:txtfmt_cfg_longformats
					let s:err_str = "Only 'u', 'b' and 'i' attributes are permitted when one of the 'short' formats is in effect"
				else
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
			" Spec contains only legal (active) and valid fmt attrs.
			" Convert the attr chars to a binary val used to get token.
			" TODO: Perhaps use Vim's or() function now that it's available.
			let mask = 0
			" Loop over individual chars.
			for atom in split(s, '\zs')
				let mask = or(mask, s:ubisrc_mask[atom])
			endfor
			return nr2char(b:txtfmt_fmt_first_tok + mask)
		endif
	elseif t ==? 'c' || t ==? 'k'
		" Assumption: Type has already been validated.
		" clr or bgc string
		" Since not default clr/bgc request, spec must match color pattern.
		" Determine which color index corresponds to color pattern.
		let clr_ind = s:Lookup_clr_namepat(t, s)
		if clr_ind == 0
			let s:err_str = "Invalid color name pattern: '".s."'"
			return ''
		elseif clr_ind < 0
			" TODO_BG: Make sure the help note below is still valid after
			" help has been updated.
			let s:err_str = "Color ".(-1 * clr_ind)." is not an active "
				\.(t ==? 'c' ? "foreground" : "background")
				\." color. (:help "
				\.(t ==? 'c' ? "txtfmtFgcolormask" : "txtfmtBgcolormask").")"
			return ''
		endif
		" IMPORTANT NOTE: clr_ind is 1-based index (1 corresponds to first
		" non-default color)
		return nr2char(
			\clr_ind + b:txtfmt_{b:txtfmt_rgn_typ_abbrevs[t]}_first_tok)
	endif
endfu
" >>>
" Function: s:Translate_fmt_clr_list() <<<
" Purpose: Translate the input comma/dot/space-separated list of fmt/clr/bgc
" spec atoms into a string of tokens suitable for insertion into the buffer.
" Validation is performed. Also, cursor offset into translated token string is
" determined based upon the presence of a dot (replaces comma when it appears
" between fmt/clr/bgc atoms - may also appear as first or last character in
" fmt/clr/bgc spec list).
" Input: Comma/dot-separated list of fmt/clr/bgc spec atoms.
" Return: String of the following format:
" <offset>,<tokstr>
" Error: Return empty string and set s:err_str
" Warning: Set s:wrn_str
fu! s:Translate_fmt_clr_list(s)
	" For convenience
	let s = a:s
	let len = strlen(s)
	let offset = -1			" -1 means not explicitly set by user
	let i = 0
	let sep = ''			"[,.] or '' for ws-separated or end-of string
	let num_fld = 0			" # of atoms encountered
	let tokstr = ''			" built up in loop
	" First check for `.' at head of list.
	" Note: On failure to find leading `.', matchend will return -1, which
	" needs to be changed to 0 so that loop starts looking at start.
	let i = matchend(s, '^\s*\.\s*')
	if i > 0
		let offset = 0
	else
		let i = 0
	endif
	" Process the fmt/clr/bgc spec atom(s) in a loop
	" Note: We've already processed any leading `.'; at this point.
	while i < len
		" Get a term and whatever separates it from any following term.
		" Note: Commas and dots not allowed except as field sep.
		" Note: Match with '$' returns strlen (even for empty string)
		" Note: This pattern will always match, but match will be empty for
		" empty term.
		" Captures:
		" 1=f/c/k component (optional, but nonexistent treated as error)
		" 2=non-whitespace sep (if it exists)
		let ms = matchlist(s, '^\([^,.[:space:]]*\)\s*\([.,]\)\?\s*', i)
		let fld = ms[1]
		let sep = ms[2]
		if !empty(fld)
			" Assumption: Pattern guarantees fld has no surrounding whitespace.
			let num_fld += 1
			" Translate non-empty field.
			let tok = s:Translate_fmt_clr_spec(fld)
			if tok == ''
				" Must have been error
				let s:err_str = "Invalid fmt/clr spec: '" . fld . "': " . s:err_str
				return ''
			endif
			let tokstr = tokstr . tok
		else
			let s:err_str = "Empty field encountered at '" . strpart(s, i)
			return ''
		endif
		" Was offset specified with `.' separator?
		" Design Decision: Signal error if list includes multiple `.'.
		" Rationale: May indicate user confusion.
		if sep == '.'
			if offset >= 0
				let s:err_str = "Only 1 `.' permitted in fmt/clr spec"
				return ''
			endif
			let offset = num_fld
		endif
		" Update for next iteration.
		let i += len(ms[0])
	endwhile
	" Check post-conditions:
	" -At least 1 term (since caller typically handles empty spec).
	"  TODO: Once I've transitioned to exception handling, could probably just
	"  return empty string for this case (i.e., stop returning error).
	" -Final sep can't be `,' (only `.' makes sense at end of list).
	if !empty(sep) && sep == ','
		let s:err_str = "Trailing comma not allowed in fmt/clr spec list"
		return ''
	elseif !num_fld
		let s:err_str = "Translate_fmt_clr_list: expected at least 1 term in fmt/clr/bgc list"
		return ''
	endif
	" Return the special format string
	return offset . ',' . tokstr
endfu
" >>>
" Function: s:Get_cur_rgn_info() <<<
" Purpose: Parse the synstack to determine active fmt/clr/bgc regions at
" cursor, returning a struct that contains either a mask (fmt) or color number
" (clr/bgc) for each region type.
" Return:
" {
"   fmt: <fmt-mask>
"   clr: <fg-color-number>
"   bgc: <bg-color-number>
" }
fu! s:Get_cur_rgn_info()
	let bgc_active = b:txtfmt_cfg_bgcolor && b:txtfmt_cfg_numbgcolors > 0
	let clr_active = b:txtfmt_cfg_numfgcolors > 0
	let rgn_idx_max_fmt = b:txtfmt_num_formats - 1
	" Note: The following provide only upper bounds for color indices; an
	" additional test for color activity will be required.
	" Note: b:txtfmt_num_colors includes the default token; hence, the -1
	let rgn_idx_max_clr = b:txtfmt_num_colors - 1
	let rgn_idx_max_bgc = b:txtfmt_num_colors - 1

	let info = {'fmt': 0, 'clr': 0, 'bgc': 0}
	" TODO: Make this static everywhere it's used.
	if !s:Is_cursor_on_char()
		" Nothing under cursor - we can't determine anything about current
		" region type
		return info
	endif
	" Define regexes used in parsing syntax name
	let re_hdr = '\%(Tf\%(0\|[1-9][0-9]*\)_\)'
	let re_rgn = '\%(' . 'fmt' . (clr_active ? '\|\%(clr\)' : '') . (bgc_active ? '\|\%(bgc\)' : '') . '\)'
	let re_num = '\%([1-9][0-9]*\)'
	let re_ftr = '\%(_rtd\)'
	" Construct a regex using the atoms above.
	let re = '^' . re_hdr . '\%(\(' . re_rgn . '\)\%(\(' . re_rgn . '\)\(' . re_rgn . '\)\?\)\?\)'
		\. '\%(_\(' . re_num . '\)\%(_\(' . re_num . '\)\%(_\(' . re_num . '\)\)\?\)\?\)'
		\. re_ftr . '\?$'
	" Get syntax stack at cursor
	for id in synstack(line('.'), col('.'))
		let s = synIDattr(id, "name")
		" Assume highlighted region of the following form:
		" Tf<idx>_<rgn>[<rgn> ...]_<num>[_<num> ...][_rtd]
		let m = matchlist(s, re)
		if empty(m)
			" Not what we're looking for...
			continue
		else
			for i in range(1, 3)
				let [rtyp, rval] = [m[i], m[i + 3]]
				" Store the bitmask/colornum
				let info[rtyp] = rval
				if empty(m[i + 1])
					break
				endif
			endfor
		endif
	endfor
	return info
endfu
" >>>
" Function: s:Parse_fmt_clr_transformer() <<<
" Input: <fck-spec>[/<selector>]
" Convert the input comma-separated f/c/k transformer spec and optional
" following selector expression to a struct.
" Input Format: In lieu of full grammar, here are some illustrative examples of
" both fmt/clr transformer spec and selector expressions.
" Transformer Spec Examples:
" fubi        add bold-underline-italic
" f+ubi       same as previous (+ is the default).
" fub-is      add bold-underline, remove italic-standout
" fub-isb     ERROR - conflicting action for 'bold'
" f-ubi       remove bold-underline-italic
" fb-i+u-s    add bold-underline, remove italic-standout
" f+b-i+u-s   same as previous
" f-          clear formatting
" f=          same as previous (literally, set to empty fmt attrs)
" f=-         same as previous (though a bit weird)
" f=ubi       set formats to exactly bold-underline-italic
" cr          set fg color to red
" c=r         same as previous
" kblue       set bg color to blue
" k=blue      same as previous
" Note: 'Clear color' specified same for c/k as for f.
" Selector Expression Examples:
" TODO...
"
" --- Output Format ---
" {
"   sel: [] | {}
"   rgns: {
"     fmt:
"       0-N - {set-fmt-mask}
"       |
"       [
"         0-N - {add-fmt-mask},
"         0-N - {sub-fmt-mask}
"       ],
"     clr: 0-{max-active-clr-num}
"     bgc: 0-{max-active-bgc-num}
"   }
" }
" Note: The value 0 in an additive mode fmt mask is a NOP. In non-additive
" (prescriptive) mode, it clears existing fmts. For colors, 0 means 'no color'.
fu! s:Parse_fmt_clr_transformer(specs)
	" Strip off the selector if it was supplied.
	" Note: The following patterns strip leading and trailing whitespace on the
	" components.
	" TODO: Consider using match() to find any `/' (would be more efficient).
	let [_, specs, sel; rest] =
		\matchlist(a:specs, '^\s*\(.\{-}\)\s*\%(/\s*\(.\{-}\)\s*\)\?$')
	if empty(specs)
		" Effectively empty spec. Return NOP object.
		" TODO: Should we warn if user has specified a selector?
		return {}
	endif
	" Initalize non-empty return object.
	let ret = {'sel': {'op': '!', 'val': 1}, 'rgns': {}}
	let rgns = ret.rgns
	if !empty(sel)
		let ret.sel = s:Parse_selector(sel)
	endif
	" Split the comma or space-separated f/c/k components
	" Note: Use of explicit commas can produce empty components anywhere, even
	" at end: set keepempty to facilitate detection.
	" Note: Because we've stripped trailing whitespace, a trailing empty
	" component can be caused only by comma; accordingly, the \ze\S is
	" superfluous.
	let re_sep = '\s*,\s*\|\s\+'
	let fcks = split(specs, re_sep, 1)
	" Loop over the f/c/k components
	for spec in fcks
		if empty(spec)
			throw "Parse_fmt_clr_transformer: empty fmt/clr/bgc components not permitted"
		endif
		" Extract token type and remainder of spec
		let [t, spec] = [spec[0], spec[1:]]
		" Validate the type
		if t !~ '[fck]'
			throw "Invalid type specifier in fmt/clr transformer spec: `" . t . "'"
		endif
		if has_key(rgns, b:txtfmt_rgn_typ_abbrevs[t])
			" We've already processed a component of this type.
			throw "Parse_fmt_clr_transformer: no more than 1 component of"
				\ . " each type (f|c|k) permitted"
		endif
		if empty(spec)
			throw "Parse_fmt_clr_transformer: `" . t . "' must be followed by valid "
				\ . (t == 'f' ? "format attributes" : "color name")
		endif
		" Switch on type
		if t == 'f'
			" Design Decision: Keep special meaning of `f-', and support several
			" other intuitive means of specifying 'no format'.
			if spec == '=' || spec == '-' || spec == '=-'
				" Clear all fmt attrs
				let rgns.fmt = 0
			else
				" Note: Special cases have all been handled.
				" Additive or prescriptive mode?
				let additive = spec[0] != '='
				let attrs = additive ? spec : spec[1:]
				" Validate form of the fmt attr list.
				" Design Decision: For now, permit empty segments: e.g., +u--b.
				" TODO: Does this make sense, or should the -- be treated as
				" error? (Consider that someone might think of -- as +.)
				if attrs !~ '^[' . (additive ? '-+' : '')
					\. b:ubisrc_fmt{b:txtfmt_num_formats-1} . ']\+$'
					throw "Invalid fmt transformer spec: `f" . spec . "'"
				endif
				" Initialize mask(s) to be built in loop.
				let masks = additive ? {'+': 0, '-': 0}  : {'=': 0}
				" Initialize default (or only) op.
				let op = additive ? '+' : '='
				" Loop over individual chars.
				for atom in split(attrs, '\zs')
					if atom == '+'
						let op = '+'
					elseif atom == '-'
						let op = '-'
					else
						" Check for conflict.
						if additive && and(masks[op == '+' ? '-' : '+'], s:ubisrc_mask[atom])
							throw "Ambiguous use of fmt attr " . atom
							\ . " in both add/sub parts of spec: `f" . spec . "'"
						endif
						let masks[op] = or(masks[op], s:ubisrc_mask[atom])
					endif
				endfor
				if additive
					" Save list of add/sub masks.
					let rgns.fmt = [masks['+'], masks['-']]
				else
					" Save scalar 'set' mask
					let rgns.fmt = masks['=']
				endif
			endif
		elseif t == 'c' || t == 'k'
			if t == 'k' && !b:txtfmt_cfg_bgcolor
				throw "Use of `k' invalid when background colors are disabled"
			endif
			" Allow several special forms for default (no) color.
			if spec == '=' || spec == '-' || spec == '=-'
				" Return to default.
				let clr_num = 0
			else
				" Strip optional `=' after c/k.
				let spec = substitute(spec, '^=', '', '')
				let clr_num = s:Lookup_clr_namepat(t, spec)
				if clr_num <= 0
					if clr_num == 0
						throw "Invalid color name pattern in fmt/clr transformer spec: `"
							\ . spec . "'"
					else
						throw "Color specified by name pattern in fmt/clr"
							\ . " transformer spec is inactive: `" . spec . "'"
					endif
				endif
			endif
			let rgns[b:txtfmt_rgn_typ_abbrevs[t]] = clr_num
		else
			throw "Invalid type specifier in fmt/clr transformer spec: " . t
		endif
	endfor
	return ret
endfu

" This version enforced the simpler (Perl regex flag style) format for the fmt
" spec: i.e., fxxx-xxx with f- by itself meaning clear formats.
" TODO: Remove after commit, perhaps moving to 'code graveyard' first.
fu! s:Parse_fmt_clr_transformer_obsolete(specs)
	" Initalize return object.
	let ret = {}
	let specs = TxtfmtUtil_strip(a:specs)
	if empty(specs)
		" Effectively empty spec.
		return ret
	endif
	" Split the comma-separated f/c/k components
	let fcks = split(specs, ',', 1)
	if len(fcks) > 3
		throw "Parse_fmt_clr_transformer: More than 3 f|c|k components not permitted"
	endif
	" Loop over the f/c/k components
	for spec in fcks
		let spec = TxtfmtUtil_strip(spec)
		if empty(spec)
			throw "Parse_fmt_clr_transformer: empty fmt/clr/bgc components not permitted"
		endif
		" Extract token type and remainder of spec
		let [tt, spec] = [spec[0], spec[1:]]
		" Validate the type
		if tt !~ '[fck]'
			throw "Invalid type specifier in fmt/clr transformer spec: `" . tt . "'"
		endif
		let spec = TxtfmtUtil_lstrip(spec)
		if empty(spec)
			throw "Parse_fmt_clr_transformer: empty fmt/clr/bgc specs not permitted"
		endif
		if has_key(ret, tt)
			throw "Parse_fmt_clr_transformer: no more than 1 component of each type (f|c|k) permitted"
		endif
		" Switch on type
		if tt == 'f'
			" Design Decision: Discard spaces (even interior), which have no
			" meaning in fmt specs.
			let spec = substitute(spec, '\s\+', '', 'g')
			if empty(spec)
				" TODO: Decide on this, noting that `f' is actually a
				" degenerate (and arguably ambiguous) form that means add/remove
				" nothing. Allow it?
				throw "Parse_fmt_clr_transformer: empty fmt specs not permitted"
			elseif spec == '-' || spec == '='
				" f- special case: return to default (mask all attributes)
				" f= by itself is a more natural version of the special case.
				" Decision: -1 permits distinction to be made if necessary
				" (i.e., between f- and (e.g.) f-ubisrc)
				"let ret.fmt = b:txtfmt_num_formats - 1
				let ret.fmt = -1
			else
				" Break into sub-parts. 'keepempty' guarantees at least an add (or
				" set) part (possibly empty).
				" Note: split() will split (e.g.) +u into 1 element.
				let re_set = '^=[ubisrc]*$'
				" Design Decision: Defer checks for fmt tok active status till
				" overall form has been validated.
				" Assumption: This regex allows completely empty spec;
				" currently, that case is caught by the earlier empty(spec)
				" test.
				let re_add = '^+\?\([ubisrc]*\)\(-\([ubisrc]*\)\)\?$'
				if spec =~ re_set
					let add = spec[1:]
					let additive = 0
					let sub = ''
				elseif spec =~ re_add
					let ms = matchlist(spec, re_add)
					let [add, sub] = [ms[1], ms[3]]
					let additive = 1
				else
					throw "Invalid fmt transformer spec: `f" . spec . "'"
				endif
				" Decision: Permit add and/or sub to be empty.
				" Rationale: Something like f+- is pointless, but not ambiguous.
				" Add to list to facilitate processing. Could short-circuit the
				" loop processing, but no need to optimize for such an unusual
				" use-case.
				let cmasks = additive ? [add, sub] : [add]
				let mask_prev = 0
				let masks = []
				" Loop over the specs (1 or 2 of set/add/sub)
				for cmask in cmasks
					" Note: Allow either to be empty. We've already verified
					" at least 1 non-empty.
					"call TxtfmtUtil_validate_fmt_char(cmask)
					if cmask !~ '^\%(['.b:ubisrc_fmt{b:txtfmt_num_formats-1}.']*\)$'
						" TODO: Factor this validation logic out; it's used elsewhere. TxtfmtUtil_<...>
						" Problem: The other place it's used uses the old
						" s:err_str mechanism.
						if cmask !~ '[^ubisrc]'
							if !b:txtfmt_cfg_longformats
								throw "Parse_fmt_clr_transformer: Only 'u', 'b' and 'i' fmt attributes are permitted when one of the 'short' formats is in effect"
							else
								throw "Parse_fmt_clr_transformer: Undercurl attribute supported only in Vim 7 or later"
							endif
						else
							throw "Parse_fmt_clr_transformer: Invalid char(s) in attribute mask within fmt/clr transformer spec: `f" . spec . "'"
						endif
					endif
					" Process individual attr chars to build sub mask.
					" Decision: Treat conflict but not redundancies as error.
					" TODO: Factor this logic out into a TxtfmtUtil_<...>
					" util.
					let mask = 0
					for atom in split(cmask, '\zs')
						if and(mask_prev, s:ubisrc_mask[atom])
							throw "Conflict in fmt/clr transformer spec: attempt to both add and remove the same attribute: `" . atom . "'"
						endif
						let mask = or(mask, s:ubisrc_mask[atom])
					endfor
					call add(masks, mask)
					" Save for conflict testing on next iteration.
					let mask_prev = mask
				endfor
				" Deconstruct the list into return list/scalar: in additive
				" mode, we return a length-2 list of masks; otherwise, scalar.
				if additive
					let ret.fmt = masks
				else
					" Extract scalar 'set' mask
					let ret.fmt = masks[0]
				endif
			endif
		elseif tt == 'c' || tt == 'k'
			if spec == '-'
				" Return to default.
				let clr_num = 0
			else
				let clr_num = s:Lookup_clr_namepat(tt, spec)
				if clr_num <= 0
					if clr_num == 0
						throw "Invalid color name pattern in fmt/clr transformer spec: `" . spec . "'"
					else
						throw "Color specified by name pattern in fmt/clr transformer spec is inactive: `" . spec . "'"
					endif
				endif
			endif
			let ret[tt == 'c' ? 'clr' : 'bgc'] = clr_num
		else
			throw "Invalid type specifier in fmt/clr transformer spec: " . tt
		endif
	endfor
	return ret
endfu
" TODO: TEMP DEBUG
fu! s:Parse(specs)
	return s:Parse_fmt_clr_transformer(a:specs)
endfu
" >>>
" Function: s:Convert_string_to_binary_mask <<<
" Convert input string consisting of 1's and 0's to corresponding binary mask.
" TODO: Not really using this now...
fu! s:Convert_string_to_binary_mask(str)
	let mask = 0
	let bit = 1
	let i = 1
	while i <= len(a:str)
		if a:str[-i : -i] == '1'
			let mask = or(mask, bit)
		endif
		let bit = bit * 2
		let i += 1
	endwhile
	return mask
endfu
" >>>
" <<< Function: s:Get_tok_info
" Synopsis: Return an object representing the input token.
" Inputs:
" c - token character
" Return: Empty object if input char not a token, else
" {
"   rgn: 'fmt'|'clr'|'bgc',
"   %% Note that both color indices and fmt masks are actually indices.
"   idx: <color-idx>|<fmt-mask>
" }
" Implementation Note: fg/bgcolormasks are strings of 1's and 0's, with string
" index correlating with color index as follows:
" color index = string idx + 1
" Example: 1st char in mask string corresponds to color 1
" Tested: 28Dec2014 (but modified since)
fu! s:Get_tok_info(c)
	let ret = {}
	" Get char code.
	let c_nr = char2nr(a:c)
	" Determine tok type
	" Start checking at final sub-region (clr < fmt < bgc)
	if b:txtfmt_cfg_bgcolor && c_nr >= b:txtfmt_bgc_first_tok && c_nr < b:txtfmt_bgc_first_tok + b:txtfmt_num_colors
		let ret.rgn = 'bgc'
	elseif c_nr >= b:txtfmt_fmt_first_tok && c_nr < b:txtfmt_fmt_first_tok + b:txtfmt_num_formats
		let ret.rgn = 'fmt'
	elseif c_nr >= b:txtfmt_clr_first_tok && c_nr < b:txtfmt_clr_first_tok + b:txtfmt_num_colors
		let ret.rgn = 'clr'
	endif
	if !empty(ret)
		if ret.rgn == 'fmt'
			let ret.idx = c_nr - b:txtfmt_fmt_first_tok
		else
			let idx = c_nr - b:txtfmt_{ret.rgn}_first_tok
			if idx == 0 || b:txtfmt_cfg_{ret.rgn == 'clr' ? 'f' : 'b'}gcolormask[idx - 1] == '1'
				let ret.idx = idx
			else
				" Inactive color! Convention is to indicate with neg. index
				let ret.idx = -idx
			endif
		endif
	endif
	return ret
endfu
">>>
" Function: s:Is_escaping_tok <<<
" Description: Return true iff char under cursor represents an escaping token.
" Tested: 29Nov2015
fu! s:Is_escaping_tok()
	if b:txtfmt_cfg_escape != 'none'
		if b:txtfmt_cfg_escape == 'bslash'
			" Are we on a bslash preceded by even # of bslashes (possibly 0),
			" followed by 0 or more bslashes, followed by any tok atom?
			" Note: Bslash is escaping whether following # of bslashes is even
			" or odd: if even, tok is escaped; if odd, final bslash is escaped.
			" TODO: Assuming the \%#=1 was for Vim bug workaround; confirm and
			" document (also for Is_escaped_tok)...
			return search('\%#=1\%#\%(\%(^\|[^\\]\)\%(\\\\\)*\)\@<='
				\.'\\\\*[' . b:txtfmt_re_any_tok_atom . ']', 'ncW')
		else
			" Are we on a tok preceded by even # (possibly 0) of itself, and
			" followed by *any* # of itself (once again, odd/even simply
			" determines whether the tok at end of seq is escaped).
			return search('\%#=1\%#\([' . b:txtfmt_re_any_tok_atom . ']\)'
				\.'\%(\%(^\|\1\@!.\)\1\%(\1\1\)*\)\@<=\1', 'ncW')
		endif
	endif
	" Can't be on an escaped tok if we're not even on a tok char.
	return 0
endfu
" >>>
" Function: s:Is_escaped_tok <<<
" Description: Return true iff char under cursor represents an escaped (i.e.,
" escapee, not escaping) token.
" Note: Original Is_esc_tok didn't differentiate between escape and escapee.
" Tested: 27Dec2014
" Reworked: 29Nov2015
" Tested: 29Nov2015
fu! s:Is_escaped_tok()
	if b:txtfmt_cfg_escape != 'none'
		if b:txtfmt_cfg_escape == 'bslash'
			" Are we at a location preceded by odd # of bslashes, matching any
			" number of bslashes (possibly 0) terminated with any tok?
			" Note: See note regarding even/odd distinction in Is_escaping_tok.
			return search('\%#=1\%#\%(\%(^\|[^\\]\)\\\%(\\\\\)*\)\@<='
				\.'\\*[' . b:txtfmt_re_any_tok_atom . ']', 'ncW')
		else
			" Differentiate between escape and escapee.
			" Are we on a tok preceded by odd # of itself?
			return search('\%#=1\%#\([' . b:txtfmt_re_any_tok_atom
				\. ']\)\@=\%(\%(^\|\%(\1\)\@!.\)\%(\%(\1\1\)*\1\)\)\@<=', 'ncW')
		endif
	endif
	" Can't be on an escaped tok if we're not even on a tok char.
	return 0
endfu
" >>>
" Function: s:Get_effective_pos <<<
fu! s:Get_effective_pos(tok)
	" Initialize return pos with copy to avoid modifications to input.
	let pos = copy(a:tok.pos)
	if a:tok.action == 'a'
		" Get the token so we can determine its length.
		let tokstr = s:Tok_nr_to_char(a:tok.rgn, a:tok.idx)
		" Effective position is after the token.
		let pos[1] += len(tokstr)
	endif
	return pos
endfu
" >>>
" Function: s:Is_phantom_null <<<
" Description: Return true iff input tok is a phantom that wasn't needed.
fu! s:Is_phantom_null(tok)
	return empty(a:tok.action) && a:tok.loc =~ '[{}]'
endfu
" >>>
" Function: s:Search_tok <<<
" Description: Attempt to find and return the next token, taking into account
" the search flags passed as input and the (optional) stopline.
" Stopline Note: If stopline is reached without finding a tok, search
" transitions to search for hlable or tok, whichever comes first.
" Motivation: Stopline should always be past beg/end of region, where any found
" toks are of interest only to synchronization/cleanup logic, for which presence
" of hlable can be significant, even in the absence of actual tok.
" Inputs:
" rgn
"     Desired region type(s), interpreted as follows:
"     <empty>  all rgn types
"     string   specified rgn type
"     list     all rgn types in list
"
" sflags
"     Pass-through for the following Vim search() flags:
"     b - search backwards
"     n - don't move cursor
"     c - allow match (of token) at cursor pos
"     Design Decision: Don't bother validating, as use of others would be
"     internal error.
"
" Return:
" {
"   %% what was found
"   typ: 'tok'|'hla'|'eob'
"   Note: When tok is not found, there are 2 possibilities:
"   1. Found hlable before end of buffer
"   2. Hit end (or start) of buffer looking for hlable
"   TODO: Consider changing typ enum to 2 flags (either at top level, or nested
"   within a 'flags' struct), 'tok' and 'eob', which would have the following
"   correspondences:
"   typ=='tok' => tok=1 eob=0
"   typ=='hla' => tok=0 eob=0
"   typ=='eob' => tok=0 eob=1
"   Question: Pros/Cons?
"   TODO: As mentioned further down, need to refactor this data structure to
"   make cleaner boundary between the tok stuff (below) and non-tok-specific
"   metadata (e.g., flags or enum).
"   %% type of token found
"   %% Note: Redundant with a:rgn if the latter is a string.
"   rgn: 'fmt'|'clr'|'bgc'
"   -- The following apply only when typ == 'tok'
"   %% integer value representing the tok found
"   idx: <color-idx>|<fmt-mask>
"   %% position of tok match
"   pos: [lnum, col],
"   %% actual character corresponding to tok
"   tok: <tok-char>
" }
" Post Condition: Unless 'n' flag supplied, will be positioned on any found tok.
fu! s:Search_tok(rgns, sflags, ...)
	" Note: For search(), stopline of 0 works like omitting arg.
	let stopline = a:0 ? a:1 : 0
	" Initialize return struct.
	" Design Decision: Because we don't need to differentiate, 'eob' is
	" currently used for both start and end of buffer: obviously, if 'b' flag is
	" passed, it means 'start of buffer'.
	let ret = {'typ': 'eob'}
	" TODO: Consider adding 'loc' somewhere in return struct.
	let re_tok = empty(a:rgns)
		\ ? b:txtfmt_re_any_tok
		\ : type(a:rgns) == 1
			\ ? b:txtfmt_re_{a:rgns}_tok
			\ : join(map(copy(a:rgns), 'b:txtfmt_re_{v:val}_tok'), '\|')
	" TODO: Consider doing away with sflags, in favor of distinct args.
	let [lnum, col] = searchpos(re_tok, a:sflags . 'W', stopline)
	if lnum
		" Cursor is on found tok (unless 'n' flag was input).
		let ret.typ = 'tok'
	else
		" No tok within stopline distance; try unbounded search that stops at
		" tok or hlable, whichever comes first.
		" Note: If user has such an inordinate number of consecutive toks that
		" this unbounded search takes noticeably long, something is pathological
		" with his buffer, and he shouldn't be surprised by slowness...
		" TODO: Make sure this submatch usage isn't affected by Vim's 'p' flag
		" idiosyncrasies/bugs (documented in correspondence with Bram on
		" list)...
		let re_hlable = s:Get_hlable_patt({})
		let re = '\(' . re_tok . '\)\|\(' . re_hlable . '\)'
		" Note: Cursor has not moved; inhibit move for now; if flags don't
		" inhibit and a tok is found, we'll move to it later.
		let [lnum, col, submatch] = searchpos(re, a:sflags . 'Wpn')
		if lnum
			" Found either tok or hlable (but haven't moved cursor to it).
			" Note: The 'p' flag's submatch value is 1 greater than capture #.
			if submatch == 2
				" IMPORTANT TODO: This case is *very* rare, but possible. (Would
				" have to be a tok at the end of the line just before stopline.)
				" Go over function logic and intended use case again...
				let ret.typ = 'tok'
				" Position on found tok unless prevented by input flag.
				if a:sflags !~ 'n'
					call cursor(lnum, col)
				endif
			else
				" Note presence of hlable but don't move to it.
				" Rationale: Purpose of typ 'hla' is to distinguish between
				" <eob> and non-<eob> cases.
				let ret.typ = 'hla'
			endif
		endif
	endif
	" If tok was found, augment it here.
	" TODO: Rework this function, perhaps taking advantage of positioning on
	" cursor when 'n' flag is omitted.
	if ret.typ == 'tok'
		" Parse the tok and store in denormalized form.
		" TODO: Consider nesting all this within a sub-object.
		" Rationale: Cleaner division between the object returned by this
		" function (which could be non-tok) and an actual tok object.
		let ret.chr = s:Get_char([lnum, col])
		let ti = s:Get_tok_info(ret.chr)
		let ret.rgn = ti.rgn
		let ret.idx = ti.idx
		let ret.pos = [lnum, col]
	endif
	return ret
endfu
" >>>
" Function: s:Get_char_unused <<<
" TODO: Get rid of this one in favor of Get_char? Consider pros/cons of each
" approach.
" Note: Advantage of this one is that it doesn't use any registers and can
" get char at arbitrary position.
" Disadvantage (if any) is that it's (at least conceptually) less efficient.
fu! s:Get_char_unused(...)
	let [lnum, col] = a:0 ? a:1 : getpos('.')[1:2]
	" Note: Vim can handle col not falling at start of mb char.
	let line = getline(lnum)[col - 1 : ]
	return line[0 : byteidx(line, 1) - 1]
endfu
" >>>
" Function: s:Delete_range(beg, end) <<<
" TODO: Decide whether to hard-code `[,`]
" TODO: Also decide whether the save/restore of pos is needed.
fu! s:Delete_range(beg, end)
	"let save_pos = getpos('.')[1:2]
	"call cursor(a:beg)
	let save_vis = {'<': getpos("'<"), '>': getpos("'>")}
	call setpos("'<", [0, a:beg[0], a:beg[1], 0])
	call setpos("'>", [0, a:end[0], a:end[1], 0])
	normal! gvd
	" Restore the visual marks
	call setpos("'<", save_vis['<'])
	call setpos("'>", save_vis['>'])
	"call cursor(save_pos)
	"let save_mark = getpos("']")[1:2]
	"call setpos("']", end)
	"exe 'normal! "_dv`]'
endfu
" >>>
" Function: s:Delete_range_vi() <<<
fu! s:Delete_range_vi(beg, end)
	call cursor(a:beg)
	normal! v
	call cursor(a:end)
	normal! d
endfu
" >>>
" Function: s:Delete_range_op() <<<
fu! s:Delete_range_op(beg, end)
	call setpos("']", [0, a:end[0], a:end[1], 0])
	call cursor(a:beg)
	normal! dv`]
endfu
" >>>
" Function: s:Move_pos_rightward_maybe() <<<
" Adjust the input position by moving it rightward (without moving to next line
" or past end of current) to the position of the nearest character that isn't
" concealed, else the final character on the line.
fu! s:Move_pos_rightward_maybe(pos)
	let save_pos = getpos('.')[1:2]
	call cursor(a:pos)
	let eol = col('$')

	" Find first char that isn't concealed, else final char on line (not
	" including eol in virtualedit mode).
	let next_col = a:pos[1]
	let col = -1
	while next_col > col && next_col < eol
		" Char under cursor is now accepted.
		let col = next_col
		let concealed = synconcealed(a:pos[0], col)
		if concealed[0]
			" Try to move to next char rightward...
			normal! l
			let next_col = col('.')
		else
			" We found an unconcealed char!
			break
		endif
	endwhile

	" Restore saved pos.
	call cursor(save_pos)

	if col > a:pos[1]
		" Position has changed.
		let a:pos[1] = col
		return 1
	endif
	return 0
endfu
" >>>
" Function: s:Get_char() <<<
" Purpose: Get and return the character at provided position (default under the cursor).
" Return: Character under the cursor or empty string if ga would display NUL
" Assumption: Caller saves and restores @z, which is used by this function
fu! s:Get_char(...)
	" Design Decision: Empty [] works like omitting position.
	let need_move = !!a:0 && !empty(a:1)
	let z_save = @z
	if need_move
		let save_pos = getpos('.')[1:2]
		call cursor(a:1)
	endif
	normal! "zyl
	if need_move
		call cursor(save_pos)
	endif
	let ret = @z
	let @z = z_save
	" Return the character
	return ret
endfu
" >>>
" Function: s:Backward_char() <<<
" Move back 1 char and return pos list representing old position (empty list
" if we're already at beginning of buffer).
fu! s:Backward_char()
	let save_pos = getpos('.')
	if save_pos[2] == 1
		" Beginning of line
		if save_pos[1] == 1
			" Already at beginning of buffer
			return []
		else
			" To end of previous line
			normal! k$
		endif
	else
		" Back a char
		normal! h
	endif
	return save_pos
endfu
" >>>
" Function: s:Forward_char() <<<
fu! s:Forward_char()
	let save_pos = getpos('.')
	" Attempt to move forward 1 char
	normal! l
	if col('.') == save_pos[2]
		" Couldn't move forward on current line.
		if save_pos[1] < line('$')
			normal! j0
		else
			" Already at end of buffer
			return {}
		endif
	endif
	return save_pos
endfu
" >>>
" Function: s:Delete_char() <<<
" Delete the specified char from buffer (default char under cursor) and return
" it.
" TODO: Cleaner than s:Delete_cur_char (doesn't require caller to save/restore
" z reg) but slower.
" TODO: Consider refactoring this and Get_char to avoid code duplication.
fu! s:Delete_char(...)
	let need_move = !!a:0
	if need_move
		let save_pos = getpos('.')[1:2]
		call cursor(a:1)
	endif
	let c = s:Get_char()
	" Delete into black hole.
	normal! "_x
	if need_move
		call cursor(save_pos)
	endif
	" Return deleted char.
	return c
endfu
" >>>
" Function: s:Adjust_sel_to_protect_escapes <<<
" Adjust visual selection to prevent splitting of escape/escapee pairs.
" Return Note: This function modifies input positions in-place.
fu! s:Adjust_sel_to_protect_escapes(opt)
	" Note: No point in continuing in the 'linewise' case.
	" Rationale: In 'linewise' case, head of region corresponds to head of
	" line, which renders split of escape/escapee pair impossible.
	if has_key(a:opt.rgn, 'beg_raw') && a:opt.rgn.beg isnot a:opt.rgn.beg_raw
		return
	endif
	" TODO: Modify Is_escaped_tok to accept a pos (possibly optional) and use
	" it without modifying cursor pos.
	call cursor(a:opt.rgn.beg)
	" Assumption: beg/end_raw positions will *never* need adjustment here.
	" Rationale: A linewise operation deals with beg/end of lines, and hence,
	" cannot split escape/escapee pair; thus, the 'raw' pos variants shouldn't
	" even be in the struct.
	if s:Is_escaped_tok()
		" Assumption: Because tok was escaped, we know there's a preceding char.
		normal! h
		let [a:opt.rgn.beg[0], a:opt.rgn.beg[1]] = [line('.'), col('.')]
	endif
	call cursor(a:opt.rgn.end)
	if s:Is_escaping_tok()
		normal! l
		let [a:opt.rgn.end[0], a:opt.rgn.end[1]] = [line('.'), col('.')]
	endif
endfu
" >>>
" Function: s:Idx_to_tok() <<<
" TODO: Figure out where to put this, or whether it's even needed.
" TODO: Consider combining rgn and idx into a simple datatype.
fu! s:Idx_to_tok(rgn, idx)
	return nr2char(b:txtfmt_{a:rgn}_first_tok + a:idx)
endfu
" >>>
" Function: s:Is_empty() <<<
" Vim's empty() considers integer 0 as empty. This one doesn't.
fu! s:Is_empty(v)
	let t = type(a:v)
	if t == 0 || t == 5
		return 0
	else
		" Defer to Vim's empty.
		return empty(a:v)
	endif
endfu
" >>>
" Function: s:Is_pos_in_rng() <<<
" TODO: A couple of these may now be unnecessary.
fu! s:Is_pos_in_rng(p, pos_beg, pos_end)
	return a:p[0] >= pos[0] && a:p[0] <= pos[0]
		\&& (a:p[0] > pos[0] || a:p[1] >= pos[1])
		\&& (a:p[0] < pos[0] || a:p[1] <= pos[1])
endfu
" >>>
" Function: s:Is_pos_before_pos() <<<
fu! s:Is_pos_before_pos(p, pos)
	return a:p[0] < a:pos[0] || (a:p[0] == a:pos[0] && a:p[1] < a:pos[1])
endfu
" >>>
" Function: s:Is_pos_after_pos() <<<
fu! s:Is_pos_after_pos(p, pos)
	return a:p[0] > a:pos[0] || (a:p[0] == a:pos[0] && a:p[1] > a:pos[1])
endfu
" >>>
" Function: s:Cmp_pos_to_pos() <<<
fu! s:Cmp_pos_to_pos(p, pos)
	return s:Is_pos_before_pos(a:p, a:pos)
		\? -1
		\: s:Is_pos_after_pos(a:p, a:pos)
			\? 1 : 0
endfu
" >>>
" Function: s:Cmp_pos_to_rng() <<<
fu! s:Cmp_pos_to_rng(p, pos_beg, pos_end)
	return s:Is_pos_before_pos(a:p, a:pos_beg)
		\? -1
		\: s:Is_pos_after_pos(a:p, a:pos_end)
			\? 1
			\: 0
endfu
" >>>
" Function: s:Is_pos_valid() <<<
" Return true iff input position is valid.
fu! s:Is_pos_valid(pos)
	return !!col([a:pos[0], a:pos[1]])
endfu
" >>>
" Function: s:Get_at_offset() <<<
" Return position at signed (character) offset from input position.
" TODO: Add detailed documentation...
fu! s:Get_at_offset(offset, pos = [], what = '')
	let off_inc = a:offset < 0 ? 1 : -1
	if !empty(a:pos) && !s:Is_pos_valid(a:pos)
		" Invalid char position! Return something empty().
		return []
	endif
	let [line, col] = empty(a:pos) ? getpos('.')[1:2] : a:pos
	let off = a:offset
	let fwd = a:offset > 0
	let ltext = getline(line)
	let linelen = len(ltext)
	let idx = col - 1
	" Note: Unlike byteidx(), charidx() can't handle empty line.
	let cidx = !linelen ? 0 : charidx(ltext, idx)
	if !fwd
		" Bootstrap reverse stepping by finding the char *past* start.
		let cidx += 1
		let idx = byteidx(ltext, cidx)
		if idx < 0
			" At eol: make subsequent byte offset subtraction work by setting to
			" position just past eol.
			" Note: Because byteidx() can handle 1 char past eol on
			" non-empty line, this should happen only for empty line.
			let idx = linelen
		endif
	endif

	" Keep going across lines till offset char steps have been peformed or we
	" run out of room in the buffer.
	while 1
		" Move across line.
		while 1
			let cidx_next = cidx + (fwd ? 1 : -1)
			" Note: byteidx returns -1 for an invalid char index, but doesn't
			" consider index of 1 char past end to be invalid.
			let idx_next = byteidx(ltext, cidx_next)
			if idx_next < 0
				" Note: Algorithm ensures idx_next can't be negative in the
				" non-forward case.
				if fwd
					" Note: Because byteidx() can handle 1 char past eol on
					" non-empty line, this should happen only for empty line.
					let idx_next = linelen
				else
					throw "Internal error: Get_at_offset():"
								\ . " byteidx() unexpectedly returned -1 for reverse direction."
				endif
			endif
			if !off
				" This is the sought char.
				if fwd
					let byte = idx
					let sz = idx_next - idx
				else
					let byte = idx_next
					let sz = idx - idx_next
				endif
				if a:what == 'pos'
					return [line, byte + 1]
				else
					let ch = strpart(ltext, byte, sz)
					if empty(a:what)
						return [[line, byte + 1], ch]
					elseif a:what == 'char'
						return ch
					endif
				endif
				" Shouldn't get here!
				throw "Internal error: Invalid `what` value passed to s:Get_at_offset: " . a:what
			endif
			" Record step taken.
			let off += off_inc
			if fwd ? !idx_next || idx_next >= linelen : idx_next <= 0
				" Hit beginning or end of line.
				break
			endif
			" Prepare for next iteration.
			let cidx = cidx_next
			let idx = idx_next
		endwhile
		" Done with current line. Have we hit beginning or end of buffer?
		if fwd ? line >= line('$') : line <= 1
			" Hit beginning or end of buffer before completing requested number
			" of steps.
			" TODO: Decide how best to handle... Throw?
			return []
		endif
		" Advance/retreat to first/last char of next/prev line, taking care to
		" position on start of a char.
		let line += fwd ? 1 : -1
		let ltext = getline(line)
		let linelen = len(ltext)
		if fwd
			let cidx = 0
			let idx = 0
		else
			if linelen
				" Get char index 1 past end.
				let idx = linelen
				let cidx = charidx(ltext, linelen - 1) + 1
			else
				" Empty line is special: make cidx 1 past, but set idx to 0 so
				" that offset subtraction reflects zero-width char.
				let idx = 0
				let cidx = 1
			endif
		endif
	endwhile
endfu
" >>>
" Function: s:Vmap_apply_fmt() <<<
" TODO: Document...
" Return the idx resulting from application of fmt pspec to fmt idx
" TODO: Perhaps rename...
fu! s:Vmap_apply_fmt(pspec, idx)
	if type(a:pspec) == 0
		" Set
		" Note: Currently, spec parse converts special form `f-' to -1 rather
		" than 0. This may change.
		return a:pspec <= 0 ? 0 : a:pspec
	else
		" Add/sub
		return and(or(a:idx, a:pspec[0]), invert(a:pspec[1]))
	endif
endfu
" >>>
" Function: s:Get_hlable_patt() <<<
" Return regex matching any single char, highlightable by input tok_info.
" Note: tok_info is irrelevant when non-aggressive cleanup is configured; also,
" caller can override configuration by passing empty tok_info (forces
" non-aggressive mode).
" Inputs:
" tok_info: {} | {
"   rgn:  'fmt'|'clr'|'bgc',
"   idx:  <color-idx>|<fmt-mask>
" }
" TODO: Should this be a function, or perhaps just a Dict?
fu! s:Get_hlable_patt(tok_info)
	" VMAPS TODO: Make this a configurable option. Alternatively, get rid of
	" aggressive cleanup (as I'm thinking non-aggressive should be default).
	let aggressive = exists('g:txtfmt_aggressive_cleanup') && g:txtfmt_aggressive_cleanup

	" Logic: A non-tok, non-whitespace character is always hlable. When we're
	" not configured for aggressive cleanup, whitespace and completely blank
	" lines are also considered to be hlable. In aggressive cleanup mode,
	" completely blank lines are never hlable (since there's nothing to
	" highlight), and whitespace is hlable if and only if the the input tok_info
	" would have an effect (empty tok_info forces non-aggressive mode).
	" Rationale: Originally, was thinking a single newline would constitute
	" hlable; however, given that you can't position the cursor on a
	" newline, it makes more sense to require a blank line.
	" Caveat: Using line/col constraints won't work when testing for '\n'
	" between end of a blank line and beginning of next (since the '\n' is
	" considered to have col pos of 1.
	if aggressive && !empty(a:tok_info)
		let [rgn, idx] = [a:tok_info.rgn, a:tok_info.idx]
		if rgn != 'fmt' || idx == 0
			" No need to test specific format attributes
			" Design Decision: Default tok is special: whether it affects
			" subsequent whitespace depends upon what precedes it (think
			" 'bleed-through'). We exclude whitespace here because we want
			" supersedence logic to err on the side of considering a default tok
			" redundant, with 'bleed-through' logic (which forces non-aggressive
			" mode) making the final determination.
			let ws_hlable = rgn == 'bgc' ? 1 : 0
		else
			" Check specific fmt attributes.
			let ws_hlable_fmt_mask = or(or(or(
				\s:ubisrc_mask['u'], s:ubisrc_mask['s']), s:ubisrc_mask['r']), s:ubisrc_mask['c'])
			let ws_hlable = and(idx, ws_hlable_fmt_mask)
		endif
		let re_extra = (ws_hlable  ? '' : '\&\S')
	else
		" Non-aggressive mode
		" Note: This matches only a fully blank line, or one containing only
		" tokens: not one containing only whitespace, nor even a single newline
		" between lines.
		let re_extra = '\|^\%(' . b:txtfmt_re_any_tok . '\)*$'
	endif
	return b:txtfmt_re_any_ntok . re_extra
endfu
" >>>
" Function: s:Contains_hlable() <<<
" Return true iff the region between pos1 and pos2 (inclusivity/exclusivity
" specified by the inc arg) contains anything hlable, given the specified rgn
" type and tok idx.
" Note: When option specifies non-aggressive cleanup, tok_info will be ignored.
" TODO: Remove dependence on optional tok_info, now that I've pretty much ruled
" out 'aggressive cleanup'.
" Rationale: For non-aggressive cleanup, anything that occupies non-zero-width
" buffer space (nzwbs) (even whitespace and newlines) is treated as hlable.
" Inputs:
" pos1: start of region to test
" pos2: end of region to test
" inc:  <both-inclusive>|[<beg-inclusive>, <end-inclusive>]
" [tok_info]: {
"   rgn:  'fmt'|'clr'|'bgc',
"   idx:  <color-idx>|<fmt-mask>
" }
" Tested: 08Jan2015 (but modified 27Nov2015)
fu! s:Contains_hlable(pos1, pos2, inc, ...)
	let tok_info = a:0 ? a:1 : {}
	" Process inclusivity arg.
	let inc = type(a:inc) == 3 ? a:inc : [a:inc, a:inc]
	let hlable = s:Get_hlable_patt(tok_info)
	" Position cursor at start of range.
	" Note: Originally, generated both start and end were handled with
	" zwa, but this can be pathologically slow when cursor is past start pos in
	" very large file.
	let savepos = getpos('.')[1:2]
	call cursor(a:pos1)
	" Generate the end of region constraint.
	let zwa = s:Make_pos_zwa({'end': {'pos': a:pos2, 'inc': inc[1]}})
	" Since cursor is positioned at start, inc[0] determines 'c' flag.
	" Note: Vim does not use line/col zwa's to constrain search - only match;
	" thus, might make sense to abandon Make_pos_zwa and friends in favor of
	" approach that uses stopline and a post-search line/col test. For now, just
	" use pos2 as explicit stopline, to avoid reliance upon line/col constraints
	" to short-circuit search. (Even without the explicit stopline, we wouldn't
	" need to worry about pathological slowness now that we're starting search
	" at beginning of search region.)
	let ret =
		\!!search(s:Apply_zwa(zwa, hlable), 'nW' . (inc[0] ? 'c' : ''),
		\a:pos2[0])
	" Restore cursor pos.
	call cursor(savepos)
	return ret
endfu
" >>>
" Function: s:Add_byte_offset() <<<
" Add specified byte offset to the input pos and return the adjusted pos.
" Inputs:
" pos:    [lnum, col]
" offset: <signed # of bytes>
" Return: [adj_lnum, adj_col]
" Note: It's possible that the returned position will point just past end of
" file or end of a line. The intended use case is such that this is not a
" problem (since cursor() can handle such positions). However, if we needed to
" guarantee valid char position, could use a save/restore around cursor() to
" find it.
fu! s:Add_byte_offset(pos, offset)
	" Handle trivial special case.
	if !a:offset | return a:pos | endif
	let [lnum, col] = a:pos
	let bi = line2byte(lnum) + col - 1
	let bi_tgt = bi + a:offset
	" Caveat!: Docs specifically allow use of line past end with line2byte,
	" but passing a byte past end to byte2line returns -1: hence, the special
	" logic...
	if bi_tgt < 0
		let bi_tgt = 1
		let lnum_tgt = 1
		let bi_bol_tgt = 1
	else
		let bi_eof = line2byte(line('$') + 1)
		if bi_tgt >= bi_eof
			" Byte offset past end - byte2line can't handle.
			" Note: Ok for byte index to be 1 past end of buffer; this will
			" simply result in col position past end of final line.
			let bi_tgt = bi_eof
			let lnum_tgt = line('$')
		else
			let lnum_tgt = byte2line(bi_tgt)
		endif
		let bi_bol_tgt = line2byte(lnum_tgt)
	endif
	return [lnum_tgt, bi_tgt - bi_bol_tgt + 1]
endfu
" >>>
" Function: s:Make_pos_zwa() <<<
" Generate and return a zero-width assertion (in plain string form) based on
" the input beg/end positions.
" Note: Apply_zwa is provided as a convenience for wrapping it onto a regex.
" Inputs:
" cfg: {
"   [beg|end]: {
"     pos: [lnum, col], %% position of region endpt
"     [inc]: 0|1        %% 1 if region endpt inclusive (default 0)
"   },
fu! s:Make_pos_zwa(cfg)
	" Assumption: beg < end
	" Note: \%>123l refers to the *start* byte index of a multi-byte char.
	" Exclusive Template: (l1 & >c1 | >l1) & (l2 & <c2 | <l2)
	" Note: To make the exclusive template inclusive, make the following
	" substitutions:
	" >c1 => (c1 | >c1)
	" <c2 => (c2 | <c2)
	let res = []
	for [side, cfg] in items(a:cfg)
		if side =~ 'beg\|end'
			let [l, c] = cfg.pos
			let gtlt = side == 'beg' ? '>' : '<'
			let re_col = '\%' . gtlt . c . 'c'
			if has_key(cfg, 'inc') && cfg.inc
				let re_col = '\%(\%' . c . 'c\|' . re_col . '\)'
			endif
			call add(res, '\%(\%' . l . 'l\&' . re_col . '\|\%' . gtlt . l . 'l\)')
		endif
	endfor
	return join(res, '\&')
endfu
" >>>
" Function: s:Apply_zwa() <<<
fu! s:Apply_zwa(zwa, patt)
	return !empty(a:zwa) ? a:zwa . '\&\%(' . a:patt . '\)' : a:patt
endfu
" >>>
" Function: s:Vmap_sync_start() <<<
" Working backwards from start of region, accumulate tokens until we find one
" that cannot be superseded (i.e., has hlable between it and subsequent tok (or
" region head); if such a token cannot be found, we prepend a positionless
" virtual tok that has the correct highlighting (and by definition, cannot be
" superseded, as it is not an actual tok).
" Post Condition: First 'tok' in list (possibly virtual) cannot be superseded.
" Note: Although Search_tok can return a tok with type 'hla', this function will
" not: if we hit hlable trying to find ss_safe tok, we prepend a positionless
" virtual tok.
" Note: If we must prepend a positionless virtual tok, we will need to use
" synstack to get the effective highlighting unless backwards search hit start
" of buffer, in which case, we know the effective highlighting without
" consulting synstack.
" Position Note: Originally, cursor was left at point at which subsequent
" forward search should start, with a flag set to indicate whether search should
" include cursor position. Now, however, subsequent logic can get the
" information it needs from toks: if final tok is actual tok, start on it and
" disallow match at cursor; otherwise, start at vsel head and allow match at
" cursor.
" TODO: Document inputs, etc...
fu! s:Vmap_sync_start(rgn, opt)
	let sel_beg_pos = a:opt.rgn.beg
	let sel_end_pos = a:opt.rgn.end
	let stopline_beg = s:Add_byte_offset(sel_beg_pos, -s:SYNC_DIST_BYTES)[0]
	" For good measure, make sure stopline is past current line.
	let stopline = max([sel_end_pos[0] + 1,
				\s:Add_byte_offset(sel_end_pos, s:SYNC_DIST_BYTES)[0]])
	" Start looking at head of vsel.
	call cursor(sel_beg_pos)
	let ss_safe = 0
	let toks = []
	let tip = {}
	" Look backward for an ss_safe tok within stop distance.
	" Design Decision: Search_tok can return actual tok prior to stopline, but
	" once we've exceeded stopline, the nearest of hlable and tok will be
	" returned. The only scenario in which we continue finding toks after having
	" exceeded stopline is the one involving a run of adjacent toks with no
	" separating hlable. In such cases, we want to keep going, stopline
	" notwithstanding, till we hit hlable or start of buffer, at which point,
	" we'll be prepending a positionless virtual tok with the effective
	" highlighting at that point. (Note that hlable, if found, won't allow us to
	" prepend an actual tok, since hlable can make only a *preceding* tok
	" ss_safe, and we've already given up on finding a preceding tok at that
	" point.
	while !ss_safe
		let ti = s:Search_tok(a:rgn, 'b', stopline_beg)
		if ti.typ != 'tok'
			" Couldn't find ss safe tok within stop distance: either hit start
			" of buf, or found hla prior to stopline.
			break
		endif
		" Add found tok, whether supersedence constraint satisfied or not.
		call insert(toks, ti)
		" Has this tok satisfied supersedence constraint?
		" Look only up to next tok (if one was found) or head of vsel.
		if s:Contains_hlable(ti.pos, empty(tip) ? sel_beg_pos : tip.pos, [0, 0])
			let ss_safe = 1
		endif
		" Cache prev for next iteration.
		let tip = ti
	endwhile
	" Did we find ss_safe tok or not?
	if !ss_safe
		" Did we hit start of buf?
		let hit_sob = ti.typ == 'eob'
		" Note: We're going to have to prepend a virtual tok to satisfy
		" supersedence constraint.
		" TODO: Make sure the virtual tok doesn't need more of the stuff
		" Search_tok normally returns. E.g., what about 'tok'? Should it be set,
		" or left empty (as for <eob>).
		let ti = {'typ': 'tok', 'pos': [], 'rgn': a:rgn}
		" Note: 'eob' in this context means *start* of buffer.
		if hit_sob || empty(s:Backward_char())
			" Either search hit head of buffer, or we're sitting at it; in
			" either case, we prepend default tok.
			let ti.idx = 0
		else
			" Sitting 1 char prior to either vsel head or actual tok.
			" Rationale: Backwards searches don't move to hlable or head of buf;
			" thus, the Backward_char() call above was made either from original
			" position at start of region or from actual found tok.
			" Sync on synstack.
			let ti.idx = s:Get_cur_rgn_info()[a:rgn]
		endif
		" TODO: Is actual char really needed in the struct? Remove if not used.
		let ti.chr = s:Idx_to_tok(a:rgn, ti.idx)
		call insert(toks, ti)
	endif

	" TODO: Consider whether this function should be responsible for fixing
	" 'stopline'. If not, could simply return the list of toks.
	return {
		\'stopline': stopline,
		\'toks': toks
	\}
endfu
" >>>
" Function: s:Vmap_collect() <<<
" Note: This function should augment tok_info with loc and action: i.e., in
" addition to the basic tok returned by Search_tok, the toks in the returned
" list will have the following:
"   loc    =~ '^[<{=}>]$'
"             < tok before region
"             { phantom head tok
"             = tok within region
"             } phantom tail tok
"             > tok after region
"          TODO: Consider treating non-phantom `{'s and `}'s as `='
"   action =~ '^[ia]\?$'
"             i              insert
"             a              append
"             <empty string> no action
"          Note: action may be assigned other values in Vmap_apply, but for now,
"          phantom head/tail will be set to i/a respectively, with all others
"          empty.
" VMAPS TODO: Update function header (document inputs, etc.).
" Possible Refinement TODO: Although it's no longer harmful (now that Vmap_apply
" knows to discard them), there's no need to add phantom toks (neither head/tail
" nor interior) for a rgn type represented only in the selector. We're no longer
" adding unnecessary interior phantoms, but as of 19Mar2016, we still add the
" head/tail ones unconditionally. Consider simply skipping the call to this
" function for rgn type's not in pspecs.
fu! s:Vmap_collect(rgn, sync_info, pspecs, opt)
	let toks = a:sync_info.toks
	" Mark toks added during sync as being prior to vsel.
	" TODO: Consider doing this in Search_tok itself...
	for tok in toks
		let tok.loc = '<'
		let tok.action = ''
	endfor
	" Final tok in list (latest in buf) is our starting point.
	let tok = toks[-1]
	if !empty(tok.pos)
		" Actual sync tok: start on it and disallow cur pos match.
		call cursor(tok.pos)
		let allow_cmatch = 0
	else
		" No actual toks found before vsel: start at head of vsel and allow
		" cur pos match.
		call cursor(a:opt.rgn.beg)
		let allow_cmatch = 1
	endif
	" Cache rgn types for which we may need to add interior phantoms.
	let other_rgns = filter(keys(a:pspecs.rgns), 'v:val != a:rgn')
	" Initialize loop vars.
	let ti_last = {}
	let sel_cmp_prev = -1
	let [sel_beg_found, sel_end_found, sel_end_found_prev] = [0, 0, 0]
	while 1
		" Look for next tok within stopline range (not necessarily in region)
		let ti = s:Search_tok(a:rgn, allow_cmatch ? 'c' : '', a:sync_info.stopline)
		let allow_cmatch = 0
		" Determine positioning of current tok wrt region:
		" -1=before 0=in 1=after
		let sel_cmp = ti.typ != 'tok'
			\? 1
			\: s:Cmp_pos_to_rng(ti.pos, a:opt.rgn.beg, a:opt.rgn.end)
		" Check for transitions wrt region.
		if sel_cmp_prev < sel_cmp
			" We've advanced w.r.t. region.
			if !sel_beg_found
				" Need to add something for head: either a phantom (here) or
				" augmented tok (end of loop).
				let sel_beg_found = 1
				" 29Nov2022_Update: Add unconditionally and clean up if it pans
				" out...
				"" If we didn't find tok at all, or found one not at region head,
				"" add phantom.
				"if ti.typ != 'tok' || ti.pos != a:opt.rgn.beg
					" Add phantom tok for region beg
					call add(toks, {
						\'typ': 'tok',
						\'rgn': a:rgn,
						\'pos': a:opt.rgn.beg[:],
						\'idx': -1,
						\'tok': '',
						\'loc': '{',
						\'action': 'i'
					\})
				"endif
			endif
			if !sel_end_found && sel_cmp == 1
				let sel_end_found = 1
				" Add phantom tail.
				" Note: sel_cmp == 1 precludes actual } tok.
				call add(toks, {
					\'typ': 'tok',
					\'rgn': a:rgn,
					\'pos': a:opt.rgn.end[:],
					\'idx': -1,
					\'tok': '',
					\'loc': '}',
					\'action': 'a'
				\})
				if ti.typ != 'tok'
					" Since we didn't find a tok before stopline, our next
					" search will look for hla between phantom tail and eob.
					" Assumption: allow_cmatch has been cleared definitively.
					call cursor(a:opt.rgn.end)
				endif
			endif
		endif
		if ti.typ == 'tok'
			" We have an actual (non-phantom) tok.
			" Note: Phantom toks are always added above.
			" Note: Originally, we set loc to '}' for non-phantom tok at end of
			" selection, but this resulted in buggy corner cases in delete case.
			" Simple (albeit wasteful) solution is to append phantom tok
			" unconditonally at region end and allow token cleanup to remove the
			" extras.
			let ti.loc = sel_cmp < 0 ? '<' : sel_cmp > 0 ? '>' : '='
			" Finish updating and add the current tok.
			let ti.action = ''
			if ti.loc == '}' | let sel_end_found = 1 | endif
			if ti.loc == '{' | let sel_beg_found = 1 | endif
			call add(toks, ti)
			if ti.loc == '>'
				" Now that we're past region, do we already have a candidate
				" 'last' tok?
				" Note: ti_last is something of a misnomer now that we're
				" keeping the final found tok; penultimate would be a better
				" name now.
				if !empty(ti_last)
					" Any hlable between ti_last and this one? If so, it's safe
					" to break out now that we've added this final tok.
					" Design Decision: Originally, discarded this tok, but since
					" we've already got it, err on the side of more cleanup.
					if s:Contains_hlable(ti_last.pos, ti.pos, [0, 0])
						break
					endif
				endif
				" Current tok is needed, and becomes candidate for 'last tok'.
				let ti_last = ti
			endif
			" Interior Phantom Logic:
			" If a non-null selector was specified, head/tail phantoms are not
			" sufficient: we also need 'interior phantoms' for all rgn types
			" represented in specs, to permit discontinuities at tokens of
			" 'other' rgn types: for example, if fmt changes are qualified by a
			" selector matching clr == 'red', simply passing into or out of a
			" red clr region could entail a change to fmt attrs.
			" Note: Interior phantoms are not needed or wanted in the smart
			" delete case.
			if empty(ti.action) && ti.loc =~ '[{=]'
				\ && a:opt['op'] != 'delete'
				for rgn in other_rgns
					" Add a phantom.
					" Note: Action 'a' is used (rather than 'i') to ensure that
					" Vmap_apply has seen the actual tok before it decides how
					" to disposition the interior phantom.
					call add(toks, {
						\'typ': 'tok',
						\'rgn': rgn,
						\'pos': ti.pos,
						\'idx': -1,
						\'tok': '',
						\'loc': '=',
						\'action': 'a'
					\})
				endfor
			endif
			" Update for next iteration...
			let sel_cmp_prev = sel_cmp
		else
			" No more tokens in range (and hit hlable or end of buffer before
			" finding one past range).
			" Special Case: If we hit end of buffer without even an hlable, add
			" a special <eob> tok; what's special about this virtual tok is that
			" it *always* supersedes an immediately preceding tok.
			" TODO: Decide whether <eob> virtual tok should be handled specially
			" (e.g., built entirely or not at all by Search_tok). Could use a
			" sentinel as simple as an empty object for <eob>. Also, consider
			" changing the 'typ' field to a pair of flags: tok and hla.
			if ti.typ == 'eob'
				" Note: Virtual marker 'token' returned by Search_tok doesn't
				" have rgn type; give it one.
				let ti.rgn = a:rgn
				let ti.idx = -1
				" Note: Setting position and action simplify the cleanup logic,
				" allowing it to treat <eob> like any other tok for ss test.
				" Note: This was added as bugfix (hotfix actually); might want
				" to look at refactoring...
				let ti.pos = [line('$'), col([line('$'), '$'])]
				let ti.action = ''
				call add(toks, ti)
			endif
			if ti.typ == 'eob' || sel_end_found_prev
				" Either we've already hit 'eob' or most recent search started
				" at end of region and found hla before eob; in either case, we
				" have all we need. Note that there could be another tok past
				" stopline, but we would have discarded it anyways if we'd found
				" it (see earlier break).
				break
			endif
		endif
		" Update for next iteration...
		let sel_cmp_prev = sel_cmp
		let sel_end_found_prev = sel_end_found
	endwhile
	return toks
endfu
" >>>
" Function: s:Vmap_apply() <<<
" Old/new_idx logic:
" old_idx: the idx that *was* active at a given point prior to the highlighting
"          operation (considering effect of tok at point).
" new_idx: the idx that *will be* active at a given point *after* the
"          highlighting operation (considering effect of previous tok, but
"          ignoring effect of tok at point). In other words... new idx is
"          effectively double-buffered, with new_idx representing previous state
"          and set_idx representing next state. Note: When we process a given
"          tok (prior to end of loop update), new_idx reflects everything that
"          came before that tok (taking highlighting operation into account),
"          but not the tok itself.
" set_idx: Set upon each iteration to the value a tok will have *after* the
"          highlighting operation is complete (considering effect of tok at
"          point).
"          Note: Whenever set_idx is assigned a value >=0, that same value will
"          be transferred to new_idx at loop end; deferring the new_idx
"          assignment allows us to defer the tok update logic (which needs to
"          know both prev (new_idx) and next (set_idx) states, because it
"          currently handles removal of both redundant toks and unnecessary
"          phantom toks).
"          Possible TODO: If we didn't bother with redundant/unnecessary tok
"          removal here (cleanup already handles), we could probably do away
"          with set_idx and the deferred set of new_idx.
" Implementation Note: old/new_idx implemented as hashes to facilitate looping
" over all rgn types at once.
fu! s:Vmap_apply(pspecs, toks, opt)
	let is_del = a:opt.op == 'delete' ? 1 : 0
	" Initialize to sentinel so that we know when we encounter first of a type.
	" Note: Having unused keys in old_idx is harmless; rgn types present in
	" a:toks determines which keys will be used.
	let old_idx = {'fmt': -1, 'clr': -1, 'bgc': -1}
	let new_idx = {}
	for tok in a:toks
		" Skip non-tokens (e.g., <eob>).
		" Assumption: Vmap_sync_start guarantees that leading element will
		" always be of type 'tok' (albeit possibly positionless virtual). There
		" will be at most one non-tok (an <eob>) of each rgn type, and the
		" current merge logic ensures any <eob>'s will be at the end of the
		" list: thus, when we encounter the first, nothing but <eob>'s remains.
		if tok.typ != 'tok' | continue | endif
		" REFACTOR_TODO: Consider skipping (with continue) toks whose rgn type
		" is neither in pspecs nor selector, which have been collected solely to
		" facilitate cleanup.

		" Boot-strap each rgn type.
		if old_idx[tok.rgn] < 0
			let old_idx[tok.rgn] = tok.idx
			let new_idx[tok.rgn] = tok.idx
			continue
		endif
		let set_idx = -1
		if tok.loc == '<'
			" Change nothing prior to region, but do stay in sync.
			let old_idx[tok.rgn] = tok.idx
			let new_idx[tok.rgn] = tok.idx
		elseif tok.loc == '{' || tok.loc == '='
			" REFACTOR_TODO: Validate combining { and = cases.
			if empty(tok.action)
				" Existing tok - not phantom
				let old_idx[tok.rgn] = tok.idx
			endif
			" The has_key test below is meant to ensure that we discard a
			" phantom whose rgn type isn't represented in pspecs, and that we do
			" so without performing selector test, and even more importantly,
			" without attempting to apply fmt/clr (since the attempt would fail
			" for rgn type not present in pspecs).
			" Note: I'm no longer adding interior phantoms for rgn types not
			" present in pspecs, so the has_key test should be superfluous.
			if has_key(a:pspecs.rgns, tok.rgn)
				\ && s:Check_selector(a:pspecs.sel, old_idx)
				" Selected! Apply highlighting.
				if tok.rgn == 'fmt'
					let set_idx = s:Vmap_apply_fmt(a:pspecs.rgns.fmt, old_idx.fmt)
				else
					"clr/bgc
					let set_idx = a:pspecs.rgns[tok.rgn]
				endif
			else
				" Either tok is unselected or its rgn type isn't represented in
				" pspec; in either case, ensure that highlighting is unchanged
				" from what it was before.
				let set_idx = old_idx[tok.rgn]
			endif
		elseif tok.loc == '}'
			if tok.action != 'a'
				" Existing tok - not phantom
				" FIXME: This shouldn't be possible any more...
				let old_idx[tok.rgn] = tok.idx
			endif
			let set_idx = old_idx[tok.rgn]
		else " past vsel
			" Assumption: Once we hit first tok past region, all remaining toks
			" must also be past region (now that <eob>'s are floated to end by
			" merge).
			" Caveat: Since each <eob> affects only its own rgn type, it's not
			" strictly necessary to float them to the end in this manner, but if
			" we didn't, we couldn't break here.
			break
		endif
		" Handle any required tok update/delete/discard.
		if set_idx >= 0
			" Action *may* be needed w.r.t. current tok.
			if empty(tok.action)
				" Existing tok
				if set_idx == new_idx[tok.rgn]
					" This one is redundant.
					" Design Decision Needed: Should we do this here because
					" it's easy, even though cleanup would handle if we left it?
					" Same question for unnecessary phantom logic below.
					let tok.action = 'd'
				elseif set_idx != tok.idx
					" Need to change existing tok.
					let tok.idx = set_idx
					let tok.action = 'r'
				endif
			else
				 " Phantom tok
				 " TODO: Decide whether to set phantoms unconditionally and
				 " allow cleanup to remove redundancies, or whether to clear
				 " action and ensure a cleared action doesn't cause anything to
				 " be added to buffer (currently, it mistakenly does).
				 if set_idx != new_idx[tok.rgn]
					 let tok.idx = set_idx
				 else
					 " Phantom unnecessary
					 let tok.action = ''
				 endif
			endif
			" Update for next iteration unless we're deleting and this tok is
			" within the delete range, in which case, we don't update new_idx
			" since we can't rely on such toks to affect anything past them.
			if !is_del || tok.loc != '='
				let new_idx[tok.rgn] = set_idx
			endif
		endif
	endfor
endfu
" >>>
" Function: s:Range_spans_pos() <<<
" Return true iff 'range' spans 'pos'.
" Note: Second component may be either an exclusive end pos or a length in
" bytes, in which case, range must not span multiple lines.
fu! s:Range_spans_pos(range, pos)
	let [beg, end] = a:range
	" Question: Should we validate range and pos?
	if type(end) == v:t_number
		" Convert provided byte count to pos.
		" Assumption: Range can't span line.
		let end = [beg[0], beg[1] + end]
		if end[1] > col([beg[0], '$'])
			throw "Internal error: s:Range_spans_pos():"
						\ . " range specified by length cannot be multiline."
		endif
	endif
	" Is pos within exclusive-at-end range?
	return a:pos[0] >= beg[0] && a:pos[0] <= end[0]
				\ && a:pos[1] >= beg[1]
				\ && a:pos[1] < end[1]
endfu
" >>>
" Function: s:Create_del_pos_adjuster() <<<
" Create and return an object that can be used to adjust positions for toks and
" bslash ranges beyond a delete range.
fu! s:Create_del_pos_adjuster(opt)
	let ret = {'vars': {}}
	" Make line/col adjustments needed to account for removal of deleted text
	" (and toks).
	let [beg, end] = [a:opt.rgn.beg, a:opt.rgn.end]
	let linewise = has_key(a:opt.rgn, 'beg_raw') && beg isnot a:opt.rgn.beg_raw
	let beg_line_len = strlen(getline(beg[0]))
	let end_line_len = strlen(getline(end[0]))
	let ret.beg = beg
	let ret.end = end
	let ret.eol_at_end = end[1] > end_line_len
	" Assumption: clen isn't used in linewise cases.
	let ret.end_clen = ret.eol_at_end ? 0 : strlen(s:Get_char(end))

	fu! ret.Adjust(bsr_or_pos) dict
		let is_tok = has_key(a:bsr_or_pos, 'pos')
		let pos = is_tok ? a:bsr_or_pos.pos : a:bsr_or_pos.beg

		if !self.eol_at_end
			if pos[0] == self.end[0]
				" Column adjustment needed
				if self.beg[0] == self.end[0]
					" Intra-line delete
					let pos[1] -= self.end[1] - self.beg[1]
						\ + (pos[1] > self.end[1] ? self.end_clen : 0)
				else
					" Add remaining chars from beg line and subtract deleted
					" chars from end line.
					let pos[1] += self.beg[1] - self.end[1]
						\ - (pos[1] > self.end[1] ? self.end_clen : 0)
				endif
				if is_tok
					let tok = a:bsr_or_pos
					" Convert an append at col 0 to an insert at col 1.
					if pos[1] == 0
						let pos[1] = 1
						if tok.action == 'a'
							let tok.action = 'i'
						endif
					elseif tok.action == 'a' && tok.loc == '}'
						" TODO: Maybe ensure this is set to first byte of mb
						" char? Probably not necessary, since cursor(line, col)
						" allows col to be any byte within char, but keeping
						" things in canonical form could prevent confusion.
						let pos[1] -= 1
					endif
				endif
			endif
		endif
		" Perform line adjustments if delete is *effectively* multi-line
		" (i.e., true multi-line or 1 line plus trailing newline).
		if self.beg[0] != self.end[0] || self.eol_at_end
			if pos[0] >= self.end[0]
				let pos[0] -= self.end[0] - self.beg[0] + (self.eol_at_end ? 1 : 0)
			endif
		endif
	endfu
	return ret
endfu
" >>>
" Function: s:Collect_bslash_ranges() <<<
fu! s:Collect_bslash_ranges(opt, sync_info, toks)
	" 2D hash: [line, col]
	" ...where col is the column past the sequence of backslashes
	let ret = {}
	" Note: This pattern includes the discarded pre-text only to facilitate
	" finding the next start index.
	" TODO: Consider using searchpos() in conjunction with matchlist().
	let re = '\([^\\]*\)\(\\\+\)\([' . b:txtfmt_re_any_tok_atom . ']\)\?'

	" Initialize object that can be used to adjust position of bslash ranges
	" beyond deletion range.
	let bsr_adj = a:opt.op == 'delete' ? s:Create_del_pos_adjuster(a:opt) : {}
	" Start at beginning of line containing either first actual tok found in
	" sync or beginning of region.
	" FIXME: How to use toks here? We have all rgn types but probably care only
	" about the earliest of all...
	let pos = empty(a:toks[0].pos) ? a:opt.rgn.beg : a:toks[0].pos
	let line = pos[0]
	" TODO: Verify that this test is correct: in particular, do we need to
	" include stopline?.
	" TODO: Consider starting at particular character on line...
	while line <= a:sync_info.stopline
		let ltext = getline(line)
		let linelen = len(ltext)
		let idx = 0
		" Loop over ranges on current line.
		while idx < linelen
			let mlist = matchlist(ltext, re, idx)
			if !empty(mlist)
				" Found a bslash sequence.
				" Note: bslashes is the only field that can't be empty.
				let [all, discard, bslashes, tok; _] = mlist
				let special = !!len(tok)
				let num_slashes = len(bslashes)
				" Determine col of start of range.
				let range_col = idx + len(discard) + 1
				" Note: For test of end against range, we use one char position
				" *beyond* end to reflect fact that append at end would put end
				" pos within the first range.
				let beg = a:opt.rgn.beg[:]
				let end = a:opt.rgn.end[:]
				let end_plus1 = s:Get_at_offset(1, end, 'pos')
				" Break the range into up to 3 segments, as necessary.
				for pos in [beg, end_plus1, []]
					" Create an entry that may be truncated (or discarded)
					" subsequently.
					let entry = { 'count': num_slashes
								\,'special': !!len(tok)
								\,'beg': [line, range_col] }
					" Is this the end sentinel or an actual region boundary?
					if !empty(pos)
						" Does the range require splitting?
						let need_split = s:Range_spans_pos([entry.beg, entry.count], pos)
						if !need_split
							continue
						endif
						let entry.count = pos[1] - range_col
					endif
					" Don't add an empty range.
					if !entry.count
						continue
					endif
					let num_slashes -= entry.count
					let range_col += entry.count
					" Don't add range that's within deletion region.
					" Assumption: Loop logic above ensures range stored in entry
					" will not straddle either end of deletion region.
					" FIXME!!!!: This test is wrong - it discards the range just
					" *after* end of region!!!!!
					if a:opt.op == 'delete'
						if !s:Cmp_pos_to_rng(entry.beg, beg, end)
							"""echomsg "Skipping range at " . string(entry.beg)
							continue
						endif
						" Adjust the positions before determining entry's
						" index/key in array/map.
						call bsr_adj.Adjust(entry)
					endif
					" Add the entry to 2-level hash of ranges keyed by line,
					" then the col just past the end of the range.
					" Important Note: The range *must* be adjusted before its
					" line/col is used.
					if !has_key(ret, entry.beg[0])
						let ret[entry.beg[0]] = {}
					endif
					let ret[entry.beg[0]][entry.beg[1] + entry.count] = entry
				endfor

				" Advance past the entire match before trying again.
				let idx += len(all)
			else
				" Finished with line
				break
			endif
		endwhile
		let line += 1
	endwhile
	"""echomsg "Returning " . string(ret)
	return ret
endfu
" >>>
" Function: s:Vmap_compute() <<<
fu! s:Vmap_compute(pspecs, opt)
	let toks = {}
	" Since the addition of selectors, it is necessary to perform syncing and
	" collection for all rgn types represented in *either* pspecs *or* selector.
	" We could limit ourselves to the union of pspecs/selector rgn types, but
	" there's an advantage to syncing/collecting unconditionally: namely, it
	" permits cleanup on *all* rgn types, even those not in pspecs/selector.
	for rgn in s:Get_active_rgns()
		let sync_info = s:Vmap_sync_start(rgn, a:opt)
		let toks[rgn] = s:Vmap_collect(rgn, sync_info, a:pspecs, a:opt)
		call s:dbg_display_toks("Vmap_collect", toks[rgn])
	endfor

	" Convert hash keyed by rgn type (generated above) to single array.
	let mtoks = s:Vmap_merge_toks(toks, sync_info, a:opt)
	call s:dbg_display_toks("after Vmap_merge_toks", mtoks)

	if b:txtfmt_cfg_escape == 'bslash'
		let a:opt['bslashes'] = s:Collect_bslash_ranges(a:opt, sync_info, mtoks)
		let g:debug = a:opt.bslashes
	endif

	" Note: Currently, pspecs.rgns cannot be empty: if it were,
	" Parse_fmt_clr_transformer would have returned empty, resulting in
	" operation cancellation.
	if !s:Is_empty(a:pspecs.rgns)
		" Apply pspec without worrying about whether mtoks will be kept.
		call s:Vmap_apply(a:pspecs, mtoks, a:opt)
	endif
	call s:dbg_display_toks("after Vmap_apply", mtoks)
	return mtoks
endfu
" >>>
" Function: s:Vmap_cmp_loc() <<<
" Compare locations of input tokens
fu! s:Vmap_cmp_loc(a, b)
	let actions = {'<': 0, '{': 1, '=': 2, '}': 3, '>': 4}
	let [a_prec, b_prec] = [actions[a:a], actions[a:b]]
	return a_prec < b_prec ? -1 : a_prec > b_prec ? 1 : 0
endfu
" >>>
" Function: s:Vmap_cmp_pos() <<<
" Compare input positions (format: [row, col])
fu! s:Vmap_cmp_pos(p_a, p_b)
	let [al, bl, ac, bc] = [a:p_a[0], a:p_b[0], a:p_a[1], a:p_b[1]]
	if al < bl
		return -1
	elseif al > bl
		return 1
	else
		" Consider col
		if ac < bc
			return -1
		elseif ac > bc
			return 1
		else
			return 0
		endif
	endif
endfu
" >>>
" Function: s:Vmap_cmp_tok() <<<
" Compare input tokens, considering first pos, then action iff necessary to
" break tie.
fu! s:Vmap_cmp_tok(t_a, t_b)
	" <eob> toks are special: they should always be last tok within a rgn type,
	" and they should all float to the end.
	if a:t_a.typ == 'eob'
		return 1
	elseif a:t_b.typ == 'eob'
		return -1
	endif
	" Design Decision: If either tok is positionless (but not <eob>), preserve
	" order.
	if !has_key(a:t_a, 'pos') || empty(a:t_a.pos)
		return -1
	elseif !has_key(a:t_b, 'pos') || empty(a:t_b.pos)
		return 1
	endif
	" No special case: compare actual positions.
	let cmp = s:Vmap_cmp_pos(a:t_a.pos, a:t_b.pos)
	if cmp
		" Position decides.
		return cmp
	endif

	" Give 'loc' a chance to break tie.
	let cmp = s:Vmap_cmp_loc(a:t_a.loc, a:t_b.loc)
	if cmp
		" loc decides.
		return cmp
	endif

	" Consider action to break positional tie.
	" Possible values: i|a|r|d
	" Assumption: Empty actions have been discarded.
	" FIXME: I don't think the assumption above is true any more.
	" Assumption: For tokens of same type, [ia] and [rd] are
	" mutually-exclusive sets: i.e., one from each set could exist at same
	" pos, but not multiples from same set. For hetereogeneous types, the sets
	" are not mutually-exclusive, but the ordering logic must still be applied
	" when the heterogeneous toks fall into different groups. When ordering is
	" inconsequential, be deterministic in preferring not to re-order a and b.
	" Question FIXME: Why are we checking for internal error here????
	let [act_a, act_b] = [a:t_a.action, a:t_b.action]
	" Test neg. return cases first to give preference to input order.
	if act_a == 'i'
		return -1
	elseif act_b == 'a'
		return -1
	elseif act_a == 'a'
		return 1
	elseif act_b == 'i'
		return 1
	elseif t_a.rgn != t_b.rgn
		" Ordering is inconsequential. Just be deterministic.
		" (TODO - which way?)
		return -1
	else
		throw printf("Internal error: mutually-exclusive actions %s, %s at position [%d, %d].",
			\act_a, act_b, a:t_a.pos[0], a:t_a.pos[1])
	endif
endfu
" >>>
" Function: s:Vmap_merge_toks() <<<
" Convert hash of tok arrays, keyed by rgn type, to a single, ordered array of
" toks, with rgn type stored in each element.
fu! s:Vmap_merge_toks(toks, sync_info, opt)
	let mtoks = []
	" Build array of the arrays to be merged.
	let aofa = []
	for toks in values(a:toks)
		if !empty(toks)
			call add(aofa, toks)
		endif
	endfor
	" As long as there are arrays to be merged...
	" Possible TODO: There are definitely more efficient ways to do this, but
	" I'm not convinced optimization is called for.
	while !empty(aofa)
		let sel_idx = -1
		" Find the array whose head tok comes next in buffer.
		for idx in range(len(aofa))
			" Compare this one with prev best (if any).
			" Caveat: In general, ordering of position-less toks matters only
			" within a rgn type, but we need to pull them as soon as possible;
			" otherwise, they can 'trap' normal toks behind them, thereby
			" preventing the normal toks from being merged into the stream at
			" the right point.
			" Special Exception: <eob> toks should always float to the end.
			" TODO: Vmap_cmp_tok handles this special logic, but consider
			" whether it belongs here, with Vmap_cmp_tok called only for toks
			" with position...
			let cmp = sel_idx < 0
				\ ? -1
				\ : s:Vmap_cmp_tok(aofa[idx][0], aofa[sel_idx][0])
			if cmp < 0
				let sel_idx = idx
			endif
		endfor
		" Merge selected tok into return array.
		call add(mtoks, aofa[sel_idx][0])
		" Remove processed tok (and possibly containing array).
		if len(aofa[sel_idx]) == 1
			" Remove tok and containing array, which is now empty.
			call remove(aofa, sel_idx)
		else
			call remove(aofa[sel_idx], 0)
		endif
	endwhile
	return mtoks
endfu
" >>>
" Function: s:Vmap_rev_and_merge() <<<
" Reverse toks and merge with mtoks (assumed to be reversed/merged already).
" TODO: Probably should just merge in-place.
" Tested: 18Apr2015 (the functional (non-merge-in-place) version)
fu! s:Vmap_rev_and_merge(mtoks, toks, opt)
	if empty(a:toks)
		" Special case: Nothing to merge.
		" Note: For consistency with non-short-circuit case, return a copy.
		" TODO: Inefficiency of this case bolsters case for performing
		" in-place merge...
		return copy(a:mtoks)
	endif
	if empty(a:mtoks)
		" Special case: Merging into empty list. Short-circuit.
		return reverse(filter(copy(a:toks), '!empty(v:val.action)'))
	endif
	" Cache some convenience refs.
	let [mtoks, toks, mtoks_ret] = [a:mtoks, a:toks, []]
	" Iterate mtoks (in descending buffer order), peeling off toks from end of
	" toks (still ordered in ascending buffer order) for merge where
	" appropriate. Each iteration, unless discarding action-less tok, append
	" element from either mtoks or toks to output list.
	let idx = len(toks) - 1
	let midx = 0
	while midx < len(mtoks)
		" Is toks exhausted?
		if idx < 0
			" No more toks to merge. No point in merging one-by-one...
			call extend(mtoks_ret, mtoks[midx :])
			break
		endif
		" At least one more tok from each list.
		" TODO: Check for non-toks
		let tok = toks[idx]
		let mtok = mtoks[midx]
		" Set non-empty iff something to add this iteration.
		let addtok = {}
		" At least 1 more tok to process
		if empty(tok.action)
			" Silently discard toks for which we have nothing to do.
			let idx -= 1
		else
			" Compare to see whether tok or mtok comes next.
			let cmp = s:Vmap_cmp_tok(tok, mtok)
			if cmp >= 0
				let addtok = tok
				let idx -= 1
			else
				let addtok = mtok
				let midx += 1
			endif
		endif
		" Caveat: In case of discarded tok, we add nothing, but avoid use of
		" :continue because we need the loop update logic.
		if !empty(addtok)
			call add(mtoks_ret, addtok)
		endif
	endwhile
	" We've fully processed the reversed/merged target list; anything
	" remaining in toks must come before it in the buffer, and can simply be
	" reversed and appended (taking care to discard action-less toks).
	if idx >= 0
		call extend(mtoks_ret, reverse(filter(toks[0:idx], '!empty(v:val.action)')))
	endif
	return mtoks_ret
endfu
" >>>
" Function: s:Tok_nr_to_char() <<<
" Return the actual tok char corresponding to inputs.
" TODO: Move somewhere else and perhaps use within Translate_fmt_clr_spec.
fu! s:Tok_nr_to_char(rgn, idx)
	return nr2char(b:txtfmt_{a:rgn}_first_tok + a:idx)
endfu
" >>>
" Function: s:Get_pos_past_rgn() <<<
" TODO: Remove if this ends up not being needed.
" Caveat: It's come to my attention that calls to getpos() with a mark of '>
" will cause large negative number to be reported for column, so if this is ever
" resurrected, will need to take that into account (probably by replacing call
" to getpos() with line()/col() equivalents, as was done in Highlight_visual and
" Delete_visual).
fu! s:Get_pos_past_rgn(opt)
	let mark = a:opt.mode == 'visual' ? "'>" : "']"
	let pos = getpos(mark)[1:2]
	" Note: It's ok to return nonexistent (past end of line) col.
	let pos[1] + len(s:Get_char(pos))
	return pos
endfu
" >>>
" Function: s:Adjust_rgn_by_off() <<<
" TODO: Possibly pull this back into Vmap_apply_changes, as there doesn't appear
" to be much value in the refactor (see commit ce94a9).
fu! s:Adjust_rgn_by_off(tok, tokstr, opt)
	let [line, col] = [a:tok.pos[0], a:tok.pos[1]]
	" Get the size of the tok that's being inserted or deleted; it will be
	" needed to calculate the signed adjustment offset (with neg. values
	" corresponding to net byte removal).
	let off = len(a:tokstr)
	" Are we in 'linewise' mode?
	" Linewise Mode Assumptions:
	" 1. bp must be col 1 and ep must be col('$')
	" 2. raw and non-raw positions are co-linear (because of the way the
	"    operator positions are calculated).
	" Note: In non-linewise mode, bpr and epr are aliases to their non-raw
	" counterparts and may lie to the right and left of them, respectively.
	let linewise = has_key(a:opt.rgn, 'beg_raw') && a:opt.rgn.beg isnot a:opt.rgn.beg_raw
	" Cache ref to the region boundary positions.
	let [bp, ep] = [a:opt.rgn.beg, a:opt.rgn.end]
	if linewise
		" Cache ref to the 'operator' boundary positions.
		let [bpr, epr] = [a:opt.rgn.beg_raw, a:opt.rgn.end_raw]
	endif

	" Finish determining the signed offset to be *added* to account for token
	" manipulation.
	if a:tok.action == 'd'
		" Since offset is *added* unconditionally, negate to account for net
		" byte removal.
		let off = -off
	elseif a:tok.action == 'r'
		" Subtract len of tok being replaced.
		let off -= len(s:Get_char(a:tok.pos))
	endif
	" Determine impact of insert/append/delete/replace on operator region
	" offsets.
	" Design Decision: In general, prefer to expand operator region to include
	" any added tokens.
	" Note: Originally, what followed was a complex sequence of nested if/else
	" blocks representing the various combinations of tok 'action' and 'loc',
	" but it's been condensed to logic that considers only the relationship
	" between manipulated token and operator region begin/end locations.
	" Design Decision: max() is used to prevent decrement of col pos below 1;
	" however, we might want to consider moving mark to end of previous line in
	" such cases.
	if line == bp[0]
		if linewise && bpr[1] > max([col, 1])
			" Assumption: bp[] isnot bpr[]
			let bpr[1] += off
		endif
		if bp[1] > max([col, 1])
			let bp[1] += off
		endif
	endif
	if line == ep[0]
		if linewise && epr[1] > max([col, 1])
			" Assumption: ep[] isnot epr[]
			let epr[1] += off
		endif
		if ep[1] > max([col, 1])
			let ep[1] += off
		endif
	endif
endfu
" >>>
" Function: s:Adjust_bslashes_maybe() <<<
" If a bslash range immediately precedes the input tok, adjust the range as
" required by the tok's action. If "specialness" of bslashes in range is not
" changing, there's nothing to do; if, on the  other hand, the range is
" transitioning from special to non-special or vice-versa, we need to halve or
" double the bslashes, respectively.
fu! s:Adjust_bslashes_maybe(bslash_ranges, tok)
	" Only tok changes (action ~= '[iad]') can necessitate bslash changes;
	" however, empty phantoms may separate bslash ranges, so we need to handle
	" them as well.
	" FIXME: Why do I have to remove the \? from the following pattern to make
	" deletion work????
	" Note: Deletion works without out; highlighting works with it...
	if a:tok.action !~ '^[iad]\?$'
	"if a:tok.action !~ '^[iad]$'
		return
	endif
	" Determine the col position under which the range would be keyed.
	let key_col = a:tok.action == 'a' ? a:tok.pos[1] + 1 : a:tok.pos[1]
	" See whether there's an applicable bslash range just before col.
	let bsr = get(a:bslash_ranges, a:tok.pos[0], {})->get(key_col, {})
	if empty(bsr)
		" No affected bslash range...
		return
	endif
	"""echomsg "Adjust_bslashes_maybe: bsr=" . string(bsr)
	" Get the line containing the bslash range.
	let linetext = getline(a:tok.pos[0])
	let re = '^\%(\\\+\)[' . b:txtfmt_re_any_tok_atom . ']'
	" Is this range special now?
	let new_special = match(linetext, re, bsr.beg[1] - 1) >= 0
	if bsr.special == new_special
		" No change: nothing to do...
		return
	endif
	" If here, specialness is changing.
	" Build a replacement line in 3 parts: leading / bslashes / trailing.
	let new_linetext = strpart(linetext, 0, bsr.beg[1] - 1)
	"""echomsg "new_linetext(1): " . new_linetext
	if new_special
		" Transition to special
		let new_linetext .= repeat('\', 2 * bsr.count)
	else
		" Transition to not special
		" Assumption: A range with odd number of bslashes would represent an
		" internal error, since it would imply the removal of an escaped token.
		" Hmmm... Perhaps we could simply cull such ranges up front, given that
		" they're effectively insulated from changes in specialness.
		if bsr.count % 2
			echoerr "Internal error: Adjust_bslashes_maybe():"
						\ . ' "specialness" changed on bslash range ending in escaped token.'
		endif
		let new_linetext .= repeat('\', bsr.count / 2)
	endif
	" Append trailing portion of new line.
	let new_linetext .= strpart(linetext, bsr.beg[1] - 1 + bsr.count)

	" Replace the old line.
	call setline(a:tok.pos[0], new_linetext)
endfu
" >>>
" Function: s:Vmap_apply_changes() <<<
" Apply changes in input tok list, which contains all 3 tok types in a single
" flat list, ordered from later in buffer to earlier.
" Side-effects:
" -Make changes specified by 'action' values.
" -Adjust positions in input object to account for changes made.
" toks: [{
"     rgn: fmt|bgc|clr,
"     idx: 0-N,
"     pos: [N,M],
"     action: '[iard]?',
"     loc: <|{|=|}|>
" }, ...]
fu! s:Vmap_apply_changes(toks, opt)
	let bslash_ranges = get(a:opt, 'bslashes', {})
	" Note: List of toks is ordered from later in buffer to earlier.
	let num_toks = len(a:toks)
	let tok_idx = 0
	while tok_idx < num_toks
		let tok = a:toks[tok_idx]
		" Originally, empty action toks were skipped here too, but empty
		" (null) phantoms are needed as triggers for bslash adjustment.
		" Note: Currently, non-phantom empty toks are discarded by earlier
		" stage.
		if tok.typ == 'eob'
			" No change from what's in buffer.
			let tok_idx += 1
			continue
		endif
		" Set to actual tok being considered iff adjustments required.
		" TODO: Consider carrying this on the tok object (since there's
		" something odd about passing either tokstr or toklen to a function
		" that's already receiving tok).
		let tokstr = ''
		if tok.action == 'd'
			" Delete tok from buffer.
			let tokstr = s:Delete_char(tok.pos)
		elseif !empty(tok.action) " [iar]
			" We're going to be inserting/replacing a tok.
			" Special Case: If insert pos is past end of non-empty line, need to
			" convert to append at last char (but inserting at col 1 of empty
			" line is ok).
			" Assumption: Caller logic ensures no append past end of line, but
			" even if it didn't, there's nothing we'd want to change here.
			let eol_col = col([tok.pos[0], '$'])
			if tok.action == 'i' && tok.pos[1] > 1 && tok.pos[1] >= eol_col
				let tok.action = 'a'
				" Note: Ok to leave pos as is - cursor() can handle pos past end
				" of line - however, for sake of s:Adjust_bslashes_maybe, let's
				" ensure it's correct.
				let tok.pos[1] = eol_col - 1
			endif
			" Note: pos could be just past end of line, in which case we simply
			" position at end.
			call cursor(tok.pos)
			let tokstr = s:Tok_nr_to_char(tok.rgn, tok.idx)
			" Insert/replace using appropriate normal mode command.
			" TODO: Perhaps change r to s to obviate need for conversion.
			exe 'normal! ' . (tok.action == 'r' ? 's' : tok.action) . "\<C-R>\<C-O>=l:tokstr\<CR>"
		endif
		" Are adjustments required?
		if !empty(bslash_ranges)
			" Bslash adjustment can be needed only just prior to a tok (either
			" existing or new) and since multiple toks can be added at same
			" buffer location, the following test is required to ensure we don't
			" adjust the same range more than once.
			let next_tok_idx = tok_idx + 1
			if next_tok_idx >= num_toks
				\ || s:Get_effective_pos(a:toks[next_tok_idx]) != s:Get_effective_pos(tok)
				call s:Adjust_bslashes_maybe(bslash_ranges, tok)
			endif
		endif

		" Adjust the region itself.
		" TODO: Decide how important this is; also, consider whether
		" s:Create_del_pos_adjuster() could be used for this.
		if !empty(tokstr)
			call s:Adjust_rgn_by_off(tok, tokstr, a:opt)
		endif
		let tok_idx += 1
	endwhile
endfu
" >>>
" Function: s:Vmap_cleanup() <<<
" Format of toks array:
" toks: [{
"     rgn: 'fmt|clr|bgc',
"     idx: -1 | 0 | 1..N,
"     pos: [row, col],
"     % action == 'd' can exist only for 'clr|bgc' before this function.
"     action: '[iard]?'
"     loc: <|{|=|}|>
" }, ...]
"
" Remove superseded and redundant toks.
" Definitions:
"   Superseded: A superseded tok is one whose effect is never felt because it is
"   followed by a tok of the same rgn type (fmt/clr/bgc) with no intervening
"   hlable.
"   Example: <fb><fi> (the <fb> is superseded)
"   Redundant: A redundant tok is one that is unnecessary because its
"   highlighting is already in effect at the point at which it occurs.
"   Example: <fb> xxx <fb> (the second <fb> is redundant)
fu! s:Vmap_cleanup(toks, opt)
	" Keep up with latest tok (per rgn) that's safe from deletion.
	" First tok is never deletable (and may not be an actual tok).
	let ti_safe_ = {}
	" Keep up with tok (per rgn) to be used in supersedence tests. Empty means
	" skip supersedence check. First tok is never superseded (because syncing
	" logic wouldn't have selected it as first tok if it were).
	let tip_ = {}
	" Within each rgn type, we skip all toks up to the first one susceptible to
	" supersedence; i.e., tok *after* (virtual or actual) sync tok. The presence
	" of a key in ti_safe indicates when we've found first tok of a rgn type.
	for idx in range(len(a:toks))
		let ti = a:toks[idx]
		if !has_key(ti_safe_, ti.rgn)
			" Bootstrap some rgn-specific refs, but otherwise skip the first tok
			" of each rgn type, which is always ss safe.
			let ti_safe_[ti.rgn] = ti
			let tip_[ti.rgn] = {}
			continue
		endif
		if ti.typ == 'tok' && ti.action == 'd'
			" Skip deleted toks, which have no impact on anything.
			" Note: Won't see 'd' for anything but colors here.
			continue
		endif
		if ti.typ == 'tok' && ti.idx < 0
			" Skip the interior phantoms that weren't needed (idx == -1).
			" Assumption: Although head/tail phantoms may ultimately be removed
			" by cleanup, they always have idx >= 0 at this point.
			continue
		endif

		" Cache some per-rgn refs.
		" Assumption: The cached refs won't be reassigned till we update for
		" next iteration.
		let tip = tip_[ti.rgn]
		let ti_safe = ti_safe_[ti.rgn]

		" Ensure that superseded or redundant toks are either discarded
		" (phantom) or deleted from buffer.
		" Note: No need to remove anything from list at this point: to ensure
		" eventual removal, set a phantom's action to null, real tok's action
		" to 'd'.
		" TODO: Perhaps a tok_is_phantom() predicate?
		" TODO: Consider whether superseded/redundant removal logic needs to
		" consider loc { and }: e.g., if there's a choice about which to remove,
		" should we prefer head or tail?

		" Special <eob> tok can supersede, but can't be redundant.
		" TODO: Perhaps add a tok/non-tok indicator instead... Or a 'virtual' flag.
		if ti.typ == 'tok'
			" Perform 1st redundancy check using tip if it exists, else ti_safe.
			" Design Decision: When both superseding and redundant toks exist,
			" prefer to delete redundant.
			if (empty(tip) ? ti_safe.idx : tip.idx) == ti.idx
				let ti.action = ti.action == 'i' || ti.action == 'a' ? '' : 'd'
				" Note: Leave tip unchanged, but don't make safe.
				continue
			endif
		endif

		" If here, we have a tok that is not *currently* redundant, but could
		" supersede a preceding non-deleted tok (in which case, it could be
		" redundant with an earlier tok uncovered by the supersedence delete).
		" Set default tip for next iter; unset below if we determine that cur
		" tok is not supersedable (i.e., if it becomes redundant due to
		" supersedence).
		let tip_next = ti
		if !empty(tip)
			" Need check for supersedence.
			" TODO: No longer considering tip means I should probably just move
			" away from the aggressive cleanup altogether...
			" Caveat: ti.typ == 'eob' implies no actual tok pos.
			if !s:Contains_hlable(tip.pos, ti.pos,
				\ [tip.action == 'i', ti.action == 'a'], tip)
				"echomsg "Not hlable between " . string(tip.pos) . ' and ' . (ti.typ == 'tok' ? string(ti.pos) : '<eob>')
				" Either delete superseded tok, or replace it with default (if
				" necessary to prevent bleed-through from 'last safe' tok).
				" Special Case: If bleed-through is from default tok, *or* from
				" a tok matching current tok, superseded tok can be deleted.
				" Another Special Case: Never 'cap' to prevent bleed-through
				" onto end of buffer.
				" Update_27Mar2016: TODO - Not sure about this anymore:
				" reevaluate bleed-through logic...
				" Note: Bleed-through cap scenario has recently become much more
				" narrow: in particular, capping occurs only when the superseded
				" and superseding toks are separated by a newline (possibly with
				" toks adjacent to newline): i.e., no hlable between toks on
				" adjacent lines.
				" Assumption: If the toks are separated by more than a single
				" line, they're separated by nzwbs: hence, Contains_hlable test
				" would prevent our getting here.
				if ti.typ != 'eob' && ti_safe.idx && ti_safe.idx != ti.idx
					\ && tip.pos[0] != ti.pos[0]
					" Cap non-default region to prevent bleed through.
					"echomsg "Cap non-default region to prevent bleed-through: " . string(tip) . " - " . string(ti) . " - ti_safe: " . string(ti_safe)
					" REFACTOR_TODO: Consider the change to this test... Change
					" was needed because Vmap_apply can change action 'i' to ''.
					" We could always leave the phantom and strip it out during
					" cleanup...
					if empty(tip.action) && tip.idx >= 0 | let tip.action = 'r' | endif
					let tip.idx = 0
					" Design Decision: Once we decide to cap, the decision is
					" final: though subsequent tok might be rendered
					" redundant, this one won't be superseded.
					let ti_safe = tip
				else
					" Nothing but maybe toks exposed. Delete superseded tok.
					"echomsg "Nothing but maybe toks exposed. Delete superseded: " . string(tip) . " - " . string(ti)
					let tip.action = tip.action == 'i' || tip.action == 'a' ? '' : 'd'
				endif
				" Supersedence test complete.
				" Do 2nd redundancy check (necessitated by tok
				" removal/replacement).
				"echomsg "Printing safe and other: " . string(ti_safe) . " - " . string(ti)
				if ti.typ == 'tok' && ti_safe.idx == ti.idx
					" The current tok has become redundant.
					"echomsg "Removing tok determined redundant by 2nd test: " . string(ti) . " - " . string(ti)
					let ti.action = ti.action == 'i' || ti.action == 'a' ? '' : 'd'
					let tip_next = {}
				endif
			else
				" A tok that isn't superseded is safe.
				let ti_safe = tip
			endif
		endif
		" Update for next iter.
		let tip_[ti.rgn] = tip_next
		let ti_safe_[ti.rgn] = ti_safe
	endfor
endfu
" >>>
" Function: s:Vmap_delete() <<<
" Delete all toks in list between phantom toks at head and tail (both
" exclusive) and make corresponding position adjustments for toks beyond the
" region. Also, delete the actual text in the buffer and adjust region bounds
" accordingly.
"
" -- Logic for line/col adjustments --
" Note: These assignments are performed only if applicable: for line adjustment,
" this means at or beyond last line of a multiline range; for col adjustment, on
" final line of range after end of range.
" foo<bar>$
" 	: tok.pos[0] -= end[0] - beg[0] + 1
" foo<bar
" 	blammo>$
" 	: tok.pos[0] -= end[0] - beg[0] + 1
" foo<$
" 	blammo>$
" 	: tok.pos[0] -= end[0] - beg[0] + 1
"
" foo<bar>baz
" 	: tok.pos[1] -= end[1] - beg[1] + end_clen

" foo<bar
" 	blammo>baz
" 	: tok.pos[0] -= end[0] - beg[0]
" 	  tok.pos[1] += beg[1] - 1 - end[1] - end_clen
" foo<$
"	blammo>baz
"	: tok.pos[0] -= end[0] - beg[0]
"	  tok.pos[1] += beg[1] - 1 - end[1] - end_clen
fu! s:Vmap_delete(toks, opt)
	" Loop over all tokens, though the we won't really start doing anything till
	" we get to phantom head.
	" Possible TODO: If location were cached somewhere, we could start there.
	let [del_head, del_tail] = [-1, -1]
	let idx = 0
	let num_toks = len(a:toks)
	while idx < num_toks
		let tok = a:toks[idx]
		if del_head < 0 && tok.loc == '='
			let del_head = idx
		endif
		if del_tail < 0 && (tok.loc == '}' || tok.loc == '>')
			if del_head >= 0
				" Delete through previous tok.
				let del_tail = idx - 1
			endif
			break
		endif
		let idx += 1
	endwhile
	" Note: Value of idx at loop exit is reused.

	" Initialize object that can be used to adjust position of tokens beyond
	" deletion range.
	let tok_adj = s:Create_del_pos_adjuster(a:opt)

	" Assumption: idx unmodified from end of previous loop: i.e., we're looking
	" only at toks that *might* require position adjustments.
	" FIXME: Refactor this loop to use Create_del_pos_adjuster().
	while idx < len(a:toks)
		let tok = a:toks[idx]
		" Skip positionless toks.
		if !empty(tok.pos)
			call tok_adj.Adjust(tok)
		endif
		let idx += 1
	endwhile

	" Now remove the buffer text in range [head, tail]
	" Call either s:Delete_range_vi or s:Delete_range_op (depending on mode) to
	" delete the text from the buffer.
	call {'s:Delete_range_' . (a:opt.mode == 'v' ? 'vi' : 'op')}(a:opt.rgn.beg, a:opt.rgn.end)
	" FIXME: Decide whether anything needs to be done to adjust region itself.
	" Design Decision: If so, allow col to float to one past end in the linewise cases.

	" Remove tok elements corresponding to what was deleted.
	" Rationale: Could introduce an 'action' type that means ignore, but
	" removing them from the list simplifies subsequent stages. Note that we
	" couldn't set to 'd', since that would result in attempt to delete already
	" deleted tok. Similarly, setting to '' would allow the deleted tok's effect
	" to be felt in (eg) tok cleanup, and we don't want that either; we want it
	" completely ignored.
	" BUGFIX_TODO: Consider implementing the ignore idea as part of a fix for
	" "smart delete" operator.
	if del_head >= 0
		call remove(a:toks, del_head, del_tail)
	endif
endfu
" >>>
" Function: s:dbg_display_toks() <<<
" Debug function that prints details of all toks in the provided list with the
" provided string for context.
fu! s:dbg_display_toks(context, toks)
	if !exists('g:txtfmt_debug_enabled') || !g:txtfmt_debug_enabled
		return
	endif
	echomsg "\r"
	echomsg a:context
	for ti in a:toks
		if ti.typ == 'eob'
			echomsg ti.rgn . ' ' . '<eob> virtual tok'
		else
			echomsg printf('(%3d, %3d): %s(%2d): => %s [%s] @ %s'
				\, empty(ti.pos) ? -1 : ti.pos[0]
				\, empty(ti.pos) ? -1 : ti.pos[1]
				\, ti.rgn, ti.idx, empty(ti.action) ? ' ' : ti.action , ti.typ, ti.loc)
		endif
	endfor
	echomsg "\r"
endfu
" >>>
" Function: s:Operate_region() <<<
fu! s:Operate_region(pspecs, opt)
	" Caveat: Because Vmap_compute() can move cursor off screen,
	" saving/restoring view is needed to prevent spurious scrolling.
	let wsv = winsaveview()
	let pspecs = a:pspecs
	if a:opt['op'] == 'delete'
		" Assumption: Caller inputs empty pspecs for delete case; transform to
		" something that will unconditionally remove all highlighting, and count
		" on the op == 'delete' in opt to handle things specially.
		" TODO VMAPS: Consider whether this is the cleanest way... Would prefer
		" not to have to hardcode the 0 idx's like this, but it simplified the
		" logic.
		" IMPORTANT TODO: Consider subsuming pspecs into opt! Can't think of any
		" reason they'd need to be separate.
		let pspecs = {'rgns': {}, 'sel': s:Sel_identity()}
		" Add a key with 0 idx value for all active regions.
		" Note: This loop is a bit kludgy; unfortunately,VimL doesn't appear to
		" offer a functional way to transform an array to a dictionary.
		for r in s:Get_active_rgns() | let pspecs.rgns[r] = 0 | endfor
	endif
	" Note: Returned array of toks is merged (all rgns combined).
	let toks = s:Vmap_compute(pspecs, a:opt)
	if a:opt['op'] == 'delete'
		" Update list to reflect toks we're going to delete before cleanup.
		call s:dbg_display_toks("before Vmap_delete", toks)
		call s:Vmap_delete(toks, a:opt)
		call s:dbg_display_toks("Vmap_delete", toks)
	endif
	" Begin cleanup phase...
	call s:dbg_display_toks("before Vmap_cleanup", toks)
	call s:Vmap_cleanup(toks, a:opt)
	call s:dbg_display_toks("after Vmap_cleanup", toks)
	" Reverse list, discarding action-less toks and non-tok virtual markers (e.g., <eob>).
	" Rationale: When applying changes to buffer, we work from the end to avoid
	" invalidating line/col offsets before they're used.
	" Note: If esc=bslash, null phantoms will be needed in Vmap_apply_changes to
	" consolidate bslash ranges, so don't remove them here.
	" TODO: Consider integrating this cleanup into Vmap_cleanup, but probably
	" wait till refactor is complete...
	call reverse(filter(toks,
				\ {idx, tok -> tok.typ == "tok" && (!empty(tok.action) || s:Is_phantom_null(tok))}))
	call s:dbg_display_toks("Reversed and discarded virtual markers and action-less toks", toks)

	if b:txtfmt_cfg_escape == 'bslash' && exists('g:txtfmt_debug_enabled') && g:txtfmt_debug_enabled
		echomsg "bslash ranges: " . string(a:opt.bslashes)
	endif
	" Apply changes to buffer (in reverse order, to ensure offsets are not
	" invalidated before use).
	call s:Vmap_apply_changes(toks, a:opt)

	call winrestview(wsv)
endfu
" >>>
" Function: s:Vmap_validate_region() <<<
" Perform both validation and adjustments required to establish pre-conditions
" as follows:
" 1. Adjust region bounds as necessary to prevent the following:
"    -splitting an escape-escapee pair
"    -excluding a char at the end of the region which is not part of the visual
"     selection, but appears to be because `> is positioned on a concealed char
"     in a sequence of one or more concealed chars at the end of the region.
"     Note: It's not necessary to perform this adjustment at the start of the
"     region because in that case, the unconcealed chars that appear to be
"     selected actually would be (since the sense of the selection is reversed).
" 2. Throw exception if the adjusted region contains nothing that can be
"    operated upon.
" Rule: Non-token characters and blank lines can be operated upon.
" TODO: I think I may prefer that this adjusted '<,'> and/or '[,']. Actually,
" I'm wondering whether it might be simpler if we used those in lieu of beg/end,
" etc...
fu! s:Vmap_validate_region(opt)
	" Make sure sel borders don't 'split' an escape-escapee pair.
	" Note: Function modifies positions in-place.
	call s:Adjust_sel_to_protect_escapes(a:opt)

	" Expand region end to include a non-concealed char under the cursor.
	call s:Move_pos_rightward_maybe(a:opt.rgn.end)

	" TODO: Consider whether we need to check for a backslash escape just prior
	" to and a token just following region... Actually, I think all we could do
	" here is check for the backslash, since we can't know at this point what
	" will follow the region after token processing and cleanup.
	" Note: Intentionally omitting optional tok_info arg.
	" Rationale: We want non-aggressive hlable interpretation.
	" BUG - Why does a \tok pair not pass validation?
	" TODO: Is there still a bug here? I'm thinking it was fixed...
	if !s:Contains_hlable(a:opt.rgn.beg, a:opt.rgn.end, [1, 1])
		throw "Nothing hlable in specified region"
	endif
	" Check for special case: nothing but toks followed by optional newline
	" between '> and end of buffer. If special case exists, expand region to
	" include the toks adjacent to region.
endfu
" >>>
" Function: s:Get_opfunc_adjusted_pos() <<<
fu! s:Get_opfunc_adjusted_pos(mode)
	" Note: Vim provides the actual start/end pos of motion, regardless of mode.
	" The raw positions may or may not be what txtfmt operation will use. Ensure
	" that beg/end contain the positions txtfmt operation will use.
	" Design Decision: Set the '_raw' positions unconditionally, but make them
	" simple aliases to non-raw in the non-linewise case. An alternative would
	" be to leave '_raw' positions undefined if they're not needed. Note that
	" positioning cursor after operation is simpler when '_raw' positions are
	" always defined, and always represent where we'd want cursor to end up.
	let rgn = {'beg_raw': getpos("'[")[1:2], 'end_raw': getpos("']")[1:2]}
	if a:mode != 'line'
		" Important Note: raw and non-raw positions are simply aliases.
		" Downstream code is expected to rely upon this for differentiating
		" linewise case from non-linewise.
		let [rgn.beg, rgn.end] = [rgn.beg_raw, rgn.end_raw]
	else
		" Set the positions used by the txtfmt operation.
		let rgn.beg = [rgn.beg_raw[0], 1]
		let rgn.end = [rgn.end_raw[0], col([rgn.end_raw[0], '$']) - 1]
	endif
	return rgn
endfu
" >>>
" Function: s:Delete_region() <<<
" Delete_region() is a workhorse function used by both Delete_visual() and
" Delete_operator().
fu! s:Delete_region(rgn, mode)
	let opt = {'rgn': a:rgn, 'op': 'delete', 'mode': a:mode}
	" Note: Validation may adjust cur pos, but never alters '< '> or '[ '].
	call s:Vmap_validate_region(opt)
	" Perform the delete.
	call s:Operate_region({}, opt)
	" Leave cursor at pos of first deleted char.
	call cursor(opt.rgn.beg)
	return opt
endfu
" >>>
" Function: s:Delete_visual() <<<
fu! s:Delete_visual()
	" Blockwise visual selections not supported!
	if visualmode() == "\<C-V>"
		throw "Delete_visual: blockwise-visual mode not supported"
	endif
	try
		let [l1, l2] = [line("'<"), line("'>")]
		" TODO: Perhaps switch to call form, since return not used.
		" TODO: Refactor the line/col workaround for problematic getpos("'>")
		" into separate function, since it's also used in Highlight_visual.
		let rgn = {
			\'beg': [l1, col([l1, col("'<")])],
			\'end': [l2, col([l2, col("'>")])]}
		let opt = s:Delete_region(rgn, 'visual')
	catch /^\(Vim:Interrupt\)\@!.*$/
		throw "Delete_visual: Unable to delete selection: "
			\. v:exception . " occurred at " . v:throwpoint
	finally
		" Leave cursor just past deleted text (as Vim does).
		call cursor(getpos("'<")[1:2])
	endtry
endfu
" >>>
" Function: s:Delete_operator() <<<
fu! s:Delete_operator(mode)
	if a:mode == 'block'
		" TODO: Can we constrain with mapping itself?
		throw "Delete_operator: blockwise motions not supported"
	endif
	let rgn = s:Get_opfunc_adjusted_pos(a:mode)
	try
		let opt = s:Delete_region(rgn, 'operator')
		" Adjust '[ and '] to account for any modifications.
		" Important Note: Intentionally setting both '[ and '] to start pos, as
		" this is what Vim does in deletion case.
		" Note: Shouldn't be any difference between raw and non-raw positions in
		" 'linewise' case.
		" Caveat: setpos fails if given an array with fewer than 3 elements.
		call setpos("'[", [0] + opt.rgn.beg_raw)
		call setpos("']", [0] + opt.rgn.beg_raw)
	catch /^\(Vim:Interrupt\)\@!.*$/
		" Restore cursor position to start of operated region.
		call cursor(getpos("'[")[1:2])
		throw "Delete_operator: Unable to delete selection: "
			\. v:exception . " occurred at " . v:throwpoint
	finally
	endtry
endfu 
" >>>
" Function: s:Highlight_region() <<<
" Highlight_region() is a workhorse function used by both Highlight_visual() and
" Highlight_operator().
fu! s:Highlight_region(rgn, mode, spec)
	" TODO: Consider factoring the common stuff out of Delete/Highlight_region...
	" Initialize a struct to hold r/w params passed to various functions.
	let opt = {'rgn': a:rgn, 'op': 'highlight', 'mode': a:mode}
	" Note: Validation may adjust region beg/end, but never alters '< and '>.
	call s:Vmap_validate_region(opt)
	" Prompt user for desired highlighting unless it was provided by caller
	" (shortcut map case).
	let tokstr = !empty(a:spec) ? a:spec : s:Prompt_fmt_clr_spec()
	" Parse and validate fmt/clr transformer spec
	let pspecs = s:Parse_fmt_clr_transformer(tokstr)
	" Check for Cancel request
	" Note: Nothing about position or mode has changed at this point.
	" TODO: In event of cancel, should we return {} as a signal to caller who may wish take advantage?
	if !empty(pspecs)
		" Perform the highlighting.
		call s:Operate_region(pspecs, opt)
	endif
	return opt
endfu
" >>>
" Function: s:Highlight_visual() <<<
fu! s:Highlight_visual(...)
	" Blockwise visual selections not supported!
	if visualmode() == "\<C-V>"
		throw "Highlight_visual: blockwise-visual mode not supported"
	endif
	try
		" If non-empty spec was provided, pass it on.
		let spec = a:0 && !empty(a:1) ? a:1 : ''

		" Caveat: Use line() and col() to work around idiosyncrasy/bug with
		" getpos(), which returns very large negative column number for '> mark.
		let [l1, l2] = [line("'<"), line("'>")]
		let opt = s:Highlight_region({
			\'beg': [l1, col([l1, col("'<")])],
			\'end': [l2, col([l2, col("'>")])]},
			\'visual',
			\spec)
		" Adjust '< and '> to account for any modifications.
		call s:Restore_visual_mode(opt.rgn.beg, opt.rgn.end, 1)
	catch /^\(Vim:Interrupt\)\@!.*$/
		throw "Highlight_visual: Unable to highlight selection: exception "
			\. v:exception . " occurred at " . v:throwpoint
	finally
		" Leave cursor at start of selection (as Vim does).
		call cursor(getpos("'<")[1:2])
	endtry
	call cursor(opt.rgn.beg)
endfu

" Wrapper used with 'opfunc' when lambdas are not supported.
if !has('lambda')
fu! s:Highlight_operator_wrapper(mode)
	" Note: s:active_shortcut_spec is set by map before executing g@
	return s:Highlight_operator(a:mode, s:txtfmt_active_shortcut_spec)
endfu
endif

" >>>
" Function: s:Highlight_operator() <<<
fu! s:Highlight_operator(mode, ...)
	if a:mode == 'block'
		" TODO: Can we constrain with mapping itself?
		throw "Highlight_operator: blockwise motions not supported"
	endif
	let rgn = s:Get_opfunc_adjusted_pos(a:mode)
	try
		" If non-empty spec was provided, pass it on.
		let spec = a:0 && !empty(a:1) ? a:1 : ''

		let opt = s:Highlight_region(rgn, 'operator', spec)
		" Adjust '[ and '] to account for any modifications.
		" Design Decision: Currently, '_raw' positions always set, though they
		" may be simple aliases to the non-raw versions (in non-linewise case).
		" Caveat: setpos fails if given an array with fewer than 3 elements.
		call setpos("'[", [0] + opt.rgn.beg_raw)
		call setpos("']", [0] + opt.rgn.end_raw)
	catch /^\(Vim:Interrupt\)\@!.*$/
		throw "Highlight_operator: Unable to highlight operated region: exception "
			\. v:exception . " occurred at " . v:throwpoint
	finally
		" Restore cursor position to start of operated region.
		" Note: Do so whether termination was normal or abnormal.
		call cursor(getpos("'[")[1:2])
	endtry
endfu
" >>>
" EBNF Grammar for highlight specs <<<
" Whitespace Handling: Within a production rule, 'opt_ws' indicates where
" whitespace is permitted (but not required).
"
" or_expr = and_expr , opt_ws , { ("||" | "|") , opt_ws , and_expr }
" and_expr = term , opt_ws , { ("&&" | "&") , opt_ws , term }
"     Note: && and || may be used instead of & and | for visual (unnecessary)
"     disambiguation.
" term = f_term
"      | ck_term
"      | "(" , opt_ws , or_expr , opt_ws , ")"
"      | "!" , opt_ws , term
" f_term = "f" , opt_ws , ( "&" | "|" | [ "=" ] ) , f_attrs
"        | "f" , opt_ws , [ "=" ] [ "-" ]
"     Note: An f_term will *never* be recognized when the & or | following the
"     'f' is the first in a pair: i.e., '&&' is *always* logical AND.
"     Note: Support all combinations of '=' , '-', even those that seem
"     silly/rendundant (e.g., `=-')
" f_attrs = f_attr , { f_attr }
" f_attr = "u" | "b" | "i" | "s" | "r" | "c"
" ck_term = ("c" | "k") , opt_ws , color_name
"         | ("c" | "k")
" color_name = color_name_char , { opt_ws (* see note *) , color_name_char }
"     Note: opt_ws applies only when cfg_color_name_compat is set.
" color_name_char = (? regex: [-_a-zA-Z0-9] ?)
"     Note: Currently no restrictions on - in name: i.e., `-' means default, and
"     `--' is an actual color name. Should it be? Or should we require alpha?
" opt_ws = { ws }
" ws = ? sequence of 1 or more whitespace chars ?
"
" Note: "No format" can be spelled in any of the following ways:
"   f=, f-, f
" Caveat: In the final case, there could be ambiguity if the following token
" is a logical AND: e.g...
"   f&cbus
" Is that...
"     f& undercurl-bold-underline-standout
" ...or...
"     f- & fg-color 'bus'
" Ways to resolve...
" 1. Put space between the `&' and subsequent chars to prevent attempt to
"    interpret the latter as f_attrs.
" 2. Put explicit `-' or `=' after f.
" 3. Use `&&' (always means logical AND).
" >>>
" Function: s:Sel_identity() <<<
fu! s:Sel_identity()
	return {'op': '!', 'val': 1}
endfu
" >>>
" Function: s:Sel_parser_tok_init() <<<
fu! s:Sel_parser_tok_init(sel)
	let ps = {'sel': a:sel, 'idx': 0}
	return ps
endfu
" >>>
" Function: s:Sel_parser_match() <<<
fu! s:Sel_parser_match(ps, re, ...)
	if a:0 ? a:1 : 0
		" Whitespace matched only explicitly.
		let si = a:ps.idx
	else
		" Start (actually anchor) search at first non-whitespace char.
		let si = match(a:ps.sel, '\S', a:ps.idx)
	endif
	if si >= 0
		" Anchor the match at point.
		let ms = matchlist(a:ps.sel, '^' . a:re, si)
		if !empty(ms)
			" Adjust parse state to consume the match.
			let a:ps.idx = si + len(ms[0])
			" Return match and all submatches.
			return ms
		endif
	endif
	" No match.
	return {}
endfu
" >>>
" Function: s:Sel_parser_accept() <<<
" Convenience method used when only boolean success/failure is desired from
" s:Sel_parser_match. Optional explicit_ws arg defaults to false, but may be
" overridden.
fu! s:Sel_parser_accept(ps, re, ...)
	return !empty(s:Sel_parser_match(a:ps, a:re, a:0 ? a:1 : 0))
endfu
" >>>
" Function: s:Sel_parser_bool_expr() <<<
" Handle both &[&] and |[|] bool expressions.
fu! s:Sel_parser_bool_expr(ps, op)
	let expr = {'op': a:op, 'expr': []}
	" Need to check for both &/| and &&/||
	let opop = a:op . a:op
	while !exists('l:got_op') || !empty(got_op)
		let sexpr = a:op == '|'
			\? s:Sel_parser_bool_expr(a:ps, '&')
			\: s:Sel_parser_term(a:ps)
		if empty(sexpr)
			if exists('l:got_op') && !empty(got_op)
				throw "Expected term following " . got_op
			else
				" Let caller decide whether no term at this position is error.
				return {}
			endif
		endif
		" Accumulate the sub-expression.
		call add(expr.expr, sexpr)
		" Check for logical operator.
		let got_op = s:Sel_parser_accept(a:ps, opop)
			\? opop : s:Sel_parser_accept(a:ps, a:op) ? a:op : ''
	endwhile
	if len(expr.expr) == 1
		" Remove useless layer (e.g., don't maintain an OR expression whose only
		" purpose is to contain an AND expression).
		let expr = expr.expr[0]
	endif
	return expr
endfu
" >>>
" Function: s:Sel_parser_try_fterm() <<<
fu! s:Sel_parser_try_fterm(ps)
	if s:Sel_parser_accept(a:ps, 'f')
		let term = {'mask': 0}
		" Caveat: Don't let the pattern consume the 1st in pair of &'s or |'s
		" Allow =- though it makes little sense.
		let m = s:Sel_parser_match(a:ps, '\s*\%(\%(||\|&&\)\)\@!'
			\. '\%(\([&|]\|=\?\)\([ubisrc]\+\)\|\(=-\|[-=]\)\?\)', 1)
		if empty(m) || empty(m[2])
			" Literally or effectively f-
			let term.op = 'f='
		else
			" Non-default attrs
			let term.op = 'f' . (empty(m[1]) ? '=' : m[1])
			" TODO: Ensure m[2] is validated somewhere: either prior to this or
			" in loop below... (Probably wait till I've refactored for latest
			" format change.)
			for attr in split(m[2], '\zs')
				let term.mask = or(term.mask, s:ubisrc_mask[attr])
			endfor
		endif
		return term
	endif
	" No fterm
	return {}
endfu
" >>>
" Function: s:Sel_parser_try_ckterm() <<<
fu! s:Sel_parser_try_ckterm(ps)
	let m = s:Sel_parser_match(a:ps, '[ck]')
	if !empty(m)
		" Looks like fg/bg color.
		let term = {'op': m[0]}
		" TODO: Regex for color name chars would simplify things.
		let re_cname_char = '[-_a-zA-Z0-9]'
		" Note: Permit interior whitespace in name if cfg_color_name_compat set.
		let m = s:Sel_parser_match(a:ps, re_cname_char
			\. '\%(' . (s:cfg_color_name_compat ? '\s*' : '') . re_cname_char . '\)*')
		let cname = empty(m) ? '-' : m[0]

		" Set color index: 0 = default, with 1 corresponding to first
		" non-default color.
		if cname == '-'
			let term.idx = 0
		else
			" TODO!!!!!!!!!!!!!!!!
			" Obviate need for boilerplate error-checking after call to
			" Lookup_clr_namepat - probably just have it throw exception
			" directly, but there's a legacy case to consider...
			let term.idx = s:Lookup_clr_namepat(term.op, cname)
			if term.idx == 0
				throw "Invalid color name pattern: '" . cname . "'"
			elseif term.idx < 0
				" TODO_BG: Make sure the help note below is still valid after
				" help has been updated.
				throw "Color ".(-1 * term.idx)." is not an active "
					\.(term.op ==? 'c' ? "foreground" : "background")
					\." color. (:help "
					\.(term.op ==? 'c' ? "txtfmtFgcolormask" : "txtfmtBgcolormask").")"
			endif
		endif
		return term
	endif
	" No clr/bgc term
	return {}
endfu
" >>>
" Function: s:Sel_parser_term() <<<
fu! s:Sel_parser_term(ps)
	let term = s:Sel_parser_try_fterm(a:ps)
	if !empty(term) | return term | endif
	" Not f-term
	let term = s:Sel_parser_try_ckterm(a:ps)
	if !empty(term) | return term | endif
	" Neither f nor ck
	if s:Sel_parser_accept(a:ps, '!')
		let term = s:Sel_parser_term(a:ps)
		let term.neg = !has_key(term, 'neg') || !term.neg
		return term
	endif
	" Neither f nor ck nor !term
	if s:Sel_parser_accept(a:ps, '(')
		let term = s:Sel_parser_bool_expr(a:ps, '|')
		if empty(term)
			throw "Expected term following `('"
		endif
		if !s:Sel_parser_accept(a:ps, ')')
			throw "Expected `&', `|' or `)'"
		endif
		return term
	endif
	return {}
endfu
" >>>
" Function: s:Sel_parser_expr_to_string() <<<
" Debug function
" TODO: Document if keeping...
fu! s:Sel_parser_expr_to_string(expr, indent)
	let sw = 2
	let s = ''
	" Print recursively.
	if has_key(a:expr, 'neg') && a:expr.neg
		let s .= "!"
	endif
	if a:expr.op =~ 'f[=&|]'
		let s .= a:expr.op . '{' . printf('$%02x', a:expr.mask) . '}'
	elseif a:expr.op =~ '[ck]'
		let s .= a:expr.op . a:expr.idx
	elseif a:expr.op =~ '[&|]'
		let s .= "(\n" . (repeat(' ', (a:indent + 1) * sw))
		let first_expr = 1
		for sexpr in a:expr.expr
			let sep = first_expr
				\? ''
				\: (' ' . (a:expr.op[0] =~ '&' ? '&&' : '||') . ' ')
			let s .= sep . s:Sel_parser_expr_to_string(sexpr, a:indent + 1)
			let first_expr = 0
		endfor
		let s .= "\n" . repeat(' ', a:indent * sw) . ")"
	endif
	return s
endfu
" >>>
" Function: s:Parse_selector() <<<
" TODO: Document...
fu! s:Parse_selector(sel)
	let ps = s:Sel_parser_tok_init(a:sel)
	try
		let expr = s:Sel_parser_bool_expr(ps, '|')
		if !empty(expr)
			"Make sure returned expression consumed all input.
			if !s:Sel_parser_accept(ps, '\s*$', 1)
				throw "Expected `&&', `||' or end of input"
			endif
			" Valid expression
			"echo s:Sel_parser_expr_to_string(expr, 0)
		endif
		return expr
	catch
		echoerr "Syntax error in selector at char offset " . ps.idx
			\. ": " . v:exception . " at " . v:throwpoint
			\. ", unconsumed input: `" . ps.sel[ps.idx:] . "'"
	endtry
endfu
" >>>
" Function: s:Check_selector() <<<
" Evaluate input expression against the input toks, returning 1 if expression
" evaluates true.
" Inputs:
" expr: expression in form returned by Parse_selector
" toks: {
"   fmt: <fmt-mask>
"   clr: <fg-color-number>
"   bgc: <bg-color-number>
" }
fu! s:Check_selector(expr, toks)
	" Cache inputs for convenience.
	let [e, t] = [a:expr, a:toks]
	" Set val within one of the if's below, handling any negation at end.
	if type(e) == 0
		let val = e
	elseif e.op == '!'
		" Boolean val.
		let val = e.val
	elseif e.op =~ '^[&|]$'
		" Initialize to identity val, toggling in loop on short-circuit.
		let val = e.op == '&' ? 1 : 0
		for sexpr in e.expr
			if !val == !!s:Check_selector(sexpr, t)
				let val = !val
				break
			endif
		endfor
	elseif e.op =~ '^f[=&|]$'
		if e.op[1] == '='
			let val = e.mask == t.fmt
		else
			let val = and(e.mask, t.fmt)
			let val = e.op[1] == '&' ? val == e.mask : !!val
		endif
	elseif e.op =~ '^[ck]$'
		"echomsg "Color comparison" . e.idx . " -- " . t[b:txtfmt_rgn_typ_abbrevs[e.op]]
		let val = e.idx == t[b:txtfmt_rgn_typ_abbrevs[e.op]]
	else
		throw "Internal Error! Invalid term in selector expression: " . string(e)
	endif
	"echomsg "Check_selector returning " . (has_key(e, 'neg') && e.neg ? !val : !!val)
	return has_key(e, 'neg') && e.neg ? !val : !!val
endfu
" >>>
" Function: s:Jump_to_tok() <<<
" Purpose: Jumps forward or backwards (as determined by a:dir), to the
" v:count1'th nearest token of type given by a:type ('c'=clr 'k'=bgc 'f'=fmt
" 'a'=any (clr, bgc or fmt)). If 'till' argument is nonzero, jump will
" position cursor one char position closer to starting location than the
" sought token. (This behavior is analogous to t and T normal mode commands.)
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
"	 				c = fg color, k = bg color, f = format,
"	 				a = fg color, bg color, or format
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
	" Determine whether we jump only to active tokens
	" By default, we don't.
	let jtin = exists('b:txtfmtJumptoinactive')
				\ ? b:txtfmtJumptoinactive
				\ : exists('g:txtfmtJumptoinactive')
				\   ? g:txtfmtJumptoinactive
				\   : 0
	" Get the search pattern
	" Design Decision Needed: Decide whether to permit inactive color tokens
	" to serve as target of jump. If this is desired, perhaps create special
	" b:txtfmt_re_CLR_<...> and b:txtfmt_re_BGC_<...> regexes. Alternatively,
	" use the b:re_no_self_esc and b:re_no_bslash_esc patterns on the
	" <...>_atom regexes.
	" Note: Let jumptoinactive option determine whether inactive tokens can
	" serve as jump targets.
	if a:type == 'c'
		let re = b:txtfmt_re_{jtin ? 'CLR' : 'clr'}_tok
	elseif a:type == 'bc'
		let re = b:txtfmt_re_{jtin ? 'CLR' : 'clr'}_stok
	elseif a:type == 'ec'
		let re = b:txtfmt_re_{jtin ? 'CLR' : 'clr'}_etok
	elseif a:type == 'k'
		let re = b:txtfmt_re_{jtin ? 'BGC' : 'bgc'}_tok
	elseif a:type == 'bk'
		let re = b:txtfmt_re_{jtin ? 'BGC' : 'bgc'}_stok
	elseif a:type == 'ek'
		let re = b:txtfmt_re_{jtin ? 'BGC' : 'bgc'}_etok
	elseif a:type == 'f'
		let re = b:txtfmt_re_fmt_tok
	elseif a:type == 'bf'
		let re = b:txtfmt_re_fmt_stok
	elseif a:type == 'ef'
		let re = b:txtfmt_re_fmt_etok
	elseif a:type == 'a'
		let re = b:txtfmt_re_{jtin ? 'ANY' : 'any'}_tok
	elseif a:type == 'ba'
		let re = b:txtfmt_re_{jtin ? 'ANY' : 'any'}_stok
	elseif a:type == 'ea'
		let re = b:txtfmt_re_{jtin ? 'ANY' : 'any'}_etok
	else
		" Error - shouldn't ever get here - just return
		return ''
	endif
	let g:re = re
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
" TODO: This mechanism should be rewritten to use modern VimL data structures.
" Also, consider whether it would make sense to use buf-local structures for <buffer> maps.
fu! s:Mapwarn_check(lhs, rhs, mode, add)
	let found = 0
	let i = 0
	if exists('g:txtfmt_mapwarn_cnt')
		while i < g:txtfmt_mapwarn_cnt
			if a:lhs == g:txtfmt_mapwarn_lhs{i} &&
				\ a:rhs == g:txtfmt_mapwarn_rhs{i} &&
				\ a:mode == g:txtfmt_mapwarn_mode{i}
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
	elseif a:mode=='x'
		let unmap_cmd = 'xunmap'
	elseif a:mode=='s'
		let unmap_cmd = 'sunmap'
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
" Function: s:Def_map2() <<<
" Purpose: Define both the level 1 and level 2 map as appropriate.
" Inputs:
" mode 	    - single char, used as input to maparg, mapcheck, etc...
" lhs1	    - lhs of first-level map
"             empty if no first-level map required (special case)
"             Rationale: Allows us to map to 2nd level internally (eg,
"             TxtfmtIndent Normal mode maps).
" lhs2	    - rhs of first-level map, lhs of second-level map
" rhs2	    - rhs of second-level map
" [noremap] - bool indicating whether 'noremap' should be used for 2nd-level map
"             Default: 1
" How: Consider whether user already has a map to level 2 (which should take
" precedence over maplevel 1). Also, make sure the map from level 2, if it
" exists, is not incorrect, etc...
" Note: Cause b:undo_ftplugin to be updated so that whatever mappings are made
" by us will be unmapped when ftplugin is unloaded.
" Return:
" 0			- success
" nonzero	- error
" NOTE: Function will echoerr to user
fu! s:Def_map2(mode, lhs1, lhs2, rhs2, ...)
	" Make sure <...> key notation works in mappings.
	let save_cpo = &cpo
	set nocp
	try
	" Determine whether 2nd level uses map or noremap.
	let noremap = a:0 ? !!a:1 : 1
	if a:mode !~ '^[niov]$'
		echoerr 'Internal error - unsupported mapmode passed to Def_map2()'
		return 1
	endif
	" Do first map level <<<
	" FIXME: I'm thinking the 1st level maps *we* create (ie, not the ones
	" user has created) should also be reflected in undo_ftplugin.
	" Caveat: This guard can prevent map changes from taking effect when changes
	" are made and :Refresh is run without first quitting Vim. When sessions are
	" involved, the problem can be even more insidious. The guard is important,
	" though, to prevent creation of default map if user has already defined his
	" own. Perhaps a warning in help, or some way of mitigating the issue?
	" TODO: Is the comment above still applicable?
	if !empty(a:lhs1) && !hasmapto(a:lhs2, a:mode)
		" User hasn't overridden the default level 1 mapping
		" Make sure there's no conflict or ambiguity between an existing map
		" and the default one we plan to add...
		let oldarg = maparg(a:lhs1, a:mode)
		let oldchk = mapcheck(a:lhs1, a:mode)
		" Do the map for buffer unless map creation is precluded by conflict
		" or ambiguity in absence of the 'create' flag.
		" Note: Do not use <unique> attribute, since that would cause an error if
		" we're overriding.
		if s:Check_map_lhs(a:mode, a:lhs1)
			exe a:mode . 'map <buffer> '.a:lhs1.' '.a:lhs2
			" Create undo action for the map just created
			call s:Undef_map(a:lhs1, a:lhs2, a:mode)
		endif
	endif
	" >>>
	" Do second map level <<<
	" Assumption: Second-level mappings have long <Scriptname><...> names,
	" preceded by <Plug>. It is safe to assume user hasn't mapped one to
	" something else...
	exe a:mode . (noremap ? 'nore' : '') . 'map'
				\ .' <silent> <buffer> '.a:lhs2.' '.a:rhs2
	" Create undo action for the map just created
	call s:Undef_map(a:lhs2, a:rhs2, a:mode)
	" >>>
	catch
		let &cpo = save_cpo
	endtry
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
" purposes only. Highlighting of column headers is accomplished via echohl
" Format: Should be something like the sample table shown below...
" Note: char-nr should use the number format indicated by
" b:txtfmt_cfg_starttok_display.
" Note: For inactive colors, an asterisk will be prepended to char-nr, and
" '(inactive)' will be appended to the description. In order to keep the
" numbers aligned properly, active colors will have a space prepended to the
" char-nr.
" TODO: Decide whether it's necessary to wrap inactive char-nr's in parens. If
" not, get rid of it.
"=== [FG] COLORS ===
"char-nr   description        clr-pattern                                clr-def
"180       no color           -
"181       Color0             ^\\%(k\\|bla\\%[ck]\\)$,c:Black,g:#000000  #000000
"182       Color1             ^blu\\%[e]$,c:DarkBlue,g:#0000FF           #0000FF
"183       Color2             ^g\\%[reen]$,c:DarkGreen,g:#00FF00         #00FF00
".
".
"=== FORMAT ===
"char-nr   description        spec
"189       no format          -
"190       italic             i
"191       bold               b
"192       bold,italic        bi
".
".
".
".
" Important Note: The subsequent lines will be output if and only if
" background colors are enabled.
"=== BG COLORS ===
"char-nr   description        clr-pattern                                clr-def
" 197      no color           -
"*198      Color0 (inactive)  ^\\%(k\\|bla\\%[ck]\\)$,c:Black,g:#000000  #000000
" 199      Color1             ^blu\\%[e]$,c:DarkBlue,g:#0000FF           #0000FF
" 200      Color2             ^g\\%[reen]$,c:DarkGreen,g:#00FF00         #00FF00
" .
" .
fu! s:ShowTokenMap()
	" Loop 2 times - first time is just to calculate column widths
	let cw1 = 0 | let cw2 = 0 | let cw3 = 0 | let cw4 = 0
	" Define an array, indexed by fgbg_idx, which may be used to build fg/bg
	" specific var names.
	let clr_or_bgc{0} = 'clr'
	let clr_or_bgc{1} = 'bgc'
	" Initialize the vars that will accumulate table text
	let fmt_header = '' | let fmt_lines = ''
	let clr_header = '' | let clr_lines = ''
	let bgc_header = '' | let bgc_lines = ''
	" Determine number format to use for char-nr column
	let use_hex = strpart(b:txtfmt_cfg_starttok_display, 0, 2) == '0x'
	let i = 0
	while i < 2
		" Loop over all format lines (1 hdr and b:txtfmt_num_formats-1 fmt)
		let iFmt = -1	" start with header line
		while iFmt < b:txtfmt_num_formats
			let line = ''	" Initialize text for current line
			" Column 1
			if iFmt == -1
				let col1_text = ' char-nr'
			else
				let col1_text = b:txtfmt_fmt_first_tok + iFmt
				if use_hex
					" Convert to hex
					let col1_text = TxtfmtUtil_num_to_hex_str(col1_text)
				endif
				" Prepend space for alignment
				let col1_text = ' ' . col1_text
			endif
			if i == 0
				" Calculate col width
				if strlen(col1_text) > cw1
					let cw1 = strlen(col1_text)
				endif
			else
				" Output line
				let line = line . (col1_text.s:MakeString(' ', cw1 + 2 - strlen(col1_text)))
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
		" Loop over fg colors and (if necessary) bg colors
		let fgbg_idx = 0
		while fgbg_idx < (b:txtfmt_cfg_bgcolor ? 2 : 1)
			if fgbg_idx == 0
				let first_tok = b:txtfmt_clr_first_tok
				let colormask = b:txtfmt_cfg_fgcolormask
			else
				let first_tok = b:txtfmt_bgc_first_tok
				let colormask = b:txtfmt_cfg_bgcolormask
			endif
			" Loop over all color tokens (even inactive ones)
			" Index note: In this loop, index 0 refers to 'no color', while index
			" 1 refers to txtfmtColor{1} (default rgb=0x000000).
			let iClr = -1
			while iClr < b:txtfmt_num_colors
				let line = ''	" Initialize text for current line
				" Column 1
				if iClr == -1
					let col1_text = ' char-nr'
				else
					if iClr >= 0
						let col1_text = (first_tok + iClr)
						if use_hex
							" Convert to hex
							let col1_text = TxtfmtUtil_num_to_hex_str(col1_text)
						endif
						" If color is inactive, prepend char-nr with asterisk
						if iClr > 0 && strpart(colormask, iClr - 1, 1) != '1'
							" This color is inactive
							let col1_text = '*' . col1_text
						else
							" Prepend space for alignment
							let col1_text = ' ' . col1_text
						endif
					endif
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
					if strpart(colormask, iClr - 1, 1) != '1'
						let col2_text = col2_text . ' (inactive)'
					endif
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
					let col3_text = b:txtfmt_{clr_or_bgc{fgbg_idx}}_namepat{iClr}
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
					let col4_text = 'clr-def'
				elseif iClr == 0
					let col4_text = 'N.A.'
				else
					let col4_text = b:txtfmt_{clr_or_bgc{fgbg_idx}}{iClr}
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
				" Accumulate line just built into the list of lines
				if i == 1
					if iClr == -1
						" Store header line separately so that echohl can be used
						if fgbg_idx == 0
							let clr_header = line
						else
							let bgc_header = line
						endif
					else
						" Regular row in table (non-header)
						if fgbg_idx == 0
							let clr_lines = clr_lines.(iClr==0?'':"\<NL>").line
						else
							let bgc_lines = bgc_lines.(iClr==0?'':"\<NL>").line
						endif
					endif
				endif
				let iClr = iClr + 1
			endwhile
			let fgbg_idx = fgbg_idx + 1
		endwhile
		let i = i + 1
	endwhile
	echohl Title
	echo b:txtfmt_cfg_bgcolor ? ' === FG COLORS ===' : ' === COLORS ==='
	echo clr_header
	echohl None
	echo clr_lines
	echohl Title
	echo ' === FORMAT ==='
	echo fmt_header
	echohl None
	echo fmt_lines
	" If bg colors are not active, we're done
	if b:txtfmt_cfg_bgcolor
		echohl Title
		echo ' === BG COLORS ==='
		echo bgc_header
		echohl None
		echo bgc_lines
	endif
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
	" Get current settings from buffer-local vars
	" Assumption: This function can be invoked only from an active txtfmt
	" buffer
	let old_starttok = b:txtfmt_cfg_starttok
	" Determine new settings
	let new_starttok = a:moveto
	" Determine amount of shift (signed value)
	let l:offset = new_starttok - old_starttok

	" Before proceeding, cache 'effective' values for bgcolor, longformats and
	" undercurl. Note that 'effective' values are those that would be in
	" effect if current Vim version were old_ver. Note that effective
	" undercurl may differ from b:txtfmt_cfg_undercurl.
	let bgcolor = b:txtfmt_cfg_bgcolor
	let longformats = b:txtfmt_cfg_longformats
	if old_ver != v:version
		" Effective undercurl could differ from b:txtfmt_cfg_undercurl
		if b:txtfmt_cfg_undercurlpref && old_ver >= b:txtfmt_const_vimver_undercurl
			" Undercurl desired and supported
			let undercurl = 1
		else
			" Undercurl either not desired or not supported
			let undercurl = 0
		endif
	else
		let undercurl = b:txtfmt_cfg_undercurl
	endif
	" Set a flag that indicates whether we will be reserving space for long
	" formats before the start of bgc range. Note that this value can be true
	" even if longformats is false. Also note that its value is N/A if
	" bgcolors are disabled.
	let lf_reserved = bgcolor && (longformats || !b:txtfmt_cfg_pack)
	" Determine size of the entire range
	let rangelen =
		\ b:txtfmt_const_tokrange_size_{bgcolor}{lf_reserved}{lf_reserved}
	" Perform upper-bound check on new range
	if !(new_starttok + rangelen - 1 <=
		\ b:txtfmt_const_tokrange_limit_{b:txtfmt_cfg_enc_class})
		" Invalid destination for move!
		echoerr "Starttok value of `".new_starttok."' causes upper bound for encoding `"
			\.b:txtfmt_cfg_enc."' to be exceeded"
		return
	endif
	" If here, move is legal.
	" Record whether buffer is modified before we start modifying it. This
	" information is used by modeline processing to determine whether save is
	" required.
	let b:txtfmt_ml_save_modified = &modified

	" Build 2 character class interiors (i.e., without the [ ]):
	" 1) all chars that are tokens under old range
	" 2) all chars that are tokens under new range
	" Begin the first range, which begins with fg color and ends either with
	" formats (no bg colors or discontinuity between formats and bg colors) or
	" bg colors.
	" Note: The end of the first range is determined independently of
	" lf_reserved, as the range includes only tokens actually used.
	let re_old_tokrange = nr2char(old_starttok).'-'
	let re_new_tokrange = nr2char(new_starttok).'-'
	if !bgcolor || !(longformats && undercurl)
		" End first range after format tokens
		" Calculate length of range
		let end_offset = b:txtfmt_const_tokrange_size_{0}{longformats}{undercurl} - 1
		" Close the range
		let re_old_tokrange = re_old_tokrange.nr2char(old_starttok + end_offset)
		let re_new_tokrange = re_new_tokrange.nr2char(new_starttok + end_offset)
		" If bgcolor is enabled, start a new range so that logic after this if
		" block needn't know or care whether it was entered
		if bgcolor
			" Determine offset to start of bgc range
			let start_offset = b:txtfmt_const_tokrange_size_{0}{lf_reserved}{lf_reserved}
			let re_old_tokrange = re_old_tokrange.nr2char(old_starttok + start_offset).'-'
			let re_new_tokrange = re_new_tokrange.nr2char(new_starttok + start_offset).'-'
		endif
	endif
	" If bgcolor is enabled, need to close the first or second range. (If no
	" bgcolor, first and only range has already been closed.)
	if bgcolor
		let end_offset = b:txtfmt_const_tokrange_size_{1}{lf_reserved}{lf_reserved} - 1
		let re_old_tokrange = re_old_tokrange.nr2char(old_starttok + end_offset)
		let re_new_tokrange = re_new_tokrange.nr2char(new_starttok + end_offset)
	endif

	" STEP 1: (If and only if escaping is permitted)
	" Before translating any tokens, need to escape characters that are not
	" currently tokens, but will be after the move. Escapes, if applicable,
	" must be taken into account.
	" Note: Also, need to escape any escape chars that would be considered escaping
	" or escaped chars after the move. E.g. (if esc=bslash)
	" <Bslash><Tok> should become <Bslash><Bslash><Bslash><Tok> to ensure that
	" the effective sequence `<Bslash><Tok>' is preserved.
	" The algorithm for `esc=bslash' may be expressed as follows: Escape each
	" char in a sequence consisting of any number of backslashes terminated
	" with a token. Note that it doesn't matter whether number of backslashes
	" is even or odd, since the assumption is that prior to the move, neither
	" the backslashes nor the token chars have any special meaning.
	if b:txtfmt_cfg_escape != 'none'
		" Note: This concat order is *much* more efficient than the
		" alternative (since tokens are less common than non-token chars)
		let re_need_esc =
			\'\%(['.re_new_tokrange.']'
			\.'\&[^'.re_old_tokrange.']\)'
		if b:txtfmt_cfg_escape == 'bslash'
			silent! exe '%s/\%(\\\%(\\*'.re_need_esc.'\)\@=\|'.re_need_esc.'\)/\\\0/g'
		elseif b:txtfmt_cfg_escape == 'self'
			" TODO_BG: Decide whether to use escape() on re_need_esc or
			" whether to hardcode the extra escapes...
			silent! exe '%s/'.substitute(b:re_no_self_esc, 'placeholder',
				\ escape(re_need_esc, '&\'), '').'/\0\0/g'
		endif
	endif

	" STEP 2: Translate token range
	let re_move = '['.re_old_tokrange.']'
	if b:txtfmt_cfg_escape != 'none'
		if b:txtfmt_cfg_escape == 'bslash'
			let re_move = b:re_no_bslash_esc.re_move
		elseif b:txtfmt_cfg_escape == 'self'
			let re_move = substitute(b:re_no_self_esc, 'placeholder', re_move, '')
		endif
	endif
	silent! exe '%s/'.re_move.'/\='
		\.'nr2char(char2nr(submatch(0)) + l:offset)'
		\.'/g'

	" STEP 3: (If and only if escaping is permitted)
	" Remove escape chars for characters that are txtfmt tokens under old
	" tokrange setting, but not under new. Also, since this post-unescaping
	" step is the complement of the pre-escaping performed above, we must
	" unescape backslashes that occur in sequences leading up to the escaped
	" token. E.g.,
	" <Bslash><Bslash><Bslash><Tok> would become <Bslash><Tok>, since neither
	" the <Bslash> nor the subsequent <Tok> is significant after the move.
	" Note that there's no need to check for even/odd number of backslashes
	" preceding tokens. The number will always be odd. For proof, see the
	" Rationale below.
	" Note: Any character that is in the old tokrange but not the new is an
	" escaped token that no longer needs escaping.
	" Rationale: All unescaped tokens of the old range have been translated,
	" and hence will be tokens in the new range as well. Thus, any token that
	" is within the old range but not within the new must, by definition, have
	" been escaped (else it would have been translated to the new range).
	" Design Decision: An escape is an escape if it's escaping any txtfmt
	" token, even a useless 'no-format' or 'no-color' token appearing outside
	" a region. (Recall that I don't highlight these to facilitate removal by
	" user...)
	" Rationale: The goal of this function is not to clean up user's file, but
	" simply to translate tokrange
	if b:txtfmt_cfg_escape != 'none'
		" Note: This concat order is *much* more efficient than the
		" alternative (since tokens are less common than non-token chars)
		let re_noneed_esc =
			\'\%(['.re_old_tokrange.']'
			\.'\&[^'.re_new_tokrange.']\)'
		" Perform substitution
		if b:txtfmt_cfg_escape == 'bslash'
			" Note: The nature of global substitutions is such that the first
			" char matched will always be an escaping (not an escaped) char.
			silent! exe '%s/\\\(\\\%(\\*'.re_noneed_esc.'\)\@=\|'.re_noneed_esc.'\)/\1/g'
		else " self-escape
			silent! exe '%s/\('.re_noneed_esc.'\)\(\1\)/\1/g'
		endif
	endif
	" Cause buffer to be refreshed with the new settings
	" Note: The following events are consumed by modeline processing logic,
	" which may need to alter the starttok value in a modeline
	" Note: <f-args> ensures that new_starttok is a string. This is important
	" because it permits the modeline processing function to respect user's
	" choice of hex or dec when altering the modeline.
	let b:txtfmt_ml_new_starttok = new_starttok
	:Refresh
endfu
endif	" if !exists('*s:MoveStartTok')
" >>>
" Function: s:GetTokInfo() <<<
" Purpose: Return a string, which gives information about a token at a
" specific line/col. If optional line/col pair is not supplied, cursor
" location will be assumed.
" Inputs:
" [line]	Optional arg #1. Line number of char for which info is desired. If
" 			present, 2nd optional arg (col) must also be supplied.
" [col]		Optional arg #2. Column number of char for which info is desired.
" 			Note: This number is actually a byte index, such as would be
" 			returned by Vim's col() function.
" Return: Variable format string as follows:
" *** fg color token ***
" c<clr_num> [(inactive)]
" Note: <clr_num> is 1 based.
" Also Note: `(inactive)' is appended if the token corresponds to a color that
" is not in the active color mask
" *** bg color token ***
" k<clr_num> [(inactive)]
" Note: <clr_num> is 1 based.
" Also Note: `(inactive)' is appended if the token corresponds to a color that
" is not in the active color mask
" *** format token ***
" f<[u][b][i]>
" i.e., the format descriptor in fiducial form
" *** non-token ***
" <char_code>
" *** invalid char location ***
" 'NUL' (just like Vim's ga builtin)
" *** invalid inputs ***
" <empty string> (and echoerr a warning)
" Note: Will show warning to user if inputs were invalid in a syntactical
" sense. (No error msg for nonexistent char position.)
" Interface note: This function is meant to be used both from a mapping (which
" assumes cursor position) and from a command (which permits user to specify
" position).
" IMPORTANT NOTE: This function is multibyte-safe.
fu! s:GetTokInfo(...)
	" The output of the if/else will be line/col of character of interest,
	" assuming the inputs are valid.
	if a:0 == 0
		" Character of interest is at cursor position
		let line = line('.')
		let col = col('.')
	elseif a:0 == 1
		" Makes no sense to supply line but not column!
		echoerr 'GetTokInfo(): Attempt to specify line without column'
		return ''
	elseif a:0 == 2
		" Check for nonnegative line number
		if a:1 =~ '^[1-9][0-9]*$'
			let line = a:1
		else
			echoerr 'GetTokInfo(): '.a:1.' is not a valid line #'
			return ''
		endif
		" Check for nonnegative col number
		if a:2 =~ '^[1-9][0-9]*$'
			let col = a:2
		else
			echoerr 'GetTokInfo(): '.a:2.' is not a valid col #'
			return ''
		endif
	else
		echoerr 'GetTokInfo(): Wrong # of args - should be 0 or 2'
		return ''
	endif
	" If here, inputs are syntactically valid and line/col represents the
	" position of character about which information is desired. Obtain a
	" string whose first character is the character of interest.
	" Note: char2nr considers only first character in a string, so we don't
	" need to strip subsequent characters yet (and we can't do so with
	" byte-aware strpart anyway).
	let ch = strpart(getline(line), col - 1)
	" Note: If input position was invalid, ch will contain empty string.
	if ch == ''
		" Char pos doesn't exist - not an error
		return 'NUL'
	endif
	" If here, we have a character! Get its character code.
	let char_nr = char2nr(ch)
	" Get *single* char corresponding to the char code.
	" Note: strpart() and expr-[] deal with bytes not chars!
	let ch = nr2char(char_nr)
	" Determine the range within which token lies
	if char_nr >= b:txtfmt_fmt_first_tok && char_nr <= b:txtfmt_fmt_last_tok
		" fmt token
		return 'f'.b:ubisrc_fmt{char_nr - b:txtfmt_fmt_first_tok}
	elseif char_nr >= b:txtfmt_clr_first_tok && char_nr <= b:txtfmt_clr_last_tok
		" clr token
		" offset 0 = 'no color', represented by 'c-'
		" offset i = txtfmtColor{i}
		" Note: User-visible array is 1-based, and b:txtfmt_clr_first_tok
		" corresponds to the default fg color token
		let offset = char_nr - b:txtfmt_clr_first_tok
		let ret_str = 'c'.(offset == 0 ? '-' : ''.offset.'')
		" Distinguish between active/inactive start color tokens
		if char_nr > b:txtfmt_clr_first_tok && ch !~ '['.b:txtfmt_re_clr_stok_atom.']'
			let ret_str = ret_str.' (inactive)'
		endif
		return ret_str
	elseif char_nr >= b:txtfmt_bgc_first_tok && char_nr <= b:txtfmt_bgc_last_tok
		" bgc token
		" offset 0 = 'no color', represented by 'k-'
		" offset i = txtfmtColor{i}
		" Note: User-visible array is 1-based, and b:txtfmt_bgc_first_tok
		" corresponds to the default bg color token
		let offset = char_nr - b:txtfmt_bgc_first_tok
		let ret_str = 'k'.(offset == 0 ? '-' : ''.offset.'')
		" Distinguish between active/inactive start color tokens
		if char_nr > b:txtfmt_bgc_first_tok && ch !~ '['.b:txtfmt_re_bgc_stok_atom.']'
			let ret_str = ret_str.' (inactive)'
		endif
		return ret_str
	else
		" Not a txtfmt token - just return ascii value
		return ''.char_nr.''
	endif
endfu
" >>>
" Menus <<<
" Return list of formats for menu in which combinations with fewer attributes
" set come first.
fu! S_Get_menu_fmts_list()
	let grps = []
	let i = 1
	while i < b:txtfmt_num_formats
		let fmt = b:ubisrc_fmt{i}
		let len = len(fmt)
		if len - 1 >= len(grps)
			call add(grps, [])
		endif
		call add(grps[len - 1], fmt)
		let i += 1
	endwhile
	" Flatten the list of grps.
	let fmts = []
	for grp in grps
		call extend(fmts, grp)
	endfor
	return fmts
endfu
" Filter out by options
fu! S_Filter_menu_fmts_list()
endfu
fu! s:Build_format_submenu(path)
	let ops = ['Add format attributes', 'Remove format attributes', 'Set format attributes']
	for op in ops
		" TODO: Need to sort such that combinations with fewer attributes set
		" come first.
		" TODO: Allow user to configure max number of attrs in a single
		" combination.
		" TODO: Allow user to specify all/any/none masks
		let i = 1
		while i < b:txtfmt_num_formats
			echo printf('menu %s.%s.%s :', escape(a:path, ' \'), escape(op, ' \'), escape(b:ubisrc_fmt{i}, ' \'))
			exe printf('menu %s.%s.%s :', escape(a:path, ' \'), escape(op, ' \'), escape(b:ubisrc_fmt{i}, ' \'))
			let i += 1
		endwhile
	endfor
endfu
fu! s:Build_color_submenu(rgn, path, name)
	if !has('menu')
		return
	endif
	if 0
		" TODO: Make this check for Txtfmt option inhibit
		return
	endif
	exe printf('menu %s.-%s- :', escape(a:path, ' \'), escape(a:name, ' \'))
	let i = 0
	" Note: b:txtfmt_num_colors includes 'no color'.
	" Also Note: <...>_namepat array is 1-based, with index 1 corresponding to
	" first actual color; <...>_colormask is 0-based, with bit 0 corresponding
	" to first actual color.
	while i < b:txtfmt_num_colors
		" TODO: Use this: b:txtfmt_cfg_{fg_or_bg}colormask[i - 1]
		if i == 1
			exe printf('menu %s.%s.%s :',
				\escape(a:path, ' \'), escape(a:name, ' \'),
				\'-real_colors-')
		endif
		exe printf('menu %s.%s.%s :',
			\escape(a:path, ' \'), escape(a:name, ' \'),
			\!i ? 'No\ color' : b:txtfmt_{a:rgn}_namepat{i})
		let i += 1
	endwhile
endfu
fu! S_Build_menus()
	unmenu Txtfmt
	call s:Build_format_submenu('Txtfmt')
	call s:Build_color_submenu('clr', 'Txtfmt', 'Set foreground color')
	call s:Build_color_submenu('bgc', 'Txtfmt', 'Set background color')

endfu
" >>>
" Configuration <<<
" Needed only for ftplugin
" Note: Performed after the Common Configuration, which sets the 'starttok'
" option, needed when processing user maps
" Check for vim-repeat <<<
" Load the autoload function if it's available, calling with invalid number of
" args to ensure the call is a nop either way.
silent! call repeat#set()
let s:have_repeat = exists('*repeat#set')
" >>>
" Function: s:Expand_user_map_macro() <<<
" Purpose: Expand the input string, which is assumed to be the `...' in one of
" the user-map expansion sequences of the form <...>.
" Return: If the macro is valid, return the expanded text, just as it would
" appear in the rhs of the map; otherwise, an empty string.
fu! s:Expand_user_map_macro(s)
	let re_ins_tok_i   = '^i\\:\(.\+\)$'
	let re_ins_tok_n   = '^n\([1-9]\d*\)\?\\\(v\?\)\([iIaAoOs]\):\(.\+\)$'
	let re_jump_to_tok = '^\([nvio]\)\([1-9]\d*\)\?\([][]\)\(t\?\)\([be]\?[fkca]\)'
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
		let expseq = s:Expand_user_map_macro(seq)
		" Was it valid?
		if expseq == ''
			let s:err_str = "Invalid usermap rhs: " . seq
			return ''
		endif
		" Append the expanded text to the return string
		let ret_str = ret_str.expseq
		" Make i2 point just past `>>' (it's on the 1st `>')
		let i2 = i2+2
	endwhile
	" Return the now completely expanded string
	return ret_str
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
" Function: s:Check_map_lhs() <<<
" Return true iff a map from 'lhs' in 'mode' should be created, displaying
" warnings/errors for map conficts/ambiguities in accordance with txtfmtMapwarn
" option setting.
fu! s:Check_map_lhs(mode, lhs)
	" Make sure there's no conflict or ambiguity between an existing map
	" and the default one we plan to add...
	let oldarg = maparg(a:lhs, a:mode)
	let oldchk = mapcheck(a:lhs, a:mode)
	" Check for conflicts and ambiguities, decoding applicable portions of
	" mapwarn option character flag string into more immediately useful
	" variables, to avoid messy ternaries in the subsequent logic.
	" Note: Create only the variables that will be used.
	" TODO: Refactor this logic into a function that returns an object
	" representing the result, so it can be used by both Def_map2() and
	" Def_map1().
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
			if !l:once_only || !s:Mapwarn_check(a:lhs, l:old_rhs, a:mode, l:once_only)
				let l:warnstr = 'Level 1 map '
					\.(l:problem == 'a' ? 'ambiguity:' : 'conflict: ')
					\.a:lhs.' already mapped to '.l:old_rhs
				if msg_or_err == 'm'
					call s:Msg('warning', l:warnstr)
				else
					call s:Msg('error', l:warnstr)
				endif
			endif
		endif
	endif
	" Return true iff map should be created.
	return !exists('l:problem') || l:create
endfu
" >>>
" Function: s:Def_map1() <<<
" Define the single-level map corresponding to the inputs, handling any
" conflicts or ambiguities in accordance with option txtfmtMapwarn.
fu! s:Def_map1(mode, lhs, rhs)
	" Make sure <...> key notation works in mappings.
	let save_cpo = &cpo
	set nocp
	try
		if a:mode !~ '^[xsvn]$'
			echoerr 'Internal error - unsupported mapmode passed to Def_map1()'
			return 1
		endif
		" Do the map for buffer unless map creation is precluded by conflict
		" or ambiguity in absence of the 'create' flag.
		" Note: Do not use <unique> attribute, since that would cause an error if
		" we're overriding.
		if s:Check_map_lhs(a:mode, a:lhs)
			" Construct the :map command.
			exe a:mode . 'map <buffer> <silent> '.a:lhs.' '.a:rhs
			" Create undo action for the map just created
			call s:Undef_map(a:lhs, a:rhs, a:mode)
		endif
	catch
		let &cpo = save_cpo
	endtry
endfu
" >>>
" Function: s:Remap_shortcut_leader() <<<
" Return a copy of the input map lhs with its leading portion remapped (if
" appropriate) in accordance with option txtfmtDefaultShortcutLeaders{}.
" Note: The translation performed is strictly textual and case-sensitive.
" I've considered performing canonicalization on special key notations (e.g.,
" "<Bslash>" => "<bslash>", "\t" => "<tab>"), but the value of this
" canonicalization seems rather limited: since the map lhs's being translated
" are all statically defined in canonical form by the plugin, the only value in
" canonicalization would be in giving the user some leeway in specifying the
" pattern(s). But a user setting txtfmtDefaultShortcutLeaders{} will be looking
" at the default entries in the documentation (which mentions the textual nature
" of the substitutions), so getting it right shouldn't be difficult.
" Consequently, though canonicalization might be worth doing eventually, it's
" not high priority...
fu! s:Remap_shortcut_leader(lhs)
	" Merge buf-local and global translation objects, with buf-local taking precedence.
	let tr = get(g:, 'txtfmtDefaultShortcutLeaders', {})->extend(get(b:, 'txtfmtDefaultShortcutLeaders', {}))
	let lhs = a:lhs
	" Guarantee: Because these maps are internally created, we can guarantee
	" that a leading < is *always* the start of special key notation.
	" Check against each key in the translation mapping.
	" TODO: Consider building tr in canonicalized form to obviate need for loop.
	for [patt, repl] in items(tr)
		" Anchor pattern at start of lhs and treat pattern as "very magic".
		let translated_lhs = substitute(lhs, '^\v%(' . patt . ')', repl, '')
		if translated_lhs != lhs
			" Translation was performed.
			return translated_lhs
		endif
	endfor
	return lhs
endfu
" >>>
" Function: s:Get_shortcut_flags() <<<
fu! s:Get_shortcut_flags()
	return {
		\ 'workInSelect': get(b:, 'txtfmtShortcutsWorkInSelect',
			\ get (g:, 'txtfmtShortcutsWorkInSelect', 0)),
		\ 'stayInVisual': get(b:, 'txtfmtShortcutsStayInVisual',
			\ get (g:, 'txtfmtShortcutsStayInVisual', 0))
	\ }
endfu
" >>>
" Function: s:Parse_user_shortcut_modes() <<<
" Input: The mode string (possibly empty) excluding the trailing `:' but
" including any stayInVisual +/- flags, the lhs and a dict containing the
" global/buf-local flags. Empty mode string implies defaults.
" Return: A dict corresponding to a single lhs
" See Parse_user_shortcut_string for format descriptions.
" Note: The flags in the returned dict represent only the *overrides* of the
" global/buf-local options: global/buf-local defaults will be taken into account
" at higher level.
fu! s:Parse_user_shortcut_modes(modes, lhs, optFlags)
	" Validate and parse the mode string, deferring mode validation until after
	" overall form of string has been validated.
	" Note: Trailing `:' is stripped by caller.
	let re_modes = '\%(\([xvosncitl]*\)\([-+]\?\)\)'
	" Design Decision: Allow whitespace around both modes and lhs if caller
	" allows it. (Currently, it's allowed after the `:' in modestring only, but
	" validation of the overall form is the responsibility of caller(s).)
	let [modes, lhs] = [trim(a:modes), trim(a:lhs)]
	let m = matchlist(modes, re_modes)
	" Check for invalid format.
	if empty(m)
		throw "Invalid shortcut map mode string: expected `[xvo]*[-+]?:', got `" . modes . ":'"
	endif
	" We have a valid mode string.
	let [_, modes, flag; rest] = m
	" Check for valid format with unsupported modes.
	if modes !~ '^[xvo]*$'
		throw "Unsupported mode(s) in shortcut map mode string: " . substitute(modes, '[xvo]', '', 'g')
	endif
	" Note: flags dict reflects only the mode-specific flag *overrides*.
	let flags = {}
	" Process any trailing +/- as stayInVisual flag.
	if !flag->empty()
		let flags.stayInVisual = flag == '+' ? 1 : 0
	endif
	if modes->empty()
		" Omitted modes implies defaults.
		let modes = (a:optFlags.workInSelect ? 'v' : 'x') . 'o'
	else
		" Canonicalize the mode string.
		let modes = modes->split('\zs')->sort()->uniq()->join('')
		" Perform additional validation on mode string.
		if modes =~ '.*x\&.*v'
			throw "Shortcut map mode string cannot contain both 'v' and 'x'"
		endif
		if flags->has_key('stayInVisual') && modes !~ '[xv]'
			call s:Msg('warning', "Ignoring useless 'stayInVisual' flag in mode string `"
				\ . modes . flag . "'")
		endif
	endif
	" Make sure lhs is a string.
	if type(lhs) != type("")
		throw "Expected shortcut map lhs to be of type string, got type " . type(lhs)
	elseif lhs->empty()
		throw "shortcut map lhs is empty"
	endif
	" Return entry for a single lhs.
	return {'modes': modes, 'lhs': lhs, 'flags': flags}
endfu
" >>>
" Function: s:Parse_user_shortcut_string() <<<
" Inputs: A string with one of the following formats:
"   ([xvo]*[-+]?:LHS)+ SPEC
" | LHS SPEC
" Note: The keys in the lhs dict can be empty (indicating default modes).
" Return: A Dict with the following format:
"   {'lhs': [{'modes': MODES, 'lhs': LHS, 'flags': FLAGS}, ...], 'rhs': RHS}
" Note: There are really only two valid use cases for multiple mode chars in a
" single LHS:
" 1. disabling select-mode map for a single shortcut when 
"    txtfmtShortcutsWorkInSelect=1: e.g., 'xo: \b fb'
" 2. desire to use mode-specific syntax for aesthetic reasons, even when
"    Visual and Operator maps use same lhs: e.g., 'vo: \b fb'
" Constraint: It makes no sense to allow 'v' with either 'x' or 's', so disallow
" it.
" Note: By default, shortcuts are defined with xmap, and thus, work only in
" visual mode; option txtfmtShortcutsWorkInSelect causes vmap to be used
" instead, but regardless of the setting of that option, it's always
" possible to specify each shortcut's behavior explicitly using
" mode-specific syntax.
" Disambiguation Note: With the simple, non-mode-specific syntax, the
" prohibition on whitespace in LHS prevents ambiguity. With the mode-specific
" syntax, ambiguity is prevented by the requirement of a `:' in each mode
" prefix, coupled with the impossibility (or at least, extreme improbability) of
" `:` in a highlighting spec. It's actually theoretically possible to have a
" colon in a custom color string, but a user who does something that silly will
" just have to disambiguate some other way: e.g., using Dictionary syntax or by
" ensuring the highlighting spec contains no whitespace.
" Rationale: Since a highlighting spec is always preceded by whitespace, a colon
" within a highlighting spec that appears as an unbroken sequence of
" non-whitespace characters could never be interpreted as part of a
" mode-specific lhs component, as backtracking would always give it back to be
" matched as part of rhs.
" Example: Multiple mode-specific LHS:
"   x+:\b o:<C-i> fbi
"   ==> {'lhs': [{'modes': 'x', 'lhs': '\b', 'flags': {'stayInVisual': 1}},
"                {'modes': 'o', 'lhs': '<C-i>', 'flags': {}}],
"        'rhs': 'fbi'}
fu! s:Parse_user_shortcut_string(sc)
	let sc = trim(a:sc)
	if empty(sc)
		" Special Case: Empty shortcut entry isn't an error.
		" Rationale: Depending on how user defines list, he may appreciate being
		" able to disable an entry simply by making it empty.
		return {}
	endif
	" In the following regex:
	" $1=([xvo]*[-+]?:LHS)+
	" $2=LHS
	" $3=SPEC
	" Note: Allow whitespace after the colon for readability.
	" Rationale: Non-whitespace must follow the *colon* before rhs.
	" Note: Explicit mode-specific syntax should be used to disambiguate a lhs
	" containing a `:'.
	" Note: To help user debug invalid shortcut definitions, leave detection of
	" invalid mode chars to Parse_user_shortcut_modes().
	let re_modes = '\%([xvosncitl]*[-+]\?:\s*\)'
	let re_lhs = '\%(\S\+\)'
	let re_mlhs = re_modes . re_lhs
	let re = '^\s*\%(\(\%(' . re_mlhs . '\)\%(\s\+' . re_mlhs . '\)*\)'
				\ . '\|\(' . re_lhs . '\)\)\s\+\(.*\S\)\s*$'
	" Validate the format and split into components.
	let m = matchlist(sc, re)
	if empty(m)
		throw 'Invalid shortcut map definition'
	endif
	" Get relevant global/buf-local flags.
	let flags = s:Get_shortcut_flags()
	if empty(m[1])
		" Single LHS for *all* modes
		let ret = {'lhs': [s:Parse_user_shortcut_modes('', m[2], flags)],
					\ 'rhs': m[3]}
	else
		" Multiple LHS will be appended in loop.
		let ret = {'lhs': [], 'rhs': m[3]}
		" Strip off and process the LHS segments in loop.
		let [sidx, idx] = [0, 0]
		let lhs_seq = m[1]
		while idx >= 0
			" FIXME: Anchor at start and trim whitespace.
			let idx = matchend(lhs_seq, '^\s*' . re_mlhs, sidx)
			if idx >= 0
				let mlhs = lhs_seq[sidx:idx-1]
				let sidx = idx
				" Split the mode prefix from the LHS.
				let idx = stridx(mlhs, ':')
				" Permit whitespace following the `:'
				let [modes, lhs] = [mlhs[:idx-1], mlhs[idx+1:]]
				" Add an entry to the returned object for the current mode-specific lhs.
				call add(ret.lhs, s:Parse_user_shortcut_modes(modes, lhs, flags))
			endif
		endwhile
	endif
	return ret
endfu
" >>>
" Function: s:Parse_user_shortcut_dict() <<<
" Input: A dict with one of the following formats:
"   {'lhs': LHS, 'rhs': RHS}
" | {'lhs': {[xvo]*[-+]?: LHS, ...}, 'rhs': RHS}
" Note: The keys in the lhs dict can be empty (indicating default modes).
" Return: Same as s:Parse_user_shortcut_string()
fu! s:Parse_user_shortcut_dict(sc)
	if empty(a:sc)
		" Design Decision: Empty element not treated as error.
		return {}
	endif
	" Make sure rhs is a string.
	if !has_key(a:sc, 'rhs')
		throw "Shortcut map dict missing key 'rhs'"
	endif
	if type(a:sc.rhs) != type("")
		throw "Expected shortcut map rhs to be of type string: " . string(a:sc.rhs)
	endif
	let ret = {'lhs': [], 'rhs': a:sc.rhs}
	" Get relevant global/buf-local flags.
	let flags = s:Get_shortcut_flags()
	if !has_key(a:sc, 'lhs')
		throw "Shortcut map dict missing key 'lhs'"
	elseif type(a:sc.lhs) == type("")
		" Single lhs for all modes
		call add(ret.lhs, s:Parse_user_shortcut_modes('', a:sc.lhs, flags))
	elseif type(a:sc.lhs) == type({})
		" Multiple, mode-specific lhs's.
		for [modes, lhs] in items(a:sc.lhs)
			" Add an entry to the returned object for the current mode-specific lhs.
			call add(ret.lhs, s:Parse_user_shortcut_modes(modes, lhs, flags))
		endfor
	else
		" Invalid lhs type!
		throw "Expected shortcut map lhs to be of type string or dictionary, got type " . type(a:sc.lhs)
	endif
	return ret
endfu
" >>>
" Function: s:Parse_user_shortcut() <<<
fu! s:Parse_user_shortcut(sc)
	if type(a:sc) == type("")
		let ret = s:Parse_user_shortcut_string(a:sc)
	elseif type(a:sc) == type({})
		let ret = s:Parse_user_shortcut_dict(a:sc)
	else
		throw "Expected shortcut map entry to be of type string or dictionary, got type " . type(a:sc)
	endif
	return ret
endfu
" >>>
" Function: s:Do_shortcut_maps() <<<
" Purpose: Create any auto map shortcuts defined in script-local (defaults),
" global and buf-local lists.
" Logic: Maps are defined in the following order: script-local, global,
" buf-local, with later definitions overriding earlier ones (possibly with
" warnings to user, as determined by the txtfmtMapwarn option). The script-local
" default maps are defined if and only if the txtfmtEnableDefaultShortcuts
" option is set.
fu! s:Do_shortcut_maps()
	" By default, builtin (default) shortcuts are not defined, but user can
	" enable with global or buf-local option (with the latter taking
	" precedence over the former).
	let enable_default_shortcuts =
			\ get(b:, 'txtfmtEnableDefaultShortcuts',
			\ get(g:, 'txtfmtEnableDefaultShortcuts', 0))
	" Note: This global/buf-local flag may be overridden by mode-specific flags.
	let globalStayInVisual = s:Get_shortcut_flags().stayInVisual
	" Loop over static defaults (if applicable), global, then buf-local instance of txtfmtShortcuts.
	" Rationale: Globals override defaults, buf-locals override globals.
	for scope in enable_default_shortcuts ? [s:, g:, b:] : [g:, b:]
		" Maintain existence hash used to detect (presumably unintentional) overrides.
		let maps = {}
		let scope_prefix = scope is s: ? 's' : scope is g: ? 'g' : scope is b: ? 'b' : ''
		" Increment at *start* of loop to facilitate use of 'continue'.
		let i = -1
		for s in get(scope, 'txtfmtShortcuts', [])
			let i += 1
			try
				" Validate and canonicalize the shortcut definition.
				let sc = s:Parse_user_shortcut(s)
			catch
				call s:Msg('warning'
					\, 'Ignoring malformed user-defined shortcut specified by '
					\, (scope is b: ? 'b:' : 'g:') . 'txtfmtShortcuts[' . i . '] = ' . string(s)
					\, "Exception at " . v:throwpoint . ": " . string(v:exception)
					\, ":help txtfmt-auto-map-shortcuts")
				continue
			endtry
			" Design Decision: Silently skip empty entries.
			if !empty(sc)
				try
					" Parse the spec for validation purposes.
					let pspecs = s:Parse_fmt_clr_transformer(sc.rhs)
				catch
					call s:Msg('warning'
						\, 'Ignoring the following user-defined shortcut:'
						\, (scope is b: ? 'b:' : 'g:') . 'txtfmtShortcuts[' . i . '] = ' . string(s)
						\, "Exception at " . v:throwpoint . ": Highlighting spec `" . sc.rhs . "' is invalid: " . string(v:exception)
						\, ":help txtfmt-auto-map-shortcuts")
					continue
				endtry
				" Create a string representing the spec suitable for wrapping
				" within '...' in function call: basically, the original string
				" with any single quotes (shouldn't really have any) doubled.
				let escaped_rhs = substitute(sc.rhs, "'", "''", "g")
				" Loop over the mode-specific lhs's, defining maps.
				for lhs_entry in sc.lhs
					" Perform any requested lhs remapping (default shortcut maps only).
					let lhs = scope is s: ? s:Remap_shortcut_leader(lhs_entry.lhs) : lhs_entry.lhs
					" Allow global/buf-local setting to be overridden by mode-specific flag.
					let stayInVisual = get(lhs_entry.flags, 'stayInVisual', globalStayInVisual)
					" Loop over modes in the mode string.
					for mode in lhs_entry.modes->split('\zs')
						let map_mode = mode
						if mode =~ '[xv]'
							" Create a Visual map.
							let rhs = ":<C-U>call <SID>Highlight_visual('" . escaped_rhs . "')<CR>"
							if mode =~ '[xv]' && stayInVisual
								" Note: The gv on the end of the map is unnecessary but harmless
								" when a vmap is executed in select-mode. It has to be done this
								" way because maps created by vmap and smap behave completely
								" differently in select-mode, such that creating a Txtfmt map with
								" smap would be pointless.
								" :help Select-mode-mapping
								let rhs .= 'gv'
							endif
						else " mode == 'o' (creates operator map)
							" Terminology: Mode char of 'o' denotes a Txtfmt "operator", which is
							" implemented as a Vim normal mode map: hence, the distinction between
							" mode and map_mode here.
							let map_mode = 'n'
							if has('lambda')
								let rhs = ':set opfunc={mode\ ->\ <SID>Highlight_operator(mode,\ '''
											\ . escape(escaped_rhs, ' \') . "')}<CR>g@"
							else
								" User is using very old (pre-8) version of Vim. No lambdas, so pass
								" the spec to wrapper via script-local var.
								" Note: This is obviously not thread-safe, but user maps are
								" inherently single-threaded...
								let rhs = ":let s:txtfmt_active_shortcut_spec = '" . escaped_rhs . "'"
									\ . "\<C-V>|set opfunc=<SID>Highlight_operator_wrapper<CR>g@"
							endif
						endif
						" Make sure we're not overriding a map already created at same scope.
						" Rationale: This would almost certainly be a mistake, whereas buf-local
						" overriding global or default isn't necessarily a mistake, and will be
						" handled by conflict/ambiguity mechanism in Def_map1().
						let k = scope_prefix . ':' . mode . ':' . lhs
						if maps->has_key(k) && maps[k] != rhs
							let map_lhs_display = "  " . map_mode . 'map ' . lhs . " "
							call s:Msg('warning'
								\, 'The following user-defined shortcut specified by '
								\. (scope is b: ? 'b:' : 'g:') . 'txtfmtShortcuts[' . i . ']...'
								\, map_lhs_display . rhs
								\, '...redefines the following:'
								\, map_lhs_display . maps[k]
								\, ":help txtfmt-auto-map-shortcuts")
						endif
						let maps[k] = rhs
						" Create the map (and corresponding undo).
						call s:Def_map1(map_mode, lhs, rhs)
					endfor
				endfor
			endif
		endfor
	endfor
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
" Function: s:Define_user_map_defaults() <<<
" Purpose: Set up some default user maps for testing...
fu! s:Define_user_map_defaults()
	" User map definition examples for test <<<

	" Map CTRL-B in insert mode to start and terminate a 'bold' region,
	" leaving the cursor positioned in the region interior, ready to type bold
	" text.
	" Hint: Similar maps might be created for underline and italic
	let g:txtfmtUsermap1 = 'inoremap <C-B> <<i\:fb.f->>'

	" Map CTRL-\f in insert mode to end current format region.
	let g:txtfmtUsermap2 = 'inoremap <C-\>f <<i\:f->>'

	" Map CTRL-\k in insert mode to end current bg color region.
	let g:txtfmtUsermap3 = 'inoremap <C-\>k <<i\:k->>'

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

	" Map <LocalLeader>rb in normal mode to make the current line bold with a
	" red background.
	let g:txtfmtUsermap12 =
	    \'nnoremap <LocalLeader>rb <<n\vI:kr,fb>><<n\vA:f-,k->>'
	" >>>
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
	" IMPORTANT TODO: Vim doesn't allow multi-byte characters above 255 to be
	" excluded!
	" Decide whether there's a workaround. For now, don't do this if we're
	" dealing with tokens above 255.
	" Note: I'm intentionally including inactive color tokens in the ranges.
	" Rationale: I don't feel that the complexity that would be added by the
	" logic to exclude them is justified by any advantage doing so would
	" provide.
	if (b:txtfmt_last_tok <= 255)
		let val = '^'.b:txtfmt_clr_first_tok.'-'.b:txtfmt_last_tok
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
	" IMPORTANT TODO: !!! The s:txtfmtUsermaplimit var set here appears not to
	" be used any longer! s:Do_user_maps handles everything, and it checks for
	" buffer-local option (as it should, and as this doesn't). Confirm, and
	" remove this...
	if exists('g:txtfmtUsermaplimit') && g:txtfmtUsermaplimit =~ '^\%(0[xX]\)\?[0-9]\+$'
		let s:txtfmtUsermaplimit = g:txtfmtUsermaplimit
	else
		" Set to reasonable default
		let s:txtfmtUsermaplimit = 25
	endif
	" >>>
	" TEST ONLY: Define some default user-maps for testing <<<
	"call s:Define_user_map_defaults()
	" >>>
	" Process any user-defined maps <<<
	call s:Do_user_maps()
	" >>>
	" Process any user-defined shortcut (preset) auto maps <<<
	call s:Do_shortcut_maps()
	" >>>
endfu
" >>>
call s:Do_config()
" >>>
" Public-interface functions <<<
" Function: g:Txtfmt_GetTokInfo() <<<
" !!!!!!!!!!!!!!!!!!!!!!
" !!!!! DEPRECATED !!!!!
" !!!!!!!!!!!!!!!!!!!!!!
" Purpose: Return a string, which gives information about a token at a
" specific line/col. If optional line/col pair is not supplied, cursor
" location will be assumed.
" Important Note: This function is conceptually a wrapper for script-local
" s:GetTokInfo. For backwards-compatibility reasons, however, the meaning of
" the 'col' parameter is slightly different. For this function, col represents
" a 1-based char index; for s:GetTokInfo it is a 1-based byte index.
" Note: See s:GetTokInfo for additional description
" Interface note: This function is meant to be used by plugin user; e.g., from
" mappings.
" IMPORTANT NOTE: This function now works for multibyte encodings.
fu! Txtfmt_GetTokInfo(...)
	" Call s:GetTokInfo with the appropriate arguments
	if a:0 == 0
		return s:GetTokInfo()
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
			" to 1-based byte index for strpart.
			let col = byteidx(getline(line), a:2 - 1) + 1
			if col == 0
				" Invalid (too large) col position - not error...
				return 'NUL'
			else
				return s:GetTokInfo(line, col)
			endif
		else
			echoerr 'Txtfmt_GetTokInfo(): '.a:2.' is not a valid col #'
			return ''
		endif
	else
		echoerr 'Txtfmt_GetTokInfo(): Wrong # of args - should be 0 or 2'
		return ''
	endif
endfu
" >>>
" Function: g:OldTxtfmt_GetTokInfo() <<<
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
" TODO_BG: Delete this "old" version of the function if I haven't rolled back
" prior to the release of 2.0...
fu! OldTxtfmt_GetTokInfo(...)
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
	" Note: Caller shouldn't be passing empty or effectively empty string, but
	" if he does, handle here, as Translate_fmt_clr_list considers it error.
	if a:s =~ '^\s*$'
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
com! -buffer -nargs=* GetTokInfo echo <SID>GetTokInfo(<f-args>)
" Note: No special retab logic if leading indent is highlighted like text.
if b:txtfmt_cfg_leadingindent != 'none'
com! -buffer -bang -nargs=1 -range=% Retab call <SID>Retab(<line1>, <line2>, '<bang>', <f-args>)
endif
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
call s:Def_map2('n', '[bf', '<Plug>TxtfmtBckToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bf', 'b', 0)<CR>")
call s:Def_map2('n', ']bf', '<Plug>TxtfmtFwdToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bf', 'f', 0)<CR>")
call s:Def_map2('n', '[bc', '<Plug>TxtfmtBckToClrBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bc', 'b', 0)<CR>")
call s:Def_map2('n', ']bc', '<Plug>TxtfmtFwdToClrBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bc', 'f', 0)<CR>")
call s:Def_map2('n', '[bk', '<Plug>TxtfmtBckToBgcBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bk', 'b', 0)<CR>")
call s:Def_map2('n', ']bk', '<Plug>TxtfmtFwdToBgcBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bk', 'f', 0)<CR>")
call s:Def_map2('n', '[ba', '<Plug>TxtfmtBckToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'ba', 'b', 0)<CR>")
call s:Def_map2('n', ']ba', '<Plug>TxtfmtFwdToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'ba', 'f', 0)<CR>")
call s:Def_map2('n', '[f' , '<Plug>TxtfmtBckToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'f' , 'b', 0)<CR>")
call s:Def_map2('n', ']f' , '<Plug>TxtfmtFwdToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'f' , 'f', 0)<CR>")
call s:Def_map2('n', '[c' , '<Plug>TxtfmtBckToClrTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'c' , 'b', 0)<CR>")
call s:Def_map2('n', ']c' , '<Plug>TxtfmtFwdToClrTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'c' , 'f', 0)<CR>")
call s:Def_map2('n', '[k' , '<Plug>TxtfmtBckToBgcTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'k' , 'b', 0)<CR>")
call s:Def_map2('n', ']k' , '<Plug>TxtfmtFwdToBgcTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'k' , 'f', 0)<CR>")
call s:Def_map2('n', '[a' , '<Plug>TxtfmtBckToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'a' , 'b', 0)<CR>")
call s:Def_map2('n', ']a' , '<Plug>TxtfmtFwdToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'a' , 'f', 0)<CR>")
call s:Def_map2('n', '[ef', '<Plug>TxtfmtBckToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ef', 'b', 0)<CR>")
call s:Def_map2('n', ']ef', '<Plug>TxtfmtFwdToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ef', 'f', 0)<CR>")
call s:Def_map2('n', '[ec', '<Plug>TxtfmtBckToClrEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ec', 'b', 0)<CR>")
call s:Def_map2('n', ']ec', '<Plug>TxtfmtFwdToClrEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ec', 'f', 0)<CR>")
call s:Def_map2('n', '[ek', '<Plug>TxtfmtBckToBgcEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ek', 'b', 0)<CR>")
call s:Def_map2('n', ']ek', '<Plug>TxtfmtFwdToBgcEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ek', 'f', 0)<CR>")
call s:Def_map2('n', '[ea', '<Plug>TxtfmtBckToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ea', 'b', 0)<CR>")
call s:Def_map2('n', ']ea', '<Plug>TxtfmtFwdToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ea', 'f', 0)<CR>")
" >>>
" visual mode jump 'to' token mappings <<<
call s:Def_map2('v', '[bf', '<Plug>TxtfmtBckToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bf', 'b', 0)<CR>")
call s:Def_map2('v', ']bf', '<Plug>TxtfmtFwdToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bf', 'f', 0)<CR>")
call s:Def_map2('v', '[bc', '<Plug>TxtfmtBckToClrBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bc', 'b', 0)<CR>")
call s:Def_map2('v', ']bc', '<Plug>TxtfmtFwdToClrBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bc', 'f', 0)<CR>")
call s:Def_map2('v', '[bk', '<Plug>TxtfmtBckToBgcBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bk', 'b', 0)<CR>")
call s:Def_map2('v', ']bk', '<Plug>TxtfmtFwdToBgcBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bk', 'f', 0)<CR>")
call s:Def_map2('v', '[ba', '<Plug>TxtfmtBckToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'ba', 'b', 0)<CR>")
call s:Def_map2('v', ']ba', '<Plug>TxtfmtFwdToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'ba', 'f', 0)<CR>")
call s:Def_map2('v', '[f' , '<Plug>TxtfmtBckToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'f' , 'b', 0)<CR>")
call s:Def_map2('v', ']f' , '<Plug>TxtfmtFwdToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'f' , 'f', 0)<CR>")
call s:Def_map2('v', '[c' , '<Plug>TxtfmtBckToClrTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'c' , 'b', 0)<CR>")
call s:Def_map2('v', ']c' , '<Plug>TxtfmtFwdToClrTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'c' , 'f', 0)<CR>")
call s:Def_map2('v', '[k' , '<Plug>TxtfmtBckToBgcTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'k' , 'b', 0)<CR>")
call s:Def_map2('v', ']k' , '<Plug>TxtfmtFwdToBgcTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'k' , 'f', 0)<CR>")
call s:Def_map2('v', '[a' , '<Plug>TxtfmtBckToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'a' , 'b', 0)<CR>")
call s:Def_map2('v', ']a' , '<Plug>TxtfmtFwdToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'a' , 'f', 0)<CR>")
call s:Def_map2('v', '[ef', '<Plug>TxtfmtBckToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ef', 'b', 0)<CR>")
call s:Def_map2('v', ']ef', '<Plug>TxtfmtFwdToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ef', 'f', 0)<CR>")
call s:Def_map2('v', '[ec', '<Plug>TxtfmtBckToClrEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ec', 'b', 0)<CR>")
call s:Def_map2('v', ']ec', '<Plug>TxtfmtFwdToClrEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ec', 'f', 0)<CR>")
call s:Def_map2('v', '[ek', '<Plug>TxtfmtBckToBgcEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ek', 'b', 0)<CR>")
call s:Def_map2('v', ']ek', '<Plug>TxtfmtFwdToBgcEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ek', 'f', 0)<CR>")
call s:Def_map2('v', '[ea', '<Plug>TxtfmtBckToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ea', 'b', 0)<CR>")
call s:Def_map2('v', ']ea', '<Plug>TxtfmtFwdToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ea', 'f', 0)<CR>")
" >>>
" operator-pending mode jump 'to' token mappings <<<
" Note: 'v' can be used with these to toggle inclusive/exclusive
call s:Def_map2('o', '[bf', '<Plug>TxtfmtBckToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bf', 'b', 0)<CR>")
call s:Def_map2('o', ']bf', '<Plug>TxtfmtFwdToFmtBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bf', 'f', 0)<CR>")
call s:Def_map2('o', '[bc', '<Plug>TxtfmtBckToClrBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bc', 'b', 0)<CR>")
call s:Def_map2('o', ']bc', '<Plug>TxtfmtFwdToClrBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bc', 'f', 0)<CR>")
call s:Def_map2('o', '[bk', '<Plug>TxtfmtBckToBgcBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bk', 'b', 0)<CR>")
call s:Def_map2('o', ']bk', '<Plug>TxtfmtFwdToBgcBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bk', 'f', 0)<CR>")
call s:Def_map2('o', '[ba', '<Plug>TxtfmtBckToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'ba', 'b', 0)<CR>")
call s:Def_map2('o', ']ba', '<Plug>TxtfmtFwdToAnyBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'ba', 'f', 0)<CR>")
call s:Def_map2('o', '[f' , '<Plug>TxtfmtBckToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'f' , 'b', 0)<CR>")
call s:Def_map2('o', ']f' , '<Plug>TxtfmtFwdToFmtTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'f' , 'f', 0)<CR>")
call s:Def_map2('o', '[c' , '<Plug>TxtfmtBckToClrTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'c' , 'b', 0)<CR>")
call s:Def_map2('o', ']c' , '<Plug>TxtfmtFwdToClrTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'c' , 'f', 0)<CR>")
call s:Def_map2('o', '[k' , '<Plug>TxtfmtBckToBgcTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'k' , 'b', 0)<CR>")
call s:Def_map2('o', ']k' , '<Plug>TxtfmtFwdToBgcTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'k' , 'f', 0)<CR>")
call s:Def_map2('o', '[a' , '<Plug>TxtfmtBckToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'a' , 'b', 0)<CR>")
call s:Def_map2('o', ']a' , '<Plug>TxtfmtFwdToAnyTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'a' , 'f', 0)<CR>")
call s:Def_map2('o', '[ef', '<Plug>TxtfmtBckToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ef', 'b', 0)<CR>")
call s:Def_map2('o', ']ef', '<Plug>TxtfmtFwdToFmtEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ef', 'f', 0)<CR>")
call s:Def_map2('o', '[ec', '<Plug>TxtfmtBckToClrEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ec', 'b', 0)<CR>")
call s:Def_map2('o', ']ec', '<Plug>TxtfmtFwdToClrEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ec', 'f', 0)<CR>")
call s:Def_map2('o', '[ek', '<Plug>TxtfmtBckToBgcEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ek', 'b', 0)<CR>")
call s:Def_map2('o', ']ek', '<Plug>TxtfmtFwdToBgcEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ek', 'f', 0)<CR>")
call s:Def_map2('o', '[ea', '<Plug>TxtfmtBckToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ea', 'b', 0)<CR>")
call s:Def_map2('o', ']ea', '<Plug>TxtfmtFwdToAnyEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ea', 'f', 0)<CR>")
" >>>
" normal mode jump 'till' token mappings <<<
call s:Def_map2('n', '[tbf', '<Plug>TxtfmtBckTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bf', 'b', 1)<CR>")
call s:Def_map2('n', ']tbf', '<Plug>TxtfmtFwdTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bf', 'f', 1)<CR>")
call s:Def_map2('n', '[tbc', '<Plug>TxtfmtBckTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bc', 'b', 1)<CR>")
call s:Def_map2('n', ']tbc', '<Plug>TxtfmtFwdTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bc', 'f', 1)<CR>")
call s:Def_map2('n', '[tbk', '<Plug>TxtfmtBckTillBgcBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bk', 'b', 1)<CR>")
call s:Def_map2('n', ']tbk', '<Plug>TxtfmtFwdTillBgcBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'bk', 'f', 1)<CR>")
call s:Def_map2('n', '[tba', '<Plug>TxtfmtBckTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'ba', 'b', 1)<CR>")
call s:Def_map2('n', ']tba', '<Plug>TxtfmtFwdTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('n', 'ba', 'f', 1)<CR>")
call s:Def_map2('n', '[tf' , '<Plug>TxtfmtBckTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'f' , 'b', 1)<CR>")
call s:Def_map2('n', ']tf' , '<Plug>TxtfmtFwdTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'f' , 'f', 1)<CR>")
call s:Def_map2('n', '[tc' , '<Plug>TxtfmtBckTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'c' , 'b', 1)<CR>")
call s:Def_map2('n', ']tc' , '<Plug>TxtfmtFwdTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'c' , 'f', 1)<CR>")
call s:Def_map2('n', '[tk' , '<Plug>TxtfmtBckTillBgcTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'k' , 'b', 1)<CR>")
call s:Def_map2('n', ']tk' , '<Plug>TxtfmtFwdTillBgcTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'k' , 'f', 1)<CR>")
call s:Def_map2('n', '[ta' , '<Plug>TxtfmtBckTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'a' , 'b', 1)<CR>")
call s:Def_map2('n', ']ta' , '<Plug>TxtfmtFwdTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('n', 'a' , 'f', 1)<CR>")
call s:Def_map2('n', '[tef', '<Plug>TxtfmtBckTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ef', 'b', 1)<CR>")
call s:Def_map2('n', ']tef', '<Plug>TxtfmtFwdTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ef', 'f', 1)<CR>")
call s:Def_map2('n', '[tec', '<Plug>TxtfmtBckTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ec', 'b', 1)<CR>")
call s:Def_map2('n', ']tec', '<Plug>TxtfmtFwdTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ec', 'f', 1)<CR>")
call s:Def_map2('n', '[tek', '<Plug>TxtfmtBckTillBgcEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ek', 'b', 1)<CR>")
call s:Def_map2('n', ']tek', '<Plug>TxtfmtFwdTillBgcEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ek', 'f', 1)<CR>")
call s:Def_map2('n', '[tea', '<Plug>TxtfmtBckTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ea', 'b', 1)<CR>")
call s:Def_map2('n', ']tea', '<Plug>TxtfmtFwdTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('n', 'ea', 'f', 1)<CR>")
" >>>
" visual mode jump 'till' token mappings <<<
call s:Def_map2('v', '[tbf', '<Plug>TxtfmtBckTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bf', 'b', 1)<CR>")
call s:Def_map2('v', ']tbf', '<Plug>TxtfmtFwdTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bf', 'f', 1)<CR>")
call s:Def_map2('v', '[tbc', '<Plug>TxtfmtBckTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bc', 'b', 1)<CR>")
call s:Def_map2('v', ']tbc', '<Plug>TxtfmtFwdTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bc', 'f', 1)<CR>")
call s:Def_map2('v', '[tbk', '<Plug>TxtfmtBckTillBgcBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bk', 'b', 1)<CR>")
call s:Def_map2('v', ']tbk', '<Plug>TxtfmtFwdTillBgcBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'bk', 'f', 1)<CR>")
call s:Def_map2('v', '[tba', '<Plug>TxtfmtBckTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'ba', 'b', 1)<CR>")
call s:Def_map2('v', ']tba', '<Plug>TxtfmtFwdTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('v', 'ba', 'f', 1)<CR>")
call s:Def_map2('v', '[tf' , '<Plug>TxtfmtBckTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'f' , 'b', 1)<CR>")
call s:Def_map2('v', ']tf' , '<Plug>TxtfmtFwdTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'f' , 'f', 1)<CR>")
call s:Def_map2('v', '[tc' , '<Plug>TxtfmtBckTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'c' , 'b', 1)<CR>")
call s:Def_map2('v', ']tc' , '<Plug>TxtfmtFwdTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'c' , 'f', 1)<CR>")
call s:Def_map2('v', '[tk' , '<Plug>TxtfmtBckTillBgcTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'k' , 'b', 1)<CR>")
call s:Def_map2('v', ']tk' , '<Plug>TxtfmtFwdTillBgcTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'k' , 'f', 1)<CR>")
call s:Def_map2('v', '[ta' , '<Plug>TxtfmtBckTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'a' , 'b', 1)<CR>")
call s:Def_map2('v', ']ta' , '<Plug>TxtfmtFwdTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('v', 'a' , 'f', 1)<CR>")
call s:Def_map2('v', '[tef', '<Plug>TxtfmtBckTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ef', 'b', 1)<CR>")
call s:Def_map2('v', ']tef', '<Plug>TxtfmtFwdTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ef', 'f', 1)<CR>")
call s:Def_map2('v', '[tec', '<Plug>TxtfmtBckTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ec', 'b', 1)<CR>")
call s:Def_map2('v', ']tec', '<Plug>TxtfmtFwdTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ec', 'f', 1)<CR>")
call s:Def_map2('v', '[tek', '<Plug>TxtfmtBckTillBgcEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ek', 'b', 1)<CR>")
call s:Def_map2('v', ']tek', '<Plug>TxtfmtFwdTillBgcEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ek', 'f', 1)<CR>")
call s:Def_map2('v', '[tea', '<Plug>TxtfmtBckTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ea', 'b', 1)<CR>")
call s:Def_map2('v', ']tea', '<Plug>TxtfmtFwdTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('v', 'ea', 'f', 1)<CR>")
" >>>
" operator-pending mode jump 'till' token mappings <<<
" Note: 'v' can be used with these to toggle inclusive/exclusive
call s:Def_map2('o', '[tbf', '<Plug>TxtfmtBckTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bf', 'b', 1)<CR>")
call s:Def_map2('o', ']tbf', '<Plug>TxtfmtFwdTillFmtBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bf', 'f', 1)<CR>")
call s:Def_map2('o', '[tbc', '<Plug>TxtfmtBckTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bc', 'b', 1)<CR>")
call s:Def_map2('o', ']tbc', '<Plug>TxtfmtFwdTillClrBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bc', 'f', 1)<CR>")
call s:Def_map2('o', '[tbk', '<Plug>TxtfmtBckTillBgcBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bk', 'b', 1)<CR>")
call s:Def_map2('o', ']tbk', '<Plug>TxtfmtFwdTillBgcBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'bk', 'f', 1)<CR>")
call s:Def_map2('o', '[tba', '<Plug>TxtfmtBckTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'ba', 'b', 1)<CR>")
call s:Def_map2('o', ']tba', '<Plug>TxtfmtFwdTillAnyBegTok', ":<C-U>call <SID>Jump_to_tok('o', 'ba', 'f', 1)<CR>")
call s:Def_map2('o', '[tf' , '<Plug>TxtfmtBckTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'f' , 'b', 1)<CR>")
call s:Def_map2('o', ']tf' , '<Plug>TxtfmtFwdTillFmtTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'f' , 'f', 1)<CR>")
call s:Def_map2('o', '[tc' , '<Plug>TxtfmtBckTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'c' , 'b', 1)<CR>")
call s:Def_map2('o', ']tc' , '<Plug>TxtfmtFwdTillClrTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'c' , 'f', 1)<CR>")
call s:Def_map2('o', '[tk' , '<Plug>TxtfmtBckTillBgcTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'k' , 'b', 1)<CR>")
call s:Def_map2('o', ']tk' , '<Plug>TxtfmtFwdTillBgcTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'k' , 'f', 1)<CR>")
call s:Def_map2('o', '[ta' , '<Plug>TxtfmtBckTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'a' , 'b', 1)<CR>")
call s:Def_map2('o', ']ta' , '<Plug>TxtfmtFwdTillAnyTok'   , ":<C-U>call <SID>Jump_to_tok('o', 'a' , 'f', 1)<CR>")
call s:Def_map2('o', '[tef', '<Plug>TxtfmtBckTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ef', 'b', 1)<CR>")
call s:Def_map2('o', ']tef', '<Plug>TxtfmtFwdTillFmtEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ef', 'f', 1)<CR>")
call s:Def_map2('o', '[tec', '<Plug>TxtfmtBckTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ec', 'b', 1)<CR>")
call s:Def_map2('o', ']tec', '<Plug>TxtfmtFwdTillClrEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ec', 'f', 1)<CR>")
call s:Def_map2('o', '[tek', '<Plug>TxtfmtBckTillBgcEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ek', 'b', 1)<CR>")
call s:Def_map2('o', ']tek', '<Plug>TxtfmtFwdTillBgcEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ek', 'f', 1)<CR>")
call s:Def_map2('o', '[tea', '<Plug>TxtfmtBckTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ea', 'b', 1)<CR>")
call s:Def_map2('o', ']tea', '<Plug>TxtfmtFwdTillAnyEndTok', ":<C-U>call <SID>Jump_to_tok('o', 'ea', 'f', 1)<CR>")
" >>>
" normal mode insert token mappings <<<
" These mappings may be used from normal mode to insert special tokens.
" Note: The first set leaves cursor in insert mode, and is probably the most
" useful. The second set enters insert mode to do the insert and puts cursor
" at correct offset prior to returning to normal mode. Works just like
" inserting the token, then hitting <Esc>.
" TODO - This one is redundant to the \vi one - use the latter instead for
" notational consistency?
call s:Def_map2('n', '<C-\><C-\>', '<Plug>TxtfmtInsertTok_n',
			\":<C-U>call <SID>Insert_tokstr('', 'i', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
" Start in normal / End in insert
call s:Def_map2('n', '<LocalLeader>i', '<Plug>TxtfmtInsertTok_i',
			\":<C-U>call <SID>Insert_tokstr('', 'i', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>I', '<Plug>TxtfmtInsertTok_I',
			\":<C-U>call <SID>Insert_tokstr('', 'I', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>a', '<Plug>TxtfmtInsertTok_a',
			\":<C-U>call <SID>Insert_tokstr('', 'a', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>A', '<Plug>TxtfmtInsertTok_A',
			\":<C-U>call <SID>Insert_tokstr('', 'A', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>o', '<Plug>TxtfmtInsertTok_o',
			\":<C-U>call <SID>Insert_tokstr('', 'o', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>O', '<Plug>TxtfmtInsertTok_O',
			\":<C-U>call <SID>Insert_tokstr('', 'O', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>s', '<Plug>TxtfmtInsertTok_s',
			\":<C-U>call <SID>Insert_tokstr('', 's', 0, 0)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
" Start in normal / End in normal
call s:Def_map2('n', '<LocalLeader>vi', '<Plug>TxtfmtInsertTok_vi',
			\":<C-U>call <SID>Insert_tokstr('', 'i', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>vI', '<Plug>TxtfmtInsertTok_vI',
			\":<C-U>call <SID>Insert_tokstr('', 'I', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>va', '<Plug>TxtfmtInsertTok_va',
			\":<C-U>call <SID>Insert_tokstr('', 'a', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>vA', '<Plug>TxtfmtInsertTok_vA',
			\":<C-U>call <SID>Insert_tokstr('', 'A', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>vo', '<Plug>TxtfmtInsertTok_vo',
			\":<C-U>call <SID>Insert_tokstr('', 'o', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>vO', '<Plug>TxtfmtInsertTok_vO',
			\":<C-U>call <SID>Insert_tokstr('', 'O', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
call s:Def_map2('n', '<LocalLeader>vs', '<Plug>TxtfmtInsertTok_vs',
			\":<C-U>call <SID>Insert_tokstr('', 's', 0, 1)<CR>"
			\.":call <SID>Adjust_cursor()<CR>")
" >>>
" insert mode insert token mappings <<<
" NOTE: Default is to use something that wouldn't be typed as text for the
" insert mode map. User may wish to remap this one to a Function key or
" something else entirely. I find <C-\><C-\> very easy to type...
call s:Def_map2('i', '<C-\><C-\>', '<Plug>TxtfmtInsertTok_i',
			\"<C-R>=<SID>Insert_tokstr('', 'i', 0, 0)<CR>"
			\."<C-R>=<SID>Adjust_cursor()<CR>")
" >>>
" auto-maps <<<
" FIXME: Should probably use vim-repeat to make highlight-altering commands
" repeatable whenever possible. The problem is that this will require
" parameterizing somehow, since it would be pointless to repeat something like
" \h if the highlighting-spec weren't somehow embodied in the repeat. Also note
" that it would be nice to be able to repeat the highlight-changing operators,
" but I don't think vim-repeat supports this.
" Possible Solution: Allow highlight spec to be specified explicitly, not
" accepted at prompt from user, and have this mechanism used in the call to
" repeat#set(). Could be implemented as optional arg to functions like
" Highlight_visual, or as wrapper function. Note that this doesn't really work
" for operators; for the operator scenario, we might allow highlight spec itself
" to be augmented with an operator motion that would be supplied to Vim with
" feedkeys().
" visual mode mappings <<<
" Note: The following will work for either visual or select mode
call s:Def_map2('v', '<LocalLeader>h', '<Plug>TxtfmtVmapHighlight',
			\":<C-U>call <SID>Highlight_visual()<CR>")
call s:Def_map2('v', '<LocalLeader>d', '<Plug>TxtfmtVmapDelete',
			\":<C-U>call <SID>Delete_visual()<CR>")
" >>>
" operator mappings <<<
call s:Def_map2('n', '<LocalLeader>h', '<Plug>TxtfmtOperatorHighlight',
			\":set opfunc=<SID>Highlight_operator<CR>g@")
call s:Def_map2('n', '<LocalLeader>d', '<Plug>TxtfmtOperatorDelete',
			\":set opfunc=<SID>Delete_operator<CR>g@")
" >>>
" >>>
" shift/indent maps <<<
" Note: No special shift logic if leading indent is highlighted like text.
if b:txtfmt_cfg_leadingindent != 'none'
" normal mode shift mappings <<<
call s:Def_map2('n', '<lt><lt>', '<Plug>TxtfmtShiftLeft',
			\":<C-U>call <SID>Lineshift('n', 1)"
			\.(s:have_repeat
			\? '<Bar>silent! call repeat#set("\<lt>Plug>TxtfmtShiftLeft")'
			\: '') . '<CR>')
call s:Def_map2('n', '>>', '<Plug>TxtfmtShiftRight',
			\":<C-U>call <SID>Lineshift('n', 0)"
			\.(s:have_repeat
			\? '<Bar>silent! call repeat#set("\<lt>Plug>TxtfmtShiftRight")'
			\: '') . '<CR>')
" >>>
" visual mode shift mappings <<<
" FIXME: Decide whether to define a separate Vmap-specific plug map.
call s:Def_map2('v', '<lt>', '<Plug>TxtfmtOperatorShiftLeft',
			\":<C-U>call <SID>Lineshift('V', 1)"
			\.(s:have_repeat
			\? '<Bar>silent! call repeat#set("\<lt>Plug>TxtfmtOperatorShiftLeft")'
			\: '') . '<CR>')
call s:Def_map2('v', '>', '<Plug>TxtfmtOperatorShiftRight',
			\":<C-U>call <SID>Lineshift('V', 0)"
			\.(s:have_repeat
			\? '<Bar>silent! call repeat#set("\<lt>Plug>TxtfmtOperatorShiftRight")'
			\: '') . '<CR>')
" >>>
" shift operator mappings <<<
" TODO: Any way to make this work with vim-repeat's dot operator?
call s:Def_map2('n', '<lt>', '<Plug>TxtfmtOperatorShiftLeft',
			\":set opfunc=<SID>Shift_left_operator<CR>g@")
call s:Def_map2('n', '>', '<Plug>TxtfmtOperatorShiftRight',
			\":set opfunc=<SID>Shift_right_operator<CR>g@")
" >>>
" insert mode indent/dedent mappings <<<
call s:Def_map2('i', '<C-T>', '<Plug>TxtfmtIndent',
			\"<Esc>:<C-U>call <SID>Indent(0)"
			\.(s:have_repeat
			\? '<Bar>silent! call repeat#set("\<lt>Esc>\<lt>Plug>(TxtfmtIndent)")'
			\: '') . '<CR>')
call s:Def_map2('i', '<C-D>', '<Plug>TxtfmtDedent',
			\"<Esc>:<C-U>call <SID>Indent(1)"
			\.(s:have_repeat
			\? '<Bar>silent! call repeat#set("\<lt>Esc>\<lt>Plug>(TxtfmtDedent)")'
			\: '') . '<CR>')

" Kludge to allow vim-repeat to work with insert-mode <C-T> and <C-D>.
" Background: Builtin <C-T> and <C-D> work with builtin `.', so our overrides
" should as well. But there are several complications:
" 1. Our overrides are insert-mode only, yet the dot operator can be executed
" only from normal mode.
" 2. Because of the way vim-repeat uses feedkeys(), dot after <C-o> works
" slightly differently from normal dot: in particular, with <C-o>, we'll already
" be back in insert mode before the fed keys are processed.
" Solution: Create normal mode maps that trigger the insert mode maps, and
" install the former with repeat#set() after execution of the latter.
if s:have_repeat
	call s:Def_map2('n', '', '<Plug>(TxtfmtIndent)', 'i<Plug>TxtfmtIndent<Esc>', 0)
	call s:Def_map2('n', '', '<Plug>(TxtfmtDedent)', 'i<Plug>TxtfmtDedent<Esc>', 0)
endif
" >>>
endif
" >>>
" normal mode get token info mapping <<<
call s:Def_map2('n', '<LocalLeader>ga', '<Plug>TxtfmtGetTokInfo',
			\":<C-U>echo <SID>GetTokInfo()<CR>")
" >>>
" NOTES <<<
" -enterinsert default is 'i'
" -mode default is 'ni'
" -<C-0> can't be used in insert-mode mapping for some reason...
" >>>
" TODO <<<
" -Add commands/functions for detecting and altering the range of character
"  codes used for txtfmt tokens.
" >>>
" >>>
" Restore compatibility options <<<
" Restore compatibility options to what they were
let &cpo = s:save_cpo
" >>>
" The stuff below may be useful one day, but until then, don't waste time
" compiling...
finish
" Code Graveyard <<<
" Function: s:At_buf_end() <<<
" TODO: !!! UNUSED - REMOVE !!!
fu! s:At_buf_end()
	return !!search('\%#.\?\%$', 'ncW')
endfu
" >>>
" Function: s:Getpos()
" Workaround for Vim Bug with getpos()
" TODO: I've removed use of this, so understand what the bug was?
fu! s:Getpos(m)
	return [line(a:m), col(a:m)]
endfu
" >>>
" >>>
	" vim: sw=4 ts=4 tw=80 foldmethod=marker foldmarker=<<<,>>> :
