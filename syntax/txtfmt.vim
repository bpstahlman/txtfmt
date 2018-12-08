" Txtfmt: Set of Vim plugins (syntax, ftplugin, plugin) for creating and
" displaying formatted text with Vim.
" File: This is the txtfmt syntax file
" Creation:	2004 Nov 06
" Last Change: 2018 Nov 23
" Maintainer:	Brett Pershing Stahlman <brettstahlman@comcast.net>
" License:	This file is placed in the public domain.
" Let the common code know whether this is syntax file or ftplugin

let s:script_name = 'syntax'
" Constants <<<
" >>>
" Common Configuration <<<
" Note: No point in having the modeline and/or global options processed by
" both the syntax and ftplugin files.
" IMPORTANT: Everything inside the "Common Configuration" fold should be
" identical between the syntax and ftplugin files. Keep in sync as changes are
" made...
if !exists('b:txtfmt_did_common_config')
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
" Config <<<
" Needed only for syntax file
" Note: Performed after the Common Configuration, which sets the 'escape'
" option, needed when defining syntax
" Function: s:Do_config() <<<
" Purpose: Do configuration required only for syntax file.
" Assumption: Common config has already been performed, so that needed options
" have been set.
fu! s:Do_config()
	" Nothing to do here now that skip def is defined in Define_syntax
	" TODO...
endfu
" >>>
call s:Do_config()
" >>>
" Function: s:Winrestcmd() <<<
" Purpose: Works just like Vim's winrestcmd, which was added in Vim 6.3,
" (and therefore, is not something I want to rely upon). Returns a
" string that can be :execute'd to restore the current window layout,
" assuming exactly the same set of windows exist when the string is
" exe'd.
" Note: winrestcmd() format looks like this:
" 1resize <height1>|vert 1resize <width1>| ... Nresize <heightN>|vert Nresize <widthN>|
" Note how the final element is terminated by a vertical bar.
" Testing: Verified for non-trivial window layout that winrestcmd() and
" this function return the same string.
fu! s:Winrestcmd()
	let i = 1
	let N = winnr('$')
	let cmd = ''
	while i <= N
		let cmd = cmd . i . 'resize ' . winheight(i) . '|vert ' . i . 'resize ' . winwidth(i) . '|'
		let i = i + 1
	endwhile
	return cmd
endfu
" >>>
" Function: s:Create_scratch_buffer() <<<
" Purpose: Create an empty scratch buffer in the current window, hiding
" the current buffer.
" Assumption: The hidden buffer will be restored by a call to
" s:Cleanup_scratch_buffer() later.
" Vim Idiosyncrasy: Special care must be taken when the current window
" contains an empty, [No Name] buffer; in that case, :hide enew will, by
" default, reuse that buffer for the newly created one (discarding any
" existing buf-local variables). This is problematic, since we need to
" return to the original buffer when we're finished with the scratch
" buffer. Note that in some cases, Txtfmt options may already have been
" stored to buf-local variables in the empty, [No Name] buffer before
" this function is called.
" Solution: Bram suggested adding a blank line to the original buffer to
" ensure it isn't reused. The blank line can be removed in the
" s:Cleanup_scratch_buffer() function.
" Note: Before this function was added, the scratch buffer was created
" in a new window. The problem with that approach was that it fails when
" there's no room to create another window.
fu! s:Create_scratch_buffer()
	" See whether the current buffer is an empty, unnamed buffer that
	" will be discarded by the :hide enew below.
	if line('$') == 1 && col('$') == 1 && bufname('%') == ''
		" Current buffer is an empty, unnamed buffer
		" To prevent its being discarded by :hide enew, add a blank
		" line, which we'll remove in the associated cleanup function
		" Make sure the buffer is modifiable, taking care to save and restore
		" current setting.
		let modifiable_save = &l:modifiable
		setlocal modifiable
		call append(1, '')
		let &l:modifiable = modifiable_save
		let s:Added_blank_line_to_empty_buffer = 1
	endif
	" Create the scratch buffer
	hide enew
	set buftype=nofile
	set bufhidden=wipe
	set noswapfile
	" The following setlocal is necessary to prevent E21 in the event that
	" 'nomodifiable' is set globally.
	setlocal modifiable
endfu
" >>>
" Function: s:Cleanup_scratch_buffer() <<<
" Purpose: Wipe out the scratch buffer in the current window, restoring
" the buffer it supplanted.
" Assumption: s:Create_scratch_buffer() was called to create the scratch
" buffer in the current window.
fu! s:Cleanup_scratch_buffer()
	" Return to the buffer that was current when the associated create
	" function was called
	" Note: The scratch buffer 'bufhidden' option will ensure that it's
	" bwipe'd
	buffer #
	if exists('s:Added_blank_line_to_empty_buffer')
		unlet s:Added_blank_line_to_empty_buffer
		" Get rid of the blank line we added in the associated create
		" function.
		" Note: Make sure the buffer is modifiable, taking care to save and
		" restore current setting.
		let modifiable_save = &l:modifiable
		setlocal modifiable
		undo
		let &l:modifiable = modifiable_save
	endif
endfu
" >>>
" Function: s:Is_match_offset_char_based() <<<
" Purpose: Return nonzero if and only if the this version of Vim treats match
" offsets as character offsets.
" Assumption: Current encoding is multi-byte
fu! s:Is_match_offset_char_based()
	let s = "AB" . nr2char(0x100) . 'C'
	" Set lazyredraw to ensure user never sees the buffer we create
	let lazyredraw_save = &lazyredraw
	set lazyredraw
	" Create a scratch buffer in the current window
	call s:Create_scratch_buffer()
	" Put the test string at the head of the new scratch buffer
	call setline(1, s)
	" Create syntax region that will include the last character on the line if
	" and only if this Vim treats match offsets as char offsets
	syn match Tf_Test /AB/me=e+2,he=e+2
	" Is the last char in the Tf_Test syntax group?
	if synIDattr(synID(line("."), col("$") - 1, 1), "name") == 'Tf_Test'
		let off_is_char = 1
	else
		let off_is_char = 0
	endif
	" Clean up the scratch buffer, returning to the previous buffer
	call s:Cleanup_scratch_buffer()
	" Restore old lazyredraw setting
	if !lazyredraw_save
		set nolazyredraw
	endif
	" Return true if and only if offsets are char-based
	return off_is_char
endfu
" >>>
" Function: s:Get_bytes_per_token() <<<
" Purpose: Return the number of bytes used to encode the first Txtfmt token,
" warning user if this number is different from the number of bytes used to
" encode the last.
" Assumption: # of bytes per token will never decrease as character codes
" increase
fu! s:Get_bytes_per_token()
	let num_bytes = strlen(nr2char(b:txtfmt_clr_first_tok))
	" Make sure first and last token comprise the same number of bytes,
	" and warn user if not...
	if num_bytes != strlen(nr2char(b:txtfmt_fmt_last_tok))
		" Note: Txtfmt highlighting will probably be incorrect, but there's
		" not much we can do about it other than warn user...
		echohl WarningMsg
		echomsg "Warning! The 'tokrange' setting you have chosen may not work correctly"
		echomsg "because not all tokens within the range are encoded with the same number"
		echomsg "of bytes. If fmt/clr regions do not display correctly, you should either"
		echomsg "choose a different 'tokrange', or apply the multi-byte patch included with"
		echomsg "the plugin."
		echohl Comment
		echomsg "    :help txtfmt-choosing-token-range"
		echohl MoreMsg
		echomsg "Hit any key to continue..."
		echohl None
		call getchar()
	endif
	" Return # of bytes used by first token
	return num_bytes
endfu
" >>>
" Function: s:Get_smart_leading_indent_patt <<<
fu! s:Get_smart_leading_indent_patt()
	" Cache a few pertinent options for convenience.
	let sw = shiftwidth()
	let ts = &ts
	let conceal = b:txtfmt_cfg_conceal
	if !&expandtab
		if sw == ts
			" 1-to-1 correspondence between tabstop and indent
			let re = '\t\+'
		else
			if ts % sw == 0 || sw % ts == 0
				if sw < ts
					" Note: A tab represents whole number of indents.
					" TODO: Do we need to guard against zero-length match?
					let re = '\t*\%( \{' . sw . '}\)\{,' . (ts / sw - 1) . '}'
				else
					" Note: A shift represents a whole number of tabs.
					let re = '\%(\t\{' . (sw / ts) . '}\)\+'
				endif
			else
				" Neither ts nor sw multiple of the other
				" Note: The following pattern examples are 'very magic', with
				" the following symbol meanings:
				" t=TAB, s=SPC, nt=# of tabs
				" N=# of pattern terms before # of spaces repeats (i.e., the
				" length of the 'inner loop' discussed below)
				" ===============
				" CASE 1: ts > sw
				" ===============
				" sw=3 ts=8
				" t{0}s{3}|t{0}s{6}|t{1}s{1}|t{1}s{4}|t{1}s{7}|t{2}s{2}|t{2}s{5}|t{3}s{0}|
				" t{3}s{3}|t{3}s{6}|t{4}s{1}|t{4}s{4}|t{4}s{7}|t{5}s{2}|t{5}s{5}|t{6}s{0}|
				" sw=2 ts=9
				" t{0}s{2}|t{0}s{4}|t{0}s{6}|t{0}s{8}|t{1}s{1}|t{1}s{3}|t{1}s{5}|t{1}s{7}|t{2}s{0}|
				" t{2}s{2}|t{2}s{4}|t{2}s{6}|t{2}s{8}|t{3}s{1}|t{3}s{3}|t{3}s{5}|t{3}s{7}|t{4}s{0}|
				" t{4}s{2}|t{4}s{4}|t{4}s{6}|t{4}s{8}|t{5}s{1}|t{5}s{3}|t{5}s{5}|t{5}s{7}|t{6}s{0}|
				" sw=4 ts=6
				" t{0}s{4}|t{1}s{2}|t{2}s{0}|
				" t{2}s{4}|t{3}s{2}|t{4}s{0}|
				" Pattern: Works conceptually like nested loop, with inner
				" loop (over j) generating N pattern terms of the following
				" form...
				"     TAB{nt[j] + i * MOD}SPC{ns[j]}
				" ...and outer loop (over i) never terminating. In practice,
				" we'll factor the (i * MOD) out of the tab repeat count so
				" that a regex repeat can be used in lieu of infinite loop.
				" LCM = lcm(sw, ts) = nt[N-1] * ts
				" MOD = LCM / ts = nt[N-1]
				" N = LCM / sw = nt[N-1] * ts / sw
				" Implementation Note: Since Vim has no LCM function, I won't
				" attempt to calculate MOD and N analytically, but will embed
				" placeholders in the generated pattern. When ns[j] becomes 0,
				" I will effectively have determined LCM (# of tabs on final
				" iteration's term), and can break out of the loop and perform
				" the appropriate substitutions.
				" ===============
				" CASE 2: ts < sw
				" ===============
				" sw=8 ts=3
				" t{2} s{2}|t{5} s{1}|t{8} s{0}|
				" t{10}s{2}|t{13}s{1}|t{16}s{0}|
				" t{18}s{2}|...
				" sw=9 ts=2
				" t{4} s{1}|t{9} s{0}|
				" t{13}s{1}|t{18}s{0}|
				" t{22}s{1}|...
				" sw=7 ts=5
				" t{1} s{2}|t{2}s{4}|t{4} s{1}|t{5} s{3}|t{7} s{0}|
				" t{8} s{2}|t{9}s{4}|t{11}s{1}|t{12}s{3}|t{14}s{0}|
				" t{15}s{2}|...
				" sw=6 ts=4
				" t{1}s{2}|t{3}s{0}
				" t{4}s{2}|t{6}s{0}

				" Loop Logic: Increase by single indent level each iteration,
				" calculating for each level the # of tabs and spaces. Break
				" out of loop when # of spaces reaches modulo (for some
				" definition of the term 'modulo' - details to follow).
				" Combining Cases: Generating term-specific # of tabs/spaces
				" works the same for both cases (i.e., ts < sw and ts > sw);
				" the 2 cases' formulas for LCM, MOD and N are the mirror
				" image of each other (i.e., sw and ts swapped). But since we
				" use only MOD, whose value is equal to the # of loop
				" iterations (which also happens to be the number of tabs in
				" final term for both cases), the logic for both cases can be
				" nearly identical.
				let [es, nt, ns, p, MOD] = [sw, sw / ts, sw % ts, '', 0]
				" Caveat: Generate the pattern in reverse: i.e., earlier
				" (shorter) terms must come later to prevent a shorter match
				" taking precedence over a longer. But there's a complication:
				" read on...
				" Loop Termination Note: Conceptually, there's nothing new
				" being generated after # of spaces (ns) first becomes 0 (end
				" of the first row in the examples above). Unfortunately,
				" because of the way regex engines work, we must include
				" additional terms for both sw < ts and sw > ts cases. The
				" difficulty stems from the fact that Vim's regex engine (like
				" many) prefers an earlier alternative, even when it's shorter
				" than a later one. Although we take care to order the
				" pattern terms descending with respect to effective # of
				" spaces, there is a peculiarity w.r.t. the final s{0} term in
				" the earlier examples: namely, because it does not require
				" the presence of spaces after the tab(s), it will match
				" instead of a *longer* match with a greater # of tabs, or
				" even the same # of tabs but more spaces. The problem is that
				" there's no simple way to instruct the regex engine to prefer
				" a match with a higher MOD value when its MOD==0 term comes
				" later in the alternation. Including nearly an extra row of
				" terms in the alternation (i.e., 2 * MOD - 1 terms) fixes
				" this issue at the expense of nearly doubling the # of
				" alternatives.
				while 1
					let p = '\t\{' . nt . '}\%(\t\{MOD}\)* \{' . ns . '}' . p
					if ns == 0
						let MOD = nt
					endif
					let es += sw
					let [nt, ns] = [es / ts, es % ts]
					" Have we reached special repeat point? (See earlier note
					" on loop termination.)
					if MOD && ns == 0
						break
					endif
					if !empty(p)
						let p = '\|' . p
					endif
				endwhile
				let p = '\%(' . p . '\)'
				let re = substitute(p, 'MOD', MOD, 'g')
			endif
		endif
	else
		" 'expandtab'
		" Multiple of 'sw' spaces
		" Design Decision: Ignore tabs, which shouldn't be used for leading
		" indent when 'expandtab' is set.
		let re = '\%( \{' . sw . '}\)\+'
	endif
	return re
endfu
" >>>

" Function: s:Adjust_capture_numbers <<<
" tgt     The target pattern to be adjusted
" ctx     The pattern that precedes tgt, whose open parens entail a shift of
"         the capture numbers in the tgt pattern
" [bias]  Optional count of open parens *prior to* ctx (i.e., for captures
"         surrounding both tgt and ctx
"         Assumption: A capture begun in tgt or ctx also ends in it.
fu! s:Adjust_capture_numbers(tgt, ctx, ...)
	let bias = a:0 ? a:1 : 0
	" Prefix regex used to ensure the bslash in \( or \N is unescaped.
	let re_prefix = '\%(\%(^\|[^\\]\)\%(\\\\\)*\)\\\zs'
	" Count capture groups in context pattern.
	" TODO: Does Vim have a function that could do this?
	let [i, len] = [0, len(a:ctx)]
	" Note: Sought pattern is length 2: hence the len - 1
	while i >= 0 && i < len - 1
		let i = match(a:ctx, re_prefix . '(', i)
		if i >= 0
			let bias += 1
			let i += 2
		endif
	endwhile
	" Shift all \\N in tgt pattern by bias
	" Design Decision: Only \1 through \9 supported by Vim, so N > 9- bias
	" would cause a problem, but that's not going to happen because bias is
	" small and determined entirely by configuration, and in any case, it
	" would be caught in testing.
	" TODO: Remove scaffolding.
	let tgt = substitute(a:tgt, re_prefix . '\(\d\)\d\@!',
		\ '\=submatch(1) + bias', 'g')
	"echomsg printf("bias=%d tgt=%s ctx=%s\n", bias, a:tgt, a:ctx)
	"echomsg tgt
	return tgt
endfu
" >>>

" Function: s:Hide_leading_indent_maybe <<<
fu! s:Hide_leading_indent_maybe()
	" Cache color uniqueness index.
	let cui = b:txtfmt_color_uniq_idx
	" TODO: Disable (set to 'none') in 'noconceal' case.
	" Initialize the leading indent regex to an option-specific template that
	" may be adjusted later.
	if b:txtfmt_cfg_leadingindent == 'none'
		" Don't create any regions for leading indent.
		return
	elseif b:txtfmt_cfg_leadingindent == 'space'
		let re_li = ' \+'
	elseif b:txtfmt_cfg_leadingindent == 'tab'
		let re_li = '\t\+'
	elseif b:txtfmt_cfg_leadingindent == 'white'
		let re_li = '\s\+'
	elseif b:txtfmt_cfg_leadingindent == 'smart'
		" Note: Generate complex pattern that depends upon effective 'sw' and
		" 'ts' settings.
		let re_li = s:Get_smart_leading_indent_patt()
	elseif b:txtfmt_cfg_leadingindent == 'width'

	endif
	" Cache regex for a single, unescaped token of any type.
	let re_tok = b:txtfmt_re_any_tok
	" Cache regex for a single (possibly escape/escapee) token
	let re_tok_atom = '[' . b:txtfmt_re_any_tok_atom . ']'
	" Modify the templates, taking 'conceal' into account.
	if b:txtfmt_cfg_conceal
		" Accept any number (including 0) of tokens before any whitespace that
		" would have been matched by the pattern.
		" Note: Intentionally deferring check for escaped tokens.
		" Rationale: Embedding in re_li could exceed capture limit; defer to a
		" look-behind assertion.
		" Design Decision Needed: Consider highlighting useless end tokens
		" within regions specially: (e.g., end clr in a fmt only region).
		" Design Consideration: They're harmless, and are cleaned up
		" automatically by auto-map operations, so we shouldn't mess up the
		" leading indent width simply to call attention to them. However, if
		" users's 'cocu' setting causes tokens to be visible, it might make
		" sense to show them somehow.
		" Hmm... Actually, they'll already be visible in that case; would we
		" want to highlight them red or something? Doing so might give the
		" mistaken impression that they're red fg/bg tokens.
		" Problem: Useless end tokens are not recognized as tokens: as it is
		" now, therefore, they already mess up the indent width. To change
		" this, I could either have 2 distinct leading indent groups (one for
		" tokens one for true whitespace), or I could keep a single leading
		" indent group, but have a low-priority end token group that is
		" contained by leading indent.
		" Explanation: An end token that ends a region (i.e., isn't useless)
		" is of type Tf_tok or Tf{cui}_tok_{bg_idx}. If the new tok group
		" (say, Tf_invalid or somesuch) is lower priority than, and cannot be
		" contained by, these groups, the definitions should be trivial. The
		" question is, how would we want these highlighted? I would say that
		" in the 'conceal' case, we'd want them concealed unless 'cocu'
		" prevents it, in which case, they'd be shown normally. Hmm... This is
		" the way it is for normal (useful) tokens. In the 'noconceal' case,
		" we probably just want the useless tokens to be visible, but what
		" about the indent width?
		let re_li = substitute(re_li, '\%( \|\\[st]\)',
			\ '\\%(' . re_tok_atom . '*&\\)', 'g')
	else " noconceal
		" Assumption: We've already returned in li='none' case.
		if b:txtfmt_cfg_leadingindent =~ 'white\|space'
			" Match token wherever a SPC would match.
			let re_li = substitute(re_li, '\( \|\\s\)',
				\ '\\%(' . re_tok_atom . '\\|\1\\)', 'g')
		else " tab|smart
			if b:txtfmt_cfg_leadingindent == 'smart'
				" Match token anywhere a SPC would match.
				" Note: Shouldn't be any literal spaces in li=tab pattern, so
				" the containing guard isn't strictly necessary.
				let re_li = substitute(re_li, ' ',
					\'\\%(' . re_tok_atom . '\\| \\)', 'g')
			endif
			" Allow any amount of a tabstop (up to and including full width)
			" to be tokens.
			" Regex: (TOK){,<ts-1>}TAB|(TOK){<ts>}
			let re_li = substitute(re_li, '\\t',
				\ '\\%(' . re_tok_atom . '\\{,' . (&ts - 1) . '}\\t'
				\.'\\|' . re_tok_atom . '\\{' . &ts . '}\\)', 'g')
		endif
	endif
	" Also match blank lines (whitespace and tokens only)
	" Note: escape-escapee test done elsewhere
	let re_li = '^\%(\%(\s\|' . re_tok_atom . '\)\+$\)\|^' . re_li
	if b:txtfmt_cfg_escape == 'self'
		" Append look-behind assertion to ensure none of the tokens matched in
		" leading indent were escape-escapee pairs. (Note that if they were,
		" backtracking could still permit shorter leading indent match.)
		" Note: We can ignore esc=bslash case because backslashes can't be
		" matched by the existing re_li pattern.
		" At this point, re_li contains backrefs, so we'll need to adjust the
		" capture numbers in the zero-width assertion we're about to append.
		" Optimization Note: The shift amount should always be 1, so we could
		" probably just hardcode \2 in lieu of \1, but using
		" Adjust_capture_numbers is a bit more future-proof.
		let re_li .= s:Adjust_capture_numbers(
			\ '\%(^\%(\%(\(' . re_tok_atom . '\)\1\)\@!.\)*\)\@<=',
			\ re_li)
	endif
	" Now create some patterns that recognize tokens/whitespace *within*
	" leading indent.
	" Note: escape-escapee test done elsewhere
	let re_tok_or_ws = '\s\|' . re_tok_atom
	" Now the tricky part: need to be able to match *any* portion of the
	" leading indent independently of the rest of it.
	" Rationale: Tokens can break leading indent into multiple segments.
	" Note: In the 'smart' and 'width' leading indent cases, we have to look
	" past the matched region to determine whether or not we're inside leading
	" indent; although we could avoid this for other li regimes, it's probably
	" not worth optimizing...
	" TODO: Although this is the way I want it (ws and tok orthogonal), would
	" still like to know why Tf_li_ws was allowed to take precedence over
	" Tf_li_tok on the second of back-to-back tokens, even though Tf_li_tok
	" is defined later, and hence, should take priority.
	let re_li_ws = '\s\+\%(' . re_li . '\)\@<='
	let re_li_tok = re_tok . '\ze\%(' . re_tok_or_ws . '\)*'
	" Caveat: Append leading indent lookbehind constraint separately,
	" adjusting its backreferences to account for captures in re_tok.
	let re_li_tok .= s:Adjust_capture_numbers('\%(' . re_li . '\)\@<=', re_li_tok)
	" Persist re_li on the buffer so that it's available to lineshift
	" functions.
	let b:txtfmt_re_leading_indent = re_li
	" Cache vars globally for debug only.
	" TODO: Remove debug assignments...
	let [g:re_li_ws, g:re_li_tok, g:re_li, g:re_tok_or_ws] =
		\ [re_li_ws, re_li_tok, re_li, re_tok_or_ws]

	" Create the non-token syntax group whose purpose is to hide all
	" highlighting in whatever is considered to be leading indent.
	exe 'syn match Tf_li_ws /' . re_li_ws . '/ contained'
		\ . ' containedin=@Tf'.cui.'_all'
	" Note: Ideally, Tf_li_tok would simply be containedin Tf_tok, but in the
	" 'conceal' case, Tf_tok is transparent, and transparent groups can't
	" contain other groups directly. Thus, in the 'conceal' case, Tf_li_tok is
	" contained in the region itself, whereas in the 'noconceal' case, it can
	" be contained directly by (non-transparent) Tf_tok.
	exe 'syn match Tf_li_tok /' . re_li_tok . '/ contained'
		\ . (b:txtfmt_cfg_conceal ? ' conceal' : '')
		\ . ' containedin=@Tf'.cui
		\ . (b:txtfmt_cfg_conceal ? '_all' : '_tok')
	if b:txtfmt_cfg_conceal
		" Make sure Tf_li_tok isn't linked to Tf_conceal. (Note that this
		" could happen if user changes 'conceal' in a modeline and re-:edits
		" the file, without restarting Vim or clearing syntax.)
		hi link Tf_li_tok NONE
	else
		" Hide tokens' foreground in leading indent since we can't conceal.
		hi link Tf_li_tok Tf_conceal
	endif
endfu
" >>>
" Achieve clr->bgc->sqc->fmt order required for highlight command.
" Assumption: a and b will always be different.
fu! s:Sort_rgn_types(a, b)
	if a == 'clr'
		return -1
	elseif a == 'fmt'
		return 1
	elseif a == 'bgc'
		return b == 'fmt' ? -1 : b == 'clr' ? 1 : -1
	elseif a == 'sqc'
		return b == 'fmt' ? -1 : b == 'clr' ? 1 : 1
endfu
" States: 0=empty, 1=expr, 2=literal
fu! s:Make_exe_builder()
	let o = {'st': 0, 'st_beg': 0, 's': ''}
	" Add list of deferred exprs and const separator.
	" Note: The resulting construct is effectively an expr, since the
	" separator will be interior if it is used at all.
	fu! o.add_list(ls, sep)
		call self.add(join(a:ls, " . '" . a:sep . "' . "))
	endfu
	" Prepare to append, taking current and next state into account.
	fu! o.prep(st, ...)
		if !self.st
			" Save initial state.
			let self.st_beg = a:st
		elseif self.st == 1
			if a:st == 1
				let self.s .= " . "
			elseif a:st == 2
				let self.s .= " . '"
			endif
		elseif self.st == 2
			if a:st == 1
				let self.s .= "' . "
			endif
		endif

		let self.st = a:0 ? a:1 : a:st
	endfu
	" Optional arg:
	"   literal
	fu! o.add(o, ...)
		if type(a:o) == 4
			" Object
			" Note: Only non-empty objects have any effect.
			if a:o.st
				call self.prep(a:o.st_beg, a:o.st)
				let self.s .= a:o.s
			endif
		else
			" expr or literal
			call self.prep(!a:0 || !a:1 ? 1 : 2)
			let self.s .= a:o
		endif
	endfu
	fu! o.get()
		return self.s
	endfu
	" Note: This doesn't change state in any way.
	fu! o.get_estr()
		" TODO: Might be able to handle empty as empty literal.
		if !self.st
			return "''"
		else
			" Handle beginning.
			let s = self.s
			if self.st_beg == 2
				let s = "'" . s
			endif
			" Handle end.
			if self.st == 2
				let s .= "'"
			endif
			return s
		endif
		" If we get here, no mods were necessary.
		return self.s
	endfu
	fu! o.reset()
		let [self.s, self.st, self.st_beg] = ['', 0, 0]
	endfu
	" Return object with encapsulated state.
	return o
endfu
" Function: s:Define_syntax() <<<
fu! s:Define_syntax()
	" Cache some useful vars <<<
	" Define convenience flags that indicate which colors are in effect
	let bgc_enabled = b:txtfmt_cfg_bgcolor && b:txtfmt_cfg_numbgcolors > 0
	let sqc_enabled = b:txtfmt_cfg_sqcolor && b:txtfmt_cfg_numsqcolors > 0
	let clr_enabled = b:txtfmt_cfg_numfgcolors > 0

	" cui (color uniqueness index) will contain a different index for each
	" color configuration (and will be empty string in the unlikely event that
	" all of numfg/bg/sqcolors are 0 - i.e., no colors used)
	" Note: This strategy is necessary because Vim's highlight groups are not
	" buffer-specific, but there is a buffer-specific version of txtfmtColor{}
	" Question from future Brett: Seriously!?!?!? Why??? Is there really a
	" sufficiently common use case to justify the added complexity??
	let cui = b:txtfmt_color_uniq_idx

	" Determine whether to use gui or cterm definitions.
	" Neovim Workaround: As part of a poorly-documented feature that provides
	" 24-bit 'true color' in terminals that support it, Neovim requires gui
	" definitions when $NVIM_TUI_ENABLE_TRUE_COLOR is set; highlighting in
	" neovim fails silently if this env var is set in terminals that don't
	" support true color. (Apparently, neovim does nothing to verify terminal
	" support.) At any rate, because of this feature, simply checking
	" has('gui_running') is no longer sufficient.
	" Caveat: Neovim's check uses an os_getenv() wrapper that treats only
	" empty string and missing altogether as false.
	" TODO: Is there any performance cost to just defining both gui and cterm
	" highlights? If not, doing so might be simpler and safer long-term.
	let use_gui_defs = has('gui_running') || has('nvim') &&
		\ exists('$NVIM_TUI_ENABLE_TRUE_COLOR') &&
		\ $NVIM_TUI_ENABLE_TRUE_COLOR != ''
	
	" Choose cterm or gui versions of color and format assignments.
	if use_gui_defs
		let eq_clr = ' guifg='
		let eq_bgc = ' guibg='
		let eq_sqc = ' guisp='
		let eq_fmt = ' gui='
	else
		let eq_clr = ' ctermfg='
		let eq_bgc = ' ctermbg='
		let eq_sqc = ' ctermsp='
		let eq_fmt = ' cterm='
	endif

	" Note: Originally, the skip patterns were assigned to region-specific
	" vars in the generated code; however, there was no need for this, as the
	" patterns were invariable, depending only on the txtfmt 'escape' option.
	" Actually, at one time, I believe the patterns were region-specific, but
	" the non-region-specific patterns are simple and efficient.
	if b:txtfmt_cfg_escape != 'none'
		if b:txtfmt_cfg_escape == 'bslash'
			let skip = ' skip=/\\./'
		else
			let skip = ' skip=/\(.\)\1/'
		endif
	else
		let skip = ''
	endif
	" >>>

	" Give ourselves a chance to make leading indent immune to highlighting
	" (as determined by 'leadingindent' option).
	" TODO: Can I move this out of this block?
	call s:Hide_leading_indent_maybe()

	" Token concealment <<<
	" Check for existence of 'conceal' patch (and desire on part of user to
	" use it)
	let conceal = ''
	let transparent = ''
	if b:txtfmt_cfg_conceal
		" Note: 'conceallevel' and 'concealcursor' are window-local
		setl conceallevel=3
		let &l:concealcursor = b:txtfmt_cfg_concealcursor
		let conceal = ' conceal'
		let transparent = ' transparent'
	endif
	" Initialize tok concealment cluster with top-level group.
	" Note: Currently, this cluster (to which bgc-specific groups may be added
	" later) is used only in 'noconceal' case: in 'conceal' case, it contains
	" only Tf_tok, which is currently used in lieu of the cluster.
	exe 'syn cluster Tf'.cui.'_tok contains=Tf_tok'
	" Note: Omit 'contained', since Tf_tok is permitted at top level. But in
	" the 'noconceal' case, use containedin=ALLBUT,<tok-cluster> to prevent a
	" pointless match of Tf_tok within one of the bgc-specific tok groups.
	exe 'syn match Tf_tok /'.b:txtfmt_re_any_tok.'/'
		\ . (b:txtfmt_cfg_conceal ? '' : ' containedin=ALLBUT,@Tf'.cui.'_tok')
		\ . conceal.transparent
	" Note: When 'conceal' is set, transparent and conceal attributes obviate
	" need to define highlighting for Tf_tok groups.
	if !b:txtfmt_cfg_conceal
		hi link Tf_tok Tf_conceal
		" Create bgc-specific tok concealment groups and associated
		" highlighting, adding the groups to the special Tf{cui}_tok
		" cluster used in containedin's ALLBUT clause.
		" Rationale: Prior to introduction of 'conceal' patch (subsequently
		" incorporated into Vim), users might have used Txtfmt tokens as word
		" separators; thus, they must appear as non-zero-width whitespace,
		" Design Decision: While it's true that this same reasoning could be
		" applied to stuff like underline and undercurl (which also can be
		" visible on whitespace), I intentionally do not create
		" format-specific token groups.
		" Rationale: 4 of the 6 format attributes (usrc) can highlight
		" whitespace, and we would need to create groups for *all*
		" permutations of these with *all* permutations of bg color! Given
		" that a "hole" in formatting tends not to be as visually arresting as
		" a hole in background color, and given that none of this even applies
		" outside the rarely used 'noconceal' case, the complexity and
		" performance hit is definitely *not* warranted.
		" SQUIGGLE_NOTE: This rationale above is even stronger now that
		" colored undercurl would need to be added to the mix.
		" Note: Use of 'cui' indices allows each buffer (potentially) to have
		" its own set of background colors; for performance reasons, however,
		" this power should be used sparingly, as it increases the total
		" number of regions.
		" Loop over active colors only (with the aid of index indirection array).
		let pi = 1
		while pi <= (b:txtfmt_cfg_bgcolor ? b:txtfmt_cfg_numbgcolors : 0)
			let i = b:txtfmt_cfg_bgcolor{pi}
			exe 'syn match Tf'.cui.'_tok_'.i.' /'.b:txtfmt_re_any_tok.'/ contained'.conceal
			exe 'hi Tf'.cui.'_tok_'.i.' '.eq_bgc.b:txtfmt_bgc{i}.eq_clr.b:txtfmt_bgc{i}
			" Note: Tf{cui}_tok name is fine since clusters/groups are in
			" distinct namespaces.
			exe 'syn cluster Tf'.cui.'_tok add=Tf'.cui.'_tok_'.i
			let pi = pi + 1
		endwhile
	endif
	" >>>
	" Create concealment highlight group <<<
	" Create a concealment highlight group, to which others can link.
	" The Ignore group is a preferred group, defined in distributed
	" syncolor.vim
	" IMPORTANT NOTE: Some of the distributed colorschemes DO NOT hide text in
	" the Ignore group. I disagree with this practice, and have posted to the
	" Vim list on the subject, but the situation is unlikely to change...
	" Fortunately, there is a workaround that always works for the GUI,and
	" sometimes works for a cterm.
	" Workaround: *Attempt* to define fg=bg. This will always work for the
	" GUI, and will work for a cterm if the colorscheme has defined ctermbg
	" for the Normal group. If the attempt fails, simply link to Ignore group,
	" which may or may not hide text.
	" Assumption: When user has both 'conceal' and 'noconceal' buffers open,
	" the Tf_conceal highlight is shared, but has no effect on the 'conceal'
	" buffers.
	if use_gui_defs
		hi Tf_conceal guifg=bg
	else
		let v:errmsg = ""
		silent! hi Tf_conceal ctermfg=bg
		if v:errmsg != ""
			" Link to Ignore and put suggestions in help file for users of
			" colorschemes that don't hide Ignore'd text.
			hi link Tf_conceal Ignore
		endif
	endif
	" >>>
	" Calculate single token match offset <<<
	" Note: This is required because of a Vim bug: as of Vim 7.1, syntax match
	" offsets are always treated as byte offsets, though the documentation
	" suggests offsets are char-based. There is a patch floating around,
	" however, which fixes this; also, Vim 7.2 *may* fix it; thus, it's not
	" safe to assume anything about whether the running Vim uses byte or char
	" offsets. If necessary, Is_match_offset_char_based will find out.
	" Note: Eventually, the if and first elseif can be combined, but for now,
	" I want to set b:txtfmt_dbg_syn_off as a debug var, in case any users
	" experience problems...
	if b:txtfmt_cfg_enc_class == '1'
		let tok_off = 1
		" Nonexistence of b:txtfmt_dbg_syn_off indicates
		" Is_match_offset_char_based wasn't run
		unlet! b:txtfmt_dbg_syn_off
	silent elseif s:Is_match_offset_char_based()
		let tok_off = 1
		let b:txtfmt_dbg_syn_off = 'char'
	else
		" Offsets are measured in bytes; hence, we need to determine how many
		" bytes per token
		let tok_off = s:Get_bytes_per_token()
		let b:txtfmt_dbg_syn_off = 'byte'
	endif
	" >>>
	" 'containedin' list (option dependent) <<<
	if b:txtfmt_cfg_nested
		" Ensure that txtfmt top-level item can be contained by a non-txtfmt
		" syntax group (e.g. C-language comment) but not another txtfmt group.
		let containedin_def = ' containedin=ALLBUT,@Tf'.cui.'_all'
		if b:txtfmt_cfg_escape != 'none'
			" Note: Need to use cluster for bgc-specific escape groups (which
			" are used in both 'conceal' and 'noconceal' cases).
			let containedin_def .= ',@Tf'.cui.'_esc,Tf_outer_esc'
		endif
		" Note: In the 'noconceal' case, cluster will be populated later.
		let containedin_def .= b:txtfmt_cfg_conceal ? ',Tf_tok' : ',@Tf'.cui.'_tok'
		if b:txtfmt_cfg_leadingindent != 'none'
			let containedin_def .= ',Tf_li_ws,Tf_li_tok,Tf_li_tok'
		endif
	else
		let containedin_def = ''
	endif
	" >>>
	" Build rgn_info list <<<
	" The rgn_info list contains metadata pertaining to the 4 types of regions
	" (fmt, clr, bgc, sqc), which is used in the loops below.
	let rgn_info = [
		\{'name': 'fmt', 'max': b:txtfmt_num_formats - 1, 'offs': []}
	\]
	" FIXME_SQUIGGLE: Probably parameterize...
	if clr_enabled
		call add(rgn_info,
			\{'name': 'clr', 'abbrev': 'fg', 'max': b:txtfmt_cfg_numfgcolors, 'offs': []})
	endif
	if bgc_enabled
		call add(rgn_info,
			\{'name': 'bgc', 'abbrev': 'bg', 'max': b:txtfmt_cfg_numbgcolors, 'offs': []})
	endif
	if sqc_enabled
		call add(rgn_info,
			\{'name': 'sqc', 'abbrev': 'sq', 'max': b:txtfmt_cfg_numsqcolors, 'offs': []})
	endif
	" For color elements (all but first element of rgn_info), build a list
	" mapping 0-based indices to actual color numbers (as used in txtfmtColor
	" and txtfmtBgColor).
	for ri in rgn_info[1:]
		let i = 1
		while i <= b:txtfmt_cfg_num{ri.abbrev}colors
			call add(ri.offs, b:txtfmt_cfg_{ri.abbrev}color{i})
			let i += 1
		endwhile
	endfor
	let num_rgn_typs = len(rgn_info)
	" >>>
	" Loop over 'order' <<<
	" Note: iord determines current 'order' (i.e., total # of rgn types involved
	" in each region); however, rotations within the nested loop below ensure
	" that all permutations of rgn type will be processed for each order.
	" Example: 
	let iord = 0
	let profs = {'all': 0, 'prelim': 0, 'ng-pre': 0, 'cmn': 0, 'subst': 0, 'tpl-processing': 0, 'jdx-update': 0, 'all-perms': 0, 'build-ng': 0, 'build-rgn-body': 0, 'syn-region': 0, 'build-highlight': 0, 'build-clusters': 0}
	let ts_all = reltime()
	while iord < num_rgn_typs
		" Process current 'order'.
		" TODO: Refactor this up higher if it's even still needed.
		" Generate list of down-counting indices corresponding to each
		" position past currently-used portion of rgn_info[]; used to
		" determine when each rgn position should be rotated to end of rgns[].
		let mods = range(num_rgn_typs, num_rgn_typs - iord, -1)
		" Increment the multi-ary 'odometer'.
		" Note: i will become < 0 only when carry occurs at index 0
		let i = 0
		while i >= 0
			"let ts = reltime()
			" TODO: Split up name/max for efficiency reasons.
			let rgns = map(rgn_info[0:iord], 'v:val.name')
			let rest = map(rgn_info[iord + 1:], 'v:val.name')
			" Sort indices into the order required by cterm=
			" Note: cterm= MUST come *after* ctermfg= to ensure that bold
			" attribute is handled correctly in a cterm.
			"	:help cterm-colors
			" Explanation: Having the term= after cterm= ensures that
			" cterm=bold really means bold, and not bright color; note that
			" bright colors can be achieved other ways (e.g., color # above 8)
			" in terminals that support them.
			" Note: dict attribute is used simply to allow us to pass data to
			" the sort function (since VimL has no closures). Alternatively,
			" could make a singleton object.
			" Note: Must supply rgn_info within a dict.
			" TODO: Consider refactoring so that the sort function is a true
			" dict function (on actual dict).
			let sidxs = sort(range(iord + 1),
				\function('s:Sort_rgn_types'), {'rgn_info': rgn_info})

			" Build templates for current 'order' <<<
			" Description of lors, sors, hors
			" These 3 arrays are a convenience. They are 2D arrays containing
			" specific useful combinations of token type indices, as follows:
			" lors: preceding (lower) order regions
			"       regions to which an end token could return us
			" sors: current (same) order regions
			"       regions to which a start token for a rgn type
			"       represented in current region could take us
			" hors: next (higher) order regions
			"       regions to which a start token *not* represented in
			"       current region could take us
			"let profs['prelim'] += str2float(reltimestr(reltime(ts)))

			" Define nextgroup <<<
			"let ts = reltime()
			let ng_xb = s:Make_exe_builder()
			call ng_xb.add(" nextgroup=", 1)
			" Transitions to lower order regions <<<
			let idx = 0
			let need_comma = 0
			let dbg = ''
			while iord && idx <= iord
				let lors = range(idx) + range(idx + 1, iord)
				" Transitions to lower-order groups (used to be rtd group)
				" TODO: Consider whether better way to do this now that no '_rtd' appended.
				call ng_xb.add((need_comma ? "," : "") . "Tf".cui."_", 1)
				call ng_xb.add(join(map(copy(lors), 'rgns[v:val]'), "") . "_", 1)
				call ng_xb.add(join(map(copy(lors), '"offs[" . v:val . "]"'), " . '_' . "))
				let idx += 1
				let need_comma = 1
			endwhile
			" >>>
			" Transitions to same order regions <<<
			let idx = 0
			while idx <= iord
				let sors = range(0, idx - 1) + range(idx + 1, iord) + [idx]
				call ng_xb.add((need_comma ? "," : "")
					\. "@Tf".cui."_" . join(map(copy(sors), 'rgns[v:val]'), ""), 1)
				if len(sors) > 1
					call ng_xb.add("_", 1)
					call ng_xb.add(join(map(copy(sors[0:-2]), '"offs[" . v:val . "]"'), " . '_' . "))
				endif
				call ng_xb.add("_all", 1)
				let idx += 1
				let need_comma = 1
			endwhile
			" >>>
			" Transitions to higher order regions <<<
			let idx = iord + 1
			while idx < num_rgn_typs
				call ng_xb.add((need_comma ? "," : "")
					\. "@Tf".cui."_" . join(rgns + [rgn_info[idx].name], "") . "_", 1)
				" TODO: The join could be factored out since it depends only on iord!!!!!
				call ng_xb.add(join(map(range(iord + 1), '"offs[" . v:val . "]"'), " . '_' . "))
				call ng_xb.add("_all", 1)
				let idx += 1
				let need_comma = 1
			endwhile
			" >>>
			" Make sure an end token ending an O1 region is concealed.
			if iord == 0
				" 1st order rgn
				call ng_xb.add(",Tf_tok", 1)
			endif
			"let rel = reltime(ts)
			"let profs['ng-pre'] += str2float(reltimestr(reltime(ts)))
			" >>>
			" Create tok/esc templates, which may be bgc-specific <<<
			" TODO: Create somewhere in single loop, or perhaps on first encounter.
			let bgc_idx = index(rgns, 'bgc')
			" TODO: Embed this logic later, changing only when necessary...
			let tok_group_xb = s:Make_exe_builder()
			let esc_group_xb = s:Make_exe_builder()
			" TODO: Handle this a different way...
			call tok_group_xb.add('Tf_tok', 1)
			call esc_group_xb.add('Tf_esc', 1)
			if bgc_idx >= 0
				" This region contains bg color; use appropriate tok/esc
				" concealment regions.
				if !b:txtfmt_cfg_conceal
					call tok_group_xb.add(',Tf'.cui.'_tok_', 1)
					call tok_group_xb.add("offs[" . bgc_idx . "]")
				endif
				if b:txtfmt_cfg_escape != 'none'
					call esc_group_xb.add(',Tf'.cui.'_esc_', 1)
					call esc_group_xb.add("offs[" . bgc_idx . "]")
				endif
			endif
			" >>>
			" Build templates for rgns, clusters and highlighting <<<
			"let ts = reltime()
			" Common name and common stuff...
			let rgn_name_xb = s:Make_exe_builder()
			call rgn_name_xb.add('Tf' . cui . '_' . join(rgns, "") . '_', 1)
			" TODO: Consider loopifying this to avoid hybrid arg to add.
			call rgn_name_xb.add(join(map(range(iord + 1), '"offs[" . v:val . "]"'), " . '_' . "))
			let cls_xb = s:Make_exe_builder()
			call cls_xb.add('syn cluster Tf'.cui.'_'.join(rgns, ""), 1)
			if iord > 0
				call cls_xb.add('_', 1)
				call cls_xb.add(join(map(range(iord), '"offs[" . v:val . "]"'), " . '_' . "))
			endif
			call cls_xb.add('_all add=', 1)
			call cls_xb.add(rgn_name_xb)
			let cls_all_xb = s:Make_exe_builder()
			call cls_all_xb.add('syn cluster Tf'.cui.'_all add=', 1)
			call cls_all_xb.add(rgn_name_xb)
			let rgn_cmn_xb = s:Make_exe_builder()
			call rgn_cmn_xb.add('syn region ', 1)
			call rgn_cmn_xb.add(rgn_name_xb)
			call rgn_cmn_xb.add(skip . ' end=/[', 1)
			call rgn_cmn_xb.add(b:txtfmt_re_any_stok_atom, 1)
			call rgn_cmn_xb.add(join(map(copy(rgns), 'b:txtfmt_re_{v:val}_etok_atom'), "")
				\. ']/me=e-'.tok_off.',he=e-'.tok_off
				\.' keepend contains=', 1)
			" Add contains= for tok and (if necessary) esc groups.
			call rgn_cmn_xb.add(tok_group_xb)
			if b:txtfmt_cfg_escape != 'none'
				call rgn_cmn_xb.add(',', 1)
				call rgn_cmn_xb.add(esc_group_xb)
			endif
			call rgn_cmn_xb.add(ng_xb)
			" Distinction: rgn1 vs rgn2
			" rgn1 refers to a region begun by a start token (can be
			" transition to same or higher order)
			" rgn2 refers to a region begun by an end token (always
			" transitions to lower order)
			let rgn_cmn1 = 
				\(iord == 0
				\ ? containedin_def
				\ : ' contained')
			let rgn_cmn2 =
				\' start=/['
				\.join(map(copy(rest), 'b:txtfmt_re_{v:val}_etok_atom'), "")
				\.']/'
				\.' contained'
			let rgn1_xb = s:Make_exe_builder()
			call rgn1_xb.add(rgn_cmn_xb)
			call rgn1_xb.add(rgn_cmn1 . ' start=/', 1)
			" TODO: Consider having a placeholder var for this...
			call rgn1_xb.add('nr2char(b:txtfmt_{rgns[-1]}_first_tok + offs[-1])')
			call rgn1_xb.add('/', 1)
			let hi_xb = s:Make_exe_builder()
			call hi_xb.add('hi ', 1)
			call hi_xb.add(rgn_name_xb)
			let idx = 0
			while idx <= iord
				let sidx = sidxs[idx]
				call hi_xb.add(eq_{rgns[sidx]}, 1)
				call hi_xb.add('b:txtfmt_{rgns[' . sidx . ']}{offs[' . sidx . ']} ')
				let idx += 1
			endwhile
			let rgn2_xb = s:Make_exe_builder()
			if iord < num_rgn_typs - 1
				call rgn2_xb.add(rgn_cmn_xb)
				" TODO: Perhaps move it down here...
				call rgn2_xb.add(rgn_cmn2, 1)
			endif
			" >>>
			" Process templates for current permutation of rgn types for current 'order' <<<
			" Convert templates to strings, then evaluate template strings in
			" loop over all permutations of rgn indices.
			" Note: The templates contain references to rgns[] and offs[],
			" which are adjusted within the loop: rgns is adjusted by
			" left-rotation within the mods[] update loop; offs[] are
			" calculated explicitly when rgn indices (jdxs[]) are updated.
			let cls_estr = cls_xb.get_estr()
			let cls_all_estr = cls_all_xb.get_estr()
			let rgn1_estr = rgn1_xb.get_estr()
			let hi_estr = hi_xb.get_estr()
			let rgn2_estr = rgn2_xb.get_estr()
			"let profs['cmn'] += str2float(reltimestr(reltime(ts)))
			"let ts = reltime()
			" Loop over all permutations of rgn indices <<<
			" Note: jdxs[] represents rgn indices for all positions involved
			" in current 'order'.
			let jdxs = repeat([1], iord + 1)
			" Convert 0-based offset to token offset
			let offs = map(range(iord + 1), 'rgn_info[v:val].name == "fmt"'
				\.' ? 1 : rgn_info[v:val].offs[0]')
			let i = iord
			" Evaluate templates in loop over all permutations of indices for
			" the current permutation of rgn types: e.g., fmt1-clr1,
			" fmt1-clr2, fmt1-clr3, etc...
			while i >= 0
				" Evaluate templates to define regions, clusters and
				" highlighting.
				"let ts1 = reltime()
				"let profs['subst'] += str2float(reltimestr(reltime(ts2)))
				"let ts2 = reltime()
				" Define regions.
				exe eval(rgn1_estr)
				if iord < num_rgn_typs - 1
					exe eval(rgn2_estr)
				endif
				"let profs['syn-region'] += str2float(reltimestr(reltime(ts2)))
				"let ts2 = reltime()
				" Define clusters.
				exe eval(cls_estr)
				exe eval(cls_all_estr)
				"let profs['build-clusters'] += str2float(reltimestr(reltime(ts2)))
				"let ts2 = reltime()
				" Define highlighting.
				exe eval(hi_estr)
				"let profs['build-highlight'] += str2float(reltimestr(reltime(ts2)))
				"let profs['tpl-processing'] += str2float(reltimestr(reltime(ts1)))
				"let ts1 = reltime()
				" Generate next permutation of rgn indices <<<
				" Increment jdxs[] as a rippling up-counter, with rgn_info[i].max
				" determining when 'carry' occurs from position i to neighbor
				" position i-1. Break out of loop as soon as ripple is
				" complete (i.e., when carry fails to occur).
				" Note: When rgn type != fmt, actual rgn index used in syntax
				" group name may not correspond to the 1-based, contiguous
				" ranges in jdxs[]; the mapping of conceptual to real offsets
				" is maintained in offs[].
				let i = iord
				while i >= 0
					let jdxs[i] += 1
					if jdxs[i] > rgn_info[i].max
						let jdxs[i] = 1
						let offs[i] = rgn_info[i].name == "fmt"
							\? 1 : rgn_info[i].offs[0]
						" Keep going leftward unless we're done
						let i -= 1
						if i < 0
							" Hit modulo in 1st position: we're done.
							break
						endif
					else
						" j hasn't rolled over
						if rgn_info[i].name == "fmt"
							let offs[i] += 1
						else
							let offs[i] = rgn_info[i].offs[jdxs[i] - 1]
						endif
						break
					endif
				endwhile
				"let profs['jdx-update'] += str2float(reltimestr(reltime(ts1)))
				" >>>
			endwhile
			"let profs['all-perms'] += str2float(reltimestr(reltime(ts)))
			" >>>
			" >>>

			" Generate next permutation of rgn types for current 'order' <<<
			" Note: We get here just after incrementing through all
			" permutations of rgn indices for current permutation of rgn types
			" at current 'order'. Next permutation of rgn types is generated
			" by using mods[] as a down-counting ripple counter, in which each
			" position triggers a leftward rotation of range rgn_info[i:] when
			" the counter at position i reaches 0.
			" Note: Because of the ripple effect, multiple rotations may occur
			" between loop entry and exit, but conceptually, it's a single
			" increment of the overall counter. (Think multiple bits in a
			" binary counter changing value.)
			let i = iord
			while i >= 0
				" Skip pointless rotation of single element
				if i < num_rgn_typs - 1
					" Rotate i to end
					" Optimization Possibility: Don't really need to rotate
					" when i == iord, since there are no affected positions
					" rightward.
					let r = remove(rgn_info, i)
					call add(rgn_info, r)
				endif
				let mods[i] -= 1
				if mods[i] <= 0
					" Carry leftward (by avoiding break) and reset modulo down
					" counter for this position.
					let mods[i] = num_rgn_typs - i
				else
					" No need to go further left.
					break
				endif
				" Keep going leftward unless we're done
				let i -= 1
				if i < 0
					" Hit modulo in 1st position: we're done.
					break
				endif
			endwhile
		endwhile
		" Increase 'order'
		let iord += 1
	endwhile
	" >>>
	"let profs['all'] += str2float(reltimestr(reltime(ts_all)))
	"echo "Profile results: " . string(profs)
	" Handle escape/escapee token pairs both inside and outside regions. <<<
	" Important Note: These groups must be defined after the fmt/clr regions,
	" since they must take precedence over them.
	" Objectives:
	" -Conceal the escape token
	" -Prevent escaped or escaping tokens from beginning a region
	" -Prevent escaped or escaping tokens from ending a region
	" Note: Must take into account the txtfmt 'escape' option
	" Also Note: Must take into account the size in bytes of the escape char
	" if this Vim treats offsets as byte offsets
	" Important TODO: Original design uses more complex txtfmt_re_{...}
	" regexes to recognize highlight regions when esc != 'none'. This is not
	" really necessary, since the match groups used to highlight escape pairs
	" effectively prevent escape-escapee tokens from beginning a highlight
	" region. (I've actually verified this.) Thus, though the more complex
	" patterns are needed for some things (e.g., jump to tok and auto-maps),
	" we could use simpler (and faster) patterns for defining the syntax
	" regions. Of course, this is not really high priority, since it affects
	" only non-default (and probably extremely rare) 'escape' settings.
	if b:txtfmt_cfg_escape == 'bslash' || b:txtfmt_cfg_escape == 'self'
		if b:txtfmt_cfg_escape == 'self'
			let re_esc_pair = '\(['.b:txtfmt_re_any_tok_atom.']\)\1'
			" Escape char is same number of bytes as a token
			let esc_off = tok_off
		elseif b:txtfmt_cfg_escape == 'bslash'
			let re_esc_pair = '\\\%(\\\%(\\*['
				\ . b:txtfmt_re_any_tok_atom.']\)\@=\|['.b:txtfmt_re_any_tok_atom.']\)'
			" Escape char is single byte
			let esc_off = 1
		endif
		" Prevent escaping or escaped tokens from starting regions (now or
		" even later, after surrounding regions have changed).
		" Note: The outer esc pair must match only at top-level (or nested in
		" non-txtfmt group): hence, unlike Tf_esc, it lacks 'contained' attr.
		exe 'syn match Tf_outer_esc /'.re_esc_pair.'/he=s+'.esc_off.containedin_def.conceal
		exe 'syn match Tf_esc /'.re_esc_pair.'/he=s+'.esc_off.' contained'.conceal
		" Define highlighting for the outer and inner escape tokens
		" Design Decision: In 'noconceal' case, these groups are needed to
		" hide the escape; in 'conceal' case, they would be needed only to
		" make the escape transparent when 'cocu' setting prevents
		" concealment, but for the sake of consistency with tokens, I prefer
		" that escapes be visible in that case; hence, I link the escape
		" groups to Tf_conceal only in 'noconceal' case.
		if !b:txtfmt_cfg_conceal
			hi link Tf_outer_esc Tf_conceal
			hi link Tf_esc Tf_conceal
		endif
		" bgc-specific esc concealment groups are needed in both 'conceal' and
		" 'noconceal' cases.
		" Rationale: There's an idiosyncrasy wrt use of transparent and
		" conceal on a contained match group with he=s+1: specifically, the
		" s+1 char is spuriously concealed. My solution is never to use
		" 'transparent' on Tf_esc groups, but define bgc-specific esc groups
		" in both 'conceal' and 'noconceal' cases. Tf_tok, on the other hand,
		" can use transparent because it doesn't limit highlighting with he=.
		" Note: To understand why we create regions for bgc but not (eg)
		" underline, undercurl, etc., see rationale near creation of
		" bgc-specific token concealment regions.
		exe 'syn cluster Tf'.cui.'_esc add=Tf_esc'
		let pi = 1
		while pi <= (b:txtfmt_cfg_bgcolor ? b:txtfmt_cfg_numbgcolors : 0)
			let i = b:txtfmt_cfg_bgcolor{pi}
			exe 'syn match Tf'.cui.'_esc_'.i.' /'.re_esc_pair.'/he=s+'.esc_off
				\ .' contained'.conceal
			exe 'hi Tf'.cui.'_esc_'.i.' '.eq_bgc.b:txtfmt_bgc{i}.eq_clr.b:txtfmt_bgc{i}
			exe 'syn cluster Tf'.cui.'_esc add=Tf'.cui.'_esc_'.i
			let pi = pi + 1
		endwhile
	endif
	" >>>
endfu	" >>>
" Function: s:Define_syntax_syncing() <<<
fu! s:Define_syntax_syncing()
	" Configure syncing based upon syncmethod and (if applicable) synclines
	" options. (Note that 'sync' is the only option visible to user. It is
	" decoded into the two more convenient options by Do_config_common ->
	" Set_syncing.)
	if b:txtfmt_cfg_syncmethod == 'fromstart'
		syn sync fromstart
	elseif b:txtfmt_cfg_syncmethod == 'minlines'
		exe 'syn sync minlines='.b:txtfmt_cfg_synclines
	endif
endfu	" >>>
" Function: s:Set_current_syntax() <<<
" Purpose: Set b:current_syntax to something sensible. If txtfmt is loaded
" in conjunction with one or more other plugins, we should set
" b:current_syntax to a dot-separated syntax name list that reflects all
" syntaxes loaded up to and including ours. Note that the b:txtfmt_syntax
" variable should permit us to do this even when other syntax plugins in the
" load chain have not respected assignments to b:current_syntax made by their
" predecessors in the load chain.
fu! s:Set_current_syntax()
	if exists('b:txtfmt_syntax') && b:txtfmt_syntax =~ '\%(^\|\.\)txtfmt\%(\.\|$\)'
		" Set b:current_syntax to the portion of b:txtfmt_syntax up to and
		" including the first (and hopefully the only) occurrence of 'txtfmt'
		let b:current_syntax =
			\ substitute(b:txtfmt_syntax,
			\ '\(.\{-}\%(^\|\.\)txtfmt\%(\.\|$\)\).*', '\1', '')
	else
		" This shouldn't happen unless user is manually sourcing the txtfmt
		" plugin files (which also shouldn't happen). Still, if it does,
		" 'txtfmt' is the most sensible choice.
		let b:current_syntax = 'txtfmt'
	endif
endfu
" >>>
" Call functions to define syntax and syntax syncing <<<
call s:Define_syntax()
call s:Define_syntax_syncing()
" >>>
" Call function to set b:current_syntax variable <<<
call s:Set_current_syntax()
" >>>
" LESSONS LEARNED <<<
" -By default, an item is contained only at top level.
" -containedin=TOP causes an item to be contained not just in top level, but
"  in an item which does not have contained set.
" -When an inner (contained) region is truncated because of a keepend in a
"  containing region, the inner regions highlighting is used up until the
"  point where truncation occurs!!!!! This is not obvious from help. However,
"  it's simple to demonstrate: copy the nested parens example from Vim help
"  as-is, but add the keepend argument to par1 region. Source the file and
"  view text with 3 levels of parens. Colors will be as expected until the
"  first close paren is encountered. It will be colored according to the
"  highlighting of the innermost nested paren region, not the outer as I would
"  have expected.
" -You can use patterns for contains=, containedin=, etc..., but only groups
"  defined at the time the command is executed will be matched! (In original
"  implementation, this is why I ran the Define_syntax() function twice. Now I
"  use clusters.)
" -With keepend, when doing matches for contained groups, the match is
"  performed prior to checking for end of containing group. If containing
"  group ends inside the contained group, the contained group will be
"  truncated, but for purposes of ms=, me=, hs=, he=, the end of the contained
"  group is not altered to reflect the point of truncation!!!!!
" -There is an apparent bug with the way contains= and containedin= work with
"  matchgroup. I have submitted to Vim list, and am awaiting Bram's return
"  from Uganda, at which time he has suggested he will investigate.
" -NOTE: A transparent group inherits the contains= arguments of its
"  containing group! (Can lead to unexpected behavior.)
" -Apparent bug with transparent groups inheriting syntax of contained group,
"  even when the current location in containing group has syntax disabled due
"  to a he=<...>. Example: An empty format region has its open delimiter
"  highlighted as error. The remainder of the region is not highlighted
"  specially. However, when a transparent escape-escapee pair appears inside
"  the empty region, it takes on Error syntax, even though it is past the
"  portion of the empty region highlighted as error.
" -IT IS POSSIBLE to have multiple containedin= attributes in the same group,
"  even where they would appear to conflict.
"  Example: group A may be contained by any group whose name matches
"  MyGroup.*Special; additionally, it may be contained in any group whose name
"  does not begin with MyGroup.
"  containedin=ALLBUT,MyGroup.* containedin=MyGroup.*Special
"  Note that this works because when Vim encounters a containedin, it simply
"  adds the appropriate contains= attributes to the specified containing
"  groups; i.e., a containedin= cannot "rule out" future containment due to a
"  subsequent containedin=.
" E56 - (* operand could be empty) workaround for the following pattern:
" \(ab\)\1*
" which will generate E56 in Vim, even though \1 cannot be empty
" Workaround: \(ab\)\%(\1\@=..\)*
" >>>

	" vim: sw=4 ts=4 foldmethod=marker foldmarker=<<<,>>> :
