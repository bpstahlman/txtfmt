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
" Achieve clr->bgc->fmt order required for highlight command.
" Assumption: a and b will always be different.
fu! s:Sort_rgn_types(a, b)
	" Assumption: Elements to be sorted are either fmt/clr/bgc/... strings or
	" dicts with 'name' key containing such strings.
	let [a, b] = type(a:a) == 1 ? [a:a, a:b] : [a:a.name, a:b.name]
	if a == 'clr'
		return -1
	elseif a == 'fmt'
		return 1
	elseif a == 'bgc'
		return b == 'fmt' ? -1 : 1
endfu
" States: 0=empty, 1=expr, 2=literal
" FIXME_COMBINATIONS: Add constructor initialization.
fu! s:Make_exe_builder()
	let o = {'st': 0, 'st_beg': 0, 's': ''}
	" Add list of deferred exprs and const separator.
	" Note: The resulting construct is effectively an expr, since the
	" separator will be interior if it is used at all.
	" FIXME: Consider making sep optional.
	fu! o.add_list(ls, sep)
		" Special Case: If sep is null, prefer `blah . blah' over `blah . "" . blah'
		call self.add(join(a:ls, a:sep ? " . '" . a:sep . "' . " : " . "))
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
" Function: s:Get_rgn_combinations_r() <<<
fu! s:Get_rgn_combinations_r(i, rem, rgns)
	let i = a:i
	let ret = []
	while i <= len(a:rgns) - a:rem
		if a:rem == 1
			" Base case
			call add(ret, [[a:rgns[i]],
				\ (i > a:i ? a:rgns[a:i : i - 1] : [])
				\ + (i < len(a:rgns) - 1 ? a:rgns[i + 1 : ] : [])])
		else
			let children = s:Get_rgn_combinations_r(i + 1, a:rem - 1, a:rgns)
			for child in children
				call insert(child[0], a:rgns[i])
				if i > a:i
					call extend(child[1], a:rgns[a:i : i - 1])
				endif
				call add(ret, child)
			endfor
		endif
		let i += 1
	endwhile
	return ret
endfu
" >>>
" Function: s:Get_rgn_combinations() <<<
" Takes a list of active rgns and a 1-based 'order' number, and builds a list
" of order-preserving combinations (assuming input list is in fiducial order).
" Note: Need to add ability to return both the pos and neg list: i.e., an
" ordered list of the rgns involved in the combination, and a second list of
" those that are *not* involved.
" TODO_COMBINATIONS: Consider combining this with its recursive workhorse
" somehow.
fu! s:Get_rgn_combinations(rgns, order)
	return s:Get_rgn_combinations_r(0, a:order, a:rgns)
endfu
" >>>
" >>>
" Function: s:Build_higher_order_cluster() <<<
" FIXME_COMBINATIONS: I've inlined this: remove...
fu! Build_higher_order_cluster(xb, cui, rgns, ho_rgn)
	let rgns = map(a:rgns[:], '{"name": v:val, "idx": "offs[" . v:key . "]"}')
		\ + [{'name': a:ho_rgn, 'idx': "'all'"}]
	let rgns = sort(rgns, function('Sort_rgn_types'))
	let names = map(rgns[:], 'v:val.name')
	let idxs = map(rgns[:], 'v:val.idx')

	call a:xb.add("@Tf" . a:cui . "_" . join(names, "") . "_", 1)
	call a:xb.add_list(idxs, "_")
endfu
" <<<
" Function: s:Define_syntax() <<<
fu! s:Define_syntax()
	" Cache some useful vars <<<
	" Define a convenience flag that indicates whether background colors are
	" in effect
	let bgc_enabled = b:txtfmt_cfg_bgcolor && b:txtfmt_cfg_numbgcolors > 0
	let clr_enabled = b:txtfmt_cfg_numfgcolors > 0

	" cui (color uniqueness index) will contain a different index for each
	" color configuration (and will be empty string in the unlikely event that
	" both numfgcolors and numbgcolors are 0 - i.e., no colors used)
	" Note: This strategy is necessary because Vim's highlight groups are not
	" buffer-specific, but there is a buffer-specific version of txtfmtColor{}
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
		let eq_fmt = ' gui='
	else
		let eq_clr = ' ctermfg='
		let eq_bgc = ' ctermbg='
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
	" The rgn_info list contains metadata pertaining to the 3 types of regions
	" (fmt, clr, bgc), which is used in the loops below.
	let rgn_info = [
		\{'name': 'fmt', 'max': b:txtfmt_num_formats - 1, 'offs': []}
	\]
	if clr_enabled
		call add(rgn_info,
			\{'name': 'clr', 'abbrev': 'fg', 'max': b:txtfmt_cfg_numfgcolors, 'offs': []})
	endif
	if bgc_enabled
		call add(rgn_info,
			\{'name': 'bgc', 'abbrev': 'bg', 'max': b:txtfmt_cfg_numbgcolors, 'offs': []})
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
	" in each region)
	let iord = 0
	let profs = {'all': 0, 'prelim': 0, 'ng-pre': 0, 'cmn': 0, 'subst': 0, 'tpl-processing': 0, 'jdx-update': 0, 'all-perms': 0, 'build-ng': 0, 'build-rgn-body': 0, 'syn-region': 0, 'build-highlight': 0, 'build-clusters': 0}
	let ts_all = reltime()
	while iord < num_rgn_typs
		" Process current 'order'.
		" Build all combinations...
		" Note: Each element of rgn_combs is a list of dicts that exist in
		" rgn_info[]. It should be considered a read-only view of only those
		" regions implicated in a particular combination.
		let rgn_combs = s:Get_rgn_combinations(rgn_info, iord + 1)
		for rgn_comb in rgn_combs
			"let ts = reltime()
			" TODO: Split up name/max for efficiency reasons.
			" FIXME_COMBINATIONS: Consider caching some more forms: e.g.,
			" rgn_comb[0/1] as named, not indexed vars. Also, consider more
			" symmetric name than rest: e.g., rgns and rems or RGNS.
			let [rgn_objs, rem_objs] = [rgn_comb[0], rgn_comb[1]]
			" TODO_COMBINATIONS: Consider changing rest to rems for
			" greater parallelism.
			let rgns = map(rgn_objs[:], 'v:val.name')
			let rest = map(rem_objs[:], 'v:val.name')
			" Assumption: Indices already in fiducial order (required by cterm=).
			" Note: cterm= MUST come *after* ctermfg= to ensure that bold
			" attribute is handled correctly in a cterm.
			"	:help cterm-colors
			" Explanation: Having the term= after cterm= ensures that
			" cterm=bold really means bold, and not bright color; note that
			" bright colors can be achieved other ways (e.g., color # above 8)
			" in terminals that support them.

			" Build templates for current 'order' <<<
			" Description of lors, sors, hors
			" These 3 arrays are a convenience. They are 2D arrays containing
			" specific useful combinations of token type indices, as follows:
			" lors: preceding (lower) order regions
			"       regions to which an end token for a rgn type represented
			"       in current region could could return us
			" sors: current (same) order regions
			"       regions to which a start token for a rgn type
			"       represented in current region could take us
			"       FIXME_COMBINATIONS: Are sors even still a thing? I mean,
			"       nothing's changing any more, right?
			" hors: next (higher) order regions
			"       regions to which a start token *not* represented in
			"       current region could take us
			"let profs['prelim'] += str2float(reltimestr(reltime(ts)))

			" Define nextgroup <<<
			"let ts = reltime()
			let ng_xb = s:Make_exe_builder()
			call ng_xb.add(" nextgroup=", 1)
			" Transitions to lower order regions <<<
			let need_comma = 0
			if iord
				for idx in range(iord + 1)
					" Transitions to lower-order groups (used to be rtd group)
					" TODO: Consider whether better way to do this now that no '_rtd' appended.
					call ng_xb.add((need_comma ? "," : "")
						\ . "Tf" . cui . "_"
						\ . join(filter(rgns[:], 'v:key != l:idx'), "")
						\ . "_", 1)
					call ng_xb.add_list(map(range(idx) + range(idx + 1, iord),
						\ '"offs[" . v:val . "]"'), "_")
					" TODO_COMBINATIONS: Decide whether these builders are still
					" warranted. Note that offs[] is not evaluated here.
					let need_comma = 1
				endfor
			endif
			" >>>
			" Transitions to same order regions <<<
			" FIXME_COMBINATIONS: I'm thinking it's possible that same order
			" regions could go away...
			" Rationale: Impossible for a single start token to change the
			" combination without changing the order.
			for idx in range(iord + 1)
				call ng_xb.add((need_comma ? "," : "") . "@Tf" . cui . "_"
					\ . join(rgns[:], "") . '_', 1)
				" TODO: Check level of quoting here...
				call ng_xb.add_list(map(range(iord + 1),
					\ 'v:val == l:idx ? "''all''" : "offs[" . v:val . "]"'), "_")
				let need_comma = 1
			endfor
			" >>>
			" Transitions to higher order regions <<<
			" Loop over the other regions (in rest)
			for orgn in rest
				" TODO: Is this check even necessary for higher order? If so,
				" perhaps pass to Build_higher_order_cluster.
				if need_comma | call ng_xb.add(",", 1) | endif
				" Pull in one of the rgn types unused at this order.
				" helper function to ensure it's inserted at the proper spot.
				" FIXME: Probably cache a list of rgn names just inside order
				" loop.
				"call s:Build_higher_order_cluster(ng_xb, cui,
				"	\ map(rgns[:], 'v:val.name'), orgn.name)
				" FIXME_COMBINATIONS: Given that we're inserting element into
				" already-sorted list, this approach may be overkill. Revisit
				" if efficiency matters.
				let objs = sort(map(rgns[:],
					\ '{"name": v:val, "idx": "offs[" . v:key . "]"}')
					\ + [{'name': orgn, 'idx': "'all'"}],
					\ function('s:Sort_rgn_types'))
				call ng_xb.add("@Tf" . cui . "_"
					\ . join(map(objs[:], 'v:val.name'), "") . "_", 1)
				call ng_xb.add_list(map(objs[:], 'v:val.idx'), "_")
				let need_comma = 1
			endfor
			" >>>
			" Make sure an end token ending a first order region is concealed.
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
			call rgn_name_xb.add_list(map(range(iord + 1), '"offs[" . v:val . "]"'), "_")

			" Create builders for the combination-specific "all" clusters.
			" Note: Eval loop will need to loop over all builders in cls_xbs.
			let cls_xbs = []
			for idx in range(iord + 1)
				let cls_xb = s:Make_exe_builder()
				call add(cls_xbs, cls_xb)
				call cls_xb.add('syn cluster Tf' . cui . '_' . join(rgns, ""), 1)
				call cls_xb.add('_', 1)
				call cls_xb.add_list(map(range(iord + 1),
					\ 'v:val == l:idx ? "''all''" : "offs[" . v:val . "]"'), "_")
				call cls_xb.add(' add=', 1)
				call cls_xb.add(rgn_name_xb)
			endfor

			" Create builder used to augment the non-specific "all" cluster.
			let cls_all_xb = s:Make_exe_builder()
			call cls_all_xb.add('syn cluster Tf' . cui . '_all add=', 1)
			call cls_all_xb.add(rgn_name_xb)

			" Create builder for the common portion of syn region
			let rgn_cmn_xb = s:Make_exe_builder()
			call rgn_cmn_xb.add('syn region ', 1)
			call rgn_cmn_xb.add(rgn_name_xb)
			call rgn_cmn_xb.add(skip . ' end=/[', 1)
			" FIXME_COMBINATIONS: With permutation-sensitive engine, any start
			" tok ended a region; however, with combinations, we care only
			" about start tokens not represented in current combination.
			call rgn_cmn_xb.add(b:txtfmt_re_any_stok_atom, 1)
			" Region is ended by end tok for any rgn type in current
			" combination.
			call rgn_cmn_xb.add(join(map(copy(rgns), 'b:txtfmt_re_{v:val}_etok_atom'), "")
				\. ']/me=e-' . tok_off . ',he=e-' . tok_off
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
			" FIXME_COMBINATIONS: Remove misleading "cmn" from these 2 var
			" names. Actually, could just add directly in the builder add() call.
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
			" FIXME_COMBINATIONS: Region can start with the start tok of *any*
			" of the regions in the current combination (not just the last as
			" with permutations).
			call rgn1_xb.add_list(map(range(iord + 1),
				\ '"nr2char(b:txtfmt_" . rgns[v:val] . "_first_tok + offs[" . v:val . "])"'), "")
			call rgn1_xb.add('/', 1)
			let hi_xb = s:Make_exe_builder()
			call hi_xb.add('hi ', 1)
			call hi_xb.add(rgn_name_xb)
			for idx in range(iord + 1)
				call hi_xb.add(eq_{rgns[idx]}, 1)
				call hi_xb.add('b:txtfmt_{rgns[' . idx . ']}{offs[' . idx . ']} ')
			endfor
			let rgn2_xb = s:Make_exe_builder()
			if iord < num_rgn_typs - 1
				call rgn2_xb.add(rgn_cmn_xb)
				" TODO: Definitely move the common stuff down here since it
				" may not even be needed...
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
			" Note: Intentionally mapping destructively over cls_xbs, which
			" won't be needed again before it's rebuilt for a new combination.
			let cls_estrs = map(cls_xbs, 'v:val.get_estr()')
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
			" Initialize offs[] with first used non-default index for each rgn
			" type (1 for fmt, first used color number for color rgns).
			" TODO_COMBINATIONS: Consider testing for offs member instead of
			" checking name.
			let offs = map(range(iord + 1), 'rgn_objs[v:val]["name"] == "fmt"'
				\.' ? 1 : rgn_objs[v:val].offs[0]')
			" Count down
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
				for cls_estr in cls_estrs
					exe eval(cls_estr)
				endfor
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
					if jdxs[i] > rgn_objs[i].max
						let jdxs[i] = 1
						let offs[i] = rgn_objs[i].name == "fmt"
							\? 1 : rgn_objs[i].offs[0]
						" Keep going leftward unless we're done
						let i -= 1
						if i < 0
							" Hit modulo in 1st position: we're done.
							break
						endif
					else
						" j hasn't rolled over
						if rgn_objs[i].name == "fmt"
							let offs[i] += 1
						else
							let offs[i] = rgn_objs[i].offs[jdxs[i] - 1]
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
		endfor
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

" IMPORTANT NOTE: The Vim script ends here. The perl script used to generate
" portions of this file follows...
finish

#!perl

# This script generates the core of the Define_syntax function of Brett
# Stahlman's Txtfmt Vim plugin
# It is designed to be invoked from a Vim external program read filter, as
# follows:
# :r !gen_txtfmt_def_syn.pl
# There are 2 passes:
# Pass 1) Generates definitions, which are specific to a token type combination
# (e.g., "fmt", "fmtclr", etc...)
# Pass 2) Generates the nested loops used to define syntax regions

@rgn = qw(clr bgc fmt);
# TODO: If I keep support for separate bgc and clr definitions, I can get rid
# of the %rhs hash (after making corresponding code modifications)
%rhs = qw(clr clr bgc bgc fmt fmt);
%ord = qw(clr 0 bgc 1 fmt 2);
# Note which types of regions can be disabled through Vim options
@can_dsbl = qw(clr bgc);
# Subtract 1 from b:txtfmt_num_formats (which currently includes the default
# format token)
%loopinfo = (
	clr => {
		cnt =>'b:txtfmt_cfg_numfgcolors',
		indarr => 'b:txtfmt_cfg_fgcolor',
	},
	bgc => {
		cnt =>'b:txtfmt_cfg_numbgcolors',
		indarr => 'b:txtfmt_cfg_bgcolor',
	},
	fmt => {
		cnt =>'b:txtfmt_num_formats - 1',
		indarr => undef,
	},
);
# TODO: Get rid of %cnt and %idxind if I elect to keep %loopinfo
%cnt = (
	clr => 'b:txtfmt_cfg_numfgcolors',
	bgc => 'b:txtfmt_cfg_numbgcolors',
	fmt => 'b:txtfmt_num_formats - 1'
);
# Define a hash supporting index indirection
%idxind = (
	clr => 'b:txtfmt_cfg_fgcolor',
	bgc => 'b:txtfmt_cfg_bgcolor',
	fmt => undef
	# Could omit fmt or leave it undef
);

# Define base indentlevel of the entire block
$init_il = 1;

# This semaphore helps determine when an "if <typ>_enabled" construct in the
# Vim code would be redundant with a containing one.
# TODO: Currently unused - remove...
my $bgc_guard_cnt = 0;

sub do_lvl($$)
{
	# Description of arrays
	# @a1 - $_[1]: lhs (fixed) array of token type names. Upon function entry,
	#      this array represents the most recent region combination to have
	#      been processed.
	# @a1n - lhs (fixed) array of token type names that will be passed in next
	#       recursive call to this function. Created by appending a single
	#       token type name from @a2
	# @a2 - $_[2]: rhs (unfixed) array of token type names. Upon function
	#       entry, this array contains the token type names not included in the
	#       currently processed region combination. (e.g, if we're currently
	#       processing "fmt-clr" regions, then @a2 contains "bgc")
	# @a2n - rhs (unfixed) array of token type names that will be passed in
	#        next recursive call to this function. Created by removing a single
	#        token type name from @a2

	# Basic algorithm
	# Loop over the token type names in @a2, appending each, in turn, to @a1
	# (and calling the result @a1n). For each iteration, process the
	# corresponding region combination, which will involve all token type names
	# contained in @a1n, then call self recursively (passing @a1n and @a2n) to
	# process the next level.
	my @a1 = @{shift()}; # Fixed portion
	my @a2 = @{shift()}; # Unfixed portion
	# Determine the level of this recursive call according to number of
	# elements in @a1
	my $lvl = @a1;
	# Loop over unfixed portion
	for my $a2 (@a2) {
		my @a1n = (@a1, $a2);
		# Create a hash mapping region names to loop var names
		my %idx = ();
		my $var = 'i';
		for my $a1n (@a1n) {
			$idx{$a1n} = $var++;
		}

		my @a2n = grep { $_ ne $a2 } @a2;
		# Description of @lor, @sor, @hor
		# These 3 arrays are a convenience. They are 2D arrays containing
		# specific useful combinations of token type names corresponding to the
		# preceding level, the current level, and the next level, respectively.

		# Lower order
		# Remove each element in turn from @a1n
		my @lor = ();
		if (@a1n > 1) {
			for my $r (@a1n) {
				push @lor, [ grep { $_ ne $r } @a1n ];
			}
		}
		# Same order
		# Move each element within @a1n to the end
		my @sor = ();
		for (my $i = 0; $i < @a1n; $i++) {
			my @r = @a1n;
			my $r = splice @r, $i, 1;
			push @sor, [ @r, $r ];
		}
		# Higher order
		# Add region types from @a2n to the end
		my @hor = ();
		for my $r (@a2n) {
			push @hor, [ @a1n, $r ];
		}
		# Determine initial indent level
		my $il = "\t" x ($init_il + $lvl);

		# Set convenience variable $need_r2_guard if and only if all the
		# region types yet to be pulled in (i.e., the ones whose end tokens
		# could begin the current region) are regions that can be disabled.
		my $need_r2_guard = join('', sort @can_dsbl) =~ join('', sort @a2n);

		# Begin outputting
		# Insert an "if <typ>_enabled" if and only if the rightmost region
		# type in the fixed array is <typ> (i.e., the region being begun
		# should not exist if <typ>_enabled is false). (If the fixed portion
		# contains <typ> prior to the last element, we're already inside an
		# "if <typ>_enabled", in which case, another would be redundant.)
		for my $typ (grep { $_ eq $a1n[-1] } @can_dsbl) {
			print "\n$il", '"============= BEGIN NON-INDENTING BLOCK =============';
			print "\n$il", "if ${typ}_enabled";
		}

		# PRE RECURSION
		# Determine current level's indent
		$il = "\t" x ($init_il + $lvl);
		print "\n$il\"===";
		print "\n$il\"*** Loop over ", join("-", @a1n), " levels";
		print "\n$il\"===";
		# TODO: Think about cleaning this up a bit and adding comments for
		# the index indirection...
		print "\n$il", "let $idx{$a1n[-1]}", ($loopinfo{$a1n[-1]}{indarr} ? 'p' : ''), " = 1";
		print "\n$il", "while $idx{$a1n[-1]}", ($loopinfo{$a1n[-1]}{indarr} ? 'p' : ''), " <= $loopinfo{$a1n[-1]}{cnt}";
		$il .= "\t";
		print "\n$il", "let $idx{$a1n[-1]} = $loopinfo{$a1n[-1]}{indarr}\{$idx{$a1n[-1]}p\}"
			if $loopinfo{$a1n[-1]}{indarr};
		print "\n$il", "let ch$idx{$a1n[-1]} = nr2char(b:txtfmt_$a1n[-1]_first_tok + $idx{$a1n[-1]})";

		print "\n$il\" Add to appropriate clusters";
		print "\n$il", "exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_",
			join("", @a1n), "_'.",
			join(".'_'.", @idx{@a1n})
		;
		print "\n$il", "exe 'syn cluster Tf'.cui.'_",
			join("", @a1n),
			(@a1n > 1
				? "_'." . join(".'_'.", @idx{@a1n[0 .. $#a1n - 1]}) . ".'"
				: ""
			),
			"_all add=Tf'.cui.'_",
			join("", @a1n),
			"_'.",
			join(".'_'.", @idx{@a1n})
		;
		# Define tok/esc concealment group if background color could be changing (i.e., if
		# last element of @a1n is 'bgc')
		# Note: tok/esc concealment group will retain setting given until we move back to
		# a lower order region
		# Also note: bgc_enabled check would be superfluous here, since
		# code won't be executed if bgc_enabled is false
		if ($a1n[-1] eq 'bgc') {
			# Note: Leave Tf_{tok,esc}_group's at their defaults when conceal is disabled.
			print "\n$il", 'if !b:txtfmt_cfg_conceal';
			print "\n$il\t", '" Ensure that this and higher order regions use bgc-specific concealment group';
			print "\n$il\t", "let Tf_tok_group = 'Tf'.cui.'_tok_'.$idx{$a1n[-1]}";
			print "\n$il\t", "if b:txtfmt_cfg_escape != 'none'";
			print "\n$il\t\t", '" Ensure that this and higher order regions use bgc-specific esc group';
			print "\n$il\t\t", "let Tf_esc_group = 'Tf'.cui.'_esc_'.$idx{$a1n[-1]}";
			print "\n$il\t", 'endif';
			print "\n$il", 'endif';
		}

		# Define nextgroup
		my $ng = "";
		for my $lor (@lor) {
			# Transitions to lower-order groups (used to be rtd group)
			# TODO: Consider whether better way to do this now that no '_rtd' appended.
			$ng .= ",Tf'.cui.'_" . join("", @$lor) . "_'." . join(".'_'.", @idx{@$lor}) . ".'";
		}
		for my $sor (@sor) {
			$ng .= ",\@Tf'.cui.'_" . join("", @$sor) .
				(@$sor > 1
					? "_'." . join(".'_'.", @idx{@{$sor}[0 .. $#$sor - 1]}) . ".'"
					: ""
				) .
				"_all";
		}
		# Note: We didn't need to worry about checking <typ>_enabled for
		# the lor and sor case (since this code will be inside an "if
		# <typ>_enabled" if <typ> is in either of those arrays); however,
		# the hor case pulls in a new region type, so we will need to
		# check it.
		my $unquoted = 0;
		for my $hor (@hor) {
			my $typ;
			if (($typ) = grep { $_ eq $hor->[-1] } @can_dsbl) {
				$ng .= ($unquoted ? '' : "'") . ".(${typ}_enabled ? '";
			}
			elsif ($unquoted) {
				$ng .= ".'";
			}
			$ng .= ",\@Tf'.cui.'_" . join("", @$hor) . "_'." .
				join(".'_'.", @idx{@a1n}) .
				".'_all";
			if ($typ) {
				$ng .= "' : '')";
				$unquoted = 1;
			}
			else {
				$unquoted = 0;
			}
		}
		if (@a1n == 1) {
			$ng .= ($unquoted ? ".'" : '') . ",Tf_tok'";
		} elsif (!$unquoted) {
			$ng .= "'";
		}
		# Use substr to strip off the leading comma at the head of $ng
		$ng = "' nextgroup=" . substr($ng, 1);

		# Add to nextgroup the special transparent group whose purpose is to
		# prevent match of useless end toks within the region.

		# Build shared portion of syn region command.
		my $rgn_body = 
			"skip" .
			"\n$il\t\\.' keepend contains='.Tf_tok_group" .
			"\n$il\t\\.(b:txtfmt_cfg_escape != 'none'" .
			"\n$il\t\\\t? ','.Tf_esc_group" .
			"\n$il\t\\\t: '')" .
			"\n$il\t\\.' end=/['.b:txtfmt_re_any_stok_atom." .
			join(".", map { "b:txtfmt_re_${_}_etok_atom" } @a1n) .
			".']/me=e-'.tok_off.',he=e-'.tok_off" .
			"\n$il\t\\.$ng";

		# If shared body is about to be used in 2 region definitions (r1
		# r2), assign it to a variable for efficiency. (Many
		# concatenations are required to build the entire nextgroup
		# clause.)
		# Note: If there are no more regions to pull in, the shared stuff
		# will be used only once, so it's more efficient to build it
		# within the region definition itself.
		if (@a2n) {
			print "\n$il\" Cache the shared stuff";
			print "\n$il", "let rgn_body = ", $rgn_body;
			# Obviate the need for subsequent logic within this script to
			# know whether we're caching nextgroup clause or not
			$rgn_body = 'rgn_body';
		}

		# Define the rgn that is begun with an stok
		print "\n$il\" Define region that is begun by a start token";
		# Save the beginning of the `syn region' statement, which is
		# common to both the region begun by start tok and the region
		# begun by end tok. (Note that they diverge where there *once*
		# was - but no longer is - an `_rtd' in the latter's region name.)
		my $rgn_head = "\n$il" .
			"exe 'syn region " .
			"Tf'.cui.'_" . join("", @a1n) . "_'." . join(".'_'.", @idx{@a1n});
		print "$rgn_head",
			"\n$il\t\\.' start=/'.ch$idx{$a1n[-1]}.'/'",
			"\n$il\t\\.", $rgn_body,
			"\n$il\t\\.",
			# TODO: containedin_def only for single regions!!!
			@a1n == 1 ? "containedin_def" : "' contained'";

		# Define the region introduced by 'no rgn' token (if it exists)
		if (@a2n) {
			# Ensure that we don't define the region if all of the <typ>'s
			# whose end token could begin the region are inactive. If at
			# least one of these <typ>'s is active, the region will be
			# defined, and <typ>_enabled ternaries will be used as
			# necessary to ensure that we don't consider end tokens for
			# inactive region types.
			if ($need_r2_guard) {
				print "\n$il", '" Define the following region if and only if at least one of the';
				print "\n$il", '" region types whose end token could begin this region is active';
				print "\n$il", '"============= BEGIN NON-INDENTING BLOCK =============';
				print "\n$il", "if ",
					join ' || ', map { "${_}_enabled" } @a2n
				;
			}
			print "\n$il\" Define region that is begun by an end token";
			print "\n$il\" (when permitted by a nextgroup)";
			print "$rgn_head",
				"\n$il\t\\.", $rgn_body,
				"\n$il\t\\.' start=/['.",
				join(".", map {
					# Ternary redundant when inside single-region type guard.
					# E.g., no need for "bgc_enabled ? ..." inside "if bgc_enabled"
					$_ =~ 'bgc|clr' && @a2n > 1
						? "(${_}_enabled ? b:txtfmt_re_${_}_etok_atom : '')"
						: "b:txtfmt_re_${_}_etok_atom"
				} @a2n),
				".']/'",
				"\n$il\t\\.' contained'";

			if ($need_r2_guard) {
				print "\n$il", "endif \" ",
					join ' || ', map { "${_}_enabled" } @a2n;
				print "\n$il", '"=============  END NON-INDENTING BLOCK  =============';
			}
		}
		# Define the highlighting region
		print "\n$il\" Define highlighting for this region";
		print "\n$il\" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is";
		print "\n$il\" handled correctly in a cterm.";
		print "\n$il\"\t:help cterm-colors";
		print "\n$il", "exe 'hi Tf'.cui.'_", join("", @a1n), "_'.",
			join(".'_'.", @idx{@a1n}),
			"\n$il\t\\.",
			join(".", map {
				"eq_$_.b:txtfmt_$rhs{$_}\{$idx{$_}\}"
				} sort { $ord{$a} <=> $ord{$b} } @a1n
			)
		;

		# RECURSE
		# Call ourself recursively to handle the next level
		do_lvl(\@a1n, \@a2n);

		# POST RECURSION
		# Update for next iteration
		my $idx = $idx{$a1n[-1]};
		if ($a1n[-1] eq 'fmt') {
			print "\n$il", "let $idx = $idx + 1";
		} else {
			print "\n$il", "let ${idx}p = ${idx}p + 1";
		}
		# Strip a level of indent
		chop $il;
		print "\n$il", "endwhile";
		# Handle departure from blocks corresponding to <typ>'s that can be
		# disabled
		if (my ($typ) = grep { $_ eq $a1n[-1] } @can_dsbl) {
			if ($typ eq 'bgc') {
				# Revert to toplevel (no bgc) matchgroup
				# Note: Code emitted won't be reached if bgc_enabled is false
				print "\n$il", '" Revert to toplevel (no background color) matchgroup';
				print "\n$il", "let Tf_tok_group = Tf_top_tok_group";
				print "\n$il", "if b:txtfmt_cfg_escape != 'none'";
				print "\n\t$il", "let Tf_esc_group = Tf_top_esc_group";
				print "\n$il", "endif";
			}
			print "\n$il", "endif \" ${typ}_enabled";
			print "\n$il", '"=============  END NON-INDENTING BLOCK  =============';
		}
	}
}

# Top level recursion
# When the following call returns, all of the loops will have been output
print "\t\" BEGIN AUTOGENERATED CODE BLOCK ", "<<<";
print "\n\t\" Last update: ", scalar(localtime), "\n";
do_lvl([], \@rgn);
print "\n\t\" END AUTOGENERATED CODE BLOCK ", ">>>";

__END__
	" vim: sw=4 ts=4 foldmethod=marker foldmarker=<<<,>>> :
