" Txtfmt: Set of Vim plugins (syntax, ftplugin, plugin) for creating and
" displaying formatted text with Vim.
" File: This is the txtfmt syntax file
" Creation:	2004 Nov 06
" Last Change: 2009 Feb 21
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
" Function: s:Is_match_offset_char_based() <<<
" Purpose: Return nonzero if and only if the this version of Vim treats match
" offsets as character offsets.
" Assumption: Current encoding is multi-byte
fu! s:Is_match_offset_char_based()
	let s = "AB" . nr2char(0x100) . 'C'
	" Set lazyredraw to ensure user never sees the buffer we create
	let lazyredraw_save = &lazyredraw
	set lazyredraw
	" Save the old buffer number
	let buf_nr = bufnr('%')
	" Create a scratch buffer
	new
	set buftype=nofile
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
	" Delete the scratch buffer and be sure we return to the txtfmt buffer
	bd
	" This is probably unnecessary, but safe...
	exe 'buffer ' . buf_nr
	" Restore old lazyredraw setting
	if !lazyredraw_save
		set nolazyredraw
	endif
	" Return true if and only if offsets are char-based
	return off_is_char
endfu
" >>>
" Function: s:Get_bytes_per_token()
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
" Function: s:Define_syntax() <<<
fu! s:Define_syntax()
	" Concealment group <<<
	" Create a concealment highlight group, to which others can link
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
	if has('gui_running')
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
	" 'skip' pattern (option dependent) <<<
	if b:txtfmt_cfg_escape != 'none'
		" Make sure tokens that are part of an escape sequence cannot end a
		" region.
		" TODO - Decide whether the inner_esc regions obviate the need for the
		" skip-def.
		if b:txtfmt_cfg_escape == 'bslash'
			" TODO - Decide on allowing newlines as tokens (\_. or . ?)
			let skip_any_def = ' skip=/\\./'
			let skip_clr_def = ' skip=/\\./'
			let skip_fmt_def = ' skip=/\\./'
		else "if b:txtfmt_cfg_escape == 'self'
			let skip_any_def = ' skip=/\('.b:txtfmt_re_any_tok_atom.'\)\1/'
			let skip_clr_def = ' skip=/\('
					\.b:txtfmt_re_any_stok_atom.'\|'.b:txtfmt_re_clr_etok_atom.'\)\1/'
			let skip_fmt_def = ' skip=/\('
					\.b:txtfmt_re_any_stok_atom.'\|'.b:txtfmt_re_fmt_etok_atom.'\)\1/'
		endif
	else
		let skip_any_def = ''
		let skip_clr_def = ''
		let skip_fmt_def = ''
	endif
	" >>>
	" Define match offset that corresponds to a single txtfmt token <<<
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
	elseif s:Is_match_offset_char_based()
		let tok_off = 1
		let b:txtfmt_dbg_syn_off = 'char'
	else
		" Offsets are measured in bytes; hence, we need to determine how many
		" bytes per token
		let tok_off = s:Get_bytes_per_token()
		let b:txtfmt_dbg_syn_off = 'byte'
	endif
	" >>>
	" 'contains' list (option dependent) <<<
	" Note: I'm intentionally keeping this out of the 'if' for skip_def to
	" keep the logic compartmentalized.
	if b:txtfmt_cfg_escape != 'none'
		" Permit txtfmt regions to contain specially highlighted pairs of
		" escape-escapee tokens.
		let contains_any_def = ' contains=Tf_any_inner_esc'
		let contains_clr_def = ' contains=Tf_clr_inner_esc'
		let contains_fmt_def = ' contains=Tf_fmt_inner_esc'
	else
		let contains_any_def = ''
		let contains_clr_def = ''
		let contains_fmt_def = ''
	endif
	" >>>
	" 'containedin' list (option dependent) <<<
	if b:txtfmt_cfg_nested
		" Ensure that txtfmt top-level item can be contained by a non-txtfmt
		" syntax group (e.g. C-language comment).
		" TODO - Perhaps put inner_esc groups into a cluster.
		if b:txtfmt_cfg_escape != 'none'
			let containedin_def = ' containedin=ALLBUT,@Tf_all,Tf_def_tok'
						\.',Tf_outer_esc,Tf_any_inner_esc,Tf_clr_inner_esc,Tf_fmt_inner_esc'
		else
			let containedin_def = ' containedin=ALLBUT,@Tf_all,Tf_def_tok'
		endif
	else
		let containedin_def = ''
	endif
	" >>>
	" Define special default token group <<<
	" This group ensures that 'no clr' and 'no fmt' tokens at top level
	" (including those that end a clr only or fmt only group) will be
	" concealed.
	" Order note: This region is defined here to ensure that it has a lower
	" priority than any of the other txtfmt regions that can be begun by a
	" default token.
	exe 'syn match Tf_def_tok /'.b:txtfmt_re_any_etok_atom.'/ contained'
	hi link Tf_def_tok Tf_conceal
	" >>>
	" Choose cterm or gui versions of color and format assignments
	if has('gui_running')
		let clr_eq = ' guifg='
		let fmt_eq = ' gui='
	else
		let clr_eq = ' ctermfg='
		let fmt_eq = ' cterm='
	endif
	" Loop over the clr only regions <<<
	let i = 1
	let ch = nr2char(b:txtfmt_clr_first_tok + 1)
	while i < b:txtfmt_num_colors
		" clr{i} region (top level) <<<
		" Add to appropriate clusters
		exe 'syn cluster Tf_all add=Tf_clr_'.i
		exe 'syn cluster Tf_clr_all add=Tf_clr_'.i
		" Define nextgroup, which is common to all definitions of this group
		let nextgroup_def = ' nextgroup=@Tf_clrfmt_'.i.'_all,@Tf_clr_all,Tf_def_tok'
		" Define a clr region that is not contained and is introduced by a clr
		" token. This one can match at top level. If txtfmt 'nested' option is
		" set, it can also match within non-txtfmt regions.
		" Order note: This definition *must* occur prior to the combined
		" fmtclr regions that begin with this fmt token.
		exe 'syn region Tf_clr_'.i.' matchgroup=Tf_conceal start=/'.ch.'/'
			\.skip_clr_def
			\.' end=/'.b:txtfmt_re_any_stok_atom.'/me=e-'.tok_off.' '
			\.' end=/'.b:txtfmt_re_clr_etok_atom.'/me=e-'.tok_off.' '
			\.nextgroup_def
			\.contains_clr_def
			\.containedin_def
		" Define a 'contained' clr region that is introduced by the 'no fmt'
		" token when permitted by a 'nextgroup'
		" NOTE: The reason we don't have to worry about a 'no fmt' token in a
		" clr only region starting a new clr region is that the 'no fmt' token
		" can't end a clr only region. (Reword)
		exe 'syn region Tf_clr_'.i.' matchgroup=Tf_conceal'
			\.' start=/'.nr2char(b:txtfmt_fmt_first_tok).'/'
			\.skip_clr_def
			\.' end=/'.b:txtfmt_re_any_stok_atom.'/me=e-'.tok_off.' '
			\.' end=/'.b:txtfmt_re_clr_etok_atom.'/me=e-'.tok_off.' '
			\.nextgroup_def
			\.contains_clr_def
			\.' contained'
		" Define highlighting for current format region
		exe 'hi Tf_clr_'.i.clr_eq.b:txtfmt_clr{i-1}
		" >>>
		" Update for next iteration
		let i = i + 1
		let ch = nr2char(b:txtfmt_clr_first_tok + i)
	endwhile
	" >>>
	" Loop over the fmt only regions <<<
	let i = 1
	let ch = nr2char(b:txtfmt_fmt_first_tok + 1)
	while i < b:txtfmt_num_formats
		" fmt{i} region (top level) <<<
		" Add to appropriate clusters
		exe 'syn cluster Tf_all add=Tf_fmt_'.i
		exe 'syn cluster Tf_fmt_all add=Tf_fmt_'.i
		" Define nextgroup, which is common to all definitions of this group
		let nextgroup_def = ' nextgroup=@Tf_fmtclr_'.i.'_all,@Tf_fmt_all,Tf_def_tok'
		" Define a fmt region that is not contained and is introduced by a fmt
		" token. This one can match at top level. If txtfmt 'nested' option is
		" set, it can also match within non-txtfmt regions.
		" Order note: This definition *must* occur prior to the combined
		" clrfmt regions that begin with this fmt token.
		exe 'syn region Tf_fmt_'.i.' matchgroup=Tf_conceal start=/'.ch.'/'
			\.skip_fmt_def
			\.' end=/'.b:txtfmt_re_any_stok_atom.'/me=e-'.tok_off.' '
			\.' end=/'.b:txtfmt_re_fmt_etok_atom.'/me=e-'.tok_off.' '
			\.nextgroup_def
			\.contains_fmt_def
			\.containedin_def
		" Define a 'contained' fmt region that is introduced by the 'no clr'
		" token when permitted by a 'nextgroup'
		exe 'syn region Tf_fmt_'.i.' matchgroup=Tf_conceal'
			\.' start=/'.nr2char(b:txtfmt_clr_first_tok).'/'
			\.skip_fmt_def
			\.' end=/'.b:txtfmt_re_any_stok_atom.'/me=e-'.tok_off.' '
			\.' end=/'.b:txtfmt_re_fmt_etok_atom.'/me=e-'.tok_off.' '
			\.nextgroup_def
			\.contains_fmt_def
			\.' contained'
		" Define highlighting for current format region
		exe 'hi Tf_fmt_'.i.fmt_eq.b:txtfmt_fmt{i}
		" >>>
		" Update for next iteration
		let i = i + 1
		let ch = nr2char(b:txtfmt_fmt_first_tok + i)
	endwhile
	" >>>
	" Loop over the clrfmt regions <<<
	let i = 1
	while i < b:txtfmt_num_colors
		" clr{i} -- fmt{j} <<<
		let j = 1
		let ch = nr2char(b:txtfmt_fmt_first_tok + 1)
		while j < b:txtfmt_num_formats
			" Add to appropriate clusters
			exe 'syn cluster Tf_all add=Tf_clrfmt_'.i.'_'.j
			exe 'syn cluster Tf_clrfmt_'.i.'_all add=Tf_clrfmt_'.i.'_'.j
			let nextgroup_def =
				\' nextgroup=Tf_clr_'.i.',Tf_fmt_'.j.',@Tf_clrfmt_'.i.'_all,@Tf_fmtclr_'.j.'_all'
			exe 'syn region Tf_clrfmt_'.i.'_'.j.' matchgroup=Tf_conceal'
				\.' start=/'.ch.'/'.skip_any_def
				\.' end=/'.b:txtfmt_re_any_tok_atom.'/me=e-'.tok_off
				\.nextgroup_def
				\.contains_any_def
				\.' contained'
			" Define the highlighting for this clr-fmt region
			" Important Note: It is important to put the format definition
			" *after* the color definition, since in a cterm, the bold
			" attribute is ignored if the color name is "Dark" something and
			" the cterm=bold comes before the ctermfg=<color>. (See help on
			" cterm-colors.)
			exe 'hi Tf_clrfmt_'.i.'_'.j.clr_eq.b:txtfmt_clr{i-1}
				\.fmt_eq.b:txtfmt_fmt{j}
			" Update for next iteration
			let j = j + 1
			let ch = nr2char(b:txtfmt_fmt_first_tok + j)
		endwhile
		" >>>
		" Update for next iteration <<<
		let i = i + 1
		" >>>
	endwhile
	" >>>
	" Loop over the fmtclr regions <<<
	let i = 1
	while i < b:txtfmt_num_formats
		" fmt{i} -- clr{j} <<<
		let j = 1
		let ch = nr2char(b:txtfmt_clr_first_tok + 1)
		while j < b:txtfmt_num_colors
			" Add to appropriate clusters
			exe 'syn cluster Tf_all add=Tf_fmtclr_'.i.'_'.j
			exe 'syn cluster Tf_fmtclr_'.i.'_all add=Tf_fmtclr_'.i.'_'.j
			let nextgroup_def =
				\' nextgroup=Tf_fmt_'.i.',Tf_clr_'.j.',@Tf_fmtclr_'.i.'_all,@Tf_clrfmt_'.j.'_all'
			exe 'syn region Tf_fmtclr_'.i.'_'.j.' matchgroup=Tf_conceal'
				\.' start=/'.ch.'/'.skip_any_def
				\.' end=/'.b:txtfmt_re_any_tok_atom.'/me=e-'.tok_off
				\.nextgroup_def
				\.contains_any_def
				\.' contained'
			" Define the highlighting for this fmt-clr region
			" Important Note: Format attribute must be defined *after* color
			" attribute. See note under corresponding definition in the clrfmt
			" block for explanation. 
			exe 'hi Tf_fmtclr_'.i.'_'.j.clr_eq.b:txtfmt_clr{j-1}
				\.fmt_eq.b:txtfmt_fmt{i}
			" Update for next iteration
			let j = j + 1
			let ch = nr2char(b:txtfmt_clr_first_tok + j)
		endwhile
		" >>>
		" Update for next iteration <<<
		let i = i + 1
		" >>>
	endwhile
	" >>>
	" Handle escape/escapee token pairs both inside and outside of fmt/clr
	" regions.
	" Important Note: These groups must be defined after the fmt/clr regions,
	" since they must take precedence over them.
	" Objectives:
	" -Conceal the escape token
	" -Prevent escaped or escaping tokens from beginning a region
	" -Prevent escaped or escaping tokens from ending a region
	" Note: Must take into account the txtfmt 'escape' option
	" Also note: Must take into account the size in bytes of the escape char
	" if this Vim treats offsets as byte offsets
	if b:txtfmt_cfg_escape == 'bslash' || b:txtfmt_cfg_escape == 'self'
		if b:txtfmt_cfg_escape == 'self'
			let re_outer_esc_pair = '\('.b:txtfmt_re_any_stok_atom.'\)\1'
			let re_any_esc_pair = '\('.b:txtfmt_re_any_tok_atom.'\)\1'
			let re_fmt_esc_pair = '\('
				\.b:txtfmt_re_any_stok_atom.'\|'.b:txtfmt_re_fmt_etok_atom.'\)\1'
			let re_clr_esc_pair = '\('
				\.b:txtfmt_re_any_stok_atom.'\|'.b:txtfmt_re_clr_etok_atom.'\)\1'
			" Escape char is same number of bytes as a token
			let esc_off = tok_off
		elseif b:txtfmt_cfg_escape == 'bslash'
			let re_outer_esc_pair = '\\'.b:txtfmt_re_any_stok_atom
			let re_any_esc_pair = '\\'.b:txtfmt_re_any_tok_atom
			let re_fmt_esc_pair = '\\'.b:txtfmt_re_fmt_tok_atom
			let re_clr_esc_pair = '\\'.b:txtfmt_re_clr_tok_atom
			" Escape char is single byte
			let esc_off = 1
		endif
		" The following group prevents escaping or escaped token from starting
		" a region, and causes the escaping token to be hidden
		exe 'syn match Tf_outer_esc /'.re_outer_esc_pair.'/he=s+'.esc_off.containedin_def
		" The following group allows escaping tokens to be hidden within a fmt/clr
		" region.
		exe 'syn match Tf_any_inner_esc /'.re_any_esc_pair.'/he=s+'.esc_off.' contains=NONE'
			\.' contained'
		exe 'syn match Tf_fmt_inner_esc /'.re_fmt_esc_pair.'/he=s+'.esc_off.' contains=NONE'
			\.' contained'
		exe 'syn match Tf_clr_inner_esc /'.re_clr_esc_pair.'/he=s+'.esc_off.' contains=NONE'
			\.' contained'
		hi link Tf_outer_esc Tf_conceal
		hi link Tf_any_inner_esc Tf_conceal
		hi link Tf_fmt_inner_esc Tf_conceal
		hi link Tf_clr_inner_esc Tf_conceal
	endif
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
	" TODO - Get rid of the function code after the following return prior to
	" release.
	return
	" SYNTAX SYNCHRONIZATION using regions correctly but with a flaw <<<
	" NOTE: This is the most promising method other than 'minlines'. The only
	" problem is, there's no good way to handle the case of txtfmt regions
	" intermixed with other types of regions. Note that a problem exists both
	" for the nested and nonested cases:
	" 'nested' problem scenario: We synchronize to a nested txtfmt region.
	" When the nested txtfmt region ends, the containing region is not
	" resumed. (If we had used 'minlines', the presumption is we would have
	" gone back far enough to parse the containing region.)
	" 'nonested' problem scenario: We incorrectly synchronize to a nested
	" txtfmt region because, not knowing anything about the non-txtfmt region
	" definitions, we have no way of knowing when we are within one.
	" TODO - If I decide to dust this off and use it, I need to look at the
	" handling of <no_fmt> and <no_clr> regions.
	if 0
	" 'skip' pattern (option dependent) <<<
	if b:txtfmt_cfg_escape != 'none'
		" Make sure tokens that are part of an escape sequence cannot end a
		" region.
		if b:txtfmt_cfg_escape == 'bslash'
			" TODO - Decide on allowing newlines as tokens (\_. or . ?)
			let skip_def = ' skip=/\\./'
		elseif b:txtfmt_cfg_escape == 'self'
			let skip_def = ' skip=/\('.b:txtfmt_re_any_tok_atom.'\)\1/'
		else
			let skip_def = ''
		endif
	else
		let skip_def = ''
	endif
	" >>>
	" 'contains' list (option dependent) <<<
	" Note: I'm intentionally keeping this out of the 'if' for skip_def to
	" keep the logic compartmentalized.
	if b:txtfmt_cfg_escape != 'none'
		" Permit txtfmt regions to contain specially highlighted pairs of
		" escape-escapee tokens.
		let contains_def = ' contains=Tf_inner_esc'
	else
		let contains_def = ''
	endif
	" >>>
	" 'containedin' list (option dependent) <<<
	if b:txtfmt_cfg_nested
		" Ensure that txtfmt top-level item can be contained by a non-txtfmt
		" syntax group (e.g. C-language comment).
		if b:txtfmt_cfg_escape != 'none'
			let containedin_def = ' containedin=ALLBUT,@Tf_all,Tf_outer_esc,Tf_inner_esc'
		else
			let containedin_def = ' containedin=ALLBUT,@Tf_all'
		endif
	else
		let containedin_def = ''
	endif
	" >>>
	" Loop over the clr only regions <<<
	let i = 1
	let ch = nr2char(b:txtfmt_clr_first_tok + 1)
	while i < b:txtfmt_num_colors
		" clr{i} region (top level) <<<
		" Add to appropriate clusters
		exe 'syn cluster Tf_all add=Tf_clr_'.i
		exe 'syn cluster Tf_clr_all add=Tf_clr_'.i
		" Define nextgroup, which is common to all definitions of this group
		let nextgroup_def = ' nextgroup=@Tf_clrfmt_'.i.'_all,@Tf_clr_all'
		" Define a clr region that is not contained and is introduced by a clr
		" token. This one can match at top level. If txtfmt 'nested' option is
		" set, it can also match within non-txtfmt regions.
		" Order note: This definition *must* occur prior to the combined
		" fmtclr regions that begin with this fmt token.
		exe 'syn region Tf_clr_'.i.' matchgroup=Ignore start=/'.ch.'/'
			\.skip_def
			\.' end=/'.b:txtfmt_re_any_tok_atom.'/me=e-1 '
			\.nextgroup_def
			\.contains_def
			\.containedin_def
		" Define a 'contained' clr region that is introduced by the 'no fmt'
		" token when permitted by a 'nextgroup'
		exe 'syn region Tf_clr_'.i.' matchgroup=Ignore'
			\.' start=/'.nr2char(b:txtfmt_fmt_first_tok).'/'
			\.skip_def
			\.' end=/'.b:txtfmt_re_any_tok_atom.'/me=e-1 '
			\.nextgroup_def
			\.contains_def
			\.' contains=Tfsm_clr_'.i
			\.' contained'
		exe 'syn sync match Tfsm_clr_'.i.' contained grouphere Tf_clr_'.i.' /./'
		" >>>
		" Update for next iteration
		let i = i + 1
		let ch = nr2char(b:txtfmt_clr_first_tok + i)
	endwhile
	" >>>
	" Loop over the fmt only regions <<<
	let i = 1
	let ch = nr2char(b:txtfmt_fmt_first_tok + 1)
	while i < b:txtfmt_num_formats
		" fmt{i} region (top level) <<<
		" Add to appropriate clusters
		exe 'syn cluster Tf_all add=Tf_fmt_'.i
		exe 'syn cluster Tf_fmt_all add=Tf_fmt_'.i
		" Define nextgroup, which is common to all definitions of this group
		let nextgroup_def = ' nextgroup=@Tf_fmtclr_'.i.'_all,@Tf_fmt_all'
		" Define a fmt region that is not contained and is introduced by a fmt
		" token. This one can match at top level. If txtfmt 'nested' option is
		" set, it can also match within non-txtfmt regions.
		" Order note: This definition *must* occur prior to the combined
		" clrfmt regions that begin with this fmt token.
		exe 'syn region Tf_fmt_'.i.' matchgroup=Ignore start=/'.ch.'/'
			\.skip_def
			\.' end=/'.b:txtfmt_re_any_tok_atom.'/me=e-1 '
			\.nextgroup_def
			\.contains_def
			\.containedin_def
		" Define a 'contained' fmt region that is introduced by the 'no clr'
		" token when permitted by a 'nextgroup'
		exe 'syn region Tf_fmt_'.i.' matchgroup=Ignore'
			\.' start=/'.nr2char(b:txtfmt_clr_first_tok).'/'
			\.skip_def
			\.' end=/'.b:txtfmt_re_any_tok_atom.'/me=e-1'
			\.nextgroup_def
			\.contains_def
			\.' contained'
			\.' contains=Tfsm_fmt_'.i
		exe 'syn sync match Tfsm_fmt_'.i.' contained grouphere Tf_fmt_'.i.' /./'
		" >>>
		" Update for next iteration
		let i = i + 1
		let ch = nr2char(b:txtfmt_fmt_first_tok + i)
	endwhile
	" >>>
	" Loop over the clrfmt regions <<<
	let i = 1
	while i < b:txtfmt_num_colors
		" clr{i} -- fmt{j} <<<
		let j = 1
		let ch = nr2char(b:txtfmt_fmt_first_tok + 1)
		while j < b:txtfmt_num_formats
			" Add to appropriate clusters
			exe 'syn cluster Tf_all add=Tf_clrfmt_'.i.'_'.j
			exe 'syn cluster Tf_clrfmt_'.i.'_all add=Tf_clrfmt_'.i.'_'.j
			let nextgroup_def =
				\' nextgroup=@Tf_clr_'.i.',@Tf_fmt_'.j.',@Tf_clrfmt_'.i.'_all,@Tf_fmtclr_'.j.'_all'
			exe 'syn region Tf_clrfmt_'.i.'_'.j.' matchgroup=Ignore'
				\.' start=/'.ch.'/'.skip_def
				\.' end=/'.b:txtfmt_re_any_tok_atom.'/me=e-1'
				\.nextgroup_def
				\.contains_def
				\.' contained'
				\.' contains='.'Tfsm_clrfmt_'.i.'_'.j
			exe 'syn sync match Tfsm_clrfmt_'.i.'_'.j.' contained grouphere Tf_clrfmt_'.i.'_'.j.' /./'
			" Update for next iteration
			let j = j + 1
			let ch = nr2char(b:txtfmt_fmt_first_tok + j)
		endwhile
		" >>>
		" Update for next iteration <<<
		let i = i + 1
		" >>>
	endwhile
	" >>>
	" Loop over the fmtclr regions <<<
	let i = 1
	while i < b:txtfmt_num_formats
		" fmt{i} -- clr{j} <<<
		let j = 1
		let ch = nr2char(b:txtfmt_clr_first_tok + 1)
		while j < b:txtfmt_num_colors
			" Add to appropriate clusters
			exe 'syn cluster Tf_all add=Tf_fmtclr_'.i.'_'.j
			exe 'syn cluster Tf_fmtclr_'.i.'_all add=Tf_fmtclr_'.i.'_'.j
			let nextgroup_def =
				\' nextgroup=@Tf_fmt_'.i.',@Tf_clr_'.j.',@Tf_fmtclr_'.i.'_all,@Tf_clrfmt_'.j.'_all'
			exe 'syn region Tf_fmtclr_'.i.'_'.j.' matchgroup=Ignore'
				\.' start=/'.ch.'/'.skip_def
				\.' end=/'.b:txtfmt_re_any_tok_atom.'/me=e-1'
				\.nextgroup_def
				\.contains_def
				\.' contained'
				\.' contains='.'Tfsm_fmtclr_'.i.'_'.j
			exe 'syn sync match Tfsm_fmtclr_'.i.'_'.j.' contained grouphere Tf_fmtclr_'.i.'_'.j.' /./'
			" Update for next iteration
			let j = j + 1
			let ch = nr2char(b:txtfmt_clr_first_tok + j)
		endwhile
		" >>>
		" Update for next iteration <<<
		let i = i + 1
		" >>>
	endwhile
	" >>>
	endif
	" >>>
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
			\substitute(b:txtfmt_syntax,
			\'\(.\{-}\%(^\|\.\)txtfmt\%(\.\|$\)\).*', '\1', '')
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
