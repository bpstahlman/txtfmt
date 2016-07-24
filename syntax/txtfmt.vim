" Txtfmt: Set of Vim plugins (syntax, ftplugin, plugin) for creating and
" displaying formatted text with Vim.
" File: This is the txtfmt syntax file
" Creation:	2004 Nov 06
" Last Change: 2016 Apr 28
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
" Function: s:Define_syntax() <<<
fu! s:Define_syntax()
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
	
	" Note: Originally, the skip patterns were assigned to region-specific
	" vars in the generated code; however, there was no need for this, as the
	" patterns were invariable, depending only on the txtfmt 'escape' option.
	" Actually, at one time, I believe the patterns were actually region
	" specific, but the simple, non-region-specific patterns are simple and
	" efficient.
	if b:txtfmt_cfg_escape != 'none'
		if b:txtfmt_cfg_escape == 'bslash'
			let skip = ' skip=/\\./'
		else
			let skip = ' skip=/\(.\)\1/'
		endif
	else
		let skip = ''
	endif
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
	" Check for existence of 'conceal' patch (and desire on part of user to
	" use it)
	if b:txtfmt_cfg_conceal
		" Note: 'conceallevel' and 'concealcursor' are window-local
		setl conceallevel=3
		let &l:concealcursor = b:txtfmt_cfg_concealcursor
		let conceal = ' conceal'
	else
		let conceal = ''
	endif
	" Create vars to facilitate switching between normal (toplevel)
	" concealment group and background-color-specific groups
	" TODO: Consider merging this with the if b:txtfmt_cfg_conceal block (above)
	if b:txtfmt_cfg_conceal
		" Note: User has choice about 'conceal', but not 'transparent'.
		exe 'syn match Tf_conceal /'.b:txtfmt_re_any_tok.'/ contained transparent contains=NONE'.conceal
	else
		" NMG_TODO: Under construction... Do we need contains=NONE (given that
		" other groups will probably include this in ALLBUT clause)?
		exe 'syn match Tf_conceal /'.b:txtfmt_re_any_tok.'/ contained contains=NONE'.conceal
	endif
	let Tf_top_tok_group = 'Tf_conceal'
	let Tf_tok_group = 'Tf_conceal'
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
		" syntax group (e.g. C-language comment).
		" TODO - Perhaps put inner_esc groups into a cluster.
		if b:txtfmt_cfg_escape != 'none'
			let containedin_def = ' containedin=ALLBUT,@Tf'.cui.'_all'
						\.',Tf_outer_esc,Tf_any_stok_inner_esc'
						\.(clr_enabled ? ',Tf_clr_etok_inner_esc' : '')
						\.(bgc_enabled ? ',Tf_bgc_etok_inner_esc' : '').',Tf_fmt_etok_inner_esc'
		else
			let containedin_def = ' containedin=ALLBUT,@Tf'.cui.'_all'
		endif
		" NMG_TODO: Don't like using Tf.*conceal.* pattern for the conceal groups. Better way?
		let containedin_def .= b:txtfmt_cfg_conceal ? ',Tf_conceal' : ',Tf.*conceal.*'
	else
		let containedin_def = ''
	endif
	" >>>
	" Choose cterm or gui versions of color and format assignments
	if use_gui_defs
		let eq_clr = ' guifg='
		let eq_bgc = ' guibg='
		let eq_fmt = ' gui='
	else
		let eq_clr = ' ctermfg='
		let eq_bgc = ' ctermbg='
		let eq_fmt = ' cterm='
	endif
	" NOTES TO PRESERVE
	" TODO - Decide whether the inner_esc regions obviate the need for the
	" skip-def. (I don't think so...)
	" UNDER CONSTRUCTION

	" Create background-color specific concealment groups
	" Note: Use cui to ensure that different buffers could have different sets
	" of background colors in effect
	" Loop over active colors only (with the aid of index indirection array)
	let pi = 1
	while pi <= (b:txtfmt_cfg_bgcolor ? b:txtfmt_cfg_numbgcolors : 0)
		let i = b:txtfmt_cfg_bgcolor{pi}
		exe 'hi Tf'.cui.'_conceal_'.i.' '.eq_bgc.b:txtfmt_bgc{i}.eq_clr.b:txtfmt_bgc{i}
		let pi = pi + 1
	endwhile

	" Important Note: The following line may be executed on the Vim command
	" line to regenerate the code within the BEGIN...<<< / END...>>> markers.
	" The script generating the code is a perl script appended to the end of
	" this file: specifically, between the `#!perl' and the __END__ marker.
	" :0/BEGIN AUTOGENERATED CODE BLOCK <\{3}/;/END AUTOGENERATED CODE BLOCK >\{3}/d|exe '.-1r !perl -x %'|exe "norm '["

	" BEGIN AUTOGENERATED CODE BLOCK <<<
	" Last update: Sat Jul 23 20:02:38 2016

	"============= BEGIN NON-INDENTING BLOCK =============
	if clr_enabled
	"===
	"*** clr
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_clr=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_clr_etok_inner_esc'
	else
		let contains_clr = ''
	endif
	let end_clr =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_clr = skip
		\.contains_clr
		\.end_clr
		\.containedin_def
	let start_clr =
		\' start=/['
		\.(bgc_enabled ? b:txtfmt_re_bgc_etok_atom : '').b:txtfmt_re_fmt_etok_atom
		\.']/'
	let r2_clr = skip
		\.contains_clr
		\.start_clr
		\.end_clr
		\.' contained'
	"============= BEGIN NON-INDENTING BLOCK =============
	if bgc_enabled
	"===
	"*** clr-bgc
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_clrbgc=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_clr_etok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc'
	else
		let contains_clrbgc = ''
	endif
	let end_clrbgc =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_clrbgc = skip
		\.contains_clrbgc
		\.end_clrbgc
		\.' contained'
	let start_clrbgc =
		\' start=/['
		\.b:txtfmt_re_fmt_etok_atom
		\.']/'
	let r2_clrbgc = skip
		\.contains_clrbgc
		\.start_clrbgc
		\.end_clrbgc
		\.' contained'
	"===
	"*** clr-bgc-fmt
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_clrbgcfmt=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_clr_etok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc'
	else
		let contains_clrbgcfmt = ''
	endif
	let end_clrbgcfmt =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_clrbgcfmt = skip
		\.contains_clrbgcfmt
		\.end_clrbgcfmt
		\.' contained'
	" Revert to toplevel (no background color) matchgroup
	endif " bgc_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"===
	"*** clr-fmt
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_clrfmt=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_clr_etok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc'
	else
		let contains_clrfmt = ''
	endif
	let end_clrfmt =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_clrfmt = skip
		\.contains_clrfmt
		\.end_clrfmt
		\.' contained'
	" Define the r2 region vars if and only if at least one of the
	" region types whose end token could begin this region is active
	"============= BEGIN NON-INDENTING BLOCK =============
	if bgc_enabled
	let start_clrfmt =
		\' start=/['
		\.b:txtfmt_re_bgc_etok_atom
		\.']/'
	let r2_clrfmt = skip
		\.contains_clrfmt
		\.start_clrfmt
		\.end_clrfmt
		\.' contained'
	endif " bgc_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"============= BEGIN NON-INDENTING BLOCK =============
	if bgc_enabled
	"===
	"*** clr-fmt-bgc
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_clrfmtbgc=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_clr_etok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc'
	else
		let contains_clrfmtbgc = ''
	endif
	let end_clrfmtbgc =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_clrfmtbgc = skip
		\.contains_clrfmtbgc
		\.end_clrfmtbgc
		\.' contained'
	" Revert to toplevel (no background color) matchgroup
	endif " bgc_enabled
	"=============  END NON-INDENTING BLOCK  =============
	endif " clr_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"============= BEGIN NON-INDENTING BLOCK =============
	if bgc_enabled
	"===
	"*** bgc
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_bgc=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc'
	else
		let contains_bgc = ''
	endif
	let end_bgc =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_bgc = skip
		\.contains_bgc
		\.end_bgc
		\.containedin_def
	let start_bgc =
		\' start=/['
		\.(clr_enabled ? b:txtfmt_re_clr_etok_atom : '').b:txtfmt_re_fmt_etok_atom
		\.']/'
	let r2_bgc = skip
		\.contains_bgc
		\.start_bgc
		\.end_bgc
		\.' contained'
	"============= BEGIN NON-INDENTING BLOCK =============
	if clr_enabled
	"===
	"*** bgc-clr
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_bgcclr=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc,'
			\.'Tf_clr_etok_inner_esc'
	else
		let contains_bgcclr = ''
	endif
	let end_bgcclr =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_bgcclr = skip
		\.contains_bgcclr
		\.end_bgcclr
		\.' contained'
	let start_bgcclr =
		\' start=/['
		\.b:txtfmt_re_fmt_etok_atom
		\.']/'
	let r2_bgcclr = skip
		\.contains_bgcclr
		\.start_bgcclr
		\.end_bgcclr
		\.' contained'
	"===
	"*** bgc-clr-fmt
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_bgcclrfmt=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc,'
			\.'Tf_clr_etok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc'
	else
		let contains_bgcclrfmt = ''
	endif
	let end_bgcclrfmt =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_bgcclrfmt = skip
		\.contains_bgcclrfmt
		\.end_bgcclrfmt
		\.' contained'
	endif " clr_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"===
	"*** bgc-fmt
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_bgcfmt=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc'
	else
		let contains_bgcfmt = ''
	endif
	let end_bgcfmt =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_bgcfmt = skip
		\.contains_bgcfmt
		\.end_bgcfmt
		\.' contained'
	" Define the r2 region vars if and only if at least one of the
	" region types whose end token could begin this region is active
	"============= BEGIN NON-INDENTING BLOCK =============
	if clr_enabled
	let start_bgcfmt =
		\' start=/['
		\.b:txtfmt_re_clr_etok_atom
		\.']/'
	let r2_bgcfmt = skip
		\.contains_bgcfmt
		\.start_bgcfmt
		\.end_bgcfmt
		\.' contained'
	endif " clr_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"============= BEGIN NON-INDENTING BLOCK =============
	if clr_enabled
	"===
	"*** bgc-fmt-clr
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_bgcfmtclr=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc,'
			\.'Tf_clr_etok_inner_esc'
	else
		let contains_bgcfmtclr = ''
	endif
	let end_bgcfmtclr =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_bgcfmtclr = skip
		\.contains_bgcfmtclr
		\.end_bgcfmtclr
		\.' contained'
	endif " clr_enabled
	"=============  END NON-INDENTING BLOCK  =============
	" Revert to toplevel (no background color) matchgroup
	endif " bgc_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"===
	"*** fmt
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_fmt=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc'
	else
		let contains_fmt = ''
	endif
	let end_fmt =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_fmt = skip
		\.contains_fmt
		\.end_fmt
		\.containedin_def
	" Define the r2 region vars if and only if at least one of the
	" region types whose end token could begin this region is active
	"============= BEGIN NON-INDENTING BLOCK =============
	if clr_enabled || bgc_enabled
	let start_fmt =
		\' start=/['
		\.(clr_enabled ? b:txtfmt_re_clr_etok_atom : '').(bgc_enabled ? b:txtfmt_re_bgc_etok_atom : '')
		\.']/'
	let r2_fmt = skip
		\.contains_fmt
		\.start_fmt
		\.end_fmt
		\.' contained'
	endif " clr_enabled || bgc_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"============= BEGIN NON-INDENTING BLOCK =============
	if clr_enabled
	"===
	"*** fmt-clr
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_fmtclr=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc,'
			\.'Tf_clr_etok_inner_esc'
	else
		let contains_fmtclr = ''
	endif
	let end_fmtclr =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_fmtclr = skip
		\.contains_fmtclr
		\.end_fmtclr
		\.' contained'
	" Define the r2 region vars if and only if at least one of the
	" region types whose end token could begin this region is active
	"============= BEGIN NON-INDENTING BLOCK =============
	if bgc_enabled
	let start_fmtclr =
		\' start=/['
		\.b:txtfmt_re_bgc_etok_atom
		\.']/'
	let r2_fmtclr = skip
		\.contains_fmtclr
		\.start_fmtclr
		\.end_fmtclr
		\.' contained'
	endif " bgc_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"============= BEGIN NON-INDENTING BLOCK =============
	if bgc_enabled
	"===
	"*** fmt-clr-bgc
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_fmtclrbgc=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc,'
			\.'Tf_clr_etok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc'
	else
		let contains_fmtclrbgc = ''
	endif
	let end_fmtclrbgc =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_fmtclrbgc = skip
		\.contains_fmtclrbgc
		\.end_fmtclrbgc
		\.' contained'
	" Revert to toplevel (no background color) matchgroup
	endif " bgc_enabled
	"=============  END NON-INDENTING BLOCK  =============
	endif " clr_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"============= BEGIN NON-INDENTING BLOCK =============
	if bgc_enabled
	"===
	"*** fmt-bgc
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_fmtbgc=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc'
	else
		let contains_fmtbgc = ''
	endif
	let end_fmtbgc =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_fmtbgc = skip
		\.contains_fmtbgc
		\.end_fmtbgc
		\.' contained'
	" Define the r2 region vars if and only if at least one of the
	" region types whose end token could begin this region is active
	"============= BEGIN NON-INDENTING BLOCK =============
	if clr_enabled
	let start_fmtbgc =
		\' start=/['
		\.b:txtfmt_re_clr_etok_atom
		\.']/'
	let r2_fmtbgc = skip
		\.contains_fmtbgc
		\.start_fmtbgc
		\.end_fmtbgc
		\.' contained'
	endif " clr_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"============= BEGIN NON-INDENTING BLOCK =============
	if clr_enabled
	"===
	"*** fmt-bgc-clr
	"===
	if b:txtfmt_cfg_escape != 'none'
		let contains_fmtbgcclr=
			\' contains='
			\.'Tf_any_stok_inner_esc,'
			\.'Tf_fmt_etok_inner_esc,'
			\.'Tf_bgc_etok_inner_esc,'
			\.'Tf_clr_etok_inner_esc'
	else
		let contains_fmtbgcclr = ''
	endif
	let end_fmtbgcclr =
		\' end=/['
		\.b:txtfmt_re_any_stok_atom
		\.b:txtfmt_re_fmt_etok_atom
		\.b:txtfmt_re_bgc_etok_atom
		\.b:txtfmt_re_clr_etok_atom
		\.']/me=e-'.tok_off.',he=e-'.tok_off
	let r1_fmtbgcclr = skip
		\.contains_fmtbgcclr
		\.end_fmtbgcclr
		\.' contained'
	endif " clr_enabled
	"=============  END NON-INDENTING BLOCK  =============
	" Revert to toplevel (no background color) matchgroup
	endif " bgc_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"============= BEGIN NON-INDENTING BLOCK =============
	if clr_enabled
	"===
	"*** Loop over clr levels
	"===
	let ip = 1
	while ip <= b:txtfmt_cfg_numfgcolors
		let i = b:txtfmt_cfg_fgcolor{ip}
		let chi = nr2char(b:txtfmt_clr_first_tok + i)
		" Add to appropriate clusters
		exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_clr_'.i
		exe 'syn cluster Tf'.cui.'_clr_all add=Tf'.cui.'_clr_'.i
		" Cache the nextgroup clause
		let ng = ' nextgroup=@Tf'.cui.'_clr_all'.(bgc_enabled ? ',@Tf'.cui.'_clrbgc_'.i.'_all' : '').',@Tf'.cui.'_clrfmt_'.i.'_all,Tf_conceal'
		" Define region that is begun by a start token
		exe 'syn region Tf'.cui.'_clr_'.i
			\.' start=/'.chi.'/'.r1_clr.' contains='.Tf_tok_group.ng
		" Define region that is begun by an end token
		" (when permitted by a nextgroup)
		exe 'syn region Tf'.cui.'_clr_'.i
			\.r2_clr.' contains='.Tf_tok_group.ng
		" Define highlighting for this region
		" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
		" handled correctly in a cterm.
		"	:help cterm-colors
		exe 'hi Tf'.cui.'_clr_'.i
			\.eq_clr.b:txtfmt_clr{i}
		"============= BEGIN NON-INDENTING BLOCK =============
		if bgc_enabled
		"===
		"*** Loop over clr-bgc levels
		"===
		let jp = 1
		while jp <= b:txtfmt_cfg_numbgcolors
			let j = b:txtfmt_cfg_bgcolor{jp}
			let chj = nr2char(b:txtfmt_bgc_first_tok + j)
			" Add to appropriate clusters
			exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_clrbgc_'.i.'_'.j
			exe 'syn cluster Tf'.cui.'_clrbgc_'.i.'_all add=Tf'.cui.'_clrbgc_'.i.'_'.j
			if !b:txtfmt_cfg_conceal
				" Ensure that this and higher order regions use bgc-specific concealment group
				let Tf_tok_group = 'Tf'.cui.'_conceal_'.j
				exe 'syn match Tf'.cui.'_conceal_'.j.' /'.b:txtfmt_re_any_tok.'/ contained contains=NONE'.conceal
			endif
			" Cache the nextgroup clause
			let ng = ' nextgroup=Tf'.cui.'_bgc_'.j.',Tf'.cui.'_clr_'.i.',@Tf'.cui.'_bgcclr_'.j.'_all,@Tf'.cui.'_clrbgc_'.i.'_all,@Tf'.cui.'_clrbgcfmt_'.i.'_'.j.'_all'
			" Define region that is begun by a start token
			exe 'syn region Tf'.cui.'_clrbgc_'.i.'_'.j
				\.' start=/'.chj.'/'.r1_clrbgc.' contains='.Tf_tok_group.ng
			" Define region that is begun by an end token
			" (when permitted by a nextgroup)
			exe 'syn region Tf'.cui.'_clrbgc_'.i.'_'.j
				\.r2_clrbgc.' contains='.Tf_tok_group.ng
			" Define highlighting for this region
			" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
			" handled correctly in a cterm.
			"	:help cterm-colors
			exe 'hi Tf'.cui.'_clrbgc_'.i.'_'.j
				\.eq_clr.b:txtfmt_clr{i}.eq_bgc.b:txtfmt_bgc{j}
			"===
			"*** Loop over clr-bgc-fmt levels
			"===
			let k = 1
			while k <= b:txtfmt_num_formats - 1
				let chk = nr2char(b:txtfmt_fmt_first_tok + k)
				" Add to appropriate clusters
				exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_clrbgcfmt_'.i.'_'.j.'_'.k
				exe 'syn cluster Tf'.cui.'_clrbgcfmt_'.i.'_'.j.'_all add=Tf'.cui.'_clrbgcfmt_'.i.'_'.j.'_'.k
				" Define region that is begun by a start token
				exe 'syn region Tf'.cui.'_clrbgcfmt_'.i.'_'.j.'_'.k
					\.' start=/'.chk.'/'.r1_clrbgcfmt.' contains='.Tf_tok_group.' nextgroup=Tf'.cui.'_bgcfmt_'.j.'_'.k.',Tf'.cui.'_clrfmt_'.i.'_'.k.',Tf'.cui.'_clrbgc_'.i.'_'.j.',@Tf'.cui.'_bgcfmtclr_'.j.'_'.k.'_all,@Tf'.cui.'_clrfmtbgc_'.i.'_'.k.'_all,@Tf'.cui.'_clrbgcfmt_'.i.'_'.j.'_all'
				" Define highlighting for this region
				" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
				" handled correctly in a cterm.
				"	:help cterm-colors
				exe 'hi Tf'.cui.'_clrbgcfmt_'.i.'_'.j.'_'.k
					\.eq_clr.b:txtfmt_clr{i}.eq_bgc.b:txtfmt_bgc{j}.eq_fmt.b:txtfmt_fmt{k}
				let k = k + 1
			endwhile
			let jp = jp + 1
		endwhile
		" Revert to toplevel (no background color) matchgroup
		let Tf_tok_group = Tf_top_tok_group
		endif " bgc_enabled
		"=============  END NON-INDENTING BLOCK  =============
		"===
		"*** Loop over clr-fmt levels
		"===
		let j = 1
		while j <= b:txtfmt_num_formats - 1
			let chj = nr2char(b:txtfmt_fmt_first_tok + j)
			" Add to appropriate clusters
			exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_clrfmt_'.i.'_'.j
			exe 'syn cluster Tf'.cui.'_clrfmt_'.i.'_all add=Tf'.cui.'_clrfmt_'.i.'_'.j
			" Cache the nextgroup clause
			let ng = ' nextgroup=Tf'.cui.'_fmt_'.j.',Tf'.cui.'_clr_'.i.',@Tf'.cui.'_fmtclr_'.j.'_all,@Tf'.cui.'_clrfmt_'.i.'_all'.(bgc_enabled ? ',@Tf'.cui.'_clrfmtbgc_'.i.'_'.j.'_all' : '')
			" Define region that is begun by a start token
			exe 'syn region Tf'.cui.'_clrfmt_'.i.'_'.j
				\.' start=/'.chj.'/'.r1_clrfmt.' contains='.Tf_tok_group.ng
			" Define the following region if and only if at least one of the
			" region types whose end token could begin this region is active
			"============= BEGIN NON-INDENTING BLOCK =============
			if bgc_enabled
			" Define region that is begun by an end token
			" (when permitted by a nextgroup)
			exe 'syn region Tf'.cui.'_clrfmt_'.i.'_'.j
				\.r2_clrfmt.' contains='.Tf_tok_group.ng
			endif " bgc_enabled
			"=============  END NON-INDENTING BLOCK  =============
			" Define highlighting for this region
			" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
			" handled correctly in a cterm.
			"	:help cterm-colors
			exe 'hi Tf'.cui.'_clrfmt_'.i.'_'.j
				\.eq_clr.b:txtfmt_clr{i}.eq_fmt.b:txtfmt_fmt{j}
			"============= BEGIN NON-INDENTING BLOCK =============
			if bgc_enabled
			"===
			"*** Loop over clr-fmt-bgc levels
			"===
			let kp = 1
			while kp <= b:txtfmt_cfg_numbgcolors
				let k = b:txtfmt_cfg_bgcolor{kp}
				let chk = nr2char(b:txtfmt_bgc_first_tok + k)
				" Add to appropriate clusters
				exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_clrfmtbgc_'.i.'_'.j.'_'.k
				exe 'syn cluster Tf'.cui.'_clrfmtbgc_'.i.'_'.j.'_all add=Tf'.cui.'_clrfmtbgc_'.i.'_'.j.'_'.k
				if !b:txtfmt_cfg_conceal
					" Ensure that this and higher order regions use bgc-specific concealment group
					let Tf_tok_group = 'Tf'.cui.'_conceal_'.k
					exe 'syn match Tf'.cui.'_conceal_'.k.' /'.b:txtfmt_re_any_tok.'/ contained contains=NONE'.conceal
				endif
				" Define region that is begun by a start token
				exe 'syn region Tf'.cui.'_clrfmtbgc_'.i.'_'.j.'_'.k
					\.' start=/'.chk.'/'.r1_clrfmtbgc.' contains='.Tf_tok_group.' nextgroup=Tf'.cui.'_fmtbgc_'.j.'_'.k.',Tf'.cui.'_clrbgc_'.i.'_'.k.',Tf'.cui.'_clrfmt_'.i.'_'.j.',@Tf'.cui.'_fmtbgcclr_'.j.'_'.k.'_all,@Tf'.cui.'_clrbgcfmt_'.i.'_'.k.'_all,@Tf'.cui.'_clrfmtbgc_'.i.'_'.j.'_all'
				" Define highlighting for this region
				" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
				" handled correctly in a cterm.
				"	:help cterm-colors
				exe 'hi Tf'.cui.'_clrfmtbgc_'.i.'_'.j.'_'.k
					\.eq_clr.b:txtfmt_clr{i}.eq_bgc.b:txtfmt_bgc{k}.eq_fmt.b:txtfmt_fmt{j}
				let kp = kp + 1
			endwhile
			" Revert to toplevel (no background color) matchgroup
			let Tf_tok_group = Tf_top_tok_group
			endif " bgc_enabled
			"=============  END NON-INDENTING BLOCK  =============
			let j = j + 1
		endwhile
		let ip = ip + 1
	endwhile
	endif " clr_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"============= BEGIN NON-INDENTING BLOCK =============
	if bgc_enabled
	"===
	"*** Loop over bgc levels
	"===
	let ip = 1
	while ip <= b:txtfmt_cfg_numbgcolors
		let i = b:txtfmt_cfg_bgcolor{ip}
		let chi = nr2char(b:txtfmt_bgc_first_tok + i)
		" Add to appropriate clusters
		exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_bgc_'.i
		exe 'syn cluster Tf'.cui.'_bgc_all add=Tf'.cui.'_bgc_'.i
		if !b:txtfmt_cfg_conceal
			" Ensure that this and higher order regions use bgc-specific concealment group
			let Tf_tok_group = 'Tf'.cui.'_conceal_'.i
			exe 'syn match Tf'.cui.'_conceal_'.i.' /'.b:txtfmt_re_any_tok.'/ contained contains=NONE'.conceal
		endif
		" Cache the nextgroup clause
		let ng = ' nextgroup=@Tf'.cui.'_bgc_all'.(clr_enabled ? ',@Tf'.cui.'_bgcclr_'.i.'_all' : '').',@Tf'.cui.'_bgcfmt_'.i.'_all,Tf_conceal'
		" Define region that is begun by a start token
		exe 'syn region Tf'.cui.'_bgc_'.i
			\.' start=/'.chi.'/'.r1_bgc.' contains='.Tf_tok_group.ng
		" Define region that is begun by an end token
		" (when permitted by a nextgroup)
		exe 'syn region Tf'.cui.'_bgc_'.i
			\.r2_bgc.' contains='.Tf_tok_group.ng
		" Define highlighting for this region
		" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
		" handled correctly in a cterm.
		"	:help cterm-colors
		exe 'hi Tf'.cui.'_bgc_'.i
			\.eq_bgc.b:txtfmt_bgc{i}
		"============= BEGIN NON-INDENTING BLOCK =============
		if clr_enabled
		"===
		"*** Loop over bgc-clr levels
		"===
		let jp = 1
		while jp <= b:txtfmt_cfg_numfgcolors
			let j = b:txtfmt_cfg_fgcolor{jp}
			let chj = nr2char(b:txtfmt_clr_first_tok + j)
			" Add to appropriate clusters
			exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_bgcclr_'.i.'_'.j
			exe 'syn cluster Tf'.cui.'_bgcclr_'.i.'_all add=Tf'.cui.'_bgcclr_'.i.'_'.j
			" Cache the nextgroup clause
			let ng = ' nextgroup=Tf'.cui.'_clr_'.j.',Tf'.cui.'_bgc_'.i.',@Tf'.cui.'_clrbgc_'.j.'_all,@Tf'.cui.'_bgcclr_'.i.'_all,@Tf'.cui.'_bgcclrfmt_'.i.'_'.j.'_all'
			" Define region that is begun by a start token
			exe 'syn region Tf'.cui.'_bgcclr_'.i.'_'.j
				\.' start=/'.chj.'/'.r1_bgcclr.' contains='.Tf_tok_group.ng
			" Define region that is begun by an end token
			" (when permitted by a nextgroup)
			exe 'syn region Tf'.cui.'_bgcclr_'.i.'_'.j
				\.r2_bgcclr.' contains='.Tf_tok_group.ng
			" Define highlighting for this region
			" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
			" handled correctly in a cterm.
			"	:help cterm-colors
			exe 'hi Tf'.cui.'_bgcclr_'.i.'_'.j
				\.eq_clr.b:txtfmt_clr{j}.eq_bgc.b:txtfmt_bgc{i}
			"===
			"*** Loop over bgc-clr-fmt levels
			"===
			let k = 1
			while k <= b:txtfmt_num_formats - 1
				let chk = nr2char(b:txtfmt_fmt_first_tok + k)
				" Add to appropriate clusters
				exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_bgcclrfmt_'.i.'_'.j.'_'.k
				exe 'syn cluster Tf'.cui.'_bgcclrfmt_'.i.'_'.j.'_all add=Tf'.cui.'_bgcclrfmt_'.i.'_'.j.'_'.k
				" Define region that is begun by a start token
				exe 'syn region Tf'.cui.'_bgcclrfmt_'.i.'_'.j.'_'.k
					\.' start=/'.chk.'/'.r1_bgcclrfmt.' contains='.Tf_tok_group.' nextgroup=Tf'.cui.'_clrfmt_'.j.'_'.k.',Tf'.cui.'_bgcfmt_'.i.'_'.k.',Tf'.cui.'_bgcclr_'.i.'_'.j.',@Tf'.cui.'_clrfmtbgc_'.j.'_'.k.'_all,@Tf'.cui.'_bgcfmtclr_'.i.'_'.k.'_all,@Tf'.cui.'_bgcclrfmt_'.i.'_'.j.'_all'
				" Define highlighting for this region
				" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
				" handled correctly in a cterm.
				"	:help cterm-colors
				exe 'hi Tf'.cui.'_bgcclrfmt_'.i.'_'.j.'_'.k
					\.eq_clr.b:txtfmt_clr{j}.eq_bgc.b:txtfmt_bgc{i}.eq_fmt.b:txtfmt_fmt{k}
				let k = k + 1
			endwhile
			let jp = jp + 1
		endwhile
		endif " clr_enabled
		"=============  END NON-INDENTING BLOCK  =============
		"===
		"*** Loop over bgc-fmt levels
		"===
		let j = 1
		while j <= b:txtfmt_num_formats - 1
			let chj = nr2char(b:txtfmt_fmt_first_tok + j)
			" Add to appropriate clusters
			exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_bgcfmt_'.i.'_'.j
			exe 'syn cluster Tf'.cui.'_bgcfmt_'.i.'_all add=Tf'.cui.'_bgcfmt_'.i.'_'.j
			" Cache the nextgroup clause
			let ng = ' nextgroup=Tf'.cui.'_fmt_'.j.',Tf'.cui.'_bgc_'.i.',@Tf'.cui.'_fmtbgc_'.j.'_all,@Tf'.cui.'_bgcfmt_'.i.'_all'.(clr_enabled ? ',@Tf'.cui.'_bgcfmtclr_'.i.'_'.j.'_all' : '')
			" Define region that is begun by a start token
			exe 'syn region Tf'.cui.'_bgcfmt_'.i.'_'.j
				\.' start=/'.chj.'/'.r1_bgcfmt.' contains='.Tf_tok_group.ng
			" Define the following region if and only if at least one of the
			" region types whose end token could begin this region is active
			"============= BEGIN NON-INDENTING BLOCK =============
			if clr_enabled
			" Define region that is begun by an end token
			" (when permitted by a nextgroup)
			exe 'syn region Tf'.cui.'_bgcfmt_'.i.'_'.j
				\.r2_bgcfmt.' contains='.Tf_tok_group.ng
			endif " clr_enabled
			"=============  END NON-INDENTING BLOCK  =============
			" Define highlighting for this region
			" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
			" handled correctly in a cterm.
			"	:help cterm-colors
			exe 'hi Tf'.cui.'_bgcfmt_'.i.'_'.j
				\.eq_bgc.b:txtfmt_bgc{i}.eq_fmt.b:txtfmt_fmt{j}
			"============= BEGIN NON-INDENTING BLOCK =============
			if clr_enabled
			"===
			"*** Loop over bgc-fmt-clr levels
			"===
			let kp = 1
			while kp <= b:txtfmt_cfg_numfgcolors
				let k = b:txtfmt_cfg_fgcolor{kp}
				let chk = nr2char(b:txtfmt_clr_first_tok + k)
				" Add to appropriate clusters
				exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_bgcfmtclr_'.i.'_'.j.'_'.k
				exe 'syn cluster Tf'.cui.'_bgcfmtclr_'.i.'_'.j.'_all add=Tf'.cui.'_bgcfmtclr_'.i.'_'.j.'_'.k
				" Define region that is begun by a start token
				exe 'syn region Tf'.cui.'_bgcfmtclr_'.i.'_'.j.'_'.k
					\.' start=/'.chk.'/'.r1_bgcfmtclr.' contains='.Tf_tok_group.' nextgroup=Tf'.cui.'_fmtclr_'.j.'_'.k.',Tf'.cui.'_bgcclr_'.i.'_'.k.',Tf'.cui.'_bgcfmt_'.i.'_'.j.',@Tf'.cui.'_fmtclrbgc_'.j.'_'.k.'_all,@Tf'.cui.'_bgcclrfmt_'.i.'_'.k.'_all,@Tf'.cui.'_bgcfmtclr_'.i.'_'.j.'_all'
				" Define highlighting for this region
				" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
				" handled correctly in a cterm.
				"	:help cterm-colors
				exe 'hi Tf'.cui.'_bgcfmtclr_'.i.'_'.j.'_'.k
					\.eq_clr.b:txtfmt_clr{k}.eq_bgc.b:txtfmt_bgc{i}.eq_fmt.b:txtfmt_fmt{j}
				let kp = kp + 1
			endwhile
			endif " clr_enabled
			"=============  END NON-INDENTING BLOCK  =============
			let j = j + 1
		endwhile
		let ip = ip + 1
	endwhile
	" Revert to toplevel (no background color) matchgroup
	let Tf_tok_group = Tf_top_tok_group
	endif " bgc_enabled
	"=============  END NON-INDENTING BLOCK  =============
	"===
	"*** Loop over fmt levels
	"===
	let i = 1
	while i <= b:txtfmt_num_formats - 1
		let chi = nr2char(b:txtfmt_fmt_first_tok + i)
		" Add to appropriate clusters
		exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_fmt_'.i
		exe 'syn cluster Tf'.cui.'_fmt_all add=Tf'.cui.'_fmt_'.i
		" Cache the nextgroup clause
		let ng = ' nextgroup=@Tf'.cui.'_fmt_all'.(clr_enabled ? ',@Tf'.cui.'_fmtclr_'.i.'_all' : '').(bgc_enabled ? ',@Tf'.cui.'_fmtbgc_'.i.'_all' : '').',Tf_conceal'
		" Define region that is begun by a start token
		exe 'syn region Tf'.cui.'_fmt_'.i
			\.' start=/'.chi.'/'.r1_fmt.' contains='.Tf_tok_group.ng
		" Define the following region if and only if at least one of the
		" region types whose end token could begin this region is active
		"============= BEGIN NON-INDENTING BLOCK =============
		if clr_enabled || bgc_enabled
		" Define region that is begun by an end token
		" (when permitted by a nextgroup)
		exe 'syn region Tf'.cui.'_fmt_'.i
			\.r2_fmt.' contains='.Tf_tok_group.ng
		endif " clr_enabled || bgc_enabled
		"=============  END NON-INDENTING BLOCK  =============
		" Define highlighting for this region
		" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
		" handled correctly in a cterm.
		"	:help cterm-colors
		exe 'hi Tf'.cui.'_fmt_'.i
			\.eq_fmt.b:txtfmt_fmt{i}
		"============= BEGIN NON-INDENTING BLOCK =============
		if clr_enabled
		"===
		"*** Loop over fmt-clr levels
		"===
		let jp = 1
		while jp <= b:txtfmt_cfg_numfgcolors
			let j = b:txtfmt_cfg_fgcolor{jp}
			let chj = nr2char(b:txtfmt_clr_first_tok + j)
			" Add to appropriate clusters
			exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_fmtclr_'.i.'_'.j
			exe 'syn cluster Tf'.cui.'_fmtclr_'.i.'_all add=Tf'.cui.'_fmtclr_'.i.'_'.j
			" Cache the nextgroup clause
			let ng = ' nextgroup=Tf'.cui.'_clr_'.j.',Tf'.cui.'_fmt_'.i.',@Tf'.cui.'_clrfmt_'.j.'_all,@Tf'.cui.'_fmtclr_'.i.'_all'.(bgc_enabled ? ',@Tf'.cui.'_fmtclrbgc_'.i.'_'.j.'_all' : '')
			" Define region that is begun by a start token
			exe 'syn region Tf'.cui.'_fmtclr_'.i.'_'.j
				\.' start=/'.chj.'/'.r1_fmtclr.' contains='.Tf_tok_group.ng
			" Define the following region if and only if at least one of the
			" region types whose end token could begin this region is active
			"============= BEGIN NON-INDENTING BLOCK =============
			if bgc_enabled
			" Define region that is begun by an end token
			" (when permitted by a nextgroup)
			exe 'syn region Tf'.cui.'_fmtclr_'.i.'_'.j
				\.r2_fmtclr.' contains='.Tf_tok_group.ng
			endif " bgc_enabled
			"=============  END NON-INDENTING BLOCK  =============
			" Define highlighting for this region
			" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
			" handled correctly in a cterm.
			"	:help cterm-colors
			exe 'hi Tf'.cui.'_fmtclr_'.i.'_'.j
				\.eq_clr.b:txtfmt_clr{j}.eq_fmt.b:txtfmt_fmt{i}
			"============= BEGIN NON-INDENTING BLOCK =============
			if bgc_enabled
			"===
			"*** Loop over fmt-clr-bgc levels
			"===
			let kp = 1
			while kp <= b:txtfmt_cfg_numbgcolors
				let k = b:txtfmt_cfg_bgcolor{kp}
				let chk = nr2char(b:txtfmt_bgc_first_tok + k)
				" Add to appropriate clusters
				exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_fmtclrbgc_'.i.'_'.j.'_'.k
				exe 'syn cluster Tf'.cui.'_fmtclrbgc_'.i.'_'.j.'_all add=Tf'.cui.'_fmtclrbgc_'.i.'_'.j.'_'.k
				if !b:txtfmt_cfg_conceal
					" Ensure that this and higher order regions use bgc-specific concealment group
					let Tf_tok_group = 'Tf'.cui.'_conceal_'.k
					exe 'syn match Tf'.cui.'_conceal_'.k.' /'.b:txtfmt_re_any_tok.'/ contained contains=NONE'.conceal
				endif
				" Define region that is begun by a start token
				exe 'syn region Tf'.cui.'_fmtclrbgc_'.i.'_'.j.'_'.k
					\.' start=/'.chk.'/'.r1_fmtclrbgc.' contains='.Tf_tok_group.' nextgroup=Tf'.cui.'_clrbgc_'.j.'_'.k.',Tf'.cui.'_fmtbgc_'.i.'_'.k.',Tf'.cui.'_fmtclr_'.i.'_'.j.',@Tf'.cui.'_clrbgcfmt_'.j.'_'.k.'_all,@Tf'.cui.'_fmtbgcclr_'.i.'_'.k.'_all,@Tf'.cui.'_fmtclrbgc_'.i.'_'.j.'_all'
				" Define highlighting for this region
				" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
				" handled correctly in a cterm.
				"	:help cterm-colors
				exe 'hi Tf'.cui.'_fmtclrbgc_'.i.'_'.j.'_'.k
					\.eq_clr.b:txtfmt_clr{j}.eq_bgc.b:txtfmt_bgc{k}.eq_fmt.b:txtfmt_fmt{i}
				let kp = kp + 1
			endwhile
			" Revert to toplevel (no background color) matchgroup
			let Tf_tok_group = Tf_top_tok_group
			endif " bgc_enabled
			"=============  END NON-INDENTING BLOCK  =============
			let jp = jp + 1
		endwhile
		endif " clr_enabled
		"=============  END NON-INDENTING BLOCK  =============
		"============= BEGIN NON-INDENTING BLOCK =============
		if bgc_enabled
		"===
		"*** Loop over fmt-bgc levels
		"===
		let jp = 1
		while jp <= b:txtfmt_cfg_numbgcolors
			let j = b:txtfmt_cfg_bgcolor{jp}
			let chj = nr2char(b:txtfmt_bgc_first_tok + j)
			" Add to appropriate clusters
			exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_fmtbgc_'.i.'_'.j
			exe 'syn cluster Tf'.cui.'_fmtbgc_'.i.'_all add=Tf'.cui.'_fmtbgc_'.i.'_'.j
			if !b:txtfmt_cfg_conceal
				" Ensure that this and higher order regions use bgc-specific concealment group
				let Tf_tok_group = 'Tf'.cui.'_conceal_'.j
				exe 'syn match Tf'.cui.'_conceal_'.j.' /'.b:txtfmt_re_any_tok.'/ contained contains=NONE'.conceal
			endif
			" Cache the nextgroup clause
			let ng = ' nextgroup=Tf'.cui.'_bgc_'.j.',Tf'.cui.'_fmt_'.i.',@Tf'.cui.'_bgcfmt_'.j.'_all,@Tf'.cui.'_fmtbgc_'.i.'_all'.(clr_enabled ? ',@Tf'.cui.'_fmtbgcclr_'.i.'_'.j.'_all' : '')
			" Define region that is begun by a start token
			exe 'syn region Tf'.cui.'_fmtbgc_'.i.'_'.j
				\.' start=/'.chj.'/'.r1_fmtbgc.' contains='.Tf_tok_group.ng
			" Define the following region if and only if at least one of the
			" region types whose end token could begin this region is active
			"============= BEGIN NON-INDENTING BLOCK =============
			if clr_enabled
			" Define region that is begun by an end token
			" (when permitted by a nextgroup)
			exe 'syn region Tf'.cui.'_fmtbgc_'.i.'_'.j
				\.r2_fmtbgc.' contains='.Tf_tok_group.ng
			endif " clr_enabled
			"=============  END NON-INDENTING BLOCK  =============
			" Define highlighting for this region
			" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
			" handled correctly in a cterm.
			"	:help cterm-colors
			exe 'hi Tf'.cui.'_fmtbgc_'.i.'_'.j
				\.eq_bgc.b:txtfmt_bgc{j}.eq_fmt.b:txtfmt_fmt{i}
			"============= BEGIN NON-INDENTING BLOCK =============
			if clr_enabled
			"===
			"*** Loop over fmt-bgc-clr levels
			"===
			let kp = 1
			while kp <= b:txtfmt_cfg_numfgcolors
				let k = b:txtfmt_cfg_fgcolor{kp}
				let chk = nr2char(b:txtfmt_clr_first_tok + k)
				" Add to appropriate clusters
				exe 'syn cluster Tf'.cui.'_all add=Tf'.cui.'_fmtbgcclr_'.i.'_'.j.'_'.k
				exe 'syn cluster Tf'.cui.'_fmtbgcclr_'.i.'_'.j.'_all add=Tf'.cui.'_fmtbgcclr_'.i.'_'.j.'_'.k
				" Define region that is begun by a start token
				exe 'syn region Tf'.cui.'_fmtbgcclr_'.i.'_'.j.'_'.k
					\.' start=/'.chk.'/'.r1_fmtbgcclr.' contains='.Tf_tok_group.' nextgroup=Tf'.cui.'_bgcclr_'.j.'_'.k.',Tf'.cui.'_fmtclr_'.i.'_'.k.',Tf'.cui.'_fmtbgc_'.i.'_'.j.',@Tf'.cui.'_bgcclrfmt_'.j.'_'.k.'_all,@Tf'.cui.'_fmtclrbgc_'.i.'_'.k.'_all,@Tf'.cui.'_fmtbgcclr_'.i.'_'.j.'_all'
				" Define highlighting for this region
				" Note: cterm= MUST come after ctermfg= to ensure that bold attribute is
				" handled correctly in a cterm.
				"	:help cterm-colors
				exe 'hi Tf'.cui.'_fmtbgcclr_'.i.'_'.j.'_'.k
					\.eq_clr.b:txtfmt_clr{k}.eq_bgc.b:txtfmt_bgc{j}.eq_fmt.b:txtfmt_fmt{i}
				let kp = kp + 1
			endwhile
			endif " clr_enabled
			"=============  END NON-INDENTING BLOCK  =============
			let jp = jp + 1
		endwhile
		" Revert to toplevel (no background color) matchgroup
		let Tf_tok_group = Tf_top_tok_group
		endif " bgc_enabled
		"=============  END NON-INDENTING BLOCK  =============
		let i = i + 1
	endwhile
	" END AUTOGENERATED CODE BLOCK >>>
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
	" TODO_BG: Guard against reliance upon _bgc_ vars when they wouldn't have
	" been defined.
	if b:txtfmt_cfg_escape == 'bslash' || b:txtfmt_cfg_escape == 'self'
		if b:txtfmt_cfg_escape == 'self'
			let re_outer_esc_pair = '\(['.b:txtfmt_re_any_stok_atom.']\)\1'
			let re_any_stok_esc_pair = '\(['.b:txtfmt_re_any_stok_atom.']\)\1'
			let re_fmt_etok_esc_pair = '\(['.b:txtfmt_re_fmt_etok_atom.']\)\1'
			if bgc_enabled
				let re_bgc_etok_esc_pair = '\(['.b:txtfmt_re_bgc_etok_atom.']\)\1'
			endif
			if clr_enabled
				let re_clr_etok_esc_pair = '\(['.b:txtfmt_re_clr_etok_atom.']\)\1'
			endif
			" Escape char is same number of bytes as a token
			let esc_off = tok_off
		elseif b:txtfmt_cfg_escape == 'bslash'
			let re_outer_esc_pair = '\\\%(\\\%(\\*['.b:txtfmt_re_any_stok_atom.']\)\@=\|['.b:txtfmt_re_any_stok_atom.']\)'
			let re_any_stok_esc_pair = '\\\%(\\\%(\\*['.b:txtfmt_re_any_stok_atom.']\)\@=\|['.b:txtfmt_re_any_stok_atom.']\)'
			let re_fmt_etok_esc_pair = '\\\%(\\\%(\\*['.b:txtfmt_re_fmt_etok_atom.']\)\@=\|['.b:txtfmt_re_fmt_etok_atom.']\)'
			if bgc_enabled
				let re_bgc_etok_esc_pair = '\\\%(\\\%(\\*['.b:txtfmt_re_bgc_etok_atom.']\)\@=\|['.b:txtfmt_re_bgc_etok_atom.']\)'
			endif
			if clr_enabled
				let re_clr_etok_esc_pair = '\\\%(\\\%(\\*['.b:txtfmt_re_clr_etok_atom.']\)\@=\|['.b:txtfmt_re_clr_etok_atom.']\)'
			endif
			" Escape char is single byte
			let esc_off = 1
		endif
		" Prevent escaping or escaped tokens from starting regions, and hide
		" escaping tokens outside regions.
		exe 'syn match Tf_outer_esc /'.re_outer_esc_pair.'/he=s+'.esc_off.containedin_def.conceal
		" Prevent escaping or escaped tokens within regions from causing
		" transitions, and hide escaping tokens within regions.
		exe 'syn match Tf_any_stok_inner_esc /'.re_any_stok_esc_pair.'/he=s+'.esc_off.' contains=NONE'
			\.' contained'.conceal
		exe 'syn match Tf_fmt_etok_inner_esc /'.re_fmt_etok_esc_pair.'/he=s+'.esc_off.' contains=NONE'
			\.' contained'.conceal
		if bgc_enabled
			exe 'syn match Tf_bgc_etok_inner_esc /'.re_bgc_etok_esc_pair.'/he=s+'.esc_off.' contains=NONE'
				\.' contained'.conceal
		endif
		if clr_enabled
			exe 'syn match Tf_clr_etok_inner_esc /'.re_clr_etok_esc_pair.'/he=s+'.esc_off.' contains=NONE'
				\.' contained'.conceal
		endif
		" Define highlighting for the outer and inner escape tokens
		hi link Tf_outer_esc Tf_conceal
		hi link Tf_any_stok_inner_esc Tf_conceal
		hi link Tf_fmt_etok_inner_esc Tf_conceal
		if bgc_enabled
			hi link Tf_bgc_etok_inner_esc Tf_conceal
		endif
		if clr_enabled
			hi link Tf_clr_etok_inner_esc Tf_conceal
		endif
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

# Define the various output passes
use constant {
	DEFS  => 0,
	LOOPS => 1,
};

# This semaphore helps determine when an "if <typ>_enabled" construct in the
# Vim code would be redundant with a containing one.
# TODO: Currently unused - remove...
my $bgc_guard_cnt = 0;

sub do_lvl($$$)
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
	my $section = shift;
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
		my $il;
		if ($section == DEFS) {
			$il = "\t";
		} else { # if $section == LOOPS
			$il = "\t" x ($init_il + $lvl);
		}

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
		if ($section == DEFS) {
			# Determine current level's indent
			$il = "\t";
			print "\n$il\"===";
			print "\n$il\"*** ", join("-", @a1n);
			print "\n$il\"===";

			print "\n$il", "if b:txtfmt_cfg_escape != 'none'";
			print "\n$il\t", "let contains_", join("", @a1n), "=",
				"\n$il\t\t\\' contains='",
				"\n$il\t\t\\.'Tf_any_stok_inner_esc,'",
				"\n$il\t\t\\.'",
				join(",'\n$il\t\t\\.'",
					map { "Tf_${_}_etok_inner_esc" } @a1n
				),
				"'"
			;
			print "\n$il", "else";
			print "\n$il\t", "let contains_", join("", @a1n), " = ''";
			print "\n$il", "endif";
			print "\n$il", "let end_", join("", @a1n), " =";
			print "\n$il\t", "\\' end=/['";
			print "\n$il\t", "\\.b:txtfmt_re_any_stok_atom";
			print "\n$il\t", "\\.",
				join("\n$il\t\\.",
					map { "b:txtfmt_re_${_}_etok_atom" } @a1n
				),
				"\n$il\t", "\\.']/me=e-'.tok_off.',he=e-'.tok_off"
			;
			# Define r1_<rgn> var
			print "\n$il", "let r1_", join("", @a1n), " = skip";
			print "\n$il\t", "\\.contains_", join("", @a1n);
			print "\n$il\t", "\\.end_", join("", @a1n);
			if (@a1n == 1) {
				print "\n$il\t", "\\.containedin_def";
			} else {
				print "\n$il\t", "\\.' contained'";
			}
			# Don't define r2_<rgn> var if there's not at least 1 higher order
			# region
			if (@a2n) {
				# Ensure that we don't define the r2 region variables if all
				# of the <typ>'s whose end token could begin the region are
				# inactive. If at least one of these <typ>'s is active, the
				# vars will be defined, and <typ>_enabled ternaries will be
				# used as necessary to ensure that we don't consider end
				# tokens for inactive region types.
				if ($need_r2_guard) {
					print "\n$il", '" Define the r2 region vars if and only if at least one of the';
					print "\n$il", '" region types whose end token could begin this region is active';
					print "\n$il", '"============= BEGIN NON-INDENTING BLOCK =============';
					print "\n$il", "if ",
						join ' || ', map { "${_}_enabled" } @a2n
					;
				}
				print "\n$il", "let start_", join("", @a1n), " =";
				print "\n$il\t", "\\' start=/['";
				print "\n$il\t", "\\.",
					join(".",
						map {
							# Wrap the end token for <typ> in ternary guard
							# unless a containing `if <typ>_enabled' renders
							# it redundant.
							# Note: The ternary is never redundant when
							# multiple types are logically or'ed in the
							# containing if: e.g.,
							# if <typ1>_enabled || <typ2>_enabled
							# Note: If $need_r2_guard is true, @a2n is
							# precisely the number of conditions in the if
							my $typ = $_;
							my $need_ternary =
								(grep { $_ eq $typ } @can_dsbl
								and !$need_r2_guard || @a2n > 1);
							(
								$need_ternary
								? "(${typ}_enabled ? "
								: ""
							) .
							"b:txtfmt_re_${typ}_etok_atom" .
							(
								$need_ternary
								? " : '')"
								: ""
							)
						} @a2n
					)
				;
				print "\n$il\t", "\\.']/'";

				print "\n$il", "let r2_", join("", @a1n), " = skip";
				print "\n$il\t", "\\.contains_", join("", @a1n);
				print "\n$il\t", "\\.start_", join("", @a1n);
				print "\n$il\t", "\\.end_", join("", @a1n);
				print "\n$il\t", "\\.' contained'";
				if ($need_r2_guard) {
					print "\n$il", "endif \" ",
						join ' || ', map { "${_}_enabled" } @a2n;
					print "\n$il", '"=============  END NON-INDENTING BLOCK  =============';
				}
			}
		} else { # $section == LOOPS
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
			# Define matchgroup if background color could be changing (i.e., if
			# last element of @a1n is 'bgc')
			# Note: matchgroup will retain setting given until we move back to
			# a lower order region
			# Also note: bgc_enabled check would be superfluous here, since
			# code won't be executed if bgc_enabled is false
			if ($a1n[-1] eq 'bgc') {
				# Note: Leave Tf_tok_group at its default when conceal is disabled.
				print "\n$il", 'if !b:txtfmt_cfg_conceal';
				print "\n$il\t", '" Ensure that this and higher order regions use bgc-specific concealment group';
				print "\n$il\t", "let Tf_tok_group = 'Tf'.cui.'_conceal_'.$idx{$a1n[-1]}";
				# Define this concealment group.
				# NMG_TODO: Think about best way to specify match.
				print "\n$il\t", "exe 'syn match Tf'.cui.'_conceal_'.$idx{$a1n[-1]}",
					".' /'.b:txtfmt_re_any_tok.'/ contained contains=NONE'.conceal";
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
				$ng .= ($unquoted ? ".'" : '') . ",Tf_conceal'";
			} elsif (!$unquoted) {
				$ng .= "'";
			}
			# Use substr to strip off the leading comma at the head of $ng
			$ng = "' nextgroup=" . substr($ng, 1);

			# If nextgroup is about to be used in 2 region definitions (r1 and
			# r2), assign it to a variable for efficiency. (Many
			# concatenations are required to build the entire nextgroup
			# clause.)
			# Note: If there are no more regions to pull in, the nextgroup
			# clause will be used only once, so it's more efficient to build
			# it within the region definition itself.
			if (@a2n) {
				print "\n$il\" Cache the nextgroup clause";
				print "\n$il", "let ng = ", $ng;
				# Obviate the need for subsequent logic within this script to
				# know whether we're caching nextgroup clause or not
				$ng = 'ng';
			}

			$contains = "' contains='.Tf_tok_group";
			# Define the rgn that is begun with an stok
			print "\n$il\" Define region that is begun by a start token";
			# Save the beginning of the `syn region' statement, which is
			# common to both the region begun by start tok and the region
			# begun by end tok. (Note that they diverge where there *once*
			# was - but no longer is - an `_rtd' in the latter's region name.)
			my $pre_start = "\n$il" .
				"exe 'syn region " .
				"Tf'.cui.'_" . join("", @a1n) . "_'." . join(".'_'.", @idx{@a1n});
			print "$pre_start",
				"\n$il\t\\.' start=/'.ch$idx{$a1n[-1]}.'/'",
				".r1_", join("", @a1n), ".$contains.$ng";

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
				print "$pre_start\n$il\t\\.",
					"r2_", join("", @a1n), ".$contains.$ng";

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
				),
			;
			# Define the highlighting region for the rtd group (if it exists)
			# Note: No longer necessary, since rtd group is same as non-rtd.
			# TODO: Get rid of this block.
			#if (@a2n) {
			#	# Link rtd to non-rtd group, since highlighting is identical
			#	print "\n$il\" Link rtd to non-rtd group";
			#	print "\n$il", "exe 'hi link Tf'.cui.'_", join("", @a1n), "_'.",
			#		join(".'_'.", @idx{@a1n}), ".'_rtd",
			#		" Tf'.cui.'_", join("", @a1n), "_'.",
			#		join(".'_'.", @idx{@a1n})
			#	;

			#}
		}

		# RECURSE
		# Call ourself recursively to handle the next level
		do_lvl($section, \@a1n, \@a2n);

		# POST RECURSION
		if ($section == DEFS) {
		} else { # if $section == LOOPS
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
		}
		# Handle departure from blocks corresponding to <typ>'s that can be
		# disabled
		if (my ($typ) = grep { $_ eq $a1n[-1] } @can_dsbl) {
			if ($typ eq 'bgc') {
				# Revert to toplevel (no bgc) matchgroup
				# Note: Code emitted won't be reached if bgc_enabled is false
				print "\n$il", '" Revert to toplevel (no background color) matchgroup';
				print "\n$il", "let Tf_tok_group = Tf_top_tok_group" if $section eq LOOPS;
			}
			print "\n$il", "endif \" ${typ}_enabled";
			print "\n$il", '"=============  END NON-INDENTING BLOCK  =============';
		}
	}
}

# Top level recursion for both sections
# When the following call returns, the entire DEFS section will have been
# output
print "\t\" BEGIN AUTOGENERATED CODE BLOCK ", "<<<";
print "\n\t\" Last update: ", scalar(localtime), "\n";
do_lvl(DEFS, [], \@rgn);
# When the following call returns, the entire LOOPS section will have been
# output
do_lvl(LOOPS, [], \@rgn);
print "\n\t\" END AUTOGENERATED CODE BLOCK ", ">>>";

__END__
	" vim: sw=4 ts=4 foldmethod=marker foldmarker=<<<,>>> :
