" Txtfmt: Set of Vim plugins (syntax, ftplugin, plugin) for creating and
" displaying formatted text with Vim.
" File: This is the global plugin file, which contains configuration code
" needed by both the ftplugin and the syntax files.
" Creation:	2004 Nov 06
" Last Change: 2008 May 10
" Maintainer:	Brett Pershing Stahlman <brettstahlman@comcast.net>
" License:	This file is placed in the public domain.

" Note: The following line is required by a packaging script
let g:TXTFMT_VERSION = "1.0e"

" Autocommands needed by refresh mechanism <<<
au FileType * call s:Txtfmt_save_filetype()
au Syntax * call s:Txtfmt_save_syntax()
fu! s:Txtfmt_save_filetype()
	let l:filetype = expand("<amatch>")
	if l:filetype =~ '\%(^\|\.\)txtfmt\%(\.\|$\)'
		let b:txtfmt_filetype = l:filetype
	endif
endfu
fu! s:Txtfmt_save_syntax()
	let l:syntax = expand("<amatch>")
	if l:syntax =~ '\%(^\|\.\)txtfmt\%(\.\|$\)'
		let b:txtfmt_syntax = l:syntax
	endif
endfu
" >>>
" Functions needed regardless of whether common config is being performed <<<
" Function: s:Is_txtfmt_modeline() <<<
" Purpose: Return nonzero if and only if the line whose text is input "looks
" like" a txtfmt modeline.
" Note: Option names and values are NOT validated. That function is performed
" by s:Process_txtfmt_modeline.
" Inputs:
" linestr  -A line of text that may or may not represent a txtfmt modeline.
" Note: Format of the modeline is as follows:
" (Based upon 1st Vim modeline format)
"	[text]{white}txtfmt:[white]{options}
"	{options} can be the following:
"	{tokrange|rng}=<hex>|<dec>[sSlL]
"	{escape|esc}={self|bslash|none}
"	{sync}={<hex>|<dec>|fromstart|none}
"	{bgcolor|bg}=<color>
"	{nested|nst}
" Must be no whitespace surrounding the '='
" Also: Since Vim will not choke on a trailing ':' (though it's not
" technically part of the 1st modeline format), neither will I.
" Define regexes used to process modeline
" Also: ' txtfmt:' with no options is not an error, but will not be recognized
" as a modeline. Rationale: If user doesn't define any options in a modeline,
" assume it's just text that looks like the start of a modeline.
let s:re_ml_start = '\%(.\{-}\%(\s\+txtfmt:\s*\)\)'
let s:re_ml_name  = '\%(\I\i*\)'
let s:re_ml_val   = '\%(\%(\\.\|[^:[:space:]]\)\+\)'
let s:re_ml_el    = '\%('.s:re_ml_name.'\%(='.s:re_ml_val.'\)\?\)'
let s:re_ml_elsep = '\%(\s\+\|\s*:\s*\)'
let s:re_ml_end   = '\%(\s*:\?\s*$\)'
" Construct pattern matching entire line, which will capture all leading text
" in \1, all options in \2, and all trailing text in \3
let s:re_modeline = '\('.s:re_ml_start.'\)\('
			\.s:re_ml_el.'\%('.s:re_ml_elsep.s:re_ml_el.'\)*\)'
			\.'\('.s:re_ml_end.'\)'
fu! s:Is_txtfmt_modeline(linestr)
	return a:linestr =~ s:re_modeline
endfu
" >>>
" >>>
" IMPORTANT NOTE: The common configuration code in this file is intended to be
" executed only upon request by either the txtfmt ftplugin or syntax file.
" Since this file resides in the Vim plugin directory, it will be sourced by
" Vim automatically whenever Vim is started; the common configuration code,
" however, will not be executed at Vim startup because the special txtfmt
" variable b:txtfmt_do_common_config will not be set at that time. When either
" the ftplugin or syntax file wishes to execute the code in this script, it
" sets b:txtfmt_do_common_config and uses :runtime to source this file. When
" the common configuration code within this file executes, it makes its output
" available to both ftplugin and syntax files via buffer-local variables.

if exists('b:txtfmt_do_common_config')
" Needed for both ftplugin and syntax
" Command: Refresh <<<
com! -buffer Refresh call s:Txtfmt_refresh()
" >>>
" Autocommands <<<
" Ensure that common configuration will be redone whenever the txtfmt buffer
" is re-read (e.g. after a txtfmt-modeline has been changed).
" Note: This autocmd allows user simply to do ":e" at the Vim command line to
" resource everything, including the option processing in common config.
augroup TxtfmtCommonConfig
	au BufReadPre <buffer> :unlet! b:txtfmt_did_common_config
augroup END
" >>>
" Common constant definitions <<<

" Define first Vim version that supported undercurl
let b:txtfmt_const_vimver_undercurl = 700
" Define several sets of constants needed by the functions used to process
" 'tokrange' option.
" Define the tokrange default
let b:txtfmt_const_tokrange_def = '180S'
" TODO - Delete the one above when I've validated the one below...
" Define tokrange defaults according to 'starttok' and 'formats' values
" TODO - Eventually, these may need to be encoding-specific, but not yet...
let b:txtfmt_const_starttok_def = 180
let b:txtfmt_const_formats_def  = 'S'
" Define the number of tokens in the txtfmt token range as a function of
" 'formats' option: N = 2 ^ {num_attributes} + 10
let b:txtfmt_const_tokrange_size_{'basic'}             = 18 
let b:txtfmt_const_tokrange_size_{'all_but_undercurl'} = 42 
let b:txtfmt_const_tokrange_size_{'all'}               = 74 
" Define the maximum character code that may be used as a txtfmt token as a
" function of encoding class. (Encoding class is specified as '1' for single
" byte, '2' for 16-bit and 'u' for unicode.)
" Note: Vim doesn't support unicode chars larger than 16-bit.
let b:txtfmt_const_tokrange_limit_{'1'} = 0xFF
let b:txtfmt_const_tokrange_limit_{'2'} = 0xFFFF
let b:txtfmt_const_tokrange_limit_{'u'} = 0xFFFF
" The following regex describes a {number} that may be used to set a numeric
" option. The txtfmt help specifies that only decimal or hexadecimal formats
" are permitted.
let b:txtfmt_re_number_atom = '\([1-9]\d*\|0x\x\+\)'

" >>>
" Parse_<...> functions <<<
" Function: s:Parse_init()
" Purpose:
" Input:
" text		- string to be parsed
" re_tok	- regex matching a token
" Return: ID that must be passed to the other parse functions (-1 indicates
" error)
" Simulated struct:
" text
" len
" re
" pos
fu! s:Parse_init(text, re_tok)
	let i = 0
	let MAX_PARSE_INSTANCES = 1000
	while i < MAX_PARSE_INSTANCES
		if !exists('s:parsedata_'.i.'_text')
			" Found a free one
			let s:parsedata_{i}_text = a:text
			let s:parsedata_{i}_len = strlen(a:text)	" for speed
			let s:parsedata_{i}_re = a:re_tok
			let s:parsedata_{i}_pos = 0
			return i
		endif
		let i = i + 1
	endwhile
	if i >= MAX_PARSE_INSTANCES
		echoerr "Internal Parse_init error - contact developer"
		return -1
	endif
endfu
" Function: s:Parse_nexttok()
" Purpose:
" Input:
" Return: The next token as a string (or empty string if no more tokens)
" Error: Not possible when used correctly, and since this is an internally
" used function, we will not check for error.
fu! s:Parse_nexttok(parse_id)
	" Note: Structures used and generated internally in controlled manner, so
	" assume parse_id points to valid struct
	let text = s:parsedata_{a:parse_id}_text
	let re = s:parsedata_{a:parse_id}_re
	let pos = s:parsedata_{a:parse_id}_pos
	let len = s:parsedata_{a:parse_id}_len
	let parse_complete = 0	" set if text exhausted
	" Did last call exhaust text?
	if pos >= len
		let parse_complete = 1
		let ret_str = ''
	else
		" text not exhausted yet - get past any whitespace
		" ^\s* will return pos if no whitespace at pos (cannot return -1)
		let pos = matchend(text, '^\s*', pos)
		" Did we move past trailing whitespace?
		if pos >= len
			let parse_complete = 1
			let ret_str = ''
		else
			" We're sitting on first char to be returned.
			" re determines how many more will be part of return str
			" Note: Force re to match at current pos if at all
			let pos2 = matchend(text, '^'.re, pos)
			if pos2 < 0
				" curr char is not part of a token, so just return it by itself
				let ret_str = text[pos]
				let pos = pos + 1
			else
				" Return the token whose end was located
				let ret_str = strpart(text, pos, pos2-pos)
				let pos = pos2
			endif
		endif
	endif
	" Only way out of this function
	if parse_complete
		call s:Parse_free(a:parse_id)
	else
		" Update pos in structure for next call...
		let s:parsedata_{a:parse_id}_pos = pos
	endif
	return ret_str
endfu
" Function: s:Parse_free()
" Purpose: Free the data structures for a particular parse instance (denoted
" by input id)
" Input: parse_id - parse instance whose data is to be freed
" Return: none
" Error: not possible
fu! s:Parse_free(parse_id)
	" Using unlet! ensures that error not possible
	unlet! s:parsedata_{a:parse_id}_text
	unlet! s:parsedata_{a:parse_id}_re
	unlet! s:parsedata_{a:parse_id}_pos
	unlet! s:parsedata_{a:parse_id}_len

endfu
" >>>
" encoding utility functions (common) <<<
let s:re_encs_1 = '^\%('
			\.'latin1\|iso-8859-n\|koi8-r\|koi8-u'
			\.'\|macroman\|cp437\|cp737\|cp775'
			\.'\|cp850\|cp852\|cp855\|cp857'
			\.'\|cp860\|cp861\|cp862\|cp863'
			\.'\|cp865\|cp866\|cp869\|cp874'
			\.'\|cp1250\|cp1251\|cp1253\|cp1254'
			\.'\|cp1255\|cp1256\|cp1257\|cp1258'
			\.'\)$'
let s:re_encs_2 = '^\%('
			\.'cp932\|euc-jp\|sjis\|cp949'
			\.'\|euc-kr\|cp936\|euc-cn\|cp950'
			\.'\|big5\|euc-tw'
			\.'\)'
let s:re_encs_u = '^\%('
			\.'utf-8\|ucs-2\|ucs-2le\|utf-16\|utf-16le\|ucs-4\|ucs-4le'
			\.'\)'
" Function: TxtfmtCommon_Encoding_get_class() <<<
" Purpose: Return single character indicating whether the input encoding name
" represents a 1-byte encoding ('1'), a 2-byte encoding ('2'), or a unicode
" encoding ('u'). If input encoding name is unrecognized, return empty string.
fu! TxtfmtCommon_Encoding_get_class(enc)
	if a:enc  =~ s:re_encs_1
		return '1'
	elseif a:enc =~ s:re_encs_2
		return '2'
	elseif a:enc =~ s:re_encs_u
		return 'u'
	else
		return ''
	endif
endfu
" >>>
" >>>
" 'tokrange' utility functions <<<
" Construct pattern that will capture char code in \1 and optional size
" specification (sSlL) in \2.
let s:re_tokrange_spec = '^\([1-9]\d*\|0x\x\+\)\([sSlL]\?\)$'

" Function: s:Tokrange_is_valid() <<<
" Purpose: Indicate whether input string is a valid tokrange spec.
fu! s:Tokrange_is_valid(spec)
	return a:spec =~ s:re_tokrange_spec
endfu
" >>>
" Function: s:Tokrange_get_starttok() <<<
" Purpose: Return 'starttok' component of the input tokrange spec.
" Note: Return value will be numeric.
" Assumption: Input string has already been validated by s:Tokrange_is_valid.
fu! s:Tokrange_get_starttok(spec)
	return 0 + substitute(a:spec, s:re_tokrange_spec, '\1', '')
endfu
" >>>
" Function: s:Tokrange_get_formats() <<<
" Purpose: Return 'formats' component of the input tokrange spec.
" Note: If optional 'formats' component is omitted, empty string will be
" returned.
" Note: formats spec will be returned in canonical form (uppercase).
" Assumption: Input string has already been validated by s:Tokrange_is_valid.
fu! s:Tokrange_get_formats(spec)
	return substitute(a:spec, s:re_tokrange_spec, '\U\2', '')
endfu
" >>>
" Function: s:Tokrange_translate_formats() <<<
fu! s:Tokrange_translate_formats(formats)
	if strlen(a:formats) == 0
		" Default to 'short' formats
		return 'basic'
	elseif a:formats ==? 'L'
		" Long formats
		if v:version >= 700
			return 'all'
		else
			return 'all_but_undercurl'
		endif
	else
		" Short formats
		return 'basic'
	endif
endfu
" >>>
" >>>
" 'sync' utility functions <<<
" Construct pattern that will validate the option value.
let s:re_sync_number_spec = '^\([1-9]\d*\|0x\x\+\)$'
let s:re_sync_name_spec = '^fromstart\|none$'

" Function: s:Sync_is_valid() <<<
" Purpose: Indicate whether input string is a valid sync option value
fu! s:Sync_is_valid(spec)
	return a:spec =~ s:re_sync_number_spec || a:spec =~ s:re_sync_name_spec
endfu
" >>>
" Function: s:Sync_get_method() <<<
" Purpose: Determine the syncmethod represented by the input sync value and
" return its name. Possible values: 'minlines', 'fromstart', 'none'
" Assumption: Input string has already been validated by s:Sync_is_valid.
fu! s:Sync_get_method(spec)
	if a:spec =~ s:re_sync_name_spec
		return a:spec
	else
		return 'minlines'
	endif
endfu
" >>>
" >>>
" 'escape' utility functions <<<
" Construct pattern that will validate the option value.
let s:re_escape_optval = '^\%(none\|bslash\|self\)$'
" Function: s:Escape_is_valid() <<<
" Purpose: Indicate whether input string is a valid escape option value
fu! s:Escape_is_valid(optval)
	return a:optval =~ s:re_escape_optval
endfu
" >>>
" >>>
" Number validation utility functions <<<
" Function: s:Number_is_valid(s)
" Purpose: Indicate whether the input string represents a valid number
fu! s:Number_is_valid(s)
	return a:s =~ '^\s*'.b:txtfmt_re_number_atom.'\s*$'
endfu
" >>>
" Function: s:Set_tokrange() <<<
" Purpose: Set b:txtfmt_cfg_starttok and b:txtfmt_cfg_formats options, taking
" into account any user-setting of 'tokrange' option, and if necessary, the
" txtfmt defaults, which may take 'encoding' into consideration.
" Note: If set via modeline, tokrange option value must be a literal tokrange
" specification; however, buf-local and global option variables may be set to
" either a valid tokrange spec, or a Vim expression that evaluates to one.
" Permitting arbitrary Vim expressions facilitates the use of complex tokrange
" selection logic, implemented by a user-defined expression or function.
"  Examples:
"      'g:My_tokrange_calculator()'
"      '&enc == "utf-8" ? 1400l : 130s' 
fu! s:Set_tokrange()
	" Cache the 'encoding' in effect
	let enc = &encoding
	" Determine the corresponding encoding class
	let enc_class = TxtfmtCommon_Encoding_get_class(enc)
	if !exists('b:txtfmt_cfg_tokrange') || strlen(b:txtfmt_cfg_tokrange) == 0
		" Either option wasn't set within modeline, or it was set to invalid
		" value.
		if exists('b:txtfmt_cfg_tokrange') && strlen(b:txtfmt_cfg_tokrange) == 0
			" Bad modeline set
			let l:warnmsg =
				\"Warning: Ignoring invalid modeline value for txtfmt `tokrange' option"
		elseif exists('b:txtfmtTokrange')
			" User overrode buf-local option. Save the option for validation
			" below...
			let b:txtfmt_cfg_tokrange = b:txtfmtTokrange
			let l:set_by = 'b'
		elseif exists('g:txtfmtTokrange')
			" User overrode global option. Save the option for validation
			" below...
			let b:txtfmt_cfg_tokrange = g:txtfmtTokrange
			let l:set_by = 'g'
		endif
	endif
	if exists('l:set_by') && (l:set_by == 'b' || l:set_by == 'g')
		" Perform special validation for buf-local/global settings, which
		" permits either a tokrange spec or a Vim expression that evaluates to
		" one.
		if !s:Tokrange_is_valid(b:txtfmt_cfg_tokrange)
			" Not a valid tokrange literal. Let's see whether it evaluates to
			" one.
			let v:errmsg = ''
			" Evaluate expression, using silent! to prevent problems in the
			" event that rhs is invalid.
			silent! exe 'let l:tokrange = '.b:txtfmt_cfg_tokrange
			if v:errmsg != ''
				" Bad expression
				let l:warnmsg =
					\"Warning: Ignoring invalid ".(l:set_by == 'b' ? 'buf-local' : 'global')
					\." value for txtfmt 'tokrange' option: ".b:txtfmt_cfg_tokrange
				" Discard the invalid setting
				let b:txtfmt_cfg_tokrange = ''
			else
				" It was a valid Vim expression. Did it produce a valid
				" tokrange spec?
				if !s:Tokrange_is_valid(l:tokrange)
					let l:warnmsg =
						\"Ignoring ".(l:set_by == 'b' ? 'buf-local' : 'global')
						\." set of txtfmt `tokrange' option: `".b:txtfmt_cfg_tokrange
						\."' produced invalid option value: ".l:tokrange
					" Discard the invalid setting
					let b:txtfmt_cfg_tokrange = ''
				else
					" Save the valid setting
					let b:txtfmt_cfg_tokrange = l:tokrange
				endif
			endif
		endif
	endif
	" Warn user if invalid user-setting is about to be overridden
	if exists('l:warnmsg')
		echoerr l:warnmsg
	endif
	" Decompose any valid user setting.
	" Note: The output from the preceding stage is b:txtfmt_cfg_tokrange,
	" which we must now decompose into b:txtfmt_cfg_starttok and
	" b:txtfmt_cfg_formats. If b:txtfmt_cfg_tokrange is nonexistent or null,
	" there is no valid user setting, in which case, we'll supply default.
	if exists('b:txtfmt_cfg_tokrange') && strlen(b:txtfmt_cfg_tokrange)
		" Decompose valid setting into starttok and formats
		let b:txtfmt_cfg_starttok = s:Tokrange_get_starttok(b:txtfmt_cfg_tokrange)
		" Formats is a bit special. For one thing, it can be omitted from a
		" tokrange spec, in which case we'll choose a default. Also, long
		" formats ('L') can mean either 'all' or 'all_but_undercurl',
		" depending upon Vim version. (Undercurl was not supported until Vim
		" version 7.0.)
		let b:txtfmt_cfg_formats = s:Tokrange_translate_formats(
			\s:Tokrange_get_formats(b:txtfmt_cfg_tokrange)
		\)
		" Perform upper-bound validation
		if b:txtfmt_cfg_starttok + b:txtfmt_const_tokrange_size_{b:txtfmt_cfg_formats} - 1
			\ > b:txtfmt_const_tokrange_limit_{enc_class}
			" Warn user and use default
			echoerr
				\ "Warning: Tokrange value '".b:txtfmt_cfg_tokrange."' causes upper"
				\." bound to be exceeded for encoding ".&enc
			" Make sure we set to default below
			unlet! b:txtfmt_cfg_starttok b:txtfmt_cfg_formats
		endif
	endif
	" If b:txtfmt_cfg_starttok is still undefined, see whether there's an
	" encoding-specific default.
	if !exists('b:txtfmt_cfg_starttok')
		" TODO - Put any logic that depends upon specific encoding here...
		" .
		" .

	endif
	" If b:txtfmt_cfg_tokrange is still undefined, see whether there's an
	" encoding-class-specific default.
	if !exists('b:txtfmt_cfg_starttok')
		if enc_class == '1'
			let b:txtfmt_cfg_starttok = b:txtfmt_const_starttok_def
			let b:txtfmt_cfg_formats  = s:Tokrange_translate_formats(b:txtfmt_const_formats_def)
		elseif enc_class == '2'
			let b:txtfmt_cfg_starttok = b:txtfmt_const_starttok_def
			let b:txtfmt_cfg_formats  = s:Tokrange_translate_formats(b:txtfmt_const_formats_def)
		elseif enc_class == 'u'
			let b:txtfmt_cfg_starttok = b:txtfmt_const_starttok_def
			let b:txtfmt_cfg_formats  = s:Tokrange_translate_formats(b:txtfmt_const_formats_def)
		else
			let b:txtfmt_cfg_starttok = b:txtfmt_const_starttok_def
			let b:txtfmt_cfg_formats  = s:Tokrange_translate_formats(b:txtfmt_const_formats_def)
		endif
	endif
	" We're done with b:txtfmt_cfg_tokrange now that it has been completely
	" decomposed.
	unlet! b:txtfmt_cfg_tokrange
endfu
" >>>
" Function: s:Set_tokrange_old() <<<
" Purpose: Set b:txtfmt_cfg_starttok and b:txtfmt_cfg_formats options, taking
" into account any user-setting of 'tokrange' option, and if necessary, the
" txtfmt defaults, which may take 'encoding' into consideration.
" Note: If set via modeline, tokrange option value must be a literal tokrange
" specification; however, buf-local and global option variables may be set to
" either a valid tokrange spec, or a Vim expression that evaluates to one.
" Permitting arbitrary Vim expressions facilitates the use of complex tokrange
" selection logic, implemented by a user-defined expression or function.
"  Examples:
"      'g:My_tokrange_calculator()'
"      '&enc == "utf-8" ? 1400l : 130s' 
fu! s:Set_tokrange_old()
	" Cache the 'encoding' in effect
	let enc = &encoding
	" Determine the corresponding encoding class
	let enc_class = TxtfmtCommon_Encoding_get_class(enc)
	if !exists('b:txtfmt_cfg_tokrange') || strlen(b:txtfmt_cfg_tokrange) == 0
		" Either option wasn't set within modeline, or it was set to invalid
		" value.
		if exists('b:txtfmt_cfg_tokrange') && strlen(b:txtfmt_cfg_tokrange) == 0
			" Bad modeline set
			let l:warnmsg =
				\"Warning: Ignoring invalid modeline value for txtfmt `tokrange' option"
		elseif exists('b:txtfmtTokrange')
			" User overrode buf-local option. Save the option for validation
			" below...
			let b:txtfmt_cfg_tokrange = b:txtfmtTokrange
			let l:set_by = 'b'
		elseif exists('g:txtfmtTokrange')
			" User overrode global option. Save the option for validation
			" below...
			let b:txtfmt_cfg_tokrange = g:txtfmtTokrange
			let l:set_by = 'g'
		endif
	endif
	if exists('l:set_by') && (l:set_by == 'b' || l:set_by == 'g')
		" Perform special validation for buf-local/global settings, which
		" permits either a tokrange spec or a Vim expression that evaluates to
		" one.
		if !s:Tokrange_is_valid(b:txtfmt_cfg_tokrange)
			" Not a valid tokrange literal. Let's see whether it evaluates to
			" one.
			let v:errmsg = ''
			" Evaluate expression, using silent! to prevent problems in the
			" event that rhs is invalid.
			silent! exe 'let l:tokrange = '.b:txtfmt_cfg_tokrange
			if v:errmsg != ''
				" Bad expression
				let l:warnmsg =
					\"Warning: Ignoring invalid ".(l:set_by == 'b' ? 'buf-local' : 'global')
					\." value for txtfmt 'tokrange' option: ".b:txtfmt_cfg_tokrange
				" Discard the invalid setting
				let b:txtfmt_cfg_tokrange = ''
			else
				" It was a valid Vim expression. Did it produce a valid
				" tokrange spec?
				if !s:Tokrange_is_valid(l:tokrange)
					let l:warnmsg =
						\"Ignoring ".(l:set_by == 'b' ? 'buf-local' : 'global')
						\." set of txtfmt `tokrange' option: `".b:txtfmt_cfg_tokrange
						\."' produced invalid option value: ".l:tokrange
					" Discard the invalid setting
					let b:txtfmt_cfg_tokrange = ''
				else
					" Save the valid setting
					let b:txtfmt_cfg_tokrange = l:tokrange
				endif
			endif
		endif
	endif
	" Warn user if invalid user-setting is about to be overridden
	if exists('l:warnmsg')
		echoerr l:warnmsg
	endif
	" Any user settings have now been processed. Loop until
	" b:txtfmt_cfg_starttok and b:txtfmt_cfg_formats have been set. (If user
	" has requested a starttok that causes encoding-specific upper bound to be
	" exceeded, it will take 2 tries; otherwise, it will take only 1.)
	while !exists('b:txtfmt_cfg_starttok')
		" If b:txtfmt_cfg_tokrange is still undefined, see whether there's an
		" encoding-specific default.
		if !exists('b:txtfmt_cfg_tokrange') || strlen(b:txtfmt_cfg_tokrange) == 0
			" TODO - Put any logic that depends upon specific encoding here...
			" .
			" .

		endif
		" If b:txtfmt_cfg_tokrange is still undefined, see whether there's an
		" encoding-class-specific default.
		if !exists('b:txtfmt_cfg_tokrange') || strlen(b:txtfmt_cfg_tokrange) == 0
			if enc_class == '1'
				let b:txtfmt_cfg_tokrange = b:txtfmt_const_tokrange_def
			elseif enc_class == '2'
				let b:txtfmt_cfg_tokrange = b:txtfmt_const_tokrange_def
			elseif enc_class == 'u'
				let b:txtfmt_cfg_tokrange = b:txtfmt_const_tokrange_def
			else
				let b:txtfmt_cfg_tokrange = b:txtfmt_const_tokrange_def
			endif
			let l:skip_upr_bnd_test = 1
		endif
		" We now have a valid tokrange spec. Split into starttok and formats
		let b:txtfmt_cfg_starttok = s:Tokrange_get_starttok(b:txtfmt_cfg_tokrange)
		" Formats is a bit special. For one thing, it can be omitted from a
		" tokrange spec, in which case we'll choose a default. Also, long
		" formats ('L') can mean either 'all' or 'all_but_undercurl',
		" depending upon Vim version. (Undercurl was not supported until Vim
		" version 7.0.)
		let formats = s:Tokrange_get_formats(b:txtfmt_cfg_tokrange)
		" If 'formats' is not yet explicit, make it so.
		if strlen(formats) == 0
			" Default to 'short' formats
			let formats = 'S'
		endif
		" Translate formats into the appropriate b:txtfmt_cfg_formats value
		if formats == 'L'
			" Long formats
			if v:version >= 700
				let b:txtfmt_cfg_formats = 'all'
			else
				let b:txtfmt_cfg_formats = 'all_but_undercurl'
			endif
		else
			" Short formats
			let b:txtfmt_cfg_formats = 'basic'
		endif
		" Perform upper-bound validation unless we know the range is valid
		if !exists('l:skip_upr_bnd_test')
			if b:txtfmt_cfg_starttok + b:txtfmt_const_tokrange_size_{b:txtfmt_cfg_formats} - 1
				\ > b:txtfmt_const_tokrange_limit_{enc_class}
				" Warn user and use default
				echoerr
					\ "Warning: Tokrange value '".b:txtfmt_cfg_tokrange."' causes upper"
					\." bound to be exceeded for encoding ".&enc
				" Set to default
				let b:txtfmt_cfg_tokrange = b:txtfmt_const_tokrange_def
				" Don't bother testing next time through
				let l:skip_upr_bnd_test = 1
				" Make sure we go through the loop again
				unlet! b:txtfmt_cfg_starttok b:txtfmt_cfg_formats
			endif
		endif
	endwhile
	" We're done with b:txtfmt_cfg_tokrange now that it has been completely
	" decomposed.
	unlet! b:txtfmt_cfg_tokrange
endfu
" >>>
" Function: s:Set_syncing() <<<
" Purpose: Set b:txtfmt_cfg_syncmethod and (if applicable) b:txtfmt_cfg_synclines
" options, according to the following logic:
" 1) If user set sync option via modeline, buffer-local option, or global
" option, attempt to use the setting with the highest priority.
" 2) If step 1 fails to set the option, either because of error or because the
" user made no attempt to set, default to minlines=250
" Note: From a user perspective, there is only the 'sync' option. For
" convenience within the plugin, we break this single option into two options:
" 'syncmethod' and 'synclines'. Currently, 'synclines' is used only when
" syncmethod=minlines.
fu! s:Set_syncing()
	if !exists('b:txtfmt_cfg_sync') || strlen(b:txtfmt_cfg_sync) == 0
		" Either option wasn't set within modeline, or it was set to invalid
		" value.
		if exists('b:txtfmt_cfg_sync') && strlen(b:txtfmt_cfg_sync) == 0
			" Bad modeline set
			let l:bad_set_by = 'm'
		elseif exists('b:txtfmtSync')
			" User overrode buf-local option
			if s:Sync_is_valid(b:txtfmtSync)
				let b:txtfmt_cfg_sync = b:txtfmtSync
			else
				let l:bad_set_by = 'b'
			endif
		elseif exists('g:txtfmtSync')
			" User overrode global option
			if s:Sync_is_valid(g:txtfmtSync)
				let b:txtfmt_cfg_sync = g:txtfmtSync
			else
				let l:bad_set_by = 'g'
			endif
		endif
	endif
	" Warn user if invalid user-setting is about to be overridden
	if exists('l:bad_set_by')
		" Note: Display the offending option value for buf-local or global
		" option, but not for modeline, since modeline processing has already
		" reported the error.
		echoerr "Warning: Ignoring invalid ".(
			\l:bad_set_by == 'm' ? "modeline" :
			\l:bad_set_by == 'b' ? "buf-local" :
			\"global") . " value for txtfmt `sync' option" . (
			\l:bad_set_by == 'm' ? '' :
			\l:bad_set_by == 'b' ? (': ' . b:txtfmtSync) :
			\(': ' . g:txtfmtSync)
	endif
	if !exists('b:txtfmt_cfg_sync') || strlen(b:txtfmt_cfg_sync) == 0
		" Set to default
		let b:txtfmt_cfg_syncmethod = 'minlines'
		let b:txtfmt_cfg_synclines = 250
	else
		" Decompose validated 'sync' option into 'syncmethod' and (if
		" applicable) 'synclines'
		let b:txtfmt_cfg_syncmethod = s:Sync_get_method(b:txtfmt_cfg_sync)
		if b:txtfmt_cfg_syncmethod == 'minlines'
			" Save the number of lines
			let b:txtfmt_cfg_synclines = (0 + b:txtfmt_cfg_sync)
		endif
	endif
	" We're done with b:txtfmt_cfg_sync now that it has been completely
	" decomposed.
	unlet! b:txtfmt_cfg_sync
endfu
" >>>
" Function: s:Translate_color_optstr() <<<
" Purpose: Process the string representing a single element from the array
" txtfmtColor{1..8}, and return the extracted information.
" Return: A comma-separated string containing the extracted information as
" follows:
" <namepat>,<ctermfg_rhs>,<guifg_rhs>
" Note that ctermfg_rhs and/or guifg_rhs may be blank, in the event that user
" specified term patterns and none of them matched. (If no term patterns are
" specified, there is an implied match with current &term value.)
" Note that return string will have commas and backslashes escaped.
" Note that the last color def that matches for each of guifg and ctermfg is
" the one that is returned to caller.
" Error: Set s:err_str and return empty string
" Details:
" Here is the format of a single string in the txtfmtColor{} array:
" <namepat>,<clrdef1>[,<clrdef2>,...,<clrdefN>]
" <clrdef> :=
" 	<c|g>[<termpatlist>]:<clrstr>
" 	<termpatlist> :=
" 		:<termpat1>:<termpat2>:...:<termpatN>
" *** Parse table ***
" st	next_st		can_end?	tok
" 0		1			n			<namepat>
" 1		2			n			,
" 2		3			n			<c|g>
" 3		4			n			:
" 4		5			n			str
" 5		4			y			:
" 5		2			y			,
" Example color optstr:
" red,c:xterm:dosterm:DarkRed,g:builtin_gui:other_gui:#FF0000
"
" Here are the meanings of the fields:
" <namepat>	-regex used to recognize the token used to specify a
" 	certain color; e.g., when prompted by one of the insert token maps. May
" 	not contain spaces or commas.
" 	Example: k\|b\%[lack]
" <c|g>				-specifies whether clrstr will be the rhs of a
" 	"ctermfg=" ('c'), or "guifg=" ('g')
" <termpat>	-pattern used to match against &term option. It is a regex pattern
" 	which will be applied as ^<termpat>$
" <clrstr>	-rhs of a "ctermfg=" or "guifg=" assignment.
" 	:help gui-colors | help cterm-colors
" Additional Note:
" Commas, colons, and backslashes appearing in fields must be
" backslash-escaped.
" Note: Due to the extra backslash escaping, it is recommended to use a
" string-literal, rather than double-quoted string.
fu! s:Translate_color_optstr(optstr)
	" optstr is the string to be parsed
	" Initialize the state machine
	let pst = 0
	" Initialize the parse engine to consider tokens to be broken only at
	" unescaped commas and colons.
	let pid = s:Parse_init(a:optstr, '\%(\\.\|[^,:]\)\+')
	if pid < 0
		let s:err_str = 'Internal error within s:Translate_color_optstr(). Contact developer.'
		echomsg 'Internal error'
		return ''
	endif
	" Extract and handle tokens in a loop
	let parse_complete = 0
	" The following 2 will be set in loop (hopefully at least 1 anyways)
	let ctermfg_rhs = ''
	let guifg_rhs = ''
	while !parse_complete
		let tok = s:Parse_nexttok(pid)
		" Note: Could handle end of string here for states in which end of
		" string is illegal - but that would make it more difficult to
		" formulate meaningful error messages.
		"if tok == '' && pst != 5
		"endif
		" Switch on the current state
		if pst == 0	" Extract non empty namepat
			let namepat = substitute(tok, '\\\(.\)', '\1', 'g')
			if namepat =~ '^[[:space:]]*$'
				let s:err_str = "Color def string must contain at least 1 non-whitespace char"
				return ''
			endif
			let pst = 1
		elseif pst == 1
			if tok == ','
				let pst = 2
			elseif tok == ''
				let s:err_str = "Expected comma, encountered end of color def string"
				return ''
			else
				let s:err_str = "Expected comma, got '".tok."'"
				return ''
			endif
		elseif pst == 2
			if tok == 'c' || tok == 'g'
				let pst = 3
				let c_or_g = tok
				" Do some initializations for this cterm/gui
				let tp_or_cs_cnt = 0
				let got_term_match = 0
			elseif tok == ''
				let s:err_str = "Expected 'c' or 'g', encountered end of color def string"
				return ''
			else
				let s:err_str = "Expected 'c' or 'g', got '".tok."'"
				return ''
			endif
		elseif pst == 3
			if tok == ':'
				let pst = 4
			elseif
				let s:err_str = "Expected ':', encountered end of color def string"
				return ''
			else
				let s:err_str = "Expected ':', got '".tok."'"
				return ''
			endif
		elseif pst == 4
			let pst = 5
			" Do some processing with this and possibly previous termpat or
			" clrstr token. Note that if previous one exists, it is termpat;
			" we can't yet know what current one is.
			let termpat_or_clrstr = substitute(tok, '\\\(.\)', '\1', 'g')
			if termpat_or_clrstr =~ '^[[:space:]]*$'
				let s:err_str = "Term patterns and color strings must contain at least one non-whitespace char"
				return ''
			endif
			" If here, update the count. Note that we won't know whether this
			" is termpat or clrstr until next time here.
			let tp_or_cs_cnt = tp_or_cs_cnt + 1
			if !got_term_match && tp_or_cs_cnt > 1
				" Process saved string as termpat
				" Pattern has implied ^ and $. Also, termpat may contain \c
				if &term =~ '^'.tp_or_cs_str.'$'
					" Found a match!
					let got_term_match = 1
				endif
			endif
			" Save current token for processing next time
			let tp_or_cs_str = termpat_or_clrstr
		elseif pst == 5
			if tok == ':'		" another termpat or clrstr
				let pst = 4
			elseif tok == ',' 	" another cterm/gui
				let pst = 2
			elseif tok == ''	" end of string - legal in state 5
				let parse_complete = 1
			else				" illegal token
				let s:err_str = "Unexpected input in color def string: ".tok
				return ''
			endif
			if tok == ',' || tok == ''
				" Need to process saved data from pst 4, which we now know to
				" be a clrstr.
				if tp_or_cs_cnt == 1 || got_term_match
					" Either no termpats were specified (implied match with
					" &term) or one of the termpats matched.
					" Note that prior ctermfg/guifg rhs strings may be
					" overwritten here, if more than one cterm/gui def exists
					" and has a match.
					if c_or_g == 'c'
						let ctermfg_rhs = tp_or_cs_str
					else
						let guifg_rhs = tp_or_cs_str
					endif
				endif
			endif
		endif
	endwhile
	" Construct the return string:
	" <namepat>,<ctermfg_rhs>,<guifg_rhs>
	let ret_str = escape(namepat, ',\').','
		\.escape(ctermfg_rhs, ',\').','.escape(guifg_rhs, ',\')
	return ret_str
endfu
" >>>
" Function: s:Process_color_options() <<<
" Purpose: Process the special color definition arrays:
" b:txtfmtColor{1..8}
" g:txtfmtColor{1..8}
" s:txtfmt_clr{1..8}
" Note that the b: and g: versions may be used to override the defaults. Cterm
" and gui may be overridden separately, and color definition strings may even
" take into account the value of &term. Note that for each possible element in
" the array, there is a default element (in s:txtfmt_clr{1..8}), which will be
" used if user has not overriden.
" Return: indirect only
" Builds the following buffer-scope arrays: (indexed by color)
" b:txtfmt_clr_namepat{}, b:txtfmt_clr{}
" Details: The format of the color array is as follows:
" The array has 8 elements (1..8), each of which represents a color region
" begun with one of the 8 successive color tokens in the token range. Each
" element is a string whose format is described in header of
" s:Translate_color_optstr()
" Rules: For each color index in range i = 1..8, check up to 3 color
" definition elements in the following order:
" b:txtfmtColor{i}
" g:txtfmtColor{i}
" s:txtfmt_clr{i}
" Set b:txtfmt_clr{} and move to next color index as soon as a suitable
" definition is found. Suitability is determined by checking
" has('gui_running') against the 'g:' or 'c:' in the color definition string,
" and if necessary, by matching the current value of 'term' against a 'term'
" pattern in the color definition string.
" Note: This function was rewritten on 10May2008. A corresponding partial
" rewrite of s:Translate_color_optstr is probably in order, but is not
" necessary, so I've left the latter function completely intact for now. (We
" don't really need both ctermfg and guifg values any more, but
" s:Translate_color_optstr still returns both...)
fu! s:Process_color_options()
	let i = 1
	" Loop over all colors (1st non-default color at index 1)
	while i < b:txtfmt_num_colors
		" Init strings to value signifying not specified or error
		let namepat = '' | let clr_rhs = ''
		" Loop over b:, g:, s:
		let array{0} = 'b:txtfmtColor'
		let array{1} = 'g:txtfmtColor'
		let array{2} = 's:txtfmt_clr'
		let j = 0
		while j < 3
			" Skip nonexistent color definitions
			if exists(array{j}.'{'.i.'}')
				exe 'let l:el = '.array{j}.'{'.i.'}'
				" If here, color definition exists. Let's see whether it contains
				" a match...
				let s = s:Translate_color_optstr(el)
				if s != ''
					" Extract fields from the escaped return string (which is
					" internally generated, and hence, does not require
					" validation)
					" TODO - Perhaps standardize this in one spot (function or
					" static return variables)
					let re_fld = '\%(\%(\\.\|[^,]\)*\)'
					let re_sfld = '\(\%(\\.\|[^,]\)*\)'
					let namepat = substitute(s, re_sfld.'.*', '\1', '')
					if has('gui_running')
						let clr_rhs = substitute(s, re_fld.','.re_fld.','.re_sfld, '\1', '')
					else
						let clr_rhs = substitute(s, re_fld.','.re_sfld.'.*', '\1', '')
					endif
					" Note: clr_rhs may be null at this point; if so, there
					" was no applicable color definition, though the color def
					" element was valid
					if strlen(clr_rhs)
						" Remove extra level of backslashes
						let namepat = substitute(namepat, '\\\(.\)', '\1', 'g')
						let clr_rhs = substitute(clr_rhs, '\\\(.\)', '\1', 'g')
					endif
				elseif array{j}[0] == 'b' || array{j}[0] == 'g'
					echomsg "Ignoring invalid user-specified color def ".array{j}.i." due to error: "
						\.s:err_str
				else
					" Shouldn't get here! Problem with defaults...
					echomsg "Internal error within Process_color_options - bad default for txtfmtColors"
						\.i." - contact developer."
				endif
				" Are we done yet?
				if strlen(clr_rhs)
					break
				endif
			endif
			let j = j + 1
		endwhile
		" Assumption: Lack of color rhs at this point implies internal error.
		" Build the buffer-specific array used in syntax file...
		" Note: i-1 accounts for the fact that the arrays over which we are
		" looping are 1-based, but the namepat and fg arrays to which we are
		" about to assign are 0-based.
		let b:txtfmt_clr_namepat{i-1} = namepat
		let b:txtfmt_clr{i-1} = clr_rhs
		" Advance to next color
		let i = i + 1
	endwhile
endfu
" >>>
" Function: s:Process_txtfmt_modeline() <<<
" Purpose: Determine whether input line is a valid txtfmt modeline. Process
" options if so. If required by s:txtfmt_ml_new_<...> variables, change
" options in the modeline itself. (Example: change 'tokrange' as requested by
" preceding call to :MoveStartTok command.)
" Return:
" 0		- no txtfmt modeline found
" -1	- txtfmt modeline containing error (bad option)
" 1		- valid txtfmt modeline found and processed
" Note: Modeline format is described under function s:Is_txtfmt_modeline.
fu! s:Process_txtfmt_modeline(line)
	" Obtain the line to be processed
	let linestr = getline(a:line)
	" Is the line a modeline?
	if !s:Is_txtfmt_modeline(linestr)
		" Note: This is not considered error - the line is simply not a
		" modeline
		return 0
	endif
	" If here, overall format is correct. Are all options valid?
	" Assume valid modeline until we find out otherwise...
	let ret_val = 1
	" Save the leading text, in case we need to create a version of the line
	" with changed options (e.g., starttok change)
	let leading = substitute(linestr, s:re_modeline, '\1', '')
	" The middle (options) part will be built up as we go
	let middle = ''
	" Save the trailing stuff
	let trailing = substitute(linestr, s:re_modeline, '\3', '')
	" Extract the {options} portion (leading/trailing stuff removed) for
	" processing
	let optstr = substitute(linestr, s:re_modeline, '\2', '')
	" Extract pieces from head of optstr as long as unprocessed options exist
	" Note: The following pattern may be used to extract element separators
	" into \1, opt name into \2, opt value (if it exists) into \3, and to
	" strip all three from the head of the string. (The remainder of the
	" modeline will be in \4.)
	" Note: Element separator is optional in the following re, since it won't
	" exist for first option, and we've already verified that it exists
	" between all other options.
	let re_opt = '\('.s:re_ml_elsep.'\)\?\('
				\.s:re_ml_name.'\)\%(=\('.s:re_ml_val.'\)\)\?\(.*\)'
	" Set this flag if we make a requested change to an option, which needs to
	" be reflected in the actual modeline text in the buffer (e.g., starttok
	" change)
	let line_changed = 0
	" Loop over all options, exiting loop early if error occurs
	while strlen(optstr) > 0 && ret_val != -1
		" Accumulate the option separator text
		let middle = middle.substitute(optstr, re_opt, '\1', '')
		" Extract option name and value
		let optn = substitute(optstr, re_opt, '\2', '')
		let optv = substitute(optstr, re_opt, '\3', '')
		" Remove the option about to be processed from head of opt str
		let optstr = substitute(optstr, re_opt, '\4', '')
		" Validate the option(s)
		if optn == 'tokrange' || optn == 'rng'
			"format: tokrange=<char_code>[sSlL]
			"Examples: '130s' '1500l' '130' '0x2000L'
			if !s:Tokrange_is_valid(optv)
				" 2 cases when option is invalid:
				" 1) We can fix the invalid tokrange by changing to the value
				"    specified by user in call to :MoveStartTok
				" 2) We must display error and use default
				if exists('s:txtfmt_ml_new_starttok')
					" Design Decision: Since option value is not valid, don't
					" attempt to preserve any existing 'formats' specifier
					let optv = s:txtfmt_ml_new_starttok.s:txtfmt_ml_new_formats
					let line_changed = 1
				else
					let s:err_str = "Invalid 'tokrange' value - must be hex or dec"
								\." char code value optionally followed by one of [sSlL]"
					" Record the attempt to set b:txtfmt_cfg_tokrange from modeline
					let b:txtfmt_cfg_tokrange = ''
					let ret_val = -1
				endif
			elseif exists('s:txtfmt_ml_new_starttok')
				" Change the starttok value, preserving both the starttok
				" number format (note that s:txtfmt_ml_new_starttok is
				" actually a string) and any existing 'formats' specification
				" Note: Since modeline setting trumps all others, an existing
				" setting should agree with s:txtfmt_ml_new_formats anyways.
				let optv = substitute(optv, b:txtfmt_re_number_atom, s:txtfmt_ml_new_starttok, '')
				let line_changed = 1
			endif
			" Save the option value, deferring processing till later...
			let b:txtfmt_cfg_tokrange = optv
		elseif optn == 'sync'
			"format: sync={<hex>|<dec>|fromstart|none}
			"Examples: 'sync=300' 'sync=0x1000' 'sync=fromstart' 'sync=none'
			if !s:Sync_is_valid(optv)
				let s:err_str = "Invalid 'sync' value - must be one of the"
							\." following: <numeric literal>, 'fromstart'"
							\.", 'none'"
				" Record the attempt to set b:txtfmt_cfg_sync from modeline
				let b:txtfmt_cfg_sync = ''
				let ret_val = -1
			endif
			" Defer processing of sync till later
			let b:txtfmt_cfg_sync = optv
		elseif optn =~ '^\(no\)\?\(nested\|nst\)$'
			" Make sure no option value was supplied to binary option
			if strlen(optv)
				let s:err_str = "Cannot assign value to binary txtfmt option 'nested'"
				let ret_val = -1
			endif
			" Option has been explicitly turned on or off
			let b:txtfmt_cfg_nested = optn =~ '^no' ? 0 : 1
		elseif optn == 'escape' || optn == 'esc'
			"format: escape=[bslash|self|none]
			if optv == 'bslash'
				let b:txtfmt_cfg_escape = 'bslash'
			elseif optv == 'self'
				let b:txtfmt_cfg_escape = 'self'
			elseif optv == 'none'
				let b:txtfmt_cfg_escape = 'none'
			else
				let s:err_str = "Invalid 'escape' value - must be 'bslash', 'self', or 'none'"
				let ret_val = -1
			endif
		else
		   let s:err_str = "Unknown txtfmt modeline option: ".optn
		   let ret_val = -1
		endif
		" Append optn[=optv] to middle
		let middle = middle.optn.(strlen(optv) ? '='.optv : '')
	endwhile
	" Processed txtfmt modeline without error
	if line_changed
		" Alter the line to reflect any option changes
		" Note: If error occurred above, optstr may be non-empty, in which
		" case, we need to append it to the already processed options in
		" middle.
		call setline(a:line, leading.middle.optstr.trailing)
	endif
	return ret_val
endfu
" >>>
" Function: s:Do_txtfmt_modeline() <<<
" Purpose: Look for txtfmt "modelines" of the following form:
" .\{-}<whitespace>txtfmt:<definitions>:
" which appear within the first or last 'modelines' lines in the buffer.
" Note: Function will search a number of lines (at start and end of buffer),
" as determined from 'modelines', provided that this value is nonzero. If
" 'modelines' is 0, default of 5 lines at beginning and end will be searched.
" Return:
" 0     - no txtfmt modeline found
" N     - N valid txtfmt modelines found and processed
" -N    - txtfmt modeline processing error occurred on the Nth modeline
"         processed
" Error_handling: If error is encountered in a modeline, the remainder of
" the offending modeline is discarded, and modeline processing is aborted;
" i.e., no more lines are searched. This is consistent with the way Vim's
" modeline processing works.
fu! s:Do_txtfmt_modeline()
	" Check for request to change starttok
	if exists('b:txtfmt_event_new_starttok')
		" Communicate the request to modeline processing function.
		" TODO - Decide whether to use optional function parameters.
		let s:txtfmt_ml_new_starttok = b:txtfmt_event_new_starttok
		let s:txtfmt_ml_new_formats = b:txtfmt_event_new_formats
		" Unlet the original to ensure that if we abort with error, we don't
		" do this next time
		" TODO - Decide whether that is best, or whether it's best to keep
		" trying until it's done...
		unlet! b:txtfmt_event_new_starttok b:txtfmt_event_new_formats
	else
		unlet! s:txtfmt_ml_new_starttok s:txtfmt_ml_new_formats
	endif
	" Keep up with # of modelines actually encountered
	let l:ml_seen = 0
	" How many lines should we search?
	" Priority is given to txtfmtModelines user option if it exists
	if exists('b:txtfmtModelines') || exists('g:txtfmtModelines')
		" User can disable by setting to 0
		let mls_use = exists('b:txtfmtModelines') ? b:txtfmtModelines : g:txtfmtModelines
	else
		" Use 'modelines' option+1 unless it's not a valid nonzero value, in
		" which case, we use default of 5
		" NOTE: 1 is added to 'modelines' option so that if modeline is at
		" highest or lowest possible line, putting a txtfmt modeline above or
		" below it, respectively, will work.
		let mls_use = &modelines > 0 ? &modelines+1 : 5
	endif
	let nlines = line('$')
	" Check first modelines lines
	" TODO - Combine the 2 loops into one, which can alter the loop counter in
	" a stepwise manner.
	let i = 1
	while i <= mls_use && i <= nlines
		let status = s:Process_txtfmt_modeline(i)
		if status == 1
			 " Successfully extracted options
			 let l:ml_seen = l:ml_seen + 1
		elseif status == -1
			 " Error processing the modeline
			 echoerr "Ignoring txtfmt modeline on line ".i.": ".s:err_str
			 return -(l:ml_seen + 1)
		endif
		" Keep looking...
		let i = i + 1
	endwhile
	" Check last modelines lines
	let i = nlines - mls_use + 1
	" Adjust if necessary to keep from checking already checked lines
	if i <= mls_use
		let i = mls_use + 1
	endif
	while i <= nlines
		let status = s:Process_txtfmt_modeline(i)
		if status == 1
			 " Successfully extracted options
			 let l:ml_seen = l:ml_seen + 1
		elseif status == -1
			 " Error processing the modeline
			 echoerr "Ignoring txtfmt modeline on line ".i.": ".s:err_str
			 return -(l:ml_seen + 1)
		endif
		" Keep looking...
		let i = i + 1
	endwhile
	" If here, we encountered no error. Return the number of modelines
	" processed (could be zero)
	return l:ml_seen
endfu
" >>>
" Function: s:Define_fmtclr_vars() <<<
fu! s:Define_fmtclr_vars()
	" Format definition array <<<
	" NOTE: This array is used for rhs of syntax definitions, but also for display
	" by ShowTokenMap.
	let b:txtfmt_fmt{0}  = 'NONE'
	let b:txtfmt_fmt{1}  = 'underline'
	let b:txtfmt_fmt{2}  = 'bold'
	let b:txtfmt_fmt{3}  = 'underline,bold'
	let b:txtfmt_fmt{4}  = 'italic'
	let b:txtfmt_fmt{5}  = 'underline,italic'
	let b:txtfmt_fmt{6}  = 'bold,italic'
	let b:txtfmt_fmt{7}  = 'underline,bold,italic'
	if b:txtfmt_cfg_formats == 'basic'
		let b:txtfmt_num_formats = 8
	elseif b:txtfmt_cfg_formats == 'all' || b:txtfmt_cfg_formats == 'all_but_undercurl'
		" 'extended' formats
		let b:txtfmt_fmt{8}  = 'standout'
		let b:txtfmt_fmt{9}  = 'underline,standout'
		let b:txtfmt_fmt{10} = 'bold,standout'
		let b:txtfmt_fmt{11} = 'underline,bold,standout'
		let b:txtfmt_fmt{12} = 'italic,standout'
		let b:txtfmt_fmt{13} = 'underline,italic,standout'
		let b:txtfmt_fmt{14} = 'bold,italic,standout'
		let b:txtfmt_fmt{15} = 'underline,bold,italic,standout'
		let b:txtfmt_fmt{16} = 'reverse'
		let b:txtfmt_fmt{17} = 'underline,reverse'
		let b:txtfmt_fmt{18} = 'bold,reverse'
		let b:txtfmt_fmt{19} = 'underline,bold,reverse'
		let b:txtfmt_fmt{20} = 'italic,reverse'
		let b:txtfmt_fmt{21} = 'underline,italic,reverse'
		let b:txtfmt_fmt{22} = 'bold,italic,reverse'
		let b:txtfmt_fmt{23} = 'underline,bold,italic,reverse'
		let b:txtfmt_fmt{24} = 'standout,reverse'
		let b:txtfmt_fmt{25} = 'underline,standout,reverse'
		let b:txtfmt_fmt{26} = 'bold,standout,reverse'
		let b:txtfmt_fmt{27} = 'underline,bold,standout,reverse'
		let b:txtfmt_fmt{28} = 'italic,standout,reverse'
		let b:txtfmt_fmt{29} = 'underline,italic,standout,reverse'
		let b:txtfmt_fmt{30} = 'bold,italic,standout,reverse'
		let b:txtfmt_fmt{31} = 'underline,bold,italic,standout,reverse'
		" If using undercurl (introduced in Vim 7.0), there will be twice as
		" many formats.
		if b:txtfmt_cfg_formats == 'all_but_undercurl'
			let b:txtfmt_num_formats = 32
		else " all
			let b:txtfmt_fmt{32} = 'undercurl'
			let b:txtfmt_fmt{33} = 'underline,undercurl'
			let b:txtfmt_fmt{34} = 'bold,undercurl'
			let b:txtfmt_fmt{35} = 'underline,bold,undercurl'
			let b:txtfmt_fmt{36} = 'italic,undercurl'
			let b:txtfmt_fmt{37} = 'underline,italic,undercurl'
			let b:txtfmt_fmt{38} = 'bold,italic,undercurl'
			let b:txtfmt_fmt{39} = 'underline,bold,italic,undercurl'
			let b:txtfmt_fmt{40} = 'standout,undercurl'
			let b:txtfmt_fmt{41} = 'underline,standout,undercurl'
			let b:txtfmt_fmt{42} = 'bold,standout,undercurl'
			let b:txtfmt_fmt{43} = 'underline,bold,standout,undercurl'
			let b:txtfmt_fmt{44} = 'italic,standout,undercurl'
			let b:txtfmt_fmt{45} = 'underline,italic,standout,undercurl'
			let b:txtfmt_fmt{46} = 'bold,italic,standout,undercurl'
			let b:txtfmt_fmt{47} = 'underline,bold,italic,standout,undercurl'
			let b:txtfmt_fmt{48} = 'reverse,undercurl'
			let b:txtfmt_fmt{49} = 'underline,reverse,undercurl'
			let b:txtfmt_fmt{50} = 'bold,reverse,undercurl'
			let b:txtfmt_fmt{51} = 'underline,bold,reverse,undercurl'
			let b:txtfmt_fmt{52} = 'italic,reverse,undercurl'
			let b:txtfmt_fmt{53} = 'underline,italic,reverse,undercurl'
			let b:txtfmt_fmt{54} = 'bold,italic,reverse,undercurl'
			let b:txtfmt_fmt{55} = 'underline,bold,italic,reverse,undercurl'
			let b:txtfmt_fmt{56} = 'standout,reverse,undercurl'
			let b:txtfmt_fmt{57} = 'underline,standout,reverse,undercurl'
			let b:txtfmt_fmt{58} = 'bold,standout,reverse,undercurl'
			let b:txtfmt_fmt{59} = 'underline,bold,standout,reverse,undercurl'
			let b:txtfmt_fmt{60} = 'italic,standout,reverse,undercurl'
			let b:txtfmt_fmt{61} = 'underline,italic,standout,reverse,undercurl'
			let b:txtfmt_fmt{62} = 'bold,italic,standout,reverse,undercurl'
			let b:txtfmt_fmt{63} = 'underline,bold,italic,standout,reverse,undercurl'
			let b:txtfmt_num_formats = 64
		endif
	endif
	" >>>
	" <<< Default color definition array
	let s:txtfmt_clr{1} = '^\\%(k\\|bla\\%[ck]\\)$,c:Black,g:#000000'
	let s:txtfmt_clr{2} = '^b\\%[lue]$,c:DarkBlue,g:#0000FF'
	let s:txtfmt_clr{3} = '^g\\%[reen]$,c:DarkGreen,g:#00FF00'
	let s:txtfmt_clr{4} = '^t\\%[urquoise]$,c:LightGreen,g:#00FFFF'
	let s:txtfmt_clr{5} = '^r\\%[ed]$,c:DarkRed,g:#FF0000'
	let s:txtfmt_clr{6} = '^v\\%[iolet]$,c:DarkMagenta,g:#FF00FF'
	let s:txtfmt_clr{7} = '^y\\%[ellow]$,c:DarkYellow,g:#FFFF00'
	let s:txtfmt_clr{8} = '^w\\%[hite]$,c:White,g:#FFFFFF'
	" Note: The following variable indicates the total number of colors,
	" including 'no color', which is not in the txtfmt_clr array.
	let b:txtfmt_num_colors = 9
	" >>>
	" Set fmt/clr specific values for convenience
	" TODO NOTE - If desired, could allow the fmt/clr ranges to be split, in
	" which case, the following 2 would be user-configurable. For now, just
	" base them on txtfmtStarttok.
	let b:txtfmt_clr_first_tok = b:txtfmt_cfg_starttok
	let b:txtfmt_clr_last_tok = b:txtfmt_cfg_starttok + b:txtfmt_num_colors - 1
	let b:txtfmt_fmt_first_tok = b:txtfmt_cfg_starttok + b:txtfmt_num_colors
	let b:txtfmt_fmt_last_tok = b:txtfmt_cfg_starttok + b:txtfmt_num_colors + b:txtfmt_num_formats - 1
endfu
" >>>
" Function: s:Define_fmtclr_regexes() <<<
" Purpose: Define regexes involving the special fmt/clr tokens.
" Assumption: The following variable(s) has been defined for the buffer:
" b:txtfmt_cfg_starttok
" Note: The start tok is user-configurable. Thus, this function should be
" called only after processing options.
fu! s:Define_fmtclr_regexes()
	" Color region tokens
	let b:txtfmt_re_clr_tok_atom = '['.nr2char(b:txtfmt_clr_first_tok).'-'.nr2char(b:txtfmt_clr_last_tok).']'
	let b:txtfmt_re_clr_stok_atom = '['.nr2char(b:txtfmt_clr_first_tok + 1).'-'.nr2char(b:txtfmt_clr_last_tok).']'
	let b:txtfmt_re_clr_etok_atom = nr2char(b:txtfmt_clr_first_tok)
	" Format region tokens
	let b:txtfmt_re_fmt_tok_atom = '['.nr2char(b:txtfmt_fmt_first_tok).'-'.nr2char(b:txtfmt_fmt_last_tok).']'
	let b:txtfmt_re_fmt_stok_atom = '['.nr2char(b:txtfmt_fmt_first_tok + 1).'-'.nr2char(b:txtfmt_fmt_last_tok).']'
	let b:txtfmt_re_fmt_etok_atom = nr2char(b:txtfmt_fmt_first_tok)
	" Combined regions
	let b:txtfmt_re_any_tok_atom =
				\'['.nr2char(b:txtfmt_clr_first_tok).'-'.nr2char(b:txtfmt_clr_last_tok)
				\.nr2char(b:txtfmt_fmt_first_tok).'-'.nr2char(b:txtfmt_fmt_last_tok).']'
	let b:txtfmt_re_any_stok_atom =
				\'['.nr2char(b:txtfmt_clr_first_tok + 1).'-'.nr2char(b:txtfmt_clr_last_tok)
				\.nr2char(b:txtfmt_fmt_first_tok + 1).'-'.nr2char(b:txtfmt_fmt_last_tok).']'
	let b:txtfmt_re_any_etok_atom =
				\'['.nr2char(b:txtfmt_fmt_first_tok).nr2char(b:txtfmt_clr_first_tok).']'

	if b:txtfmt_cfg_escape == 'bslash'
		" The following pattern is a zero-width look-behind assertion, which
		" matches only at a non-backslash-escaped position.
		let noesc = '\%(\%(^\|[^\\]\)\%(\\\\\)*\\\)\@<!'
		" clr
		let b:txtfmt_re_clr_tok = noesc.b:txtfmt_re_clr_tok_atom
		let b:txtfmt_re_clr_stok = noesc.b:txtfmt_re_clr_stok_atom
		let b:txtfmt_re_clr_etok = noesc.nr2char(b:txtfmt_clr_first_tok)
		let b:txtfmt_re_clr_ntok = '\%('.b:txtfmt_re_clr_tok.'\)\@!.'
		" fmt
		let b:txtfmt_re_fmt_tok = noesc.b:txtfmt_re_fmt_tok_atom
		let b:txtfmt_re_fmt_stok = noesc.b:txtfmt_re_fmt_stok_atom
		let b:txtfmt_re_fmt_etok = noesc.nr2char(b:txtfmt_fmt_first_tok)
		let b:txtfmt_re_fmt_ntok = '\%('.b:txtfmt_re_fmt_tok.'\)\@!.'
		" clr/fmt combined
		let b:txtfmt_re_any_tok = noesc.b:txtfmt_re_any_tok_atom
		let b:txtfmt_re_any_stok = noesc.b:txtfmt_re_any_stok_atom
		let b:txtfmt_re_any_etok =
					\noesc.'['.nr2char(b:txtfmt_clr_first_tok).nr2char(b:txtfmt_fmt_first_tok).']'
		let b:txtfmt_re_any_ntok = '\%('.b:txtfmt_re_any_tok.'\)\@!.'
	elseif b:txtfmt_cfg_escape == 'self'
		" The following pattern serves as the template for finding tokens that
		" are neither escaping nor escaped.
		let tmpl = '\%(\(placeholder\)\%(\1\)\@!\)\@=\%(\%(^\|\%(\1\)\@!.\)\%(\1\1\)*\1\)\@<!.'
		" clr
		let b:txtfmt_re_clr_tok = substitute(tmpl, 'placeholder', b:txtfmt_re_clr_tok_atom, '')
		let b:txtfmt_re_clr_stok = substitute(tmpl, 'placeholder', b:txtfmt_re_clr_stok_atom, '')
		let b:txtfmt_re_clr_etok = substitute(tmpl, 'placeholder', nr2char(b:txtfmt_clr_first_tok), '')
		let b:txtfmt_re_clr_ntok = '\%('.b:txtfmt_re_clr_tok.'\)\@!.'
		" fmt
		let b:txtfmt_re_fmt_tok = substitute(tmpl, 'placeholder', b:txtfmt_re_fmt_tok_atom, '')
		let b:txtfmt_re_fmt_stok = substitute(tmpl, 'placeholder', b:txtfmt_re_fmt_stok_atom, '')
		let b:txtfmt_re_fmt_etok = substitute(tmpl, 'placeholder', nr2char(b:txtfmt_fmt_first_tok), '')
		let b:txtfmt_re_fmt_ntok = '\%('.b:txtfmt_re_fmt_tok.'\)\@!.'
		" clr/fmt combined
		let b:txtfmt_re_any_tok = substitute(tmpl, 'placeholder', b:txtfmt_re_any_tok_atom, '')
		let b:txtfmt_re_any_stok = substitute(tmpl, 'placeholder', b:txtfmt_re_any_stok_atom, '')
		let b:txtfmt_re_any_etok = substitute(tmpl, 'placeholder',
					\'['.nr2char(b:txtfmt_clr_first_tok).nr2char(b:txtfmt_fmt_first_tok).']', '')
		let b:txtfmt_re_any_ntok = '\%('.b:txtfmt_re_any_tok.'\)\@!.'
	else
		" No escaping of tokens
		" clr
		let b:txtfmt_re_clr_tok = b:txtfmt_re_clr_tok_atom
		let b:txtfmt_re_clr_stok = b:txtfmt_re_clr_stok_atom
		let b:txtfmt_re_clr_etok = nr2char(b:txtfmt_clr_first_tok)
		let b:txtfmt_re_clr_ntok = '[^'.nr2char(b:txtfmt_clr_first_tok).'-'.nr2char(b:txtfmt_clr_last_tok).']'
		" fmt
		let b:txtfmt_re_fmt_tok = b:txtfmt_re_fmt_tok_atom
		let b:txtfmt_re_fmt_stok = b:txtfmt_re_fmt_stok_atom
		let b:txtfmt_re_fmt_etok = nr2char(b:txtfmt_fmt_first_tok)
		let b:txtfmt_re_fmt_ntok = '[^'.nr2char(b:txtfmt_fmt_first_tok).'-'.nr2char(b:txtfmt_fmt_last_tok).']'
		" clr/fmt combined
		let b:txtfmt_re_any_tok = b:txtfmt_re_any_tok_atom
		let b:txtfmt_re_any_stok = b:txtfmt_re_any_stok_atom
		let b:txtfmt_re_any_etok =
					\'['.nr2char(b:txtfmt_clr_first_tok).nr2char(b:txtfmt_fmt_first_tok).']'
		let b:txtfmt_re_any_ntok =
					\'[^'
					\.nr2char(b:txtfmt_clr_first_tok).'-'.nr2char(b:txtfmt_clr_last_tok)
					\.nr2char(b:txtfmt_fmt_first_tok).'-'.nr2char(b:txtfmt_fmt_last_tok)
					\.']'
	endif
endfu
" >>>
" Function: s:Do_config_common() <<<
" Purpose: Set script local variables, taking into account whether user has
" overriden via txtfmt globals.
fu! s:Do_config_common()
	" unlet any buffer-specific options that may be set by txtfmt modeline <<<
	unlet! b:txtfmt_cfg_tokrange b:txtfmt_cfg_sync b:txtfmt_cfg_escape b:txtfmt_cfg_nested
	" >>>
	" Attempt to process modeline <<<
	let ml_status = s:Do_txtfmt_modeline()
	" >>>
	" 'tokrange' option <<<
	" Note: 'starttok' and 'formats' are distinct options internally, but may
	" be set only as a unit by the plugin user. Even if tokrange was set
	" within modeline, there is work yet to be done.
	call s:Set_tokrange()
	" >>>
	" 'sync' option <<<
	" Note: 'syncmethod' and 'synclines' are distinct options internally, but
	" may be set only as a unit by the plugin user. Even if sync was set
	" within modeline, there is work yet to be done.
	call s:Set_syncing()
	" >>>
	" 'escape' option <<<
	if !exists('b:txtfmt_cfg_escape') || strlen(b:txtfmt_cfg_escape) == 0
		" Either option wasn't set within modeline, or it was set to invalid
		" value
		unlet! l:bad_set_by
		if exists('b:txtfmt_cfg_escape') && strlen(b:txtfmt_cfg_escape) == 0
			" Bad modeline set
			let l:bad_set_by = 'm'
		elseif exists('b:txtfmtEscape')
			" User overrode buf-local option
			if s:Escape_is_valid(b:txtfmtEscape)
				let b:txtfmt_cfg_escape = b:txtfmtEscape
			else
				let l:bad_set_by = 'b'
			endif
		elseif exists('g:txtfmtEscape')
			" User overrode global option
			if s:Escape_is_valid(g:txtfmtEscape)
				let b:txtfmt_cfg_escape = g:txtfmtEscape
			else
				let l:bad_set_by = 'g'
			endif
		endif
		" Warn user if invalid user-setting is about to be overridden
		if exists('l:bad_set_by')
			" Note: Display the offending option value for buf-local or global
			" option, but not for modeline, since modeline processing has
			" already reported the error.
			echoerr "Warning: Ignoring invalid ".(
				\l:bad_set_by == 'm' ? "modeline" :
				\l:bad_set_by == 'b' ? "buf-local" :
				\"global") . " value for txtfmt `escape' option" . (
				\l:bad_set_by == 'm' ? '' :
				\l:bad_set_by == 'b' ? (': ' . b:txtfmtEscape) :
				\(': ' . g:txtfmtEscape)
		endif
		if !exists('b:txtfmt_cfg_escape') || strlen(b:txtfmt_cfg_escape) == 0
			" Set to default
			let b:txtfmt_cfg_escape = 'none'
		endif
	endif
	" >>>
	" 'nested' option <<<
	if !exists('b:txtfmt_cfg_nested') || strlen(b:txtfmt_cfg_nested) == 0
		" Either option wasn't set within modeline, or it was set to invalid
		" value
		if exists('b:txtfmt_cfg_nested') && strlen(b:txtfmt_cfg_nested) == 0
			" Bad modeline set. Warn user that we're about to override. Note
			" that modeline processing has already reported the specific
			" error.
			echoerr "Warning: Ignoring invalid modeline value for txtfmt `nested' option"
		elseif exists('b:txtfmtNested')
			" User overrode buf-local option
			" Note: Invalid setting impossible for boolean
			let b:txtfmt_cfg_nested = b:txtfmtNested
		elseif exists('g:txtfmtNested')
			" User overrode global option
			" Note: Invalid setting impossible for boolean
			let b:txtfmt_cfg_nested = g:txtfmtNested
		endif
		if !exists('b:txtfmt_cfg_nested') || strlen(b:txtfmt_cfg_nested) == 0
			" Set to default (on)
			let b:txtfmt_cfg_nested = 1
		endif
	endif
	" >>>
	" Define various buffer-specific variables now that fmt/clr ranges are fixed.
	" TODO: Perhaps combine the following 2 functions in some way...
	call s:Define_fmtclr_vars()
	" Define fmt/clr regexes - used in both syntax and ftplugin <<<
	call s:Define_fmtclr_regexes()
	" >>>
	" Process color options <<<
	call s:Process_color_options()
	" >>>
endfu
" >>>
call s:Do_config_common()
" Define buffer-local constants <<<
" For convenience, associate format indices with their respective
" '[u][b][i][s][r][c]' string, in fiducial form. Note that fiducial form may
" be used for display, but is also a valid (but not the only) fmt spec.
let b:ubisrc_fmt0  = '-'
let b:ubisrc_fmt1  = 'u'
let b:ubisrc_fmt2  = 'b'
let b:ubisrc_fmt3  = 'bu'
let b:ubisrc_fmt4  = 'i'
let b:ubisrc_fmt5  = 'iu'
let b:ubisrc_fmt6  = 'ib'
let b:ubisrc_fmt7  = 'ibu'
let b:ubisrc_fmt8  = 's'
let b:ubisrc_fmt9  = 'su'
let b:ubisrc_fmt10 = 'sb'
let b:ubisrc_fmt11 = 'sbu'
let b:ubisrc_fmt12 = 'si'
let b:ubisrc_fmt13 = 'siu'
let b:ubisrc_fmt14 = 'sib'
let b:ubisrc_fmt15 = 'sibu'
let b:ubisrc_fmt16 = 'r'
let b:ubisrc_fmt17 = 'ru'
let b:ubisrc_fmt18 = 'rb'
let b:ubisrc_fmt19 = 'rbu'
let b:ubisrc_fmt20 = 'ri'
let b:ubisrc_fmt21 = 'riu'
let b:ubisrc_fmt22 = 'rib'
let b:ubisrc_fmt23 = 'ribu'
let b:ubisrc_fmt24 = 'rs'
let b:ubisrc_fmt25 = 'rsu'
let b:ubisrc_fmt26 = 'rsb'
let b:ubisrc_fmt27 = 'rsbu'
let b:ubisrc_fmt28 = 'rsi'
let b:ubisrc_fmt29 = 'rsiu'
let b:ubisrc_fmt30 = 'rsib'
let b:ubisrc_fmt31 = 'rsibu'
let b:ubisrc_fmt32 = 'c'
let b:ubisrc_fmt33 = 'cu'
let b:ubisrc_fmt34 = 'cb'
let b:ubisrc_fmt35 = 'cbu'
let b:ubisrc_fmt36 = 'ci'
let b:ubisrc_fmt37 = 'ciu'
let b:ubisrc_fmt38 = 'cib'
let b:ubisrc_fmt39 = 'cibu'
let b:ubisrc_fmt40 = 'cs'
let b:ubisrc_fmt41 = 'csu'
let b:ubisrc_fmt42 = 'csb'
let b:ubisrc_fmt43 = 'csbu'
let b:ubisrc_fmt44 = 'csi'
let b:ubisrc_fmt45 = 'csiu'
let b:ubisrc_fmt46 = 'csib'
let b:ubisrc_fmt47 = 'csibu'
let b:ubisrc_fmt48 = 'cr'
let b:ubisrc_fmt49 = 'cru'
let b:ubisrc_fmt50 = 'crb'
let b:ubisrc_fmt51 = 'crbu'
let b:ubisrc_fmt52 = 'cri'
let b:ubisrc_fmt53 = 'criu'
let b:ubisrc_fmt54 = 'crib'
let b:ubisrc_fmt55 = 'cribu'
let b:ubisrc_fmt56 = 'crs'
let b:ubisrc_fmt57 = 'crsu'
let b:ubisrc_fmt58 = 'crsb'
let b:ubisrc_fmt59 = 'crsbu'
let b:ubisrc_fmt60 = 'crsi'
let b:ubisrc_fmt61 = 'crsiu'
let b:ubisrc_fmt62 = 'crsib'
let b:ubisrc_fmt63 = 'crsibu'
" >>>
else " if exists('b:txtfmt_do_common_config')
" Function: s:Txtfmt_refresh() <<<
" Purpose: Invoked by buffer-local command Refresh when user wishes to
" reload txtfmt plugins safely for the current buffer; e.g., after changing
" option settings.
" Important Note: This function must be within the else of an if
" exists('b:txtfmt_do_common_config'); otherwise, we will get an error when this
" function causes the plugins to be re-sourced, since the re-sourcing of this
" file will result in an attempt to redefine the function while it is running!
fu! s:Txtfmt_refresh()
	" Ensure that common configuration code will not be skipped next time
	unlet! b:txtfmt_did_common_config
	" Determine whether txtfmt ftplugin is loaded
	if exists('b:loaded_txtfmt')
		" b:loaded_txtfmt is set only within ftplugin/txtfmt.vim and unlet by
		" b:undo_ftplugin; hence, its existence indicates that txtfmt ftplugin
		" is currently loaded. Cache the filetype that was cached at load
		" time.
		let l:current_filetype = b:txtfmt_filetype
	endif
	" Determine whether txtfmt syntax plugin is loaded
	let v:errmsg = ''
	silent! syn sync match Tf_existence_test grouphere Tf_fmt_1 /\%^/
	if v:errmsg == ''
		" No error means txtfmt syntax plugin is loaded. Cache the syntax name
		" that was cached at load time.
		let l:current_syntax = b:txtfmt_syntax
	endif
	" Is there anything to refresh?
	if !exists('l:current_filetype') && !exists('l:current_syntax')
		echomsg "Warning: Useless call to Refresh: "
			\."no txtfmt plugins appear to be loaded."
		return
	endif
	" If here, there was a reason for the Txtfmt_refresh call. Cause ftplugin
	" and/or syntax plugin to be reloaded via FileType and Syntax sets, as
	" appropriate.
	if exists('l:current_syntax')
		" We're going to attempt to reload syntax plugin. Unload it now
		" (causing b:current_syntax to be unlet). If we set filetype below,
		" and b:current_syntax exists afterwards, we'll know syntax was loaded
		" via syntaxset autocmd linked to FileType event. Alternatively,
		" could simply unlet b:current_syntax here...
		set syntax=OFF
	endif
	if exists('l:current_filetype')
		" Set filetype to whatever it was before
		exe 'set filetype=' . l:current_filetype
	endif
	if exists('l:current_syntax')
		" Syntax may have been loaded already, but if not, we'll need to do it
		" manually...
		if exists('b:current_syntax')
			" Syntax was loaded as a result of the filetype set. Make sure it
			" appears to be the correct one.
			if b:current_syntax != l:current_syntax
				echomsg "Warning: Txtfmt attempted to restore syntax to `"
				\.l:current_syntax."'. Result was `".b:current_syntax."'"
				echomsg "I'm guessing you have loaded the txtfmt plugins "
				\."in a non-standard manner. See txtfmt help for more information."
			endif
		else
			" Syntax wasn't linked to filetype. Load the desired syntax manually.
			exe 'set syntax=' . l:current_syntax
		endif
	endif	
endfu
" >>>
endif " if exists('b:txtfmt_do_common_config')
" Function: s:MakeTestPage() <<<
" Purpose: Build a "test-page" in a scratch buffer, to show user how color
" and format regions will look with current definitions and on current
" terminal. (Useful to prevent user from having to insert all the color and
" format regions manually with text such as "Here's a little test...")
" How: Create a scratch buffer whose filetype is set to txtfmt. Add some
" explanation lines at the top, followed by one line for each color, as
" follows:
" color<num> none i b bi u ui ub ubi ...
" Note: The function is script-local, as it is designed to be invoked from a
" command.
" IMPORTANT NOTE: Special care must be taken when defining this function, as
" it creates a buffer with 'ft' set to txtfmt, which causes the script to be
" re-sourced. This leads to E127 'Cannot redefine function' when fu[!] is
" encountered, since the function is in the process of executing.
if !exists('*s:MakeTestPage')
fu! s:MakeTestPage(...)
	if a:0 == 1
		" User provided optional modeline arguments. Before opening scratch
		" buffer, make sure the modeline constructed from the arguments has at
		" least the overall appearance of being valid. (Option name/value
		" validation is performed only after opening the test page buffer.)
		if !s:Is_txtfmt_modeline("\<Tab>txtfmt:".a:1)
			" Warn of invalid modeline and return without creating the test
			" buffer
			echoerr "Invalid arguments passed to :MakeTestPage command: `".a:1."'"
			return
		endif
	endif
	" Open the buffer
	new
	set buftype=nofile
	set bufhidden=hide
	set noswapfile
	" If user provided modeline, add it to top of file before setting filetype
	" to txtfmt...
	if a:0 == 1
		let modeline = a:1
		if modeline =~ '\S'
			call setline(1, "\<Tab>txtfmt:".modeline)
		endif
	elseif a:0 > 1
		" This should never happen, since this function is called from a
		" mapping.
		echoerr "Too many arguments passed to MakeTestPage."
			\." (Note that this function should not be called directly.)"
	endif
	set filetype=txtfmt
	" Set page formatting options
	" TODO - Decide whether the following are necessary anymore. (I'm
	" formatting things explicitly now...)
	set noai ts=4 sw=4 tw=78
	set wrap
	" Cache some special tokens that will be used on this page
	let tok_fb = Txtfmt_GetTokStr('fb')
	let tok_fui = Txtfmt_GetTokStr('fui')
	let tok_fu = Txtfmt_GetTokStr('fu')
	let tok_fmt_end = nr2char(b:txtfmt_fmt_first_tok)
	let tok_clr_end = nr2char(b:txtfmt_clr_first_tok)
	call append(line('$'), tok_fb)
	call append(line('$'),
		\"************************")
	$center
	call append(line('$'),
		\"*** TXTFMT TEST PAGE ***")
	$center
	call append(line('$'),
		\"************************")
	$center
	call append(line('$'),
		\"=============================================================================")
	call append(line('$'),
		\"*** Overview ***")
	$center
	call append(line('$'), tok_fmt_end)
	call append(line('$'), "")
	call append(line('$'),
		\"The purpose of this page is to present an overview of the txtfmt highlighting")
	call append(line('$'),
		\"that results from the global txtfmt options and any txtfmt modeline settings")
	call append(line('$'),
		\"passed to the MakeTestPage command.")
	call append(line('$'), "")
	call append(line('$'),
		\"	:help txtfmt-MakeTestPage")
	call append(line('$'), "")
	call append(line('$'),
		\"The text on the page has been chosen to display all possible combinations of")
	call append(line('$'),
		\"color and format regions, and if applicable, to illustrate the escaping of")
	call append(line('$'),
		\"tokens and the nesting of txtfmt regions.")
	call append(line('$'), tok_fb)
	call append(line('$'),
		\"=============================================================================")
	call append(line('$'),
		\"*** Colors and Formats ***")
	$center
	call append(line('$'), tok_fmt_end)
	call append(line('$'),
		\tok_fui.'Configuration:'.tok_fb
		\."tokrange =".tok_fmt_end.b:txtfmt_cfg_starttok
		\.(b:txtfmt_cfg_formats == 'basic' ? 'S' : 'L'))
	call append(line('$'),
		\" (start token: ".b:txtfmt_cfg_starttok.", formats: '".b:txtfmt_cfg_formats."')")
	call append(line('$'), '')
	call append(line('$'),
		\"Each line in the fmt/clr table below corresponds either to the default color")
	call append(line('$'),
		\"(no color), or to one of the ".(b:txtfmt_num_colors-1)." distinct colors, whose default definitions may")
	call append(line('$'),
		\"be overridden by plugin user via the special global array g:txtfmtColor{}.")
	call append(line('$'), '')
	call append(line('$'),
		\'    :help txtfmt-defining-colors')
	call append(line('$'), '')
	call append(line('$'),
		\"The ".b:txtfmt_num_formats." permutations of the format attributes ")
	call append(line('$'),
		\'(u=underline, b=bold, i=italic'
		\.(b:txtfmt_cfg_formats == 'all' || b:txtfmt_cfg_formats == 'all_but_undercurl' ?
		\', s=standout, r=reverse' : '')
		\.(b:txtfmt_cfg_formats == 'all' ? ', c=undercurl' : '')
		\.')')
	call append(line('$'), "are shown on each color line for completeness.")
	call append(line('$'), tok_fb)
	call append(line('$'),
		\"IMPORTANT NOTE:".tok_fmt_end."Txtfmt chooses a default range for clr/fmt tokens, which works")
	call append(line('$'),
		\"well on most terminals; however, this range may not be suitable for all")
	call append(line('$'),
		\"terminals. In particular, Vim cannot highlight those characters displayed by")
	call append(line('$'),
		\"the terminal as special 2-character sequences (e.g., ^M, ^B, etc...). Although")
	call append(line('$'),
		\"coloring and formatting of text will work when these characters are used as")
	call append(line('$'),
		\"tokens, their use is discouraged, because txtfmt is unable to conceal them. If")
	call append(line('$'),
		\"any such control sequences are visible in the sample text below, you may wish")
	call append(line('$'),
		\"to try a different range, either by setting global txtfmt option")
	call append(line('$'),
		\"g:txtfmtTokrange to a different value, or by including a different definition")
	call append(line('$'),
		\"in a txtfmt modeline string passed to the MakeTestPage command. Either way, you")
	call append(line('$'),
		\"will need to invoke MakeTestPage again to see whether the changed settings are")
	call append(line('$'),
		\"better.")
	call append(line('$'), "")
	call append(line('$'),
		\"    :help txtfmt-choosing-token-range")
	call append(line('$'), '')
	call append(line('$'), tok_fb)
	call append(line('$'),
		\'--color/format table--')
	call append(line('$'), tok_fmt_end)

	" Determine line on which to start the fmt/clr table
	let iLine = line('.')
	" Put the text into the buffer
	" Note: iClr in the loop below is a 1-based color index, with 0 reserved
	" for the default (no) color token.
	let iClr = 0
	while iClr < b:txtfmt_num_colors
		" Build the string for this line
		if iClr == 0
			let s = " no color    "
		else
			let s = nr2char(b:txtfmt_clr_first_tok + iClr)
			let s = s.'Color '.iClr."     "
		endif
		" Loop over format attributes
		let iFmt = 0
		while iFmt < b:txtfmt_num_formats
			if iFmt == 0
				let s = s.'-'
			else
				let s = s.nr2char(b:txtfmt_fmt_first_tok + iFmt)
				let s = s.b:ubisrc_fmt{iFmt}
			endif
			let iFmt = iFmt + 1
		endwhile
		" Don't allow formatting to spill over onto next line
		let s = s.nr2char(b:txtfmt_fmt_first_tok)
		call append(line('$'), s)
		let iClr = iClr + 1
	endwhile
	" Return to default fmt/clr
	call append(line('$'),
		\nr2char(b:txtfmt_fmt_first_tok).nr2char(b:txtfmt_clr_first_tok))

	call append(line('$'), tok_fb)
	call append(line('$'),
		\"=============================================================================")
	call append(line('$'),
		\"*** Escaping txtfmt tokens ***")
	$center
	call append(line('$'), tok_fmt_end)
	call append(line('$'),
		\tok_fui.'Configuration:'.tok_fb."escape".tok_fmt_end."= ".b:txtfmt_cfg_escape)
	call append(line('$'), "")
	call append(line('$'),
		\"    :help txtfmt-escape")
	" Now display text specific to the option setting
	if b:txtfmt_cfg_escape != 'none'
		call append(line('$'), '')
		call append(line('$'),
			\tok_fb.'--Escaping tokens outside a region--'.tok_fmt_end)
		call append(line('$'),
			\"The following shows that all tokens (other than the \"no fmt\" / \"no clr\" tokens)")
		call append(line('$'),
			\"may be escaped to prevent them from beginning a region:")
		call append(line('$'), '')
		call append(line('$'),
			\tok_fb.'*color tokens*'.tok_fmt_end)
		" Loop over all clr tokens, inserting an escaped version of each.
		let s = ''
		let iClr = 1
		while iClr < b:txtfmt_num_colors
			let tok = nr2char(b:txtfmt_clr_first_tok + iClr)
			let s = s.(b:txtfmt_cfg_escape == 'self' ? tok : '\').tok
			let iClr = iClr + 1
		endwhile
		call append(line('$'), s)
		call append(line('$'),
			\tok_fb.'*format tokens*'.tok_fmt_end)
		" Loop over all fmt tokens, inserting an escaped version of each.
		let s = ''
		let iFmt = 1
		while iFmt < b:txtfmt_num_formats
			let tok = nr2char(b:txtfmt_fmt_first_tok + iFmt)
			let s = s.(b:txtfmt_cfg_escape == 'self' ? tok : '\').tok
			let iFmt = iFmt + 1
		endwhile
		call append(line('$'), s)
		call append(line('$'), '')
		call append(line('$'),
			\tok_fb.'--Escaping tokens inside a region--'.tok_fmt_end)
		call append(line('$'), tok_fui)
		call append(line('$'),
			\"Here's a little swatch of \"underline, italic\" text. On the line below are some")
		call append(line('$'),
			\"escaped tokens, which, in their unescaped form, would alter the region's")
		call append(line('$'),
			\"formatting:")
		call append(line('$'),
			\(b:txtfmt_cfg_escape == 'self' ? tok_fb : '\').tok_fb
			\.' (escaped bold token), '
			\.(b:txtfmt_cfg_escape == 'self' ? tok_fmt_end : '\').tok_fmt_end
			\.' (escaped "no fmt" token)')
		call append(line('$'),
			\"As you can see, the escaping characters are concealed, and the formatting is")
		call append(line('$'),
			\"unaffected by the escaped tokens.")
		call append(line('$'),
			\"Note: After you have viewed the rest of this page, you may wish to experiment")
		call append(line('$'),
			\"by removing the escape tokens to see how the formatting is affected.")
	else
		" Inform user that escaping is not configured
		call append(line('$'), '')
		call append(line('$'),
			\"Escaping of txtfmt tokens is currently disabled.")
	endif

	call append(line('$'), tok_fb)
	call append(line('$'),
		\"=============================================================================")
	call append(line('$'),
		\"*** Nesting txtfmt regions ***")
	$center
	call append(line('$'), tok_fmt_end)
	call append(line('$'),
		\tok_fui.'Configuration:'.tok_fb.(b:txtfmt_cfg_nested ? "nested" : "nonested").tok_fmt_end)
	call append(line('$'), "")
	call append(line('$'),
		\"    :help txtfmt-escape")
	" Now display text specific to the option setting
	if b:txtfmt_cfg_nested
		call append(line('$'), '')
		call append(line('$'),
			\"/* Here's a sample comment (italicized), intended to illustrate the nesting of")
		call append(line('$'),
			\" * txtfmt regions within non-txtfmt regions.")
		call append(line('$'),
			\" *")
		call append(line('$'),
			\" * The following txtfmt token -->".tok_fu."<-- begins a nested \"underline\" region, which")
		call append(line('$'),
			\" * ends with the following \"no fmt\" token. -->".tok_fmt_end."<--")
		call append(line('$'),
			\" * As you can see, the comment resumes automatically after the nested region")
		call append(line('$'),
			\" * ends. */")
		call append(line('$'), "")
		call append(line('$'),
			\"Non-txtfmt regions may be divided into two categories: those with the")
		call append(line('$'),
			\"'keepend' attribute, and those without it. To demonstrate the effect of the")
		call append(line('$'),
			\"'keepend' attribute on nested txtfmt regions, I have defined two additional")
		call append(line('$'),
			\"regions, enclosed by curly braces and square brackets respectively. The curly")
		call append(line('$'),
			\"brace region does not have the 'keepend' attribute; the square bracket region")
		call append(line('$'),
			\"does. Both regions are highlighted in bold.")
		call append(line('$'),
			\"{ Once again, here's a".tok_fu."nested \"underline\" txtfmt region, followed by a curly")
		call append(line('$'),
			\"brace. }")
		call append(line('$'),
			\"As you can see, the nested txtfmt region was *not* terminated by the")
		call append(line('$'),
			\"closing curly brace. In fact, the curly brace region was extended by the")
		call append(line('$'),
			\"txtfmt region. Notice how the following txtfmt \"no fmt\" token -->".tok_fmt_end."<--")
		call append(line('$'),
			\"permits the resumption of the curly brace region}, which is finally ended by")
		call append(line('$'),
			\"the unobscured closing curly brace.")
		call append(line('$'),
			\"[ Notice, by contrast, how both the".tok_fu."nested \"underline\" txtfmt region and the")
		call append(line('$'),
			\"square bracket region itself are terminated by the following square bracket ]")
		call append(line('$'),
			\"because the square bracket region was defined with the 'keepend' attribute.")


		" Define comment, curly brace, and square brace regions...
		syn region Tf_example_comment start=+/\*+ end=+\*/+ keepend
		hi Tf_example_comment cterm=italic gui=italic
		syn region Tf_example_curly start=+{+ end=+}+
		hi Tf_example_curly cterm=bold gui=bold
		syn region Tf_example_square start=+\[+ end=+\]+ keepend
		hi Tf_example_square cterm=bold gui=bold
	else
		" Inform user that nesting is not configured
		call append(line('$'), "")
		call append(line('$'),
			\"Nesting of txtfmt regions is currently disabled.")
	endif

endfu
endif	" if !exists('*s:MakeTestPage')
" >>>
" Public-interface commands <<<
" TODO - Add this command to undo list - Should it redefine (com!)?
com! -nargs=? MakeTestPage call s:MakeTestPage(<f-args>)
" >>>
	" vim: sw=4 ts=4 foldmethod=marker foldmarker=<<<,>>> :
