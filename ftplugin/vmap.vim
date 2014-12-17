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

let b:txtfmt_num_formats = 64

" Simulated above this line.

" Binary progression: ubisrc
let s:ubisrc_mask = {'u': 1, 'b': 2, 'i': 4, 's': 8, 'r': 16, 'c': 32}

fu! s:Parse_fmt_clr_transformer(spec)
	" Initalize return object.
	let ret = {'f': {}, 'c': {}, 'k': {}}
	if empty(a:spec)
		" Effectively empty spec.
		return ret
	endif
	" Function const
	let re_tok = '['.b:ubisrc_fmt{b:txtfmt_num_formats-1}.']'
	" TODO: Rename to f_atom(s) or somesuch
	let re_atom = '\%(' . re_tok . '\%(>\(' . re_tok . '\)\)\?\)'
	let re_atoms = '^' . re_atom . '\+$'
	echo re_atoms
	" Split the comma-separated f/c/k components.
	" Design Decision: Deliberately permit more than 1 of each type, which will behave as though they had been
	" concatenated together.
	" Design Decision: Silently ignore redundancies (same add/remove multiple times) and harmless oddities (null
	" component in comma-sep list, or empty f, c or k spec).
	" Note: keepempty 0 ensures that empty components at beginning or end will be silently discarded, but logic in loop
	" needs to handle interior empty components.
	for spec in split(a:spec, ',')
		if empty(spec)
			" Discard completely empty component in comma-separated list.
			continue
		endif
		" Get token type
		let tt = spec[0]
		" Validate the type
		if !has_key(ret, tt)
			let s:err_str = "Invalid type specifier in fmt/clr transformer spec: `" . tt . "'"
			return {}
		endif
		" Get ref to type-specific object within return object, permitting multiple components of the same type (f/c/k)
		" to act as one.
		let rret = ret[tt]
		if tt == 'f'
			let atoms = spec[1:]
			if empty(atoms)
				" Discard empty f spec
				continue
			endif
			" Initialize if necessary (now that we know the spec isn't empty)
			if empty(rret)
				let rret.add = 0
				let rret.sub = 0
				let rret.replace = {}
			endif
			" Break into add/sub parts. 'keepempty' guarantees at least an add part (possibly empty).
			let [add; rest] = split(atoms, '-', 1)
			if len(rest) > 1
				let s:err_str = "Too many hyphens in fmt transformer spec: `f" . atoms . "'"
				return {}
			endif
			let sub = empty(rest) ? '' : rest[0]
			if empty(add) && empty(sub)
				" f- special case: return to default (mask all attributes)
				if rret.add || !empty(rret.replace)
					let s:err_str = "Conflict in fmt/clr transformer spec: cannot use special `f-' form with any other add/replace mechanism"
				endif
				let rret.sub = b:txtfmt_num_formats - 1
			else
				" At least 1 of add/sub is non-null.
				" Process sub first to facilitate validation of add/replace specs.
				if !empty(sub)
					if sub !~ re_atoms
						let s:err_str = "Invalid char(s) in attribute removal section of fmt/clr transformer spec: `f" . atoms . "'"
						return {}
					endif
					" Process individual attr chars to build sub mask.
					" Decision: Don't treat redundancies as error.
					for atom in split(sub, '\zs')
						if and(rret.add, s:ubisrc_mask[atom]) || index(values(rret.replace), atom) != -1
							let s:err_str = "Conflict in fmt/clr transformer spec: attempt to both add and remove the same attribute: `" . atom . "'"
							return {}
						endif
						let rret.sub = or(rret.sub, s:ubisrc_mask[atom])
					endfor
				endif
				if !empty(add)
					if add !~ re_atoms
						let s:err_str = "Invalid char(s) in attribute addition section of fmt/clr transformer spec: `f" . atoms . "'"
						return {}
					endif
					" Process individual atoms: e.g., c or c>c
					" Note: The split() pattern matches at start of all chars not preceded by `>'.
					for atom in split(add, '>\@<!' . re_atom . '\@=')
						" Note: Earlier validation guarantees valid atom.
						let [s; rest] = split(atom, '>')
						if empty(rest)
							" Normal add
							if and(rret.sub, s:ubisrc_mask[s])
								let s:err_str = "Conflict in fmt/clr transformer spec: attempt to both add and remove the same attribute: `" . s . "'"
								return {}
							endif
							let rret.add = or(rret.add, s:ubisrc_mask[s])
						else
							" Transforming (replacement) add
							let t = rest[0]
							if and(rret.sub, s:ubisrc_mask[t])
								" TODO: Asymmetry here in way f- conflict is reported (since we don't know here whether
								" f- was used).
								let s:err_str = "Conflict in fmt/clr transformer spec: attempt to both add and remove the same attribute: `" . t . "'"
								return {}
							endif
							" Check for conflict.
							" Decision: Silently ignore non-conflicting redundancy.
							if has_key(rret.replace, s:ubisrc_mask[s]) && rret.replace[s:ubisrc_mask[s]] != s:ubisrc_mask[t]
								let s:err_str = "Conflict in fmt/clr transformer spec: mutually-exclusive replacements specified for `"
									\. s . "' attribute: `" . rret.replace[s:ubisrc_mask[s]] . "' and `" . t . "'"
								return {}
							endif
							" Augment replacement dict.
							let rret.replace[s:ubisrc_mask[s]] = s:ubisrc_mask[t]
						endif
					endfor
				endif
			endif
		elseif tt == 'c' || tt == 'k'
			" Valid Formats:
			" c{namepat} c- c{namepat1}>[{namepat2}]
			" Note: c{namepat}> is a degenerate case of the final form: it replaces color matching {namepat} with
			" nothing (i.e., no color).
			let atoms = spec[1:]
			if empty(atoms)
				" Discard empty f spec
				continue
			endif
			if empty(rret)
				" Constraint: set and replace fields are mutually-exclusive; thus, use set's value as a flag indicating
				" which is used.
				" If used, set is a color number between 0 (no color) and max active color.
				let rret.set = -1
				" If used, replace is a dict mapping old color numbers to new.
				let rret.replace = {}
			endif
			" Break into atoms.
			for atom in split(atoms, ';')
				let ls = []
			   	for s in split(atom, '>')
					call add(ls, 
				endfor
				if empty(rest)
					
			endfor
		else
			let s:err_str = "Invalid type specifier in fmt/clr transformer spec: ".tt
			return {}
		endif
	endfor
	return ret
endfu

fu! Test(spec)
	unlet! s:err_str
	echo string(s:Parse_fmt_clr_transformer(a:spec))
	if (exists('s:err_str'))
		echoerr s:err_str
	endif
endfu


" vim: sw=4 ts=4 tw=120 foldmethod=marker foldmarker=<<<,>>> :
