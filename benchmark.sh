#! /bin/bash

declare OUTDIR=${1:-./}
declare -A tfbrs=([master]=master [special]=colored-undercurl)
declare -A exes=([invloop]=~/src/vim/src/vim-invloop [master]=~/src/vim/src/vim)
declare -a lis=(li=none li=smart)
declare -a escs=(esc=none esc=bslash)
declare -a nesteds=(nonested nested)
declare cmd1= cmd2=
cmd1+='+set nomore rdt=100000|let txtfmtAllowxl=1'
# Leave dq string open since options will be inserted.
cmd1+='|let txtfmtMapwarn=""|exe "MakeTestPage rng=0xe000xu'
# Scroll 3 screens to get into the thick of the costly regions.
cmd2+='"|exe "normal 3\<c-f>"'
# In case there's a HitEnter prompt that's not prevented by 'nomore'...
# Note: txtfmtMapwarn="" obviates need for this now, but it doesn't hurt.
# Caveat: Putting the <cr> in normal doesn't work.
cmd2+='|call feedkeys("\<cr>")'
cmd2+='|call feedkeys(":qa\<cr>")'

#  0.00      0.72     0.00        1     0.00     0.00  x11_setup_selection
#
# %         the percentage of the total running time of the

for tfbr in "${!tfbrs[@]}"; do
	# Important Note: We need to check out the branch to ensure that gprof uses the correct source when analyzing the pre-built binary.
	# Question: Does gprof use the source? If not, don't bother checking out.
	echo "Processing branch ${tfbrs[$tfbr]}"
	git checkout "${tfbrs[$tfbr]}"
	# FIXME: Don't do this way. I don't think this would even work at this point because of the git checkout above...
	pushd $OUTDIR
	for exe in "${!exes[@]}"; do
		for li in "${lis[@]}"; do
			for esc in "${escs[@]}"; do
				for nested in "${nesteds[@]}"; do
					# Generate the current option combination.
					arg="$cmd1 $li $esc $nested $cmd2"
					# Run vim with current option combination.
					"${exes[$exe]}" "$arg"
					# Post-process the profile data, teeing the full profile
					# into configuration-specific filename, but also extracting
					# summary information for display.
					out=gprof-$tfbr-$exe-${li/=/_}-${esc/=/_}-$nested.out
					cum_time=$(gprof "${exes[$exe]}" gmon.out | tee $out |
						sed -nr '/^\s*[0-9.]+\s+[0-9.]+\s+[0-9.]+/{
							:loop
							/^\s*$/{g;s/^\s*\S+\s+(\S+).*/\1/p;q}
							h;n;b loop
						}')
					# Output all components of current combination to
					# facilitate sort/filter in spreadsheet.
					echo -e "$cum_time\t$tfbr\t$exe\t$li\t$esc\t$nested"
					# Pause to allow memory to settle.
					sleep 1
				done
			done
		done
	done
	popd
done

# vim:ts=4:sw=4:noet
