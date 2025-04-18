



common_parents(Source_fs, Target_fs, Subvols_json, Source_uuid) :-
	% convert the json to a list of subvols
	atom_json_dict(Subvols_json, Subvols, []),
	retractall(subvol(_)),
	% assert a subvol for each subvol
	findall(_,
		(
			member(Subvol, Subvols),
			assertz(subvol(Subvol))
		),
	_),
	% read back the subvols
	findall(Subvol, subvol(Subvol), Subvols2),
	% print the subvols
	print('Subvols: ', Subvols2).

