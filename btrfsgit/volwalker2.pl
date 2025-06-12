
:- use_module(library(main)).
:- use_module(library(http/json)).
% Define dynamic predicates to store subvolume info
% subvol(Ro, Fs, Uuid, ParentUuid, ReceivedUuid, Deleted)
% Optional fields (ParentUuid, ReceivedUuid) are 'null' if missing/empty.
:- dynamic subvol/6.

% Assert subvolume facts from the JSON dictionary list
assert_subvols([]).
assert_subvols([SubvolDict|Rest]) :-
    get_dict(local_uuid, SubvolDict, Uuid), % Mandatory
    get_dict(fs_uuid, SubvolDict, Fs), % Mandatory
    get_dict(parent_uuid, SubvolDict, ParentUuid),
    get_dict(received_uuid, SubvolDict, ReceivedUuid),
    get_dict(ro, SubvolDict, Ro),
    get_dict(deleted, SubvolDict, Deleted),
    assertz(subvol(Ro, Fs, Uuid, ParentUuid, ReceivedUuid, Deleted)),
    assert_subvols(Rest).

% Main predicate called from command line
find_common_parents(SubvolsJson, SourceUuid, SourceFs, TargetFs) :-
    atom_json_dict(SubvolsJson, SubvolsDictList, []),
    % Clean up previous facts and assert new ones
    retractall(subvol/6),
    assert_subvols(SubvolsDictList),
    findall(_,
    	(
    		common_parent(SourceUuid, SourceFs, TargetFs, Uuid),
    		printf('%s\n', [Uuid])
		),
		_).

% Alternative predicate that reads JSON from file
find_common_parents_from_file(JsonFile, SourceUuid, SourceFs, TargetFs) :-
    open(JsonFile, read, Stream),
    json_read_dict(Stream, SubvolsDictList),
    close(Stream),
    % Clean up previous facts and assert new ones
    retractall(subvol/6),
    assert_subvols(SubvolsDictList),
    findall(_,
    	(
    		common_parent(SourceUuid, SourceFs, TargetFs, Uuid),
    		printf('%s\n', [Uuid])
		),
		_).

common_parent(SourceUuid, SourceFs, TargetFs, Uuid) :-
	% trivial case, the SourceFs and TargetFs are the same, this Subvol is directly usable as a parent
	(
		SourceFs = TargetFs,
		Uuid = SourceUuid
	)
	;
	(
		ancestor_chain(SourceUuid, AncestorUuid),
		ro_chain(AncestorUuid, Uuid),
		subvol(true, SourceFs, Uuid, _, _, false),
		ro_chain(Uuid, RemoteUuid),
		subvol(true, TargetFs, RemoteUuid, _, _, false)
	).


% find the subvol itself + any ancestors
ancestor_chain(Uuid, Uuid).

ancestor_chain(Uuid, AncestorUuid) :-
	subvol(_, _, Uuid, ParentUuid, _, _),
	ancestor_chain(ParentUuid, AncestorUuid).

ancestor_chain(Uuid, AncestorUuid) :-
	subvol(_, _, Uuid, _, ReceivedUuid, _),
	ancestor_chain(ReceivedUuid, AncestorUuid).

% find the subvol itself (if it's ro) + any ancestors or children in a read-only chain
ro_chain(Uuid, Member) :-
	(
			(
				subvol(true, _, Uuid, _, _, _),
				Uuid = Member
			)
		;
			(
				subvol(true, _, Uuid, ParentUuid, _, _),
				ro_chain(ParentUuid, Member)
			)
		;
			(
				subvol(true, _, DescendantUuid, Uuid, _, _),
				ro_chain(DescendantUuid, Member)
			)
		;
			(
				subvol(true, _, TransferredUuid, _, Uuid, _),
				ro_chain(TransferredUuid, Member)
			)
		;
			(
				subvol(true, _, Uuid, _, TransferredUuid, _),
				ro_chain(TransferredUuid, Member)
			)
	).
