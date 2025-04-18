



:- use_module(library(main)).
:- use_module(library(http/json)).
% Define dynamic predicates to store subvolume info
% subvol(UUID, FS, ParentUUID, ReceivedUUID, RO)
% Optional fields (ParentUUID, ReceivedUUID) are 'null' if missing/empty.
:- dynamic subvol/5.
:- dynamic candidate/1. % candidate(UUID).

% Assert subvolume facts from the JSON dictionary list
assert_subvols([]).
assert_subvols([SubvolDict|Rest]) :-
    get_dict(local_uuid, SubvolDict, UUID), % Mandatory
    get_dict(fs, SubvolDict, FS), % Mandatory
    get_dict(parent_uuid, SubvolDict, ParentUUID),
    get_dict(received_uuid, SubvolDict, ReceivedUUID),
    get_dict(ro, SubvolDict, RO),
    assertz(subvol(UUID, FS, ParentUUID, ReceivedUUID, RO)),
    assert_subvols(Rest).


walk(UUID, SourceFS, TargetFS) :-
    subvol(UUID, _, _, _, _),
    (
        % Check if this UUID is a potential candidate itself
        is_ro_on_fs(UUID, SourceFS),
        has_ro_descendant_on_fs(UUID, TargetFS),
        \+ candidate(UUID), % Avoid duplicates
        assertz(candidate(UUID)) % Found a candidate
    ;
        % Otherwise, continue walking up the parent chain
        (parent_uuid(UUID, Parent), walk(Parent, SourceFS, TargetFS))
    ).
walk(_, _, _). % Stop if UUID doesn't exist in subvol facts or has no valid parent

% Check if a subvolume is read-only and on the specified fs
is_ro_on_fs(UUID, FS) :-
    subvol(UUID, FS, _, _, true). % Check RO status and FS directly from fact

% Check if a subvolume has any read-only descendant (including itself) on the target fs
has_ro_descendant_on_fs(UUID, TargetFS) :-
    is_ro_on_fs(UUID, TargetFS), % Check if the current one matches
    !. % Found one, cut
has_ro_descendant_on_fs(UUID, TargetFS) :-
    % Find children (snapshot or received) by querying subvol facts
    ( subvol(ChildUUID, _, UUID, _, _) % Child is a snapshot of UUID
    ; subvol(ChildUUID, _, _, UUID, _) % Child was received from UUID
    ),
    has_ro_descendant_on_fs(ChildUUID, TargetFS).

% Main predicate called from command line
main(Argv) :-
    % Argv = ['volwalker2.pl', SourceUUID, SourceFS, TargetFS, JsonData]
    Argv = [_, SourceUUID, SourceFS, TargetFS, JsonData],

    % Parse JSON data
    atom_json_dict(JsonData, SubvolsDictList, []), % Keep original dict list for now

    % Clean up previous facts and assert new ones
    retractall(subvol/5),
    retractall(candidate/1),
    assert_subvols(SubvolsDictList), % Assert facts from the dict list

    % Start the walk from the source UUID
    walk(SourceUUID, SourceFS, TargetFS),

    % Retrieve candidate UUIDs
    findall(UUID, candidate(UUID), Candidates),

    % Print candidates (order is not guaranteed, but likely assertion order)
    print_candidates(Candidates),
    halt(0). % Exit successfully

main(_) :-
    write('Usage: swipl volwalker2.pl <SourceUUID> <SourceFS> <TargetFS> ''<JSON_Subvols_Data>''\n'),
    halt(1). % Exit with error

% Print candidate UUIDs, one per line
print_candidates([]).
print_candidates([UUID|Rest]) :- % Now just takes UUID
    writeln(UUID),
    print_candidates(Rest).

