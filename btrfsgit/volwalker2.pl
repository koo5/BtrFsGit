



:- use_module(library(main)).
:- use_module(library(http/json)).
:- use_module(library(dict_schema)). % For accessing nested dict keys safely

% Define dynamic predicates to store subvolume info
:- dynamic subvol/6. % subvol(LocalUUID, ParentUUID, ReceivedUUID, IsRO, Machine, SubvolID).
:- dynamic candidate/1. % candidate(UUID).

% Helper to safely get a value from a dict, returning default if key missing or value is null
safe_get(Dict, Key, Default, Value) :-
    (get_dict(Key, Dict, RawValue), RawValue \= @(null))
    -> Value = RawValue
    ;  Value = Default.

% Assert subvolume facts from the JSON dictionary list
assert_subvols([]).
assert_subvols([SubvolDict|Rest]) :-
    safe_get(SubvolDict, local_uuid, '', LocalUUID),
    safe_get(SubvolDict, parent_uuid, '', ParentUUID),
    safe_get(SubvolDict, received_uuid, '', ReceivedUUID),
    safe_get(SubvolDict, ro, false, IsRO), % Default to false if missing
    safe_get(SubvolDict, machine, '', Machine),
    safe_get(SubvolDict, subvol_id, -1, SubvolID), % Default to -1 if missing
    (LocalUUID \= '' -> % Only assert if we have a valid UUID
        assertz(subvol(LocalUUID, ParentUUID, ReceivedUUID, IsRO, Machine, SubvolID))
    ; true),
    assert_subvols(Rest).

% Determine the parent UUID (prefer received_uuid)
parent_uuid(UUID, ParentUUID) :-
    subvol(UUID, _, ReceivedUUID, _, _, _),
    ReceivedUUID \= '', % Check if ReceivedUUID is valid
    !, % Cut: Use ReceivedUUID if it exists
    ParentUUID = ReceivedUUID.
parent_uuid(UUID, ParentUUID) :-
    subvol(UUID, ParentUUID, _, _, _, _),
    ParentUUID \= ''. % Use ParentUUID if ReceivedUUID is not valid

% Walk up the parent chain
walk(UUID, SourceMachine, TargetMachine) :-
    subvol(UUID, _, _, _, _, _), % Ensure the UUID exists in our facts
    !, % Don't backtrack into finding the subvol fact again
    (
        % Check if this UUID is a potential candidate itself
        is_ro_on_machine(UUID, SourceMachine),
        has_ro_descendant_on_machine(UUID, TargetMachine),
        \+ candidate(UUID), % Avoid duplicates
        assertz(candidate(UUID)) % Found a candidate
    ;
        % Otherwise, continue walking up the parent chain
        (parent_uuid(UUID, Parent), walk(Parent, SourceMachine, TargetMachine))
    ).
walk(_, _, _). % Stop if UUID doesn't exist or has no parent

% Check if a subvolume is read-only and on the specified machine
is_ro_on_machine(UUID, Machine) :-
    subvol(UUID, _, _, true, Machine, _). % IsRO must be true

% Check if a subvolume has any read-only descendant (including itself) on the target machine
has_ro_descendant_on_machine(UUID, TargetMachine) :-
    is_ro_on_machine(UUID, TargetMachine),
    !. % Found one, cut
has_ro_descendant_on_machine(UUID, TargetMachine) :-
    % Find children (snapshot or received)
    (subvol(ChildUUID, UUID, '', _, _, _) ; subvol(ChildUUID, '', UUID, _, _, _)),
    has_ro_descendant_on_machine(ChildUUID, TargetMachine).

% Main predicate called from command line
main(Argv) :-
    % Argv = ['volwalker2.pl', SourceUUID, SourceMachine, TargetMachine, JsonData]
    Argv = [_, SourceUUID, SourceMachine, TargetMachine, JsonData],

    % Parse JSON data
    atom_json_dict(JsonData, SubvolsList, []),

    % Clean up previous facts and assert new ones
    retractall(subvol(_,_,_,_,_,_)),
    retractall(candidate(_)),
    assert_subvols(SubvolsList),

    % Start the walk from the source UUID
    walk(SourceUUID, SourceMachine, TargetMachine),

    % Retrieve and print candidates (sorted by SubvolID descending)
    findall(UUID-SubvolID, (candidate(UUID), subvol(UUID,_,_,_,_,SubvolID)), CandidatesWithID),
    keysort(CandidatesWithID, SortedCandidatesWithID), % Sorts by key (UUID) - need to sort by SubvolID
    predsort(compare_subvol_id_desc, CandidatesWithID, SortedBySubvolID),

    print_candidates(SortedBySubvolID),
    halt(0). % Exit successfully

main(_) :-
    write('Usage: swipl volwalker2.pl <SourceUUID> <SourceMachine> <TargetMachine> <JSON_Subvols_Data>\n'),
    halt(1). % Exit with error

% Comparator for sorting by SubvolID descending
compare_subvol_id_desc(Order, _-ID1, _-ID2) :-
    compare(OrderNum, ID2, ID1), % Note the reversed order for descending sort
    (OrderNum = 0 -> Order = (=) ; OrderNum = 1 -> Order = (>) ; Order = (<)).


% Print candidate UUIDs, one per line
print_candidates([]).
print_candidates([UUID-_|Rest]) :-
    writeln(UUID),
    print_candidates(Rest).

