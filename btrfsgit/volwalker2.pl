



:- use_module(library(main)).
:- use_module(library(http/json)).
% Define dynamic predicates to store subvolume info
:- dynamic subvol/1. % subvol(Dict). Stores the subvolume dict directly.
:- dynamic candidate/1. % candidate(UUID).

% Assert subvolume facts from the JSON dictionary list
assert_subvols([]).
assert_subvols([SubvolDict|Rest]) :-
    % Ensure the dict has a local_uuid before asserting
    (get_dict(local_uuid, SubvolDict, LocalUUID), LocalUUID \= '', LocalUUID \= @(null))
    -> assertz(subvol(SubvolDict))
    ;  true, % Skip assertion if local_uuid is missing or empty/null
    assert_subvols(Rest).

% Determine the parent UUID (prefer received_uuid)
parent_uuid(UUID, ParentUUID) :-
    subvol(Dict),
    Dict.local_uuid == UUID, % Find the subvol dict for the given UUID
    get_dict(received_uuid, Dict, ReceivedUUID), % Check if received_uuid exists and is not empty/null
    ReceivedUUID \= '', ReceivedUUID \= @(null),
    !, % Cut: Use ReceivedUUID if it exists and is valid
    ParentUUID = ReceivedUUID.
parent_uuid(UUID, ParentUUID) :-
    subvol(Dict),
    Dict.local_uuid == UUID, % Find the subvol dict
    get_dict(parent_uuid, Dict, ParentUUIDValue), % Check if parent_uuid exists and is not empty/null
    ParentUUIDValue \= '', ParentUUIDValue \= @(null),
    !, % Cut: Use ParentUUID if it exists and is valid
    ParentUUID = ParentUUIDValue.
% If neither received_uuid nor parent_uuid is valid, parent_uuid/2 fails for this UUID.

% Walk up the parent chain
walk(UUID, SourceMachine, TargetMachine) :-
    subvol(Dict), Dict.local_uuid == UUID, % Find the subvol dict for the UUID
    !, % Don't backtrack into finding the subvol fact again
    (
        % Check if this UUID is a potential candidate itself
        is_ro_on_machine(Dict, SourceMachine), % Pass the Dict
        has_ro_descendant_on_machine(UUID, TargetMachine), % UUID is enough here
        \+ candidate(UUID), % Avoid duplicates
        assertz(candidate(UUID)) % Found a candidate
    ;
        % Otherwise, continue walking up the parent chain
        (parent_uuid(UUID, Parent), walk(Parent, SourceMachine, TargetMachine))
    ).
walk(_, _, _). % Stop if UUID doesn't exist in subvol facts or has no valid parent

% Check if a subvolume is read-only and on the specified machine
is_ro_on_machine(Dict, Machine) :-
    get_dict(ro, Dict, true), % Check if 'ro' key exists and is true
    get_dict(machine, Dict, Machine). % Check if 'machine' key exists and matches

% Check if a subvolume has any read-only descendant (including itself) on the target machine
has_ro_descendant_on_machine(UUID, TargetMachine) :-
    subvol(Dict), Dict.local_uuid == UUID,
    is_ro_on_machine(Dict, TargetMachine), % Check if the current one matches
    !. % Found one, cut
has_ro_descendant_on_machine(UUID, TargetMachine) :-
    % Find children (snapshot or received) by iterating through all subvols
    subvol(ChildDict),
    (
        (get_dict(parent_uuid, ChildDict, UUID), UUID \= '', UUID \= @(null)) % Child is a snapshot of UUID
    ;
        (get_dict(received_uuid, ChildDict, UUID), UUID \= '', UUID \= @(null)) % Child was received from UUID
    ),
    ChildUUID = ChildDict.local_uuid,
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

    % Retrieve candidate UUIDs and their corresponding Dicts
    findall(UUID-Dict, (candidate(UUID), subvol(Dict), Dict.local_uuid == UUID), CandidatesWithDict),

    % Sort candidates by SubvolID descending (handle missing subvol_id)
    predsort(compare_subvol_id_desc, CandidatesWithDict, SortedCandidates),

    print_candidates(SortedCandidates),
    halt(0). % Exit successfully

main(_) :-
    write('Usage: swipl volwalker2.pl <SourceUUID> <SourceMachine> <TargetMachine> <JSON_Subvols_Data>\n'),
    halt(1). % Exit with error

% Comparator for sorting UUID-Dict pairs by SubvolID descending
% Handles cases where subvol_id might be missing, treating them as lowest priority (-1)
compare_subvol_id_desc(Order, _-Dict1, _-Dict2) :-
    (get_dict(subvol_id, Dict1, ID1Value) -> ID1 = ID1Value ; ID1 = -1),
    (get_dict(subvol_id, Dict2, ID2Value) -> ID2 = ID2Value ; ID2 = -1),
    compare(OrderNum, ID2, ID1), % Note the reversed order for descending sort
    (OrderNum == 0 -> Order = (=) ; OrderNum == 1 -> Order = (>) ; Order = (<)).


% Print candidate UUIDs, one per line
print_candidates([]).
print_candidates([UUID-_Dict|Rest]) :- % Match UUID-Dict pair, ignore Dict
    writeln(UUID),
    print_candidates(Rest).

