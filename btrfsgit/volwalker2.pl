



:- use_module(library(main)).
:- use_module(library(http/json)).
% Define dynamic predicates to store subvolume info
% subvol(UUID, Machine, ParentUUID, ReceivedUUID, RO, SubvolID)
% Optional fields (ParentUUID, ReceivedUUID, SubvolID) are 'null' if missing/empty.
:- dynamic subvol/6.
:- dynamic candidate/1. % candidate(UUID).

% Helper to get value from dict or return default
get_val(Key, Dict, Value, Default) :-
    (get_dict(Key, Dict, RawValue), RawValue \= "", RawValue \= null)
    -> Value = RawValue
    ;  Value = Default.

% Assert subvolume facts from the JSON dictionary list
assert_subvols([]).
assert_subvols([SubvolDict|Rest]) :-
    get_dict(local_uuid, SubvolDict, UUID), % Mandatory
    get_dict(machine, SubvolDict, Machine), % Mandatory
    get_val(parent_uuid, SubvolDict, ParentUUID, null), % Optional
    get_val(received_uuid, SubvolDict, ReceivedUUID, null), % Optional
    get_val(ro, SubvolDict, RO, false), % Optional, default false
    get_val(subvol_id, SubvolDict, SubvolID, null), % Optional
    assertz(subvol(UUID, Machine, ParentUUID, ReceivedUUID, RO, SubvolID)),
    assert_subvols(Rest).

% Determine the parent UUID (prefer received_uuid)
parent_uuid(UUID, ParentUUID) :-
    subvol(UUID, _, _, ReceivedUUID, _, _), % Find the subvol fact for the UUID
    ReceivedUUID \= null, % Check if received_uuid exists
    !, % Cut: Use ReceivedUUID if it exists
    ParentUUID = ReceivedUUID.
parent_uuid(UUID, ParentUUID) :-
    subvol(UUID, _, ParentUUIDValue, _, _, _), % Find the subvol fact
    ParentUUIDValue \= null, % Check if parent_uuid exists
    !, % Cut: Use ParentUUID if it exists
    ParentUUID = ParentUUIDValue.
% If neither received_uuid nor parent_uuid is valid, parent_uuid/2 fails for this UUID.

% Walk up the parent chain
walk(UUID, SourceMachine, TargetMachine) :-
    subvol(UUID, _, _, _, _, _), % Check if UUID exists in facts
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
walk(_, _, _). % Stop if UUID doesn't exist in subvol facts or has no valid parent

% Check if a subvolume is read-only and on the specified machine
is_ro_on_machine(UUID, Machine) :-
    subvol(UUID, Machine, _, _, true, _). % Check RO status and Machine directly from fact

% Check if a subvolume has any read-only descendant (including itself) on the target machine
has_ro_descendant_on_machine(UUID, TargetMachine) :-
    is_ro_on_machine(UUID, TargetMachine), % Check if the current one matches
    !. % Found one, cut
has_ro_descendant_on_machine(UUID, TargetMachine) :-
    % Find children (snapshot or received) by querying subvol facts
    ( subvol(ChildUUID, _, UUID, _, _, _) % Child is a snapshot of UUID
    ; subvol(ChildUUID, _, _, UUID, _, _) % Child was received from UUID
    ),
    has_ro_descendant_on_machine(ChildUUID, TargetMachine).

% Main predicate called from command line
main(Argv) :-
    % Argv = ['volwalker2.pl', SourceUUID, SourceMachine, TargetMachine, JsonData]
    Argv = [_, SourceUUID, SourceMachine, TargetMachine, JsonData],

    % Parse JSON data
    atom_json_dict(JsonData, SubvolsDictList, []), % Keep original dict list for now

    % Clean up previous facts and assert new ones
    retractall(subvol/6),
    retractall(candidate/1),
    assert_subvols(SubvolsDictList), % Assert facts from the dict list

    % Start the walk from the source UUID
    walk(SourceUUID, SourceMachine, TargetMachine),

    % Retrieve candidate UUIDs
    findall(UUID, candidate(UUID), Candidates),

    % Sort candidates by SubvolID descending (handle missing subvol_id)
    predsort(compare_subvol_id_desc, Candidates, SortedCandidates),

    print_candidates(SortedCandidates),
    halt(0). % Exit successfully

main(_) :-
    write('Usage: swipl volwalker2.pl <SourceUUID> <SourceMachine> <TargetMachine> ''<JSON_Subvols_Data>''\n'),
    halt(1). % Exit with error

% Comparator for sorting UUIDs by SubvolID descending
% Handles cases where subvol_id is null, treating them as lowest priority (-1)
compare_subvol_id_desc(Order, UUID1, UUID2) :-
    subvol(UUID1, _, _, _, _, ID1Value),
    subvol(UUID2, _, _, _, _, ID2Value),
    (ID1Value == null -> ID1 = -1 ; ID1 = ID1Value),
    (ID2Value == null -> ID2 = -1 ; ID2 = ID2Value),
    compare(OrderNum, ID2, ID1), % Note the reversed order for descending sort
    (OrderNum == 0 -> Order = (=) ; OrderNum == 1 -> Order = (>) ; Order = (<)).


% Print candidate UUIDs, one per line
print_candidates([]).
print_candidates([UUID|Rest]) :- % Now just takes UUID
    writeln(UUID),
    print_candidates(Rest).

