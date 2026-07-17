% volwalker2.pl - find usable `btrfs send -p` parents, in Prolog.
%
% Generalization of volwalker.py (VolWalker), not a port of it. Where the Python
% version follows a single parent link upward (received_uuid preferred, parent_uuid
% dropped when both are set) and only walks read-only chains downward, this version:
%   - tracks ancestry over any number of hops, following BOTH parent_uuid and
%     received_uuid edges, across any number of machines/filesystems,
%   - treats "same content" as the UNDIRECTED closure of snapshot/receive edges
%     between read-only subvolumes, so content can be found by walking up a chain
%     (e.g. from a received copy back to the snapshot it was sent from) as well as down.
%
% A subvolume Uuid is a candidate parent for sending SourceUuid from SourceFs to
% TargetFs iff:
%   - some ancestor A of SourceUuid has a read-only capture I (A itself if read-only,
%     or a read-only snapshot/copy taken directly of A),
%   - I's content also exists on TargetFs (same_content reaches a read-only subvol
%     there), and
%   - Uuid is a read-only subvol on SourceFs with that same content.
%
% Guardrails (see tests/test_common_parents.py):
%   - an edge into a read-write subvolume is NOT content-preserving (the rw subvol
%     may have changed since the snapshot was taken); naively walking up through one
%     yields snapshots that never reached the target as false parents.
%   - if the read-only-ness of a chain is broken, the subvol and its descendants
%     are of no use (tests/negative/test2.sh).
%
% subvol(Ro, Fs, Uuid, ParentUuid, ReceivedUuid, Deleted)
% Optional fields (ParentUuid, ReceivedUuid) are 'null' if missing/empty.
% Deleted subvols stay walkable (ancestry and content edges through them still
% hold), but a deleted copy on the target is no evidence the content is still
% there. Filtering deleted subvols out of the *candidates* is left to the
% Python caller (volwalker2.py).

:- use_module(library(http/json)).
:- use_module(library(solution_sequences)).

:- dynamic subvol/6.

% tabling makes the closures terminate even on cyclic or garbage input data
:- table ancestor/2.
:- table same_content/2.


% Entry point: read the subvol list from a JSON file and print one candidate uuid
% per line, deduplicated, roughly nearest-ancestry first.
find_common_parents_from_file(JsonFile, SourceUuid, SourceFs, TargetFs) :-
    setup_call_cleanup(
        open(JsonFile, read, Stream),
        json_read_dict(Stream, SubvolsDictList),
        close(Stream)),
    retractall(subvol(_, _, _, _, _, _)),
    abolish_all_tables,
    assert_subvols(SubvolsDictList),
    forall(distinct(Uuid, common_parent(SourceUuid, SourceFs, TargetFs, Uuid)),
           format("~w~n", [Uuid])).


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


common_parent(SourceUuid, SourceFs, TargetFs, Uuid) :-
    ancestor(SourceUuid, AncestorUuid),
    capture(AncestorUuid, I),
    % I's content made it to the target filesystem (and still exists there)...
    once((same_content(I, RemoteUuid),
          subvol(true, TargetFs, RemoteUuid, _, _, false))),
    % ...so any read-only subvol on the source side with that content is usable:
    same_content(I, Uuid),
    subvol(true, SourceFs, Uuid, _, _, _).


% ancestry: up through parent_uuid AND received_uuid, any number of hops,
% regardless of read-only-ness or filesystem.
ancestor(Uuid, Uuid) :-
    subvol(_, _, Uuid, _, _, _).
ancestor(Uuid, AncestorUuid) :-
    up(Uuid, P),
    ancestor(P, AncestorUuid).

up(Uuid, P) :-
    subvol(_, _, Uuid, ParentUuid, ReceivedUuid, _),
    (   P = ParentUuid
    ;   P = ReceivedUuid
    ),
    P \== null,
    subvol(_, _, P, _, _, _).   % the chain ends where the data does


% capture(A, I): a read-only capture of ancestor A: A itself if read-only, or a
% read-only snapshot/copy taken directly of A. This is the one place an edge may
% start at a read-write subvol - that is exactly how history is captured from a
% live subvolume. A received incremental (received_uuid set) is NOT a capture of
% its parent_uuid - it is that parent plus a delta from elsewhere.
capture(A, A) :- subvol(true, _, A, _, _, _).
capture(A, I) :- subvol(true, _, I, A, null, _).
capture(A, I) :- subvol(true, _, I, _, A, _).


% same_content(X, Y): undirected closure over content-preserving edges: snapshot
% or send/receive relationships where BOTH endpoints are read-only.
same_content(X, X) :-
    subvol(true, _, X, _, _, _).
same_content(X, Y) :-
    content_edge(X, Z),
    same_content(Z, Y).

content_edge(X, Z) :-
    ro(X),
    (   edge(X, Z)
    ;   edge(Z, X)
    ),
    ro(Z).

% a snapshot edge preserves content only if the child is not a received subvol:
% a received incremental's parent_uuid is the -p parent it was rebuilt from, plus
% a delta - different content. Its content is identified by received_uuid instead.
edge(Parent, Child) :- subvol(_, _, Child, Parent, null, _).
edge(Sent, Received) :- subvol(_, _, Received, _, Sent, _).

ro(X) :- subvol(true, _, X, _, _, _).
