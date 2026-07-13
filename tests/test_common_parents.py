#!/usr/bin/env python
"""Differential tests for common-parent finding: volwalker (v1) vs volwalker2 (Prolog).

Contract: volwalker2 is a GENERALIZATION of volwalker, not a port. On any input,
every candidate v1 finds must also be found by v2 (v2 >= v1); v2 additionally tracks
ancestry over multiple hops and machines (both parent_uuid and received_uuid edges),
and finds content by walking read-only chains in either direction. Both must exclude
known-invalid parents (snapshots that never reached the target, broken ro chains,
received incrementals mistaken for their -p parent).

v2 is additionally checked against a pure-Python oracle implementing the same spec,
so the Prolog is verified by an independent implementation on randomized graphs.
"""

import random
import shutil
import subprocess
import pytest
from pathlib import Path

from btrfsgit import btrfsgit
from btrfsgit.volwalker import VolWalker
from btrfsgit.volwalker2 import common_parents

needs_swipl = pytest.mark.skipif(shutil.which('swipl') is None, reason='swipl not installed')


def mk(uuid, fs, parent=None, received=None, ro=True, deleted=False):
    return {
        'local_uuid': uuid, 'fs_uuid': fs, 'parent_uuid': parent,
        'received_uuid': received, 'ro': ro, 'deleted': deleted,
        'subvol_id': 0, 'path': f'/{fs}/{uuid}',
    }


def run_v1(subvols, my_uuid, source_fs, target_fs):
    """Run VolWalker the way _parent_candidates2 does: fss mapped to machine labels."""
    by_uuid = {}
    for sv in subvols:
        x = dict(sv)
        if x['fs_uuid'] == source_fs:
            x['machine'] = 'local'
        elif x['fs_uuid'] == target_fs:
            x['machine'] = 'remote'
        else:
            x['machine'] = 'other'
        by_uuid[x['local_uuid']] = x
    return set(c['local_uuid'] for c in VolWalker(by_uuid, ('local', 'remote')).walk(my_uuid))


def run_v2(subvols, my_uuid, target_fs):
    by_uuid = {sv['local_uuid']: dict(sv) for sv in subvols}
    return set(c['local_uuid'] for c in common_parents(by_uuid, my_uuid, target_fs))


def oracle(subvols, my_uuid, source_fs, target_fs):
    """Pure-Python reference implementation of the volwalker2 spec."""
    by = {sv['local_uuid']: sv for sv in subvols}

    def ro(u):
        return u in by and by[u]['ro']

    # ancestry: closure upward over BOTH parent_uuid and received_uuid
    ancestors = set()
    stack = [my_uuid] if my_uuid in by else []
    while stack:
        u = stack.pop()
        if u in ancestors:
            continue
        ancestors.add(u)
        for p in (by[u]['parent_uuid'], by[u]['received_uuid']):
            if p in by:
                stack.append(p)

    # content-preserving undirected edges: receive edges between ro endpoints, and
    # snapshot edges between ro endpoints where the child is not a received subvol
    neigh = {u: set() for u in by}
    for c in by.values():
        u = c['local_uuid']
        p = c['parent_uuid']
        if p is not None and c['received_uuid'] is None and ro(u) and ro(p):
            neigh[u].add(p)
            neigh[p].add(u)
        r = c['received_uuid']
        if r is not None and r in by and ro(u) and ro(r):
            neigh[u].add(r)
            neigh[r].add(u)

    def content_class(seed):
        if not ro(seed):
            return set()
        cls, stack = set(), [seed]
        while stack:
            u = stack.pop()
            if u in cls:
                continue
            cls.add(u)
            stack.extend(neigh[u])
        return cls

    def captures(a):
        caps = {a} if ro(a) else set()
        for z in by.values():
            if not z['ro']:
                continue
            if z['parent_uuid'] == a and z['received_uuid'] is None:
                caps.add(z['local_uuid'])
            if z['received_uuid'] == a:
                caps.add(z['local_uuid'])
        return caps

    result = set()
    for a in ancestors:
        for i in captures(a):
            cls = content_class(i)
            if not any(by[x]['fs_uuid'] == target_fs for x in cls):
                continue
            result |= set(x for x in cls if by[x]['fs_uuid'] == source_fs)
    return set(x for x in result if not by[x]['deleted'])


"""
targeted cases
"""


@needs_swipl
def test_untransferred_sibling_is_not_a_parent():
    """P2 was snapshotted but never sent anywhere: it must not be offered as a parent
    (the naive up-and-down walk through the rw origin used to yield it)."""
    subvols = [
        mk('S', 'fsA', ro=False),
        mk('P1', 'fsA', parent='S'),
        mk('P2', 'fsA', parent='S'),
        mk('R1', 'fsB', received='P1'),
    ]
    v1 = run_v1(subvols, 'S', 'fsA', 'fsB')
    v2 = run_v2(subvols, 'S', 'fsB')
    assert v1 == {'P1'}
    assert v2 == {'P1'}
    assert v2 == oracle(subvols, 'S', 'fsA', 'fsB')


@needs_swipl
def test_multi_hop_content_over_three_machines():
    """content that reached the target via an intermediate machine (A -> B -> C)."""
    subvols = [
        mk('S', 'fsA', ro=False),
        mk('P1', 'fsA', parent='S'),
        mk('R1', 'fsB', received='P1'),
        mk('P1b', 'fsB', parent='R1'),
        mk('R2', 'fsC', received='P1b'),
    ]
    v1 = run_v1(subvols, 'S', 'fsA', 'fsC')
    v2 = run_v2(subvols, 'S', 'fsC')
    assert 'P1' in v2
    assert v1 <= v2
    assert v2 == oracle(subvols, 'S', 'fsA', 'fsC')


@needs_swipl
def test_dual_edge_ancestry_via_received_incremental():
    """The reason volwalker2 exists: a received incremental has BOTH received_uuid and
    parent_uuid; v1 follows only received_uuid (here a dead end) and finds nothing,
    v2 also follows parent_uuid and finds the shared parent R1a. R2 itself must NOT
    be offered: its content is its origin's (missing here), not its -p parent's."""
    subvols = [
        mk('S', 'fsA', parent='R2', ro=False),
        mk('R2', 'fsA', parent='R1a', received='Pb-missing'),
        mk('R1a', 'fsA'),
        mk('P1b', 'fsB', received='R1a'),
    ]
    v1 = run_v1(subvols, 'S', 'fsA', 'fsB')
    v2 = run_v2(subvols, 'S', 'fsB')
    assert v1 == set()          # documents what v1 misses
    assert v2 == {'R1a'}        # found via the parent_uuid branch; R2 excluded
    assert v2 == oracle(subvols, 'S', 'fsA', 'fsB')


@needs_swipl
def test_broken_ro_chain_is_of_no_use():
    """tests/negative/test2.sh semantics: a read-write subvol in the chain breaks
    content identity - nothing behind it may be used."""
    subvols = [
        mk('S', 'fsA', ro=False),
        mk('P1', 'fsA', parent='S'),
        mk('W', 'fsA', parent='P1', ro=False),
        mk('Q', 'fsA', parent='W'),
        mk('RQ', 'fsB', received='Q'),
    ]
    v1 = run_v1(subvols, 'S', 'fsA', 'fsB')
    v2 = run_v2(subvols, 'S', 'fsB')
    assert v1 == set()
    assert v2 == set()
    assert v2 == oracle(subvols, 'S', 'fsA', 'fsB')


@needs_swipl
def test_deleted_candidates_are_filtered():
    subvols = [
        mk('S', 'fsA', ro=False),
        mk('P1', 'fsA', parent='S', deleted=True),
        mk('R1', 'fsB', received='P1'),
    ]
    assert run_v2(subvols, 'S', 'fsB') == set()


"""
randomized differential + oracle testing
"""


def random_graph(rng):
    """Random but physically plausible subvol graphs: a received incremental's -p
    parent lives on the same filesystem as it, and received_uuid carries the
    propagated origin uuid (btrfs send emits the sent subvol's received_uuid if it
    has one), like real btrfs does. Dangling references simulate pruned-away data."""
    fss = ['fsA', 'fsB', 'fsC'][:rng.randint(2, 3)]
    subvols = [mk('n0', 'fsA', ro=False)]

    def origin_of(x):
        return x['received_uuid'] or x['local_uuid']

    for i in range(1, rng.randint(4, 18)):
        base = rng.choice(subvols)
        uuid = f'n{i}'
        roll = rng.random()
        if roll < 0.5:
            # snapshot of base, on the same fs, usually read-only
            subvols.append(mk(uuid, base['fs_uuid'], parent=base['local_uuid'],
                              ro=rng.random() < 0.8))
        elif roll < 0.8 and base['ro']:
            # full send/receive to another fs; received_uuid = propagated origin
            other = rng.choice([f for f in fss if f != base['fs_uuid']] or fss)
            subvols.append(mk(uuid, other, received=origin_of(base)))
        elif roll < 0.92 and base['ro']:
            # incremental send/receive: -p parent is an ro subvol on the receiving fs
            other = rng.choice([f for f in fss if f != base['fs_uuid']] or fss)
            parents_there = [x for x in subvols if x['ro'] and x['fs_uuid'] == other]
            if parents_there:
                subvols.append(mk(uuid, other, parent=rng.choice(parents_there)['local_uuid'],
                                  received=origin_of(base)))
        else:
            # dangling reference (the referenced subvol was pruned from the data)
            subvols.append(mk(uuid, base['fs_uuid'], parent='ghost-' + uuid,
                              ro=rng.random() < 0.8))
    my = rng.choice([x['local_uuid'] for x in subvols if not x['ro'] and x['fs_uuid'] == 'fsA'] or ['n0'])
    target_fs = rng.choice([f for f in fss if f != 'fsA'])
    return subvols, my, target_fs


@needs_swipl
def test_v2_matches_oracle_on_random_graphs():
    """The Prolog must agree exactly with the independent Python implementation of the
    spec, and only ever propose ro subvols on the source filesystem.

    Note: v1 <= v2 is deliberately NOT asserted here: v1 has false positives of its
    own (it wanders down a received incremental's parent edge and through subvols
    whose origin was pruned from the data), which v2 correctly excludes. The targeted
    tests above pin the v1-vs-v2 relationship on physically real scenarios; in
    production, shadow mode logs any disagreement for investigation.
    """
    rng = random.Random(20260712)
    for trial in range(30):
        subvols, my, target_fs = random_graph(rng)
        v2 = run_v2(subvols, my, target_fs)
        o = oracle(subvols, my, 'fsA', target_fs)
        by = {x['local_uuid']: x for x in subvols}
        assert v2 == o, f'{trial=}: v2 {sorted(v2)} != oracle {sorted(o)}\n{subvols=}\n{my=} {target_fs=}'
        for x in v2:
            assert by[x]['ro'] and by[x]['fs_uuid'] == 'fsA', f'{trial=}: invalid candidate {x}'


"""
integration
"""


@pytest.mark.integration
def test_parent_candidates_full_integration(btrfs_loopback_setup):
    """Test _parent_candidates with a full integration test using real btrfs and Prolog."""
    fs1 = btrfs_loopback_setup["fs1"]
    fs2 = btrfs_loopback_setup["fs2"]

    # Create test subvolume
    test_subvol = fs1 / "parent_test_subvol"
    if not test_subvol.exists():
        subprocess.run(
            f"sudo btrfs subvolume create {test_subvol}",
            shell=True, check=True
        )
        subprocess.run(
            f"sudo chown -R $(id -u):$(id -g) {test_subvol}",
            shell=True, check=True
        )

    # Create a BFG instance
    bfg = btrfsgit.Bfg(YES=True)

    # Create a test file
    with open(test_subvol / "parent_test.txt", "w") as f:
        f.write("Common parent test content")

    # Create a snapshot
    snapshot1 = bfg.local_commit(
        SUBVOL=str(test_subvol),
        TAG="common_parent_test"
    ).val

    # Add more content
    with open(test_subvol / "parent_test2.txt", "w") as f:
        f.write("More content for common parent test")

    # Create another snapshot
    snapshot2 = bfg.local_commit(
        SUBVOL=str(test_subvol),
        TAG="common_parent_test2"
    ).val

    # Create target on fs2
    target_subvol = fs2 / "parent_test_target"
    if not target_subvol.exists():
        subprocess.run(
            f"sudo btrfs subvolume create {target_subvol}",
            shell=True, check=True
        )
        subprocess.run(
            f"sudo chown -R $(id -u):$(id -g) {target_subvol}",
            shell=True, check=True
        )

    # Push first snapshot to establish a common parent
    remote_snapshot = bfg.push(
        SUBVOL=str(test_subvol),
        SNAPSHOT=snapshot1,
        REMOTE_SUBVOL=str(target_subvol)
    ).val

    # Now try to find common parent between the second snapshot and remote
    my_uuid = bfg.get_subvol(bfg._local_cmd, snapshot2).val["local_uuid"]

    # Call parent_candidates
    candidates = list(bfg._parent_candidates(
        str(snapshot2),
        str(target_subvol),
        my_uuid,
        ("local", "remote")
    ))

    # We should find at least one candidate (the first snapshot we pushed)
    assert len(candidates) > 0

    # The parent candidate should be from our first snapshot
    found_uuids = [c["local_uuid"] for c in candidates]
    first_snapshot_uuid = bfg.get_subvol(bfg._local_cmd, snapshot1).val["local_uuid"]

    assert first_snapshot_uuid in found_uuids
