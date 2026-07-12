"""End-to-end tests: bfg commands emit the right quads through schnabel.

Mocks the actual ``btrfs`` / ssh subprocess calls so tests run anywhere.
"""

import io
from pathlib import Path
from unittest.mock import patch

import pytest
from rdflib import Literal, URIRef

from btrfsgit.btrfsgit import (
    Bfg,
    Res,
    _parse_pv_log_line,
    _emit_bytes_progress,
)
from schnabel import EventLog
from schnabel.vocab import (
    BFG,
    latest_invocation,
    points_to,
    status as status_pred,
    error as error_pred,
    invoked_by,
    STATUS_COMPLETE,
    STATUS_FAILED,
)


@pytest.fixture
def bfg_with_log():
    bfg = Bfg(YES=True)
    bfg._log = EventLog({"backend": "memory"})
    return bfg


# ---------------------------------------------------------------------------
# local_commit
# ---------------------------------------------------------------------------


def test_local_commit_emits_invocation_graph(bfg_with_log):
    bfg = bfg_with_log
    fake_snapshot = Path("/tmp/.bfg_snapshots/data/data_2026-05-20_12-00-00_from_testhost")

    with patch.object(bfg, "_figure_out_snapshot_name", return_value=fake_snapshot), \
         patch.object(bfg, "_local_make_ro_snapshot") as make_ro:
        res = bfg.local_commit(SUBVOL="/data")

    assert make_ro.called
    assert res.val == fake_snapshot

    inv_rows = list(bfg._log.query(
        f"SELECT ?inv WHERE {{ <{latest_invocation}> <{points_to}> ?inv }}"
    ))
    assert len(inv_rows) == 1
    inv = inv_rows[0][0]
    assert str(inv).endswith("_localcommit")

    quads = list(bfg._log.quads(inv))
    predicates = {str(q[1]) for q in quads}
    objects = {str(q[2]) for q in quads}
    assert str(BFG.LocalCommit) in objects
    assert str(BFG.onHost) in predicates
    assert str(BFG.subvol) in predicates
    assert str(BFG.intendedSnapshot) in predicates
    assert str(BFG.snapshot) in predicates
    assert str(BFG.abspath) in predicates

    status_rows = list(bfg._log.query(
        f"SELECT ?s WHERE {{ GRAPH <{inv}> {{ <{inv}> <{status_pred}> ?s }} }}"
    ))
    assert {str(r[0]) for r in status_rows} == {str(STATUS_COMPLETE)}


def test_local_commit_emits_failure_when_snapshot_raises(bfg_with_log):
    bfg = bfg_with_log
    fake_snapshot = Path("/tmp/.bfg_snapshots/data/data_2026-05-20_12-00-01_from_testhost")

    with patch.object(bfg, "_figure_out_snapshot_name", return_value=fake_snapshot), \
         patch.object(bfg, "_local_make_ro_snapshot",
                      side_effect=RuntimeError("simulated btrfs failure")):
        with pytest.raises(RuntimeError, match="simulated btrfs failure"):
            bfg.local_commit(SUBVOL="/data")

    inv_rows = list(bfg._log.query(
        f"SELECT ?inv WHERE {{ <{latest_invocation}> <{points_to}> ?inv }}"
    ))
    assert len(inv_rows) == 1
    inv = inv_rows[0][0]

    status_rows = list(bfg._log.query(
        f"SELECT ?s WHERE {{ GRAPH <{inv}> {{ <{inv}> <{status_pred}> ?s }} }}"
    ))
    assert {str(r[0]) for r in status_rows} == {str(STATUS_FAILED)}

    error_rows = list(bfg._log.query(
        f"SELECT ?e WHERE {{ GRAPH <{inv}> {{ <{inv}> <{error_pred}> ?e }} }}"
    ))
    assert any("simulated btrfs failure" in str(r[0]) for r in error_rows)


def test_local_commit_is_null_safe_when_no_quadstore(monkeypatch):
    monkeypatch.delenv("QUADSTORE", raising=False)
    bfg = Bfg(YES=True)
    assert bfg._log.is_null is True

    fake_snapshot = Path("/tmp/.bfg_snapshots/data/data_2026-05-20_12-00-02_from_testhost")
    with patch.object(bfg, "_figure_out_snapshot_name", return_value=fake_snapshot), \
         patch.object(bfg, "_local_make_ro_snapshot"):
        res = bfg.local_commit(SUBVOL="/data")
    assert res.val == fake_snapshot


# ---------------------------------------------------------------------------
# push
# ---------------------------------------------------------------------------


def _stub_push_deps(bfg, *, parent_record=None):
    """Patch the helpers push() calls into so we can run it without btrfs/ssh."""
    snapshot_parent_dir = Res("/backup/.bfg_snapshots/data")

    patches = [
        patch.object(bfg, "calculate_default_snapshot_parent_dir",
                     return_value=snapshot_parent_dir),
        patch.object(bfg, "_remote_cmd"),
        patch.object(bfg, "get_subvol",
                     return_value=Res({"local_uuid": "my-uuid-here"})),
        patch.object(bfg, "find_common_parent",
                     return_value=Res(parent_record)),
        patch.object(bfg, "local_send"),
    ]
    return patches


def test_push_emits_parent_snapshot_when_one_is_found(bfg_with_log):
    bfg = bfg_with_log
    parent_record = {
        "abspath": "/backup/.bfg_snapshots/data/data_2026-05-19_07-00-00_from_jj",
        "local_uuid": "parent-local-uuid",
        "received_uuid": "parent-received-uuid",
    }
    patches = _stub_push_deps(bfg, parent_record=parent_record)
    for p in patches:
        p.start()
    try:
        res = bfg.push(SUBVOL="/data",
                       SNAPSHOT="/.bfg_snapshots/data/data_2026-05-20_12-00-00_from_jj",
                       REMOTE_SUBVOL="/backup/data")
    finally:
        for p in patches:
            p.stop()

    assert "data_2026-05-20_12-00-00_from_jj" in res.val

    inv_rows = list(bfg._log.query(
        f"SELECT ?inv WHERE {{ <{latest_invocation}> <{points_to}> ?inv }}"
    ))
    assert len(inv_rows) == 1
    inv = inv_rows[0][0]
    assert str(inv).endswith("_push")

    # The parent snapshot is reified as its own resource inside the inv graph.
    parent_rows = list(bfg._log.query(f"""
        SELECT ?p ?abs ?lu ?ru WHERE {{ GRAPH <{inv}> {{
            <{inv}> <{BFG.parentSnapshot}> ?p .
            ?p <{BFG.abspath}> ?abs .
            OPTIONAL {{ ?p <{BFG.localUuid}> ?lu }} .
            OPTIONAL {{ ?p <{BFG.receivedUuid}> ?ru }} .
        }} }}
    """))
    assert len(parent_rows) == 1
    p, abs_, lu, ru = parent_rows[0]
    assert str(abs_) == parent_record["abspath"]
    assert str(lu) == parent_record["local_uuid"]
    assert str(ru) == parent_record["received_uuid"]

    # The pushedTo path is emitted as the result location.
    pushed_rows = list(bfg._log.query(
        f"SELECT ?d WHERE {{ GRAPH <{inv}> {{ <{inv}> <{BFG.pushedTo}> ?d }} }}"
    ))
    assert any("data_2026-05-20_12-00-00_from_jj" in str(r[0]) for r in pushed_rows)

    status_rows = list(bfg._log.query(
        f"SELECT ?s WHERE {{ GRAPH <{inv}> {{ <{inv}> <{status_pred}> ?s }} }}"
    ))
    assert {str(r[0]) for r in status_rows} == {str(STATUS_COMPLETE)}


def test_push_skips_parent_emission_when_no_common_parent_found(bfg_with_log):
    bfg = bfg_with_log
    patches = _stub_push_deps(bfg, parent_record=None)
    for p in patches:
        p.start()
    try:
        bfg.push(SUBVOL="/data",
                 SNAPSHOT="/.bfg_snapshots/data/data_2026-05-20_12-00-00_from_jj",
                 REMOTE_SUBVOL="/backup/data")
    finally:
        for p in patches:
            p.stop()

    inv_rows = list(bfg._log.query(
        f"SELECT ?inv WHERE {{ <{latest_invocation}> <{points_to}> ?inv }}"
    ))
    inv = inv_rows[0][0]
    parent_rows = list(bfg._log.query(
        f"SELECT ?p WHERE {{ GRAPH <{inv}> {{ <{inv}> <{BFG.parentSnapshot}> ?p }} }}"
    ))
    # No parent was found, so no parentSnapshot triple should exist.
    assert parent_rows == []


# ---------------------------------------------------------------------------
# byte counting: pure-function helpers
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("line,last,expected", [
    ("100", 0, 100),
    ("100\n", 0, 100),
    ("  200  \n", 100, 200),
    ("50", 100, None),        # not monotonically increasing
    ("100", 100, None),       # equal isn't > last
    ("", 0, None),            # blank
    ("garbage", 0, None),     # non-numeric
    ("-1", 0, None),          # negative not > 0
])
def test_parse_pv_log_line(line, last, expected):
    assert _parse_pv_log_line(line, last) == expected


def test_emit_bytes_progress_writes_to_both_sinks_when_invocation_present(bfg_with_log):
    bfg = bfg_with_log
    with bfg._log.invocation(BFG.Push) as inv:
        _emit_bytes_progress(inv, 12345, 'local_send (test)')

    rows = list(bfg._log.query(f"""
        SELECT ?n WHERE {{ GRAPH <{inv.iri}> {{ <{inv.iri}> <{BFG.bytesTransferred}> ?n }} }}
    """))
    assert [int(r[0]) for r in rows] == [12345]


def test_emit_bytes_progress_is_safe_when_invocation_is_none():
    # Must not raise; just goes to _prerr.
    _emit_bytes_progress(None, 999, 'local_send (test)')


# ---------------------------------------------------------------------------
# local_send: pv injection
# ---------------------------------------------------------------------------


def test_local_send_injects_pv_when_available(bfg_with_log, monkeypatch):
    captured = {}

    def fake_check_call(cmd, **kwargs):
        captured['cmd'] = cmd
        return 0

    monkeypatch.setattr('btrfsgit.btrfsgit.subprocess.check_call', fake_check_call)
    monkeypatch.setattr('btrfsgit.btrfsgit._PV_AVAILABLE', True)

    with bfg_with_log._log.invocation(BFG.Push) as inv:
        bfg_with_log.local_send('/snap/path', ' > /dev/null', None, [], invocation=inv)

    assert 'btrfs send' in captured['cmd']
    assert 'pv -nbf' in captured['cmd']


def test_local_send_skips_pv_when_unavailable(bfg_with_log, monkeypatch):
    captured = {}

    def fake_check_call(cmd, **kwargs):
        captured['cmd'] = cmd
        return 0

    monkeypatch.setattr('btrfsgit.btrfsgit.subprocess.check_call', fake_check_call)
    monkeypatch.setattr('btrfsgit.btrfsgit._PV_AVAILABLE', False)

    with bfg_with_log._log.invocation(BFG.Push) as inv:
        bfg_with_log.local_send('/snap/path', ' > /dev/null', None, [], invocation=inv)

    assert 'btrfs send' in captured['cmd']
    assert 'pv -nbf' not in captured['cmd']


# ---------------------------------------------------------------------------
# remote_send: in-process byte counter
# ---------------------------------------------------------------------------


class _FakePopen:
    """Just enough of a subprocess.Popen to feed remote_send's read/write loop."""

    def __init__(self, *args, stdout=None, stdin=None, **kwargs):
        # If stdout is requested as PIPE, we'll prefill from `_FakePopen.next_stdout`.
        self.stdout = _FakePopen._pending_stdout if stdout is not None else None
        self.stdin = io.BytesIO() if stdin is not None else None
        self.returncode = 0

    def wait(self):
        return 0

    _pending_stdout: io.BytesIO = None


def test_remote_send_emits_bytes_periodically_and_final(bfg_with_log, monkeypatch):
    bfg = bfg_with_log
    # 200 MiB → 3 mid-stream emits (at 64, 128, 192 MiB) + 1 final
    data = b'\x00' * (200 * 1024 * 1024)
    _FakePopen._pending_stdout = io.BytesIO(data)

    monkeypatch.setattr('btrfsgit.btrfsgit.subprocess.Popen', _FakePopen)

    with bfg._log.invocation(BFG.Pull) as inv:
        bfg.remote_send('/remote/snap', '/local/dir', None, [], invocation=inv)

    byte_rows = list(bfg._log.query(f"""
        SELECT ?n WHERE {{ GRAPH <{inv.iri}> {{ <{inv.iri}> <{BFG.bytesTransferred}> ?n }} }}
    """))
    counts = sorted(int(r[0]) for r in byte_rows)
    # At least three intermediate emits plus the final.
    assert len(counts) >= 4
    # Final emit equals the full transferred size.
    assert counts[-1] == len(data)


def test_push_emits_parent_when_caller_provides_one(bfg_with_log):
    bfg = bfg_with_log
    # When the caller passes PARENT explicitly, find_common_parent isn't even
    # called, but we still want the parent reified in the invocation.
    patches = _stub_push_deps(bfg, parent_record=None)
    for p in patches:
        p.start()
    try:
        bfg.push(SUBVOL="/data",
                 SNAPSHOT="/.bfg_snapshots/data/data_2026-05-20_12-00-00_from_jj",
                 REMOTE_SUBVOL="/backup/data",
                 PARENT="/backup/.bfg_snapshots/data/data_2026-05-19_07-00-00_from_jj")
    finally:
        for p in patches:
            p.stop()

    inv_rows = list(bfg._log.query(
        f"SELECT ?inv WHERE {{ <{latest_invocation}> <{points_to}> ?inv }}"
    ))
    inv = inv_rows[0][0]
    parent_abs_rows = list(bfg._log.query(f"""
        SELECT ?abs WHERE {{ GRAPH <{inv}> {{
            <{inv}> <{BFG.parentSnapshot}> ?p .
            ?p <{BFG.abspath}> ?abs .
        }} }}
    """))
    assert [str(r[0]) for r in parent_abs_rows] == \
        ["/backup/.bfg_snapshots/data/data_2026-05-19_07-00-00_from_jj"]


# ---------------------------------------------------------------------------
# compound commands link their sub-invocations as children
# ---------------------------------------------------------------------------


def test_commit_and_push_links_local_commit_and_push_as_children(bfg_with_log):
    bfg = bfg_with_log
    fake_snapshot = Path("/tmp/.bfg_snapshots/data/data_2026-05-21_07-00-00_from_jj")
    parent_record = {"abspath": "/backup/.bfg_snapshots/data/data_2026-05-20_07-00-00_from_jj"}

    with patch.object(bfg, "_figure_out_snapshot_name", return_value=fake_snapshot), \
         patch.object(bfg, "_local_make_ro_snapshot"), \
         patch.object(bfg, "calculate_default_snapshot_parent_dir",
                      return_value=Res("/backup/.bfg_snapshots/data")), \
         patch.object(bfg, "_remote_cmd"), \
         patch.object(bfg, "get_subvol", return_value=Res({"local_uuid": "u"})), \
         patch.object(bfg, "find_common_parent", return_value=Res(parent_record)), \
         patch.object(bfg, "local_send"):
        bfg.commit_and_push(SUBVOL="/data", REMOTE_SUBVOL="/backup/data")

    # Three invocations should exist: commit_and_push (outer), local_commit, push.
    rows = list(bfg._log.query(f"""
        PREFIX core: <urn:schnabel:vocab:core:>
        SELECT ?inv ?type ?parent WHERE {{
            GRAPH ?inv {{
                ?inv a ?type .
                OPTIONAL {{ ?inv <{invoked_by}> ?parent }}
            }}
        }}
    """))
    by_type = {str(t).rsplit(":", 1)[-1]: (str(inv), str(p) if p else None)
               for inv, t, p in rows}
    assert set(by_type.keys()) == {"CommitAndPush", "LocalCommit", "Push"}

    outer_iri, outer_parent = by_type["CommitAndPush"]
    lc_iri, lc_parent = by_type["LocalCommit"]
    push_iri, push_parent = by_type["Push"]
    assert outer_parent is None
    assert lc_parent == outer_iri
    assert push_parent == outer_iri


def test_remote_commit_and_pull_links_three_sub_invocations(bfg_with_log):
    bfg = bfg_with_log
    fake_remote_snap = Path("/d2/.bfg_snapshots/data/data_2026-05-21_remote_commit")
    fake_local_snap_dir = Res("/tmp/.bfg_snapshots/data")

    with patch.object(bfg, "calculate_default_snapshot_path",
                      return_value=Res(fake_remote_snap)), \
         patch.object(bfg, "_remote_make_ro_snapshot"), \
         patch.object(bfg, "calculate_default_snapshot_parent_dir",
                      return_value=fake_local_snap_dir), \
         patch.object(bfg, "_local_cmd"), \
         patch.object(bfg, "get_subvol", return_value=Res({"local_uuid": "u"})), \
         patch.object(bfg, "find_common_parent", return_value=Res(None)), \
         patch.object(bfg, "remote_send"), \
         patch.object(bfg, "stash_local"):
        bfg.remote_commit_and_pull(REMOTE_SUBVOL="/data", SUBVOL="/local-data")

    rows = list(bfg._log.query(f"""
        PREFIX core: <urn:schnabel:vocab:core:>
        SELECT ?inv ?type ?parent WHERE {{
            GRAPH ?inv {{
                ?inv a ?type .
                OPTIONAL {{ ?inv <{invoked_by}> ?parent }}
            }}
        }}
    """))
    by_type = {str(t).rsplit(":", 1)[-1]: (str(inv), str(p) if p else None)
               for inv, t, p in rows}
    assert set(by_type.keys()) == \
        {"RemoteCommitAndPull", "RemoteCommit", "Pull", "CheckoutLocal"}

    outer_iri, outer_parent = by_type["RemoteCommitAndPull"]
    assert outer_parent is None
    # The three sub-invocations all link to the compound outer.
    for kind in ("RemoteCommit", "Pull", "CheckoutLocal"):
        _, parent = by_type[kind]
        assert parent == outer_iri, f"{kind} should link to outer compound"
