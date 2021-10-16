Module main
===========

Classes
-------

`Bfg(sshstr='')`
:   

    ### Methods

    `calculate_snapshot_parent_dir(s, VOL)`
    :   VOL: your subvolume (for example /data).
        Calculate the default snapshot parent dir. In the filesystem tree, it is on the same level as your subvolume, for example /.bfg_snapshots.data.

    `calculate_snapshot_path(s, VOL, TAG)`
    :   calculate the filesystem path where a snapshot should go, given a subvolume and a tag

    `checkout_local(s, SNAPSHOT, SUBVOLUME)`
    :   stash your SUBVOLUME, and replace it with SNAPSHOT

    `checkout_remote(s, SNAPSHOT, SUBVOLUME)`
    :   ssh into the other machine,
        stash your SUBVOLUME, and replace it with SNAPSHOT

    `commit(s, VOL='/', SNAPSHOTS_CONTAINER=None, TAG=None, SNAPSHOT=None)`
    :   come up with a filesystem path for a snapshot, and snapshot VOL.

    `commit_and_generate_patch(s)`
    :

    `commit_and_push(s, fs_root_mount_point=None, subvolume='/', remote_subvolume='/bfg', parents: List[str] = None)`
    :

    `commit_and_push_and_checkout(s, fs_root_mount_point=None, subvolume='/', remote_subvolume='/bfg')`
    :

    `find_common_parents(s, fs_root_mount_point='/', subvolume='/', remote_subvolume='/')`
    :

    `local_make_ro_snapshot(s, VOL, SNAPSHOT)`
    :   make a read-only snapshot of VOL into SNAPSHOT, locally

    `push(s, fs_root_mount_point, subvolume, snapshot, remote_subvolume, parents=None)`
    :   try to figure out shared parents, if not provided.
        subvolume is probably not needed and fs_root_mount_point can be used?

    `stash_local(s, SUBVOLUME)`
    :   snapshot and delete your SUBVOLUME