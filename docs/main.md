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

    `checkout(s, what, where)`
    :

    `commit(s, VOL='/', SNAPSHOTS_CONTAINER=None, TAG=None, SNAPSHOT=None)`
    :

    `commit_and_generate_patch(s)`
    :

    `commit_and_push(s, fs_root_mount_point=None, subvolume='/', remote_subvolume='/bfg', parents: List[str] = None)`
    :

    `commit_and_push_and_checkout(s, subvolume='/', remote_subvolume='/')`
    :

    `find_common_parents(s, fs_root_mount_point='/', subvolume='/', remote_subvolume='/')`
    :

    `local_make_ro_snapshot(s, VOL, SNAPSHOT)`
    :

    `push(s, fs_root_mount_point, subvolume, snapshot, remote_subvolume, parents=None)`
    :

    `stash(s, what)`
    :