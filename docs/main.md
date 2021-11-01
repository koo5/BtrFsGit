Module main
===========
python 3.8 required (at least for shlex.join)

Classes
-------

`Bfg(LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=None, REMOTE_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=None, sshstr='', YES=False)`
:   

    ### Methods

    `calculate_default_snapshot_parent_dir(s, SUBVOLUME)`
    :   SUBVOLUME: your subvolume (for example /data).
        Calculate the default snapshot parent dir. In the filesystem tree, it is on the same level as your subvolume, for example `/.bfg_snapshots.data`

    `calculate_default_snapshot_path(s, SUBVOLUME, TAG)`
    :   calculate the filesystem path where a snapshot should go, given a subvolume and a tag

    `checkout_local(s, SNAPSHOT, SUBVOLUME)`
    :   stash your SUBVOLUME, and replace it with SNAPSHOT

    `checkout_remote(s, SNAPSHOT, SUBVOLUME)`
    :   ssh into the other machine,
        stash your SUBVOLUME, and replace it with SNAPSHOT

    `commit_and_generate_patch(s, SUBVOLUME='/', PATCH_FILE_DIR='/', PARENTS: List[str] = None)`
    :   store a `btrfs send` stream locally
        
        :param SUBVOLUME:
        :param PATCH_FILE_DIR:
        :param PARENTS:
        :return:

    `commit_and_push(s, SUBVOLUME='/', REMOTE_SUBVOLUME='/bfg', PARENTS: List[str] = None)`
    :   commit, and transfer the snapshot into .bfg_snapshots on the other machine

    `commit_and_push_and_checkout(s, SUBVOLUME, REMOTE_SUBVOLUME, PARENTS: List[str] = None)`
    :   Snapshot your data, "btrfs send"/"btrfs receive" the snapshot to the other machine, and checkout it there
        
        :param FS_ROOT_MOUNT_POINT: mount point of SUBVOLUME filesystem
        :param SUBVOLUME: your data
        :param REMOTE_SUBVOLUME: desired filesystem path of your data on the other machine
        :return: filesystem path of the snapshot created on the other machine

    `find_common_parent(s, subvolume, remote_subvolume, my_uuid, direction)`
    :

    `get_subvol_uuid_by_path(s, runner, path)`
    :

    `local_commit(s, SUBVOLUME='/', TAG=None, SNAPSHOT=None)`
    :   come up with a filesystem path for a snapshot, and snapshot SUBVOLUME.
        :param SNAPSHOT: override default filesystem path where snapshot will be created
        :param TAG: override the tag for the default SNAPSHOT (hostname by default)

    `local_send(s, SNAPSHOT, target, PARENT, CLONESRCS)`
    :

    `parent_candidates(s, subvolume, remote_subvolume, my_uuid, direction)`
    :

    `pull(s, REMOTE_SNAPSHOT, LOCAL_SUBVOLUME, PARENT=None, CLONESRCS=[])`
    :

    `push(s, SUBVOLUME, SNAPSHOT, REMOTE_SUBVOLUME, PARENT=None, CLONESRCS=[])`
    :   Try to figure out shared parents, if not provided.
        
        todo: subvolume is probably not needed and fs_root_mount_point can be used?

    `remote_commit(s, REMOTE_SUBVOLUME)`
    :

    `remote_commit_and_pull(s, REMOTE_SUBVOLUME, SUBVOLUME)`
    :   same as commit_and_push_and_checkout but going the other direction
        
        :param FS_ROOT_MOUNT_POINT:
        :param REMOTE_SUBVOLUME:
        :param SUBVOLUME:
        :return:

    `remote_send(s, REMOTE_SNAPSHOT, LOCAL_DIR, PARENT, CLONESRCS)`
    :

    `stash_local(s, SUBVOLUME)`
    :   snapshot and delete your SUBVOLUME
        
        todo: maybe an alternative way should be to just move it?

    `stash_remote(s, SUBVOLUME)`
    :   snapshot and delete your SUBVOLUME
