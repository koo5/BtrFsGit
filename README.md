# BFG
Work in progress.
B-tree Filesystem Git attempts to enable git-like workflow for subvolumes. Commit, push, checkout, stash, pull..

## why
I built this because my scenario is not just simple backup, but also transfering subvolumes back and forth between multiple machines, where no one machine is a single source of truth. In other words, a desktop computer and a notebook, and a subvol with a bunch of VM images. And then maybe a bunch of external backup HDDs. 

## cool features
* It tries to figure out shared parents smartly.
* No config files, just specify a source subvol and a target subvol on the command line.

## planned features / wishlist
* automatically saving and propagating `sub list` dumps - to allow finding shared parents also for offine generating of send streams, even across multiple machine hops
* Generating a send stream, and applying it later.
* some kind of integration with https://github.com/csirac2/snazzer/#snazzer for integrity checks
* maybe some automation for non-BTRFS backups, ie, create a snapshot, rsync it to an ext4, (and apply snazzer..)

## example workflow
[test1](tests/test1.sh)

## available commands
[docs](docs/)

## prerequisites

### install
This isnt a proper python package yet. Python3.8 is expected. Checkout the repo, do
```
 virtualenv -p /usr/bin/python3.8 venv
 pip install -r requirements.txt 
 
```
### mount the root
#### problem
If you want to work with subvolumes mounted with `subvol=..`: This is how linux distributions set up your system by default. In this case, BFG would not be able to automatically find the filesystem path of a subvolume given its UUID, so, it wouldn't be able to call `btrfs send` with correct `-c` parents.
#### solution
make sure that the root subvolume of your BTRFS filesystem is always mounted. For example my fstab entry:
```
/dev/mapper/nvme0n1p6_crypt /nvme0n1p6_crypt_root  btrfs   defaults,subvol=   0   2
```
For some operations, you will need to pass this filesystem path like so: `--LOCAL_FS_ROOT_MOUNT_POINT=...` or `--REMOTE_FS_ROOT_MOUNT_POINT=...`.
### avoid nested subvolumes
#### problem
To be able to make use of stash and checkout, the subvolume that you want to manage with BFG should not contain other subvolumes, so that it can be `btrfs subvolume delete`'d automatically.
#### solution
As an example, i have a subvolume `/data`. By default, BFG will store all its data in `/.bfg_snapshots.data`, and i don't have snapper or similar doing stuff in `/data/.snapshots` or similar.

### prevent writes to incomplete snapshots
#### problem
BTRFS doesn't make a subvolume read-only when it's `btrfs receive`-ing. If another program writes into it at that time, something bad will happen..
#### solution
make sure it doesn't happen!


