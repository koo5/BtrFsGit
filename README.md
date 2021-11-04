# BFG
B-tree Filesystem Git attempts to enable git-like workflow for subvolumes. Commit, push, checkout, stash, pull..

<p align="center">
<a href="https://pypi.python.org/pypi/btrfsgit">
    <img src="https://img.shields.io/pypi/v/btrfsgit.svg"
        alt = "Release Status">
</a>


## user install:
```pip install --user BtrFsGit```


## dev install:
```
pip install --user poetry
`cd BtrFsGit
poetry install # only installs the executable into somewhere like `/.cache/pypoetry/virtualenvs/bfg-iXQCHChq-py3.6/bin/`. It just doesn't have a "development mode" like setuptools have with `pip install -e .`. So find that directory, and copy the `bfg` into your `~/.local/bin/`. But that's about to be [fixed soon](https://github.com/python-poetry/poetry/issues/34).
```



## status
Undertested, but `commit_and_push_and_checkout`, `remote_commit_and_pull` and other commands work. python-fire (the CLI lib) behaves in unexpected ways sometimes. Data loss could occur ;)

## why
I built this because my scenario is not just simple backup, but also transfering subvolumes back and forth between multiple machines, where no one machine is a single source of truth. In other words, a desktop computer and a notebook, and a subvol with a bunch of VM images. And then maybe a bunch of external backup HDDs.

## cool features
* It tries to figure out shared parents smartly, by walking the uuids of subvolumes of both filesystems. It doesn't just expect the last transferred snapshot to "be there", in a fixed location, like other tools do.
* No config files, just specify a source subvol and a target subvol (and the ID 5 mount point) on the command line, and in case of a remote machine, a ssh command to use.

## what this doesn's do (yet?)
* snapshot pruning
* cleanup after failure / .tmp destination
* finding shared parent by simply listing the snapshots dirs
* config files

## todo
* what happens when there is only an incomplete snapshot on target?

## planned features
* automatically saving and propagating `sub list` dumps - to allow finding shared parents also for offine generating of send streams, even across multiple machine hops
* Generating a send stream, and applying it later.

## wishlist
* some kind of integration with https://github.com/csirac2/snazzer/#snazzer for integrity checks
* maybe some automation for non-BTRFS backups, ie, create a snapshot, rsync it to an ext4, (and apply snazzer..)

## what this will probably never be
* an attempt to immitate more of git, like merging, exact same command syntax, commit messages (well maybe commit messages would make sense, maybe as a backend to datalad?)..


## example workflow
this is how i ping-pong my data between my two machines:
```
bfg   \
  --YES=true  \  #  no confirmations
  --LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=/nvme0n1p6_crypt_root  \  # ugly hack
  --sshstr='/opt/hpnssh/usr/bin/ssh   -p 2222   -o TCPRcvBufPoll=yes -o NoneSwitch=yes  -o NoneEnabled=yes     koom@10.0.0.20'  \
  commit_and_push_and_checkout  \  # the command
  --SUBVOLUME=/d \  # source
  --REMOTE_SUBVOLUME=/mx500data/lean  # target
```
...this:
* makes a read-only snapshot of /d/ in /.bfg_snapshots.d/<timestamp>_from_<hostname>
* finds the best shared parent and sends the snapshot to the other machine over ssh
* receives it on the other machine in /mx500data/.bfg_snapshots.lean
* makes a read-only snapshot of /mx500data/lean in /mx500data/.bfg_snapshots.lean/<timestamp>_stash
* deletes /mx500data/lean
* makes a read-write snapshot of the received snapshot, in /mx500data/lean


And back:
```
bfg   --YES=true    --REMOTE_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=/mx500data    --sshstr='/opt/hpnssh/usr/bin/ssh   -p 2222   -o TCPRcvBufPoll=yes -o NoneSwitch=yes  -o NoneEnabled=yes     koom@10.0.0.20'   remote_commit_and_pull   --SUBVOLUME=/d  --REMOTE_SUBVOLUME=/mx500data/lean
```
full output:
[example_session.md](misc/example_session.md)

see also:
[test1](tests/test1.sh)

## available commands
[docs](docs/bfg/bfg.md)

## prerequisites

### install
This isnt a proper python package yet. Python3.8 is expected. Checkout the repo, do
```
 virtualenv -p /usr/bin/python3.8 venv
 pip install -r requirements.txt

```
### mount the root
#### problem
If you want to work with subvolumes mounted with `subvol=..`: This is how linux distributions set up your system by default. In this case, BFG would not be able to automatically find the filesystem path of a subvolume given its UUID, so, it wouldn't be able to call `btrfs send` with correct `-p` parents.
#### solution
make sure that the root subvolume of your BTRFS filesystem is always mounted. For example my fstab entry:
```
/dev/mapper/nvme0n1p6_crypt /nvme0n1p6_crypt_root  btrfs   defaults,subvol=   0   2
```
For some operations, you will need to pass this mountpoint like so: `--LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=...` or `--REMOTE_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=...`.
### avoid nested subvolumes
#### problem
To be able to make use of stash and checkout, the subvolume that you want to manage with BFG should not contain other subvolumes, so that it can be `btrfs subvolume delete`'d without affecting your snapshots or other subvolumes. (or possibly we could just `mv`?)
#### solution
As an example, i have a subvolume `/data`, and by default, BFG will store all snapshots in `/.bfg_snapshots.data`, and i don't have snapper doing stuff in `/data/.snapshots`.

### prevent writes to incomplete snapshots
#### problem
BTRFS doesn't make a subvolume read-only when it's `btrfs receive`-ing. If another program writes into it at that time, something bad will happen..
#### solution
don't do it!


