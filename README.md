# BtrFsGit - BFG

<p align="center"><a href="https://pypi.python.org/pypi/btrfsgit"><img src="https://img.shields.io/pypi/v/btrfsgit.svg" alt = "pypi Release Status"></a>

BFG stands for "B-tree Filesystem Git". It borrows git concepts for operations on BTRFS subvolumes without a central location. Commit, push, stash, checkout, pull, etc. And it tries to get the most out of incremental send/receive. I built this because my scenario is not just simple backup, but also transfering subvolumes back and forth between multiple machines, where no one machine is a single source of truth. In other words, a desktop computer and a notebook, and a subvol with a bunch of VM images. And then maybe a bunch of external backup HDDs...


## cool features
* It tries to figure out shared parents smartly, by walking the uuids of subvolumes of both filesystems. It doesn't just expect the last transferred snapshot to "be there", in a fixed location, like other tools do.
* No config files, just specify a source subvol and a target subvol on the command line, and in case of a remote machine, a ssh command to use.


## what this doesn't do (yet?)
* snapshot pruning
* cleanup after failure / .tmp destination
* finding shared parent by simply listing the snapshots dirs
* config files - for configuring your "remotes", for example.


## current todo
* what happens when there is only an incomplete snapshot on target? it should be rw, so the receive should fail. Do we want to get into ensuring that a snapshot is complete before using it? This sounds more like snazzer territory, but otoh, we can at least refuse to use .tmp's by default?

## planned features
* automatically saving and propagating `sub list` dumps - to allow finding shared parents also for offine generating of send streams, even across multiple machine hops
* Generating a send stream, and applying it later.

## wishlist
* some kind of integration with https://github.com/csirac2/snazzer/#snazzer for integrity checks
* maybe some automation for non-BTRFS backups, ie, create a snapshot, rsync it to an ext4, (and apply snazzer..)

## what this will probably never be
* an attempt to immitate more of git, like merging, exact same command syntax, commit messages (well maybe commit messages would make sense, maybe as a backend to datalad?)..


## status
`commit_and_push_and_checkout`, `remote_commit_and_pull`, and other commands work, but shared parent logic and some other areas are still being improved. The CLI lib we use, python-fire, behaves in unexpected ways sometimes.


## install:
```pip install --user btrfsgit```


## example workflow

this is how i ping-pong my data between my two machines:
```
bfg   \
  --sshstr=$SSHSTR  \
  commit_and_push_and_checkout  \
  --SUBVOLUME=/d \
  --REMOTE_SUBVOLUME=/mx500data/lean
```

...this:
* comes up with a snapshot name, by default this is "{timestamp}_from_{hostname}"
* makes a read-only snapshot of /d/ as /.bfg_snapshots.d/{snapshot name}
* finds the best shared parent and sends the snapshot to the other machine over ssh
* receives it on the other machine as /mx500data/.bfg_snapshots.lean/.incomplete/{snapshot name}
* makes a read-only snapshot of /mx500data/lean as /mx500data/.bfg_snapshots.lean/{timestamp}_stash
* deletes /mx500data/lean
* makes a read-write snapshot of the received snapshot, as /mx500data/lean


And back: `bfg --sshstr=$SSHSTR remote_commit_and_pull --SUBVOLUME=/d --REMOTE_SUBVOLUME=/mx500data/lean`


in this case my SSHSTR = `'/opt/hpnssh/usr/bin/ssh  -p 2222  -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes  koom@10.0.0.20"`

full output:
[example_session.md](misc/example_session.md)

see also:
[test1](tests/test1.sh)

## available commands
[docs](docs/bfg/bfg.md)


## prerequisites
### mount the root
#### problem
If your root partition is BTRFS, your "/" is probably not the true top level subvolume (id 5) of the filesystem, but merely "/@". This is how linux distributions set up your system by default. These non-toplevel mounts make it hard for BFG to map subvolume UUIDs to full filesystem paths, so, it may not be able to figure out the correct `-p` argument for `btrfs send` commands.
#### solution
Make sure that the root subvolume of your BTRFS filesystem is always mounted. For example my fstab entry is:
```
/dev/mapper/nvme0n1p6_crypt /nvme0n1p6_crypt_root  btrfs   defaults,subvol=   0   2
```

### avoid nested subvolumes
#### problem
To be able to make use of stash and checkout, the subvolume that you want to manage with BFG should not contain other subvolumes, so that it can be `btrfs subvolume delete`'d without affecting your snapshots or other subvolumes. (or possibly we could just `mv`?)
#### solution
As an example, i have a subvolume `/data`, and by default, BFG will store all snapshots in `/.bfg_snapshots.data`, and i don't have snapper doing stuff in `/data/.snapshots`.

### prevent writes to incomplete snapshots
#### problem
BTRFS doesn't make a subvolume read-only when it's `btrfs receive`-ing. If another program writes into it at that time, something bad will happen..
#### solution
Don't touch them! Snapshots in progress are stored in `.incomplete/`.


## dev install:
```
pip install --user poetry
`cd BtrFsGit
poetry install # only installs the executable into somewhere like `/.cache/pypoetry/virtualenvs/bfg-iXQCHChq-py3.6/bin/`. It just doesn't have a "development mode" like setuptools have with `pip install -e .`. So find that directory, and copy the `bfg` into your `~/.local/bin/`. But that's about to be [fixed soon](https://github.com/python-poetry/poetry/issues/34).
```

