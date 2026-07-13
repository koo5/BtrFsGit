import logging, os
import json, subprocess, tempfile
from datetime import datetime
from pathlib import Path


class Volwalker2Error(Exception):
	pass


def json_encoder(obj):
	if isinstance(obj, datetime):
		return obj.isoformat()
	elif isinstance(obj, Path):
		return str(obj)
	raise TypeError(f"Object of type {type(obj)} is not JSON serializable")


def common_parents(by_uuid, my_uuid, target_fs, timeout=120):
	"""
	Find candidate `btrfs send -p` parents by querying volwalker2.pl (SWI-Prolog).

	by_uuid: dict of subvolume records by local_uuid. Each record needs local_uuid,
		fs_uuid, parent_uuid, received_uuid, ro; deleted defaults to False.
	my_uuid: uuid of the subvolume we are transferring
	target_fs: uuid (or other fs identifier consistent with the records' fs_uuid)
		of the filesystem we are transferring to

	Yields the candidate records, deduplicated, roughly nearest-ancestry first
	(the order Prolog emits them in). Records marked deleted are skipped.
	"""
	if my_uuid not in by_uuid:
		raise Volwalker2Error(f'{my_uuid=} not in by_uuid')

	source_fs = by_uuid[my_uuid]['fs_uuid']
	logging.debug(f'find common parents for transferring {my_uuid=} from {source_fs=} to {target_fs=}')

	# serializable copies - don't mutate the caller's records
	subvols = [
		dict(x, path=str(x.get('path', '')), deleted=x.get('deleted', False))
		for x in by_uuid.values()
	]
	subvols_json = json.dumps(subvols, default=json_encoder)

	script_path = os.path.join(os.path.dirname(__file__), 'volwalker2.pl')

	# Write JSON to a temporary file to avoid command line length limits
	with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
		json_file = f.name
		f.write(subvols_json)

	try:
		cmd = ['swipl', '-s', script_path, '-g',
			f'find_common_parents_from_file("{json_file}", "{my_uuid}", "{source_fs}", "{target_fs}")',
			'-t', 'halt']
		logging.debug(f'Running command: {cmd}')
		p = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
		if p.returncode != 0:
			raise Volwalker2Error(f'Prolog exited with code {p.returncode}: {p.stderr}')

		for line in p.stdout.splitlines():
			uuid = line.strip()
			if not uuid:
				continue
			if uuid not in by_uuid:
				raise Volwalker2Error(f'Prolog returned unknown uuid {uuid!r}; stderr: {p.stderr}')
			x = by_uuid[uuid]
			if x.get('deleted'):
				logging.debug(f'candidate {uuid} is deleted, skipping')
				continue
			logging.debug(f'found candidate {x}')
			yield x
	finally:
		os.unlink(json_file)
