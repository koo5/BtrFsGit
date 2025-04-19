import logging, sys, os
import json, subprocess


def common_parents(s, by_uuid, my_uuid, target_fs):
	source_fs = by_uuid[my_uuid]['fs']
	logging.debug(f'find common parent for transferring {my_uuid=} from {source_fs=} to {target_fs=}')

	if my_uuid not in by_uuid:
		logging.warn('my_uuid not in by_uuid')
		sys.exit(1)

	for i in by_uuid:
		by_uuid[i]['path'] = str(by_uuid[i]['path'])

	candidates = []

	subvols_json = json.dumps(by_uuid.values())

	# spawn prolog and yield results as they come in
	p = subprocess.Popen(['swipl' , '-s', os.basename(__file__) + '/volwalker2.pl', '-g', 'find_common_parents(' + subvols_json + ', "' + my_uuid + '", "' + source_fs + '", "' + target_fs + '")', '-t', 'halt'], stdout=subprocess.PIPE, text=True, bufsize=1)
	with p.stdout:
		for line in iter(p.stdout.readline, b''):
			line = line.strip()
			uuid = line
			if uuid not in by_uuid:
				logging.error(f'uuid {uuid} not in by_uuid')
				sys.exit(1)
			x = by_uuid[uuid]
			if x['deleted']:
				logging.debug(f'snapshot {uuid} is deleted')
				continue
			candidates.append(x)
			logging.debug(f'found candidate {x}')

	# sort by subvol id, just in case prolog does not return them in the best order
	candidates.sort(key=lambda x: x['subvol_id'])
	for x in candidates.items():
		yield x

