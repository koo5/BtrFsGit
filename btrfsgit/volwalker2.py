import logging, sys, os
import json, subprocess
from datetime import datetime
from pathlib import Path


def common_parents(by_uuid, my_uuid, target_fs):
	"""
	by_uuid: dict of subvolumes by uuid
	my_uuid: uuid of the subvolume we are transferring
	target_fs: target filesystem uuid
	"""
	source_fs = by_uuid[my_uuid]['fs_uuid']
	logging.debug(f'find common parent for transferring {my_uuid=} from {source_fs=} to {target_fs=}')

	if my_uuid not in by_uuid:
		logging.warn('my_uuid not in by_uuid')
		sys.exit(1)

	for i in by_uuid:
		by_uuid[i]['path'] = str(by_uuid[i]['path'])

	candidates = []

	# Custom JSON encoder for datetime and Path objects
	def json_encoder(obj):
		if isinstance(obj, datetime):
			return obj.isoformat()
		elif isinstance(obj, Path):
			return str(obj)
		raise TypeError(f"Object of type {type(obj)} is not JSON serializable")
	
	subvols_json = json.dumps(list(by_uuid.values()), default=json_encoder)

	# spawn prolog and yield results as they come in
	script_path = os.path.join(os.path.dirname(__file__), 'volwalker2.pl')
	
	# Write JSON to a temporary file to avoid command line length limits
	import tempfile
	with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
		json_file = f.name
		f.write(subvols_json)
	
	try:
		# Debug: write JSON content to see what we're passing
		logging.debug(f'Writing JSON to {json_file} for Prolog query')
		logging.debug(f'Query: find_common_parents_from_file("{json_file}", "{my_uuid}", "{source_fs}", "{target_fs}")')
		# Also save a copy for debugging
		import shutil
		debug_file = '/tmp/bfg_debug_prolog.json'
		shutil.copy(json_file, debug_file)
		logging.debug(f'JSON data also saved to {debug_file} for debugging')
		
		# Pass filename instead of JSON data
		cmd = ['swipl' , '-s', script_path, '-g', 
			f'find_common_parents_from_file("{json_file}", "{my_uuid}", "{source_fs}", "{target_fs}")', 
			'-t', 'halt']
		logging.debug(f'Running command: {cmd}')
		p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, bufsize=1)
		# Set a timeout to prevent hanging
		import signal
		
		def timeout_handler(signum, frame):
			p.kill()
			raise TimeoutError("Prolog query timed out")
		
		# Set 30 second timeout
		signal.signal(signal.SIGALRM, timeout_handler)
		signal.alarm(30)
		
		try:
			with p.stdout:
				for line in iter(p.stdout.readline, b''):
					line = line.strip()
					if not line:  # Skip empty lines
						continue
					uuid = line
					if uuid not in by_uuid:
						logging.error(f'uuid {uuid} not in by_uuid')
						# Check stderr for Prolog errors
						stderr_output = p.stderr.read()
						if stderr_output:
							logging.error(f'Prolog error: {stderr_output}')
						sys.exit(1)
					x = by_uuid[uuid]
					if x['deleted']:
						logging.debug(f'snapshot {uuid} is deleted')
						continue
					candidates.append(x)
					logging.debug(f'found candidate {x}')
			
			# Wait for process to complete
			p.wait()
			
			# Check for errors
			if p.returncode != 0:
				stderr_output = p.stderr.read()
				logging.error(f'Prolog exited with code {p.returncode}: {stderr_output}')
		finally:
			# Cancel the alarm
			signal.alarm(0)

		# sort by subvol id, just in case prolog does not return them in the best order
		candidates.sort(key=lambda x: x['subvol_id'])
		for x in candidates:
			yield x
	finally:
		# Clean up temporary file
		os.unlink(json_file)

