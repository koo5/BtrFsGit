
	def _filter_out_wrong_parents(s, snapshot, parents):
		"""filter out parents that aren't usable for snapshot"""
		parents2 = parents[:]
		counter = 0
		for p in parents:
			cmd = s._sudo + ['btrfs', 'send', '-c', p, snapshot]
			print(shlex.join(cmd) + ' ...')
			proc = subprocess.Popen(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)

			if s._read_first_bytes(proc.stdout) != 0:
				proc.kill()
				continue

			stderr = b''
			while True:
				r = proc.stderr.read(100000)
				stderr += r
				if len(r) == 0:
					break

			proc.kill()

			print(str(stderr))
			print('ok..')

			if b'\nERROR: parent determination failed for ' in stderr:
				parents2.remove(p)
				counter += 1
			else:
				print(stderr)


		_prerr(f'filtered out {counter} unsuitable parents')
		return parents2




	def _read_first_bytes(s, stdout):
		result = b''
		while True:
			o = stdout.read(10)
			result += o
			if len(o) == 0:
				break
			if len(result) > 10:
				break
		return len(result)

