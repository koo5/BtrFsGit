import logging
import json



class VolWalker:
	""" walks subvolume records to find common parents
	"""

	def __init__(s, subvols_by_local_uuid, direction):

		s.source = direction[0]
		s.target = direction[1]
		#logging.debug('subvols_by_local_uuid:')
		#for k,v in subvols_by_local_uuid.items():
		#	logging.debug((k,v))
		#logging.debug('/subvols_by_local_uuid')
		s.by_uuid = subvols_by_local_uuid

	def parent(s, uuid):
		v = s.by_uuid[uuid]
		logging.debug(v)
		if v['received_uuid']:
			return v['received_uuid']
		if v['parent_uuid']:
			return v['parent_uuid']


	def walk(s, my_uuid):
		logging.debug('walk ' + repr(my_uuid))

		if my_uuid not in s.by_uuid:
			# the show almost stops here, but only almost. We could still look up all subvols that have this uuid as a parent/received uuid, and pursue those. It wouldn't be known if the missing subvol was ro or rw, so, these could be presented as only the last case options to try.
			logging.info('my_uuid not in s.by_uuid')
			return #fixme

		for i in s.ro_chain(my_uuid):
			# ^ grab my_uuid if it's ro, otherwise grab it's ro children, and then their children recursively. What this accomplishes is that we'll be looking at a direct ro snapshot (or at the ro subvol itself), so that we can now check if it made it to the other side:

			logging.debug(f'i: {i}.')

			for remote_snapshot in s.ro_descendants_chain(i, s.target):
				remote_snapshot_local_uuid = remote_snapshot['local_uuid']
				logging.debug(f'{remote_snapshot_local_uuid} is on remote.')
				for x in s.ro_descendants_chain(i, s.source):
					x_local_uuid = x['local_uuid']
					logging.debug(f'local counterpart: {x_local_uuid}.')
					yield x
				# we only care that a 'remote' snapshot exists, not how many there are:
				break

		p = s.parent(my_uuid)
		logging.debug('parent is ' + repr(p))
		if p:
			yield from s.walk(p)


	def ro_chain(s, uuid):
		v = s.by_uuid.get(uuid)
		if not v:
			return
		if v['ro']:
			yield uuid
		for k,v in s.by_uuid.items():
			if v['received_uuid'] == uuid:
				yield from s.ro_chain2(v['local_uuid'])
			if v['parent_uuid'] == uuid:
				yield from s.ro_chain2(v['local_uuid'])


	def ro_chain2(s, uuid):
		v = s.by_uuid.get(uuid)
		if not v:
			return
		if not v['ro']:
			return
		yield from s.ro_chain(uuid)


	def ro_descendants_chain0(s, my_uuid, machine):
		# find all descendants created through send/receive or snapshotting
		for k,v in s.by_uuid.items():
			if v['received_uuid'] == my_uuid:
				yield from s.ro_descendants_chain(v['local_uuid'], machine)
			if v['parent_uuid'] == my_uuid:
				yield from s.ro_descendants_chain(v['local_uuid'], machine)


	def ro_descendants_chain(s, my_uuid, machine):
		v = s.by_uuid.get(my_uuid)
		if not v:
			return

		# at any case, if the read-only-ness chain is broken,
		# the subvol or its descendants are of no use.
		# idk how btrfs looks at this, maybe if there were actually no modifications, it'd keep a constant gen id, and a ro child snapshot of it could be used? # nope, see tests/negative/test2.sh

		if v['ro'] == False:
			return
		else:
			# if this item of the chain happens to be on the remote machine,
			# the chain is a good candidate for -p
			if v['machine'] == machine:
				yield v

		yield from s.ro_descendants_chain0(my_uuid, machine)




