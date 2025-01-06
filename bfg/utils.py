import logging
import json


from bfg.bfg_logging import configure_logging_from_env
configure_logging_from_env()


class Res:
	"""helper class for passing results of Fire-invoked functions around and make sure they're printed understandably and machine-readably"""
	def __init__(s, value):
		s.val = value
	def __repr__(s):
		return json.dumps({'result':s.val})
	def __str__(s):
		return json.dumps({'result':s.val})





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
			logging.debug('my_uuid not in s.by_uuid')
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




def create_subvol_dump():
	"""
	I feel that it makes more sense to store the dumps in their raw form, that is, the outputs of `btrfs sub list`.
	This way, we can diff, review, delete lines etc easily.
	This is gonna be a bit silly in that we need to store the outputs of `sub list` and `sub list -r` separately, because `sub list` has no option to just print the read-only status in a column or something.
	dumps should probably be organized first by fs uuid and second by timestamp

	:return: subvol dump path
	"""
	pass

def load_subvol_dumps():
	"""
	todo.
	only the most recent dump for each fs will be used.

	"""
	return []





def is_most_recent_common_snapshot(path):
	"""
	this whole check is skipped if --disregard-db is given.
	otherwise, pruning is skipped if we return true here.
	additional, or different, middle-ground approach, could be to store a snapshot database locally, but such database should allow concurrent operations, so, at least sqlite, a plain file would not be suitable.
	---
	identify source and destination filesystems by their uuids.
	find the most recent common snapshot of subvol between two filesystems by consulting the database.
	---
	the database would ideally first be updated to reflect the current state of the filesystems.
	this is easy locally, but remote hosts can be offline.
	we could run remote commands, but we'll just rely on each host updating the db by itself (for example after pruning)
	---






	"""
	pass

