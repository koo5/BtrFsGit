import logging
import json



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
		logging.debug('subvols_by_local_uuid:')
		for k,v in subvols_by_local_uuid.items():
			logging.debug((k,v))
		logging.debug('/subvols_by_local_uuid')


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
			# the show almost stops here, but only almost. We could still look up all subvols that have this uuid as a parent/received uuid, and pursue those. It wouldn't be known if the missing subvol was ro or rw, so, these could be presented as only the last options to try for -p, or only as a -c.
			return


		if s.by_uuid[my_uuid]['machine'] == s.source:
			for _ in s.ro_descendants_chain(my_uuid, s.target):
				yield from s.ro_descendants_chain(my_uuid, s.source)
				break # we only care that a 'remote' snapshot exists, not how many there are

		p = s.parent(my_uuid)
		logging.debug('parent is ' + repr(p))
		if p:
			yield from s.walk(p)


	def ro_descendants_chain(s, my_uuid, machine):
		v = s.by_uuid.get(my_uuid)
		if not v:
			return

		# at any case, if the read-only-ness chain is broken,
		# the subvol or its descendants are of no use.
		# Or again, use as last-instance options?
		if v['ro'] == False:
			return
		else:
			# if this item of the chain happens to be on the remote machine,
			# the chain is a good candidate for -p
			if v['machine'] == machine:
				yield v

		# find all descendants created through send/receive or snapshotting
		for k,v in s.by_uuid.items():
			if v['received_uuid'] == my_uuid:
				yield from s.ro_descendants_chain(v['local_uuid'], machine)
			if v['parent_uuid'] == my_uuid:
				yield from s.ro_descendants_chain(v['local_uuid'], machine)



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

