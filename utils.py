
class Res:
	"""helper class for passing results of Fire-invoked functions around and make sure they're printed understandably and machine-readably"""
	def __init__(s, value):
		s.val = value
	def __repr__(s):
		return json.dumps({'result':s.val})
	def __str__(s):
		return json.dumps({'result':s.val})


def _prerr(*a):
	print(*a, file = sys.stderr)



class VolWalker:

	def __init__(s, subvols_by_local_uuid):


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
		yield from s.ro_descendants_chain(my_uuid)
		p = s.parent(my_uuid)
		logging.debug('parent is ' + repr(p))
		if p:
			yield from s.walk(p)


	def ro_descendants_chain(s, my_uuid):
		v = s.by_uuid.get(my_uuid)
		if not v:
			return

		# at any case, if the read-only-ness chain is broken,
		# the subvol or its descendants are of no use
		if v['ro'] == False:
			return

		# if this item of the chain happens to be on the remote machine,
		# it's a good candidate for -p
		if v['machine'] == 'remote':
			yield v

		# find all descendants created through send/receive or snapshotting
		for k,v in s.by_uuid.items():
			if v['received_uuid'] == my_uuid:
				yield from s.ro_descendants_chain(v['local_uuid'])
			if v['parent_uuid'] == my_uuid:
				yield from s.ro_descendants_chain(v['local_uuid'])



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

