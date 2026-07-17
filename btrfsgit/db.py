import logging
from contextlib import contextmanager
from typing import List
from typing import Optional
from sqlalchemy import ForeignKey
from sqlalchemy import String, text
from sqlalchemy.orm import DeclarativeBase
from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy.orm import relationship
from sqlalchemy import create_engine
from sqlalchemy.orm import Session


class Base(DeclarativeBase):
	pass


class Snapshot(Base):
	__tablename__ = "snapshots"
	id: Mapped[str] = mapped_column(nullable=False)
	fs_uuid: Mapped[str] = mapped_column(nullable=False)
	local_uuid: Mapped[str] = mapped_column(primary_key=True)
	parent_uuid: Mapped[Optional[str]] = mapped_column()
	received_uuid: Mapped[Optional[str]] = mapped_column()
	host: Mapped[str] = mapped_column(nullable=False)
	fs: Mapped[str] = mapped_column(nullable=False)
	path: Mapped[str] = mapped_column(nullable=False)
	deleted: Mapped[bool] = mapped_column(default=False, nullable=False)
	subvol_id: Mapped[Optional[int]] = mapped_column(nullable=False)
	ro: Mapped[bool] = mapped_column(nullable=False)




_engine = None

def get_engine():
	global _engine
	if _engine:
		return _engine

	host = 'hours.internal'
	db = 'bfg'
	table = 'snapshots'
	user = 'bfg'
	password = 'bfg'

	#return psycopg2.connect(f"dbname={db} user={user} host={host} password={password}")

	conn_str = f"postgresql+psycopg2://{user}:{password}@{host}/{db}" # postgresql://bfg:bfg@hours.internal/bfg
	logging.getLogger().debug(f"Connecting to {conn_str}")
	l = logging.getLogger('sqlalchemy').getEffectiveLevel()
	_engine = create_engine(conn_str, echo=(l < 20))
	Base.metadata.create_all(_engine)
	logging.getLogger().debug(f"Connected.")
	return _engine



def session():
	session = Session(get_engine())
	return session


def mark_deleted(local_uuids):
	"""
	Flag snapshots as deleted in the db, by local_uuid. Called right after subvolumes
	are actually deleted, so that other machines computing shared parents between two
	update_db runs don't base their decisions on phantom rows.
	"""
	if not local_uuids:
		return
	s = session()
	with s.begin():
		s.query(Snapshot).filter(Snapshot.local_uuid.in_(list(local_uuids))).update(
			{'deleted': True}, synchronize_session=False)


BFG_LOCK_KEY = 1

@contextmanager
def advisory_lock():
	"""Acquire a PostgreSQL advisory lock to serialize update_db / prune operations."""
	log = logging.getLogger('bfg')
	conn = get_engine().connect()
	try:
		log.info("Acquiring advisory lock...")
		conn.execute(text("SELECT pg_advisory_lock(:key)"), {"key": BFG_LOCK_KEY})
		log.debug("Advisory lock acquired.")
		yield
	finally:
		conn.execute(text("SELECT pg_advisory_unlock(:key)"), {"key": BFG_LOCK_KEY})
		log.debug("Advisory lock released.")
		conn.close()


