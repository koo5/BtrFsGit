import logging
from typing import List
from typing import Optional
from sqlalchemy import ForeignKey
from sqlalchemy import String
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
	fs_uuid: Mapped[str] = mapped_column(nullable=False)
	local_uuid: Mapped[str] = mapped_column(primary_key=True)
	parent_uuid: Mapped[Optional[str]] = mapped_column()
	received_uuid: Mapped[Optional[str]] = mapped_column()
	host: Mapped[str] = mapped_column(nullable=False)
	fs: Mapped[str] = mapped_column(nullable=False)
	path: Mapped[str] = mapped_column(nullable=False)
	deleted: Mapped[bool] = mapped_column(default=False, nullable=False)




def get_engine():

	host = 'hours.internal'
	db = 'bfg'
	table = 'snapshots'
	user = 'bfg'
	password = 'bfg'

	#return psycopg2.connect(f"dbname={db} user={user} host={host} password={password}")

	conn_str = f"postgresql+psycopg2://{user}:{password}@{host}/{db}" # postgresql://bfg:bfg@hours.internal/bfg
	l = logging.getLogger('sqlalchemy').getEffectiveLevel()
	engine = create_engine(conn_str, echo=(l < 20))
	Base.metadata.create_all(engine)
	return engine

engine = get_engine()


def session():
	session = Session(engine)
	return session


