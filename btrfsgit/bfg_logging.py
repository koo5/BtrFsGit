import logging
import logging.config
import os
import argparse

def configure_logging():
	log_config_file = os.path.join(os.path.dirname(__file__), 'logging.conf')
	logging.config.fileConfig(log_config_file)
	overrides = parse_logging_overrides()
	update_logging_levels(overrides)


def parse_logging_overrides():
	overrides = {}
	for key, value in os.environ.items():
		if key.startswith('BFG_LOGGING_'):
			logger_name = key[len('BFG_LOGGING_'):].lower()
			level = getattr(logging, value.upper(), None)
			if level is not None:
				overrides[logger_name] = level
	#print(overrides)
	return overrides


def update_logging_levels(overrides):
	for logger_name, level in overrides.items():
		logging.getLogger(logger_name).setLevel(level)


