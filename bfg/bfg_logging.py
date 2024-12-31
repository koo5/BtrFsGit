import logging, os


def configure_logging_from_env():
	log_level_str = os.getenv("BFG_LOG_LEVEL", "WARNING")
	numeric_level = getattr(logging, log_level_str.upper())
	logging.basicConfig(level=numeric_level)

