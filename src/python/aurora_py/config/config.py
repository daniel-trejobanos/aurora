from json import load
import os


class AuroraConfiguration:

    def __init__(self):
        current_dir = str(os.path.dirname(__file__))
        self._config_path = current_dir + "/config.json"
        with open(self._config_path, "r") as config_file:
            self._config_dictionary = load(config_file)

    @property
    def observations(self):
        return self._config_dictionary['observations']
