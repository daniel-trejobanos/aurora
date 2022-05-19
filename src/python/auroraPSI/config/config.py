from json import load

import pkg_resources


class AuroraConfiguration:

    def __init__(self):
        self._config_path = pkg_resources.resource_filename('auroraPSI', 'config/config.json')
        with open(self._config_path, "r") as config_file:
            self._config_dictionary = load(config_file)

    @property
    def observations(self):
        return self._config_dictionary['observations']
