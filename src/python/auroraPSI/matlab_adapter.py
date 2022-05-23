import logging
from auroraPSI.config.config import AuroraConfiguration
from auroraPSI.sources import Sources



class MATAdapter(Sources):

    def __init__(self, contents_dictionary):
        """
        Reads the string with the contents of a IGOR Pro Format
        :param file_contents: string in IGOR PRO format
        """
        self.logger = logging.getLogger('auroraPSI')
        self._contents_dict = contents_dictionary
        self._config = AuroraConfiguration()
        self._location = None
        self._sources = None
