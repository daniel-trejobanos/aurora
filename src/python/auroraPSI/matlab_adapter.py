import logging
from ast import literal_eval

import numpy as np
import pandas as pd
from auroraPSI.config.config import AuroraConfiguration
from auroraPSI.sources import Sources
from auroraPSI.validation import ValidationError
from auroraPSI.validation import require_not_empty



class MATAdapter(Sources):

    def __init__(self, contents_dictionary):
        """
        Reads the string with the contents of a IGOR Pro Format
        :param file_contents: string in IGOR PRO format
        """
        self.logger = logging.getLogger('auroraPSI')
        self._contents_dict = contents_dictionary
        self._config = AuroraConfiguration()
        self._latitudes = None
        self._longitudes = None
