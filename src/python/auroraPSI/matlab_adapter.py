import logging
from auroraPSI.config.config import AuroraConfiguration
from auroraPSI.sources import Sources
from re import split


def split_variables_heading(variable_heading):
    split_list = split("0*\d+-", variable_heading)
    return_list = [variable for variable in split_list if len(variable) > 0]
    return_list = [variable.strip() for variable in return_list ]
    return_list = [variable.strip(",")  for variable in return_list]
    return return_list


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
