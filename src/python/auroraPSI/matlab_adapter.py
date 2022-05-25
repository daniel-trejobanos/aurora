import logging
from auroraPSI.config.config import AuroraConfiguration
from auroraPSI.sources import Sources
from re import split


def split_variables_heading(variable_heading):
    split_list = split("0*\d+-", variable_heading)
    return_list = [variable for variable in split_list if len(variable) > 0]
    return_list = [variable.strip("\\n") for variable in return_list]
    return_list = [variable.strip() for variable in return_list ]
    return_list = [variable.strip(",")  for variable in return_list]
    return_list = [variable for variable in return_list if len(variable) > 0]
    return return_list


class MATAdapter(Sources):

    @property
    def time(self):
        pass

    @property
    def location(self):
        pass

    @property
    def variables(self):
        return self._variables

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
        self._variables = None
        headers_name = self._config.sources["header"]
        headers = contents_dictionary[headers_name]
        self._read_heading(headers)

    def _read_heading(self, header):
        # TODO make the splitheadings configurable
        var_header, rest = header.split("Var:")
        variables, rest = rest.split("Units:")
        self._variables = split_variables_heading(variables)
        units, stations = rest.split("Stations (Cells dimention):")
        stations, rest = stations.split("\\n")
        self._stations = split_variables_heading(stations)
        split_string = header.split(",")



