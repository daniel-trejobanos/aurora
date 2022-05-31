import logging
from pathlib import Path

import pandas as pd

from auroraPSI.config.config import AuroraConfiguration
from auroraPSI.sources import Sources
from re import split
import numpy as np

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
        return self.data_frame.iloc[:, 0]

    @property
    def locations(self):
        return self.data_frame.location

    @property
    def variables(self):
        return self.data_frame.columns

    @property
    def data(self):
        return self._data_frame

    def save(self, path:Path):
        self.data_frame.to_feather(path)


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
        headers = str(contents_dictionary[headers_name])
        self._read_heading(headers)
        self._read_data()
        self._read_location()
        self._data_frame = None

    def _read_heading(self, header):
        # TODO make the splitheadings configurable
        var_header, rest = header.split("Var:")
        variables, rest = rest.split("Units:")
        self._variables = split_variables_heading(variables)
        units, stations = rest.split("Stations (Cells dimention):")
        stations, rest = stations.split("\\n")
        self._stations_list = split_variables_heading(stations)

    def _read_data(self):
        self._data = self._contents_dict[self._config.sources["PMFdata"]][0,:].tolist()

    def _read_location(self):
        self._longitude_list = self._contents_dict[self._config.sources["lon_site"]][0].tolist()
        self._latitude_list = self._contents_dict[self._config.sources["lat_site"]][0].tolist()
        self._station_type_list = self._contents_dict[self._config.sources["site_type"]][0].tolist()

    def to_pandas(self):
        self._zipped_locations = zip(
            self._data,
            self._stations_list,
            self._longitude_list,
            self._latitude_list,
            self._station_type_list
        )
        all_locations_list = []
        for variable_data, station, lon, lat, station_type in list(self._zipped_locations):
            print(station)
            location_df = pd.DataFrame(data=variable_data, columns= self._variables)
            location_df['station'] = station
            location_df['lon'] = lon
            location_df['lat'] = lat
            location_df['type'] = np.repeat(station_type, variable_data.shape[0])
            all_locations_list.append(location_df)


        concatenated_df =  pd.concat(all_locations_list,axis=0)
        time_column = [column for column in concatenated_df.columns if "Time" in column]
        rename_map={str(*time_column): "time"}
        concatenated_df.rename(rename_map, inplace=True)
        return concatenated_df

    @property
    def data_frame(self) -> pd.DataFrame:
        if self._data_frame is None:
            self._data_frame = self.to_pandas()
        return self.to_pandas()
