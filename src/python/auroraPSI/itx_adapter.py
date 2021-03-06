import logging
from ast import literal_eval

import numpy as np
import pandas as pd
from auroraPSI.config.config import AuroraConfiguration
from auroraPSI.observations import Observations
from auroraPSI.validation import ValidationError
from auroraPSI.validation import require_not_empty


def split_wave_name(wave_name_string: str):
    if "/N" in wave_name_string:
        output_tuple = split_multidimensional_wave_name(wave_name_string)
    else:
        output_tuple = split_multiple_wave_name(wave_name_string)
    return output_tuple


def split_multidimensional_wave_name(wave_name_string: str):
    shape_string = wave_name_string.split("=")[1].split('\t')[0]
    shape_tuple = literal_eval(shape_string)
    wave_name = wave_name_string.split("=")[1].split('\t')[1]
    return wave_name, shape_tuple


def split_multiple_wave_name(wave_name_string: str):
    number_of_waves = len(wave_name_string.split('\t')) - 1
    shape_tuple = (None, number_of_waves)
    wave_name_tuple: tuple[str, ...] = tuple(wave_name_string.split('\t')[1:])
    if len(wave_name_tuple) == 1:
        wave_name_string = wave_name_tuple[0]
        return wave_name_string, shape_tuple
    else:
        return wave_name_tuple, shape_tuple


class ItxAdapter(Observations):

    def __init__(self, file_contents):
        """
        Reads the string with the contents of a IGOR Pro Format
        :param file_contents: string in IGOR PRO format
        """
        self.logger = logging.getLogger('auroraPSI')
        self._waves_in_line = None
        self._waves_names = None
        self._lines = None
        self._contents = None
        self._waves_shapes = None
        self._waves_positions = None
        self._read_file_contents(file_contents)
        self._config = AuroraConfiguration()

    def get_times(self) -> np.array:
        """
        returns the vector of timestamps in utc format
        :return:
        """
        time_wave = self.get_wave_data(self._config.observations["time"])
        utc_times = self.to_utc_time(time_wave)
        return utc_times

    def get_amus(self):
        return self.get_wave_data(self._config.observations["amus"])

    def get_data(self):
        return self.get_wave_data(self._config.observations["data"])

    def get_errors(self):
        return self.get_wave_data(self._config.observations["error"])

    def get_location(self):
        pass

    def get_features(self):
        pass

    def _read_file_contents(self, contents: str):
        """
        Reads a string (contents of an itx file) and  computes the number of lines
        :param contents: string in Igor Pro format
        """
        require_not_empty(contents)
        self._contents = contents.split('\n')
        if self._contents[-1] == '':
            self._contents = self._contents[:-1]
        self._lines = len(self._contents)
        self._waves_in_line = [(idx, line) for idx, line in enumerate(self._contents) if "WAVES" in line]
        try:
            # read the special start of wave line
            require_not_empty(self._waves_in_line)
        except ValidationError as val:
            self.logger.exception("There are no waves in the itx file")
            raise val

    def _read_waves(self):
        """
        This function will read the special start of wave lines
        It will store the wave shape in a dictionary and the wave starting position in another.
        :return:
        """
        # store the wave shapes in a dictionary with wave name as key
        self._waves_shapes: dict = dict([split_wave_name(wave) for idx, wave in self._waves_in_line])
        # store the wave starting position in a dictionary with wave name as key
        self._waves_positions = dict(zip(self.waves_shapes.keys(), [idx for idx, _ in self._waves_in_line]))
        # use BEGIN and END keywords to deduce the number of rows in the wave (when its not marked as N)
        for wave_name, wave_position in self._waves_positions.items():
            if self._waves_shapes[wave_name][0] is None:
                sublist = self._contents[(wave_position + 2):]
                offset = 0
                for idx, string in enumerate(sublist):
                    offset = idx
                    if "END" in string:
                        break
                self.waves_shapes[wave_name] = (offset, self._waves_shapes[wave_name][1])
        # add the wave names as a different property for completeness
        self._waves_names = list(self._waves_positions.keys())

    @property
    def lines(self) -> int:
        """
        The number lines on the original string
        :return: integer with the number of lines in the IGOR PRO file
        """
        return self._lines

    @property
    def contents(self) -> str:
        """
        The string in IGOR PRO format
        :return:
        """
        return self._contents

    @property
    def waves_shapes(self) -> dict[str, tuple]:
        if self._waves_shapes is None:
            self._read_waves()
        return self._waves_shapes

    @property
    def waves_positions(self) -> dict[str, int]:
        if self._waves_positions is None:
            self._read_waves()
        return self._waves_positions

    @property
    def waves_names(self) -> list[str]:
        if self._waves_names is None:
            self._read_waves()
        return self._waves_names

    def get_wave_data(self, wave_name: str) -> np.array:

        wave_position = self.waves_positions[wave_name]
        wave_shape = self.waves_shapes[wave_name]
        # we skip the BEGIN line
        wave_data_begin = wave_position + 2
        nrows = wave_shape[0]
        wave_data = np.loadtxt(self.contents[wave_data_begin: wave_data_begin + nrows])
        return wave_data

    @staticmethod
    def to_utc_time(time_wave: np.array):
        """
        Converts an array of integers (timestamps) to utc time, given that itx uses the mac epoch.
        :param time_wave: array of timestamps (np array)
        """
        mac_epoch = np.datetime64('1904-01-01')
        time_wave_delta = np.vectorize(lambda x: np.timedelta64(x, 's'))(time_wave.astype(int))
        return time_wave_delta + mac_epoch

    def to_pandas(self) -> dict[str, pd.DataFrame]:
        data_columns = [str(amu) for amu in self.get_amus()]
        data_df = pd.DataFrame(columns=data_columns,
                               data=self.get_data(), index=self.get_times())

        error_columns = [column + "_err" for column in data_columns]
        error_df = pd.DataFrame(columns=error_columns,
                                data=self.get_errors(),
                                index=self.get_times())
        return {"data": data_df, "errors": error_df}

    @classmethod
    def read_file(cls, file_path: str):
        with open(file_path, "r") as file:
            return cls(file.read())
