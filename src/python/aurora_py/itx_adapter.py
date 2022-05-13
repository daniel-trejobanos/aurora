from src.python.aurora_py.observations import Observations
from ast import literal_eval
from src.python.aurora_py.config.py import Configuration

def split_wave_name(wave_name_string: str):
    output_tuple = (None, None)
    if "/N" in wave_name_string:
        output_tuple = split_multidimensional_wave_name(wave_name_string)
    else:
        output_tuple = split_multiple_wave_name(wave_name_string)
    return output_tuple

def split_multidimensional_wave_name(wave_name_string: str):
    shape_string = wave_name_string.split("=")[1].split('\t')[0]
    shape_tuple = literal_eval(shape_string)
    wave_name = wave_name_string.split("=")[1].split('\t')[1]
    return (wave_name, shape_tuple)

def split_multiple_wave_name(wave_name_string:str):
    number_of_waves = len(wave_name_string.split('\t')) - 1
    shape_tuple = (None, number_of_waves)
    wave_name_tuple = tuple(wave_name_string.split('\t')[1:])
    if len(wave_name_tuple) == 1:
        wave_name_tuple = wave_name_tuple[0]
    return (wave_name_tuple, shape_tuple)


class ItxAdapter(Observations):

    def __init__(self, file_contents):
        """
        Reads the string with the contents of a IGOR Pro Format
        :param file_contents: string in IGOR PRO format
        """
        self._waves_names = None
        self._lines = None
        self._contents = None
        self._waves_shapes = None
        self._waves_positions = None
        self._read_file_contents(file_contents)
        self._config = Configuration()

    def get_times(self):
        pass

    def get_amus(self):
        pass

    def get_data(self):
        pass

    def get_location(self):
        pass

    def get_features(self):
        pass

    def _read_file_contents(self, contents: str):
        """
        Reads a string (contents of an itx file) and  computes the number of lines
        :param contents: string in Igor Pro format
        """
        self._contents = contents.split('\n')
        if self._contents[-1] == '':
            self._contents = self._contents[:-1]
        self._lines = len(self._contents)

    def _read_waves(self):
        """
        This function will read the special start of wave lines
        It will store the wave shape in a dictionary and the wave starting position in another.
        :return:
        """
        # read the special start of wave line
        wave_strings = [(idx, line) for idx,line in enumerate(self._contents) if "WAVES" in line]
        # store the wave shapes in a dictionary with wave name as key
        self._waves_shapes:dict = dict([split_wave_name(wave) for idx, wave in wave_strings ])
        # store the wave starting position in a dictionary with wave name as key
        self._waves_positions = dict(zip(self.waves_shapes.keys(), [idx for idx,_ in wave_strings]))
        # use BEGIN and END keywords to deduce the number of rows in the wave (when its not marked as N)
        for wave_name, wave_position in self._waves_positions.items():
            if self._waves_shapes[wave_name][0] is None:
                sublist = self._contents[(wave_position + 2):]
                offset = 0
                for idx, string in enumerate(sublist):
                    offset=idx
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

    def get_wave_data(self, wave_name:str) -> np.array:

        wave_position = self.waves_positions[wave_name]
        wave_shape = self.waves_shapes[wave_name]
        # we skip the BEGIN line
        wave_data_begin = wave_position + 2
        nrows = wave_shape[0]
        wave_data = np.loadtxt(self.contents[wave_data_begin: wave_data_begin + nrows])
        return wave_data

    def wave_data(self, wave_name):
        pass
