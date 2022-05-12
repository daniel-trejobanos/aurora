from src.python.aurora_py.observations import Observations
from ast import literal_eval


def _split_wave_name(wave_name_string: str):
    dim_and_name = wave_name_string.split("=")[1]
    wave_dim, wave_name = dim_and_name.split("\t")
    return (wave_name, literal_eval(wave_dim)  )


class ItxAdapter(Observations):

    def __init__(self, file_contents):
        self._waves_names = None
        self._lines = None
        self._contents = None
        self._waves_shapes = None
        self._waves_positions = None
        self._read_file_contents(file_contents)

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
        self._contents = contents.split('\r')
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
        wave_strings = [(idx, line) for idx,line in enumerate(self._contents) if "WAVES/N=" in line]
        # store the wave shapes in a dictionary with wave name as key
        self._waves_shapes:dict = dict([_split_wave_name(wave) for idx, wave in wave_strings])
        # store the wave starting position in a dictionary with wave name as key
        self._waves_positions = dict(zip(self.waves_shapes.keys(), [idx for idx,_ in wave_strings]))
        # add the wave names as a different property for completeness
        self._waves_names = list(self._waves_positions.keys())

    @property
    def lines(self) -> int:
        return self._lines

    @property
    def contents(self) -> str:
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


    def wave_data(self, wave_name):
        pass
