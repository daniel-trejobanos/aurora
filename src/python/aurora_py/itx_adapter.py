from src.python.aurora_py.observations import Observations
from ast import literal_eval

class ItxAdapter(Observations):

    def __init__(self, file_contents):
        self._lines = None
        self._contents = None
        self._waves = None
        self._read_file_contents(file_contents)

    def get_times(self):
        pass

    def get_amus(self):
        pass

    def get_data(self):
        self._read_waves(self)

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
        wave_strings = [line for line in self._contents if "WAVES/N=" in line ]
        self._waves = [self._split_wave_name(wave) for wave in wave_strings]

    def _split_wave_name(self, wave_name_string:str):
        dim_and_name = wave_name_string.split("=")[1]
        wave_dim, wave_name = dim_and_name.split("\t")
        return literal_eval(wave_dim), wave_name

    @property
    def lines(self) -> int:
        return self._lines

    @property
    def contents(self) -> str:
        return self._contents

    @property