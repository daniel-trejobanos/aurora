from src.python.aurora_py.observations import Observations
from ast import literal_eval


def _split_wave_name(wave_name_string: str):
    dim_and_name = wave_name_string.split("=")[1]
    wave_dim, wave_name = dim_and_name.split("\t")
    return (wave_name, literal_eval(wave_dim)  )


class ItxAdapter(Observations):

    def __init__(self, file_contents):
        self._lines = None
        self._contents = None
        self._waves_shapes = None
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
        wave_strings = [(idx, line) for idx,line in enumerate(self._contents) if "WAVES/N=" in line]
        self._waves_shapes = dict([_split_wave_name(wave) for idx, wave in wave_strings])

    @property
    def lines(self) -> int:
        return self._lines

    @property
    def contents(self) -> str:
        return self._contents

    @property
    def waves_shapes(self) -> list[dict[tuple, str]]:
        if self._waves_shapes is None:
            self._read_waves()
        return self._waves_shapes

    def wave_data(self, wave_name):
        pass
