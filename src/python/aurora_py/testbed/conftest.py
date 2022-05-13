import numpy as np
import pytest


class MockItxFile:
    def __init__(self):
        first_line = 'IGOR\n'
        second_line = 'WAVES/N=(3,2)\tOrg_Specs\n'
        third_line = 'BEGIN\n'
        data_lines = '\t0.38642111\t0.16979307\n' \
                     '\t0.38642111\t0.16979307\n' \
                     '\t0.38642111\t0.16979307\n'
        end_line = 'END\n'
        first_wave = first_line + second_line + third_line + data_lines + end_line
        second_line = 'WAVES/D\tutc_time\n'
        third_line = 'BEGIN\n'
        data_lines = '\t0.38642111\n' \
                     '\t0.38642111\n' \
                     '\t0.38642111\n'
        end_line = 'END\n'
        second_wave = second_line + third_line + data_lines + end_line
        self._contents = first_wave + second_wave
        self._lines = self._contents.count('\n')
        first_wave_data = np.array([[0.38642111, 0.16979307],
                         [0.38642111, 0.16979307],
                         [0.38642111, 0.16979307]])
        second_wave_data = np.array([0.38642111,0.38642111,0.38642111])
        self._wave_data = {'Org_Specs': first_wave_data, 'utc_time':second_wave_data}

    @property
    def contents(self):
        return self._contents

    @property
    def lines(self):
        return self._lines

    @property
    def waves_shapes(self):
        return {'Org_Specs': (3, 2), 'utc_time':(3,1) }

    @property
    def waves_positions(self):
        return {'Org_Specs': 1, 'utc_time': 7 }

    @property
    def waves_names(self):
        return ['Org_Specs', 'utc_time']

    @property
    def wave_data(self):
        return self._wave_data


@pytest.fixture()
def mock_itx_file():
    return MockItxFile()
