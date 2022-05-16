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
        second_line = 'WAVES/D\tacsm_utc_time\n'
        third_line = 'BEGIN\n'
        data_lines = '\t3598270200.0\n' \
                     '\t3598270201.0\n' \
                     '\t3598270202.0\n'
        end_line = 'END\n'
        second_wave = second_line + third_line + data_lines + end_line
        self._contents = first_wave + second_wave
        self._lines = self._contents.count('\n')
        first_wave_data = np.array([[0.38642111, 0.16979307],
                         [0.38642111, 0.16979307],
                         [0.38642111, 0.16979307]])
        second_wave_data = np.array([3598270200.0, 3598270201.0, 3598270202.0])
        self._wave_data = {'Org_Specs': first_wave_data, 'acsm_utc_time':second_wave_data}

    @property
    def contents(self):
        return self._contents

    @property
    def lines(self):
        return self._lines

    @property
    def waves_shapes(self):
        return {'Org_Specs': (3, 2), 'acsm_utc_time':(3,1) }

    @property
    def waves_positions(self):
        return {'Org_Specs': 1, 'acsm_utc_time': 7 }

    @property
    def waves_names(self):
        return ['Org_Specs', 'acsm_utc_time']

    @property
    def wave_data(self):
        return self._wave_data

    @property
    def times(self):
        return np.array(['2018-01-08T15:30:00', '2018-01-08T15:30:01',
               '2018-01-08T15:30:02'], dtype='datetime64[s]')


@pytest.fixture()
def mock_itx_file():
    return MockItxFile()
