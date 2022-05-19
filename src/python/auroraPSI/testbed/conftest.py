import numpy as np
import pandas as pd
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
        second_line = 'WAVES/D\tamus\n'
        third_line = 'BEGIN\n'
        data_lines = '\t12.0\n' \
                     '\t14.0\n'
        end_line = 'END\n'
        third_wave = second_line + third_line + data_lines + end_line
        self._contents = first_wave + second_wave + third_wave
        self._lines = self._contents.count('\n')
        first_wave_data = np.array([[0.38642111, 0.16979307],
                                    [0.38642111, 0.16979307],
                                    [0.38642111, 0.16979307]])
        second_wave_data = np.array([3598270200.0, 3598270201.0, 3598270202.0])
        third_wave_data = np.array([12.0, 14.0])
        self._wave_data:dict[str, np.array] = {'Org_Specs': first_wave_data,
                                               'acsm_utc_time': second_wave_data,
                                               "amus": third_wave_data}

    @property
    def contents(self):
        return self._contents

    @property
    def lines(self):
        return self._lines

    @property
    def waves_shapes(self):
        return {'Org_Specs': (3, 2), 'acsm_utc_time': (3, 1), 'amus': (2, 1)}

    @property
    def waves_positions(self):
        return {'Org_Specs': 1, 'acsm_utc_time': 7, 'amus': 13}

    @property
    def waves_names(self):
        return ['Org_Specs', 'acsm_utc_time', 'amus']

    @property
    def wave_data(self) -> dict[str, np.array]:
        return self._wave_data

    @property
    def times(self):
        return np.array(['2018-01-08T15:30:00', '2018-01-08T15:30:01',
                         '2018-01-08T15:30:02'], dtype='datetime64[s]')

    @property
    def amus(self):
        return self._wave_data['amus']

    @property
    def data(self):
        return self._wave_data['Org_Specs']

    @property
    def df(self):
        data_df = pd.DataFrame(data=self.data,
                            columns=self.amus,
                            index=self.times)

@pytest.fixture()
def mock_itx_file():
    return MockItxFile()
