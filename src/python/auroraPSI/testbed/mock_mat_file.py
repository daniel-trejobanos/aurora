import numpy as np

class MockMatlabFile:

    def __init__(self):
        self._header = 'Var: 1-Time (MATLAB, UTC), 02-NO3, 03-SO4, 04-NH4, 05-CL, 06-BC,  07-OAtot,  08-OOAtot, ' \
                       '09-NA, 10-FPRM(fine primary PM)11-LV-OOA (MO-OOA), 12-SV-OOA (LO-OOA), 13-BBOA, 14-COA, ' \
                       '15-HOA, 16-SFOA, 17-CCOA, 18-58-OA, 19-Coffee-OA, 20-ShipIndustry-OA, 21-SeasaltOA, ' \
                       '22-CSOA \\n23-O3, 24-SO2, 25-NO, 26-NO2, 27-NOx, 28-CO, 29-NH3, 30-HNO3, 31-HONO, 32-MONOTERP ' \
                       '33-ISOP 34-Temperature [oC], 35-RH [%], 36-Press [hPa], 37-WS [m/s], 38-WD [deg.],  ' \
                       '39-PBLH [m]\\nUnits: PM [ug/m3], gas [ppbv]\\n9 Stations (Cells dimention): 1-Bologna, ' \
                       '2-Finokalia, 3-Mace Head, 4-Marseille, 5-Montsec, 6-Paris, 7-San Pietro Capofiume, 8-Zurich, ' \
                       '9-SMEARII\\nTime (UTC, Matlab time) '

        self._header_list_station_name = [
            '1-Bologna',
            '2-Finokalia',
            '3-Mace Head',
            '4-Marseille',
            '5-Montsec',
            '6-Paris',
            '7-San Pietro Capofiume',
            '8-Zurich',
            '9-SMEARII'
        ]

        self._header_list_var = [
            '1-Time (MATLAB, UTC)',
            '02-NO3',
            '03-SO4',
            '04-NH4',
            '05-CL',
            '06-BC',
            '07-OAtot',
            '08-OOAtot',
            '09-NA',
            '10-FPRM(fine primary PM)',
            '11-LV-OOA (MO-OOA)',
            '12-SV-OOA (LO-OOA)',
            '13-BBOA',
            '14-COA',
            '15-HOA',
            '16-SFOA',
            '17-CCOA',
            '18-58-OA',
            '19-Coffee-OA',
            '20-ShipIndustry-OA',
            '21-SeasaltOA',
            '22-CSOA',
            '23-O3',
            '24-SO2',
            '25-NO',
            '26-NO2',
            '27-NOx',
            '28-CO',
            '29-NH3',
            '30-HNO3',
            '31-HONO',
            '32-MONOTERP',
            '33-ISOP',
            '34-Temperature [oC]',
            '35-RH [%]',
            '36-Press [hPa]',
            '37-WS [m/s]',
            '38-WD [deg.]',
            '39-PBLH [m]'
        ]

        self._mock_array = np.zeros(10,39)
        self._n_stations = len(self._header_list_station_name)
        self._mock_list_data = [self._mock_array] * self._n_stations

    @property
    def header(self):
        return self._header

    @property
    def header_list_station_name(self):
        return self._header_list_station_name

    @property
    def header_list_var(self):
        return self._header_list_var

    @property
    def mock_array(self):
        return self._mock_array

    @property
    def n_stations(self):
        return self._n_stations

    @property
    def mock_list_data(self):
        return self._mock_list_data