import pytest
from auroraPSI.matlab_adapter import MATAdapter, split_variables_heading


def test_split_header_variables(mock_mat_file):
    expected_list_variables = mock_mat_file.header_list_var
    variable_string = mock_mat_file.header_variable_name
    assert split_variables_heading(variable_string) == expected_list_variables


def test_split_header_stations(mock_mat_file):
    expected_list_stations = mock_mat_file.header_list_station_name
    station_string = mock_mat_file.header_station_name
    assert split_variables_heading(station_string) == expected_list_stations


@pytest.mark.xfail
def test_header_reading(mock_mat_file):
    mat_adapter = MATAdapter(mock_mat_file.header)
    assert mat_adapter.variables == mock_mat_file.header_list_var
