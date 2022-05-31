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


def test_header_reading(mock_mat_file):
    mat_adapter = MATAdapter(mock_mat_file.file_contents)
    assert mat_adapter.variables == mock_mat_file.header_list_var


def test_variable_reading(mock_mat_file):
    mat_adapter = MATAdapter(mock_mat_file.file_contents)
    assert mat_adapter.data == mock_mat_file.mock_list_data


def test_pmf_reading(mock_mat_file):
    mat_adapter = MATAdapter(mock_mat_file.file_contents)
    assert mat_adapter.data == mock_mat_file.mock_list_data



@pytest.mark.xfail
def test_to_pandas(mock_mat_file):
    assert False

# TODO
def test_to_UTC_time(mock_mat_file):
    assert False