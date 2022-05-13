import numpy as np
import pytest

from src.python.aurora_py.itx_adapter import ItxAdapter, split_multiple_wave_name, split_multidimensional_wave_name


def test_split_multidimensional_wave_name():
    expected_tuple = ("Org_Specs", (2,3))
    assert split_multidimensional_wave_name("WAVES/N=(2,3)\tOrg_Specs") == expected_tuple

def test_split_multiple_wave_name():
    expected_tuple = (("acsm_utc_time","SO4","NO3","NH4","Chl"), (None,5))
    result_tuple = split_multiple_wave_name("WAVES/D\tacsm_utc_time\tSO4\tNO3\tNH4\tChl")
    assert result_tuple == expected_tuple

def test_read_dim(mock_itx_file):
    file_contents = mock_itx_file.contents
    itx_adapter = ItxAdapter(file_contents)
    assert mock_itx_file.lines == itx_adapter.lines


def test_read_wave_shapes(mock_itx_file):
    file_contents = mock_itx_file.contents
    itx_adapter = ItxAdapter(file_contents)
    assert mock_itx_file.waves_shapes == itx_adapter.waves_shapes


def test_read_wave_position(mock_itx_file):
    file_contents = mock_itx_file.contents
    itx_adapter = ItxAdapter(file_contents)
    assert mock_itx_file.waves_positions == itx_adapter.waves_positions


def test_read_wave_names(mock_itx_file):
    file_contents = mock_itx_file.contents
    itx_adapter = ItxAdapter(file_contents)
    assert mock_itx_file.waves_names == itx_adapter.waves_names


def test_read_wave_data(mock_itx_file):
    file_contents = mock_itx_file.contents
    itx_adapter = ItxAdapter(file_contents)
    first_wave = mock_itx_file.waves_names[0]
    np.testing.assert_array_equal( mock_itx_file.wave_data, itx_adapter.get_wave_data(first_wave))
