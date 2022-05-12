import numpy as np

from src.python.aurora_py.itx_adapter import ItxAdapter


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
