from src.python.aurora_py.itx_adapter import ItxAdapter


def test_read_dim(mock_itx_file):
    file_contents = mock_itx_file.contents
    itx_adapter = ItxAdapter(file_contents)
    assert itx_adapter.lines == mock_itx_file.lines

def test_read_wave(mock_itx_file):
    file_contents = mock_itx_file.contents
    itx_adapter = ItxAdapter(file_contents)
    assert itx_adapter.waves == mock_itx_file.waves
