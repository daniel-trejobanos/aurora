from src.python.aurora_py.itx_adapter import ItxAdapter


def test_read_dim(mock_itx_file):
    file_contents = mock_itx_file.contents
    itx_adapter = ItxAdapter(file_contents)
    assert itx_adapter.lines == mock_itx_file.lines
