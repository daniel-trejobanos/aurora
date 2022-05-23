from auroraPSI.matlab_adapter import MATAdapter

@pytest.mark.xfail
def test_header_reading():
    mat_adapter = MATAdapter()
    assert mat_adapter._read_heading() == test_header
