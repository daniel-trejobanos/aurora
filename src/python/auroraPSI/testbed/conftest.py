import pytest
from auroraPSI.testbed.mock_itx_file import MockItxFile
from auroraPSI.testbed.mock_mat_file import MockMatlabFile

@pytest.fixture()
def mock_itx_file():
    return MockItxFile()

@pytest.fixture()
def mock_mat_file():
    return MockMatlabFile()
