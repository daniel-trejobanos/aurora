import pytest


class MockItxFile:
    def __init__(self):
        first_line = 'IGOR\r'
        second_line = 'WAVES/N=(3,2)\tOrg_Specs\r'
        third_line = 'BEGIN\r'
        data_lines = '\t0.38642111\t0.16979307\r' \
                     '\t0.38642111\t0.16979307\r' \
                     '\t0.38642111\t0.16979307\r'
        self._contents = first_line + second_line + third_line + data_lines
        self._lines = 6

    @property
    def contents(self):
        return self._contents

    @property
    def lines(self):
        return self._lines

    @property
    def waves(self):
        return {'Org_Specs': (3, 2) }


@pytest.fixture()
def mock_itx_file():
    return MockItxFile()
