import pytest

from src.python.aurora_py.parameters import Parameters
from src.python.aurora_py.simulator import Simulator, SimulatorName


class MockParameterAdapter(Parameters):

    @property
    def values(self):
        return self.values

    def is_correct_target(self, simulator: Simulator):
        if self.model_name != Simulator.name:
            return False

    @property
    def target_simulator(self):
        return self.model_name

    @target_simulator.setter
    def target_simulator(self, model_name: SimulatorName):
        if model_name in SimulatorName:
            self.model_name = model_name
        else:
            raise ValueError("Invalid model name")


@pytest.mark.parametrize("test_model_names", ["CAMx65", 1, 1232, 6.5, None])
def test_parameters_target_simulator_property_exception(test_model_names):
    with pytest.raises(TypeError) as type_err:
        mock_adapter = MockParameterAdapter()
        mock_adapter.target_simulator = test_model_names


@pytest.mark.parametrize("test_model_names", [SimulatorName.CAMx65, SimulatorName.CAMx71])
def test_parameter_target_simulator_property(test_model_names):
    mock_adapter = MockParameterAdapter()
    mock_adapter.target_simulator = test_model_names
    assert True
