import pytest

from auroraPSI.parameters import Parameters
from auroraPSI.simulator import Simulator, SimulatorName


class MockParameterAdapter(Parameters):

    def __init__(self):
        self._values = None
        self._target_simulator = None

    @property
    def values(self):
        if self._values is None:
            raise ValueError("values is None")
        return self._values

    def is_correct_target(self, simulator: Simulator):
        return self.target_simulator == simulator.name

    @property
    def target_simulator(self) -> SimulatorName:
        if self._target_simulator is None:
            raise ValueError("target_simulator is None")
        return self._target_simulator

    @target_simulator.setter
    def target_simulator(self, model_name: SimulatorName):
        if model_name in SimulatorName:
            self._target_simulator = model_name
        else:
            raise ValueError("Invalid model name")


class MockSimulator(Simulator):

    def __init__(self):
        self._parameters = None
        self._simulator_name = None

    def _is_initialized(self) -> bool:
        return self._parameters is not None and \
               self._simulator_name is not None

    def _is_correct_configuration(self) -> bool:
        return self._is_initialized() and \
               self._parameters.is_correct_target(self)

    def run(self) -> int:
        if self._is_correct_configuration():
            print("Model running")
            return 1
        else:
            print("Simulator configuration error")
            return 0

    @property
    def parameters(self) -> Parameters:
        return self._parameters

    @parameters.setter
    def parameters(self, parameters: Parameters):
        self._parameters = parameters

    @property
    def name(self) -> SimulatorName:
        return self._simulator_name

    @name.setter
    def name(self, simulator_name: SimulatorName):
        self._simulator_name = simulator_name


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


model_combinations = [(SimulatorName.CAMx65, SimulatorName.CAMx71, False),
                      (SimulatorName.CAMx71, SimulatorName.CAMx65, False),
                      (SimulatorName.CAMx71, SimulatorName.CAMx71, True),
                      (SimulatorName.CAMx65, SimulatorName.CAMx65, True)]


@pytest.mark.parametrize("test_model_parameters, test_model_simulator, expected", model_combinations)
def test_is_correct_target(test_model_parameters, test_model_simulator, expected):
    mock_adapter = MockParameterAdapter()
    mock_adapter.target_simulator = test_model_simulator
    mock_simulator = MockSimulator()
    mock_simulator.name = test_model_parameters
    assert mock_adapter.is_correct_target(mock_simulator) == expected


@pytest.mark.parametrize("test_model_parameters, test_model_simulator, expected", model_combinations)
def test_simulator_run(test_model_parameters, test_model_simulator, expected):
    mock_adapter = MockParameterAdapter()
    mock_adapter.target_simulator = test_model_simulator
    mock_simulator = MockSimulator()
    mock_simulator.name = test_model_parameters
    mock_simulator.parameters = mock_adapter
    assert expected == mock_simulator.run()
