from abc import ABC, abstractmethod
from enum import Enum

class SimulatorName(Enum):
    CAMx65 = 1
    CAMx71 = 2

class Simulator(ABC):
    """
    class to wrap the CAMx simulators.
    """

    @abstractmethod
    def run(self):
        pass

    @property
    @abstractmethod
    def parameters(self):
        pass

    @parameters.setter
    @abstractmethod
    def parameters(self, parameters):
        pass

    @property
    @abstractmethod
    def name(self):
        pass

    @name.setter
    @abstractmethod
    def name(self, simulator_name: SimulatorName):
        pass