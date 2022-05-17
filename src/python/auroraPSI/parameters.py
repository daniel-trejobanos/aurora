from abc import ABC, abstractmethod

from auroraPSI.simulator import Simulator


class Parameters(ABC):
    """
    Port for setting the parameters
    """

    @property
    @abstractmethod
    def values(self):
        pass

    @values.setter
    @abstractmethod
    def values(self, values):
        pass

    @property
    @abstractmethod
    def target_simulator(self):
        pass

    @target_simulator.setter
    @abstractmethod
    def target_simulator(self, simulator:Simulator):
        pass

    @abstractmethod
    def is_correct_target(self, simulator:Simulator):
        pass
