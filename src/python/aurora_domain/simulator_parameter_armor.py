from abc import ABC, abstractmethod

from src.python.aurora_domain.simulator import Simulator
from src.python.aurora_domain.sources import Sources


class SimulatorParameterArmor:

    def __init__(self, simulator: Simulator, sources: Sources):
        self.simulator = simulator
        self.sources = sources

    @abstractmethod
    def setup_simulator_parameters(self):
        pass

    @abstractmethod
    def get_parameters(self):
        pass
