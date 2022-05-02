from abc import ABC, abstractmethod

from src.python.aurora_py.emulator import Emulator
from src.python.aurora_py.simulator import Simulator


class SimulatorOutputArmor:
    """
    This port is for reading output from the simulator
    """
    def __init__(self, simulator:Simulator, emulator:Emulator):
        self.simulator = Simulator
        self.emulator = emulator

    @abstractmethod
    def get_output(self):
        pass

    def prepare_output(self):
        pass
