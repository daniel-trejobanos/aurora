from abc import ABC

class Simulator(ABC):
    """
    class to wrap the CAMx simulators.
    """
    @abstractmethod
    def run(self):
        pass