from abc import ABC, abstractmethod
from pathlib import Path

class Sources:
    """
    Class representing the OA  and inorganic sources detected by the  sensors
    """

    @property
    @abstractmethod
    def time(self):
        pass

    @property
    @abstractmethod
    def locations(self):
        pass

    @property
    @abstractmethod
    def variables(self):
        pass


    @abstractmethod
    def save(self, path:Path):
        pass