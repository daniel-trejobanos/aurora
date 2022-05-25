from abc import ABC, abstractmethod

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
    def location(self):
        pass

    @property
    @abstractmethod
    def variables(self):
        pass
