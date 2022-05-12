from abc import ABC,abstractmethod

class Observations(ABC):
    """
    Class to represent observations captured from a devise, conceptualised as a
    matrix with mu/z as columns and time points as rows.
    """

    @abstractmethod
    def get_times(self):
        pass

    @abstractmethod
    def get_amus(self):
        pass

    @abstractmethod
    def get_data(self):
        pass

    @abstractmethod
    def get_location(self):
        pass

    @abstractmethod
    def get_features( self):
        pass
