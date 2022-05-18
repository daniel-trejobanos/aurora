from abc import ABC, abstractmethod


class ObservationPlotter(ABC):

    @abstractmethod
    def scatter_plot(self):
        pass

    @abstractmethod
    def date_plot(self):
        pass

    @abstractmethod
    def histogram_plot(self):
        pass

    @abstractmethod
    def all_plot(self):
        pass