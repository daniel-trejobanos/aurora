from auroraPSI.location import Location


class Station:
    def __init__(self):
        self._location = None

    @property
    def location(self):
        return self._location

    @location.setter
    def location(self, location):
        self._location = location
