
class Sources:
    """
    Class representing the OA  and inorganic sources detected by the  sensors
    """
    def __init__(self, time, location):
        self._time = time
        self._location = location

    @property
    def time(self):
        return self.time

    @property
    def location(self):
        return self.location

