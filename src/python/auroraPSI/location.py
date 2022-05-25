from enum import Enum

class LocationType:
    Urban = 1
    Urban_sub = 2
    Rural = 3


class Location:
    """
    class to represent a location from the AURORA project
    """

    def __init__(self):
        self._latitude = None
        self._longitude = None
        self._location_type = None

    @property
    def latitude(self) -> float:
        return self._latitude

    @latitude.setter
    def latitude(self, latitude: float):
        self._latitude = latitude

    @property
    def longitude(self) -> float:
        return self._longitude

    @longitude.setter
    def longitude(self, longitude: float):
        self._longitude = longitude

    @property
    def location_type(self):
        return self._location_type

    @location_type.setter
    def location_type(self, location_type: LocationType):
        self._location_type = location_type

    def to_dict(self):
        return {"lat": self._latitude,
                "lon": self._longitude,
                "type": self._location_type
                }
