import math
import numpy as np

import astro_time

class Astro_Coordinates:
    def __init__(self):
        A = np.array([[1, 2, 3], [3, 4, 5]])
   
    def DMSToDec(self, degrees, minutes, seconds):
        return degrees + minutes / 60.0 + seconds / 3600.0

    def DecToDMS(self, angle):
        degrees = math.trunc(angle)
        degrees_fraction = angle - degrees

        minutes = math.trunc(degrees_fraction * 60)
        minutes_fraction = degrees_fraction - minutes / 60

        seconds = minutes_fraction * 3600
        if seconds >= 59.99:
            seconds  = 0.0
            minutes += 1

        if minutes >= 60:
            minutes -= 60
            degrees += 1

        if degrees >= 360:
            degrees -= 360
            
        return degrees, minutes, seconds

    def RightAscensionToHourAngle(self, \
        ra_hour, ra_minute, ra_second, \
        local_year, local_month, local_day, local_hour, local_minute, local_second, daylight_savings, zone_correction, \
        longitude):

        at = astro_time.Astro_Time()

        universal_time          = at.LocalTimeToUniversalTime(local_year, local_month, local_day, local_hour, local_minute, local_second, daylight_savings, zone_correction)
        universal_time_fraction = at.HMSToDec(universal_time[3], universal_time[4], universal_time[5])

        Greenwich_year  = universal_time[0]
        Greenwich_month = universal_time[1]
        Greenwich_day   = universal_time[2]

        Greenwich_sidereal_time_hour, Greenwich_sidereal_time_minute, Greenwich_sidereal_time_second = at.UniversalTimeToGreenwichSiderealTime(Greenwich_year, Greenwich_month, Greenwich_day, universal_time_fraction, 0, 0)
        Greenwich_sidereal_time_fraction = at.HMSToDec(Greenwich_sidereal_time_hour, Greenwich_sidereal_time_minute, Greenwich_sidereal_time_second)

        local_sidereal_time_hour, local_sidereal_time_minute, local_sidereal_time_second = at.GreenwichSiderealTimeToLocalSiderealTime(Greenwich_sidereal_time_fraction, 0, 0, longitude)
        local_sidereal_time_fraction = at.HMSToDec(local_sidereal_time_hour, local_sidereal_time_minute, local_sidereal_time_second)
        
        right_ascension = self.DMSToDec(ra_hour, ra_minute, ra_second)

        hour_angle = (local_sidereal_time_fraction - right_ascension) % 24

        return at.DecToHMS(hour_angle)
