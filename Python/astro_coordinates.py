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

    def HourAngleToRightAscension(self, \
        hour_angle_hours, hour_angle_minutes, hour_angle_seconds, \
        local_year, local_month, local_day, local_hour, local_minute, local_second, daylight_savings, zone_correction, \
        longitude
        ):

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

        hour_angle_fractions = at.HMSToDec(hour_angle_hours, hour_angle_minutes, hour_angle_seconds)

        right_ascension = (local_sidereal_time_fraction - hour_angle_fractions) % 24

        return at.DecToHMS(right_ascension)
    
    # Coordinate systems
    #   All in degrees
    #
    #   horizon
    #       azimuth             - east from north (alpha)
    #       altitude            - up from horizon (A)
    #
    #   equatorial
    #       right ascension     - east from the first point of Aries (alpha)
    #       hour angle          - west from south (H)
    #       declination         - up from the equator (delta)
    #
    #   ecliptic
    #       ecliptic longitude  - east from the first point of Aries (gamma)
    #       ecliptic latitude   - (beta)
    #
    #   galactic
    #       galactic longitude  - galactic centre to sun, (l)
    #       galactic latitude   - (b)
    #
    #   phi     - geographical latitude
    #   st      - local sidereal time
    #   epsilon - obliquity of the ecliptic

    # transforms to alpha, A to H, delta
    def Equatorial_T_Horizon(self, phi):
        a = math.radians(phi)
        return np.array([[-math.sin(a),  0, math.cos(a)], \
                         [           0, -1,           0], \
                         [ math.cos(a),   0, math.sin(a)]])

    # transforms to H, delta to alpha, A
    def Horizon_T_Equatorial(self, phi):
        a = math.radians(phi)
        return np.array([[-math.sin(a),  0, math.cos(a)], \
                         [           0, -1,           0], \
                         [ math.cos(a),  0, math.sin(a)]])

    # transforms to alpha, delta to H, delta
    def Equatorial_T_Horizon(self, ST):
        a = math.radians(ST)
        return np.array([[ math.cos(a),  math.sin(a), 0], \
                         [ math.sin(a), -math.cos(a), 0], \
                         [           0,            0, 1]])
        
    # transforms to H, delta to alpha, delta
    def Horizon_T_Equatorial(self, ST):
        a = math.radians(ST)
        return np.array([[ math.cos(a),  math.sin(a), 0], \
                         [ math.sin(a), -math.cos(a), 0], \
                         [           0,            0, 1]])

    # transform alpha, delta to l, b
    def Galactic_T_Equatorial(self):
        return np.array([[-0.0669887, -0.8727558, -0.4835389], \
                         [ 0.4927285, -0.4503470,  0.7445846], \
                         [-0.8676008, -0.1883746,  0.4601998]])

    # transform l, b to alpha, delta
    def Equatorial_T_Galactic(self):
        return np.array([[-0.0669888,  0.4927285, -0.8676008], \
                         [-0.8727557, -0.4503469, -0.1883746], \
                         [-0.4835389,  0.7445846,  0.4601998]])

    def To_Vector(self, a, b):
        u = math.radians(a)
        v = math.radians(b)

        return np.array([math.cos(u) * math.cos(v), math.sin(u) * math.cos(v), math.sin(v)])

    def From_Vector(self, v):
        theta = math.degrees(math.atan(v[1], v[0]))
        psi   = math.degrees(math.sin(v[2]))

        return theta, psi
