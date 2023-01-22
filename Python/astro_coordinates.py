import math
import numpy as np

import astro_time

class Astro_Coordinates:
    def __init__(self):
        A = np.array([[1, 2, 3], [3, 4, 5]])
   
    def DMSToDec(self, degrees, minutes, seconds):
        if degrees >= 0:
            return degrees + minutes / 60.0 + seconds / 3600.0
        else:
            return degrees - minutes / 60.0 - seconds / 3600.0

    def DecToDMS(self, angle):
        degrees = math.trunc(angle)
        degrees_decion = angle - degrees

        minutes = math.trunc(degrees_decion * 60)
        minutes_decion = degrees_decion - minutes / 60

        seconds = minutes_decion * 3600
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
        universal_time_decion = at.HMSToDec(universal_time[3], universal_time[4], universal_time[5])

        Greenwich_year  = universal_time[0]
        Greenwich_month = universal_time[1]
        Greenwich_day   = universal_time[2]

        Greenwich_sidereal_time_hour, Greenwich_sidereal_time_minute, Greenwich_sidereal_time_second = at.UniversalTimeToGreenwichSiderealTime(Greenwich_year, Greenwich_month, Greenwich_day, universal_time_decion, 0, 0)
        Greenwich_sidereal_time_decion = at.HMSToDec(Greenwich_sidereal_time_hour, Greenwich_sidereal_time_minute, Greenwich_sidereal_time_second)

        local_sidereal_time_hour, local_sidereal_time_minute, local_sidereal_time_second = at.GreenwichSiderealTimeToLocalSiderealTime(Greenwich_sidereal_time_decion, 0, 0, longitude)
        local_sidereal_time_decion = at.HMSToDec(local_sidereal_time_hour, local_sidereal_time_minute, local_sidereal_time_second)
        
        right_ascension = self.DMSToDec(ra_hour, ra_minute, ra_second)

        hour_angle = (local_sidereal_time_decion - right_ascension) % 24

        return at.DecToHMS(hour_angle)

    def HourAngleToRightAscension(self, \
        hour_angle_hours, hour_angle_minutes, hour_angle_seconds, \
        local_year, local_month, local_day, local_hour, local_minute, local_second, daylight_savings, zone_correction, \
        longitude
        ):

        at = astro_time.Astro_Time()

        universal_time          = at.LocalTimeToUniversalTime(local_year, local_month, local_day, local_hour, local_minute, local_second, daylight_savings, zone_correction)
        universal_time_decion = at.HMSToDec(universal_time[3], universal_time[4], universal_time[5])

        Greenwich_year  = universal_time[0]
        Greenwich_month = universal_time[1]
        Greenwich_day   = universal_time[2]

        Greenwich_sidereal_time_hour, Greenwich_sidereal_time_minute, Greenwich_sidereal_time_second = at.UniversalTimeToGreenwichSiderealTime(Greenwich_year, Greenwich_month, Greenwich_day, universal_time_decion, 0, 0)
        Greenwich_sidereal_time_decion = at.HMSToDec(Greenwich_sidereal_time_hour, Greenwich_sidereal_time_minute, Greenwich_sidereal_time_second)

        local_sidereal_time_hour, local_sidereal_time_minute, local_sidereal_time_second = at.GreenwichSiderealTimeToLocalSiderealTime(Greenwich_sidereal_time_decion, 0, 0, longitude)
        local_sidereal_time_decion = at.HMSToDec(local_sidereal_time_hour, local_sidereal_time_minute, local_sidereal_time_second)

        hour_angle_decions = at.HMSToDec(hour_angle_hours, hour_angle_minutes, hour_angle_seconds)

        right_ascension = (local_sidereal_time_decion - hour_angle_decions) % 24

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

    # transforms from alpha, A to H, delta
    def Equatorial_T_Horizon_lat(self, phi):
        a = math.radians(phi)
        return np.array([[-math.sin(a),  0, math.cos(a)], \
                         [           0, -1,           0], \
                         [ math.cos(a),  0, math.sin(a)]])

    # transforms from H, delta to alpha, A
    def Horizon_T_Equatorial_lat(self, phi):
        a = math.radians(phi)
        return np.array([[-math.sin(a),  0, math.cos(a)], \
                         [           0, -1,           0], \
                         [ math.cos(a),  0, math.sin(a)]])

    # transforms from alpha, delta to H, delta
    def Equatorial_T_Horizon_ST(self, ST):
        a = math.radians(ST)
        return np.array([[ math.cos(a),  math.sin(a), 0], \
                         [ math.sin(a), -math.cos(a), 0], \
                         [           0,            0, 1]])
        
    # transforms from H, delta to alpha, delta
    def Horizon_T_Equatorial_ST(self, ST):
        a = math.radians(ST)
        return np.array([[ math.cos(a),  math.sin(a), 0], \
                         [ math.sin(a), -math.cos(a), 0], \
                         [           0,            0, 1]])

    # transforms from alpha, delta to gamma, beta
    def Ecliptic_T_Equatorial(self, epsilon):
        a = math.radians(epsilon)
        return np.array([[ 1,           0,           0], \
                         [ 0,  math.cos(a), math.sin(a)], \
                         [ 0, -math.sin(a), math.cos(a)]])

    # transforms from gamma, beta to alpha, delta
    def Equatorial_T_Ecliptic(self, epsilon):
        a = math.radians(epsilon)
        return np.array([[ 1,           0,            0], \
                         [ 0, math.cos(a), -math.sin(a)], \
                         [ 0, math.sin(a),  math.cos(a)]])


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
        theta = math.degrees(math.atan2(v[1], v[0]))
        psi   = math.degrees(math.asin(v[2]))

        return theta, psi

    def ConvertEclipticToHorizonCoordinates(self, \
        obliquity, \
        ecliptic_latitude, ecliptic_longitude, \
        earth_latitude, local_sidereal_time_hours):

        v = self.To_Vector(ecliptic_longitude, ecliptic_latitude)

        equatorial_T_ecliptic = self.Equatorial_T_Ecliptic(obliquity)

        local_sidereal_time_degs = local_sidereal_time_hours * 15
        horizon_T_equatorial = self.Horizon_T_Equatorial_ST(local_sidereal_time_degs)

        horizon_T_ecliptic = np.dot(horizon_T_equatorial, equatorial_T_ecliptic)

        equatorial_T_horizon = self.Equatorial_T_Horizon_lat(earth_latitude)

        equatorial_T_ecliptic = np.dot(equatorial_T_horizon, horizon_T_ecliptic)

        w = np.dot(equatorial_T_ecliptic, v)

        A, a = self.From_Vector(w)

        return A, a