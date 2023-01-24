import math
import numpy as np

import astro_time

class Astro_Coordinates:
    def __init__(self):
        A = np.array([[1, 2, 3], [3, 4, 5]])
   
    ### 21, page 39 
    def DMSToDec(self, degrees, minutes, seconds):
        if degrees >= 0 or degrees == 0 and minutes >= 0:
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

    ### 24, page 43
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

    ### 31, page 60
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

    
    ### 32, page 66
    def AngleBetweenObjects(self, a_lat_degs, a_lon_hours, b_lat_degs, b_lon_hours):

        a_lat = math.radians(a_lat_degs)
        a_lon = math.radians(a_lon_hours * 15)
        b_lat = math.radians(b_lat_degs)
        b_lon = math.radians(b_lon_hours * 15)

        cos_d_rads = math.sin(a_lat) * math.sin(b_lat) + math.cos(a_lat) * math.cos(b_lat) * math.cos(a_lon - b_lon)
        d_rads = math.acos(cos_d_rads)
        d_degs = math.degrees(d_rads)

        return d_degs

    def VerticalShiftDegs(self):
        return 0.5667 # degrees, constant for now

    ### 33, page 67
    def __RiseOrSetTime(self, is_rise, right_ascension, declination, geo_latitude_degs, geo_longitude_degs, greenwich_year, greenwich_month, greenwhich_day):
        declination_rads       = math.radians(declination)
        vertical_shift_rads    = math.radians(self.VerticalShiftDegs())
        geo_latitude_degs_rads = math.radians(geo_latitude_degs)

        cos_H = -(math.sin(vertical_shift_rads) + math.sin(geo_latitude_degs_rads) * math.sin(declination_rads)) / (math.cos(geo_latitude_degs_rads) * math.cos(declination_rads))
        
        if cos_H < -1.0 or cos_H > 1.0:
            return False

        H = math.degrees(math.acos(cos_H)) / 15
        
        cos_Ar = (math.sin(declination_rads) + math.sin(vertical_shift_rads) * math.sin(geo_latitude_degs_rads)) / (math.cos(vertical_shift_rads) * math.cos(geo_latitude_degs_rads))

        if is_rise:
            Ar = math.degrees(math.acos(cos_Ar)) % 360
        else:
            Ar = 360 - math.degrees(math.acos(cos_Ar)) % 360

        at = astro_time.Astro_Time()

        if is_rise:
            local_sidereal_time_dec = (right_ascension - H) % 24
        else: # set time
            local_sidereal_time_dec = (right_ascension + H) % 24

        local_sidereal_time_hour, local_sidereal_time_minute, local_sidereal_time_second = at.DecToHMS(local_sidereal_time_dec)
        greenwich_sidereal_hour, greenwich_sidereal_minute, greenwich_sidereal_second = at.LocalSiderealTimeToGreenwichSiderealTime(local_sidereal_time_hour, local_sidereal_time_minute, local_sidereal_time_second, geo_longitude_degs)
        
        return at.GreenwichSiderealTimeToUniversalTime(greenwich_year, greenwich_month, greenwhich_day, greenwich_sidereal_hour, greenwich_sidereal_minute, greenwich_sidereal_second), Ar

    def RiseTime(self, right_ascension, declination, geo_latitude_degs, geo_longitude_degs, greenwich_year, greenwich_month, greenwhich_day):
        return self.__RiseOrSetTime(True, right_ascension, declination, geo_latitude_degs, geo_longitude_degs, greenwich_year, greenwich_month, greenwhich_day)

    def SetTime(self, right_ascension, declination, geo_latitude_degs, geo_longitude_degs, greenwich_year, greenwich_month, greenwhich_day):
        return self.__RiseOrSetTime(False, right_ascension, declination, geo_latitude_degs, geo_longitude_degs, greenwich_year, greenwich_month, greenwhich_day)

    def __compute_P(self, epoch):
        at = astro_time.Astro_Time()

        julian_date = at.CalendarDateToJulianDate(epoch[0], epoch[1], epoch[2])

        T = (julian_date - 2451545) / 36525

        zeta_A  = 0.6406161 * T + 0.0000839 * T * T + 0.0000050 * T * T * T
        z_A     = 0.6406161 * T + 0.0003041 * T * T + 0.0000051 * T * T * T
        theta_A = 0.5567530 * T + 0.0001185 * T * T + 0.0000116 * T * T * T

        cx = math.cos(math.radians(zeta_A))
        sx = math.sin(math.radians(zeta_A))
        cz = math.cos(math.radians(z_A))
        sz = math.sin(math.radians(z_A))
        ct = math.cos(math.radians(theta_A))
        st = math.sin(math.radians(theta_A))

        return np.array([[ cx * ct * cz - sx * sz,  cx * ct * sz + sx * cz,  cx * st],\
                         [-sx * ct * cz - cx * sz, -sx * ct * sz + cx * cz, -sx * st],\
                         [-st * cz,                -st * sz,                 ct]])

    ### 34, page 71
    def Precession(self, epoch_1, epoch_2, right_ascension_1_hours, declination_1_degrees):
        at = astro_time.Astro_Time()

        right_ascension_1_degrees = right_ascension_1_hours * 15

        v = self.To_Vector(right_ascension_1_degrees, declination_1_degrees)

        P_tag = self.__compute_P(epoch_1)
        P     = np.transpose(self.__compute_P(epoch_2))

        s       = np.dot(P_tag, v)
        m, n, p = np.dot(P,     s)

        right_ascension_2_hours = at.DecToHMS(math.degrees(math.atan2(n, m)) / 15)
        declination_2_degrees   = self.DecToDMS(math.degrees(math.asin(p)))

        return right_ascension_2_hours, declination_2_degrees
        
    ### 35, page 76
    def Nutation_seconds(self, date):
        at = astro_time.Astro_Time()

        julian_date = at.CalendarDateToJulianDate(date[0], date[1], date[2])

        T_centuries = (julian_date - 2415020) / 36525

        A = 100.002136 * T_centuries
        L_degrees = 279.6967 + 360 * (A - math.trunc(A))

        B = 5.372617 * T_centuries
        omega = 259.1833 - 360 * (B - math.trunc(B))

        delta_ecliptic_lat_arcsec =   9.2 * math.cos(math.radians(omega)) + 0.5 * math.cos(math.radians(2 * L_degrees))
        delta_ecliptic_lon_arcsec = -17.2 * math.sin(math.radians(omega)) - 1.3 * math.sin(math.radians(2 * L_degrees))

        return delta_ecliptic_lat_arcsec, delta_ecliptic_lon_arcsec

    ### 36, page 78
    def Aberration_seconds(self, ecliptic_lat_dec, ecliptic_lon_dec):
        return 0, 0
