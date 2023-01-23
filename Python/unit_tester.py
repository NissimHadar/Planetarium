import unittest

import astro_time
import astro_coordinates

class Common(unittest.TestCase):
    def assertTuplesAlmostEqual(self, a, b):
        if len(a) != len(b):
            return False

        for i in range(len(a)):
            self.assertAlmostEqual(a[i], b[i], 2)

class TestTime(Common):
    def test_DateOfEaster(self):
        at = astro_time.Astro_Time()

        self.assertEqual(at.DateOfEaster(1580), (-1, -1))

        self.assertEqual(at.DateOfEaster(2009), (4, 12))
        self.assertEqual(at.DateOfEaster(2012), (4,  8))
        self.assertEqual(at.DateOfEaster(2023), (4,  9))

    def test_IsLeapYear(self):
        at = astro_time.Astro_Time()

        self.assertEqual(at.IsLeapYear(1997), False)
        self.assertEqual(at.IsLeapYear(2012), True )
        self.assertEqual(at.IsLeapYear(1900), False)
        self.assertEqual(at.IsLeapYear(2000), True )

    def test_DayToDayNumber(self):
        at = astro_time.Astro_Time()

        # Check illegal month
        self.assertEqual(at.DayToDayNumber(2023,  0,  1), -1)
        self.assertEqual(at.DayToDayNumber(2023, 13,  1), -1)

        # Check illegal day
        self.assertEqual(at.DayToDayNumber(2023,  2, -1), -1)

        self.assertEqual(at.DayToDayNumber(2023,  1, 32), -1)
        self.assertEqual(at.DayToDayNumber(2023,  2, 29), -1)
        self.assertEqual(at.DayToDayNumber(2023,  3, 32), -1)
        self.assertEqual(at.DayToDayNumber(2023,  4, 31), -1)
        self.assertEqual(at.DayToDayNumber(2023,  5, 32), -1)
        self.assertEqual(at.DayToDayNumber(2023,  6, 31), -1)
        self.assertEqual(at.DayToDayNumber(2023,  7, 32), -1)
        self.assertEqual(at.DayToDayNumber(2023,  8, 32), -1)
        self.assertEqual(at.DayToDayNumber(2023,  9, 31), -1)
        self.assertEqual(at.DayToDayNumber(2023, 10, 32), -1)
        self.assertEqual(at.DayToDayNumber(2023, 11, 31), -1)
        self.assertEqual(at.DayToDayNumber(2023, 12, 32), -1)

        self.assertEqual(at.DayToDayNumber(2024,  1, 32), -1)
        self.assertEqual(at.DayToDayNumber(2024,  2, 30), -1)
        self.assertEqual(at.DayToDayNumber(2024,  3, 32), -1)
        self.assertEqual(at.DayToDayNumber(2024,  4, 31), -1)
        self.assertEqual(at.DayToDayNumber(2024,  5, 32), -1)
        self.assertEqual(at.DayToDayNumber(2024,  6, 31), -1)
        self.assertEqual(at.DayToDayNumber(2024,  7, 32), -1)
        self.assertEqual(at.DayToDayNumber(2024,  8, 32), -1)
        self.assertEqual(at.DayToDayNumber(2024,  9, 31), -1)
        self.assertEqual(at.DayToDayNumber(2024, 10, 32), -1)
        self.assertEqual(at.DayToDayNumber(2024, 11, 31), -1)
        self.assertEqual(at.DayToDayNumber(2024, 12, 32), -1)

        self.assertEqual(at.DayToDayNumber(2023,  1,  1),    1)
        self.assertEqual(at.DayToDayNumber(2023,  1, 31),   31)
        self.assertEqual(at.DayToDayNumber(2023,  2,  1),   32)
        self.assertEqual(at.DayToDayNumber(2023,  3,  1),   60)
        self.assertEqual(at.DayToDayNumber(2023,  4,  3),   93)
        self.assertEqual(at.DayToDayNumber(2023,  5, 28),  148)
        self.assertEqual(at.DayToDayNumber(2023,  6,  1),  152)
        self.assertEqual(at.DayToDayNumber(2023,  7,  9),  190)
        self.assertEqual(at.DayToDayNumber(2023,  8, 15),  227)
        self.assertEqual(at.DayToDayNumber(2023,  9, 29),  272)
        self.assertEqual(at.DayToDayNumber(2023, 10,  9),  282)
        self.assertEqual(at.DayToDayNumber(2023, 11, 20),  324)
        self.assertEqual(at.DayToDayNumber(2023, 12, 31),  365)

        self.assertEqual(at.DayToDayNumber(2024,  1,  1),    1)
        self.assertEqual(at.DayToDayNumber(2024,  1, 31),   31)
        self.assertEqual(at.DayToDayNumber(2024,  2,  1),   32)
        self.assertEqual(at.DayToDayNumber(2024,  3,  1),   61)
        self.assertEqual(at.DayToDayNumber(2024,  4,  3),   94)
        self.assertEqual(at.DayToDayNumber(2024,  5, 28),  149)
        self.assertEqual(at.DayToDayNumber(2024,  6,  1),  153)
        self.assertEqual(at.DayToDayNumber(2024,  7,  9),  191)
        self.assertEqual(at.DayToDayNumber(2024,  8, 15),  228)
        self.assertEqual(at.DayToDayNumber(2024,  9, 29),  273)
        self.assertEqual(at.DayToDayNumber(2024, 10,  9),  283)
        self.assertEqual(at.DayToDayNumber(2024, 11, 20),  325)
        self.assertEqual(at.DayToDayNumber(2024, 12, 31),  366)

    def test_CalendarDateToJulianDate(self):
        at = astro_time.Astro_Time()

        self.assertEqual(at.CalendarDateToJulianDate(2009, 6, 19.75),           2455002.25)
        self.assertEqual(at.CalendarDateToJulianDate(2013, 1,  0.02083333333 ), 2456292.5208333335)

    def test_julianDateToCalendarDay(self):
        at = astro_time.Astro_Time()

        self.assertEqual(at.JulianDateToCalendarDate(2455002.25), (2009, 6, 19.75))

        self.assertEqual(at.JulianDateToCalendarDate(at.CalendarDateToJulianDate(2030,  4, 27.5 )), (2030,  4, 27.5 ))
        self.assertEqual(at.JulianDateToCalendarDate(at.CalendarDateToJulianDate(1920,  5, 23.25)), (1920,  5, 23.25))

    def test_DayOfWeek(self):
        at = astro_time.Astro_Time()

        self.assertEqual(at.DayOfWeek(1960, 1, 31), 0)
        self.assertEqual(at.DayOfWeek(2023, 1, 15), 0)
        self.assertEqual(at.DayOfWeek(2045, 3,  7), 2)

    def test_HMSToDec(self):
        at = astro_time.Astro_Time()

        self.assertAlmostEqual(at.HMSToDec( 11, 30,  0    ),  11.5       )
        self.assertAlmostEqual(at.HMSToDec( 19, 31, 27    ),  19.52416666)
        self.assertAlmostEqual(at.HMSToDec( 17, 51, 28.688),  17.85796888)
        self.assertAlmostEqual(at.HMSToDec(-17, 51, 28.688), -17.85796888)

    def test_DecToHMS(self):
        at = astro_time.Astro_Time()

        self.assertTuplesAlmostEqual(at.DecToHMS(11.5),               (11, 30,  0))
        self.assertTuplesAlmostEqual(at.DecToHMS(19.524166666666666), (19, 31, 27))
        self.assertTuplesAlmostEqual(at.DecToHMS(at.HMSToDec( 1,  2,  3)), ( 1,  2,  3))
        self.assertTuplesAlmostEqual(at.DecToHMS(at.HMSToDec( 4,  6,  9)), ( 4,  6,  9))
        self.assertTuplesAlmostEqual(at.DecToHMS(at.HMSToDec( 7, 12, 33)), ( 7, 12, 33))
        self.assertTuplesAlmostEqual(at.DecToHMS(at.HMSToDec(16, 45, 45)), (16, 45, 45))

    def test_LocalTimeToUniversalTime(self):
        at = astro_time.Astro_Time()

        self.assertTuplesAlmostEqual(at.LocalTimeToUniversalTime(2013, 7, 1, 3, 37, 0, 1, 4), (2013, 6, 30, 22, 37, 0))

    def test_UniversalTimeToLocalTime(self):
        at = astro_time.Astro_Time()

        self.assertTuplesAlmostEqual(at.UniversalTimeToLocalTime(2013, 6, 30, 22, 37, 0, 1, 4), (2013, 7, 1, 3, 37, 0))


    def test_UniversalTimeToGreenwichSiderealTime(self):
        at = astro_time.Astro_Time()

        self.assertTuplesAlmostEqual(at.UniversalTimeToGreenwichSiderealTime(1980, 4, 22, 14, 36, 51.67), (4, 40, 5.23))

    def test_GreenwichSiderealTimeToUniversalTime(self):
        at = astro_time.Astro_Time()

        self.assertTuplesAlmostEqual(at.GreenwichSiderealTimeToUniversalTime(1980, 4, 22, 4, 40, 5.23), (14, 36, 51.67))

    def test_GreenwichSiderealTimeToLocalSiderealTime(self):
        at = astro_time.Astro_Time()

        self.assertTuplesAlmostEqual(at.GreenwichSiderealTimeToLocalSiderealTime(4, 40, 5.23, -64), (0, 24, 5.23))

    def test_LocalSiderealTimeToGreenwichSiderealTime(self):
        at = astro_time.Astro_Time()

        self.assertTuplesAlmostEqual(at.LocalSiderealTimeToGreenwichSiderealTime(0, 24, 5.23, -64), (4, 40, 5.23))

class TestCoordinates(Common):
    def test_DMSToDec(self):
        ac = astro_coordinates.Astro_Coordinates()

        self.assertAlmostEqual(ac.DMSToDec( 11, 30,  0    ),  11.5)
        self.assertAlmostEqual(ac.DMSToDec( 19, 31, 27    ),  19.524166666666666)
        self.assertAlmostEqual(ac.DMSToDec( 17, 51, 28.688),  17.85796888)
        self.assertAlmostEqual(ac.DMSToDec(-17, 51, 28.688), -17.85796888)

    def test_DecToDMS(self):
        ac = astro_coordinates.Astro_Coordinates()

        self.assertTuplesAlmostEqual(ac.DecToDMS(11.5), (11, 30,  0))
        self.assertTuplesAlmostEqual(ac.DecToDMS(182.5241667), (182, 31, 27))

    def test_RightAscensionToHourAngle(self):
        ac = astro_coordinates.Astro_Coordinates()

        self.assertTuplesAlmostEqual(ac.RightAscensionToHourAngle(18, 32, 21, 1980, 4, 22, 14, 36, 51.67, 0, -4, -64), (9, 52, 23.6555))

    def test_HourAngleToRightAscension(self):
        ac = astro_coordinates.Astro_Coordinates()

        self.assertTuplesAlmostEqual(ac.HourAngleToRightAscension(9, 52, 23.6555, 1980, 4, 22, 14, 36, 51.67, 0, -4, -64), (18, 32, 21))

    def test_ToFromVector(self):
        ac = astro_coordinates.Astro_Coordinates()

        self.assertTuplesAlmostEqual(ac.To_Vector(  0,  0), ( 1,      0,     0    ))
        self.assertTuplesAlmostEqual(ac.To_Vector( 45,  0), ( 0.707,  0.707, 0    ))
        self.assertTuplesAlmostEqual(ac.To_Vector( 90,  0), ( 0,      1,     0    ))
        self.assertTuplesAlmostEqual(ac.To_Vector(135,  0), (-0.707,  0.707, 0    ))
        self.assertTuplesAlmostEqual(ac.To_Vector(180,  0), (-1,      0,     0    ))
        self.assertTuplesAlmostEqual(ac.To_Vector(225,  0), (-0.707, -0.707, 0    ))
        self.assertTuplesAlmostEqual(ac.To_Vector(270,  0), ( 0,     -1,     0    ))
        self.assertTuplesAlmostEqual(ac.To_Vector(360,  0), ( 1,      0,     0    ))

        self.assertTuplesAlmostEqual(ac.To_Vector( 0,  45), (  0.707, 0,     0.707))
        self.assertTuplesAlmostEqual(ac.To_Vector( 0,  90), (  0,     0,     1    ))
        self.assertTuplesAlmostEqual(ac.To_Vector( 0, 135), ( -0.707, 0,     0.707))
        self.assertTuplesAlmostEqual(ac.To_Vector( 0, 180), (  -1,    0,     0    ))
        self.assertTuplesAlmostEqual(ac.To_Vector( 0, 225), ( -0.707, 0,    -0.707))
        self.assertTuplesAlmostEqual(ac.To_Vector( 0, 270), (  0,     0,    -1    ))
        self.assertTuplesAlmostEqual(ac.To_Vector( 0, 315), (  0.707, 0,    -0.707))
        self.assertTuplesAlmostEqual(ac.To_Vector( 0, 360), ( 1,      0,     0    ))

    def test_ConvertEclipticToHorizonCoordinates(self):
        ac = astro_coordinates.Astro_Coordinates()
        at = astro_time.Astro_Time()

        obliquity = ac.DMSToDec(23, 26, 46.45)

        planet_ecliptic_latitude  = ac.DMSToDec(-17, 51, 28.688)
        planet_ecliptic_longitude = ac.DMSToDec( 97, 38, 17.228)
        
        earth_latitude = ac.DMSToDec(52, 10, 31)
        
        local_sidereal_time_hours = at.HMSToDec(5, 9, 21.103)

        expected_azimuth  = ac.DMSToDec(153, 29, 31)
        expected_altitude = ac.DMSToDec( 40, 23, 58)

        actual_azimuth, actual_altitude = ac.ConvertEclipticToHorizonCoordinates(\
            obliquity, \
            planet_ecliptic_latitude, planet_ecliptic_longitude, \
            earth_latitude, local_sidereal_time_hours)

        self.assertTuplesAlmostEqual((actual_azimuth, actual_altitude), (expected_azimuth, expected_altitude))

    def test_AngleBetweenObjects(self):
        ac = astro_coordinates.Astro_Coordinates()
        at = astro_time.Astro_Time()

        a_lat_degs  = ac.DMSToDec( -8, 13, 30  ) # beta Orion
        a_lon_hours = at.HMSToDec(  5, 13, 31.7)
        b_lat_degs  = ac.DMSToDec(-16, 41, 11  ) # alpha Canis Majoris
        b_lon_hours = at.HMSToDec(  6, 44, 13.4)

        self.assertTuplesAlmostEqual(ac.DecToDMS(ac.AngleBetweenObjects(a_lat_degs, a_lon_hours, b_lat_degs, b_lon_hours)), (23, 40, 25.8579))

    def test_RiseSetTime(self):
        ac = astro_coordinates.Astro_Coordinates()
        at = astro_time.Astro_Time()

        right_ascension = at.HMSToDec(23, 39, 20)
        declination     = ac.DMSToDec(21, 42,  0)
        geo_latitude    = 30
        geo_longitude   = 64
        greenwich_year  = 2010
        greenwich_month = 8
        greenwhich_day  = 24

        rise_time = ac.RiseTime(right_ascension, declination, geo_latitude, geo_longitude, greenwich_year, greenwich_month, greenwhich_day)
        self.assertTuplesAlmostEqual(rise_time, (14, 16, 18.0116, 64.3623))

        set_time = ac.SetTime(right_ascension, declination, geo_latitude, geo_longitude, greenwich_year, greenwich_month, greenwhich_day)
        self.assertTuplesAlmostEqual(rise_time, (4, 10, 1.1645, 295.637652))

if __name__ == '__main__':
    unittest.main()
