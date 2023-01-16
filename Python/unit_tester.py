import unittest

import astro_time

class TestTime(unittest.TestCase):
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

        self.assertEqual(at.HMSToDec(11, 30,  0), 11.5)
        self.assertEqual(at.HMSToDec(19, 31, 27), 19.524166666666666)

    def test_DecToHMS(self):
        at = astro_time.Astro_Time()

        self.assertEqual(at.DecToHMS(11.5),               (11, 30,  0))
        self.assertEqual(at.DecToHMS(19.524166666666666), (19, 31, 27))

        self.assertEqual(at.DecToHMS(at.HMSToDec( 1,  2,  3)), ( 1,  2,  3))
        self.assertEqual(at.DecToHMS(at.HMSToDec( 4,  6,  9)), ( 4,  6,  9))
        self.assertEqual(at.DecToHMS(at.HMSToDec( 7, 12, 33)), ( 7, 12, 33))
        self.assertEqual(at.DecToHMS(at.HMSToDec(16, 45, 45)), (16, 45, 45))

    def test_LocalTimeToUniversalTime(self):
        at = astro_time.Astro_Time()

        self.assertEqual(at.LocalTimeToUniversalTime(2013, 7, 1, 3, 37, 0, 1, 4), (2013, 6, 30, 22, 37, 0))
if __name__ == '__main__':
    unittest.main()
