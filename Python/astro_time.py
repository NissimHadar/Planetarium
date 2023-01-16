import math

class Astro_Time:
    def __init__(self):
        #                  Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
        self.days_normal = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        self.days_leap   = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        
    # Based on the method in Butcher's Ecclesiastical Calendar (valid from 1583)
    def DateOfEaster(self, year):
        if year < 1583:
            return -1, -1
        
        a = year % 19
        b = math.trunc(year / 100)
        c = year % 100
        d = math.trunc(b / 4)
        e = b % 4
        f = math.trunc((b + 8) / 25)
        g = math.trunc((b - f + 1) / 3)
        h = (19 * a + b - d -g + 15) % 30
        i = math.trunc(c / 4)
        k = c % 4
        l = (32 + 2 * (e + i) - h - k) % 7
        m = math.trunc((a + 11 * h + 22 * l)/451)
        n = math.trunc((h + l - 7 * m + 114) / 31)
        p = (h + l - 7 * m + 114) % 31

        day = p + 1
        month = n

        return month, day

    def IsLeapYear(self, year):
        if (year % 4) != 0:
            return False
        
        if (year % 100) != 0:
            return True
        
        return (year % 400) == 0

    # Returns -1 if date is illegal
    def DayToDayNumber(self, year, month, day):
        if month < 1 or month > 12:
            return -1

        if day < 0:
            return -1

        if not self.IsLeapYear(year):
            if day > self.days_normal[month - 1]:
                return -1
        else:
            if day > self.days_leap[month - 1]:
                return -1

        days = 0
        for m in range(month -1 ):
            if not self.IsLeapYear(year):
                days += self.days_normal[m]
            else:
                days += self.days_leap[m]

        return days + day

    # The Julian day number (JDN) is the integer assigned to a whole solar day in the Julian day count starting from noon 
    # Universal Time, with Julian day number 0 assigned to the day starting at noon on Monday, January 1, 4713 BC, 
    # proleptic Julian calendar (November 24, 4714 BC, in the proleptic Gregorian calendar)
    # The Julian date (JD) of any instant is the Julian day number plus the fraction of a day since the preceding noon in Universal Time. 
    # Julian dates are expressed as a Julian day number with a decimal fraction added.
    def CalendarDateToJulianDate(self, year, month, day):
        if month < 3:
            Y = year - 1
            M = month + 12
        else:
            Y = year
            M = month

        if year > 1582 or year == 1582 and month > 10 or year == 1582 and month == 10 and day > 15:
            A = math.trunc(Y / 100)
            B = 2 - A + math.trunc(A / 4)
        else:
            B = 0

        if Y < 0:
            C = math.trunc((365.25 * Y) - 0.75)
        else:
            C = math.trunc(365.25 * Y)

        D = math.trunc(30.6001 * (M + 1))
        
        return B + C + D + day + 1720994.5

    def JulianDateToCalendarDate(self, julianDate):
        I = math.trunc(julianDate + 0.5)
        F = julianDate + 0.5 - I
  
        if I > 2299160:
            A = math.trunc((I - 1867216.25) / 36524.25)
            B = I + A - math.trunc(A / 4) + 1
        else:
            B = I
  
        C = B + 1524
        D = math.trunc((C - 122.1) / 365.25)
        E = math.trunc(365.25 * D)
        G = math.trunc((C - E) / 30.6001)

        day = C - E + F - math.trunc(30.6001 * G)

        if G < 13.5:
            month = G - 1
        else:
            month = G -13

        if month > 2.5:
            year = D - 4716
        else:
            year = D - 4715

        return year, month, day

    # 0 is Sunday
    def DayOfWeek(self, year, month, day):
        jd = self.CalendarDateToJulianDate(year, month, day)
        A = (jd  + 1.5) / 7

        return math.floor((A - math.floor(A)) *  7 + 0.5)

    def HMSToDec(self, hour, minute, second):
        return hour + minute / 60.0 + second / 3600.0

    def DecToHMS(self, time):
        hour = math.trunc(time)
        hour_fraction = time - hour

        minute = math.trunc(hour_fraction * 60)
        minute_fraction = hour_fraction - minute / 60

        second = math.trunc(minute_fraction * 3600 + 0.5)
        if second >= 60:
            second -= 60
            minute += 1

        if minute >= 60:
            minute -= 60
            hour += 1

        if hour >= 24:
            hour -= 24
            
        return hour, minute, second

    # dayLightSavings - 1 for daylight savings, else 0
    def LocalTimeToUniversalTime(self, year, month, day, hour, minute, second, daylight_savings, zone_correction):
        local_civil_time = self.HMSToDec(hour, minute, second)
        universal_time = local_civil_time - daylight_savings - zone_correction

        Julian_date = self.CalendarDateToJulianDate(year, month, universal_time / 24 + day)
        Greenwich_year, Greenwich_month, Greenwich_day_fraction = self.JulianDateToCalendarDate(Julian_date)

        Greenwich_day = math.trunc(Greenwich_day_fraction)
        decimal_time  = 24 * (Greenwich_day_fraction - Greenwich_day)
        hour, minute, second = self.DecToHMS(decimal_time)

        return Greenwich_year, Greenwich_month, Greenwich_day, hour, minute, second

    def UniversalTimeToLocalTime(self, year, month, day, hour, minute, second, daylight_savings, zone_correction):
        universal_time = self.HMSToDec(hour, minute, second)
        zone_time      = universal_time + zone_correction
        local_time     = zone_time + daylight_savings
        Julian_day     = self.CalendarDateToJulianDate(year, month, day) + local_time / 24

        local_year, local_month, local_day_fraction = self.JulianDateToCalendarDate(Julian_day)
        local_day = math.trunc(local_day_fraction)
        
        local_time = 24 * (local_day_fraction - local_day)
        local_hour, local_minute, local_second = self.DecToHMS(local_time)

        return local_year, local_month, local_day, local_hour, local_minute, local_second