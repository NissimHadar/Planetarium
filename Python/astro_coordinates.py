import math

class Astro_Coordinates:
    def __init__(self):
        self.days_normal = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
   
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
