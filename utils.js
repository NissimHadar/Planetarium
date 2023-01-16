function Radians(W) {
  return W * 0.01745329252;
}

function Degrees(W) {
  return W * 57.29577951;
}

function Unwind(W) {
  return W - 6.283185308 * Math.floor(W / 6.283185308);
}

function decimalDegreesToDecimalHours(decimalDegrees) {
  return decimalDegrees / 15.0;
}

function dmsToDegs(degrees, minutes, seconds) {
  var A = Math.abs(seconds) / 60.0;
  var B = (Math.abs(minutes) + A) / 60.0;
  var C = Math.abs(degrees) + B;
  
  if ((degrees < 0) || (minutes < 0) || (seconds < 0)) {
      return -C;
  } else {
      return C;
  }
}

function hoursMinutesSecondsToDecimalHours(hours, minutes, seconds) {
  var A = Math.abs(seconds) / 60.0;
  var B = (Math.abs(minutes) + A) / 60.0;
  var C = Math.abs(hours) + B;

  if ((hours < 0) || (minutes < 0) || (seconds < 0)) {
    return -C;
  } else {
    return C;
  }
}

function calendarDateToJulianDate(year, month, day) {
  var Y;
  var M;
  if (month < 3) {
    Y = year - 1;
    M = month + 12;
  } else {
    Y = year;
    M = month;
  }

  var A;
  var B;
  if (year > 1582) {
    A = Math.floor(Y / 100);
    B = 2 - A + Math.floor(A / 4);
  } else {
    if ((year == 1582) && (month > 10)) {
      A = Math.floor(Y / 100);
      B = 2 - A + Math.floor(A / 4);
    } else {
      if ((year == 1582) && (month == 10) && (day >= 15)) {
        A = Math.floor(Y / 100);
        B = 2 - A + Math.floor(A / 4);
      } else {
        B = 0;
      }
    }
  }

  var C;
  if (Y < 0) {
    C = Math.floor((365.25 * Y) - 0.75);
  } else {
    C = Math.floor(365.25 * Y);
  }

  var D = Math.floor(30.6001 * (M + 1));
  return B + C + D + day + 1720994.5;
}

function julianDateToCalendarDay(julianDate) {
  var I = Math.floor(julianDate + 0.5);
  var F = julianDate + 0.5 - I;
  var A = Math.floor((I - 1867216.25) / 36524.25);
  
  var B;
  if (I > 2299160) {
    B = I + 1 + A - Math.floor(A / 4);
  } else {
    B = I;
  }
  
  var C = B + 1524;
  var D = Math.floor((C - 122.1) / 365.25);
  var E = Math.floor(365.25 * D);
  var G = Math.floor((C - E) / 30.6001);
  return C - E + F - Math.floor(30.6001 * G);
}

// dayLightSavings - 1 for daylight savings, else 0
function localTimeToUniversalTime(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day) {
  var localCivilTime = hoursMinutesSecondsToDecimalHours(hours, minutes, seconds);
  var universalTime = localCivilTime - dayLightSavings - zoneCorrection;
  
  return universalTime;
}

function julianDateToCalendarMonth(julianDate) {
  var I = Math.floor(julianDate + 0.5);
  var F = julianDate + 0.5 - I;
  var A = Math.floor((I - 1867216.25) / 36524.25);

  var B;
  if (I > 2299160) {
    B = I + 1 + A - Math.floor(A / 4);
  } else {
    B = I;
  }
  
  var C = B + 1524;
  var D = Math.floor((C - 122.1) / 365.25);
  var E = Math.floor(365.25 * D);
  var G = Math.floor((C - E) / 30.6001);
  
  if (G < 13.5) {
    return G - 1;
  } else {
    return G - 13;
  }
}

function julianDateToCalendarYear(julianDate) {
  var I = Math.floor(julianDate + 0.5);
  var F = julianDate + 0.5 - I;
  var A = Math.floor((I - 1867216.25) / 36524.25);

  var B;    
  if (I > 2299160) {
    B = I + 1 + A - Math.floor(A / 4);
  } else {
    B = I;
  }
  
  var C = B + 1524;
  var D = Math.floor((C - 122.1) / 365.25);
  var E = Math.floor(365.25 * D);
  var G = Math.floor((C - E) / 30.6001);
  
  var H;
  if (G < 13.5) {
    H = G - 1;
  } else {
    H = G - 13;
  }
  
  if (H > 2.5) {
    return D - 4716;
  } else {
    return D - 4715;
  }
}

function localTimeToGreenwichDay(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day) {
  var A = hoursMinutesSecondsToDecimalHours(hours, minutes, seconds);
  var B = A - dayLightSavings - zoneCorrection;
  var C = day + (Math.floor(B / 24));
  var D = calendarDateToJulianDate(year, month, C);
  var E = julianDateToCalendarDay(D);
  
  return Math.floor(E);
}

function localTimeToGreenwichMonth(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day) {
  var A = hoursMinutesSecondsToDecimalHours(hours, minutes, seconds);
  var B = A - dayLightSavings - zoneCorrection;
  var C = day + (Math.floor(B / 24));
  var D = calendarDateToJulianDate(year, month, C);
  
  return julianDateToCalendarMonth(D);
}

function localTimeToGreenwichYear(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day) {
  var A = hoursMinutesSecondsToDecimalHours(hours, minutes, seconds);
  var B = A - dayLightSavings - zoneCorrection;
  var C = day + (Math.floor(B / 24));
  var D = calendarDateToJulianDate(year, month, C);
  
  return julianDateToCalendarYear(D);
}

function NutatObl(year, month, day) {
  var DJ = calendarDateToJulianDate(year, month, day) - 2415020.0;
  var T = DJ / 36525.0;
  var T2 = T * T;
  
  var A = 100.0021358 * T;
  var B = 360.0 * (A - Math.floor(A));
  
  var L1 = 279.6967 + 0.000303 * T2 + B;
  var L2 = 2.0 * Radians(L1);
  
  A = 1336.855231 * T;
  B = 360.0 * (A - Math.floor(A));
  
  var D1 = 270.4342 - 0.001133 * T2 + B;
  var D2 = 2.0 * Radians(D1);
  
  A = 99.99736056 * T;
  B = 360.0 * (A - Math.floor(A));
  
  var M1 = 358.4758 - 0.00015 * T2 + B;
  M1 = Radians(M1);
  
  A = 1325.552359 * T;
  B = 360.0 * (A - Math.floor(A));
  var M2 = 296.1046 + 0.009192 * T2 + B;
  var M2 = Radians(M2);
  
  A = 5.372616667 * T;
  B = 360.0 * (A - Math.floor(A));
  var N1 = 259.1833 + 0.002078 * T2 - B;
  var N1 = Radians(N1);
  var N2 = 2 * N1;

  var DDO = (9.21 + 0.00091 * T) * Math.cos(N1);
  DDO = DDO + (0.5522 - 0.00029 * T) * Math.cos(L2) - 0.0904 * Math.cos(N2);
  DDO = DDO + 0.0884 * Math.cos(D2) + 0.0216 * Math.cos(L2 + M1);
  DDO = DDO + 0.0183 * Math.cos(D2 - N1) + 0.0113 * Math.cos(D2 + M2);
  DDO = DDO - 0.0093 * Math.cos(L2 - M1) - 0.0066 * Math.cos(L2 - N1);

  return DDO / 3600.0;
}

function Obliq(year, month, day) {
    var A = calendarDateToJulianDate(year, month, day);
    var B = A - 2415020.0;
    var C = (B / 36525.0) - 1.0;
    var D = C * (46.815 + C * (0.0006 - (C * 0.00181)));
    var E = D / 3600.0;
    return 23.43929167 - E + NutatObl(year, month, day);
}

function eclipticCoordsToRightAscension(correctedLongitude, ELM, ELS, moonEclipticLatitude, BM, BS, greenwichYear, greenwichMonth, greenwichDay) {
  var A = Radians(dmsToDegs(correctedLongitude, ELM, ELS));
  var B = Radians(dmsToDegs(moonEclipticLatitude, BM, BS));
  var C = Radians(Obliq(greenwichYear, greenwichMonth, greenwichDay));
  var D = Math.sin(A) * Math.cos(C) - Math.tan(B) * Math.sin(C);
  var E = Math.cos(A);
  var F = Degrees(Math.atan2(D, E));
  
  return F - 360.0 * Math.floor(F / 360.0);
}

function eclipticCoordsToDeclination(
  longitudeDegrees, longitudeMinutes, longitudeSeconds, 
  latitudeDegrees, latitudeMinutes, latitudeSeconds, 
  greenwichYear, greenwichMonth, greenwichDay
) {
  var A = Radians(dmsToDegs(longitudeDegrees, longitudeMinutes, longitudeSeconds));
  var B = Radians(dmsToDegs(latitudeDegrees, latitudeMinutes, latitudeSeconds));
  var C = Radians(Obliq(greenwichYear, greenwichMonth, greenwichDay));
  var D = Math.sin(B) * Math.cos(C) + Math.cos(B) * Math.sin(C) * Math.sin(A);
  return Degrees(Math.asin(D));
}

function TrueAnomaly(AM, EC) {
  var TP = 6.283185308;
  var M = AM - TP * Math.floor(AM / TP);
  AE = M;
        
  while(true) {
    var D = AE - (EC * Math.sin(AE)) - M;
        
    if (Math.abs(D) < 0.000001) {
      var A = Math.sqrt((1.0 + EC) / (1.0 - EC)) * Math.tan(AE / 2.0);
      var AT = 2.0 * Math.atan(A);
      return AT;
    }
    
    D = D / (1.0 - (EC * Math.cos(AE)));
    AE = AE - D;
  }
}

function EccentricAnomaly(AM, EC) {
  var TP = 6.283185308;
  var M = AM - TP * Math.floor(AM / TP);
  AE = M;
        
  while (true) {;
    var D = AE - (EC * Math.sin(AE)) - M;
    if (Math.abs(D) < 0.000001) {
      return AE;
    }
        
    D = D / (1.0 - (EC * Math.cos(AE)));
    AE = AE - D;
  }
}

function universalTimetoGreenwichSiderealTime(universalTime, greenwichYear, greenwichMonth, greenwichDay) {
    var julianDate = calendarDateToJulianDate(greenwichYear, greenwichMonth, greenwichDay);
    var s = julianDate - 2451545;
    var t = s / 36525;
    
    var t0 = 6.697374558 + 2400.051336 * t + 0.000025862 * t * t;
    t0 = t0 - 24 * Math.floor(t0 / 24);
    
    var a = universalTime * 1.002737909;
    
    var gst = t0 + a;
    gst = gst - 24 * Math.floor(gst / 24);
    
    return gst
}

function greenwichSideralTimeToLocalSideralTime(decimalHours, longitude_degs) {
    var offset = longitude_degs / 15.0;
    var lst = decimalHours + offset;
    
    return lst;
}

// all parameters are local values
function rightAscensionToHourAngle(rightAscension, year, month, day, dayLightSavings, zoneCorrection, hours, minutes, seconds, longitude_degs) {
    var universalTime = localTimeToUniversalTime(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    
    var greenwichYear  = localTimeToGreenwichYear (hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var greenwichMonth = localTimeToGreenwichMonth(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var greenwichDay   = localTimeToGreenwichDay  (hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    
    var gst = universalTimetoGreenwichSiderealTime(universalTime, greenwichYear, greenwichMonth, greenwichDay);
    var lst = greenwichSideralTimeToLocalSideralTime(gst, longitude_degs);
    
    var h1 = lst - rightAscension;
    
    if (h1 < 0) {
        return h1 + 24.0;
    } else {
        return h1;
    }
}

function equatorialToHorizontalCoordinatesAzimuth(hourAngle, declination, latitude_degs) {
    var sinA = Math.sin(Radians(declination)) * Math.sin(Radians(latitude_degs)) + 
               Math.cos(Radians(declination)) * Math.cos(Radians(latitude_degs)) * Math.cos(Radians(hourAngle));
               
    var A_degs = Degrees(Math.asin(sinA));
    
    var x = Math.sin(Radians(declination)) - Math.sin(Radians(latitude_degs)) * sinA;
    var y = -Math.cos(Radians(declination)) * Math.cos(Radians(latitude_degs)) * Math.sin(Radians(hourAngle));
    
    var A = Math.atan2(y, x);
    var B = Degrees(A);
    
    return B - 360.0 * Math.floor(B / 360.0);
}

function equatorialToHorizontalCoordinatesAltitude(hourAngle, declination, latitude_degs) {
    var sinA = Math.sin(Radians(declination)) * Math.sin(Radians(latitude_degs)) + 
               Math.cos(Radians(declination)) * Math.cos(Radians(latitude_degs)) * Math.cos(Radians(hourAngle));
               
    return Degrees(Math.asin(sinA));
}
