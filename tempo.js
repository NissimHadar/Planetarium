//
//  planetarium.js
//  scripts/developer
//
//  Created by Nissim Hadar on 2018/2/15.
//  Copyright 2013 High Fidelity, Inc.
//
//  Distributed under the Apache License, Version 2.0.
//  See the accompanying file LICENSE or http://www.apache.org/licenses/LICENSE-2.0.html
//
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
function RightAscensionToHourAngle(rightAscension, year, month, day, dayLightSavings, zoneCorrection, hours, minutes, seconds, longitude_degs) {
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

function MoonLong(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day) {
    var UT = localTimeToUniversalTime(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var GD = localTimeToGreenwichDay(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var GM = localTimeToGreenwichMonth(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var GY = localTimeToGreenwichYear(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var DJ = calendarDateToJulianDate(GY, GM, GD) - 2415020;
    var T = (DJ / 36525.0) + (UT / 876600.0);
    var T2 = T * T;

    var M1 = 27.32158213;
    var M2 = 365.2596407;
    var M3 = 27.55455094;
    var M4 = 29.53058868;
    var M5 = 27.21222039;
    var M6 = 6798.363307;
    
    var Q = calendarDateToJulianDate(GY, GM, GD) - 2415020 + (UT / 24.0);
    
    M1 = Q / M1;
    M2 = Q / M2;
    M3 = Q / M3;
    M4 = Q / M4;
    M5 = Q / M5;
    M6 = Q / M6;
    
    M1 = 360.0 * (M1 - Math.floor(M1));
    M2 = 360.0 * (M2 - Math.floor(M2));
    M3 = 360.0 * (M3 - Math.floor(M3));
    M4 = 360.0 * (M4 - Math.floor(M4));
    M5 = 360.0 * (M5 - Math.floor(M5));
    M6 = 360.0 * (M6 - Math.floor(M6));

    var ML = 270.434164 + M1 - (0.001133 - 0.0000019 * T) * T2;
    var MS = 358.475833 + M2 - (0.00015 + 0.0000033 * T) * T2;
    var MD = 296.104608 + M3 + (0.009192 + 0.0000144 * T) * T2;
    var ME1 = 350.737486 + M4 - (0.001436 - 0.0000019 * T) * T2;
    var MF = 11.250889 + M5 - (0.003211 + 0.0000003 * T) * T2;
    var NA = 259.183275 - M6 + (0.002078 + 0.0000022 * T) * T2;
    
    var A = Radians(51.2 + 20.2 * T);
    var S1 = Math.sin(A);
    
    var S2 = Math.sin(Radians(NA));
    
    var B = 346.56 + (132.87 - 0.0091731 * T) * T;
    var S3 = 0.003964 * Math.sin(Radians(B));
    
    var C = Radians(NA + 275.05 - 2.3 * T);
    
    var S4 = Math.sin(C);
    
    ML = ML + 0.000233 * S1 + S3 + 0.001964 * S2;
    MS = MS - 0.001778 * S1;
    MD = MD + 0.000817 * S1 + S3 + 0.002541 * S2;
    MF = MF + S3 - 0.024691 * S2 - 0.004328 * S4;
    ME1 = ME1 + 0.002011 * S1 + S3 + 0.001964 * S2;
    
    var E = 1.0 - (0.002495 + 0.00000752 * T) * T;
    E2 = E * E;
    
    ML = Radians(ML);
    MS = Radians(MS);
    NA = Radians(NA);
    ME1 = Radians(ME1);
    MF = Radians(MF);
    MD = Radians(MD);

    var L = 6.28875 * Math.sin(MD) + 1.274018 * Math.sin(2 * ME1 - MD);
    L = L + 0.658309 * Math.sin(2 * ME1) + 0.213616 * Math.sin(2 * MD);
    L = L - E * 0.185596 * Math.sin(MS) - 0.114336 * Math.sin(2 * MF);
    L = L + 0.058793 * Math.sin(2 * (ME1 - MD));
    L = L + 0.057212 * E * Math.sin(2 * ME1 - MS - MD) + 0.05332 * Math.sin(2 * ME1 + MD);
    L = L + 0.045874 * E * Math.sin(2 * ME1 - MS) + 0.041024 * E * Math.sin(MD - MS);
    L = L - 0.034718 * Math.sin(ME1) - E * 0.030465 * Math.sin(MS + MD);
    L = L + 0.015326 * Math.sin(2 * (ME1 - MF)) - 0.012528 * Math.sin(2 * MF + MD);
    L = L - 0.01098 * Math.sin(2 * MF - MD) + 0.010674 * Math.sin(4 * ME1 - MD);
    L = L + 0.010034 * Math.sin(3 * MD) + 0.008548 * Math.sin(4 * ME1 - 2 * MD);
    L = L - E * 0.00791 * Math.sin(MS - MD + 2 * ME1) - E * 0.006783 * Math.sin(2 * ME1 + MS);
    L = L + 0.005162 * Math.sin(MD - ME1) + E * 0.005 * Math.sin(MS + ME1);
    L = L + 0.003862 * Math.sin(4 * ME1) + E * 0.004049 * Math.sin(MD - MS + 2 * ME1);
    L = L + 0.003996 * Math.sin(2 * (MD + ME1)) + 0.003665 * Math.sin(2 * ME1 - 3 * MD);
    L = L + E * 0.002695 * Math.sin(2 * MD - MS) + 0.002602 * Math.sin(MD - 2 * (MF + ME1));
    L = L + E * 0.002396 * Math.sin(2 * (ME1 - MD) - MS) - 0.002349 * Math.sin(MD + ME1);
    L = L + E2 * 0.002249 * Math.sin(2 * (ME1 - MS)) - E * 0.002125 * Math.sin(2 * MD + MS);
    L = L - E2 * 0.002079 * Math.sin(2 * MS) + E2 * 0.002059 * Math.sin(2 * (ME1 - MS) - MD);
    L = L - 0.001773 * Math.sin(MD + 2 * (ME1 - MF)) - 0.001595 * Math.sin(2 * (MF + ME1));;
    L = L + E * 0.00122 * Math.sin(4 * ME1 - MS - MD) - 0.00111 * Math.sin(2 * (MD + MF))
    L = L + 0.000892 * Math.sin(MD - 3 * ME1) - E * 0.000811 * Math.sin(MS + MD + 2 * ME1);
    L = L + E * 0.000761 * Math.sin(4 * ME1 - MS - 2 * MD);
    L = L + E2 * 0.000704 * Math.sin(MD - 2 * (MS + ME1));
    L = L + E * 0.000693 * Math.sin(MS - 2 * (MD - ME1));
    L = L + E * 0.000598 * Math.sin(2 * (ME1 - MF) - MS);
    L = L + 0.00055 * Math.sin(MD + 4 * ME1) + 0.000538 * Math.sin(4 * MD);
    L = L + E * 0.000521 * Math.sin(4 * ME1 - MS) + 0.000486 * Math.sin(2 * MD - ME1);
    L = L + E2 * 0.000717 * Math.sin(MD - 2 * MS);
    
    var MM = Unwind(ML + Radians(L));

    return Degrees(MM);
}

function MoonLat(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day) {
    var UT = localTimeToUniversalTime(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var GD = localTimeToGreenwichDay(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var GM = localTimeToGreenwichMonth(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var GY = localTimeToGreenwichYear(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var DJ = calendarDateToJulianDate(year, month, day) - 2415020;
    var T = (DJ / 36525.0) + (UT / 876600.0);
    var T2 = T * T;

    var M1 = 27.32158213;
    var M2 = 365.2596407;
    var M3 = 27.55455094;
    var M4 = 29.53058868;
    var M5 = 27.21222039;
    var M6 = 6798.363307;
    
    var Q = calendarDateToJulianDate(year, month, day) - 2415020 + (UT / 24.0);

    M1 = Q / M1;
    M2 = Q / M2;
    M3 = Q / M3;
    M4 = Q / M4;
    M5 = Q / M5;
    M6 = Q / M6;
    
    M1 = 360.0 * (M1 - Math.floor(M1));
    M2 = 360.0 * (M2 - Math.floor(M2));
    M3 = 360.0 * (M3 - Math.floor(M3));
    M4 = 360.0 * (M4 - Math.floor(M4));
    M5 = 360.0 * (M5 - Math.floor(M5));
    M6 = 360.0 * (M6 - Math.floor(M6));

    var ML = 270.434164 + M1 - (0.001133 - 0.0000019 * T) * T2;
    var MS = 358.475833 + M2 - (0.00015 + 0.0000033 * T) * T2;
    var MD = 296.104608 + M3 + (0.009192 + 0.0000144 * T) * T2;
    var ME1 = 350.737486 + M4 - (0.001436 - 0.0000019 * T) * T2;
    var MF = 11.250889 + M5 - (0.003211 + 0.0000003 * T) * T2;
    var NA = 259.183275 - M6 + (0.002078 + 0.0000022 * T) * T2;
    
    var A = Radians(51.2 + 20.2 * T);
    var S1 = Math.sin(A);
    var S2 = Math.sin(Radians(NA));
    
    var B = 346.56 + (132.87 - 0.0091731 * T) * T;
    var S3 = 0.003964 * Math.sin(Radians(B));
    
    var C = Radians(NA + 275.05 - 2.3 * T);
    var S4 = Math.sin(C);
    
    ML = ML + 0.000233 * S1 + S3 + 0.001964 * S2;
    MS = MS - 0.001778 * S1;
    MD = MD + 0.000817 * S1 + S3 + 0.002541 * S2;
    MF = MF + S3 - 0.024691 * S2 - 0.004328 * S4;
    ME1 = ME1 + 0.002011 * S1 + S3 + 0.001964 * S2;
    
    var E = 1.0 - (0.002495 + 0.00000752 * T) * T;
    var E2 = E * E;
    
    ML = Radians(ML);
    MS = Radians(MS);
    NA = Radians(NA);
    ME1 = Radians(ME1);
    MF = Radians(MF);
    MD = Radians(MD);

    var G = 5.128189 * Math.sin(MF) + 0.280606 * Math.sin(MD + MF);
    G = G + 0.277693 * Math.sin(MD - MF) + 0.173238 * Math.sin(2 * ME1 - MF);
    G = G + 0.055413 * Math.sin(2 * ME1 + MF - MD) + 0.046272 * Math.sin(2 * ME1 - MF - MD);
    G = G + 0.032573 * Math.sin(2 * ME1 + MF) + 0.017198 * Math.sin(2 * MD + MF);
    G = G + 0.009267 * Math.sin(2 * ME1 + MD - MF) + 0.008823 * Math.sin(2 * MD - MF);
    G = G + E * 0.008247 * Math.sin(2 * ME1 - MS - MF) + 0.004323 * Math.sin(2 * (ME1 - MD) - MF);
    G = G + 0.0042 * Math.sin(2 * ME1 + MF + MD) + E * 0.003372 * Math.sin(MF - MS - 2 * ME1);
    G = G + E * 0.002472 * Math.sin(2 * ME1 + MF - MS - MD);
    G = G + E * 0.002222 * Math.sin(2 * ME1 + MF - MS);
    G = G + E * 0.002072 * Math.sin(2 * ME1 - MF - MS - MD);
    G = G + E * 0.001877 * Math.sin(MF - MS + MD) + 0.001828 * Math.sin(4 * ME1 - MF - MD);
    G = G - E * 0.001803 * Math.sin(MF + MS) - 0.00175 * Math.sin(3 * MF);
    G = G + E * 0.00157 * Math.sin(MD - MS - MF) - 0.001487 * Math.sin(MF + ME1);
    G = G - E * 0.001481 * Math.sin(MF + MS + MD) + E * 0.001417 * Math.sin(MF - MS - MD);
    G = G + E * 0.00135 * Math.sin(MF - MS) + 0.00133 * Math.sin(MF - ME1);
    G = G + 0.001106 * Math.sin(MF + 3 * MD) + 0.00102 * Math.sin(4 * ME1 - MF);
    G = G + 0.000833 * Math.sin(MF + 4 * ME1 - MD) + 0.000781 * Math.sin(MD - 3 * MF);
    G = G + 0.00067 * Math.sin(MF + 4 * ME1 - 2 * MD) + 0.000606 * Math.sin(2 * ME1 - 3 * MF);
    G = G + 0.000597 * Math.sin(2 * (ME1 + MD) - MF);
    G = G + E * 0.000492 * Math.sin(2 * ME1 + MD - MS - MF) + 0.00045 * Math.sin(2 * (MD - ME1) - MF);
    G = G + 0.000439 * Math.sin(3 * MD - MF) + 0.000423 * Math.sin(MF + 2 * (ME1 + MD));
    G = G + 0.000422 * Math.sin(2 * ME1 - MF - 3 * MD) - E * 0.000367 * Math.sin(MS + MF + 2 * ME1 - MD);
    G = G - E * 0.000353 * Math.sin(MS + MF + 2 * ME1) + 0.000331 * Math.sin(MF + 4 * ME1);
    G = G + E * 0.000317 * Math.sin(2 * ME1 + MF - MS + MD);
    G = G + E2 * 0.000306 * Math.sin(2 * (ME1 - MS) - MF) - 0.000283 * Math.sin(MD + 3 * MF);
    
    var W1 = 0.0004664 * Math.cos(NA);
    var W2 = 0.0000754 * Math.cos(C);
    var BM = Radians(G) * (1.0 - W1 - W2);

    return Degrees(BM);
}

function nutatLong(year, month, day) {
    var DJ = calendarDateToJulianDate(year, month, day) - 2415020;
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
    N1 = Radians(N1);
    var N2 = 2 * N1;

    var DP = (-17.2327 - 0.01737 * T) * Math.sin(N1);
    DP = DP + (-1.2729 - 0.00013 * T) * Math.sin(L2) + 0.2088 * Math.sin(N2);
    DP = DP - 0.2037 * Math.sin(D2) + (0.1261 - 0.00031 * T) * Math.sin(M1);
    DP = DP + 0.0675 * Math.sin(M2) - (0.0497 - 0.00012 * T) * Math.sin(L2 + M1);
    DP = DP - 0.0342 * Math.sin(D2 - N1) - 0.0261 * Math.sin(D2 + M2);
    DP = DP + 0.0214 * Math.sin(L2 - M1) - 0.0149 * Math.sin(L2 - D2 + M2);
    DP = DP + 0.0124 * Math.sin(L2 - N1) + 0.0114 * Math.sin(D2 - M2);

    return DP / 3600.0;
  }

  function moonHorizontalParallax(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day) {
    var UT = localTimeToUniversalTime(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var GD = localTimeToGreenwichDay(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var GM = localTimeToGreenwichMonth(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var GY = localTimeToGreenwichYear(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var DJ = calendarDateToJulianDate(year, month, day) - 2415020;
    var T = (DJ / 36525.0) + (UT / 876600.0);
    var T2 = T * T;

    var M1 = 27.32158213;
    var M2 = 365.2596407;
    var M3 = 27.55455094;
    var M4 = 29.53058868;
    var M5 = 27.21222039;
    var M6 = 6798.363307;
    
    var Q = calendarDateToJulianDate(year, month, day) - 2415020 + (UT / 24.0);
    
    M1 = Q / M1;
    M2 = Q / M2;
    M3 = Q / M3;
    M4 = Q / M4;
    M5 = Q / M5;
    M6 = Q / M6;
    
    M1 = 360.0 * (M1 - Math.floor(M1));
    M2 = 360.0 * (M2 - Math.floor(M2));
    M3 = 360.0 * (M3 - Math.floor(M3));
    M4 = 360.0 * (M4 - Math.floor(M4));
    M5 = 360.0 * (M5 - Math.floor(M5));
    M6 = 360.0 * (M6 - Math.floor(M6));

    var ML = 270.434164 + M1 - (0.001133 - 0.0000019 * T) * T2;
    var MS = 358.475833 + M2 - (0.00015 + 0.0000033 * T) * T2;
    var MD = 296.104608 + M3 + (0.009192 + 0.0000144 * T) * T2;
    var ME1 = 350.737486 + M4 - (0.001436 - 0.0000019 * T) * T2;
    var MF = 11.250889 + M5 - (0.003211 + 0.0000003 * T) * T2;
    var NA = 259.183275 - M6 + (0.002078 + 0.0000022 * T) * T2;
    
    var A = Radians(51.2 + 20.2 * T);
    var S1 = Math.sin(A);
    var S2 = Math.sin(Radians(NA));
    
    var B = 346.56 + (132.87 - 0.0091731 * T) * T;
    var S3 = 0.003964 * Math.sin(Radians(B));
    
    var C = Radians(NA + 275.05 - 2.3 * T);
    var S4 = Math.sin(C);
    
    ML = ML + 0.000233 * S1 + S3 + 0.001964 * S2;
    MS = MS - 0.001778 * S1;
    MD = MD + 0.000817 * S1 + S3 + 0.002541 * S2;
    MF = MF + S3 - 0.024691 * S2 - 0.004328 * S4;
    ME1 = ME1 + 0.002011 * S1 + S3 + 0.001964 * S2;
    var E = 1.0 - (0.002495 + 0.00000752 * T) * T;
    var E2 = E * E;
    
    ML = Radians(ML);
    MS = Radians(MS);
    NA = Radians(NA);
    ME1 = Radians(ME1);
    MF = Radians(MF);
    MD = Radians(MD);

    var PM = 0.950724 + 0.051818 * Math.cos(MD) + 0.009531 * Math.cos(2 * ME1 - MD);
    PM = PM + 0.007843 * Math.cos(2 * ME1) + 0.002824 * Math.cos(2 * MD);
    PM = PM + 0.000857 * Math.cos(2 * ME1 + MD) + E * 0.000533 * Math.cos(2 * ME1 - MS);
    PM = PM + E * 0.000401 * Math.cos(2 * ME1 - MD - MS);
    PM = PM + E * 0.00032 * Math.cos(MD - MS) - 0.000271 * Math.cos(ME1);
    PM = PM - E * 0.000264 * Math.cos(MS + MD) - 0.000198 * Math.cos(2 * MF - MD);
    PM = PM + 0.000173 * Math.cos(3 * MD) + 0.000167 * Math.cos(4 * ME1 - MD);
    PM = PM - E * 0.000111 * Math.cos(MS) + 0.000103 * Math.cos(4 * ME1 - 2 * MD);
    PM = PM - 0.000084 * Math.cos(2 * MD - 2 * ME1) - E * 0.000083 * Math.cos(2 * ME1 + MS);
    PM = PM + 0.000079 * Math.cos(2 * ME1 + 2 * MD) + 0.000072 * Math.cos(4 * ME1);
    PM = PM + E * 0.000064 * Math.cos(2 * ME1 - MS + MD) - E * 0.000063 * Math.cos(2 * ME1 + MS - MD);
    PM = PM + E * 0.000041 * Math.cos(MS + ME1) + E * 0.000035 * Math.cos(2 * MD - MS);
    PM = PM - 0.000033 * Math.cos(3 * MD - 2 * ME1) - 0.00003 * Math.cos(MD + ME1);
    PM = PM - 0.000029 * Math.cos(2 * (MF - ME1)) - E * 0.000029 * Math.cos(2 * MD + MS);
    PM = PM + E2 * 0.000026 * Math.cos(2 * (ME1 - MS)) - 0.000023 * Math.cos(2 * (MF - ME1) + MD);
    PM = PM + E * 0.000019 * Math.cos(4 * ME1 - MS - MD);
    
    return PM;
}

function EarthMoonDistance(moonHorizontalParallax) {
    var earthRadius = 6378.14;
    return earthRadius / Math.sin(Radians(moonHorizontalParallax));
}

function MoonRightAscension(
    correctedLongitudeDegs, correctedLongitudeMins, correctedLongitudeSecs,
    moonEclipticLatitudeDegs, moonEclipticLatitudeMins, moonEclipticLatitudeSecs,
    greenwichYear, greenwichMonth, greenwichDay
) {
    return decimalDegreesToDecimalHours(
        eclipticCoordsToRightAscension(
            correctedLongitudeDegs, correctedLongitudeMins, correctedLongitudeSecs,
            moonEclipticLatitudeDegs, moonEclipticLatitudeMins, moonEclipticLatitudeSecs,
            greenwichYear, greenwichMonth, greenwichDay
        )
    );
}

(function() {
    function parseJSON(json) {
        try {
            return JSON.parse(json);
        } catch (e) {
            return undefined;
        }
    }
    
    // Address of resources - https://hifi-content.s3.amazonaws.com/nissim/planetarium/
    
    this.preload = function(entityID) { // We don't have the entityID before the preload
        // Define user data
        var userDataProperties = {
            "userData": "{ \"latitude\": 47.0, \"longitude\": 122.0, \"year\": 2018, \"month\": 6, \"day\": 13, \"hour\": 20, \"minute\": 0 }"
        };

        Entities.editEntity(entityID, userDataProperties);
  
        // Zero the head position
        MyAvatar.bodyYaw   = 0.0;
        MyAvatar.bodyPitch = 0.0;
        MyAvatar.bodyRoll  = 0.0;
        MyAvatar.headYaw   = 0.0;
        MyAvatar.headPitch = 0.0;
        MyAvatar.headRoll  = 0.0;
        
        MyAvatar.position = { x: 0.0, y: 0.0, z: 0.0 };
        
        var listOfEntities = Entities.findEntities(MyAvatar.position, 20000);
        var moon;
        for (var i = 0; i < listOfEntities.length; ++i) {
            var properties =  Entities.getEntityProperties(listOfEntities[i]);
            if (properties.name === "Moon") {
                moon = properties;
                break;
            }
        }
        
        var dataRead = false;
        var hour   = 0;
        var minute = 0;

        Script.setInterval(
            function() {
                // Read back user data
                var userData = Entities.getEntityProperties(entityID, 'userData').userData;
                var data = parseJSON(userData);
                
                var latitude_degs  = data.latitude;
                var longitude_degs = data.longitude;
                var year           = data.year;
                var month          = data.month;
                var day            = data.day;
                
                if (!dataRead) {
                    dataRead = true;
                    hour           = data.hour;
                    minute         = data.minute;
                } else {
                    minute += 10;
                    if (minute > 60.0) {
                        hour += 1;
                        minute -= 60;
                        if (hour >= 24) {
                            hour = 0;
                        }
                    }
                }

                // Orient zone (i.e. star map) according to lat/lon and date/time
                var timeZoneCorrection = -8;
                var daylightSavings    =  0;
                var hourAngle = RightAscensionToHourAngle(
                    0.0, 
                    year, month, day, 
                    daylightSavings, timeZoneCorrection, 
                    hour, minute, 0.0, 
                    longitude_degs
                );
                 
                var zeroPointAzimuth_degs  = equatorialToHorizontalCoordinatesAzimuth (hourAngle, 0.0, latitude_degs);
                var zeroPointAltitude_degs = equatorialToHorizontalCoordinatesAltitude(hourAngle, 0.0, latitude_degs);
                var zeroPointAzimuth_rads  = Radians(zeroPointAzimuth_degs);
                var zeroPointAltitude_rads = Radians(zeroPointAltitude_degs);
               
                // Star map position
                Entities.editEntity(entityID, { rotation: Quat.fromPitchYawRollDegrees(zeroPointAltitude_degs, zeroPointAzimuth_degs, 0.0) });
                
                // Moon
                hour = 0;
                minute = 0;
                second = 0;
                dayLightSavings = 0;
                zoneCorrection = 0;
                year = 2003;
                month = 9;
                day = 1;
                var moonLongitude_degs = MoonLong(hour, minute, second, dayLightSavings, zoneCorrection, year, month, day);
                var moonLatitude_degs  = MoonLat (hour, minute, second, dayLightSavings, zoneCorrection, year, month, day);

                var correctedLongitude_degs = moonLongitude_degs + nutatLong(year, month, day);

                var greenwichYear  = localTimeToGreenwichYear (hour, minute, second, dayLightSavings, zoneCorrection, year, month, day);
                var greenwichMonth = localTimeToGreenwichMonth(hour, minute, second, dayLightSavings, zoneCorrection, year, month, day);
                var greenwichDay   = localTimeToGreenwichDay  (hour, minute, second, dayLightSavings, zoneCorrection, year, month, day);
                
                var moonRightAscension_hours = MoonRightAscension(correctedLongitude_degs, 0, 0,  moonLatitude_degs, 0, 0,
                                                                  greenwichYear, greenwichMonth, greenwichDay);
                                                            
                var moonDeclination_hours = eclipticCoordsToDeclination(correctedLongitude_degs, 0, 0, moonLatitude_degs, 0, 0,
                                                                        greenwichYear, greenwichMonth, greenwichDay);
                print(moonDeclination_hours);
                // This is just for fun
                ////Entities.editEntity(moon.id, { rotation: Quat.fromPitchYawRollDegrees(solarAltitude_degs, solarAzimuth_degs, 0.0) }); 
            },

            1000       // Run every 1 seconds!!!
        );
    };
});