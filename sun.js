Script.include("./utils.js");

function sunLongitude(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day) {
    var AA = localTimeToGreenwichDay(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var BB = localTimeToGreenwichMonth(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var CC = localTimeToGreenwichYear(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);
    var UT = localTimeToUniversalTime(hours, minutes, seconds, dayLightSavings, zoneCorrection, year, month, day);

    var DJ = calendarDateToJulianDate(CC, BB, AA) - 2415020;
    var T = (DJ / 36525.0) + (UT / 876600.0)
    var T2 = T * T;

    var A = 100.0021359 * T;
    var B = 360.0 * (A - Math.floor(A));
    var L = 279.69668 + 0.0003025 * T2 + B;
    
    A = 99.99736042 * T;
    B = 360.0 * (A - Math.floor(A));
    
    var M1 = 358.47583 - (0.00015 + 0.0000033 * T) * T2 + B;
    var EC = 0.01675104 - 0.0000418 * T - 0.000000126 * T2;

    var AM = Radians(M1)
    var AT = TrueAnomaly(AM, EC);
    var AE = EccentricAnomaly(AM, EC);

    A = 62.55209472 * T;
    B = 360.0 * (A - Math.floor(A));
    
    var A1 = Radians(153.23 + B);
    A = 125.1041894 * T;
    B = 360.0 * (A - Math.floor(A));
    
    var B1 = Radians(216.57 + B);
    A = 91.56766028 * T;
    B = 360.0 * (A - Math.floor(A));
    
    var C1 = Radians(312.69 + B);
    A = 1236.853095 * T;
    B = 360.0 * (A - Math.floor(A));
    
    var D1 = Radians(350.74 - 0.00144 * T2 + B);
    var E1 = Radians(231.19 + 20.2 * T);
    
    A = 183.1353208 * T;
    B = 360.0 * (A - Math.floor(A));
    
    var H1 = Radians(353.4 + B);

    var D2 = 0.00134 * Math.cos(A1) + 0.00154 * Math.cos(B1) + 0.002 * Math.cos(C1);
    D2 = D2 + 0.00179 * Math.sin(D1) + 0.00178 * Math.sin(E1);
    
    var D3 = 0.00000543 * Math.sin(A1) + 0.00001575 * Math.sin(B1);
    D3 = D3 + 0.00001627 * Math.sin(C1) + 0.00003076 * Math.cos(D1);
    D3 = D3 + 0.00000927 * Math.sin(H1);

    var SR = AT + Radians(L - M1 + D2);
    var TP = 6.283185308
    SR = SR - TP * Math.floor(SR / TP);
    
    return Degrees(SR);
}

function sunRightAscension(
    correctedLongitudeDegs, correctedLongitudeMins, correctedLongitudeSecs,
    sunEclipticLatitudeDegs, sunEclipticLatitudeMins, sunEclipticLatitudeSecs,
    greenwichYear, greenwichMonth, greenwichDay
) {
    return decimalDegreesToDecimalHours(
        eclipticCoordsToRightAscension(
            correctedLongitudeDegs, correctedLongitudeMins, correctedLongitudeSecs,
            sunEclipticLatitudeDegs, sunEclipticLatitudeMins, sunEclipticLatitudeSecs,
            greenwichYear, greenwichMonth, greenwichDay
        )
    );
}
