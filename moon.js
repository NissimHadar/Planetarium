Script.include("./utils.h");

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

function moonRightAscension(
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
