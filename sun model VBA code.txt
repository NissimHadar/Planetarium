DefDbl A-Z

Function CDJD(Day As Double, Month As Double, Year As Double) As Double

    If (Month < 3) Then
        Y = Year - 1
        M = Month + 12
    Else
        Y = Year
        M = Month
    End If
    
    If (Year > 1582) Then
        A = Fix(Y / 100)
        B = 2 - A + Fix(A / 4)
    Else
     If (Year = 1582) And (Month > 10) Then
        A = Fix(Y / 100)
        B = 2 - A + Fix(A / 4)
     Else
      If (Year = 1582) And (Month = 10) And (Day >= 15) Then
        A = Fix(Y / 100)
        B = 2 - A + Fix(A / 4)
      Else
        B = 0
      End If
     End If
    End If
          
    If (Y < 0) Then
        C = Fix((365.25 * Y) - 0.75)
    Else
        C = Fix(365.25 * Y)
    End If
    
    D = Fix(30.6001 * (M + 1))
    CDJD = B + C + D + Day + 1720994.5
    
End Function

Function JDCDay(JD As Double) As Double
    I = Fix(JD + 0.5)
    F = JD + 0.5 - I
    A = Fix((I - 1867216.25) / 36524.25)
    
    If (I > 2299160) Then
        B = I + 1 + A - Fix(A / 4)
    Else
        B = I
    End If
    
    C = B + 1524
    D = Fix((C - 122.1) / 365.25)
    E = Fix(365.25 * D)
    G = Fix((C - E) / 30.6001)
    JDCDay = C - E + F - Fix(30.6001 * G)
    
End Function

Function JDCMonth(JD As Double) As Double

    I = Fix(JD + 0.5)
    F = JD + 0.5 - I
    A = Fix((I - 1867216.25) / 36524.25)
    
    If (I > 2299160) Then
        B = I + 1 + A - Fix(A / 4)
    Else
        B = I
    End If
    
    C = B + 1524
    D = Fix((C - 122.1) / 365.25)
    E = Fix(365.25 * D)
    G = Fix((C - E) / 30.6001)
    
    If (G < 13.5) Then
        JDCMonth = G - 1
    Else
        JDCMonth = G - 13
    End If
    
End Function
Function JDCYear(JD As Double) As Double

    I = Fix(JD + 0.5)
    F = JD + 0.5 - I
    A = Fix((I - 1867216.25) / 36524.25)
    
    If (I > 2299160) Then
        B = I + 1 + A - Fix(A / 4)
    Else
        B = I
    End If
    
    C = B + 1524
    D = Fix((C - 122.1) / 365.25)
    E = Fix(365.25 * D)
    G = Fix((C - E) / 30.6001)
    
    If (G < 13.5) Then
        H = G - 1
    Else
        H = G - 13
    End If
    
    If (H > 2.5) Then
        JDCYear = D - 4716
    Else
        JDCYear = D - 4715
    End If
    
End Function

Function FDOW(JD As Double) As String

    J = Fix(JD - 0.5) + 0.5
    N = (J + 1.5) Mod 7
    
    If (N = 0) Then
     FDOW = "Sunday"
     ElseIf (N = 1) Then
      FDOW = "Monday"
      ElseIf (N = 2) Then
       FDOW = "Tuesday"
       ElseIf (N = 3) Then
        FDOW = "Wednesday"
        ElseIf (N = 4) Then
         FDOW = "Thursday"
         ElseIf (N = 5) Then
          FDOW = "Friday"
          ElseIf (N = 6) Then
           FDOW = "Saturday"
           Else
            FDOW = "Unknown"
           End If

End Function

Function HMSDH(H As Double, M As Double, S As Double) As Double

    A = Abs(S) / 60
    B = (Abs(M) + A) / 60
    C = Abs(H) + B
    
    If ((H < 0) Or (M < 0) Or (S < 0)) Then
        HMSDH = -C
    Else
        HMSDH = C
    End If
    
End Function

Function DHHour(DH As Double) As Double

    A = Abs(DH)
    B = A * 3600
    C = Round(B - 60 * Fix(B / 60), 2)
    
    If (C = 60) Then
        D = 0
        E = B + 60
    Else
        D = C
        E = B
    End If
    
    If (DH < 0) Then
        DHHour = -Fix(E / 3600)
    Else
        DHHour = Fix(E / 3600)
    End If
    
End Function

Function DHMin(DH As Double) As Double

    A = Abs(DH)
    B = A * 3600
    C = Round(B - 60 * Fix(B / 60), 2)
    
    If (C = 60) Then
        D = 0
        E = B + 60
    Else
        D = C
        E = B
    End If
    
    DHMin = Fix(E / 60) Mod 60
 
End Function

Function DHSec(DH As Double) As Double

    A = Abs(DH)
    B = A * 3600
    C = Round(B - 60 * Fix(B / 60), 2)
    If (C = 60) Then
        D = 0
    Else
        D = C
    End If
    
    DHSec = D

End Function

Function LctUT(LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double) As Double


    A = HMSDH(LCH, LCM, LCS)
    B = A - DS - ZC
    C = LD + (B / 24#)
    D = CDJD(C, LM, LY)
    E = JDCDay(D)
    E1 = Fix(E)
    LctUT = 24# * (E - E1)
    
End Function

Function LctGDay(LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double) As Double
    
    A = HMSDH(LCH, LCM, LCS)
    B = A - DS - ZC
    C = LD + (B / 24)
    D = CDJD(C, LM, LY)
    E = JDCDay(D)
    LctGDay = Fix(E)

End Function

Function LctGMonth(LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double) As Double

    A = HMSDH(LCH, LCM, LCS)
    B = A - DS - ZC
    C = LD + (B / 24)
    D = CDJD(C, LM, LY)
    LctGMonth = JDCMonth(D)

End Function

Function LctGYear(LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double) As Double

    A = HMSDH(LCH, LCM, LCS)
    B = A - DS - ZC
    C = LD + (B / 24)
    D = CDJD(C, LM, LY)
    LctGYear = JDCYear(D)

End Function

Function UTLct(UH As Double, UM As Double, US As Double, DS As Double, ZC As Double, GD As Double, GM As Double, GY As Double) As Double

    A = HMSDH(UH, UM, US)
    B = A + ZC
    C = B + DS
    D = CDJD(GD, GM, GY) + (C / 24)
    E = JDCDay(D)
    E1 = Fix(E)
    UTLct = 24 * (E - E1)
    
End Function

Function UTLcDay(UH As Double, UM As Double, US As Double, DS As Double, ZC As Double, GD As Double, GM As Double, GY As Double) As Double

    A = HMSDH(UH, UM, US)
    B = A + ZC
    C = B + DS
    D = CDJD(GD, GM, GY) + (C / 24)
    E = JDCDay(D)
    E1 = Fix(E)
    UTLcDay = E1
    
End Function

Function UTLcMonth(UH As Double, UM As Double, US As Double, DS As Double, ZC As Double, GD As Double, GM As Double, GY As Double) As Double

    A = HMSDH(UH, UM, US)
    B = A + ZC
    C = B + DS
    D = CDJD(GD, GM, GY) + (C / 24)
    UTLcMonth = JDCMonth(D)
    
End Function

Function UTLcYear(UH As Double, UM As Double, US As Double, DS As Double, ZC As Double, GD As Double, GM As Double, GY As Double) As Double

    A = HMSDH(UH, UM, US)
    B = A + ZC
    C = B + DS
    D = CDJD(GD, GM, GY) + (C / 24)
    UTLcYear = JDCYear(D)
    
End Function

Function UTGST(UH As Double, UM As Double, US As Double, GD As Double, GM As Double, GY As Double) As Double

    A = CDJD(GD, GM, GY)
    B = A - 2451545
    C = B / 36525
    D = 6.697374558 + (2400.051336 * C) + (0.000025862 * C * C)
    E = D - (24 * Int(D / 24))
    F = HMSDH(UH, UM, US)
    G = F * 1.002737909
    H = E + G
    UTGST = H - (24 * Int(H / 24))
    
End Function

Function GSTUT(GSH As Double, GSM As Double, GSS As Double, GD As Double, GM As Double, GY As Double) As Double

    A = CDJD(GD, GM, GY)
    B = A - 2451545
    C = B / 36525
    D = 6.697374558 + (2400.051336 * C) + (0.000025862 * C * C)
    E = D - (24 * Int(D / 24))
    F = HMSDH(GSH, GSM, GSS)
    G = F - E
    H = G - (24 * Int(G / 24))
    GSTUT = H * 0.9972695663
    
End Function

Function eGSTUT(GSH As Double, GSM As Double, GSS As Double, GD As Double, GM As Double, GY As Double) As String

    A = CDJD(GD, GM, GY)
    B = A - 2451545
    C = B / 36525
    D = 6.697374558 + (2400.051336 * C) + (0.000025862 * C * C)
    E = D - (24 * Int(D / 24))
    F = HMSDH(GSH, GSM, GSS)
    G = F - E
    H = G - (24 * Int(G / 24))
    If ((H * 0.9972695663) < (4 / 60)) Then
        eGSTUT = "Warning"
    Else
        eGSTUT = "OK"
    End If
    
End Function

Function GSTLST(GH As Double, GM As Double, GS As Double, L As Double) As Double

    A = HMSDH(GH, GM, GS)
    B = L / 15#
    C = A + B
    GSTLST = C - (24# * Int(C / 24))
    
End Function

Function LSTGST(LH As Double, LM As Double, LS As Double, L As Double) As Double

    A = HMSDH(LH, LM, LS)
    B = L / 15#
    C = A - B
    LSTGST = C - (24# * Int(C / 24))
    
End Function

Function DMSDD(D As Double, M As Double, S As Double) As Double

    A = Abs(S) / 60
    B = (Abs(M) + A) / 60
    C = Abs(D) + B
    
    If ((D < 0) Or (M < 0) Or (S < 0)) Then
        DMSDD = -C
    Else
        DMSDD = C
    End If
    
End Function

Function DDDeg(DD As Double) As Double

    A = Abs(DD)
    B = A * 3600
    C = Round(B - 60 * Fix(B / 60), 2)
    
    If (C = 60) Then
        D = 0
        E = B + 60
    Else
        D = C
        E = B
    End If
    
    If (DD < 0) Then
        DDDeg = -Fix(E / 3600)
    Else
        DDDeg = Fix(E / 3600)
    End If
    
End Function

Function DDMin(DD As Double) As Double

    A = Abs(DD)
    B = A * 3600
    C = Round(B - 60 * Fix(B / 60), 2)
    
    If (C = 60) Then
        D = 0
        E = B + 60
    Else
        D = C
        E = B
    End If
    
    DDMin = Fix(E / 60) Mod 60
 
End Function

Function DDSec(DD As Double) As Double

    A = Abs(DD)
    B = A * 3600
    C = Round(B - 60 * Fix(B / 60), 2)
    If (C = 60) Then
        D = 0
    Else
        D = C
    End If
    
    DDSec = D

End Function

Function DDDH(DD As Double) As Double

    DDDH = DD / 15#

End Function

Function DHDD(DH As Double) As Double

    DHDD = DH * 15#
    
End Function

Function RAHA(RH As Double, RM As Double, RS As Double, LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double, L As Double) As Double

    A = LctUT(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
    B = LctGDay(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
    C = LctGMonth(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
    D = LctGYear(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
    E = UTGST(A, 0, 0, B, C, D)
    F = GSTLST(E, 0, 0, L)
    G = HMSDH(RH, RM, RS)
    H = F - G
    If (H < 0) Then
        RAHA = 24 + H
    Else
        RAHA = H
    End If
    
End Function

Function HARA(HH As Double, HM As Double, HS As Double, LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double, L As Double) As Double

    A = LctUT(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
    B = LctGDay(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
    C = LctGMonth(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
    D = LctGYear(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
    E = UTGST(A, 0, 0, B, C, D)
    F = GSTLST(E, 0, 0, L)
    G = HMSDH(HH, HM, HS)
    H = F - G
    If (H < 0) Then
        HARA = 24 + H
    Else
        HARA = H
    End If
    
End Function

Function Radians(W As Double) As Double
    
    Radians = W * 0.01745329252
    
End Function

Function Degrees(W As Double) As Double

    Degrees = W * 57.29577951
    
End Function

Function Atan2(X As Double, Y As Double) As Double

    B = 3.1415926535
    If (Abs(X) < 1E-20) Then
        If (Y < 0) Then
            A = -B / 2#
        Else
            A = B / 2#
        End If
    Else
        A = Atn(Y / X)
    End If
    
    If (X < 0) Then A = B + A
    If (A < 0) Then A = A + 2 * B
    Atan2 = A
    
End Function

Function Asin(W)

    Asin = Atn(W / Sqr(1 - W * W) + 1E-20)
    
End Function

Function Acos(W As Double) As Double

    Acos = 1.570796327 - Asin(W)
        
End Function

Function EQAz(HH As Double, HM As Double, HS As Double, DD As Double, DM As Double, DS As Double, P As Double) As Double

    A = HMSDH(HH, HM, HS)
    B = A * 15
    C = Radians(B)
    D = DMSDD(DD, DM, DS)
    E = Radians(D)
    F = Radians(P)
    G = Sin(E) * Sin(F) + Cos(E) * Cos(F) * Cos(C)
    H = -Cos(E) * Cos(F) * Sin(C)
    I = Sin(E) - (Sin(F) * G)
    J = Degrees(Atan2(I, H))
    EQAz = J - 360# * Int(J / 360#)
        
End Function

Function EQAlt(HH As Double, HM As Double, HS As Double, DD As Double, DM As Double, DS As Double, P As Double) As Double

    A = HMSDH(HH, HM, HS)
    B = A * 15
    C = Radians(B)
    D = DMSDD(DD, DM, DS)
    E = Radians(D)
    F = Radians(P)
    G = Sin(E) * Sin(F) + Cos(E) * Cos(F) * Cos(C)
    EQAlt = Degrees(Asin(G))
        
End Function

Function HORDec(AZD As Double, AZM As Double, AZS As Double, ALD As Double, ALM As Double, ALS As Double, P As Double) As Double

    A = DMSDD(AZD, AZM, AZS)
    B = DMSDD(ALD, ALM, ALS)
    C = Radians(A)
    D = Radians(B)
    E = Radians(P)
    F = Sin(D) * Sin(E) + Cos(D) * Cos(E) * Cos(C)
    HORDec = Degrees(Asin(F))
        
End Function

Function HORHa(AZD As Double, AZM As Double, AZS As Double, ALD As Double, ALM As Double, ALS As Double, P As Double) As Double

    A = DMSDD(AZD, AZM, AZS)
    B = DMSDD(ALD, ALM, ALS)
    C = Radians(A)
    D = Radians(B)
    E = Radians(P)
    F = Sin(D) * Sin(E) + Cos(D) * Cos(E) * Cos(C)
    G = -Cos(D) * Cos(E) * Sin(C)
    H = Sin(D) - Sin(E) * F
    I = DDDH(Degrees(Atan2(H, G)))
    HORHa = I - 24# * Int(I / 24#)
    
End Function

Function Obliq(GD As Double, GM As Double, GY As Double) As Double

    A = CDJD(GD, GM, GY)
    B = A - 2415020#
    C = (B / 36525#) - 1#
    D = C * (46.815 + C * (0.0006 - (C * 0.00181)))
    E = D / 3600#
    Obliq = 23.43929167 - E + NutatObl(GD, GM, GY)

End Function

Function ECDec(ELD As Double, ELM As Double, ELS As Double, BD As Double, BM As Double, BS As Double, GD As Double, GM As Double, GY As Double) As Double

    A = Radians(DMSDD(ELD, ELM, ELS))
    B = Radians(DMSDD(BD, BM, BS))
    C = Radians(Obliq(GD, GM, GY))
    D = Sin(B) * Cos(C) + Cos(B) * Sin(C) * Sin(A)
    ECDec = Degrees(Asin(D))
        
End Function
Function ECRA(ELD As Double, ELM As Double, ELS As Double, BD As Double, BM As Double, BS As Double, GD As Double, GM As Double, GY As Double) As Double

    A = Radians(DMSDD(ELD, ELM, ELS))
    B = Radians(DMSDD(BD, BM, BS))
    C = Radians(Obliq(GD, GM, GY))
    D = Sin(A) * Cos(C) - Tan(B) * Sin(C)
    E = Cos(A)
    F = Degrees(Atan2(E, D))
    ECRA = F - 360# * Int(F / 360#)
    
End Function

Function EQElat(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double, GD As Double, GM As Double, GY As Double) As Double

    A = Radians(DHDD(HMSDH(RAH, RAM, RAS)))
    B = Radians(DMSDD(DD, DM, DS))
    C = Radians(Obliq(GD, GM, GY))
    D = Sin(B) * Cos(C) - Cos(B) * Sin(C) * Sin(A)
    EQElat = Degrees(Asin(D))
        
End Function

Function EQElong(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double, GD As Double, GM As Double, GY As Double) As Double

    A = Radians(DHDD(HMSDH(RAH, RAM, RAS)))
    B = Radians(DMSDD(DD, DM, DS))
    C = Radians(Obliq(GD, GM, GY))
    D = Sin(A) * Cos(C) + Tan(B) * Sin(C)
    E = Cos(A)
    F = Degrees(Atan2(E, D))
    EQElong = F - 360# * Int(F / 360#)
    
End Function

Function EQGlong(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double) As Double

    A = Radians(DHDD(HMSDH(RAH, RAM, RAS)))
    B = Radians(DMSDD(DD, DM, DS))
    C = Cos(Radians(27.4))
    D = Sin(Radians(27.4))
    E = Radians(192.25)
    F = Cos(B) * C * Cos(A - E) + Sin(B) * D
    G = Sin(B) - F * D
    H = Cos(B) * Sin(A - E) * C
    I = Degrees(Atan2(H, G)) + 33
    EQGlong = I - 360 * Int(I / 360)
    
End Function

Function EQGlat(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double) As Double

    A = Radians(DHDD(HMSDH(RAH, RAM, RAS)))
    B = Radians(DMSDD(DD, DM, DS))
    C = Cos(Radians(27.4))
    D = Sin(Radians(27.4))
    E = Radians(192.25)
    F = Cos(B) * C * Cos(A - E) + Sin(B) * D
    EQGlat = Degrees(Asin(F))
    
End Function

Function Precess2RA(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double, DA As Double, MA As Double, YA As Double, DB As Double, MB As Double, YB As Double) As Double
Dim MT(3, 3) As Double, CV(3) As Double, HL(3) As Double, MV(3, 3) As Double
Dim II, J As Integer

        DY = DA: MN = MA: YR = YA: GoSub 2750
        MT(1, 1) = C1 * C3 * C2 - S1 * S2: MT(1, 2) = -S1 * C3 * C2 - C1 * S2
        MT(1, 3) = -S3 * C2: MT(2, 1) = C1 * C3 * S2 + S1 * C2
        MT(2, 2) = -S1 * C3 * S2 + C1 * C2: MT(2, 3) = -S3 * S2
        MT(3, 1) = C1 * S3: MT(3, 2) = -S1 * S3: MT(3, 3) = C3

        DY = DB: MN = MB: YR = YB: GoSub 2750
        MV(1, 1) = C1 * C3 * C2 - S1 * S2: MV(2, 1) = -S1 * C3 * C2 - C1 * S2
        MV(3, 1) = -S3 * C2: MV(1, 2) = C1 * C3 * S2 + S1 * C2
        MV(2, 2) = -S1 * C3 * S2 + C1 * C2: MV(3, 2) = -S3 * S2
        MV(1, 3) = C1 * S3: MV(2, 3) = -S1 * S3: MV(3, 3) = C3
        Pi = 3.141592654: TP = 2# * Pi

        X = Radians(DHDD(HMSDH(RAH, RAM, RAS))): Y = Radians(DMSDD(DD, DM, DS))
        CN = Cos(Y): CV(1) = Cos(X) * CN
        CV(2) = Sin(X) * CN: CV(3) = Sin(Y)

        For J = 1 To 3: SM = 0#
        For II = 1 To 3: SM = SM + MT(II, J%) * CV(II): Next II
        HL(J%) = SM: Next J%
        For II = 1 To 3: CV(II) = HL(II): Next II

        For J% = 1 To 3: SM = 0#
        For II = 1 To 3: SM = SM + MV(II, J%) * CV(II): Next II
        HL(J%) = SM: Next J%
        For II = 1 To 3: CV(II) = HL(II): Next II

        P = Atan2(CV(1), CV(2))
        Precess2RA = DDDH(Degrees(P))
        GoTo 2795

2750    DJ = CDJD(DY, MN, YR) - 2415020#: T = (DJ - 36525#) / 36525#
        XA = (((0.000005 * T) + 0.0000839) * T + 0.6406161) * T
        ZA = (((0.0000051 * T) + 0.0003041) * T + 0.6406161) * T
        TA = (((-0.0000116 * T) - 0.0001185) * T + 0.556753) * T
        XA = Radians(XA): ZA = Radians(ZA): TA = Radians(TA)
        C1 = Cos(XA): C2 = Cos(ZA): C3 = Cos(TA)
        S1 = Sin(XA): S2 = Sin(ZA): S3 = Sin(TA)
        Return

2795    Rem No operation

End Function

Function Precess2Dec(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double, DA As Double, MA As Double, YA As Double, DB As Double, MB As Double, YB As Double) As Double
Dim MT(3, 3) As Double, CV(3) As Double, HL(3) As Double, MV(3, 3) As Double
Dim II, J As Integer

        DY = DA: MN = MA: YR = YA: GoSub 2750
        MT(1, 1) = C1 * C3 * C2 - S1 * S2: MT(1, 2) = -S1 * C3 * C2 - C1 * S2
        MT(1, 3) = -S3 * C2: MT(2, 1) = C1 * C3 * S2 + S1 * C2
        MT(2, 2) = -S1 * C3 * S2 + C1 * C2: MT(2, 3) = -S3 * S2
        MT(3, 1) = C1 * S3: MT(3, 2) = -S1 * S3: MT(3, 3) = C3

        DY = DB: MN = MB: YR = YB: GoSub 2750
        MV(1, 1) = C1 * C3 * C2 - S1 * S2: MV(2, 1) = -S1 * C3 * C2 - C1 * S2
        MV(3, 1) = -S3 * C2: MV(1, 2) = C1 * C3 * S2 + S1 * C2
        MV(2, 2) = -S1 * C3 * S2 + C1 * C2: MV(3, 2) = -S3 * S2
        MV(1, 3) = C1 * S3: MV(2, 3) = -S1 * S3: MV(3, 3) = C3
        Pi = 3.141592654: TP = 2# * Pi

        X = Radians(DHDD(HMSDH(RAH, RAM, RAS))): Y = Radians(DMSDD(DD, DM, DS))
        CN = Cos(Y): CV(1) = Cos(X) * CN
        CV(2) = Sin(X) * CN: CV(3) = Sin(Y)

        For J = 1 To 3: SM = 0#
        For II = 1 To 3: SM = SM + MT(II, J%) * CV(II): Next II
        HL(J%) = SM: Next J%
        For II = 1 To 3: CV(II) = HL(II): Next II

        For J% = 1 To 3: SM = 0#
        For II = 1 To 3: SM = SM + MV(II, J%) * CV(II): Next II
        HL(J%) = SM: Next J%
        For II = 1 To 3: CV(II) = HL(II): Next II

        Q = Asin(CV(3))
        Precess2Dec = Degrees(Q)
        GoTo 2795

2750    DJ = CDJD(DY, MN, YR) - 2415020#: T = (DJ - 36525#) / 36525#
        XA = (((0.000005 * T) + 0.0000839) * T + 0.6406161) * T
        ZA = (((0.0000051 * T) + 0.0003041) * T + 0.6406161) * T
        TA = (((-0.0000116 * T) - 0.0001185) * T + 0.556753) * T
        XA = Radians(XA): ZA = Radians(ZA): TA = Radians(TA)
        C1 = Cos(XA): C2 = Cos(ZA): C3 = Cos(TA)
        S1 = Sin(XA): S2 = Sin(ZA): S3 = Sin(TA)
        Return

2795    Rem No operation

End Function

Function Angle(XX1 As Double, XM1 As Double, XS1 As Double, DD1 As Double, DM1 As Double, DS1 As Double, XX2 As Double, XM2 As Double, XS2 As Double, DD2 As Double, DM2 As Double, DS2 As Double, S As String) As Double
    
    If ((S = "H") Or (S = "h")) Then
        A = DHDD(HMSDH(XX1, XM1, XS1))
    Else
        A = DMSDD(XX1, XM1, XD1)
    End If
    B = Radians(A)
    C = DMSDD(DD1, DM1, DS1)
    D = Radians(C)
    
    If ((S = "H") Or (S = "h")) Then
        E = DHDD(HMSDH(XX2, XM2, XS2))
    Else
        E = DMSDD(XX2, XM2, XD2)
    End If
    F = Radians(E)
    G = DMSDD(DD2, DM2, DS2)
    H = Radians(G)
    I = Acos(Sin(D) * Sin(H) + Cos(D) * Cos(H) * Cos(B - F))
    Angle = Degrees(I)

End Function

Function RSLSTR(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double, VD As Double, G As Double) As Double

    A = HMSDH(RAH, RAM, RAS)
    B = Radians(DHDD(A))
    C = Radians(DMSDD(DD, DM, DS))
    D = Radians(VD)
    E = Radians(G)
    F = -(Sin(D) + Sin(E) * Sin(C)) / (Cos(E) * Cos(C))
    
    If (Abs(F) < 1#) Then
        H = Acos(F)
    Else
        H = 0
    End If
        
    I = DDDH(Degrees(B - H))
    RSLSTR = I - 24 * Int(I / 24#)

End Function

Function eRS(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double, VD As Double, G As Double) As String

    A = HMSDH(RAH, RAM, RAS)
    B = Radians(DHDD(A))
    C = Radians(DMSDD(DD, DM, DS))
    D = Radians(VD)
    E = Radians(G)
    F = -(Sin(D) + Sin(E) * Sin(C)) / (Cos(E) * Cos(C))
    
    eRS = "OK"
    If (F >= 1#) Then
        eRS = "** never rises"
    End If
    If (F <= -1#) Then
        eRS = "** circumpolar"
    End If

End Function

Function RSLSTS(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double, VD As Double, G As Double) As Double

    A = HMSDH(RAH, RAM, RAS)
    B = Radians(DHDD(A))
    C = Radians(DMSDD(DD, DM, DS))
    D = Radians(VD)
    E = Radians(G)
    F = -(Sin(D) + Sin(E) * Sin(C)) / (Cos(E) * Cos(C))
    
    If (Abs(F) < 1#) Then
        H = Acos(F)
    Else
        H = 0
    End If
        
    I = DDDH(Degrees(B + H))
    RSLSTS = I - 24 * Int(I / 24#)

End Function

Function RSAZR(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double, VD As Double, G As Double) As Double

    A = HMSDH(RAH, RAM, RAS)
    B = Radians(DHDD(A))
    C = Radians(DMSDD(DD, DM, DS))
    D = Radians(VD)
    E = Radians(G)
    F = (Sin(C) + Sin(D) * Sin(E)) / (Cos(D) * Cos(E))
    
    If (eRS(RAH, RAM, RAS, DD, DM, DS, VD, G) = "OK") Then
        H = Acos(F)
    Else
        H = 0
    End If
        
    I = Degrees(H)
    RSAZR = I - 360# * Int(I / 360#)

End Function

Function RSAZS(RAH As Double, RAM As Double, RAS As Double, DD As Double, DM As Double, DS As Double, VD As Double, G As Double) As Double

    A = HMSDH(RAH, RAM, RAS)
    B = Radians(DHDD(A))
    C = Radians(DMSDD(DD, DM, DS))
    D = Radians(VD)
    E = Radians(G)
    F = (Sin(C) + Sin(D) * Sin(E)) / (Cos(D) * Cos(E))
    
    If (eRS(RAH, RAM, RAS, DD, DM, DS, VD, G) = "OK") Then
        H = Acos(F)
    Else
        H = 0
    End If
        
    I = 360# - Degrees(H)
    RSAZS = I - 360# * Int(I / 360#)

End Function

Function NutatLong(GD As Double, GM As Double, GY As Double) As Double

        DJ = CDJD(GD, GM, GY) - 2415020#: T = DJ / 36525#: T2 = T * T
        A = 100.0021358 * T: B = 360# * (A - Int(A))
        L1 = 279.6967 + 0.000303 * T2 + B: l2 = 2# * Radians(L1)
        A = 1336.855231 * T: B = 360# * (A - Int(A))
        D1 = 270.4342 - 0.001133 * T2 + B: D2 = 2# * Radians(D1)
        A = 99.99736056 * T: B = 360# * (A - Int(A))
        M1 = 358.4758 - 0.00015 * T2 + B: M1 = Radians(M1)
        A = 1325.552359 * T: B = 360# * (A - Int(A))
        M2 = 296.1046 + 0.009192 * T2 + B: M2 = Radians(M2)
        A = 5.372616667 * T: B = 360# * (A - Int(A))
        N1 = 259.1833 + 0.002078 * T2 - B: N1 = Radians(N1)
        N2 = 2 * N1

        DP = (-17.2327 - 0.01737 * T) * Sin(N1)
        DP = DP + (-1.2729 - 0.00013 * T) * Sin(l2) + 0.2088 * Sin(N2)
        DP = DP - 0.2037 * Sin(D2) + (0.1261 - 0.00031 * T) * Sin(M1)
        DP = DP + 0.0675 * Sin(M2) - (0.0497 - 0.00012 * T) * Sin(l2 + M1)
        DP = DP - 0.0342 * Sin(D2 - N1) - 0.0261 * Sin(D2 + M2)
        DP = DP + 0.0214 * Sin(l2 - M1) - 0.0149 * Sin(l2 - D2 + M2)
        DP = DP + 0.0124 * Sin(l2 - N1) + 0.0114 * Sin(D2 - M2)

        NutatLong = DP / 3600#
        
End Function

Function NutatObl(GD As Double, GM As Double, GY As Double) As Double

        DJ = CDJD(GD, GM, GY) - 2415020#: T = DJ / 36525#: T2 = T * T
        A = 100.0021358 * T: B = 360# * (A - Int(A))
        L1 = 279.6967 + 0.000303 * T2 + B: l2 = 2# * Radians(L1)
        A = 1336.855231 * T: B = 360# * (A - Int(A))
        D1 = 270.4342 - 0.001133 * T2 + B: D2 = 2# * Radians(D1)
        A = 99.99736056 * T: B = 360# * (A - Int(A))
        M1 = 358.4758 - 0.00015 * T2 + B: M1 = Radians(M1)
        A = 1325.552359 * T: B = 360# * (A - Int(A))
        M2 = 296.1046 + 0.009192 * T2 + B: M2 = Radians(M2)
        A = 5.372616667 * T: B = 360# * (A - Int(A))
        N1 = 259.1833 + 0.002078 * T2 - B: N1 = Radians(N1)
        N2 = 2 * N1

        DDO = (9.21 + 0.00091 * T) * Cos(N1)
        DDO = DDO + (0.5522 - 0.00029 * T) * Cos(l2) - 0.0904 * Cos(N2)
        DDO = DDO + 0.0884 * Cos(D2) + 0.0216 * Cos(l2 + M1)
        DDO = DDO + 0.0183 * Cos(D2 - N1) + 0.0113 * Cos(D2 + M2)
        DDO = DDO - 0.0093 * Cos(l2 - M1) - 0.0066 * Cos(l2 - N1)

        NutatObl = DDO / 3600#
        
End Function

Function Refract(Y2 As Double, SW As String, PR As Double, TR As Double) As Double

        Y = Radians(Y2)
        If ((Left$(SW, 1) = "T") Or (Left$(SW, 1) = "t")) Then
            D = -1#
        Else
            D = 1#
        End If
        
        If (D = -1#) Then
            Y3 = Y: Y1 = Y: R1 = 0
3020        Y = Y1 + R1: Q = Y: GoSub 3035: R2 = RF
            If ((R2 = 0) Or (Abs(R2 - R1) < 0.000001)) Then
                Q = Y3
                GoTo 3075
            End If
            R1 = R2: GoTo 3020
        Else
            GoSub 3035
            Q = Y
            GoTo 3075
        End If

3035    If (Y < 0.2617994) Then
            GoTo 3050
        Else
            RF = -D * 0.00007888888 * PR / ((273 + TR) * Tan(Y))
            Return
        End If
        
3050    If (Y < -0.087) Then
            Q = 0
            RF = 0
            GoTo 3075
        End If

        YD = Degrees(Y)
        A = ((0.00002 * YD + 0.0196) * YD + 0.1594) * PR
        B = (273 + TR) * ((0.0845 * YD + 0.505) * YD + 1#)
        RF = Radians(-(A / B) * D)
        Return

3075    Refract = Degrees(Q + RF)

End Function

Function ParallaxHA(HH As Double, HM As Double, HS As Double, DD As Double, DM As Double, DS As Double, SW As String, GP As Double, HT As Double, HP As Double) As Double

        A = Radians(GP): C1 = Cos(A): S1 = Sin(A)
        U = Atn(0.996647 * S1 / C1)
        C2 = Cos(U): S2 = Sin(U): B = HT / 6378160#
        RS = (0.996647 * S2) + (B * S1)
        RC = C2 + (B * C1): TP = 6.283185308

        RP = 1 / Sin(Radians(HP))
        
        X = Radians(DHDD(HMSDH(HH, HM, HS))): X1 = X
        Y = Radians(DMSDD(DD, DM, DS)): Y1 = Y
        
        If ((Left$(SW, 1) = "T") Or (Left$(SW, 1) = "t")) Then
            D = 1#
        Else
            D = -1#
        End If
        
        If (D = 1) Then
            GoSub 2870
            GoTo 2895
        End If
        
        P1 = 0: Q1 = 0
2845    GoSub 2870: P2 = P - X: Q2 = Q - Y
        AA = Abs(P2 - P1): BB = Abs(Q2 - Q1)
        
        If ((AA < 0.000001) And (BB < 0.000001)) Then
            P = X1 - P2: Q = Y1 - Q2: X = X1: Y = Y1
            GoTo 2895
        End If
        
        X = X1 - P2: Y = Y1 - Q2: P1 = P2: Q1 = Q2: GoTo 2845

2870    CX = Cos(X): SY = Sin(Y): CY = Cos(Y)
        AA = (RC * Sin(X)) / ((RP * CY) - (RC * CX))
        DX = Atn(AA): P = X + DX: CP = Cos(P)
        P = P - TP * Int(P / TP)
        Q = Atn(CP * (RP * SY - RS) / (RP * CY * CX - RC))
        Return

2895    ParallaxHA = DDDH(Degrees(P))

End Function

Function ParallaxDec(HH As Double, HM As Double, HS As Double, DD As Double, DM As Double, DS As Double, SW As String, GP As Double, HT As Double, HP As Double) As Double

        A = Radians(GP): C1 = Cos(A): S1 = Sin(A)
        U = Atn(0.996647 * S1 / C1)
        C2 = Cos(U): S2 = Sin(U): B = HT / 6378160#
        RS = (0.996647 * S2) + (B * S1)
        RC = C2 + (B * C1): TP = 6.283185308

        RP = 1 / Sin(Radians(HP))
        
        X = Radians(DHDD(HMSDH(HH, HM, HS))): X1 = X
        Y = Radians(DMSDD(DD, DM, DS)): Y1 = Y
        
        If ((Left$(SW, 1) = "T") Or (Left$(SW, 1) = "t")) Then
            D = 1#
        Else
            D = -1#
        End If
        
        If (D = 1) Then
            GoSub 2870
            GoTo 2895
        End If
        
        P1 = 0: Q1 = 0
2845    GoSub 2870: P2 = P - X: Q2 = Q - Y
        AA = Abs(P2 - P1): BB = Abs(Q2 - Q1)
        
        If ((AA < 0.000001) And (BB < 0.000001)) Then
            P = X1 - P2: Q = Y1 - Q2: X = X1: Y = Y1
            GoTo 2895
        End If
        
        X = X1 - P2: Y = Y1 - Q2: P1 = P2: Q1 = Q2: GoTo 2845

2870    CX = Cos(X): SY = Sin(Y): CY = Cos(Y)
        AA = (RC * Sin(X)) / ((RP * CY) - (RC * CX))
        DX = Atn(AA): P = X + DX: CP = Cos(P)
        P = P - TP * Int(P / TP)
        Q = Atn(CP * (RP * SY - RS) / (RP * CY * CX - RC))
        Return

2895    ParallaxDec = Degrees(Q)

End Function

Function TrueAnomaly(AM As Double, EC As Double) As Double

        TP = 6.283185308: M = AM - TP * Int(AM / TP): AE = M
3305    D = AE - (EC * Sin(AE)) - M
        
        If (Abs(D) < 0.000001) Then
            GoTo 3320
        End If
        
        D = D / (1# - (EC * Cos(AE))): AE = AE - D: GoTo 3305

3320    A = Sqr((1# + EC) / (1# - EC)) * Tan(AE / 2#)
        AT = 2# * Atn(A)
        TrueAnomaly = AT
        
End Function

Function EccentricAnomaly(AM As Double, EC As Double) As Double

        TP = 6.283185308: M = AM - TP * Int(AM / TP): AE = M
3305    D = AE - (EC * Sin(AE)) - M
        
        If (Abs(D) < 0.000001) Then
            GoTo 3320
        End If
        
        D = D / (1# - (EC * Cos(AE))): AE = AE - D: GoTo 3305

3320    EccentricAnomaly = AE
        
End Function

Function SunLong(LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double) As Double

        AA = LctGDay(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        BB = LctGMonth(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        CC = LctGYear(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        UT = LctUT(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        DJ = CDJD(AA, BB, CC) - 2415020#
        T = (DJ / 36525#) + (UT / 876600#): T2 = T * T
        A = 100.0021359 * T: B = 360# * (A - Int(A))
        L = 279.69668 + 0.0003025 * T2 + B
        A = 99.99736042 * T: B = 360# * (A - Int(A))
        M1 = 358.47583 - (0.00015 + 0.0000033 * T) * T2 + B
        EC = 0.01675104 - 0.0000418 * T - 0.000000126 * T2

        AM = Radians(M1)
        AT = TrueAnomaly(AM, EC): AE = EccentricAnomaly(AM, EC)

        A = 62.55209472 * T: B = 360# * (A - Int(A))
        A1 = Radians(153.23 + B)
        A = 125.1041894 * T: B = 360# * (A - Int(A))
        B1 = Radians(216.57 + B)
        A = 91.56766028 * T: B = 360# * (A - Int(A))
        C1 = Radians(312.69 + B)
        A = 1236.853095 * T: B = 360# * (A - Int(A))
        D1 = Radians(350.74 - 0.00144 * T2 + B)
        E1 = Radians(231.19 + 20.2 * T)
        A = 183.1353208 * T: B = 360# * (A - Int(A))
        H1 = Radians(353.4 + B)

        D2 = 0.00134 * Cos(A1) + 0.00154 * Cos(B1) + 0.002 * Cos(C1)
        D2 = D2 + 0.00179 * Sin(D1) + 0.00178 * Sin(E1)
        D3 = 0.00000543 * Sin(A1) + 0.00001575 * Sin(B1)
        D3 = D3 + 0.00001627 * Sin(C1) + 0.00003076 * Cos(D1)
        D3 = D3 + 0.00000927 * Sin(H1)

        SR = AT + Radians(L - M1 + D2): TP = 6.283185308
        SR = SR - TP * Int(SR / TP)
        SunLong = Degrees(SR)
        
End Function

Function SunDist(LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double) As Double

        AA = LctGDay(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        BB = LctGMonth(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        CC = LctGYear(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        UT = LctUT(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        DJ = CDJD(AA, BB, CC) - 2415020#
        T = (DJ / 36525#) + (UT / 876600#): T2 = T * T
        A = 100.0021359 * T: B = 360# * (A - Int(A))
        L = 279.69668 + 0.0003025 * T2 + B
        A = 99.99736042 * T: B = 360# * (A - Int(A))
        M1 = 358.47583 - (0.00015 + 0.0000033 * T) * T2 + B
        EC = 0.01675104 - 0.0000418 * T - 0.000000126 * T2

        AM = Radians(M1)
        AT = TrueAnomaly(AM, EC): AE = EccentricAnomaly(AM, EC)

        A = 62.55209472 * T: B = 360# * (A - Int(A))
        A1 = Radians(153.23 + B)
        A = 125.1041894 * T: B = 360# * (A - Int(A))
        B1 = Radians(216.57 + B)
        A = 91.56766028 * T: B = 360# * (A - Int(A))
        C1 = Radians(312.69 + B)
        A = 1236.853095 * T: B = 360# * (A - Int(A))
        D1 = Radians(350.74 - 0.00144 * T2 + B)
        E1 = Radians(231.19 + 20.2 * T)
        A = 183.1353208 * T: B = 360# * (A - Int(A))
        H1 = Radians(353.4 + B)

        D2 = 0.00134 * Cos(A1) + 0.00154 * Cos(B1) + 0.002 * Cos(C1)
        D2 = D2 + 0.00179 * Sin(D1) + 0.00178 * Sin(E1)
        D3 = 0.00000543 * Sin(A1) + 0.00001575 * Sin(B1)
        D3 = D3 + 0.00001627 * Sin(C1) + 0.00003076 * Cos(D1)
        D3 = D3 + 0.00000927 * Sin(H1)

        RR = 1.0000002 * (1# - EC * Cos(AE)) + D3
        SunDist = RR
        
End Function

Function SunDia(LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double) As Double

    A = SunDist(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
    SunDia = 0.533128 / A
    
End Function

Function SunTrueAnomaly(LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double) As Double

        AA = LctGDay(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        BB = LctGMonth(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        CC = LctGYear(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        UT = LctUT(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        DJ = CDJD(AA, BB, CC) - 2415020#
        T = (DJ / 36525#) + (UT / 876600#): T2 = T * T
        A = 100.0021359 * T: B = 360# * (A - Int(A))
        L = 279.69668 + 0.0003025 * T2 + B
        A = 99.99736042 * T: B = 360# * (A - Int(A))
        M1 = 358.47583 - (0.00015 + 0.0000033 * T) * T2 + B
        EC = 0.01675104 - 0.0000418 * T - 0.000000126 * T2

        AM = Radians(M1)
        SunTrueAnomaly = Degrees(TrueAnomaly(AM, EC))
        
End Function

Function SunMeanAnomaly(LCH As Double, LCM As Double, LCS As Double, DS As Double, ZC As Double, LD As Double, LM As Double, LY As Double) As Double

        AA = LctGDay(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        BB = LctGMonth(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        CC = LctGYear(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        UT = LctUT(LCH, LCM, LCS, DS, ZC, LD, LM, LY)
        DJ = CDJD(AA, BB, CC) - 2415020#
        T = (DJ / 36525#) + (UT / 876600#): T2 = T * T
        A = 100.0021359 * T: B = 360# * (A - Int(A))
        M1 = 358.47583 - (0.00015 + 0.0000033 * T) * T2 + B
        AM = Unwind(Radians(M1))
        SunMeanAnomaly = AM
        
End Function

Function SunEcc(GD As Double, GM As Double, GY As Double) As Double

    T = (CDJD(GD, GM, GY) - 2415020#) / 36525#
    T2 = T * T
    SunEcc = 0.01675104 - 0.0000418 * T - 0.000000126 * T2
    
End Function

Function SunElong(GD As Double, GM As Double, GY As Double) As Double

    T = (CDJD(GD, GM, GY) - 2415020#) / 36525#
    T2 = T * T
    X = 279.6966778 + 36000.76892 * T + 0.0003025 * T2
    SunElong = X - 360# * Int(X / 360#)
    
End Function

Function SunPeri(GD As Double, GM As Double, GY As Double) As Double

    T = (CDJD(GD, GM, GY) - 2415020#) / 36525#
    T2 = T * T
    X = 281.2208444 + 1.719175 * T + 0.000452778 * T2
    SunPeri = X - 360# * Int(X / 360#)
    
End Function

Function Ablong(UH As Double, UM As Double, US As Double, GD As Double, GM As Double, GY As Double, LD As Double, LM As Double, LS As Double, BD As Double, BM As Double, BS As Double) As Double

    A = DMSDD(LD, LM, LS)
    B = DMSDD(BD, BM, BS)
    C = SunLong(UH, UM, US, 0, 0, GD, GM, GY)
    D = Radians(C - A)
    E = Radians(B)
    F = -20.5 * Cos(D) / Cos(E)
    Ablong = A + (F / 3600#)
    
End Function

Function Ablat(UH As Double, UM As Double, US As Double, GD As Double, GM As Double, GY As Double, LD As Double, LM As Double, LS As Double, BD As Double, BM As Double, BS As Double) As Double

    A = DMSDD(LD, LM, LS)
    B = DMSDD(BD, BM, BS)
    C = SunLong(UH, UM, US, 0, 0, GD, GM, GY)
    D = Radians(C - A)
    E = Radians(B)
    F = -20.5 * Sin(D) * Sin(E)
    Ablat = B + (F / 3600#)
    
End Function

Function SunriseLCT(LD As Double, LM As Double, LY As Double, DS As Double, ZC As Double, GL As Double, GP As Double) As Double
Dim S As String

        DI = 0.8333333
        GD = LctGDay(12, 0, 0, DS, ZC, LD, LM, LY)
        GM = LctGMonth(12, 0, 0, DS, ZC, LD, LM, LY)
        GY = LctGYear(12, 0, 0, DS, ZC, LD, LM, LY)
        SR = SunLong(12, 0, 0, DS, ZC, LD, LM, LY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        
        If (eGSTUT(X, 0, 0, GD, GM, GS) <> "OK") Then
            XX = -99#
            GoTo 3740
        End If
        
        SR = SunLong(UT, 0, 0, 0, 0, GD, GM, GY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        XX = UTLct(UT, 0, 0, DS, ZC, GD, GM, GY)
        GoTo 3740
        
3710    A = SR + NutatLong(GD, GM, GY) - 0.005694
        X = ECRA(A, 0, 0, 0, 0, 0, GD, GM, GY)
        Y = ECDec(A, 0, 0, 0, 0, 0, GD, GM, GY)
        LA = RSLSTR(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        S = eRS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        Return

3740    SunriseLCT = XX
                
End Function

Function eSunRS(LD As Double, LM As Double, LY As Double, DS As Double, ZC As Double, GL As Double, GP As Double) As String
Dim S As String

        S = ""
        DI = 0.8333333
        GD = LctGDay(12, 0, 0, DS, ZC, LD, LM, LY)
        GM = LctGMonth(12, 0, 0, DS, ZC, LD, LM, LY)
        GY = LctGYear(12, 0, 0, DS, ZC, LD, LM, LY)
        SR = SunLong(12, 0, 0, DS, ZC, LD, LM, LY)
        GoSub 3710
        
        If (S <> "OK") Then
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        SR = SunLong(UT, 0, 0, 0, 0, GD, GM, GY)
        GoSub 3710
        GoTo 3740

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        
        If (eGSTUT(X, 0, 0, GD, GM, GS) <> "OK") Then
            S = S + " GST to UT conversion warning"
            GoTo 3740
        End If
        
        GoTo 3740
        
3710    A = SR + NutatLong(GD, GM, GY) - 0.005694
        X = ECRA(A, 0, 0, 0, 0, 0, GD, GM, GY)
        Y = ECDec(A, 0, 0, 0, 0, 0, GD, GM, GY)
        LA = RSLSTR(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        S = eRS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        Return

3740    eSunRS = S
                
End Function

Function SunsetLCT(LD As Double, LM As Double, LY As Double, DS As Double, ZC As Double, GL As Double, GP As Double) As Double
Dim S As String

        DI = 0.8333333
        GD = LctGDay(12, 0, 0, DS, ZC, LD, LM, LY)
        GM = LctGMonth(12, 0, 0, DS, ZC, LD, LM, LY)
        GY = LctGYear(12, 0, 0, DS, ZC, LD, LM, LY)
        SR = SunLong(12, 0, 0, DS, ZC, LD, LM, LY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        
        If (eGSTUT(X, 0, 0, GD, GM, GS) <> "OK") Then
            XX = -99#
            GoTo 3740
        End If
        
        SR = SunLong(UT, 0, 0, 0, 0, GD, GM, GY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        XX = UTLct(UT, 0, 0, DS, ZC, GD, GM, GY)
        GoTo 3740
        
3710    A = SR + NutatLong(GD, GM, GY) - 0.005694
        X = ECRA(A, 0, 0, 0, 0, 0, GD, GM, GY)
        Y = ECDec(A, 0, 0, 0, 0, 0, GD, GM, GY)
        LA = RSLSTS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        S = eRS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        Return

3740    SunsetLCT = XX
                
End Function

Function SunsetAZ(LD As Double, LM As Double, LY As Double, DS As Double, ZC As Double, GL As Double, GP As Double) As Double
Dim S As String

        DI = 0.8333333
        GD = LctGDay(12, 0, 0, DS, ZC, LD, LM, LY)
        GM = LctGMonth(12, 0, 0, DS, ZC, LD, LM, LY)
        GY = LctGYear(12, 0, 0, DS, ZC, LD, LM, LY)
        SR = SunLong(12, 0, 0, DS, ZC, LD, LM, LY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        
        If (eGSTUT(X, 0, 0, GD, GM, GS) <> "OK") Then
            XX = -99#
            GoTo 3740
        End If
        
        SR = SunLong(UT, 0, 0, 0, 0, GD, GM, GY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        XX = RSAZS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        GoTo 3740
        
3710    A = SR + NutatLong(GD, GM, GY) - 0.005694
        X = ECRA(A, 0, 0, 0, 0, 0, GD, GM, GY)
        Y = ECDec(A, 0, 0, 0, 0, 0, GD, GM, GY)
        LA = RSLSTS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        S = eRS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        Return

3740    SunsetAZ = XX
                
End Function

Function SunriseAZ(LD As Double, LM As Double, LY As Double, DS As Double, ZC As Double, GL As Double, GP As Double) As Double
Dim S As String

        DI = 0.8333333
        GD = LctGDay(12, 0, 0, DS, ZC, LD, LM, LY)
        GM = LctGMonth(12, 0, 0, DS, ZC, LD, LM, LY)
        GY = LctGYear(12, 0, 0, DS, ZC, LD, LM, LY)
        SR = SunLong(12, 0, 0, DS, ZC, LD, LM, LY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        
        If (eGSTUT(X, 0, 0, GD, GM, GS) <> "OK") Then
            XX = -99#
            GoTo 3740
        End If
        
        SR = SunLong(UT, 0, 0, 0, 0, GD, GM, GY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        XX = RSAZR(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        GoTo 3740
        
3710    A = SR + NutatLong(GD, GM, GY) - 0.005694
        X = ECRA(A, 0, 0, 0, 0, 0, GD, GM, GY)
        Y = ECDec(A, 0, 0, 0, 0, 0, GD, GM, GY)
        LA = RSLSTR(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        S = eRS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        Return

3740    SunriseAZ = XX
                
End Function

Function TwilightAMLCT(LD As Double, LM As Double, LY As Double, DS As Double, ZC As Double, GL As Double, GP As Double, TT As String) As Double
Dim S As String

        DI = 18#
        If (TT = "C") Or (TT = "c") Then
            DI = 6#
        End If
        If (TT = "N") Or (TT = "n") Then
            DI = 12#
        End If
        
        GD = LctGDay(12, 0, 0, DS, ZC, LD, LM, LY)
        GM = LctGMonth(12, 0, 0, DS, ZC, LD, LM, LY)
        GY = LctGYear(12, 0, 0, DS, ZC, LD, LM, LY)
        SR = SunLong(12, 0, 0, DS, ZC, LD, LM, LY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        
        If (eGSTUT(X, 0, 0, GD, GM, GS) <> "OK") Then
            XX = -99#
            GoTo 3740
        End If
        
        SR = SunLong(UT, 0, 0, 0, 0, GD, GM, GY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = -99#
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        XX = UTLct(UT, 0, 0, DS, ZC, GD, GM, GY)
        GoTo 3740
        
3710    A = SR + NutatLong(GD, GM, GY) - 0.005694
        X = ECRA(A, 0, 0, 0, 0, 0, GD, GM, GY)
        Y = ECDec(A, 0, 0, 0, 0, 0, GD, GM, GY)
        LA = RSLSTR(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        S = eRS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        Return

3740    TwilightAMLCT = XX
                
End Function

Function TwilightPMLCT(LD As Double, LM As Double, LY As Double, DS As Double, ZC As Double, GL As Double, GP As Double, TT As Double) As Double
Dim S As String

        DI = 18#
        If (TT = "C") Or (TT = "c") Then
            DI = 6#
        End If
        If (TT = "N") Or (TT = "n") Then
            DI = 12#
        End If
        
        GD = LctGDay(12, 0, 0, DS, ZC, LD, LM, LY)
        GM = LctGMonth(12, 0, 0, DS, ZC, LD, LM, LY)
        GY = LctGYear(12, 0, 0, DS, ZC, LD, LM, LY)
        SR = SunLong(12, 0, 0, DS, ZC, LD, LM, LY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = 0
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        
        If (eGSTUT(X, 0, 0, GD, GM, GS) <> "OK") Then
            XX = 0
            GoTo 3740
        End If
        
        SR = SunLong(UT, 0, 0, 0, 0, GD, GM, GY)
        GoSub 3710
        
        If (S <> "OK") Then
            XX = 0
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        XX = UTLct(UT, 0, 0, DS, ZC, GD, GM, GY)
        GoTo 3740
        
3710    A = SR + NutatLong(GD, GM, GY) - 0.005694
        X = ECRA(A, 0, 0, 0, 0, 0, GD, GM, GY)
        Y = ECDec(A, 0, 0, 0, 0, 0, GD, GM, GY)
        LA = RSLSTS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        S = eRS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        Return

3740    TwilightPMLCT = XX
                
End Function

Function eTwilight(LD As Double, LM As Double, LY As Double, DS As Double, ZC As Double, GL As Double, GP As Double, TT As Double) As String
Dim S As String

        S = ""
        DI = 18#
        If (TT = "C") Or (TT = "c") Then
            DI = 6#
        End If
        If (TT = "N") Or (TT = "n") Then
            DI = 12#
        End If
        
        GD = LctGDay(12, 0, 0, DS, ZC, LD, LM, LY)
        GM = LctGMonth(12, 0, 0, DS, ZC, LD, LM, LY)
        GY = LctGYear(12, 0, 0, DS, ZC, LD, LM, LY)
        SR = SunLong(12, 0, 0, DS, ZC, LD, LM, LY)
        GoSub 3710
        
        If (S <> "OK") Then
            GoTo 3740
        End If

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        SR = SunLong(UT, 0, 0, 0, 0, GD, GM, GY)
        GoSub 3710
        GoTo 3740

        X = LSTGST(LA, 0, 0, GL): UT = GSTUT(X, 0, 0, GD, GM, GY)
        
        If (eGSTUT(X, 0, 0, GD, GM, GS) <> "OK") Then
            S = S + " GST to UT conversion warning"
            GoTo 3740
        End If
        
        GoTo 3740
        
3710    A = SR + NutatLong(GD, GM, GY) - 0.005694
        X = ECRA(A, 0, 0, 0, 0, 0, GD, GM, GY)
        Y = ECDec(A, 0, 0, 0, 0, 0, GD, GM, GY)
        LA = RSLSTR(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        S = eRS(DDDH(X), 0, 0, Y, 0, 0, DI, GP)
        
        If (Left$(S, 4) = "** c") Then
            S = "** lasts all night"
         Else
         If (Left$(S, 4) = "** n") Then
             S = "** Sun too far below horizon"
         End If
        End If
        
        Return

3740    eTwilight = S
                
End Function

Function EqOfTime(GD As Double, GM As Double, GY As Double) As Double

    A = SunLong(12, 0, 0, 0, 0, GD, GM, GY)
    B = DDDH(ECRA(A, 0, 0, 0, 0, 0, GD, GM, GY))
    C = GSTUT(B, 0, 0, GD, GM, GY)
    EqOfTime = C - 12#
    
End Function

Function HeliogLong(TH As Double, RH As Double, GD As Double, GM As Double, GY As Double) As Double

    A = CDJD(GD, GM, GY)
    B = (A - 2415020#) / 36525#
    C = DMSDD(74, 22, 0) + (84# * B / 60#)
    D = SunLong(0, 0, 0, 0, 0, GD, GM, GY)
    E = Sin(Radians(C - D)) * Cos(Radians(DMSDD(7, 15, 0)))
    F = -Cos(Radians(C - D))
    G = Degrees(Atan2(F, E))
    H = 360# - (360# * (A - 2398220#) / 25.38)
    I = H - 360# * Int(H / 360#)
    J = I + G
    K = J - 360# * Int(J / 360#)
    L = Asin(Sin(Radians(D - C)) * Sin(Radians(DMSDD(7, 15, 0))))
    M = Degrees(Atn(-Cos(Radians(D)) * Tan(Radians(Obliq(GD, GM, GY)))))
    N = Degrees(Atn(-Cos(Radians(C - D)) * Tan(Radians(DMSDD(7, 15, 0)))))
    O = M + N
    P = RH / 60#
    Q = Asin(2# * P / SunDia(0, 0, 0, 0, 0, GD, GM, GY)) - Radians(P)
    R = Degrees(Asin(Sin(L) * Cos(Q) + Cos(L) * Sin(Q) * Cos(Radians(O - TH))))
    S = Degrees(Asin(Sin(Q) * Sin(Radians(O - TH)) / Cos(Radians(R)))) + J
    T = S - 360# * Int(S / 360#)
    HeliogLong = T

End Function

Function HeliogLat(TH As Double, RH As Double, GD As Double, GM As Double, GY As Double) As Double

    A = CDJD(GD, GM, GY)
    B = (A - 2415020#) / 36525#
    C = DMSDD(74, 22, 0) + (84# * B / 60#)
    D = SunLong(0, 0, 0, 0, 0, GD, GM, GY)
    E = Sin(Radians(C - D)) * Cos(Radians(DMSDD(7, 15, 0)))
    F = -Cos(Radians(C - D))
    G = Degrees(Atan2(F, E))
    H = 360# - (360# * (A - 2398220#) / 25.38)
    I = H - 360# * Int(H / 360#)
    J = I + G
    K = J - 360# * Int(J / 360#)
    L = Asin(Sin(Radians(D - C)) * Sin(Radians(DMSDD(7, 15, 0))))
    M = Degrees(Atn(-Cos(Radians(D)) * Tan(Radians(Obliq(GD, GM, GY)))))
    N = Degrees(Atn(-Cos(Radians(C - D)) * Tan(Radians(DMSDD(7, 15, 0)))))
    O = M + N
    P = RH / 60#
    Q = Asin(2# * P / SunDia(0, 0, 0, 0, 0, GD, GM, GY)) - Radians(P)
    R = Degrees(Asin(Sin(L) * Cos(Q) + Cos(L) * Sin(Q) * Cos(Radians(O - TH))))
    HeliogLat = R

End Function

Function CRN(GD As Double, GM As Double, GY As Double) As Double

    A = CDJD(GD, GM, GY)
    CRN = 1690 + Round((A - 2444235.34) / 27.2753, 0)
    
End Function
Function Unwind(W As Double) As Double

    Unwind = W - 6.283185308 * Int(W / 6.283185308)
    
End Function
Function PlanetLong(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, S As String) As Double

Const a11 = 178.179078, a12 = 415.2057519, a13 = 0.0003011, a14 = 0
Const a21 = 75.899697, a22 = 1.5554889, a23 = 0.0002947, a24 = 0
Const a31 = 0.20561421, a32 = 0.00002046, a33 = -0.00000003, a34 = 0
Const a41 = 7.002881, a42 = 0.0018608, a43 = -0.0000183, a44 = 0
Const a51 = 47.145944, a52 = 1.1852083, a53 = 0.0001739, a54 = 0
Const a61 = 0.3870986, a62 = 6.74, a63 = -0.42

Const b11 = 342.767053, b12 = 162.5533664, b13 = 0.0003097, b14 = 0
Const b21 = 130.163833, b22 = 1.4080361, b23 = -0.0009764, b24 = 0
Const b31 = 0.00682069, b32 = -0.00004774, b33 = 0.000000091, b34 = 0
Const b41 = 3.393631, b42 = 0.0010058, b43 = -0.000001, b44 = 0
Const b51 = 75.779647, b52 = 0.89985, b53 = 0.00041, b54 = 0
Const b61 = 0.7233316, b62 = 16.92, b63 = -4.4

Const c11 = 293.737334, c12 = 53.17137642, c13 = 0.0003107, c14 = 0
Const c21 = 334.218203, c22 = 1.8407584, c23 = 0.0001299, c24 = -0.00000119
Const c31 = 0.0933129, c32 = 0.000092064, c33 = -0.000000077, c34 = 0
Const c41 = 1.850333, c42 = -0.000675, c43 = 0.0000126, c44 = 0
Const c51 = 48.786442, c52 = 0.7709917, c53 = -0.0000014, c54 = -0.00000533
Const c61 = 1.5236883, c62 = 9.36, c63 = -1.52

Const d11 = 238.049257, d12 = 8.434172183, d13 = 0.0003347, d14 = -0.00000165
Const d21 = 12.720972, d22 = 1.6099617, d23 = 0.00105627, d24 = -0.00000343
Const d31 = 0.04833475, d32 = 0.00016418, d33 = -0.0000004676, d34 = -0.0000000017
Const d41 = 1.308736, d42 = -0.0056961, d43 = 0.0000039, d44 = 0
Const d51 = 99.443414, d52 = 1.01053, d53 = 0.00035222, d54 = -0.00000851
Const d61 = 5.202561, d62 = 196.74, d63 = -9.4

Const e11 = 266.564377, e12 = 3.398638567, e13 = 0.0003245, e14 = -0.0000058
Const e21 = 91.098214, e22 = 1.9584158, e23 = 0.00082636, e24 = 0.00000461
Const e31 = 0.05589232, e32 = -0.0003455, e33 = -0.000000728, e34 = 0.00000000074
Const e41 = 2.492519, e42 = -0.0039189, e43 = -0.00001549, e44 = 0.00000004
Const e51 = 112.790414, e52 = 0.8731951, e53 = -0.00015218, e54 = -0.00000531
Const e61 = 9.554747, e62 = 165.6, e63 = -8.88

Const f11 = 244.19747, f12 = 1.194065406, f13 = 0.000316, f14 = -0.0000006
Const f21 = 171.548692, f22 = 1.4844328, f23 = 0.0002372, f24 = -0.00000061
Const f31 = 0.0463444, f32 = -0.00002658, f33 = 0.000000077, f34 = 0
Const f41 = 0.772464, f42 = 0.0006253, f43 = 0.0000395, f44 = 0
Const f51 = 73.477111, f52 = 0.4986678, f53 = 0.0013117, f54 = 0
Const f61 = 19.21814, f62 = 65.8, f63 = -7.19

Const g11 = 84.457994, g12 = 0.6107942056, g13 = 0.0003205, g14 = -0.0000006
Const g21 = 46.727364, g22 = 1.4245744, g23 = 0.00039082, g24 = -0.000000605
Const g31 = 0.00899704, g32 = 0.00000633, g33 = -0.000000002, g34 = 0
Const g41 = 1.779242, g42 = -0.0095436, g43 = -0.0000091, g44 = 0
Const g51 = 130.681389, g52 = 1.098935, g53 = 0.00024987, g54 = -0.000004718
Const g61 = 30.10957, g62 = 62.2, g63 = -6.87

Dim IP As Integer, I As Integer, J As Integer, K As Integer
Dim PL(7, 9) As Double, AP(7) As Double

        IP = 0: B = LctUT(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        A = CDJD(GD, GM, GY)
        T = ((A - 2415020#) / 36525#) + (B / 876600#)

        Select Case UCase(S)
            Case "MERCURY"
                IP = 1
           Case "VENUS"
                IP = 2
            Case "MARS"
                IP = 3
           Case "JUPITER"
                IP = 4
            Case "SATURN"
                IP = 5
            Case "URANUS"
                IP = 6
            Case "NEPTUNE"
                IP = 7
            Case Else
                EP = 0
                GoTo 5700
        End Select
    
        I = 1
        A0 = a11: A1 = a12: A2 = a13: A3 = a14
        B0 = a21: B1 = a22: B2 = a23: B3 = a24
        C0 = a31: C1 = a32: C2 = a33: C3 = a34
        D0 = a41: D1 = a42: D2 = a43: D3 = a44
        E0 = a51: E1 = a52: E2 = a53: E3 = a54
        F = a61: G = a62: H = a63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 2
        A0 = b11: A1 = b12: A2 = b13: A3 = b14
        B0 = b21: B1 = b22: B2 = b23: B3 = b24
        C0 = b31: C1 = b32: C2 = b33: C3 = b34
        D0 = b41: D1 = b42: D2 = b43: D3 = b44
        E0 = b51: E1 = b52: E2 = b53: E3 = b54
        F = b61: G = b62: H = b63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 3
        A0 = c11: A1 = c12: A2 = c13: A3 = c14
        B0 = c21: B1 = c22: B2 = c23: B3 = c24
        C0 = c31: C1 = c32: C2 = c33: C3 = c34
        D0 = c41: D1 = c42: D2 = c43: D3 = c44
        E0 = c51: E1 = c52: E2 = c53: E3 = c54
        F = c61: G = c62: H = c63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 4
        A0 = d11: A1 = d12: A2 = d13: A3 = d14
        B0 = d21: B1 = d22: B2 = d23: B3 = d24
        C0 = d31: C1 = d32: C2 = d33: C3 = d34
        D0 = d41: D1 = d42: D2 = d43: D3 = d44
        E0 = d51: E1 = d52: E2 = d53: E3 = d54
        F = d61: G = d62: H = d63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 5
        A0 = e11: A1 = e12: A2 = e13: A3 = e14
        B0 = e21: B1 = e22: B2 = e23: B3 = e24
        C0 = e31: C1 = e32: C2 = e33: C3 = e34
        D0 = e41: D1 = e42: D2 = e43: D3 = e44
        E0 = e51: E1 = e52: E2 = e53: E3 = e54
        F = e61: G = e62: H = e63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 6
        A0 = f11: A1 = f12: A2 = f13: A3 = f14
        B0 = f21: B1 = f22: B2 = f23: B3 = f24
        C0 = f31: C1 = f32: C2 = f33: C3 = f34
        D0 = f41: D1 = f42: D2 = f43: D3 = f44
        E0 = f51: E1 = f52: E2 = f53: E3 = f54
        F = f61: G = f62: H = f63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 7
        A0 = g11: A1 = g12: A2 = g13: A3 = g14
        B0 = g21: B1 = g22: B2 = g23: B3 = g24
        C0 = g31: C1 = g32: C2 = g33: C3 = g34
        D0 = g41: D1 = g42: D2 = g43: D3 = g44
        E0 = g51: E1 = g52: E2 = g53: E3 = g54
        F = g61: G = g62: H = g63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        LI = 0: Pi = 3.1415926536: TP = 2# * Pi
        MS = SunMeanAnomaly(LH, LM, LS, DS, ZC, DY, MN, YR)
        SR = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR))
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR): LG = SR + Pi

        For K = 1 To 2

        For J = 1 To 7
        AP(J) = Radians(PL(J, 1) - PL(J, 3) - LI * PL(J, 2))
        Next J

        QA = 0: QB = 0: QC = 0: QD = 0: QE = 0: QF = 0: QG = 0
        If (IP < 5) Then
            On IP GoSub 4685, 4735, 4810, 4945
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoSub 4945, 4945, 4945
        End If

        EC = PL(IP, 4) + QD: AM = AP(IP) + QE: AT = TrueAnomaly(AM, EC)
        PVV = (PL(IP, 7) + QF) * (1# - EC * EC) / (1# + EC * Cos(AT))
        LP = Degrees(AT) + PL(IP, 3) + Degrees(QC - QE): LP = Radians(LP)
        OM = Radians(PL(IP, 6)): LO = LP - OM
        SO = Sin(LO): CO = Cos(LO)
        INN = Radians(PL(IP, 5)): PVV = PVV + QB
        SP = SO * Sin(INN): Y = SO * Cos(INN)
        PS = Asin(SP) + QG: SP = Sin(PS)
        PD = Atan2(CO, Y) + OM + Radians(QA)
        PD = Unwind(PD)
        CI = Cos(PS): RD = PVV * CI: LL = PD - LG
        RH = RE * RE + PVV * PVV - 2# * RE * PVV * CI * Cos(LL)
        RH = Sqr(RH): LI = RH * 0.005775518

        If (K = 1) Then
            L0 = PD
            V0 = RH
            S0 = PS
            P0 = PVV
        End If

        Next K

        L1 = Sin(LL): l2 = Cos(LL)
        
        If (IP < 3) Then
            EP = Atn(-1# * RD * L1 / (RE - RD * l2)) + LG + Pi
        Else
            EP = Atn(RE * L1 / (RD - RE * l2)) + PD
        End If

        EP = Unwind(EP)
        BP = Atn(RD * SP * Sin(EP - PD) / (CI * RE * L1))
        GoTo 5700

4685    QA = 0.00204 * Cos(5 * AP(2) - 2 * AP(1) + 0.21328)
        QA = QA + 0.00103 * Cos(2 * AP(2) - AP(1) - 2.8046)
        QA = QA + 0.00091 * Cos(2 * AP(4) - AP(1) - 0.64582)
        QA = QA + 0.00078 * Cos(5 * AP(2) - 3 * AP(1) + 0.17692)

        QB = 0.000007525 * Cos(2 * AP(4) - AP(1) + 0.925251)
        QB = QB + 0.000006802 * Cos(5 * AP(2) - 3 * AP(1) - 4.53642)
        QB = QB + 0.000005457 * Cos(2 * AP(2) - 2 * AP(1) - 1.24246)
        QB = QB + 0.000003569 * Cos(5 * AP(2) - AP(1) - 1.35699)
        Return

4735    QC = 0.00077 * Sin(4.1406 + T * 2.6227): QC = Radians(QC): QE = QC

        QA = 0.00313 * Cos(2 * MS - 2 * AP(2) - 2.587)
        QA = QA + 0.00198 * Cos(3 * MS - 3 * AP(2) + 0.044768)
        QA = QA + 0.00136 * Cos(MS - AP(2) - 2.0788)
        QA = QA + 0.00096 * Cos(3 * MS - 2 * AP(2) - 2.3721)
        QA = QA + 0.00082 * Cos(AP(4) - AP(2) - 3.6318)

        QB = 0.000022501 * Cos(2 * MS - 2 * AP(2) - 1.01592)
        QB = QB + 0.000019045 * Cos(3 * MS - 3 * AP(2) + 1.61577)
        QB = QB + 0.000006887 * Cos(AP(4) - AP(2) - 2.06106)
        QB = QB + 0.000005172 * Cos(MS - AP(2) - 0.508065)
        QB = QB + 0.00000362 * Cos(5 * MS - 4 * AP(2) - 1.81877)
        QB = QB + 0.000003283 * Cos(4 * MS - 4 * AP(2) + 1.10851)
        QB = QB + 0.000003074 * Cos(2 * AP(4) - 2 * AP(2) - 0.962846)
        Return

4810    A = 3 * AP(4) - 8 * AP(3) + 4 * MS: SA = Sin(A): CA = Cos(A)
        QC = -(0.01133 * SA + 0.00933 * CA): QC = Radians(QC): QE = QC

        QA = 0.00705 * Cos(AP(4) - AP(3) - 0.85448)
        QA = QA + 0.00607 * Cos(2 * AP(4) - AP(3) - 3.2873)
        QA = QA + 0.00445 * Cos(2 * AP(4) - 2 * AP(3) - 3.3492)
        QA = QA + 0.00388 * Cos(MS - 2 * AP(3) + 0.35771)
        QA = QA + 0.00238 * Cos(MS - AP(3) + 0.61256)
        QA = QA + 0.00204 * Cos(2 * MS - 3 * AP(3) + 2.7688)
        QA = QA + 0.00177 * Cos(3 * AP(3) - AP(2) - 1.0053)
        QA = QA + 0.00136 * Cos(2 * MS - 4 * AP(3) + 2.6894)
        QA = QA + 0.00104 * Cos(AP(4) + 0.30749)

        QB = 0.000053227 * Cos(AP(4) - AP(3) + 0.717864)
        QB = QB + 0.000050989 * Cos(2 * AP(4) - 2 * AP(3) - 1.77997)
        QB = QB + 0.000038278 * Cos(2 * AP(4) - AP(3) - 1.71617)
        QB = QB + 0.000015996 * Cos(MS - AP(3) - 0.969618)
        QB = QB + 0.000014764 * Cos(2 * MS - 3 * AP(3) + 1.19768)
        QB = QB + 0.000008966 * Cos(AP(4) - 2 * AP(3) + 0.761225)
        QB = QB + 0.000007914 * Cos(3 * AP(4) - 2 * AP(3) - 2.43887)
        QB = QB + 0.000007004 * Cos(2 * AP(4) - 3 * AP(3) - 1.79573)
        QB = QB + 0.00000662 * Cos(MS - 2 * AP(3) + 1.97575)
        QB = QB + 0.00000493 * Cos(3 * AP(4) - 3 * AP(3) - 1.33069)
        QB = QB + 0.000004693 * Cos(3 * MS - 5 * AP(3) + 3.32665)
        QB = QB + 0.000004571 * Cos(2 * MS - 4 * AP(3) + 4.27086)
        QB = QB + 0.000004409 * Cos(3 * AP(4) - AP(3) - 2.02158)
        Return

4945    J1 = T / 5# + 0.1: J2 = Unwind(4.14473 + 52.9691 * T)
        J3 = Unwind(4.641118 + 21.32991 * T)
        J4 = Unwind(4.250177 + 7.478172 * T)
        J5 = 5# * J3 - 2# * J2: J6 = 2# * J2 - 6# * J3 + 3# * J4

        If (IP < 5) Then
            On IP GoTo 5190, 5190, 5190, 4980
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoTo 4980, 5505, 5505, 5190
        End If

4980    J7 = J3 - J2: U1 = Sin(J3): U2 = Cos(J3): U3 = Sin(2# * J3)
        U4 = Cos(2# * J3): U5 = Sin(J5): U6 = Cos(J5)
        U7 = Sin(2# * J5): U8 = Sin(J6): U9 = Sin(J7)
        UA = Cos(J7): UB = Sin(2# * J7): UC = Cos(2# * J7)
        UD = Sin(3# * J7): UE = Cos(3# * J7): UF = Sin(4# * J7)
        UG = Cos(4# * J7): VH = Cos(5# * J7)
        
        If (IP = 5) Then
            GoTo 5200
        End If

        QC = (0.331364 - (0.010281 + 0.004692 * J1) * J1) * U5
        QC = QC + (0.003228 - (0.064436 - 0.002075 * J1) * J1) * U6
        QC = QC - (0.003083 + (0.000275 - 0.000489 * J1) * J1) * U7
        QC = QC + 0.002472 * U8 + 0.013619 * U9 + 0.018472 * UB
        QC = QC + 0.006717 * UD + 0.002775 * UF + 0.006417 * UB * U1
        QC = QC + (0.007275 - 0.001253 * J1) * U9 * U1 + 0.002439 * UD * U1
        QC = QC - (0.035681 + 0.001208 * J1) * U9 * U2 - 0.003767 * UC * U1
        QC = QC - (0.033839 + 0.001125 * J1) * UA * U1 - 0.004261 * UB * U2
        QC = QC + (0.001161 * J1 - 0.006333) * UA * U2 + 0.002178 * U2
        QC = QC - 0.006675 * UC * U2 - 0.002664 * UE * U2 - 0.002572 * U9 * U3
        QC = QC - 0.003567 * UB * U3 + 0.002094 * UA * U4 + 0.003342 * UC * U4
        QC = Radians(QC)

        QD = (3606 + (130 - 43 * J1) * J1) * U5 + (1289 - 580 * J1) * U6
        QD = QD - 6764 * U9 * U1 - 1110 * UB * U1 - 224 * UD * U1 - 204 * U1
        QD = QD + (1284 + 116 * J1) * UA * U1 + 188 * UC * U1
        QD = QD + (1460 + 130 * J1) * U9 * U2 + 224 * UB * U2 - 817 * U2
        QD = QD + 6074 * U2 * UA + 992 * UC * U2 + 508 * UE * U2 + 230 * UG * U2
        QD = QD + 108 * VH * U2 - (956 + 73 * J1) * U9 * U3 + 448 * UB * U3
        QD = QD + 137 * UD * U3 + (108 * J1 - 997) * UA * U3 + 480 * UC * U3
        QD = QD + 148 * UE * U3 + (99 * J1 - 956) * U9 * U4 + 490 * UB * U4
        QD = QD + 158 * UD * U4 + 179 * U4 + (1024 + 75 * J1) * UA * U4
        QD = QD - 437 * UC * U4 - 132 * UE * U4: QD = QD * 0.0000001

        VK = (0.007192 - 0.003147 * J1) * U5 - 0.004344 * U1
        VK = VK + (J1 * (0.000197 * J1 - 0.000675) - 0.020428) * U6
        VK = VK + 0.034036 * UA * U1 + (0.007269 + 0.000672 * J1) * U9 * U1
        VK = VK + 0.005614 * UC * U1 + 0.002964 * UE * U1 + 0.037761 * U9 * U2
        VK = VK + 0.006158 * UB * U2 - 0.006603 * UA * U2 - 0.005356 * U9 * U3
        VK = VK + 0.002722 * UB * U3 + 0.004483 * UA * U3
        VK = VK - 0.002642 * UC * U3 + 0.004403 * U9 * U4
        VK = VK - 0.002536 * UB * U4 + 0.005547 * UA * U4 - 0.002689 * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 205 * UA - 263 * U6 + 693 * UC + 312 * UE + 147 * UG + 299 * U9 * U1
        QF = QF + 181 * UC * U1 + 204 * UB * U2 + 111 * UD * U2 - 337 * UA * U2
        QF = QF - 111 * UC * U2: QF = QF * 0.000001
5190    Return

5200    UI = Sin(3 * J3): UJ = Cos(3 * J3): UK = Sin(4 * J3)
        UL = Cos(4 * J3): VI = Cos(2 * J5): UN = Sin(5 * J7)
        J8 = J4 - J3: UO = Sin(2 * J8): UP = Cos(2 * J8)
        UQ = Sin(3 * J8): UR = Cos(3 * J8)

        QC = 0.007581 * U7 - 0.007986 * U8 - 0.148811 * U9
        QC = QC - (0.814181 - (0.01815 - 0.016714 * J1) * J1) * U5
        QC = QC - (0.010497 - (0.160906 - 0.0041 * J1) * J1) * U6
        QC = QC - 0.015208 * UD - 0.006339 * UF - 0.006244 * U1
        QC = QC - 0.0165 * UB * U1 - 0.040786 * UB
        QC = QC + (0.008931 + 0.002728 * J1) * U9 * U1 - 0.005775 * UD * U1
        QC = QC + (0.081344 + 0.003206 * J1) * UA * U1 + 0.015019 * UC * U1
        QC = QC + (0.085581 + 0.002494 * J1) * U9 * U2 + 0.014394 * UC * U2
        QC = QC + (0.025328 - 0.003117 * J1) * UA * U2 + 0.006319 * UE * U2
        QC = QC + 0.006369 * U9 * U3 + 0.009156 * UB * U3 + 0.007525 * UQ * U3
        QC = QC - 0.005236 * UA * U4 - 0.007736 * UC * U4 - 0.007528 * UR * U4
        QC = Radians(QC)

        QD = (-7927 + (2548 + 91 * J1) * J1) * U5
        QD = QD + (13381 + (1226 - 253 * J1) * J1) * U6 + (248 - 121 * J1) * U7
        QD = QD - (305 + 91 * J1) * VI + 412 * UB + 12415 * U1
        QD = QD + (390 - 617 * J1) * U9 * U1 + (165 - 204 * J1) * UB * U1
        QD = QD + 26599 * UA * U1 - 4687 * UC * U1 - 1870 * UE * U1 - 821 * UG * U1
        QD = QD - 377 * VH * U1 + 497 * UP * U1 + (163 - 611 * J1) * U2
        QD = QD - 12696 * U9 * U2 - 4200 * UB * U2 - 1503 * UD * U2 - 619 * UF * U2
        QD = QD - 268 * UN * U2 - (282 + 1306 * J1) * UA * U2
        QD = QD + (-86 + 230 * J1) * UC * U2 + 461 * UO * U2 - 350 * U3
        QD = QD + (2211 - 286 * J1) * U9 * U3 - 2208 * UB * U3 - 568 * UD * U3
        QD = QD - 346 * UF * U3 - (2780 + 222 * J1) * UA * U3
        QD = QD + (2022 + 263 * J1) * UC * U3 + 248 * UE * U3 + 242 * UQ * U3
        QD = QD + 467 * UR * U3 - 490 * U4 - (2842 + 279 * J1) * U9 * U4
        QD = QD + (128 + 226 * J1) * UB * U4 + 224 * UD * U4
        QD = QD + (-1594 + 282 * J1) * UA * U4 + (2162 - 207 * J1) * UC * U4
        QD = QD + 561 * UE * U4 + 343 * UG * U4 + 469 * UQ * U4 - 242 * UR * U4
        QD = QD - 205 * U9 * UI + 262 * UD * UI + 208 * UA * UJ - 271 * UE * UJ
        QD = QD - 382 * UE * UK - 376 * UD * UL: QD = QD * 0.0000001

        VK = (0.077108 + (0.007186 - 0.001533 * J1) * J1) * U5
        VK = VK - 0.007075 * U9
        VK = VK + (0.045803 - (0.014766 + 0.000536 * J1) * J1) * U6
        VK = VK - 0.072586 * U2 - 0.075825 * U9 * U1 - 0.024839 * UB * U1
        VK = VK - 0.008631 * UD * U1 - 0.150383 * UA * U2
        VK = VK + 0.026897 * UC * U2 + 0.010053 * UE * U2
        VK = VK - (0.013597 + 0.001719 * J1) * U9 * U3 + 0.011981 * UB * U4
        VK = VK - (0.007742 - 0.001517 * J1) * UA * U3
        VK = VK + (0.013586 - 0.001375 * J1) * UC * U3
        VK = VK - (0.013667 - 0.001239 * J1) * U9 * U4
        VK = VK + (0.014861 + 0.001136 * J1) * UA * U4
        VK = VK - (0.013064 + 0.001628 * J1) * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 572 * U5 - 1590 * UB * U2 + 2933 * U6 - 647 * UD * U2
        QF = QF + 33629 * UA - 344 * UF * U2 - 3081 * UC + 2885 * UA * U2
        QF = QF - 1423 * UE + (2172 + 102 * J1) * UC * U2 - 671 * UG
        QF = QF + 296 * UE * U2 - 320 * VH - 267 * UB * U3 + 1098 * U1
        QF = QF - 778 * UA * U3 - 2812 * U9 * U1 + 495 * UC * U3 + 688 * UB * U1
        QF = QF + 250 * UE * U3 - 393 * UD * U1 - 856 * U9 * U4 - 228 * UF * U1
        QF = QF + 441 * UB * U4 + 2138 * UA * U1 + 296 * UC * U4 - 999 * UC * U1
        QF = QF + 211 * UE * U4 - 642 * UE * U1 - 427 * U9 * UI - 325 * UG * U1
        QF = QF + 398 * UD * UI - 890 * U2 + 344 * UA * UJ + 2206 * U9 * U2
        QF = QF - 427 * UE * UJ: QF = QF * 0.000001

        QG = 0.000747 * UA * U1 + 0.001069 * UA * U2 + 0.002108 * UB * U3
        QG = QG + 0.001261 * UC * U3 + 0.001236 * UB * U4 - 0.002075 * UC * U4
        QG = Radians(QG): Return

5505    J8 = Unwind(1.46205 + 3.81337 * T): J9 = 2 * J8 - J4
        VJ = Sin(J9): UU = Cos(J9): UV = Sin(2 * J9)
        UW = Cos(2 * J9)
        
        If (IP = 7) Then
            GoTo 5675
        End If

        JA = J4 - J2: JB = J4 - J3: JC = J8 - J4
        QC = (0.864319 - 0.001583 * J1) * VJ
        QC = QC + (0.082222 - 0.006833 * J1) * UU + 0.036017 * UV
        QC = QC - 0.003019 * UW + 0.008122 * Sin(J6): QC = Radians(QC)

        VK = 0.120303 * VJ + 0.006197 * UV
        VK = VK + (0.019472 - 0.000947 * J1) * UU
        QE = QC - (Radians(VK) / PL(IP, 4))

        QD = (163 * J1 - 3349) * VJ + 20981 * UU + 1311 * UW: QD = QD * 0.0000001

        QF = -0.003825 * UU

        QA = (-0.038581 + (0.002031 - 0.00191 * J1) * J1) * Cos(J4 + JB)
        QA = QA + (0.010122 - 0.000988 * J1) * Sin(J4 + JB)
        A = (0.034964 - (0.001038 - 0.000868 * J1) * J1) * Cos(2 * J4 + JB)
        QA = A + QA + 0.005594 * Sin(J4 + 3 * JC) - 0.014808 * Sin(JA)
        QA = QA - 0.005794 * Sin(JB) + 0.002347 * Cos(JB)
        QA = QA + 0.009872 * Sin(JC) + 0.008803 * Sin(2 * JC)
        QA = QA - 0.004308 * Sin(3 * JC)

        UX = Sin(JB): UY = Cos(JB): UZ = Sin(J4)
        VA = Cos(J4): VB = Sin(2 * J4): VC = Cos(2 * J4)
        QG = (0.000458 * UX - 0.000642 * UY - 0.000517 * Cos(4 * JC)) * UZ
        QG = QG - (0.000347 * UX + 0.000853 * UY + 0.000517 * Sin(4 * JB)) * VA
        QG = QG + 0.000403 * (Cos(2 * JC) * VB + Sin(2 * JC) * VC)
        QG = Radians(QG)

        QB = -25948 + 4985 * Cos(JA) - 1230 * VA + 3354 * UY
        QB = QB + 904 * Cos(2 * JC) + 894 * (Cos(JC) - Cos(3 * JC))
        QB = QB + (5795 * VA - 1165 * UZ + 1388 * VC) * UX
        QB = QB + (1351 * VA + 5702 * UZ + 1388 * VB) * UY
        QB = QB * 0.000001: Return

5675    JA = J8 - J2: JB = J8 - J3: JC = J8 - J4
        QC = (0.001089 * J1 - 0.589833) * VJ
        QC = QC + (0.004658 * J1 - 0.056094) * UU - 0.024286 * UV
        QC = Radians(QC)

        VK = 0.024039 * VJ - 0.025303 * UU + 0.006206 * UV
        VK = VK - 0.005992 * UW: QE = QC - (Radians(VK) / PL(IP, 4))

        QD = 4389 * VJ + 1129 * UV + 4262 * UU + 1089 * UW
        QD = QD * 0.0000001

        QF = 8189 * UU - 817 * VJ + 781 * UW: QF = QF * 0.000001

        VD = Sin(2 * JC): VE = Cos(2 * JC)
        VF = Sin(J8): VG = Cos(J8)
        QA = -0.009556 * Sin(JA) - 0.005178 * Sin(JB)
        QA = QA + 0.002572 * VD - 0.002972 * VE * VF - 0.002833 * VD * VG

        QG = 0.000336 * VE * VF + 0.000364 * VD * VG: QG = Radians(QG)

        QB = -40596 + 4992 * Cos(JA) + 2744 * Cos(JB)
        QB = QB + 2044 * Cos(JC) + 1051 * VE: QB = QB * 0.000001

        Return
        
5700    PlanetLong = Degrees(Unwind(EP))

End Function

Function PlanetLat(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, S As String) As Double

Const a11 = 178.179078, a12 = 415.2057519, a13 = 0.0003011, a14 = 0
Const a21 = 75.899697, a22 = 1.5554889, a23 = 0.0002947, a24 = 0
Const a31 = 0.20561421, a32 = 0.00002046, a33 = -0.00000003, a34 = 0
Const a41 = 7.002881, a42 = 0.0018608, a43 = -0.0000183, a44 = 0
Const a51 = 47.145944, a52 = 1.1852083, a53 = 0.0001739, a54 = 0
Const a61 = 0.3870986, a62 = 6.74, a63 = -0.42

Const b11 = 342.767053, b12 = 162.5533664, b13 = 0.0003097, b14 = 0
Const b21 = 130.163833, b22 = 1.4080361, b23 = -0.0009764, b24 = 0
Const b31 = 0.00682069, b32 = -0.00004774, b33 = 0.000000091, b34 = 0
Const b41 = 3.393631, b42 = 0.0010058, b43 = -0.000001, b44 = 0
Const b51 = 75.779647, b52 = 0.89985, b53 = 0.00041, b54 = 0
Const b61 = 0.7233316, b62 = 16.92, b63 = -4.4

Const c11 = 293.737334, c12 = 53.17137642, c13 = 0.0003107, c14 = 0
Const c21 = 334.218203, c22 = 1.8407584, c23 = 0.0001299, c24 = -0.00000119
Const c31 = 0.0933129, c32 = 0.000092064, c33 = -0.000000077, c34 = 0
Const c41 = 1.850333, c42 = -0.000675, c43 = 0.0000126, c44 = 0
Const c51 = 48.786442, c52 = 0.7709917, c53 = -0.0000014, c54 = -0.00000533
Const c61 = 1.5236883, c62 = 9.36, c63 = -1.52

Const d11 = 238.049257, d12 = 8.434172183, d13 = 0.0003347, d14 = -0.00000165
Const d21 = 12.720972, d22 = 1.6099617, d23 = 0.00105627, d24 = -0.00000343
Const d31 = 0.04833475, d32 = 0.00016418, d33 = -0.0000004676, d34 = -0.0000000017
Const d41 = 1.308736, d42 = -0.0056961, d43 = 0.0000039, d44 = 0
Const d51 = 99.443414, d52 = 1.01053, d53 = 0.00035222, d54 = -0.00000851
Const d61 = 5.202561, d62 = 196.74, d63 = -9.4

Const e11 = 266.564377, e12 = 3.398638567, e13 = 0.0003245, e14 = -0.0000058
Const e21 = 91.098214, e22 = 1.9584158, e23 = 0.00082636, e24 = 0.00000461
Const e31 = 0.05589232, e32 = -0.0003455, e33 = -0.000000728, e34 = 0.00000000074
Const e41 = 2.492519, e42 = -0.0039189, e43 = -0.00001549, e44 = 0.00000004
Const e51 = 112.790414, e52 = 0.8731951, e53 = -0.00015218, e54 = -0.00000531
Const e61 = 9.554747, e62 = 165.6, e63 = -8.88

Const f11 = 244.19747, f12 = 1.194065406, f13 = 0.000316, f14 = -0.0000006
Const f21 = 171.548692, f22 = 1.4844328, f23 = 0.0002372, f24 = -0.00000061
Const f31 = 0.0463444, f32 = -0.00002658, f33 = 0.000000077, f34 = 0
Const f41 = 0.772464, f42 = 0.0006253, f43 = 0.0000395, f44 = 0
Const f51 = 73.477111, f52 = 0.4986678, f53 = 0.0013117, f54 = 0
Const f61 = 19.21814, f62 = 65.8, f63 = -7.19

Const g11 = 84.457994, g12 = 0.6107942056, g13 = 0.0003205, g14 = -0.0000006
Const g21 = 46.727364, g22 = 1.4245744, g23 = 0.00039082, g24 = -0.000000605
Const g31 = 0.00899704, g32 = 0.00000633, g33 = -0.000000002, g34 = 0
Const g41 = 1.779242, g42 = -0.0095436, g43 = -0.0000091, g44 = 0
Const g51 = 130.681389, g52 = 1.098935, g53 = 0.00024987, g54 = -0.000004718
Const g61 = 30.10957, g62 = 62.2, g63 = -6.87

Dim IP As Integer, I As Integer, J As Integer, K As Integer
Dim PL(7, 9) As Double, AP(7) As Double

        IP = 0: B = LctUT(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        A = CDJD(GD, GM, GY)
        T = ((A - 2415020#) / 36525#) + (B / 876600#)

        Select Case UCase(S)
            Case "MERCURY"
                IP = 1
           Case "VENUS"
                IP = 2
            Case "MARS"
                IP = 3
           Case "JUPITER"
                IP = 4
            Case "SATURN"
                IP = 5
            Case "URANUS"
                IP = 6
            Case "NEPTUNE"
                IP = 7
            Case Else
                EP = 0
                GoTo 5700
        End Select
   
        I = 1
        A0 = a11: A1 = a12: A2 = a13: A3 = a14
        B0 = a21: B1 = a22: B2 = a23: B3 = a24
        C0 = a31: C1 = a32: C2 = a33: C3 = a34
        D0 = a41: D1 = a42: D2 = a43: D3 = a44
        E0 = a51: E1 = a52: E2 = a53: E3 = a54
        F = a61: G = a62: H = a63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 2
        A0 = b11: A1 = b12: A2 = b13: A3 = b14
        B0 = b21: B1 = b22: B2 = b23: B3 = b24
        C0 = b31: C1 = b32: C2 = b33: C3 = b34
        D0 = b41: D1 = b42: D2 = b43: D3 = b44
        E0 = b51: E1 = b52: E2 = b53: E3 = b54
        F = b61: G = b62: H = b63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 3
        A0 = c11: A1 = c12: A2 = c13: A3 = c14
        B0 = c21: B1 = c22: B2 = c23: B3 = c24
        C0 = c31: C1 = c32: C2 = c33: C3 = c34
        D0 = c41: D1 = c42: D2 = c43: D3 = c44
        E0 = c51: E1 = c52: E2 = c53: E3 = c54
        F = c61: G = c62: H = c63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 4
        A0 = d11: A1 = d12: A2 = d13: A3 = d14
        B0 = d21: B1 = d22: B2 = d23: B3 = d24
        C0 = d31: C1 = d32: C2 = d33: C3 = d34
        D0 = d41: D1 = d42: D2 = d43: D3 = d44
        E0 = d51: E1 = d52: E2 = d53: E3 = d54
        F = d61: G = d62: H = d63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 5
        A0 = e11: A1 = e12: A2 = e13: A3 = e14
        B0 = e21: B1 = e22: B2 = e23: B3 = e24
        C0 = e31: C1 = e32: C2 = e33: C3 = e34
        D0 = e41: D1 = e42: D2 = e43: D3 = e44
        E0 = e51: E1 = e52: E2 = e53: E3 = e54
        F = e61: G = e62: H = e63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 6
        A0 = f11: A1 = f12: A2 = f13: A3 = f14
        B0 = f21: B1 = f22: B2 = f23: B3 = f24
        C0 = f31: C1 = f32: C2 = f33: C3 = f34
        D0 = f41: D1 = f42: D2 = f43: D3 = f44
        E0 = f51: E1 = f52: E2 = f53: E3 = f54
        F = f61: G = f62: H = f63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 7
        A0 = g11: A1 = g12: A2 = g13: A3 = g14
        B0 = g21: B1 = g22: B2 = g23: B3 = g24
        C0 = g31: C1 = g32: C2 = g33: C3 = g34
        D0 = g41: D1 = g42: D2 = g43: D3 = g44
        E0 = g51: E1 = g52: E2 = g53: E3 = g54
        F = g61: G = g62: H = g63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        LI = 0: Pi = 3.1415926536: TP = 2# * Pi
        MS = SunMeanAnomaly(LH, LM, LS, DS, ZC, DY, MN, YR)
        SR = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR))
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR): LG = SR + Pi

        For K = 1 To 2

        For J = 1 To 7
        AP(J) = Radians(PL(J, 1) - PL(J, 3) - LI * PL(J, 2))
        Next J

        QA = 0: QB = 0: QC = 0: QD = 0: QE = 0: QF = 0: QG = 0
        If (IP < 5) Then
            On IP GoSub 4685, 4735, 4810, 4945
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoSub 4945, 4945, 4945
        End If

        EC = PL(IP, 4) + QD: AM = AP(IP) + QE: AT = TrueAnomaly(AM, EC)
        PVV = (PL(IP, 7) + QF) * (1# - EC * EC) / (1# + EC * Cos(AT))
        LP = Degrees(AT) + PL(IP, 3) + Degrees(QC - QE): LP = Radians(LP)
        OM = Radians(PL(IP, 6)): LO = LP - OM
        SO = Sin(LO): CO = Cos(LO)
        INN = Radians(PL(IP, 5)): PVV = PVV + QB
        SP = SO * Sin(INN): Y = SO * Cos(INN)
        PS = Asin(SP) + QG: SP = Sin(PS)
        PD = Atan2(CO, Y) + OM + Radians(QA)
        PD = Unwind(PD)
        CI = Cos(PS): RD = PVV * CI: LL = PD - LG
        RH = RE * RE + PVV * PVV - 2# * RE * PVV * CI * Cos(LL)
        RH = Sqr(RH): LI = RH * 0.005775518

        If (K = 1) Then
            L0 = PD
            V0 = RH
            S0 = PS
            P0 = PVV
        End If

        Next K

        L1 = Sin(LL): l2 = Cos(LL)
        
        If (IP < 3) Then
            EP = Atn(-1# * RD * L1 / (RE - RD * l2)) + LG + Pi
        Else
            EP = Atn(RE * L1 / (RD - RE * l2)) + PD
        End If

        EP = Unwind(EP)
        BP = Atn(RD * SP * Sin(EP - PD) / (CI * RE * L1))
        GoTo 5700

4685    QA = 0.00204 * Cos(5 * AP(2) - 2 * AP(1) + 0.21328)
        QA = QA + 0.00103 * Cos(2 * AP(2) - AP(1) - 2.8046)
        QA = QA + 0.00091 * Cos(2 * AP(4) - AP(1) - 0.64582)
        QA = QA + 0.00078 * Cos(5 * AP(2) - 3 * AP(1) + 0.17692)

        QB = 0.000007525 * Cos(2 * AP(4) - AP(1) + 0.925251)
        QB = QB + 0.000006802 * Cos(5 * AP(2) - 3 * AP(1) - 4.53642)
        QB = QB + 0.000005457 * Cos(2 * AP(2) - 2 * AP(1) - 1.24246)
        QB = QB + 0.000003569 * Cos(5 * AP(2) - AP(1) - 1.35699)
        Return

4735    QC = 0.00077 * Sin(4.1406 + T * 2.6227): QC = Radians(QC): QE = QC

        QA = 0.00313 * Cos(2 * MS - 2 * AP(2) - 2.587)
        QA = QA + 0.00198 * Cos(3 * MS - 3 * AP(2) + 0.044768)
        QA = QA + 0.00136 * Cos(MS - AP(2) - 2.0788)
        QA = QA + 0.00096 * Cos(3 * MS - 2 * AP(2) - 2.3721)
        QA = QA + 0.00082 * Cos(AP(4) - AP(2) - 3.6318)

        QB = 0.000022501 * Cos(2 * MS - 2 * AP(2) - 1.01592)
        QB = QB + 0.000019045 * Cos(3 * MS - 3 * AP(2) + 1.61577)
        QB = QB + 0.000006887 * Cos(AP(4) - AP(2) - 2.06106)
        QB = QB + 0.000005172 * Cos(MS - AP(2) - 0.508065)
        QB = QB + 0.00000362 * Cos(5 * MS - 4 * AP(2) - 1.81877)
        QB = QB + 0.000003283 * Cos(4 * MS - 4 * AP(2) + 1.10851)
        QB = QB + 0.000003074 * Cos(2 * AP(4) - 2 * AP(2) - 0.962846)
        Return

4810    A = 3 * AP(4) - 8 * AP(3) + 4 * MS: SA = Sin(A): CA = Cos(A)
        QC = -(0.01133 * SA + 0.00933 * CA): QC = Radians(QC): QE = QC

        QA = 0.00705 * Cos(AP(4) - AP(3) - 0.85448)
        QA = QA + 0.00607 * Cos(2 * AP(4) - AP(3) - 3.2873)
        QA = QA + 0.00445 * Cos(2 * AP(4) - 2 * AP(3) - 3.3492)
        QA = QA + 0.00388 * Cos(MS - 2 * AP(3) + 0.35771)
        QA = QA + 0.00238 * Cos(MS - AP(3) + 0.61256)
        QA = QA + 0.00204 * Cos(2 * MS - 3 * AP(3) + 2.7688)
        QA = QA + 0.00177 * Cos(3 * AP(3) - AP(2) - 1.0053)
        QA = QA + 0.00136 * Cos(2 * MS - 4 * AP(3) + 2.6894)
        QA = QA + 0.00104 * Cos(AP(4) + 0.30749)

        QB = 0.000053227 * Cos(AP(4) - AP(3) + 0.717864)
        QB = QB + 0.000050989 * Cos(2 * AP(4) - 2 * AP(3) - 1.77997)
        QB = QB + 0.000038278 * Cos(2 * AP(4) - AP(3) - 1.71617)
        QB = QB + 0.000015996 * Cos(MS - AP(3) - 0.969618)
        QB = QB + 0.000014764 * Cos(2 * MS - 3 * AP(3) + 1.19768)
        QB = QB + 0.000008966 * Cos(AP(4) - 2 * AP(3) + 0.761225)
        QB = QB + 0.000007914 * Cos(3 * AP(4) - 2 * AP(3) - 2.43887)
        QB = QB + 0.000007004 * Cos(2 * AP(4) - 3 * AP(3) - 1.79573)
        QB = QB + 0.00000662 * Cos(MS - 2 * AP(3) + 1.97575)
        QB = QB + 0.00000493 * Cos(3 * AP(4) - 3 * AP(3) - 1.33069)
        QB = QB + 0.000004693 * Cos(3 * MS - 5 * AP(3) + 3.32665)
        QB = QB + 0.000004571 * Cos(2 * MS - 4 * AP(3) + 4.27086)
        QB = QB + 0.000004409 * Cos(3 * AP(4) - AP(3) - 2.02158)
        Return

4945    J1 = T / 5# + 0.1: J2 = Unwind(4.14473 + 52.9691 * T)
        J3 = Unwind(4.641118 + 21.32991 * T)
        J4 = Unwind(4.250177 + 7.478172 * T)
        J5 = 5# * J3 - 2# * J2: J6 = 2# * J2 - 6# * J3 + 3# * J4

        If (IP < 5) Then
            On IP GoTo 5190, 5190, 5190, 4980
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoTo 4980, 5505, 5505, 5190
        End If

4980    J7 = J3 - J2: U1 = Sin(J3): U2 = Cos(J3): U3 = Sin(2# * J3)
        U4 = Cos(2# * J3): U5 = Sin(J5): U6 = Cos(J5)
        U7 = Sin(2# * J5): U8 = Sin(J6): U9 = Sin(J7)
        UA = Cos(J7): UB = Sin(2# * J7): UC = Cos(2# * J7)
        UD = Sin(3# * J7): UE = Cos(3# * J7): UF = Sin(4# * J7)
        UG = Cos(4# * J7): VH = Cos(5# * J7)
        
        If (IP = 5) Then
            GoTo 5200
        End If

        QC = (0.331364 - (0.010281 + 0.004692 * J1) * J1) * U5
        QC = QC + (0.003228 - (0.064436 - 0.002075 * J1) * J1) * U6
        QC = QC - (0.003083 + (0.000275 - 0.000489 * J1) * J1) * U7
        QC = QC + 0.002472 * U8 + 0.013619 * U9 + 0.018472 * UB
        QC = QC + 0.006717 * UD + 0.002775 * UF + 0.006417 * UB * U1
        QC = QC + (0.007275 - 0.001253 * J1) * U9 * U1 + 0.002439 * UD * U1
        QC = QC - (0.035681 + 0.001208 * J1) * U9 * U2 - 0.003767 * UC * U1
        QC = QC - (0.033839 + 0.001125 * J1) * UA * U1 - 0.004261 * UB * U2
        QC = QC + (0.001161 * J1 - 0.006333) * UA * U2 + 0.002178 * U2
        QC = QC - 0.006675 * UC * U2 - 0.002664 * UE * U2 - 0.002572 * U9 * U3
        QC = QC - 0.003567 * UB * U3 + 0.002094 * UA * U4 + 0.003342 * UC * U4
        QC = Radians(QC)

        QD = (3606 + (130 - 43 * J1) * J1) * U5 + (1289 - 580 * J1) * U6
        QD = QD - 6764 * U9 * U1 - 1110 * UB * U1 - 224 * UD * U1 - 204 * U1
        QD = QD + (1284 + 116 * J1) * UA * U1 + 188 * UC * U1
        QD = QD + (1460 + 130 * J1) * U9 * U2 + 224 * UB * U2 - 817 * U2
        QD = QD + 6074 * U2 * UA + 992 * UC * U2 + 508 * UE * U2 + 230 * UG * U2
        QD = QD + 108 * VH * U2 - (956 + 73 * J1) * U9 * U3 + 448 * UB * U3
        QD = QD + 137 * UD * U3 + (108 * J1 - 997) * UA * U3 + 480 * UC * U3
        QD = QD + 148 * UE * U3 + (99 * J1 - 956) * U9 * U4 + 490 * UB * U4
        QD = QD + 158 * UD * U4 + 179 * U4 + (1024 + 75 * J1) * UA * U4
        QD = QD - 437 * UC * U4 - 132 * UE * U4: QD = QD * 0.0000001

        VK = (0.007192 - 0.003147 * J1) * U5 - 0.004344 * U1
        VK = VK + (J1 * (0.000197 * J1 - 0.000675) - 0.020428) * U6
        VK = VK + 0.034036 * UA * U1 + (0.007269 + 0.000672 * J1) * U9 * U1
        VK = VK + 0.005614 * UC * U1 + 0.002964 * UE * U1 + 0.037761 * U9 * U2
        VK = VK + 0.006158 * UB * U2 - 0.006603 * UA * U2 - 0.005356 * U9 * U3
        VK = VK + 0.002722 * UB * U3 + 0.004483 * UA * U3
        VK = VK - 0.002642 * UC * U3 + 0.004403 * U9 * U4
        VK = VK - 0.002536 * UB * U4 + 0.005547 * UA * U4 - 0.002689 * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 205 * UA - 263 * U6 + 693 * UC + 312 * UE + 147 * UG + 299 * U9 * U1
        QF = QF + 181 * UC * U1 + 204 * UB * U2 + 111 * UD * U2 - 337 * UA * U2
        QF = QF - 111 * UC * U2: QF = QF * 0.000001
5190    Return

5200    UI = Sin(3 * J3): UJ = Cos(3 * J3): UK = Sin(4 * J3)
        UL = Cos(4 * J3): VI = Cos(2 * J5): UN = Sin(5 * J7)
        J8 = J4 - J3: UO = Sin(2 * J8): UP = Cos(2 * J8)
        UQ = Sin(3 * J8): UR = Cos(3 * J8)

        QC = 0.007581 * U7 - 0.007986 * U8 - 0.148811 * U9
        QC = QC - (0.814181 - (0.01815 - 0.016714 * J1) * J1) * U5
        QC = QC - (0.010497 - (0.160906 - 0.0041 * J1) * J1) * U6
        QC = QC - 0.015208 * UD - 0.006339 * UF - 0.006244 * U1
        QC = QC - 0.0165 * UB * U1 - 0.040786 * UB
        QC = QC + (0.008931 + 0.002728 * J1) * U9 * U1 - 0.005775 * UD * U1
        QC = QC + (0.081344 + 0.003206 * J1) * UA * U1 + 0.015019 * UC * U1
        QC = QC + (0.085581 + 0.002494 * J1) * U9 * U2 + 0.014394 * UC * U2
        QC = QC + (0.025328 - 0.003117 * J1) * UA * U2 + 0.006319 * UE * U2
        QC = QC + 0.006369 * U9 * U3 + 0.009156 * UB * U3 + 0.007525 * UQ * U3
        QC = QC - 0.005236 * UA * U4 - 0.007736 * UC * U4 - 0.007528 * UR * U4
        QC = Radians(QC)

        QD = (-7927 + (2548 + 91 * J1) * J1) * U5
        QD = QD + (13381 + (1226 - 253 * J1) * J1) * U6 + (248 - 121 * J1) * U7
        QD = QD - (305 + 91 * J1) * VI + 412 * UB + 12415 * U1
        QD = QD + (390 - 617 * J1) * U9 * U1 + (165 - 204 * J1) * UB * U1
        QD = QD + 26599 * UA * U1 - 4687 * UC * U1 - 1870 * UE * U1 - 821 * UG * U1
        QD = QD - 377 * VH * U1 + 497 * UP * U1 + (163 - 611 * J1) * U2
        QD = QD - 12696 * U9 * U2 - 4200 * UB * U2 - 1503 * UD * U2 - 619 * UF * U2
        QD = QD - 268 * UN * U2 - (282 + 1306 * J1) * UA * U2
        QD = QD + (-86 + 230 * J1) * UC * U2 + 461 * UO * U2 - 350 * U3
        QD = QD + (2211 - 286 * J1) * U9 * U3 - 2208 * UB * U3 - 568 * UD * U3
        QD = QD - 346 * UF * U3 - (2780 + 222 * J1) * UA * U3
        QD = QD + (2022 + 263 * J1) * UC * U3 + 248 * UE * U3 + 242 * UQ * U3
        QD = QD + 467 * UR * U3 - 490 * U4 - (2842 + 279 * J1) * U9 * U4
        QD = QD + (128 + 226 * J1) * UB * U4 + 224 * UD * U4
        QD = QD + (-1594 + 282 * J1) * UA * U4 + (2162 - 207 * J1) * UC * U4
        QD = QD + 561 * UE * U4 + 343 * UG * U4 + 469 * UQ * U4 - 242 * UR * U4
        QD = QD - 205 * U9 * UI + 262 * UD * UI + 208 * UA * UJ - 271 * UE * UJ
        QD = QD - 382 * UE * UK - 376 * UD * UL: QD = QD * 0.0000001

        VK = (0.077108 + (0.007186 - 0.001533 * J1) * J1) * U5
        VK = VK - 0.007075 * U9
        VK = VK + (0.045803 - (0.014766 + 0.000536 * J1) * J1) * U6
        VK = VK - 0.072586 * U2 - 0.075825 * U9 * U1 - 0.024839 * UB * U1
        VK = VK - 0.008631 * UD * U1 - 0.150383 * UA * U2
        VK = VK + 0.026897 * UC * U2 + 0.010053 * UE * U2
        VK = VK - (0.013597 + 0.001719 * J1) * U9 * U3 + 0.011981 * UB * U4
        VK = VK - (0.007742 - 0.001517 * J1) * UA * U3
        VK = VK + (0.013586 - 0.001375 * J1) * UC * U3
        VK = VK - (0.013667 - 0.001239 * J1) * U9 * U4
        VK = VK + (0.014861 + 0.001136 * J1) * UA * U4
        VK = VK - (0.013064 + 0.001628 * J1) * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 572 * U5 - 1590 * UB * U2 + 2933 * U6 - 647 * UD * U2
        QF = QF + 33629 * UA - 344 * UF * U2 - 3081 * UC + 2885 * UA * U2
        QF = QF - 1423 * UE + (2172 + 102 * J1) * UC * U2 - 671 * UG
        QF = QF + 296 * UE * U2 - 320 * VH - 267 * UB * U3 + 1098 * U1
        QF = QF - 778 * UA * U3 - 2812 * U9 * U1 + 495 * UC * U3 + 688 * UB * U1
        QF = QF + 250 * UE * U3 - 393 * UD * U1 - 856 * U9 * U4 - 228 * UF * U1
        QF = QF + 441 * UB * U4 + 2138 * UA * U1 + 296 * UC * U4 - 999 * UC * U1
        QF = QF + 211 * UE * U4 - 642 * UE * U1 - 427 * U9 * UI - 325 * UG * U1
        QF = QF + 398 * UD * UI - 890 * U2 + 344 * UA * UJ + 2206 * U9 * U2
        QF = QF - 427 * UE * UJ: QF = QF * 0.000001

        QG = 0.000747 * UA * U1 + 0.001069 * UA * U2 + 0.002108 * UB * U3
        QG = QG + 0.001261 * UC * U3 + 0.001236 * UB * U4 - 0.002075 * UC * U4
        QG = Radians(QG): Return

5505    J8 = Unwind(1.46205 + 3.81337 * T): J9 = 2 * J8 - J4
        VJ = Sin(J9): UU = Cos(J9): UV = Sin(2 * J9)
        UW = Cos(2 * J9)
        
        If (IP = 7) Then
            GoTo 5675
        End If

        JA = J4 - J2: JB = J4 - J3: JC = J8 - J4
        QC = (0.864319 - 0.001583 * J1) * VJ
        QC = QC + (0.082222 - 0.006833 * J1) * UU + 0.036017 * UV
        QC = QC - 0.003019 * UW + 0.008122 * Sin(J6): QC = Radians(QC)

        VK = 0.120303 * VJ + 0.006197 * UV
        VK = VK + (0.019472 - 0.000947 * J1) * UU
        QE = QC - (Radians(VK) / PL(IP, 4))

        QD = (163 * J1 - 3349) * VJ + 20981 * UU + 1311 * UW: QD = QD * 0.0000001

        QF = -0.003825 * UU

        QA = (-0.038581 + (0.002031 - 0.00191 * J1) * J1) * Cos(J4 + JB)
        QA = QA + (0.010122 - 0.000988 * J1) * Sin(J4 + JB)
        A = (0.034964 - (0.001038 - 0.000868 * J1) * J1) * Cos(2 * J4 + JB)
        QA = A + QA + 0.005594 * Sin(J4 + 3 * JC) - 0.014808 * Sin(JA)
        QA = QA - 0.005794 * Sin(JB) + 0.002347 * Cos(JB)
        QA = QA + 0.009872 * Sin(JC) + 0.008803 * Sin(2 * JC)
        QA = QA - 0.004308 * Sin(3 * JC)

        UX = Sin(JB): UY = Cos(JB): UZ = Sin(J4)
        VA = Cos(J4): VB = Sin(2 * J4): VC = Cos(2 * J4)
        QG = (0.000458 * UX - 0.000642 * UY - 0.000517 * Cos(4 * JC)) * UZ
        QG = QG - (0.000347 * UX + 0.000853 * UY + 0.000517 * Sin(4 * JB)) * VA
        QG = QG + 0.000403 * (Cos(2 * JC) * VB + Sin(2 * JC) * VC)
        QG = Radians(QG)

        QB = -25948 + 4985 * Cos(JA) - 1230 * VA + 3354 * UY
        QB = QB + 904 * Cos(2 * JC) + 894 * (Cos(JC) - Cos(3 * JC))
        QB = QB + (5795 * VA - 1165 * UZ + 1388 * VC) * UX
        QB = QB + (1351 * VA + 5702 * UZ + 1388 * VB) * UY
        QB = QB * 0.000001: Return

5675    JA = J8 - J2: JB = J8 - J3: JC = J8 - J4
        QC = (0.001089 * J1 - 0.589833) * VJ
        QC = QC + (0.004658 * J1 - 0.056094) * UU - 0.024286 * UV
        QC = Radians(QC)

        VK = 0.024039 * VJ - 0.025303 * UU + 0.006206 * UV
        VK = VK - 0.005992 * UW: QE = QC - (Radians(VK) / PL(IP, 4))

        QD = 4389 * VJ + 1129 * UV + 4262 * UU + 1089 * UW
        QD = QD * 0.0000001

        QF = 8189 * UU - 817 * VJ + 781 * UW: QF = QF * 0.000001

        VD = Sin(2 * JC): VE = Cos(2 * JC)
        VF = Sin(J8): VG = Cos(J8)
        QA = -0.009556 * Sin(JA) - 0.005178 * Sin(JB)
        QA = QA + 0.002572 * VD - 0.002972 * VE * VF - 0.002833 * VD * VG

        QG = 0.000336 * VE * VF + 0.000364 * VD * VG: QG = Radians(QG)

        QB = -40596 + 4992 * Cos(JA) + 2744 * Cos(JB)
        QB = QB + 2044 * Cos(JC) + 1051 * VE: QB = QB * 0.000001

        Return
        
5700    PlanetLat = Degrees(BP)

End Function

Function PlanetDist(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, S As String) As Double

Const a11 = 178.179078, a12 = 415.2057519, a13 = 0.0003011, a14 = 0
Const a21 = 75.899697, a22 = 1.5554889, a23 = 0.0002947, a24 = 0
Const a31 = 0.20561421, a32 = 0.00002046, a33 = -0.00000003, a34 = 0
Const a41 = 7.002881, a42 = 0.0018608, a43 = -0.0000183, a44 = 0
Const a51 = 47.145944, a52 = 1.1852083, a53 = 0.0001739, a54 = 0
Const a61 = 0.3870986, a62 = 6.74, a63 = -0.42

Const b11 = 342.767053, b12 = 162.5533664, b13 = 0.0003097, b14 = 0
Const b21 = 130.163833, b22 = 1.4080361, b23 = -0.0009764, b24 = 0
Const b31 = 0.00682069, b32 = -0.00004774, b33 = 0.000000091, b34 = 0
Const b41 = 3.393631, b42 = 0.0010058, b43 = -0.000001, b44 = 0
Const b51 = 75.779647, b52 = 0.89985, b53 = 0.00041, b54 = 0
Const b61 = 0.7233316, b62 = 16.92, b63 = -4.4

Const c11 = 293.737334, c12 = 53.17137642, c13 = 0.0003107, c14 = 0
Const c21 = 334.218203, c22 = 1.8407584, c23 = 0.0001299, c24 = -0.00000119
Const c31 = 0.0933129, c32 = 0.000092064, c33 = -0.000000077, c34 = 0
Const c41 = 1.850333, c42 = -0.000675, c43 = 0.0000126, c44 = 0
Const c51 = 48.786442, c52 = 0.7709917, c53 = -0.0000014, c54 = -0.00000533
Const c61 = 1.5236883, c62 = 9.36, c63 = -1.52

Const d11 = 238.049257, d12 = 8.434172183, d13 = 0.0003347, d14 = -0.00000165
Const d21 = 12.720972, d22 = 1.6099617, d23 = 0.00105627, d24 = -0.00000343
Const d31 = 0.04833475, d32 = 0.00016418, d33 = -0.0000004676, d34 = -0.0000000017
Const d41 = 1.308736, d42 = -0.0056961, d43 = 0.0000039, d44 = 0
Const d51 = 99.443414, d52 = 1.01053, d53 = 0.00035222, d54 = -0.00000851
Const d61 = 5.202561, d62 = 196.74, d63 = -9.4

Const e11 = 266.564377, e12 = 3.398638567, e13 = 0.0003245, e14 = -0.0000058
Const e21 = 91.098214, e22 = 1.9584158, e23 = 0.00082636, e24 = 0.00000461
Const e31 = 0.05589232, e32 = -0.0003455, e33 = -0.000000728, e34 = 0.00000000074
Const e41 = 2.492519, e42 = -0.0039189, e43 = -0.00001549, e44 = 0.00000004
Const e51 = 112.790414, e52 = 0.8731951, e53 = -0.00015218, e54 = -0.00000531
Const e61 = 9.554747, e62 = 165.6, e63 = -8.88

Const f11 = 244.19747, f12 = 1.194065406, f13 = 0.000316, f14 = -0.0000006
Const f21 = 171.548692, f22 = 1.4844328, f23 = 0.0002372, f24 = -0.00000061
Const f31 = 0.0463444, f32 = -0.00002658, f33 = 0.000000077, f34 = 0
Const f41 = 0.772464, f42 = 0.0006253, f43 = 0.0000395, f44 = 0
Const f51 = 73.477111, f52 = 0.4986678, f53 = 0.0013117, f54 = 0
Const f61 = 19.21814, f62 = 65.8, f63 = -7.19

Const g11 = 84.457994, g12 = 0.6107942056, g13 = 0.0003205, g14 = -0.0000006
Const g21 = 46.727364, g22 = 1.4245744, g23 = 0.00039082, g24 = -0.000000605
Const g31 = 0.00899704, g32 = 0.00000633, g33 = -0.000000002, g34 = 0
Const g41 = 1.779242, g42 = -0.0095436, g43 = -0.0000091, g44 = 0
Const g51 = 130.681389, g52 = 1.098935, g53 = 0.00024987, g54 = -0.000004718
Const g61 = 30.10957, g62 = 62.2, g63 = -6.87

Dim IP As Integer, I As Integer, J As Integer, K As Integer
Dim PL(7, 9) As Double, AP(7) As Double

        IP = 0: B = LctUT(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        A = CDJD(GD, GM, GY)
        T = ((A - 2415020#) / 36525#) + (B / 876600#)

        Select Case UCase(S)
            Case "MERCURY"
                IP = 1
           Case "VENUS"
                IP = 2
            Case "MARS"
                IP = 3
           Case "JUPITER"
                IP = 4
            Case "SATURN"
                IP = 5
            Case "URANUS"
                IP = 6
            Case "NEPTUNE"
                IP = 7
            Case Else
                EP = 0
                GoTo 5700
        End Select
    

        I = 1
        A0 = a11: A1 = a12: A2 = a13: A3 = a14
        B0 = a21: B1 = a22: B2 = a23: B3 = a24
        C0 = a31: C1 = a32: C2 = a33: C3 = a34
        D0 = a41: D1 = a42: D2 = a43: D3 = a44
        E0 = a51: E1 = a52: E2 = a53: E3 = a54
        F = a61: G = a62: H = a63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 2
        A0 = b11: A1 = b12: A2 = b13: A3 = b14
        B0 = b21: B1 = b22: B2 = b23: B3 = b24
        C0 = b31: C1 = b32: C2 = b33: C3 = b34
        D0 = b41: D1 = b42: D2 = b43: D3 = b44
        E0 = b51: E1 = b52: E2 = b53: E3 = b54
        F = b61: G = b62: H = b63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 3
        A0 = c11: A1 = c12: A2 = c13: A3 = c14
        B0 = c21: B1 = c22: B2 = c23: B3 = c24
        C0 = c31: C1 = c32: C2 = c33: C3 = c34
        D0 = c41: D1 = c42: D2 = c43: D3 = c44
        E0 = c51: E1 = c52: E2 = c53: E3 = c54
        F = c61: G = c62: H = c63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 4
        A0 = d11: A1 = d12: A2 = d13: A3 = d14
        B0 = d21: B1 = d22: B2 = d23: B3 = d24
        C0 = d31: C1 = d32: C2 = d33: C3 = d34
        D0 = d41: D1 = d42: D2 = d43: D3 = d44
        E0 = d51: E1 = d52: E2 = d53: E3 = d54
        F = d61: G = d62: H = d63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 5
        A0 = e11: A1 = e12: A2 = e13: A3 = e14
        B0 = e21: B1 = e22: B2 = e23: B3 = e24
        C0 = e31: C1 = e32: C2 = e33: C3 = e34
        D0 = e41: D1 = e42: D2 = e43: D3 = e44
        E0 = e51: E1 = e52: E2 = e53: E3 = e54
        F = e61: G = e62: H = e63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 6
        A0 = f11: A1 = f12: A2 = f13: A3 = f14
        B0 = f21: B1 = f22: B2 = f23: B3 = f24
        C0 = f31: C1 = f32: C2 = f33: C3 = f34
        D0 = f41: D1 = f42: D2 = f43: D3 = f44
        E0 = f51: E1 = f52: E2 = f53: E3 = f54
        F = f61: G = f62: H = f63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 7
        A0 = g11: A1 = g12: A2 = g13: A3 = g14
        B0 = g21: B1 = g22: B2 = g23: B3 = g24
        C0 = g31: C1 = g32: C2 = g33: C3 = g34
        D0 = g41: D1 = g42: D2 = g43: D3 = g44
        E0 = g51: E1 = g52: E2 = g53: E3 = g54
        F = g61: G = g62: H = g63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        LI = 0: Pi = 3.1415926536: TP = 2# * Pi
        MS = SunMeanAnomaly(LH, LM, LS, DS, ZC, DY, MN, YR)
        SR = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR))
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR): LG = SR + Pi

        K = 1
        
        For J = 1 To 7
        AP(J) = Radians(PL(J, 1) - PL(J, 3) - LI * PL(J, 2))
        Next J

        QA = 0: QB = 0: QC = 0: QD = 0: QE = 0: QF = 0: QG = 0
        If (IP < 5) Then
            On IP GoSub 4685, 4735, 4810, 4945
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoSub 4945, 4945, 4945
        End If

        EC = PL(IP, 4) + QD: AM = AP(IP) + QE: AT = TrueAnomaly(AM, EC)
        PVV = (PL(IP, 7) + QF) * (1# - EC * EC) / (1# + EC * Cos(AT))
        LP = Degrees(AT) + PL(IP, 3) + Degrees(QC - QE): LP = Radians(LP)
        OM = Radians(PL(IP, 6)): LO = LP - OM
        SO = Sin(LO): CO = Cos(LO)
        INN = Radians(PL(IP, 5)): PVV = PVV + QB
        SP = SO * Sin(INN): Y = SO * Cos(INN)
        PS = Asin(SP) + QG: SP = Sin(PS)
        PD = Atan2(CO, Y) + OM + Radians(QA)
        PD = Unwind(PD)
        CI = Cos(PS): RD = PVV * CI: LL = PD - LG
        RH = RE * RE + PVV * PVV - 2# * RE * PVV * CI * Cos(LL)
        RH = Sqr(RH): LI = RH * 0.005775518

        VO = RH
        GoTo 5700

4685    QA = 0.00204 * Cos(5 * AP(2) - 2 * AP(1) + 0.21328)
        QA = QA + 0.00103 * Cos(2 * AP(2) - AP(1) - 2.8046)
        QA = QA + 0.00091 * Cos(2 * AP(4) - AP(1) - 0.64582)
        QA = QA + 0.00078 * Cos(5 * AP(2) - 3 * AP(1) + 0.17692)

        QB = 0.000007525 * Cos(2 * AP(4) - AP(1) + 0.925251)
        QB = QB + 0.000006802 * Cos(5 * AP(2) - 3 * AP(1) - 4.53642)
        QB = QB + 0.000005457 * Cos(2 * AP(2) - 2 * AP(1) - 1.24246)
        QB = QB + 0.000003569 * Cos(5 * AP(2) - AP(1) - 1.35699)
        Return

4735    QC = 0.00077 * Sin(4.1406 + T * 2.6227): QC = Radians(QC): QE = QC

        QA = 0.00313 * Cos(2 * MS - 2 * AP(2) - 2.587)
        QA = QA + 0.00198 * Cos(3 * MS - 3 * AP(2) + 0.044768)
        QA = QA + 0.00136 * Cos(MS - AP(2) - 2.0788)
        QA = QA + 0.00096 * Cos(3 * MS - 2 * AP(2) - 2.3721)
        QA = QA + 0.00082 * Cos(AP(4) - AP(2) - 3.6318)

        QB = 0.000022501 * Cos(2 * MS - 2 * AP(2) - 1.01592)
        QB = QB + 0.000019045 * Cos(3 * MS - 3 * AP(2) + 1.61577)
        QB = QB + 0.000006887 * Cos(AP(4) - AP(2) - 2.06106)
        QB = QB + 0.000005172 * Cos(MS - AP(2) - 0.508065)
        QB = QB + 0.00000362 * Cos(5 * MS - 4 * AP(2) - 1.81877)
        QB = QB + 0.000003283 * Cos(4 * MS - 4 * AP(2) + 1.10851)
        QB = QB + 0.000003074 * Cos(2 * AP(4) - 2 * AP(2) - 0.962846)
        Return

4810    A = 3 * AP(4) - 8 * AP(3) + 4 * MS: SA = Sin(A): CA = Cos(A)
        QC = -(0.01133 * SA + 0.00933 * CA): QC = Radians(QC): QE = QC

        QA = 0.00705 * Cos(AP(4) - AP(3) - 0.85448)
        QA = QA + 0.00607 * Cos(2 * AP(4) - AP(3) - 3.2873)
        QA = QA + 0.00445 * Cos(2 * AP(4) - 2 * AP(3) - 3.3492)
        QA = QA + 0.00388 * Cos(MS - 2 * AP(3) + 0.35771)
        QA = QA + 0.00238 * Cos(MS - AP(3) + 0.61256)
        QA = QA + 0.00204 * Cos(2 * MS - 3 * AP(3) + 2.7688)
        QA = QA + 0.00177 * Cos(3 * AP(3) - AP(2) - 1.0053)
        QA = QA + 0.00136 * Cos(2 * MS - 4 * AP(3) + 2.6894)
        QA = QA + 0.00104 * Cos(AP(4) + 0.30749)

        QB = 0.000053227 * Cos(AP(4) - AP(3) + 0.717864)
        QB = QB + 0.000050989 * Cos(2 * AP(4) - 2 * AP(3) - 1.77997)
        QB = QB + 0.000038278 * Cos(2 * AP(4) - AP(3) - 1.71617)
        QB = QB + 0.000015996 * Cos(MS - AP(3) - 0.969618)
        QB = QB + 0.000014764 * Cos(2 * MS - 3 * AP(3) + 1.19768)
        QB = QB + 0.000008966 * Cos(AP(4) - 2 * AP(3) + 0.761225)
        QB = QB + 0.000007914 * Cos(3 * AP(4) - 2 * AP(3) - 2.43887)
        QB = QB + 0.000007004 * Cos(2 * AP(4) - 3 * AP(3) - 1.79573)
        QB = QB + 0.00000662 * Cos(MS - 2 * AP(3) + 1.97575)
        QB = QB + 0.00000493 * Cos(3 * AP(4) - 3 * AP(3) - 1.33069)
        QB = QB + 0.000004693 * Cos(3 * MS - 5 * AP(3) + 3.32665)
        QB = QB + 0.000004571 * Cos(2 * MS - 4 * AP(3) + 4.27086)
        QB = QB + 0.000004409 * Cos(3 * AP(4) - AP(3) - 2.02158)
        Return

4945    J1 = T / 5# + 0.1: J2 = Unwind(4.14473 + 52.9691 * T)
        J3 = Unwind(4.641118 + 21.32991 * T)
        J4 = Unwind(4.250177 + 7.478172 * T)
        J5 = 5# * J3 - 2# * J2: J6 = 2# * J2 - 6# * J3 + 3# * J4

        If (IP < 5) Then
            On IP GoTo 5190, 5190, 5190, 4980
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoTo 4980, 5505, 5505, 5190
        End If

4980    J7 = J3 - J2: U1 = Sin(J3): U2 = Cos(J3): U3 = Sin(2# * J3)
        U4 = Cos(2# * J3): U5 = Sin(J5): U6 = Cos(J5)
        U7 = Sin(2# * J5): U8 = Sin(J6): U9 = Sin(J7)
        UA = Cos(J7): UB = Sin(2# * J7): UC = Cos(2# * J7)
        UD = Sin(3# * J7): UE = Cos(3# * J7): UF = Sin(4# * J7)
        UG = Cos(4# * J7): VH = Cos(5# * J7)
        
        If (IP = 5) Then
            GoTo 5200
        End If

        QC = (0.331364 - (0.010281 + 0.004692 * J1) * J1) * U5
        QC = QC + (0.003228 - (0.064436 - 0.002075 * J1) * J1) * U6
        QC = QC - (0.003083 + (0.000275 - 0.000489 * J1) * J1) * U7
        QC = QC + 0.002472 * U8 + 0.013619 * U9 + 0.018472 * UB
        QC = QC + 0.006717 * UD + 0.002775 * UF + 0.006417 * UB * U1
        QC = QC + (0.007275 - 0.001253 * J1) * U9 * U1 + 0.002439 * UD * U1
        QC = QC - (0.035681 + 0.001208 * J1) * U9 * U2 - 0.003767 * UC * U1
        QC = QC - (0.033839 + 0.001125 * J1) * UA * U1 - 0.004261 * UB * U2
        QC = QC + (0.001161 * J1 - 0.006333) * UA * U2 + 0.002178 * U2
        QC = QC - 0.006675 * UC * U2 - 0.002664 * UE * U2 - 0.002572 * U9 * U3
        QC = QC - 0.003567 * UB * U3 + 0.002094 * UA * U4 + 0.003342 * UC * U4
        QC = Radians(QC)

        QD = (3606 + (130 - 43 * J1) * J1) * U5 + (1289 - 580 * J1) * U6
        QD = QD - 6764 * U9 * U1 - 1110 * UB * U1 - 224 * UD * U1 - 204 * U1
        QD = QD + (1284 + 116 * J1) * UA * U1 + 188 * UC * U1
        QD = QD + (1460 + 130 * J1) * U9 * U2 + 224 * UB * U2 - 817 * U2
        QD = QD + 6074 * U2 * UA + 992 * UC * U2 + 508 * UE * U2 + 230 * UG * U2
        QD = QD + 108 * VH * U2 - (956 + 73 * J1) * U9 * U3 + 448 * UB * U3
        QD = QD + 137 * UD * U3 + (108 * J1 - 997) * UA * U3 + 480 * UC * U3
        QD = QD + 148 * UE * U3 + (99 * J1 - 956) * U9 * U4 + 490 * UB * U4
        QD = QD + 158 * UD * U4 + 179 * U4 + (1024 + 75 * J1) * UA * U4
        QD = QD - 437 * UC * U4 - 132 * UE * U4: QD = QD * 0.0000001

        VK = (0.007192 - 0.003147 * J1) * U5 - 0.004344 * U1
        VK = VK + (J1 * (0.000197 * J1 - 0.000675) - 0.020428) * U6
        VK = VK + 0.034036 * UA * U1 + (0.007269 + 0.000672 * J1) * U9 * U1
        VK = VK + 0.005614 * UC * U1 + 0.002964 * UE * U1 + 0.037761 * U9 * U2
        VK = VK + 0.006158 * UB * U2 - 0.006603 * UA * U2 - 0.005356 * U9 * U3
        VK = VK + 0.002722 * UB * U3 + 0.004483 * UA * U3
        VK = VK - 0.002642 * UC * U3 + 0.004403 * U9 * U4
        VK = VK - 0.002536 * UB * U4 + 0.005547 * UA * U4 - 0.002689 * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 205 * UA - 263 * U6 + 693 * UC + 312 * UE + 147 * UG + 299 * U9 * U1
        QF = QF + 181 * UC * U1 + 204 * UB * U2 + 111 * UD * U2 - 337 * UA * U2
        QF = QF - 111 * UC * U2: QF = QF * 0.000001
5190    Return

5200    UI = Sin(3 * J3): UJ = Cos(3 * J3): UK = Sin(4 * J3)
        UL = Cos(4 * J3): VI = Cos(2 * J5): UN = Sin(5 * J7)
        J8 = J4 - J3: UO = Sin(2 * J8): UP = Cos(2 * J8)
        UQ = Sin(3 * J8): UR = Cos(3 * J8)

        QC = 0.007581 * U7 - 0.007986 * U8 - 0.148811 * U9
        QC = QC - (0.814181 - (0.01815 - 0.016714 * J1) * J1) * U5
        QC = QC - (0.010497 - (0.160906 - 0.0041 * J1) * J1) * U6
        QC = QC - 0.015208 * UD - 0.006339 * UF - 0.006244 * U1
        QC = QC - 0.0165 * UB * U1 - 0.040786 * UB
        QC = QC + (0.008931 + 0.002728 * J1) * U9 * U1 - 0.005775 * UD * U1
        QC = QC + (0.081344 + 0.003206 * J1) * UA * U1 + 0.015019 * UC * U1
        QC = QC + (0.085581 + 0.002494 * J1) * U9 * U2 + 0.014394 * UC * U2
        QC = QC + (0.025328 - 0.003117 * J1) * UA * U2 + 0.006319 * UE * U2
        QC = QC + 0.006369 * U9 * U3 + 0.009156 * UB * U3 + 0.007525 * UQ * U3
        QC = QC - 0.005236 * UA * U4 - 0.007736 * UC * U4 - 0.007528 * UR * U4
        QC = Radians(QC)

        QD = (-7927 + (2548 + 91 * J1) * J1) * U5
        QD = QD + (13381 + (1226 - 253 * J1) * J1) * U6 + (248 - 121 * J1) * U7
        QD = QD - (305 + 91 * J1) * VI + 412 * UB + 12415 * U1
        QD = QD + (390 - 617 * J1) * U9 * U1 + (165 - 204 * J1) * UB * U1
        QD = QD + 26599 * UA * U1 - 4687 * UC * U1 - 1870 * UE * U1 - 821 * UG * U1
        QD = QD - 377 * VH * U1 + 497 * UP * U1 + (163 - 611 * J1) * U2
        QD = QD - 12696 * U9 * U2 - 4200 * UB * U2 - 1503 * UD * U2 - 619 * UF * U2
        QD = QD - 268 * UN * U2 - (282 + 1306 * J1) * UA * U2
        QD = QD + (-86 + 230 * J1) * UC * U2 + 461 * UO * U2 - 350 * U3
        QD = QD + (2211 - 286 * J1) * U9 * U3 - 2208 * UB * U3 - 568 * UD * U3
        QD = QD - 346 * UF * U3 - (2780 + 222 * J1) * UA * U3
        QD = QD + (2022 + 263 * J1) * UC * U3 + 248 * UE * U3 + 242 * UQ * U3
        QD = QD + 467 * UR * U3 - 490 * U4 - (2842 + 279 * J1) * U9 * U4
        QD = QD + (128 + 226 * J1) * UB * U4 + 224 * UD * U4
        QD = QD + (-1594 + 282 * J1) * UA * U4 + (2162 - 207 * J1) * UC * U4
        QD = QD + 561 * UE * U4 + 343 * UG * U4 + 469 * UQ * U4 - 242 * UR * U4
        QD = QD - 205 * U9 * UI + 262 * UD * UI + 208 * UA * UJ - 271 * UE * UJ
        QD = QD - 382 * UE * UK - 376 * UD * UL: QD = QD * 0.0000001

        VK = (0.077108 + (0.007186 - 0.001533 * J1) * J1) * U5
        VK = VK - 0.007075 * U9
        VK = VK + (0.045803 - (0.014766 + 0.000536 * J1) * J1) * U6
        VK = VK - 0.072586 * U2 - 0.075825 * U9 * U1 - 0.024839 * UB * U1
        VK = VK - 0.008631 * UD * U1 - 0.150383 * UA * U2
        VK = VK + 0.026897 * UC * U2 + 0.010053 * UE * U2
        VK = VK - (0.013597 + 0.001719 * J1) * U9 * U3 + 0.011981 * UB * U4
        VK = VK - (0.007742 - 0.001517 * J1) * UA * U3
        VK = VK + (0.013586 - 0.001375 * J1) * UC * U3
        VK = VK - (0.013667 - 0.001239 * J1) * U9 * U4
        VK = VK + (0.014861 + 0.001136 * J1) * UA * U4
        VK = VK - (0.013064 + 0.001628 * J1) * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 572 * U5 - 1590 * UB * U2 + 2933 * U6 - 647 * UD * U2
        QF = QF + 33629 * UA - 344 * UF * U2 - 3081 * UC + 2885 * UA * U2
        QF = QF - 1423 * UE + (2172 + 102 * J1) * UC * U2 - 671 * UG
        QF = QF + 296 * UE * U2 - 320 * VH - 267 * UB * U3 + 1098 * U1
        QF = QF - 778 * UA * U3 - 2812 * U9 * U1 + 495 * UC * U3 + 688 * UB * U1
        QF = QF + 250 * UE * U3 - 393 * UD * U1 - 856 * U9 * U4 - 228 * UF * U1
        QF = QF + 441 * UB * U4 + 2138 * UA * U1 + 296 * UC * U4 - 999 * UC * U1
        QF = QF + 211 * UE * U4 - 642 * UE * U1 - 427 * U9 * UI - 325 * UG * U1
        QF = QF + 398 * UD * UI - 890 * U2 + 344 * UA * UJ + 2206 * U9 * U2
        QF = QF - 427 * UE * UJ: QF = QF * 0.000001

        QG = 0.000747 * UA * U1 + 0.001069 * UA * U2 + 0.002108 * UB * U3
        QG = QG + 0.001261 * UC * U3 + 0.001236 * UB * U4 - 0.002075 * UC * U4
        QG = Radians(QG): Return

5505    J8 = Unwind(1.46205 + 3.81337 * T): J9 = 2 * J8 - J4
        VJ = Sin(J9): UU = Cos(J9): UV = Sin(2 * J9)
        UW = Cos(2 * J9)
        
        If (IP = 7) Then
            GoTo 5675
        End If

        JA = J4 - J2: JB = J4 - J3: JC = J8 - J4
        QC = (0.864319 - 0.001583 * J1) * VJ
        QC = QC + (0.082222 - 0.006833 * J1) * UU + 0.036017 * UV
        QC = QC - 0.003019 * UW + 0.008122 * Sin(J6): QC = Radians(QC)

        VK = 0.120303 * VJ + 0.006197 * UV
        VK = VK + (0.019472 - 0.000947 * J1) * UU
        QE = QC - (Radians(VK) / PL(IP, 4))

        QD = (163 * J1 - 3349) * VJ + 20981 * UU + 1311 * UW: QD = QD * 0.0000001

        QF = -0.003825 * UU

        QA = (-0.038581 + (0.002031 - 0.00191 * J1) * J1) * Cos(J4 + JB)
        QA = QA + (0.010122 - 0.000988 * J1) * Sin(J4 + JB)
        A = (0.034964 - (0.001038 - 0.000868 * J1) * J1) * Cos(2 * J4 + JB)
        QA = A + QA + 0.005594 * Sin(J4 + 3 * JC) - 0.014808 * Sin(JA)
        QA = QA - 0.005794 * Sin(JB) + 0.002347 * Cos(JB)
        QA = QA + 0.009872 * Sin(JC) + 0.008803 * Sin(2 * JC)
        QA = QA - 0.004308 * Sin(3 * JC)

        UX = Sin(JB): UY = Cos(JB): UZ = Sin(J4)
        VA = Cos(J4): VB = Sin(2 * J4): VC = Cos(2 * J4)
        QG = (0.000458 * UX - 0.000642 * UY - 0.000517 * Cos(4 * JC)) * UZ
        QG = QG - (0.000347 * UX + 0.000853 * UY + 0.000517 * Sin(4 * JB)) * VA
        QG = QG + 0.000403 * (Cos(2 * JC) * VB + Sin(2 * JC) * VC)
        QG = Radians(QG)

        QB = -25948 + 4985 * Cos(JA) - 1230 * VA + 3354 * UY
        QB = QB + 904 * Cos(2 * JC) + 894 * (Cos(JC) - Cos(3 * JC))
        QB = QB + (5795 * VA - 1165 * UZ + 1388 * VC) * UX
        QB = QB + (1351 * VA + 5702 * UZ + 1388 * VB) * UY
        QB = QB * 0.000001: Return

5675    JA = J8 - J2: JB = J8 - J3: JC = J8 - J4
        QC = (0.001089 * J1 - 0.589833) * VJ
        QC = QC + (0.004658 * J1 - 0.056094) * UU - 0.024286 * UV
        QC = Radians(QC)

        VK = 0.024039 * VJ - 0.025303 * UU + 0.006206 * UV
        VK = VK - 0.005992 * UW: QE = QC - (Radians(VK) / PL(IP, 4))

        QD = 4389 * VJ + 1129 * UV + 4262 * UU + 1089 * UW
        QD = QD * 0.0000001

        QF = 8189 * UU - 817 * VJ + 781 * UW: QF = QF * 0.000001

        VD = Sin(2 * JC): VE = Cos(2 * JC)
        VF = Sin(J8): VG = Cos(J8)
        QA = -0.009556 * Sin(JA) - 0.005178 * Sin(JB)
        QA = QA + 0.002572 * VD - 0.002972 * VE * VF - 0.002833 * VD * VG

        QG = 0.000336 * VE * VF + 0.000364 * VD * VG: QG = Radians(QG)

        QB = -40596 + 4992 * Cos(JA) + 2744 * Cos(JB)
        QB = QB + 2044 * Cos(JC) + 1051 * VE: QB = QB * 0.000001

        Return
        
5700    PlanetDist = VO

End Function

Function PlanetHLat(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, S As String) As Double

Const a11 = 178.179078, a12 = 415.2057519, a13 = 0.0003011, a14 = 0
Const a21 = 75.899697, a22 = 1.5554889, a23 = 0.0002947, a24 = 0
Const a31 = 0.20561421, a32 = 0.00002046, a33 = -0.00000003, a34 = 0
Const a41 = 7.002881, a42 = 0.0018608, a43 = -0.0000183, a44 = 0
Const a51 = 47.145944, a52 = 1.1852083, a53 = 0.0001739, a54 = 0
Const a61 = 0.3870986, a62 = 6.74, a63 = -0.42

Const b11 = 342.767053, b12 = 162.5533664, b13 = 0.0003097, b14 = 0
Const b21 = 130.163833, b22 = 1.4080361, b23 = -0.0009764, b24 = 0
Const b31 = 0.00682069, b32 = -0.00004774, b33 = 0.000000091, b34 = 0
Const b41 = 3.393631, b42 = 0.0010058, b43 = -0.000001, b44 = 0
Const b51 = 75.779647, b52 = 0.89985, b53 = 0.00041, b54 = 0
Const b61 = 0.7233316, b62 = 16.92, b63 = -4.4

Const c11 = 293.737334, c12 = 53.17137642, c13 = 0.0003107, c14 = 0
Const c21 = 334.218203, c22 = 1.8407584, c23 = 0.0001299, c24 = -0.00000119
Const c31 = 0.0933129, c32 = 0.000092064, c33 = -0.000000077, c34 = 0
Const c41 = 1.850333, c42 = -0.000675, c43 = 0.0000126, c44 = 0
Const c51 = 48.786442, c52 = 0.7709917, c53 = -0.0000014, c54 = -0.00000533
Const c61 = 1.5236883, c62 = 9.36, c63 = -1.52

Const d11 = 238.049257, d12 = 8.434172183, d13 = 0.0003347, d14 = -0.00000165
Const d21 = 12.720972, d22 = 1.6099617, d23 = 0.00105627, d24 = -0.00000343
Const d31 = 0.04833475, d32 = 0.00016418, d33 = -0.0000004676, d34 = -0.0000000017
Const d41 = 1.308736, d42 = -0.0056961, d43 = 0.0000039, d44 = 0
Const d51 = 99.443414, d52 = 1.01053, d53 = 0.00035222, d54 = -0.00000851
Const d61 = 5.202561, d62 = 196.74, d63 = -9.4

Const e11 = 266.564377, e12 = 3.398638567, e13 = 0.0003245, e14 = -0.0000058
Const e21 = 91.098214, e22 = 1.9584158, e23 = 0.00082636, e24 = 0.00000461
Const e31 = 0.05589232, e32 = -0.0003455, e33 = -0.000000728, e34 = 0.00000000074
Const e41 = 2.492519, e42 = -0.0039189, e43 = -0.00001549, e44 = 0.00000004
Const e51 = 112.790414, e52 = 0.8731951, e53 = -0.00015218, e54 = -0.00000531
Const e61 = 9.554747, e62 = 165.6, e63 = -8.88

Const f11 = 244.19747, f12 = 1.194065406, f13 = 0.000316, f14 = -0.0000006
Const f21 = 171.548692, f22 = 1.4844328, f23 = 0.0002372, f24 = -0.00000061
Const f31 = 0.0463444, f32 = -0.00002658, f33 = 0.000000077, f34 = 0
Const f41 = 0.772464, f42 = 0.0006253, f43 = 0.0000395, f44 = 0
Const f51 = 73.477111, f52 = 0.4986678, f53 = 0.0013117, f54 = 0
Const f61 = 19.21814, f62 = 65.8, f63 = -7.19

Const g11 = 84.457994, g12 = 0.6107942056, g13 = 0.0003205, g14 = -0.0000006
Const g21 = 46.727364, g22 = 1.4245744, g23 = 0.00039082, g24 = -0.000000605
Const g31 = 0.00899704, g32 = 0.00000633, g33 = -0.000000002, g34 = 0
Const g41 = 1.779242, g42 = -0.0095436, g43 = -0.0000091, g44 = 0
Const g51 = 130.681389, g52 = 1.098935, g53 = 0.00024987, g54 = -0.000004718
Const g61 = 30.10957, g62 = 62.2, g63 = -6.87

Dim IP As Integer, I As Integer, J As Integer, K As Integer
Dim PL(7, 9) As Double, AP(7) As Double

        IP = 0: B = LctUT(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        A = CDJD(GD, GM, GY)
        T = ((A - 2415020#) / 36525#) + (B / 876600#)

        Select Case UCase(S)
            Case "MERCURY"
                IP = 1
           Case "VENUS"
                IP = 2
            Case "MARS"
                IP = 3
           Case "JUPITER"
                IP = 4
            Case "SATURN"
                IP = 5
            Case "URANUS"
                IP = 6
            Case "NEPTUNE"
                IP = 7
            Case Else
                EP = 0
                GoTo 5700
        End Select
    
        I = 1
        A0 = a11: A1 = a12: A2 = a13: A3 = a14
        B0 = a21: B1 = a22: B2 = a23: B3 = a24
        C0 = a31: C1 = a32: C2 = a33: C3 = a34
        D0 = a41: D1 = a42: D2 = a43: D3 = a44
        E0 = a51: E1 = a52: E2 = a53: E3 = a54
        F = a61: G = a62: H = a63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 2
        A0 = b11: A1 = b12: A2 = b13: A3 = b14
        B0 = b21: B1 = b22: B2 = b23: B3 = b24
        C0 = b31: C1 = b32: C2 = b33: C3 = b34
        D0 = b41: D1 = b42: D2 = b43: D3 = b44
        E0 = b51: E1 = b52: E2 = b53: E3 = b54
        F = b61: G = b62: H = b63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 3
        A0 = c11: A1 = c12: A2 = c13: A3 = c14
        B0 = c21: B1 = c22: B2 = c23: B3 = c24
        C0 = c31: C1 = c32: C2 = c33: C3 = c34
        D0 = c41: D1 = c42: D2 = c43: D3 = c44
        E0 = c51: E1 = c52: E2 = c53: E3 = c54
        F = c61: G = c62: H = c63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 4
        A0 = d11: A1 = d12: A2 = d13: A3 = d14
        B0 = d21: B1 = d22: B2 = d23: B3 = d24
        C0 = d31: C1 = d32: C2 = d33: C3 = d34
        D0 = d41: D1 = d42: D2 = d43: D3 = d44
        E0 = d51: E1 = d52: E2 = d53: E3 = d54
        F = d61: G = d62: H = d63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 5
        A0 = e11: A1 = e12: A2 = e13: A3 = e14
        B0 = e21: B1 = e22: B2 = e23: B3 = e24
        C0 = e31: C1 = e32: C2 = e33: C3 = e34
        D0 = e41: D1 = e42: D2 = e43: D3 = e44
        E0 = e51: E1 = e52: E2 = e53: E3 = e54
        F = e61: G = e62: H = e63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 6
        A0 = f11: A1 = f12: A2 = f13: A3 = f14
        B0 = f21: B1 = f22: B2 = f23: B3 = f24
        C0 = f31: C1 = f32: C2 = f33: C3 = f34
        D0 = f41: D1 = f42: D2 = f43: D3 = f44
        E0 = f51: E1 = f52: E2 = f53: E3 = f54
        F = f61: G = f62: H = f63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 7
        A0 = g11: A1 = g12: A2 = g13: A3 = g14
        B0 = g21: B1 = g22: B2 = g23: B3 = g24
        C0 = g31: C1 = g32: C2 = g33: C3 = g34
        D0 = g41: D1 = g42: D2 = g43: D3 = g44
        E0 = g51: E1 = g52: E2 = g53: E3 = g54
        F = g61: G = g62: H = g63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        LI = 0: Pi = 3.1415926536: TP = 2# * Pi
        MS = SunMeanAnomaly(LH, LM, LS, DS, ZC, DY, MN, YR)
        SR = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR))
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR): LG = SR + Pi

        K = 1
        
        For J = 1 To 7
        AP(J) = Radians(PL(J, 1) - PL(J, 3) - LI * PL(J, 2))
        Next J

        QA = 0: QB = 0: QC = 0: QD = 0: QE = 0: QF = 0: QG = 0
        If (IP < 5) Then
            On IP GoSub 4685, 4735, 4810, 4945
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoSub 4945, 4945, 4945
        End If

        EC = PL(IP, 4) + QD: AM = AP(IP) + QE: AT = TrueAnomaly(AM, EC)
        PVV = (PL(IP, 7) + QF) * (1# - EC * EC) / (1# + EC * Cos(AT))
        LP = Degrees(AT) + PL(IP, 3) + Degrees(QC - QE): LP = Radians(LP)
        OM = Radians(PL(IP, 6)): LO = LP - OM
        SO = Sin(LO): CO = Cos(LO)
        INN = Radians(PL(IP, 5)): PVV = PVV + QB
        SP = SO * Sin(INN): Y = SO * Cos(INN)
        PS = Asin(SP) + QG: SP = Sin(PS)
        PD = Atan2(CO, Y) + OM + Radians(QA)
        PD = Unwind(PD)
        CI = Cos(PS): RD = PVV * CI: LL = PD - LG
        RH = RE * RE + PVV * PVV - 2# * RE * PVV * CI * Cos(LL)
        RH = Sqr(RH): LI = RH * 0.005775518

        GoTo 5700

4685    QA = 0.00204 * Cos(5 * AP(2) - 2 * AP(1) + 0.21328)
        QA = QA + 0.00103 * Cos(2 * AP(2) - AP(1) - 2.8046)
        QA = QA + 0.00091 * Cos(2 * AP(4) - AP(1) - 0.64582)
        QA = QA + 0.00078 * Cos(5 * AP(2) - 3 * AP(1) + 0.17692)

        QB = 0.000007525 * Cos(2 * AP(4) - AP(1) + 0.925251)
        QB = QB + 0.000006802 * Cos(5 * AP(2) - 3 * AP(1) - 4.53642)
        QB = QB + 0.000005457 * Cos(2 * AP(2) - 2 * AP(1) - 1.24246)
        QB = QB + 0.000003569 * Cos(5 * AP(2) - AP(1) - 1.35699)
        Return

4735    QC = 0.00077 * Sin(4.1406 + T * 2.6227): QC = Radians(QC): QE = QC

        QA = 0.00313 * Cos(2 * MS - 2 * AP(2) - 2.587)
        QA = QA + 0.00198 * Cos(3 * MS - 3 * AP(2) + 0.044768)
        QA = QA + 0.00136 * Cos(MS - AP(2) - 2.0788)
        QA = QA + 0.00096 * Cos(3 * MS - 2 * AP(2) - 2.3721)
        QA = QA + 0.00082 * Cos(AP(4) - AP(2) - 3.6318)

        QB = 0.000022501 * Cos(2 * MS - 2 * AP(2) - 1.01592)
        QB = QB + 0.000019045 * Cos(3 * MS - 3 * AP(2) + 1.61577)
        QB = QB + 0.000006887 * Cos(AP(4) - AP(2) - 2.06106)
        QB = QB + 0.000005172 * Cos(MS - AP(2) - 0.508065)
        QB = QB + 0.00000362 * Cos(5 * MS - 4 * AP(2) - 1.81877)
        QB = QB + 0.000003283 * Cos(4 * MS - 4 * AP(2) + 1.10851)
        QB = QB + 0.000003074 * Cos(2 * AP(4) - 2 * AP(2) - 0.962846)
        Return

4810    A = 3 * AP(4) - 8 * AP(3) + 4 * MS: SA = Sin(A): CA = Cos(A)
        QC = -(0.01133 * SA + 0.00933 * CA): QC = Radians(QC): QE = QC

        QA = 0.00705 * Cos(AP(4) - AP(3) - 0.85448)
        QA = QA + 0.00607 * Cos(2 * AP(4) - AP(3) - 3.2873)
        QA = QA + 0.00445 * Cos(2 * AP(4) - 2 * AP(3) - 3.3492)
        QA = QA + 0.00388 * Cos(MS - 2 * AP(3) + 0.35771)
        QA = QA + 0.00238 * Cos(MS - AP(3) + 0.61256)
        QA = QA + 0.00204 * Cos(2 * MS - 3 * AP(3) + 2.7688)
        QA = QA + 0.00177 * Cos(3 * AP(3) - AP(2) - 1.0053)
        QA = QA + 0.00136 * Cos(2 * MS - 4 * AP(3) + 2.6894)
        QA = QA + 0.00104 * Cos(AP(4) + 0.30749)

        QB = 0.000053227 * Cos(AP(4) - AP(3) + 0.717864)
        QB = QB + 0.000050989 * Cos(2 * AP(4) - 2 * AP(3) - 1.77997)
        QB = QB + 0.000038278 * Cos(2 * AP(4) - AP(3) - 1.71617)
        QB = QB + 0.000015996 * Cos(MS - AP(3) - 0.969618)
        QB = QB + 0.000014764 * Cos(2 * MS - 3 * AP(3) + 1.19768)
        QB = QB + 0.000008966 * Cos(AP(4) - 2 * AP(3) + 0.761225)
        QB = QB + 0.000007914 * Cos(3 * AP(4) - 2 * AP(3) - 2.43887)
        QB = QB + 0.000007004 * Cos(2 * AP(4) - 3 * AP(3) - 1.79573)
        QB = QB + 0.00000662 * Cos(MS - 2 * AP(3) + 1.97575)
        QB = QB + 0.00000493 * Cos(3 * AP(4) - 3 * AP(3) - 1.33069)
        QB = QB + 0.000004693 * Cos(3 * MS - 5 * AP(3) + 3.32665)
        QB = QB + 0.000004571 * Cos(2 * MS - 4 * AP(3) + 4.27086)
        QB = QB + 0.000004409 * Cos(3 * AP(4) - AP(3) - 2.02158)
        Return

4945    J1 = T / 5# + 0.1: J2 = Unwind(4.14473 + 52.9691 * T)
        J3 = Unwind(4.641118 + 21.32991 * T)
        J4 = Unwind(4.250177 + 7.478172 * T)
        J5 = 5# * J3 - 2# * J2: J6 = 2# * J2 - 6# * J3 + 3# * J4

        If (IP < 5) Then
            On IP GoTo 5190, 5190, 5190, 4980
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoTo 4980, 5505, 5505, 5190
        End If

4980    J7 = J3 - J2: U1 = Sin(J3): U2 = Cos(J3): U3 = Sin(2# * J3)
        U4 = Cos(2# * J3): U5 = Sin(J5): U6 = Cos(J5)
        U7 = Sin(2# * J5): U8 = Sin(J6): U9 = Sin(J7)
        UA = Cos(J7): UB = Sin(2# * J7): UC = Cos(2# * J7)
        UD = Sin(3# * J7): UE = Cos(3# * J7): UF = Sin(4# * J7)
        UG = Cos(4# * J7): VH = Cos(5# * J7)
        
        If (IP = 5) Then
            GoTo 5200
        End If

        QC = (0.331364 - (0.010281 + 0.004692 * J1) * J1) * U5
        QC = QC + (0.003228 - (0.064436 - 0.002075 * J1) * J1) * U6
        QC = QC - (0.003083 + (0.000275 - 0.000489 * J1) * J1) * U7
        QC = QC + 0.002472 * U8 + 0.013619 * U9 + 0.018472 * UB
        QC = QC + 0.006717 * UD + 0.002775 * UF + 0.006417 * UB * U1
        QC = QC + (0.007275 - 0.001253 * J1) * U9 * U1 + 0.002439 * UD * U1
        QC = QC - (0.035681 + 0.001208 * J1) * U9 * U2 - 0.003767 * UC * U1
        QC = QC - (0.033839 + 0.001125 * J1) * UA * U1 - 0.004261 * UB * U2
        QC = QC + (0.001161 * J1 - 0.006333) * UA * U2 + 0.002178 * U2
        QC = QC - 0.006675 * UC * U2 - 0.002664 * UE * U2 - 0.002572 * U9 * U3
        QC = QC - 0.003567 * UB * U3 + 0.002094 * UA * U4 + 0.003342 * UC * U4
        QC = Radians(QC)

        QD = (3606 + (130 - 43 * J1) * J1) * U5 + (1289 - 580 * J1) * U6
        QD = QD - 6764 * U9 * U1 - 1110 * UB * U1 - 224 * UD * U1 - 204 * U1
        QD = QD + (1284 + 116 * J1) * UA * U1 + 188 * UC * U1
        QD = QD + (1460 + 130 * J1) * U9 * U2 + 224 * UB * U2 - 817 * U2
        QD = QD + 6074 * U2 * UA + 992 * UC * U2 + 508 * UE * U2 + 230 * UG * U2
        QD = QD + 108 * VH * U2 - (956 + 73 * J1) * U9 * U3 + 448 * UB * U3
        QD = QD + 137 * UD * U3 + (108 * J1 - 997) * UA * U3 + 480 * UC * U3
        QD = QD + 148 * UE * U3 + (99 * J1 - 956) * U9 * U4 + 490 * UB * U4
        QD = QD + 158 * UD * U4 + 179 * U4 + (1024 + 75 * J1) * UA * U4
        QD = QD - 437 * UC * U4 - 132 * UE * U4: QD = QD * 0.0000001

        VK = (0.007192 - 0.003147 * J1) * U5 - 0.004344 * U1
        VK = VK + (J1 * (0.000197 * J1 - 0.000675) - 0.020428) * U6
        VK = VK + 0.034036 * UA * U1 + (0.007269 + 0.000672 * J1) * U9 * U1
        VK = VK + 0.005614 * UC * U1 + 0.002964 * UE * U1 + 0.037761 * U9 * U2
        VK = VK + 0.006158 * UB * U2 - 0.006603 * UA * U2 - 0.005356 * U9 * U3
        VK = VK + 0.002722 * UB * U3 + 0.004483 * UA * U3
        VK = VK - 0.002642 * UC * U3 + 0.004403 * U9 * U4
        VK = VK - 0.002536 * UB * U4 + 0.005547 * UA * U4 - 0.002689 * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 205 * UA - 263 * U6 + 693 * UC + 312 * UE + 147 * UG + 299 * U9 * U1
        QF = QF + 181 * UC * U1 + 204 * UB * U2 + 111 * UD * U2 - 337 * UA * U2
        QF = QF - 111 * UC * U2: QF = QF * 0.000001
5190    Return

5200    UI = Sin(3 * J3): UJ = Cos(3 * J3): UK = Sin(4 * J3)
        UL = Cos(4 * J3): VI = Cos(2 * J5): UN = Sin(5 * J7)
        J8 = J4 - J3: UO = Sin(2 * J8): UP = Cos(2 * J8)
        UQ = Sin(3 * J8): UR = Cos(3 * J8)

        QC = 0.007581 * U7 - 0.007986 * U8 - 0.148811 * U9
        QC = QC - (0.814181 - (0.01815 - 0.016714 * J1) * J1) * U5
        QC = QC - (0.010497 - (0.160906 - 0.0041 * J1) * J1) * U6
        QC = QC - 0.015208 * UD - 0.006339 * UF - 0.006244 * U1
        QC = QC - 0.0165 * UB * U1 - 0.040786 * UB
        QC = QC + (0.008931 + 0.002728 * J1) * U9 * U1 - 0.005775 * UD * U1
        QC = QC + (0.081344 + 0.003206 * J1) * UA * U1 + 0.015019 * UC * U1
        QC = QC + (0.085581 + 0.002494 * J1) * U9 * U2 + 0.014394 * UC * U2
        QC = QC + (0.025328 - 0.003117 * J1) * UA * U2 + 0.006319 * UE * U2
        QC = QC + 0.006369 * U9 * U3 + 0.009156 * UB * U3 + 0.007525 * UQ * U3
        QC = QC - 0.005236 * UA * U4 - 0.007736 * UC * U4 - 0.007528 * UR * U4
        QC = Radians(QC)

        QD = (-7927 + (2548 + 91 * J1) * J1) * U5
        QD = QD + (13381 + (1226 - 253 * J1) * J1) * U6 + (248 - 121 * J1) * U7
        QD = QD - (305 + 91 * J1) * VI + 412 * UB + 12415 * U1
        QD = QD + (390 - 617 * J1) * U9 * U1 + (165 - 204 * J1) * UB * U1
        QD = QD + 26599 * UA * U1 - 4687 * UC * U1 - 1870 * UE * U1 - 821 * UG * U1
        QD = QD - 377 * VH * U1 + 497 * UP * U1 + (163 - 611 * J1) * U2
        QD = QD - 12696 * U9 * U2 - 4200 * UB * U2 - 1503 * UD * U2 - 619 * UF * U2
        QD = QD - 268 * UN * U2 - (282 + 1306 * J1) * UA * U2
        QD = QD + (-86 + 230 * J1) * UC * U2 + 461 * UO * U2 - 350 * U3
        QD = QD + (2211 - 286 * J1) * U9 * U3 - 2208 * UB * U3 - 568 * UD * U3
        QD = QD - 346 * UF * U3 - (2780 + 222 * J1) * UA * U3
        QD = QD + (2022 + 263 * J1) * UC * U3 + 248 * UE * U3 + 242 * UQ * U3
        QD = QD + 467 * UR * U3 - 490 * U4 - (2842 + 279 * J1) * U9 * U4
        QD = QD + (128 + 226 * J1) * UB * U4 + 224 * UD * U4
        QD = QD + (-1594 + 282 * J1) * UA * U4 + (2162 - 207 * J1) * UC * U4
        QD = QD + 561 * UE * U4 + 343 * UG * U4 + 469 * UQ * U4 - 242 * UR * U4
        QD = QD - 205 * U9 * UI + 262 * UD * UI + 208 * UA * UJ - 271 * UE * UJ
        QD = QD - 382 * UE * UK - 376 * UD * UL: QD = QD * 0.0000001

        VK = (0.077108 + (0.007186 - 0.001533 * J1) * J1) * U5
        VK = VK - 0.007075 * U9
        VK = VK + (0.045803 - (0.014766 + 0.000536 * J1) * J1) * U6
        VK = VK - 0.072586 * U2 - 0.075825 * U9 * U1 - 0.024839 * UB * U1
        VK = VK - 0.008631 * UD * U1 - 0.150383 * UA * U2
        VK = VK + 0.026897 * UC * U2 + 0.010053 * UE * U2
        VK = VK - (0.013597 + 0.001719 * J1) * U9 * U3 + 0.011981 * UB * U4
        VK = VK - (0.007742 - 0.001517 * J1) * UA * U3
        VK = VK + (0.013586 - 0.001375 * J1) * UC * U3
        VK = VK - (0.013667 - 0.001239 * J1) * U9 * U4
        VK = VK + (0.014861 + 0.001136 * J1) * UA * U4
        VK = VK - (0.013064 + 0.001628 * J1) * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 572 * U5 - 1590 * UB * U2 + 2933 * U6 - 647 * UD * U2
        QF = QF + 33629 * UA - 344 * UF * U2 - 3081 * UC + 2885 * UA * U2
        QF = QF - 1423 * UE + (2172 + 102 * J1) * UC * U2 - 671 * UG
        QF = QF + 296 * UE * U2 - 320 * VH - 267 * UB * U3 + 1098 * U1
        QF = QF - 778 * UA * U3 - 2812 * U9 * U1 + 495 * UC * U3 + 688 * UB * U1
        QF = QF + 250 * UE * U3 - 393 * UD * U1 - 856 * U9 * U4 - 228 * UF * U1
        QF = QF + 441 * UB * U4 + 2138 * UA * U1 + 296 * UC * U4 - 999 * UC * U1
        QF = QF + 211 * UE * U4 - 642 * UE * U1 - 427 * U9 * UI - 325 * UG * U1
        QF = QF + 398 * UD * UI - 890 * U2 + 344 * UA * UJ + 2206 * U9 * U2
        QF = QF - 427 * UE * UJ: QF = QF * 0.000001

        QG = 0.000747 * UA * U1 + 0.001069 * UA * U2 + 0.002108 * UB * U3
        QG = QG + 0.001261 * UC * U3 + 0.001236 * UB * U4 - 0.002075 * UC * U4
        QG = Radians(QG): Return

5505    J8 = Unwind(1.46205 + 3.81337 * T): J9 = 2 * J8 - J4
        VJ = Sin(J9): UU = Cos(J9): UV = Sin(2 * J9)
        UW = Cos(2 * J9)
        
        If (IP = 7) Then
            GoTo 5675
        End If

        JA = J4 - J2: JB = J4 - J3: JC = J8 - J4
        QC = (0.864319 - 0.001583 * J1) * VJ
        QC = QC + (0.082222 - 0.006833 * J1) * UU + 0.036017 * UV
        QC = QC - 0.003019 * UW + 0.008122 * Sin(J6): QC = Radians(QC)

        VK = 0.120303 * VJ + 0.006197 * UV
        VK = VK + (0.019472 - 0.000947 * J1) * UU
        QE = QC - (Radians(VK) / PL(IP, 4))

        QD = (163 * J1 - 3349) * VJ + 20981 * UU + 1311 * UW: QD = QD * 0.0000001

        QF = -0.003825 * UU

        QA = (-0.038581 + (0.002031 - 0.00191 * J1) * J1) * Cos(J4 + JB)
        QA = QA + (0.010122 - 0.000988 * J1) * Sin(J4 + JB)
        A = (0.034964 - (0.001038 - 0.000868 * J1) * J1) * Cos(2 * J4 + JB)
        QA = A + QA + 0.005594 * Sin(J4 + 3 * JC) - 0.014808 * Sin(JA)
        QA = QA - 0.005794 * Sin(JB) + 0.002347 * Cos(JB)
        QA = QA + 0.009872 * Sin(JC) + 0.008803 * Sin(2 * JC)
        QA = QA - 0.004308 * Sin(3 * JC)

        UX = Sin(JB): UY = Cos(JB): UZ = Sin(J4)
        VA = Cos(J4): VB = Sin(2 * J4): VC = Cos(2 * J4)
        QG = (0.000458 * UX - 0.000642 * UY - 0.000517 * Cos(4 * JC)) * UZ
        QG = QG - (0.000347 * UX + 0.000853 * UY + 0.000517 * Sin(4 * JB)) * VA
        QG = QG + 0.000403 * (Cos(2 * JC) * VB + Sin(2 * JC) * VC)
        QG = Radians(QG)

        QB = -25948 + 4985 * Cos(JA) - 1230 * VA + 3354 * UY
        QB = QB + 904 * Cos(2 * JC) + 894 * (Cos(JC) - Cos(3 * JC))
        QB = QB + (5795 * VA - 1165 * UZ + 1388 * VC) * UX
        QB = QB + (1351 * VA + 5702 * UZ + 1388 * VB) * UY
        QB = QB * 0.000001: Return

5675    JA = J8 - J2: JB = J8 - J3: JC = J8 - J4
        QC = (0.001089 * J1 - 0.589833) * VJ
        QC = QC + (0.004658 * J1 - 0.056094) * UU - 0.024286 * UV
        QC = Radians(QC)

        VK = 0.024039 * VJ - 0.025303 * UU + 0.006206 * UV
        VK = VK - 0.005992 * UW: QE = QC - (Radians(VK) / PL(IP, 4))

        QD = 4389 * VJ + 1129 * UV + 4262 * UU + 1089 * UW
        QD = QD * 0.0000001

        QF = 8189 * UU - 817 * VJ + 781 * UW: QF = QF * 0.000001

        VD = Sin(2 * JC): VE = Cos(2 * JC)
        VF = Sin(J8): VG = Cos(J8)
        QA = -0.009556 * Sin(JA) - 0.005178 * Sin(JB)
        QA = QA + 0.002572 * VD - 0.002972 * VE * VF - 0.002833 * VD * VG

        QG = 0.000336 * VE * VF + 0.000364 * VD * VG: QG = Radians(QG)

        QB = -40596 + 4992 * Cos(JA) + 2744 * Cos(JB)
        QB = QB + 2044 * Cos(JC) + 1051 * VE: QB = QB * 0.000001

        Return
        
5700    PlanetHLat = Degrees(PS)

End Function
Function PlanetHLong1(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, S As String) As Double

Const a11 = 178.179078, a12 = 415.2057519, a13 = 0.0003011, a14 = 0
Const a21 = 75.899697, a22 = 1.5554889, a23 = 0.0002947, a24 = 0
Const a31 = 0.20561421, a32 = 0.00002046, a33 = -0.00000003, a34 = 0
Const a41 = 7.002881, a42 = 0.0018608, a43 = -0.0000183, a44 = 0
Const a51 = 47.145944, a52 = 1.1852083, a53 = 0.0001739, a54 = 0
Const a61 = 0.3870986, a62 = 6.74, a63 = -0.42

Const b11 = 342.767053, b12 = 162.5533664, b13 = 0.0003097, b14 = 0
Const b21 = 130.163833, b22 = 1.4080361, b23 = -0.0009764, b24 = 0
Const b31 = 0.00682069, b32 = -0.00004774, b33 = 0.000000091, b34 = 0
Const b41 = 3.393631, b42 = 0.0010058, b43 = -0.000001, b44 = 0
Const b51 = 75.779647, b52 = 0.89985, b53 = 0.00041, b54 = 0
Const b61 = 0.7233316, b62 = 16.92, b63 = -4.4

Const c11 = 293.737334, c12 = 53.17137642, c13 = 0.0003107, c14 = 0
Const c21 = 334.218203, c22 = 1.8407584, c23 = 0.0001299, c24 = -0.00000119
Const c31 = 0.0933129, c32 = 0.000092064, c33 = -0.000000077, c34 = 0
Const c41 = 1.850333, c42 = -0.000675, c43 = 0.0000126, c44 = 0
Const c51 = 48.786442, c52 = 0.7709917, c53 = -0.0000014, c54 = -0.00000533
Const c61 = 1.5236883, c62 = 9.36, c63 = -1.52

Const d11 = 238.049257, d12 = 8.434172183, d13 = 0.0003347, d14 = -0.00000165
Const d21 = 12.720972, d22 = 1.6099617, d23 = 0.00105627, d24 = -0.00000343
Const d31 = 0.04833475, d32 = 0.00016418, d33 = -0.0000004676, d34 = -0.0000000017
Const d41 = 1.308736, d42 = -0.0056961, d43 = 0.0000039, d44 = 0
Const d51 = 99.443414, d52 = 1.01053, d53 = 0.00035222, d54 = -0.00000851
Const d61 = 5.202561, d62 = 196.74, d63 = -9.4

Const e11 = 266.564377, e12 = 3.398638567, e13 = 0.0003245, e14 = -0.0000058
Const e21 = 91.098214, e22 = 1.9584158, e23 = 0.00082636, e24 = 0.00000461
Const e31 = 0.05589232, e32 = -0.0003455, e33 = -0.000000728, e34 = 0.00000000074
Const e41 = 2.492519, e42 = -0.0039189, e43 = -0.00001549, e44 = 0.00000004
Const e51 = 112.790414, e52 = 0.8731951, e53 = -0.00015218, e54 = -0.00000531
Const e61 = 9.554747, e62 = 165.6, e63 = -8.88

Const f11 = 244.19747, f12 = 1.194065406, f13 = 0.000316, f14 = -0.0000006
Const f21 = 171.548692, f22 = 1.4844328, f23 = 0.0002372, f24 = -0.00000061
Const f31 = 0.0463444, f32 = -0.00002658, f33 = 0.000000077, f34 = 0
Const f41 = 0.772464, f42 = 0.0006253, f43 = 0.0000395, f44 = 0
Const f51 = 73.477111, f52 = 0.4986678, f53 = 0.0013117, f54 = 0
Const f61 = 19.21814, f62 = 65.8, f63 = -7.19

Const g11 = 84.457994, g12 = 0.6107942056, g13 = 0.0003205, g14 = -0.0000006
Const g21 = 46.727364, g22 = 1.4245744, g23 = 0.00039082, g24 = -0.000000605
Const g31 = 0.00899704, g32 = 0.00000633, g33 = -0.000000002, g34 = 0
Const g41 = 1.779242, g42 = -0.0095436, g43 = -0.0000091, g44 = 0
Const g51 = 130.681389, g52 = 1.098935, g53 = 0.00024987, g54 = -0.000004718
Const g61 = 30.10957, g62 = 62.2, g63 = -6.87

Dim IP As Integer, I As Integer, J As Integer, K As Integer
Dim PL(7, 9) As Double, AP(7) As Double

        IP = 0: B = LctUT(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        A = CDJD(GD, GM, GY)
        T = ((A - 2415020#) / 36525#) + (B / 876600#)

        Select Case UCase(S)
            Case "MERCURY"
                IP = 1
           Case "VENUS"
                IP = 2
            Case "MARS"
                IP = 3
           Case "JUPITER"
                IP = 4
            Case "SATURN"
                IP = 5
            Case "URANUS"
                IP = 6
            Case "NEPTUNE"
                IP = 7
            Case Else
                EP = 0
                GoTo 5700
        End Select
    

        I = 1
        A0 = a11: A1 = a12: A2 = a13: A3 = a14
        B0 = a21: B1 = a22: B2 = a23: B3 = a24
        C0 = a31: C1 = a32: C2 = a33: C3 = a34
        D0 = a41: D1 = a42: D2 = a43: D3 = a44
        E0 = a51: E1 = a52: E2 = a53: E3 = a54
        F = a61: G = a62: H = a63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 2
        A0 = b11: A1 = b12: A2 = b13: A3 = b14
        B0 = b21: B1 = b22: B2 = b23: B3 = b24
        C0 = b31: C1 = b32: C2 = b33: C3 = b34
        D0 = b41: D1 = b42: D2 = b43: D3 = b44
        E0 = b51: E1 = b52: E2 = b53: E3 = b54
        F = b61: G = b62: H = b63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 3
        A0 = c11: A1 = c12: A2 = c13: A3 = c14
        B0 = c21: B1 = c22: B2 = c23: B3 = c24
        C0 = c31: C1 = c32: C2 = c33: C3 = c34
        D0 = c41: D1 = c42: D2 = c43: D3 = c44
        E0 = c51: E1 = c52: E2 = c53: E3 = c54
        F = c61: G = c62: H = c63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 4
        A0 = d11: A1 = d12: A2 = d13: A3 = d14
        B0 = d21: B1 = d22: B2 = d23: B3 = d24
        C0 = d31: C1 = d32: C2 = d33: C3 = d34
        D0 = d41: D1 = d42: D2 = d43: D3 = d44
        E0 = d51: E1 = d52: E2 = d53: E3 = d54
        F = d61: G = d62: H = d63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 5
        A0 = e11: A1 = e12: A2 = e13: A3 = e14
        B0 = e21: B1 = e22: B2 = e23: B3 = e24
        C0 = e31: C1 = e32: C2 = e33: C3 = e34
        D0 = e41: D1 = e42: D2 = e43: D3 = e44
        E0 = e51: E1 = e52: E2 = e53: E3 = e54
        F = e61: G = e62: H = e63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 6
        A0 = f11: A1 = f12: A2 = f13: A3 = f14
        B0 = f21: B1 = f22: B2 = f23: B3 = f24
        C0 = f31: C1 = f32: C2 = f33: C3 = f34
        D0 = f41: D1 = f42: D2 = f43: D3 = f44
        E0 = f51: E1 = f52: E2 = f53: E3 = f54
        F = f61: G = f62: H = f63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 7
        A0 = g11: A1 = g12: A2 = g13: A3 = g14
        B0 = g21: B1 = g22: B2 = g23: B3 = g24
        C0 = g31: C1 = g32: C2 = g33: C3 = g34
        D0 = g41: D1 = g42: D2 = g43: D3 = g44
        E0 = g51: E1 = g52: E2 = g53: E3 = g54
        F = g61: G = g62: H = g63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        LI = 0: Pi = 3.1415926536: TP = 2# * Pi
        MS = SunMeanAnomaly(LH, LM, LS, DS, ZC, DY, MN, YR)
        SR = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR))
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR): LG = SR + Pi

        K = 1

        For J = 1 To 7
        AP(J) = Radians(PL(J, 1) - PL(J, 3) - LI * PL(J, 2))
        Next J

        QA = 0: QB = 0: QC = 0: QD = 0: QE = 0: QF = 0: QG = 0
        If (IP < 5) Then
            On IP GoSub 4685, 4735, 4810, 4945
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoSub 4945, 4945, 4945
        End If

        EC = PL(IP, 4) + QD: AM = AP(IP) + QE: AT = TrueAnomaly(AM, EC)
        PVV = (PL(IP, 7) + QF) * (1# - EC * EC) / (1# + EC * Cos(AT))
        LP = Degrees(AT) + PL(IP, 3) + Degrees(QC - QE): LP = Radians(LP)
        OM = Radians(PL(IP, 6)): LO = LP - OM
        SO = Sin(LO): CO = Cos(LO)
        INN = Radians(PL(IP, 5)): PVV = PVV + QB
        SP = SO * Sin(INN): Y = SO * Cos(INN)
        PS = Asin(SP) + QG: SP = Sin(PS)
        PD = Atan2(CO, Y) + OM + Radians(QA)
        PD = Unwind(PD)
        CI = Cos(PS): RD = PVV * CI: LL = PD - LG
        RH = RE * RE + PVV * PVV - 2# * RE * PVV * CI * Cos(LL)
        RH = Sqr(RH): LI = RH * 0.005775518

        GoTo 5700

4685    QA = 0.00204 * Cos(5 * AP(2) - 2 * AP(1) + 0.21328)
        QA = QA + 0.00103 * Cos(2 * AP(2) - AP(1) - 2.8046)
        QA = QA + 0.00091 * Cos(2 * AP(4) - AP(1) - 0.64582)
        QA = QA + 0.00078 * Cos(5 * AP(2) - 3 * AP(1) + 0.17692)

        QB = 0.000007525 * Cos(2 * AP(4) - AP(1) + 0.925251)
        QB = QB + 0.000006802 * Cos(5 * AP(2) - 3 * AP(1) - 4.53642)
        QB = QB + 0.000005457 * Cos(2 * AP(2) - 2 * AP(1) - 1.24246)
        QB = QB + 0.000003569 * Cos(5 * AP(2) - AP(1) - 1.35699)
        Return

4735    QC = 0.00077 * Sin(4.1406 + T * 2.6227): QC = Radians(QC): QE = QC

        QA = 0.00313 * Cos(2 * MS - 2 * AP(2) - 2.587)
        QA = QA + 0.00198 * Cos(3 * MS - 3 * AP(2) + 0.044768)
        QA = QA + 0.00136 * Cos(MS - AP(2) - 2.0788)
        QA = QA + 0.00096 * Cos(3 * MS - 2 * AP(2) - 2.3721)
        QA = QA + 0.00082 * Cos(AP(4) - AP(2) - 3.6318)

        QB = 0.000022501 * Cos(2 * MS - 2 * AP(2) - 1.01592)
        QB = QB + 0.000019045 * Cos(3 * MS - 3 * AP(2) + 1.61577)
        QB = QB + 0.000006887 * Cos(AP(4) - AP(2) - 2.06106)
        QB = QB + 0.000005172 * Cos(MS - AP(2) - 0.508065)
        QB = QB + 0.00000362 * Cos(5 * MS - 4 * AP(2) - 1.81877)
        QB = QB + 0.000003283 * Cos(4 * MS - 4 * AP(2) + 1.10851)
        QB = QB + 0.000003074 * Cos(2 * AP(4) - 2 * AP(2) - 0.962846)
        Return

4810    A = 3 * AP(4) - 8 * AP(3) + 4 * MS: SA = Sin(A): CA = Cos(A)
        QC = -(0.01133 * SA + 0.00933 * CA): QC = Radians(QC): QE = QC

        QA = 0.00705 * Cos(AP(4) - AP(3) - 0.85448)
        QA = QA + 0.00607 * Cos(2 * AP(4) - AP(3) - 3.2873)
        QA = QA + 0.00445 * Cos(2 * AP(4) - 2 * AP(3) - 3.3492)
        QA = QA + 0.00388 * Cos(MS - 2 * AP(3) + 0.35771)
        QA = QA + 0.00238 * Cos(MS - AP(3) + 0.61256)
        QA = QA + 0.00204 * Cos(2 * MS - 3 * AP(3) + 2.7688)
        QA = QA + 0.00177 * Cos(3 * AP(3) - AP(2) - 1.0053)
        QA = QA + 0.00136 * Cos(2 * MS - 4 * AP(3) + 2.6894)
        QA = QA + 0.00104 * Cos(AP(4) + 0.30749)

        QB = 0.000053227 * Cos(AP(4) - AP(3) + 0.717864)
        QB = QB + 0.000050989 * Cos(2 * AP(4) - 2 * AP(3) - 1.77997)
        QB = QB + 0.000038278 * Cos(2 * AP(4) - AP(3) - 1.71617)
        QB = QB + 0.000015996 * Cos(MS - AP(3) - 0.969618)
        QB = QB + 0.000014764 * Cos(2 * MS - 3 * AP(3) + 1.19768)
        QB = QB + 0.000008966 * Cos(AP(4) - 2 * AP(3) + 0.761225)
        QB = QB + 0.000007914 * Cos(3 * AP(4) - 2 * AP(3) - 2.43887)
        QB = QB + 0.000007004 * Cos(2 * AP(4) - 3 * AP(3) - 1.79573)
        QB = QB + 0.00000662 * Cos(MS - 2 * AP(3) + 1.97575)
        QB = QB + 0.00000493 * Cos(3 * AP(4) - 3 * AP(3) - 1.33069)
        QB = QB + 0.000004693 * Cos(3 * MS - 5 * AP(3) + 3.32665)
        QB = QB + 0.000004571 * Cos(2 * MS - 4 * AP(3) + 4.27086)
        QB = QB + 0.000004409 * Cos(3 * AP(4) - AP(3) - 2.02158)
        Return

4945    J1 = T / 5# + 0.1: J2 = Unwind(4.14473 + 52.9691 * T)
        J3 = Unwind(4.641118 + 21.32991 * T)
        J4 = Unwind(4.250177 + 7.478172 * T)
        J5 = 5# * J3 - 2# * J2: J6 = 2# * J2 - 6# * J3 + 3# * J4

        If (IP < 5) Then
            On IP GoTo 5190, 5190, 5190, 4980
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoTo 4980, 5505, 5505, 5190
        End If

4980    J7 = J3 - J2: U1 = Sin(J3): U2 = Cos(J3): U3 = Sin(2# * J3)
        U4 = Cos(2# * J3): U5 = Sin(J5): U6 = Cos(J5)
        U7 = Sin(2# * J5): U8 = Sin(J6): U9 = Sin(J7)
        UA = Cos(J7): UB = Sin(2# * J7): UC = Cos(2# * J7)
        UD = Sin(3# * J7): UE = Cos(3# * J7): UF = Sin(4# * J7)
        UG = Cos(4# * J7): VH = Cos(5# * J7)
        
        If (IP = 5) Then
            GoTo 5200
        End If

        QC = (0.331364 - (0.010281 + 0.004692 * J1) * J1) * U5
        QC = QC + (0.003228 - (0.064436 - 0.002075 * J1) * J1) * U6
        QC = QC - (0.003083 + (0.000275 - 0.000489 * J1) * J1) * U7
        QC = QC + 0.002472 * U8 + 0.013619 * U9 + 0.018472 * UB
        QC = QC + 0.006717 * UD + 0.002775 * UF + 0.006417 * UB * U1
        QC = QC + (0.007275 - 0.001253 * J1) * U9 * U1 + 0.002439 * UD * U1
        QC = QC - (0.035681 + 0.001208 * J1) * U9 * U2 - 0.003767 * UC * U1
        QC = QC - (0.033839 + 0.001125 * J1) * UA * U1 - 0.004261 * UB * U2
        QC = QC + (0.001161 * J1 - 0.006333) * UA * U2 + 0.002178 * U2
        QC = QC - 0.006675 * UC * U2 - 0.002664 * UE * U2 - 0.002572 * U9 * U3
        QC = QC - 0.003567 * UB * U3 + 0.002094 * UA * U4 + 0.003342 * UC * U4
        QC = Radians(QC)

        QD = (3606 + (130 - 43 * J1) * J1) * U5 + (1289 - 580 * J1) * U6
        QD = QD - 6764 * U9 * U1 - 1110 * UB * U1 - 224 * UD * U1 - 204 * U1
        QD = QD + (1284 + 116 * J1) * UA * U1 + 188 * UC * U1
        QD = QD + (1460 + 130 * J1) * U9 * U2 + 224 * UB * U2 - 817 * U2
        QD = QD + 6074 * U2 * UA + 992 * UC * U2 + 508 * UE * U2 + 230 * UG * U2
        QD = QD + 108 * VH * U2 - (956 + 73 * J1) * U9 * U3 + 448 * UB * U3
        QD = QD + 137 * UD * U3 + (108 * J1 - 997) * UA * U3 + 480 * UC * U3
        QD = QD + 148 * UE * U3 + (99 * J1 - 956) * U9 * U4 + 490 * UB * U4
        QD = QD + 158 * UD * U4 + 179 * U4 + (1024 + 75 * J1) * UA * U4
        QD = QD - 437 * UC * U4 - 132 * UE * U4: QD = QD * 0.0000001

        VK = (0.007192 - 0.003147 * J1) * U5 - 0.004344 * U1
        VK = VK + (J1 * (0.000197 * J1 - 0.000675) - 0.020428) * U6
        VK = VK + 0.034036 * UA * U1 + (0.007269 + 0.000672 * J1) * U9 * U1
        VK = VK + 0.005614 * UC * U1 + 0.002964 * UE * U1 + 0.037761 * U9 * U2
        VK = VK + 0.006158 * UB * U2 - 0.006603 * UA * U2 - 0.005356 * U9 * U3
        VK = VK + 0.002722 * UB * U3 + 0.004483 * UA * U3
        VK = VK - 0.002642 * UC * U3 + 0.004403 * U9 * U4
        VK = VK - 0.002536 * UB * U4 + 0.005547 * UA * U4 - 0.002689 * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 205 * UA - 263 * U6 + 693 * UC + 312 * UE + 147 * UG + 299 * U9 * U1
        QF = QF + 181 * UC * U1 + 204 * UB * U2 + 111 * UD * U2 - 337 * UA * U2
        QF = QF - 111 * UC * U2: QF = QF * 0.000001
5190    Return

5200    UI = Sin(3 * J3): UJ = Cos(3 * J3): UK = Sin(4 * J3)
        UL = Cos(4 * J3): VI = Cos(2 * J5): UN = Sin(5 * J7)
        J8 = J4 - J3: UO = Sin(2 * J8): UP = Cos(2 * J8)
        UQ = Sin(3 * J8): UR = Cos(3 * J8)

        QC = 0.007581 * U7 - 0.007986 * U8 - 0.148811 * U9
        QC = QC - (0.814181 - (0.01815 - 0.016714 * J1) * J1) * U5
        QC = QC - (0.010497 - (0.160906 - 0.0041 * J1) * J1) * U6
        QC = QC - 0.015208 * UD - 0.006339 * UF - 0.006244 * U1
        QC = QC - 0.0165 * UB * U1 - 0.040786 * UB
        QC = QC + (0.008931 + 0.002728 * J1) * U9 * U1 - 0.005775 * UD * U1
        QC = QC + (0.081344 + 0.003206 * J1) * UA * U1 + 0.015019 * UC * U1
        QC = QC + (0.085581 + 0.002494 * J1) * U9 * U2 + 0.014394 * UC * U2
        QC = QC + (0.025328 - 0.003117 * J1) * UA * U2 + 0.006319 * UE * U2
        QC = QC + 0.006369 * U9 * U3 + 0.009156 * UB * U3 + 0.007525 * UQ * U3
        QC = QC - 0.005236 * UA * U4 - 0.007736 * UC * U4 - 0.007528 * UR * U4
        QC = Radians(QC)

        QD = (-7927 + (2548 + 91 * J1) * J1) * U5
        QD = QD + (13381 + (1226 - 253 * J1) * J1) * U6 + (248 - 121 * J1) * U7
        QD = QD - (305 + 91 * J1) * VI + 412 * UB + 12415 * U1
        QD = QD + (390 - 617 * J1) * U9 * U1 + (165 - 204 * J1) * UB * U1
        QD = QD + 26599 * UA * U1 - 4687 * UC * U1 - 1870 * UE * U1 - 821 * UG * U1
        QD = QD - 377 * VH * U1 + 497 * UP * U1 + (163 - 611 * J1) * U2
        QD = QD - 12696 * U9 * U2 - 4200 * UB * U2 - 1503 * UD * U2 - 619 * UF * U2
        QD = QD - 268 * UN * U2 - (282 + 1306 * J1) * UA * U2
        QD = QD + (-86 + 230 * J1) * UC * U2 + 461 * UO * U2 - 350 * U3
        QD = QD + (2211 - 286 * J1) * U9 * U3 - 2208 * UB * U3 - 568 * UD * U3
        QD = QD - 346 * UF * U3 - (2780 + 222 * J1) * UA * U3
        QD = QD + (2022 + 263 * J1) * UC * U3 + 248 * UE * U3 + 242 * UQ * U3
        QD = QD + 467 * UR * U3 - 490 * U4 - (2842 + 279 * J1) * U9 * U4
        QD = QD + (128 + 226 * J1) * UB * U4 + 224 * UD * U4
        QD = QD + (-1594 + 282 * J1) * UA * U4 + (2162 - 207 * J1) * UC * U4
        QD = QD + 561 * UE * U4 + 343 * UG * U4 + 469 * UQ * U4 - 242 * UR * U4
        QD = QD - 205 * U9 * UI + 262 * UD * UI + 208 * UA * UJ - 271 * UE * UJ
        QD = QD - 382 * UE * UK - 376 * UD * UL: QD = QD * 0.0000001

        VK = (0.077108 + (0.007186 - 0.001533 * J1) * J1) * U5
        VK = VK - 0.007075 * U9
        VK = VK + (0.045803 - (0.014766 + 0.000536 * J1) * J1) * U6
        VK = VK - 0.072586 * U2 - 0.075825 * U9 * U1 - 0.024839 * UB * U1
        VK = VK - 0.008631 * UD * U1 - 0.150383 * UA * U2
        VK = VK + 0.026897 * UC * U2 + 0.010053 * UE * U2
        VK = VK - (0.013597 + 0.001719 * J1) * U9 * U3 + 0.011981 * UB * U4
        VK = VK - (0.007742 - 0.001517 * J1) * UA * U3
        VK = VK + (0.013586 - 0.001375 * J1) * UC * U3
        VK = VK - (0.013667 - 0.001239 * J1) * U9 * U4
        VK = VK + (0.014861 + 0.001136 * J1) * UA * U4
        VK = VK - (0.013064 + 0.001628 * J1) * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 572 * U5 - 1590 * UB * U2 + 2933 * U6 - 647 * UD * U2
        QF = QF + 33629 * UA - 344 * UF * U2 - 3081 * UC + 2885 * UA * U2
        QF = QF - 1423 * UE + (2172 + 102 * J1) * UC * U2 - 671 * UG
        QF = QF + 296 * UE * U2 - 320 * VH - 267 * UB * U3 + 1098 * U1
        QF = QF - 778 * UA * U3 - 2812 * U9 * U1 + 495 * UC * U3 + 688 * UB * U1
        QF = QF + 250 * UE * U3 - 393 * UD * U1 - 856 * U9 * U4 - 228 * UF * U1
        QF = QF + 441 * UB * U4 + 2138 * UA * U1 + 296 * UC * U4 - 999 * UC * U1
        QF = QF + 211 * UE * U4 - 642 * UE * U1 - 427 * U9 * UI - 325 * UG * U1
        QF = QF + 398 * UD * UI - 890 * U2 + 344 * UA * UJ + 2206 * U9 * U2
        QF = QF - 427 * UE * UJ: QF = QF * 0.000001

        QG = 0.000747 * UA * U1 + 0.001069 * UA * U2 + 0.002108 * UB * U3
        QG = QG + 0.001261 * UC * U3 + 0.001236 * UB * U4 - 0.002075 * UC * U4
        QG = Radians(QG): Return

5505    J8 = Unwind(1.46205 + 3.81337 * T): J9 = 2 * J8 - J4
        VJ = Sin(J9): UU = Cos(J9): UV = Sin(2 * J9)
        UW = Cos(2 * J9)
        
        If (IP = 7) Then
            GoTo 5675
        End If

        JA = J4 - J2: JB = J4 - J3: JC = J8 - J4
        QC = (0.864319 - 0.001583 * J1) * VJ
        QC = QC + (0.082222 - 0.006833 * J1) * UU + 0.036017 * UV
        QC = QC - 0.003019 * UW + 0.008122 * Sin(J6): QC = Radians(QC)

        VK = 0.120303 * VJ + 0.006197 * UV
        VK = VK + (0.019472 - 0.000947 * J1) * UU
        QE = QC - (Radians(VK) / PL(IP, 4))

        QD = (163 * J1 - 3349) * VJ + 20981 * UU + 1311 * UW: QD = QD * 0.0000001

        QF = -0.003825 * UU

        QA = (-0.038581 + (0.002031 - 0.00191 * J1) * J1) * Cos(J4 + JB)
        QA = QA + (0.010122 - 0.000988 * J1) * Sin(J4 + JB)
        A = (0.034964 - (0.001038 - 0.000868 * J1) * J1) * Cos(2 * J4 + JB)
        QA = A + QA + 0.005594 * Sin(J4 + 3 * JC) - 0.014808 * Sin(JA)
        QA = QA - 0.005794 * Sin(JB) + 0.002347 * Cos(JB)
        QA = QA + 0.009872 * Sin(JC) + 0.008803 * Sin(2 * JC)
        QA = QA - 0.004308 * Sin(3 * JC)

        UX = Sin(JB): UY = Cos(JB): UZ = Sin(J4)
        VA = Cos(J4): VB = Sin(2 * J4): VC = Cos(2 * J4)
        QG = (0.000458 * UX - 0.000642 * UY - 0.000517 * Cos(4 * JC)) * UZ
        QG = QG - (0.000347 * UX + 0.000853 * UY + 0.000517 * Sin(4 * JB)) * VA
        QG = QG + 0.000403 * (Cos(2 * JC) * VB + Sin(2 * JC) * VC)
        QG = Radians(QG)

        QB = -25948 + 4985 * Cos(JA) - 1230 * VA + 3354 * UY
        QB = QB + 904 * Cos(2 * JC) + 894 * (Cos(JC) - Cos(3 * JC))
        QB = QB + (5795 * VA - 1165 * UZ + 1388 * VC) * UX
        QB = QB + (1351 * VA + 5702 * UZ + 1388 * VB) * UY
        QB = QB * 0.000001: Return

5675    JA = J8 - J2: JB = J8 - J3: JC = J8 - J4
        QC = (0.001089 * J1 - 0.589833) * VJ
        QC = QC + (0.004658 * J1 - 0.056094) * UU - 0.024286 * UV
        QC = Radians(QC)

        VK = 0.024039 * VJ - 0.025303 * UU + 0.006206 * UV
        VK = VK - 0.005992 * UW: QE = QC - (Radians(VK) / PL(IP, 4))

        QD = 4389 * VJ + 1129 * UV + 4262 * UU + 1089 * UW
        QD = QD * 0.0000001

        QF = 8189 * UU - 817 * VJ + 781 * UW: QF = QF * 0.000001

        VD = Sin(2 * JC): VE = Cos(2 * JC)
        VF = Sin(J8): VG = Cos(J8)
        QA = -0.009556 * Sin(JA) - 0.005178 * Sin(JB)
        QA = QA + 0.002572 * VD - 0.002972 * VE * VF - 0.002833 * VD * VG

        QG = 0.000336 * VE * VF + 0.000364 * VD * VG: QG = Radians(QG)

        QB = -40596 + 4992 * Cos(JA) + 2744 * Cos(JB)
        QB = QB + 2044 * Cos(JC) + 1051 * VE: QB = QB * 0.000001

        Return
        
5700    PlanetHLong1 = Degrees(LP)

End Function

Function PlanetHLong2(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, S As String) As Double

Const a11 = 178.179078, a12 = 415.2057519, a13 = 0.0003011, a14 = 0
Const a21 = 75.899697, a22 = 1.5554889, a23 = 0.0002947, a24 = 0
Const a31 = 0.20561421, a32 = 0.00002046, a33 = -0.00000003, a34 = 0
Const a41 = 7.002881, a42 = 0.0018608, a43 = -0.0000183, a44 = 0
Const a51 = 47.145944, a52 = 1.1852083, a53 = 0.0001739, a54 = 0
Const a61 = 0.3870986, a62 = 6.74, a63 = -0.42

Const b11 = 342.767053, b12 = 162.5533664, b13 = 0.0003097, b14 = 0
Const b21 = 130.163833, b22 = 1.4080361, b23 = -0.0009764, b24 = 0
Const b31 = 0.00682069, b32 = -0.00004774, b33 = 0.000000091, b34 = 0
Const b41 = 3.393631, b42 = 0.0010058, b43 = -0.000001, b44 = 0
Const b51 = 75.779647, b52 = 0.89985, b53 = 0.00041, b54 = 0
Const b61 = 0.7233316, b62 = 16.92, b63 = -4.4

Const c11 = 293.737334, c12 = 53.17137642, c13 = 0.0003107, c14 = 0
Const c21 = 334.218203, c22 = 1.8407584, c23 = 0.0001299, c24 = -0.00000119
Const c31 = 0.0933129, c32 = 0.000092064, c33 = -0.000000077, c34 = 0
Const c41 = 1.850333, c42 = -0.000675, c43 = 0.0000126, c44 = 0
Const c51 = 48.786442, c52 = 0.7709917, c53 = -0.0000014, c54 = -0.00000533
Const c61 = 1.5236883, c62 = 9.36, c63 = -1.52

Const d11 = 238.049257, d12 = 8.434172183, d13 = 0.0003347, d14 = -0.00000165
Const d21 = 12.720972, d22 = 1.6099617, d23 = 0.00105627, d24 = -0.00000343
Const d31 = 0.04833475, d32 = 0.00016418, d33 = -0.0000004676, d34 = -0.0000000017
Const d41 = 1.308736, d42 = -0.0056961, d43 = 0.0000039, d44 = 0
Const d51 = 99.443414, d52 = 1.01053, d53 = 0.00035222, d54 = -0.00000851
Const d61 = 5.202561, d62 = 196.74, d63 = -9.4

Const e11 = 266.564377, e12 = 3.398638567, e13 = 0.0003245, e14 = -0.0000058
Const e21 = 91.098214, e22 = 1.9584158, e23 = 0.00082636, e24 = 0.00000461
Const e31 = 0.05589232, e32 = -0.0003455, e33 = -0.000000728, e34 = 0.00000000074
Const e41 = 2.492519, e42 = -0.0039189, e43 = -0.00001549, e44 = 0.00000004
Const e51 = 112.790414, e52 = 0.8731951, e53 = -0.00015218, e54 = -0.00000531
Const e61 = 9.554747, e62 = 165.6, e63 = -8.88

Const f11 = 244.19747, f12 = 1.194065406, f13 = 0.000316, f14 = -0.0000006
Const f21 = 171.548692, f22 = 1.4844328, f23 = 0.0002372, f24 = -0.00000061
Const f31 = 0.0463444, f32 = -0.00002658, f33 = 0.000000077, f34 = 0
Const f41 = 0.772464, f42 = 0.0006253, f43 = 0.0000395, f44 = 0
Const f51 = 73.477111, f52 = 0.4986678, f53 = 0.0013117, f54 = 0
Const f61 = 19.21814, f62 = 65.8, f63 = -7.19

Const g11 = 84.457994, g12 = 0.6107942056, g13 = 0.0003205, g14 = -0.0000006
Const g21 = 46.727364, g22 = 1.4245744, g23 = 0.00039082, g24 = -0.000000605
Const g31 = 0.00899704, g32 = 0.00000633, g33 = -0.000000002, g34 = 0
Const g41 = 1.779242, g42 = -0.0095436, g43 = -0.0000091, g44 = 0
Const g51 = 130.681389, g52 = 1.098935, g53 = 0.00024987, g54 = -0.000004718
Const g61 = 30.10957, g62 = 62.2, g63 = -6.87

Dim IP As Integer, I As Integer, J As Integer, K As Integer
Dim PL(7, 9) As Double, AP(7) As Double

        IP = 0: B = LctUT(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        A = CDJD(GD, GM, GY)
        T = ((A - 2415020#) / 36525#) + (B / 876600#)

        Select Case UCase(S)
            Case "MERCURY"
                IP = 1
           Case "VENUS"
                IP = 2
            Case "MARS"
                IP = 3
           Case "JUPITER"
                IP = 4
            Case "SATURN"
                IP = 5
            Case "URANUS"
                IP = 6
            Case "NEPTUNE"
                IP = 7
            Case Else
                EP = 0
                GoTo 5700
        End Select
    

        I = 1
        A0 = a11: A1 = a12: A2 = a13: A3 = a14
        B0 = a21: B1 = a22: B2 = a23: B3 = a24
        C0 = a31: C1 = a32: C2 = a33: C3 = a34
        D0 = a41: D1 = a42: D2 = a43: D3 = a44
        E0 = a51: E1 = a52: E2 = a53: E3 = a54
        F = a61: G = a62: H = a63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 2
        A0 = b11: A1 = b12: A2 = b13: A3 = b14
        B0 = b21: B1 = b22: B2 = b23: B3 = b24
        C0 = b31: C1 = b32: C2 = b33: C3 = b34
        D0 = b41: D1 = b42: D2 = b43: D3 = b44
        E0 = b51: E1 = b52: E2 = b53: E3 = b54
        F = b61: G = b62: H = b63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 3
        A0 = c11: A1 = c12: A2 = c13: A3 = c14
        B0 = c21: B1 = c22: B2 = c23: B3 = c24
        C0 = c31: C1 = c32: C2 = c33: C3 = c34
        D0 = c41: D1 = c42: D2 = c43: D3 = c44
        E0 = c51: E1 = c52: E2 = c53: E3 = c54
        F = c61: G = c62: H = c63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 4
        A0 = d11: A1 = d12: A2 = d13: A3 = d14
        B0 = d21: B1 = d22: B2 = d23: B3 = d24
        C0 = d31: C1 = d32: C2 = d33: C3 = d34
        D0 = d41: D1 = d42: D2 = d43: D3 = d44
        E0 = d51: E1 = d52: E2 = d53: E3 = d54
        F = d61: G = d62: H = d63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 5
        A0 = e11: A1 = e12: A2 = e13: A3 = e14
        B0 = e21: B1 = e22: B2 = e23: B3 = e24
        C0 = e31: C1 = e32: C2 = e33: C3 = e34
        D0 = e41: D1 = e42: D2 = e43: D3 = e44
        E0 = e51: E1 = e52: E2 = e53: E3 = e54
        F = e61: G = e62: H = e63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 6
        A0 = f11: A1 = f12: A2 = f13: A3 = f14
        B0 = f21: B1 = f22: B2 = f23: B3 = f24
        C0 = f31: C1 = f32: C2 = f33: C3 = f34
        D0 = f41: D1 = f42: D2 = f43: D3 = f44
        E0 = f51: E1 = f52: E2 = f53: E3 = f54
        F = f61: G = f62: H = f63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 7
        A0 = g11: A1 = g12: A2 = g13: A3 = g14
        B0 = g21: B1 = g22: B2 = g23: B3 = g24
        C0 = g31: C1 = g32: C2 = g33: C3 = g34
        D0 = g41: D1 = g42: D2 = g43: D3 = g44
        E0 = g51: E1 = g52: E2 = g53: E3 = g54
        F = g61: G = g62: H = g63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        LI = 0: Pi = 3.1415926536: TP = 2# * Pi
        MS = SunMeanAnomaly(LH, LM, LS, DS, ZC, DY, MN, YR)
        SR = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR))
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR): LG = SR + Pi

        K = 1

        For J = 1 To 7
        AP(J) = Radians(PL(J, 1) - PL(J, 3) - LI * PL(J, 2))
        Next J

        QA = 0: QB = 0: QC = 0: QD = 0: QE = 0: QF = 0: QG = 0
        If (IP < 5) Then
            On IP GoSub 4685, 4735, 4810, 4945
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoSub 4945, 4945, 4945
        End If

        EC = PL(IP, 4) + QD: AM = AP(IP) + QE: AT = TrueAnomaly(AM, EC)
        PVV = (PL(IP, 7) + QF) * (1# - EC * EC) / (1# + EC * Cos(AT))
        LP = Degrees(AT) + PL(IP, 3) + Degrees(QC - QE): LP = Radians(LP)
        OM = Radians(PL(IP, 6)): LO = LP - OM
        SO = Sin(LO): CO = Cos(LO)
        INN = Radians(PL(IP, 5)): PVV = PVV + QB
        SP = SO * Sin(INN): Y = SO * Cos(INN)
        PS = Asin(SP) + QG: SP = Sin(PS)
        PD = Atan2(CO, Y) + OM + Radians(QA)
        PD = Unwind(PD)
        CI = Cos(PS): RD = PVV * CI: LL = PD - LG
        RH = RE * RE + PVV * PVV - 2# * RE * PVV * CI * Cos(LL)
        RH = Sqr(RH): LI = RH * 0.005775518

        GoTo 5700

4685    QA = 0.00204 * Cos(5 * AP(2) - 2 * AP(1) + 0.21328)
        QA = QA + 0.00103 * Cos(2 * AP(2) - AP(1) - 2.8046)
        QA = QA + 0.00091 * Cos(2 * AP(4) - AP(1) - 0.64582)
        QA = QA + 0.00078 * Cos(5 * AP(2) - 3 * AP(1) + 0.17692)

        QB = 0.000007525 * Cos(2 * AP(4) - AP(1) + 0.925251)
        QB = QB + 0.000006802 * Cos(5 * AP(2) - 3 * AP(1) - 4.53642)
        QB = QB + 0.000005457 * Cos(2 * AP(2) - 2 * AP(1) - 1.24246)
        QB = QB + 0.000003569 * Cos(5 * AP(2) - AP(1) - 1.35699)
        Return

4735    QC = 0.00077 * Sin(4.1406 + T * 2.6227): QC = Radians(QC): QE = QC

        QA = 0.00313 * Cos(2 * MS - 2 * AP(2) - 2.587)
        QA = QA + 0.00198 * Cos(3 * MS - 3 * AP(2) + 0.044768)
        QA = QA + 0.00136 * Cos(MS - AP(2) - 2.0788)
        QA = QA + 0.00096 * Cos(3 * MS - 2 * AP(2) - 2.3721)
        QA = QA + 0.00082 * Cos(AP(4) - AP(2) - 3.6318)

        QB = 0.000022501 * Cos(2 * MS - 2 * AP(2) - 1.01592)
        QB = QB + 0.000019045 * Cos(3 * MS - 3 * AP(2) + 1.61577)
        QB = QB + 0.000006887 * Cos(AP(4) - AP(2) - 2.06106)
        QB = QB + 0.000005172 * Cos(MS - AP(2) - 0.508065)
        QB = QB + 0.00000362 * Cos(5 * MS - 4 * AP(2) - 1.81877)
        QB = QB + 0.000003283 * Cos(4 * MS - 4 * AP(2) + 1.10851)
        QB = QB + 0.000003074 * Cos(2 * AP(4) - 2 * AP(2) - 0.962846)
        Return

4810    A = 3 * AP(4) - 8 * AP(3) + 4 * MS: SA = Sin(A): CA = Cos(A)
        QC = -(0.01133 * SA + 0.00933 * CA): QC = Radians(QC): QE = QC

        QA = 0.00705 * Cos(AP(4) - AP(3) - 0.85448)
        QA = QA + 0.00607 * Cos(2 * AP(4) - AP(3) - 3.2873)
        QA = QA + 0.00445 * Cos(2 * AP(4) - 2 * AP(3) - 3.3492)
        QA = QA + 0.00388 * Cos(MS - 2 * AP(3) + 0.35771)
        QA = QA + 0.00238 * Cos(MS - AP(3) + 0.61256)
        QA = QA + 0.00204 * Cos(2 * MS - 3 * AP(3) + 2.7688)
        QA = QA + 0.00177 * Cos(3 * AP(3) - AP(2) - 1.0053)
        QA = QA + 0.00136 * Cos(2 * MS - 4 * AP(3) + 2.6894)
        QA = QA + 0.00104 * Cos(AP(4) + 0.30749)

        QB = 0.000053227 * Cos(AP(4) - AP(3) + 0.717864)
        QB = QB + 0.000050989 * Cos(2 * AP(4) - 2 * AP(3) - 1.77997)
        QB = QB + 0.000038278 * Cos(2 * AP(4) - AP(3) - 1.71617)
        QB = QB + 0.000015996 * Cos(MS - AP(3) - 0.969618)
        QB = QB + 0.000014764 * Cos(2 * MS - 3 * AP(3) + 1.19768)
        QB = QB + 0.000008966 * Cos(AP(4) - 2 * AP(3) + 0.761225)
        QB = QB + 0.000007914 * Cos(3 * AP(4) - 2 * AP(3) - 2.43887)
        QB = QB + 0.000007004 * Cos(2 * AP(4) - 3 * AP(3) - 1.79573)
        QB = QB + 0.00000662 * Cos(MS - 2 * AP(3) + 1.97575)
        QB = QB + 0.00000493 * Cos(3 * AP(4) - 3 * AP(3) - 1.33069)
        QB = QB + 0.000004693 * Cos(3 * MS - 5 * AP(3) + 3.32665)
        QB = QB + 0.000004571 * Cos(2 * MS - 4 * AP(3) + 4.27086)
        QB = QB + 0.000004409 * Cos(3 * AP(4) - AP(3) - 2.02158)
        Return

4945    J1 = T / 5# + 0.1: J2 = Unwind(4.14473 + 52.9691 * T)
        J3 = Unwind(4.641118 + 21.32991 * T)
        J4 = Unwind(4.250177 + 7.478172 * T)
        J5 = 5# * J3 - 2# * J2: J6 = 2# * J2 - 6# * J3 + 3# * J4

        If (IP < 5) Then
            On IP GoTo 5190, 5190, 5190, 4980
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoTo 4980, 5505, 5505, 5190
        End If

4980    J7 = J3 - J2: U1 = Sin(J3): U2 = Cos(J3): U3 = Sin(2# * J3)
        U4 = Cos(2# * J3): U5 = Sin(J5): U6 = Cos(J5)
        U7 = Sin(2# * J5): U8 = Sin(J6): U9 = Sin(J7)
        UA = Cos(J7): UB = Sin(2# * J7): UC = Cos(2# * J7)
        UD = Sin(3# * J7): UE = Cos(3# * J7): UF = Sin(4# * J7)
        UG = Cos(4# * J7): VH = Cos(5# * J7)
        
        If (IP = 5) Then
            GoTo 5200
        End If

        QC = (0.331364 - (0.010281 + 0.004692 * J1) * J1) * U5
        QC = QC + (0.003228 - (0.064436 - 0.002075 * J1) * J1) * U6
        QC = QC - (0.003083 + (0.000275 - 0.000489 * J1) * J1) * U7
        QC = QC + 0.002472 * U8 + 0.013619 * U9 + 0.018472 * UB
        QC = QC + 0.006717 * UD + 0.002775 * UF + 0.006417 * UB * U1
        QC = QC + (0.007275 - 0.001253 * J1) * U9 * U1 + 0.002439 * UD * U1
        QC = QC - (0.035681 + 0.001208 * J1) * U9 * U2 - 0.003767 * UC * U1
        QC = QC - (0.033839 + 0.001125 * J1) * UA * U1 - 0.004261 * UB * U2
        QC = QC + (0.001161 * J1 - 0.006333) * UA * U2 + 0.002178 * U2
        QC = QC - 0.006675 * UC * U2 - 0.002664 * UE * U2 - 0.002572 * U9 * U3
        QC = QC - 0.003567 * UB * U3 + 0.002094 * UA * U4 + 0.003342 * UC * U4
        QC = Radians(QC)

        QD = (3606 + (130 - 43 * J1) * J1) * U5 + (1289 - 580 * J1) * U6
        QD = QD - 6764 * U9 * U1 - 1110 * UB * U1 - 224 * UD * U1 - 204 * U1
        QD = QD + (1284 + 116 * J1) * UA * U1 + 188 * UC * U1
        QD = QD + (1460 + 130 * J1) * U9 * U2 + 224 * UB * U2 - 817 * U2
        QD = QD + 6074 * U2 * UA + 992 * UC * U2 + 508 * UE * U2 + 230 * UG * U2
        QD = QD + 108 * VH * U2 - (956 + 73 * J1) * U9 * U3 + 448 * UB * U3
        QD = QD + 137 * UD * U3 + (108 * J1 - 997) * UA * U3 + 480 * UC * U3
        QD = QD + 148 * UE * U3 + (99 * J1 - 956) * U9 * U4 + 490 * UB * U4
        QD = QD + 158 * UD * U4 + 179 * U4 + (1024 + 75 * J1) * UA * U4
        QD = QD - 437 * UC * U4 - 132 * UE * U4: QD = QD * 0.0000001

        VK = (0.007192 - 0.003147 * J1) * U5 - 0.004344 * U1
        VK = VK + (J1 * (0.000197 * J1 - 0.000675) - 0.020428) * U6
        VK = VK + 0.034036 * UA * U1 + (0.007269 + 0.000672 * J1) * U9 * U1
        VK = VK + 0.005614 * UC * U1 + 0.002964 * UE * U1 + 0.037761 * U9 * U2
        VK = VK + 0.006158 * UB * U2 - 0.006603 * UA * U2 - 0.005356 * U9 * U3
        VK = VK + 0.002722 * UB * U3 + 0.004483 * UA * U3
        VK = VK - 0.002642 * UC * U3 + 0.004403 * U9 * U4
        VK = VK - 0.002536 * UB * U4 + 0.005547 * UA * U4 - 0.002689 * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 205 * UA - 263 * U6 + 693 * UC + 312 * UE + 147 * UG + 299 * U9 * U1
        QF = QF + 181 * UC * U1 + 204 * UB * U2 + 111 * UD * U2 - 337 * UA * U2
        QF = QF - 111 * UC * U2: QF = QF * 0.000001
5190    Return

5200    UI = Sin(3 * J3): UJ = Cos(3 * J3): UK = Sin(4 * J3)
        UL = Cos(4 * J3): VI = Cos(2 * J5): UN = Sin(5 * J7)
        J8 = J4 - J3: UO = Sin(2 * J8): UP = Cos(2 * J8)
        UQ = Sin(3 * J8): UR = Cos(3 * J8)

        QC = 0.007581 * U7 - 0.007986 * U8 - 0.148811 * U9
        QC = QC - (0.814181 - (0.01815 - 0.016714 * J1) * J1) * U5
        QC = QC - (0.010497 - (0.160906 - 0.0041 * J1) * J1) * U6
        QC = QC - 0.015208 * UD - 0.006339 * UF - 0.006244 * U1
        QC = QC - 0.0165 * UB * U1 - 0.040786 * UB
        QC = QC + (0.008931 + 0.002728 * J1) * U9 * U1 - 0.005775 * UD * U1
        QC = QC + (0.081344 + 0.003206 * J1) * UA * U1 + 0.015019 * UC * U1
        QC = QC + (0.085581 + 0.002494 * J1) * U9 * U2 + 0.014394 * UC * U2
        QC = QC + (0.025328 - 0.003117 * J1) * UA * U2 + 0.006319 * UE * U2
        QC = QC + 0.006369 * U9 * U3 + 0.009156 * UB * U3 + 0.007525 * UQ * U3
        QC = QC - 0.005236 * UA * U4 - 0.007736 * UC * U4 - 0.007528 * UR * U4
        QC = Radians(QC)

        QD = (-7927 + (2548 + 91 * J1) * J1) * U5
        QD = QD + (13381 + (1226 - 253 * J1) * J1) * U6 + (248 - 121 * J1) * U7
        QD = QD - (305 + 91 * J1) * VI + 412 * UB + 12415 * U1
        QD = QD + (390 - 617 * J1) * U9 * U1 + (165 - 204 * J1) * UB * U1
        QD = QD + 26599 * UA * U1 - 4687 * UC * U1 - 1870 * UE * U1 - 821 * UG * U1
        QD = QD - 377 * VH * U1 + 497 * UP * U1 + (163 - 611 * J1) * U2
        QD = QD - 12696 * U9 * U2 - 4200 * UB * U2 - 1503 * UD * U2 - 619 * UF * U2
        QD = QD - 268 * UN * U2 - (282 + 1306 * J1) * UA * U2
        QD = QD + (-86 + 230 * J1) * UC * U2 + 461 * UO * U2 - 350 * U3
        QD = QD + (2211 - 286 * J1) * U9 * U3 - 2208 * UB * U3 - 568 * UD * U3
        QD = QD - 346 * UF * U3 - (2780 + 222 * J1) * UA * U3
        QD = QD + (2022 + 263 * J1) * UC * U3 + 248 * UE * U3 + 242 * UQ * U3
        QD = QD + 467 * UR * U3 - 490 * U4 - (2842 + 279 * J1) * U9 * U4
        QD = QD + (128 + 226 * J1) * UB * U4 + 224 * UD * U4
        QD = QD + (-1594 + 282 * J1) * UA * U4 + (2162 - 207 * J1) * UC * U4
        QD = QD + 561 * UE * U4 + 343 * UG * U4 + 469 * UQ * U4 - 242 * UR * U4
        QD = QD - 205 * U9 * UI + 262 * UD * UI + 208 * UA * UJ - 271 * UE * UJ
        QD = QD - 382 * UE * UK - 376 * UD * UL: QD = QD * 0.0000001

        VK = (0.077108 + (0.007186 - 0.001533 * J1) * J1) * U5
        VK = VK - 0.007075 * U9
        VK = VK + (0.045803 - (0.014766 + 0.000536 * J1) * J1) * U6
        VK = VK - 0.072586 * U2 - 0.075825 * U9 * U1 - 0.024839 * UB * U1
        VK = VK - 0.008631 * UD * U1 - 0.150383 * UA * U2
        VK = VK + 0.026897 * UC * U2 + 0.010053 * UE * U2
        VK = VK - (0.013597 + 0.001719 * J1) * U9 * U3 + 0.011981 * UB * U4
        VK = VK - (0.007742 - 0.001517 * J1) * UA * U3
        VK = VK + (0.013586 - 0.001375 * J1) * UC * U3
        VK = VK - (0.013667 - 0.001239 * J1) * U9 * U4
        VK = VK + (0.014861 + 0.001136 * J1) * UA * U4
        VK = VK - (0.013064 + 0.001628 * J1) * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 572 * U5 - 1590 * UB * U2 + 2933 * U6 - 647 * UD * U2
        QF = QF + 33629 * UA - 344 * UF * U2 - 3081 * UC + 2885 * UA * U2
        QF = QF - 1423 * UE + (2172 + 102 * J1) * UC * U2 - 671 * UG
        QF = QF + 296 * UE * U2 - 320 * VH - 267 * UB * U3 + 1098 * U1
        QF = QF - 778 * UA * U3 - 2812 * U9 * U1 + 495 * UC * U3 + 688 * UB * U1
        QF = QF + 250 * UE * U3 - 393 * UD * U1 - 856 * U9 * U4 - 228 * UF * U1
        QF = QF + 441 * UB * U4 + 2138 * UA * U1 + 296 * UC * U4 - 999 * UC * U1
        QF = QF + 211 * UE * U4 - 642 * UE * U1 - 427 * U9 * UI - 325 * UG * U1
        QF = QF + 398 * UD * UI - 890 * U2 + 344 * UA * UJ + 2206 * U9 * U2
        QF = QF - 427 * UE * UJ: QF = QF * 0.000001

        QG = 0.000747 * UA * U1 + 0.001069 * UA * U2 + 0.002108 * UB * U3
        QG = QG + 0.001261 * UC * U3 + 0.001236 * UB * U4 - 0.002075 * UC * U4
        QG = Radians(QG): Return

5505    J8 = Unwind(1.46205 + 3.81337 * T): J9 = 2 * J8 - J4
        VJ = Sin(J9): UU = Cos(J9): UV = Sin(2 * J9)
        UW = Cos(2 * J9)
        
        If (IP = 7) Then
            GoTo 5675
        End If

        JA = J4 - J2: JB = J4 - J3: JC = J8 - J4
        QC = (0.864319 - 0.001583 * J1) * VJ
        QC = QC + (0.082222 - 0.006833 * J1) * UU + 0.036017 * UV
        QC = QC - 0.003019 * UW + 0.008122 * Sin(J6): QC = Radians(QC)

        VK = 0.120303 * VJ + 0.006197 * UV
        VK = VK + (0.019472 - 0.000947 * J1) * UU
        QE = QC - (Radians(VK) / PL(IP, 4))

        QD = (163 * J1 - 3349) * VJ + 20981 * UU + 1311 * UW: QD = QD * 0.0000001

        QF = -0.003825 * UU

        QA = (-0.038581 + (0.002031 - 0.00191 * J1) * J1) * Cos(J4 + JB)
        QA = QA + (0.010122 - 0.000988 * J1) * Sin(J4 + JB)
        A = (0.034964 - (0.001038 - 0.000868 * J1) * J1) * Cos(2 * J4 + JB)
        QA = A + QA + 0.005594 * Sin(J4 + 3 * JC) - 0.014808 * Sin(JA)
        QA = QA - 0.005794 * Sin(JB) + 0.002347 * Cos(JB)
        QA = QA + 0.009872 * Sin(JC) + 0.008803 * Sin(2 * JC)
        QA = QA - 0.004308 * Sin(3 * JC)

        UX = Sin(JB): UY = Cos(JB): UZ = Sin(J4)
        VA = Cos(J4): VB = Sin(2 * J4): VC = Cos(2 * J4)
        QG = (0.000458 * UX - 0.000642 * UY - 0.000517 * Cos(4 * JC)) * UZ
        QG = QG - (0.000347 * UX + 0.000853 * UY + 0.000517 * Sin(4 * JB)) * VA
        QG = QG + 0.000403 * (Cos(2 * JC) * VB + Sin(2 * JC) * VC)
        QG = Radians(QG)

        QB = -25948 + 4985 * Cos(JA) - 1230 * VA + 3354 * UY
        QB = QB + 904 * Cos(2 * JC) + 894 * (Cos(JC) - Cos(3 * JC))
        QB = QB + (5795 * VA - 1165 * UZ + 1388 * VC) * UX
        QB = QB + (1351 * VA + 5702 * UZ + 1388 * VB) * UY
        QB = QB * 0.000001: Return

5675    JA = J8 - J2: JB = J8 - J3: JC = J8 - J4
        QC = (0.001089 * J1 - 0.589833) * VJ
        QC = QC + (0.004658 * J1 - 0.056094) * UU - 0.024286 * UV
        QC = Radians(QC)

        VK = 0.024039 * VJ - 0.025303 * UU + 0.006206 * UV
        VK = VK - 0.005992 * UW: QE = QC - (Radians(VK) / PL(IP, 4))

        QD = 4389 * VJ + 1129 * UV + 4262 * UU + 1089 * UW
        QD = QD * 0.0000001

        QF = 8189 * UU - 817 * VJ + 781 * UW: QF = QF * 0.000001

        VD = Sin(2 * JC): VE = Cos(2 * JC)
        VF = Sin(J8): VG = Cos(J8)
        QA = -0.009556 * Sin(JA) - 0.005178 * Sin(JB)
        QA = QA + 0.002572 * VD - 0.002972 * VE * VF - 0.002833 * VD * VG

        QG = 0.000336 * VE * VF + 0.000364 * VD * VG: QG = Radians(QG)

        QB = -40596 + 4992 * Cos(JA) + 2744 * Cos(JB)
        QB = QB + 2044 * Cos(JC) + 1051 * VE: QB = QB * 0.000001

        Return
        
5700    PlanetHLong2 = Degrees(PD)

End Function

Function PlanetRVect(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, S As String) As Double

Const a11 = 178.179078, a12 = 415.2057519, a13 = 0.0003011, a14 = 0
Const a21 = 75.899697, a22 = 1.5554889, a23 = 0.0002947, a24 = 0
Const a31 = 0.20561421, a32 = 0.00002046, a33 = -0.00000003, a34 = 0
Const a41 = 7.002881, a42 = 0.0018608, a43 = -0.0000183, a44 = 0
Const a51 = 47.145944, a52 = 1.1852083, a53 = 0.0001739, a54 = 0
Const a61 = 0.3870986, a62 = 6.74, a63 = -0.42

Const b11 = 342.767053, b12 = 162.5533664, b13 = 0.0003097, b14 = 0
Const b21 = 130.163833, b22 = 1.4080361, b23 = -0.0009764, b24 = 0
Const b31 = 0.00682069, b32 = -0.00004774, b33 = 0.000000091, b34 = 0
Const b41 = 3.393631, b42 = 0.0010058, b43 = -0.000001, b44 = 0
Const b51 = 75.779647, b52 = 0.89985, b53 = 0.00041, b54 = 0
Const b61 = 0.7233316, b62 = 16.92, b63 = -4.4

Const c11 = 293.737334, c12 = 53.17137642, c13 = 0.0003107, c14 = 0
Const c21 = 334.218203, c22 = 1.8407584, c23 = 0.0001299, c24 = -0.00000119
Const c31 = 0.0933129, c32 = 0.000092064, c33 = -0.000000077, c34 = 0
Const c41 = 1.850333, c42 = -0.000675, c43 = 0.0000126, c44 = 0
Const c51 = 48.786442, c52 = 0.7709917, c53 = -0.0000014, c54 = -0.00000533
Const c61 = 1.5236883, c62 = 9.36, c63 = -1.52

Const d11 = 238.049257, d12 = 8.434172183, d13 = 0.0003347, d14 = -0.00000165
Const d21 = 12.720972, d22 = 1.6099617, d23 = 0.00105627, d24 = -0.00000343
Const d31 = 0.04833475, d32 = 0.00016418, d33 = -0.0000004676, d34 = -0.0000000017
Const d41 = 1.308736, d42 = -0.0056961, d43 = 0.0000039, d44 = 0
Const d51 = 99.443414, d52 = 1.01053, d53 = 0.00035222, d54 = -0.00000851
Const d61 = 5.202561, d62 = 196.74, d63 = -9.4

Const e11 = 266.564377, e12 = 3.398638567, e13 = 0.0003245, e14 = -0.0000058
Const e21 = 91.098214, e22 = 1.9584158, e23 = 0.00082636, e24 = 0.00000461
Const e31 = 0.05589232, e32 = -0.0003455, e33 = -0.000000728, e34 = 0.00000000074
Const e41 = 2.492519, e42 = -0.0039189, e43 = -0.00001549, e44 = 0.00000004
Const e51 = 112.790414, e52 = 0.8731951, e53 = -0.00015218, e54 = -0.00000531
Const e61 = 9.554747, e62 = 165.6, e63 = -8.88

Const f11 = 244.19747, f12 = 1.194065406, f13 = 0.000316, f14 = -0.0000006
Const f21 = 171.548692, f22 = 1.4844328, f23 = 0.0002372, f24 = -0.00000061
Const f31 = 0.0463444, f32 = -0.00002658, f33 = 0.000000077, f34 = 0
Const f41 = 0.772464, f42 = 0.0006253, f43 = 0.0000395, f44 = 0
Const f51 = 73.477111, f52 = 0.4986678, f53 = 0.0013117, f54 = 0
Const f61 = 19.21814, f62 = 65.8, f63 = -7.19

Const g11 = 84.457994, g12 = 0.6107942056, g13 = 0.0003205, g14 = -0.0000006
Const g21 = 46.727364, g22 = 1.4245744, g23 = 0.00039082, g24 = -0.000000605
Const g31 = 0.00899704, g32 = 0.00000633, g33 = -0.000000002, g34 = 0
Const g41 = 1.779242, g42 = -0.0095436, g43 = -0.0000091, g44 = 0
Const g51 = 130.681389, g52 = 1.098935, g53 = 0.00024987, g54 = -0.000004718
Const g61 = 30.10957, g62 = 62.2, g63 = -6.87

Dim IP As Integer, I As Integer, J As Integer, K As Integer
Dim PL(7, 9) As Double, AP(7) As Double

        IP = 0: B = LctUT(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LCH, LCM, LCS, DS, ZC, DY, MN, YR)
        A = CDJD(GD, GM, GY)
        T = ((A - 2415020#) / 36525#) + (B / 876600#)

        Select Case UCase(S)
            Case "MERCURY"
                IP = 1
           Case "VENUS"
                IP = 2
            Case "MARS"
                IP = 3
           Case "JUPITER"
                IP = 4
            Case "SATURN"
                IP = 5
            Case "URANUS"
                IP = 6
            Case "NEPTUNE"
                IP = 7
            Case Else
                EP = 0
                GoTo 5700
        End Select
    

        I = 1
        A0 = a11: A1 = a12: A2 = a13: A3 = a14
        B0 = a21: B1 = a22: B2 = a23: B3 = a24
        C0 = a31: C1 = a32: C2 = a33: C3 = a34
        D0 = a41: D1 = a42: D2 = a43: D3 = a44
        E0 = a51: E1 = a52: E2 = a53: E3 = a54
        F = a61: G = a62: H = a63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 2
        A0 = b11: A1 = b12: A2 = b13: A3 = b14
        B0 = b21: B1 = b22: B2 = b23: B3 = b24
        C0 = b31: C1 = b32: C2 = b33: C3 = b34
        D0 = b41: D1 = b42: D2 = b43: D3 = b44
        E0 = b51: E1 = b52: E2 = b53: E3 = b54
        F = b61: G = b62: H = b63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 3
        A0 = c11: A1 = c12: A2 = c13: A3 = c14
        B0 = c21: B1 = c22: B2 = c23: B3 = c24
        C0 = c31: C1 = c32: C2 = c33: C3 = c34
        D0 = c41: D1 = c42: D2 = c43: D3 = c44
        E0 = c51: E1 = c52: E2 = c53: E3 = c54
        F = c61: G = c62: H = c63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

       I = 4
        A0 = d11: A1 = d12: A2 = d13: A3 = d14
        B0 = d21: B1 = d22: B2 = d23: B3 = d24
        C0 = d31: C1 = d32: C2 = d33: C3 = d34
        D0 = d41: D1 = d42: D2 = d43: D3 = d44
        E0 = d51: E1 = d52: E2 = d53: E3 = d54
        F = d61: G = d62: H = d63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 5
        A0 = e11: A1 = e12: A2 = e13: A3 = e14
        B0 = e21: B1 = e22: B2 = e23: B3 = e24
        C0 = e31: C1 = e32: C2 = e33: C3 = e34
        D0 = e41: D1 = e42: D2 = e43: D3 = e44
        E0 = e51: E1 = e52: E2 = e53: E3 = e54
        F = e61: G = e62: H = e63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 6
        A0 = f11: A1 = f12: A2 = f13: A3 = f14
        B0 = f21: B1 = f22: B2 = f23: B3 = f24
        C0 = f31: C1 = f32: C2 = f33: C3 = f34
        D0 = f41: D1 = f42: D2 = f43: D3 = f44
        E0 = f51: E1 = f52: E2 = f53: E3 = f54
        F = f61: G = f62: H = f63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        I = 7
        A0 = g11: A1 = g12: A2 = g13: A3 = g14
        B0 = g21: B1 = g22: B2 = g23: B3 = g24
        C0 = g31: C1 = g32: C2 = g33: C3 = g34
        D0 = g41: D1 = g42: D2 = g43: D3 = g44
        E0 = g51: E1 = g52: E2 = g53: E3 = g54
        F = g61: G = g62: H = g63
        
        AA = A1 * T
        B = 360# * (AA - Int(AA))
        C = A0 + B + (A3 * T + A2) * T * T:
        PL(I, 1) = C - 360# * Int(C / 360#)
        PL(I, 2) = (A1 * 0.009856263) + (A2 + A3) / 36525#
        PL(I, 3) = ((B3 * T + B2) * T + B1) * T + B0
        PL(I, 4) = ((C3 * T + C2) * T + C1) * T + C0
        PL(I, 5) = ((D3 * T + D2) * T + D1) * T + D0
        PL(I, 6) = ((E3 * T + E2) * T + E1) * T + E0
        PL(I, 7) = F
        PL(I, 8) = G
        PL(I, 9) = H

        LI = 0: Pi = 3.1415926536: TP = 2# * Pi
        MS = SunMeanAnomaly(LH, LM, LS, DS, ZC, DY, MN, YR)
        SR = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR))
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR): LG = SR + Pi

        K = 1

        For J = 1 To 7
        AP(J) = Radians(PL(J, 1) - PL(J, 3) - LI * PL(J, 2))
        Next J

        QA = 0: QB = 0: QC = 0: QD = 0: QE = 0: QF = 0: QG = 0
        If (IP < 5) Then
            On IP GoSub 4685, 4735, 4810, 4945
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoSub 4945, 4945, 4945
        End If

        EC = PL(IP, 4) + QD: AM = AP(IP) + QE: AT = TrueAnomaly(AM, EC)
        PVV = (PL(IP, 7) + QF) * (1# - EC * EC) / (1# + EC * Cos(AT))
        LP = Degrees(AT) + PL(IP, 3) + Degrees(QC - QE): LP = Radians(LP)
        OM = Radians(PL(IP, 6)): LO = LP - OM
        SO = Sin(LO): CO = Cos(LO)
        INN = Radians(PL(IP, 5)): PVV = PVV + QB
        SP = SO * Sin(INN): Y = SO * Cos(INN)
        PS = Asin(SP) + QG: SP = Sin(PS)
        PD = Atan2(CO, Y) + OM + Radians(QA)
        PD = Unwind(PD)
        CI = Cos(PS): RD = PVV * CI: LL = PD - LG
        RH = RE * RE + PVV * PVV - 2# * RE * PVV * CI * Cos(LL)
        RH = Sqr(RH): LI = RH * 0.005775518

        GoTo 5700

4685    QA = 0.00204 * Cos(5 * AP(2) - 2 * AP(1) + 0.21328)
        QA = QA + 0.00103 * Cos(2 * AP(2) - AP(1) - 2.8046)
        QA = QA + 0.00091 * Cos(2 * AP(4) - AP(1) - 0.64582)
        QA = QA + 0.00078 * Cos(5 * AP(2) - 3 * AP(1) + 0.17692)

        QB = 0.000007525 * Cos(2 * AP(4) - AP(1) + 0.925251)
        QB = QB + 0.000006802 * Cos(5 * AP(2) - 3 * AP(1) - 4.53642)
        QB = QB + 0.000005457 * Cos(2 * AP(2) - 2 * AP(1) - 1.24246)
        QB = QB + 0.000003569 * Cos(5 * AP(2) - AP(1) - 1.35699)
        Return

4735    QC = 0.00077 * Sin(4.1406 + T * 2.6227): QC = Radians(QC): QE = QC

        QA = 0.00313 * Cos(2 * MS - 2 * AP(2) - 2.587)
        QA = QA + 0.00198 * Cos(3 * MS - 3 * AP(2) + 0.044768)
        QA = QA + 0.00136 * Cos(MS - AP(2) - 2.0788)
        QA = QA + 0.00096 * Cos(3 * MS - 2 * AP(2) - 2.3721)
        QA = QA + 0.00082 * Cos(AP(4) - AP(2) - 3.6318)

        QB = 0.000022501 * Cos(2 * MS - 2 * AP(2) - 1.01592)
        QB = QB + 0.000019045 * Cos(3 * MS - 3 * AP(2) + 1.61577)
        QB = QB + 0.000006887 * Cos(AP(4) - AP(2) - 2.06106)
        QB = QB + 0.000005172 * Cos(MS - AP(2) - 0.508065)
        QB = QB + 0.00000362 * Cos(5 * MS - 4 * AP(2) - 1.81877)
        QB = QB + 0.000003283 * Cos(4 * MS - 4 * AP(2) + 1.10851)
        QB = QB + 0.000003074 * Cos(2 * AP(4) - 2 * AP(2) - 0.962846)
        Return

4810    A = 3 * AP(4) - 8 * AP(3) + 4 * MS: SA = Sin(A): CA = Cos(A)
        QC = -(0.01133 * SA + 0.00933 * CA): QC = Radians(QC): QE = QC

        QA = 0.00705 * Cos(AP(4) - AP(3) - 0.85448)
        QA = QA + 0.00607 * Cos(2 * AP(4) - AP(3) - 3.2873)
        QA = QA + 0.00445 * Cos(2 * AP(4) - 2 * AP(3) - 3.3492)
        QA = QA + 0.00388 * Cos(MS - 2 * AP(3) + 0.35771)
        QA = QA + 0.00238 * Cos(MS - AP(3) + 0.61256)
        QA = QA + 0.00204 * Cos(2 * MS - 3 * AP(3) + 2.7688)
        QA = QA + 0.00177 * Cos(3 * AP(3) - AP(2) - 1.0053)
        QA = QA + 0.00136 * Cos(2 * MS - 4 * AP(3) + 2.6894)
        QA = QA + 0.00104 * Cos(AP(4) + 0.30749)

        QB = 0.000053227 * Cos(AP(4) - AP(3) + 0.717864)
        QB = QB + 0.000050989 * Cos(2 * AP(4) - 2 * AP(3) - 1.77997)
        QB = QB + 0.000038278 * Cos(2 * AP(4) - AP(3) - 1.71617)
        QB = QB + 0.000015996 * Cos(MS - AP(3) - 0.969618)
        QB = QB + 0.000014764 * Cos(2 * MS - 3 * AP(3) + 1.19768)
        QB = QB + 0.000008966 * Cos(AP(4) - 2 * AP(3) + 0.761225)
        QB = QB + 0.000007914 * Cos(3 * AP(4) - 2 * AP(3) - 2.43887)
        QB = QB + 0.000007004 * Cos(2 * AP(4) - 3 * AP(3) - 1.79573)
        QB = QB + 0.00000662 * Cos(MS - 2 * AP(3) + 1.97575)
        QB = QB + 0.00000493 * Cos(3 * AP(4) - 3 * AP(3) - 1.33069)
        QB = QB + 0.000004693 * Cos(3 * MS - 5 * AP(3) + 3.32665)
        QB = QB + 0.000004571 * Cos(2 * MS - 4 * AP(3) + 4.27086)
        QB = QB + 0.000004409 * Cos(3 * AP(4) - AP(3) - 2.02158)
        Return

4945    J1 = T / 5# + 0.1: J2 = Unwind(4.14473 + 52.9691 * T)
        J3 = Unwind(4.641118 + 21.32991 * T)
        J4 = Unwind(4.250177 + 7.478172 * T)
        J5 = 5# * J3 - 2# * J2: J6 = 2# * J2 - 6# * J3 + 3# * J4

        If (IP < 5) Then
            On IP GoTo 5190, 5190, 5190, 4980
        End If
        
        If (IP > 4) Then
            On (IP - 4) GoTo 4980, 5505, 5505, 5190
        End If

4980    J7 = J3 - J2: U1 = Sin(J3): U2 = Cos(J3): U3 = Sin(2# * J3)
        U4 = Cos(2# * J3): U5 = Sin(J5): U6 = Cos(J5)
        U7 = Sin(2# * J5): U8 = Sin(J6): U9 = Sin(J7)
        UA = Cos(J7): UB = Sin(2# * J7): UC = Cos(2# * J7)
        UD = Sin(3# * J7): UE = Cos(3# * J7): UF = Sin(4# * J7)
        UG = Cos(4# * J7): VH = Cos(5# * J7)
        
        If (IP = 5) Then
            GoTo 5200
        End If

        QC = (0.331364 - (0.010281 + 0.004692 * J1) * J1) * U5
        QC = QC + (0.003228 - (0.064436 - 0.002075 * J1) * J1) * U6
        QC = QC - (0.003083 + (0.000275 - 0.000489 * J1) * J1) * U7
        QC = QC + 0.002472 * U8 + 0.013619 * U9 + 0.018472 * UB
        QC = QC + 0.006717 * UD + 0.002775 * UF + 0.006417 * UB * U1
        QC = QC + (0.007275 - 0.001253 * J1) * U9 * U1 + 0.002439 * UD * U1
        QC = QC - (0.035681 + 0.001208 * J1) * U9 * U2 - 0.003767 * UC * U1
        QC = QC - (0.033839 + 0.001125 * J1) * UA * U1 - 0.004261 * UB * U2
        QC = QC + (0.001161 * J1 - 0.006333) * UA * U2 + 0.002178 * U2
        QC = QC - 0.006675 * UC * U2 - 0.002664 * UE * U2 - 0.002572 * U9 * U3
        QC = QC - 0.003567 * UB * U3 + 0.002094 * UA * U4 + 0.003342 * UC * U4
        QC = Radians(QC)

        QD = (3606 + (130 - 43 * J1) * J1) * U5 + (1289 - 580 * J1) * U6
        QD = QD - 6764 * U9 * U1 - 1110 * UB * U1 - 224 * UD * U1 - 204 * U1
        QD = QD + (1284 + 116 * J1) * UA * U1 + 188 * UC * U1
        QD = QD + (1460 + 130 * J1) * U9 * U2 + 224 * UB * U2 - 817 * U2
        QD = QD + 6074 * U2 * UA + 992 * UC * U2 + 508 * UE * U2 + 230 * UG * U2
        QD = QD + 108 * VH * U2 - (956 + 73 * J1) * U9 * U3 + 448 * UB * U3
        QD = QD + 137 * UD * U3 + (108 * J1 - 997) * UA * U3 + 480 * UC * U3
        QD = QD + 148 * UE * U3 + (99 * J1 - 956) * U9 * U4 + 490 * UB * U4
        QD = QD + 158 * UD * U4 + 179 * U4 + (1024 + 75 * J1) * UA * U4
        QD = QD - 437 * UC * U4 - 132 * UE * U4: QD = QD * 0.0000001

        VK = (0.007192 - 0.003147 * J1) * U5 - 0.004344 * U1
        VK = VK + (J1 * (0.000197 * J1 - 0.000675) - 0.020428) * U6
        VK = VK + 0.034036 * UA * U1 + (0.007269 + 0.000672 * J1) * U9 * U1
        VK = VK + 0.005614 * UC * U1 + 0.002964 * UE * U1 + 0.037761 * U9 * U2
        VK = VK + 0.006158 * UB * U2 - 0.006603 * UA * U2 - 0.005356 * U9 * U3
        VK = VK + 0.002722 * UB * U3 + 0.004483 * UA * U3
        VK = VK - 0.002642 * UC * U3 + 0.004403 * U9 * U4
        VK = VK - 0.002536 * UB * U4 + 0.005547 * UA * U4 - 0.002689 * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 205 * UA - 263 * U6 + 693 * UC + 312 * UE + 147 * UG + 299 * U9 * U1
        QF = QF + 181 * UC * U1 + 204 * UB * U2 + 111 * UD * U2 - 337 * UA * U2
        QF = QF - 111 * UC * U2: QF = QF * 0.000001
5190    Return

5200    UI = Sin(3 * J3): UJ = Cos(3 * J3): UK = Sin(4 * J3)
        UL = Cos(4 * J3): VI = Cos(2 * J5): UN = Sin(5 * J7)
        J8 = J4 - J3: UO = Sin(2 * J8): UP = Cos(2 * J8)
        UQ = Sin(3 * J8): UR = Cos(3 * J8)

        QC = 0.007581 * U7 - 0.007986 * U8 - 0.148811 * U9
        QC = QC - (0.814181 - (0.01815 - 0.016714 * J1) * J1) * U5
        QC = QC - (0.010497 - (0.160906 - 0.0041 * J1) * J1) * U6
        QC = QC - 0.015208 * UD - 0.006339 * UF - 0.006244 * U1
        QC = QC - 0.0165 * UB * U1 - 0.040786 * UB
        QC = QC + (0.008931 + 0.002728 * J1) * U9 * U1 - 0.005775 * UD * U1
        QC = QC + (0.081344 + 0.003206 * J1) * UA * U1 + 0.015019 * UC * U1
        QC = QC + (0.085581 + 0.002494 * J1) * U9 * U2 + 0.014394 * UC * U2
        QC = QC + (0.025328 - 0.003117 * J1) * UA * U2 + 0.006319 * UE * U2
        QC = QC + 0.006369 * U9 * U3 + 0.009156 * UB * U3 + 0.007525 * UQ * U3
        QC = QC - 0.005236 * UA * U4 - 0.007736 * UC * U4 - 0.007528 * UR * U4
        QC = Radians(QC)

        QD = (-7927 + (2548 + 91 * J1) * J1) * U5
        QD = QD + (13381 + (1226 - 253 * J1) * J1) * U6 + (248 - 121 * J1) * U7
        QD = QD - (305 + 91 * J1) * VI + 412 * UB + 12415 * U1
        QD = QD + (390 - 617 * J1) * U9 * U1 + (165 - 204 * J1) * UB * U1
        QD = QD + 26599 * UA * U1 - 4687 * UC * U1 - 1870 * UE * U1 - 821 * UG * U1
        QD = QD - 377 * VH * U1 + 497 * UP * U1 + (163 - 611 * J1) * U2
        QD = QD - 12696 * U9 * U2 - 4200 * UB * U2 - 1503 * UD * U2 - 619 * UF * U2
        QD = QD - 268 * UN * U2 - (282 + 1306 * J1) * UA * U2
        QD = QD + (-86 + 230 * J1) * UC * U2 + 461 * UO * U2 - 350 * U3
        QD = QD + (2211 - 286 * J1) * U9 * U3 - 2208 * UB * U3 - 568 * UD * U3
        QD = QD - 346 * UF * U3 - (2780 + 222 * J1) * UA * U3
        QD = QD + (2022 + 263 * J1) * UC * U3 + 248 * UE * U3 + 242 * UQ * U3
        QD = QD + 467 * UR * U3 - 490 * U4 - (2842 + 279 * J1) * U9 * U4
        QD = QD + (128 + 226 * J1) * UB * U4 + 224 * UD * U4
        QD = QD + (-1594 + 282 * J1) * UA * U4 + (2162 - 207 * J1) * UC * U4
        QD = QD + 561 * UE * U4 + 343 * UG * U4 + 469 * UQ * U4 - 242 * UR * U4
        QD = QD - 205 * U9 * UI + 262 * UD * UI + 208 * UA * UJ - 271 * UE * UJ
        QD = QD - 382 * UE * UK - 376 * UD * UL: QD = QD * 0.0000001

        VK = (0.077108 + (0.007186 - 0.001533 * J1) * J1) * U5
        VK = VK - 0.007075 * U9
        VK = VK + (0.045803 - (0.014766 + 0.000536 * J1) * J1) * U6
        VK = VK - 0.072586 * U2 - 0.075825 * U9 * U1 - 0.024839 * UB * U1
        VK = VK - 0.008631 * UD * U1 - 0.150383 * UA * U2
        VK = VK + 0.026897 * UC * U2 + 0.010053 * UE * U2
        VK = VK - (0.013597 + 0.001719 * J1) * U9 * U3 + 0.011981 * UB * U4
        VK = VK - (0.007742 - 0.001517 * J1) * UA * U3
        VK = VK + (0.013586 - 0.001375 * J1) * UC * U3
        VK = VK - (0.013667 - 0.001239 * J1) * U9 * U4
        VK = VK + (0.014861 + 0.001136 * J1) * UA * U4
        VK = VK - (0.013064 + 0.001628 * J1) * UC * U4
        QE = QC - (Radians(VK) / PL(IP, 4))

        QF = 572 * U5 - 1590 * UB * U2 + 2933 * U6 - 647 * UD * U2
        QF = QF + 33629 * UA - 344 * UF * U2 - 3081 * UC + 2885 * UA * U2
        QF = QF - 1423 * UE + (2172 + 102 * J1) * UC * U2 - 671 * UG
        QF = QF + 296 * UE * U2 - 320 * VH - 267 * UB * U3 + 1098 * U1
        QF = QF - 778 * UA * U3 - 2812 * U9 * U1 + 495 * UC * U3 + 688 * UB * U1
        QF = QF + 250 * UE * U3 - 393 * UD * U1 - 856 * U9 * U4 - 228 * UF * U1
        QF = QF + 441 * UB * U4 + 2138 * UA * U1 + 296 * UC * U4 - 999 * UC * U1
        QF = QF + 211 * UE * U4 - 642 * UE * U1 - 427 * U9 * UI - 325 * UG * U1
        QF = QF + 398 * UD * UI - 890 * U2 + 344 * UA * UJ + 2206 * U9 * U2
        QF = QF - 427 * UE * UJ: QF = QF * 0.000001

        QG = 0.000747 * UA * U1 + 0.001069 * UA * U2 + 0.002108 * UB * U3
        QG = QG + 0.001261 * UC * U3 + 0.001236 * UB * U4 - 0.002075 * UC * U4
        QG = Radians(QG): Return

5505    J8 = Unwind(1.46205 + 3.81337 * T): J9 = 2 * J8 - J4
        VJ = Sin(J9): UU = Cos(J9): UV = Sin(2 * J9)
        UW = Cos(2 * J9)
        
        If (IP = 7) Then
            GoTo 5675
        End If

        JA = J4 - J2: JB = J4 - J3: JC = J8 - J4
        QC = (0.864319 - 0.001583 * J1) * VJ
        QC = QC + (0.082222 - 0.006833 * J1) * UU + 0.036017 * UV
        QC = QC - 0.003019 * UW + 0.008122 * Sin(J6): QC = Radians(QC)

        VK = 0.120303 * VJ + 0.006197 * UV
        VK = VK + (0.019472 - 0.000947 * J1) * UU
        QE = QC - (Radians(VK) / PL(IP, 4))

        QD = (163 * J1 - 3349) * VJ + 20981 * UU + 1311 * UW: QD = QD * 0.0000001

        QF = -0.003825 * UU

        QA = (-0.038581 + (0.002031 - 0.00191 * J1) * J1) * Cos(J4 + JB)
        QA = QA + (0.010122 - 0.000988 * J1) * Sin(J4 + JB)
        A = (0.034964 - (0.001038 - 0.000868 * J1) * J1) * Cos(2 * J4 + JB)
        QA = A + QA + 0.005594 * Sin(J4 + 3 * JC) - 0.014808 * Sin(JA)
        QA = QA - 0.005794 * Sin(JB) + 0.002347 * Cos(JB)
        QA = QA + 0.009872 * Sin(JC) + 0.008803 * Sin(2 * JC)
        QA = QA - 0.004308 * Sin(3 * JC)

        UX = Sin(JB): UY = Cos(JB): UZ = Sin(J4)
        VA = Cos(J4): VB = Sin(2 * J4): VC = Cos(2 * J4)
        QG = (0.000458 * UX - 0.000642 * UY - 0.000517 * Cos(4 * JC)) * UZ
        QG = QG - (0.000347 * UX + 0.000853 * UY + 0.000517 * Sin(4 * JB)) * VA
        QG = QG + 0.000403 * (Cos(2 * JC) * VB + Sin(2 * JC) * VC)
        QG = Radians(QG)

        QB = -25948 + 4985 * Cos(JA) - 1230 * VA + 3354 * UY
        QB = QB + 904 * Cos(2 * JC) + 894 * (Cos(JC) - Cos(3 * JC))
        QB = QB + (5795 * VA - 1165 * UZ + 1388 * VC) * UX
        QB = QB + (1351 * VA + 5702 * UZ + 1388 * VB) * UY
        QB = QB * 0.000001: Return

5675    JA = J8 - J2: JB = J8 - J3: JC = J8 - J4
        QC = (0.001089 * J1 - 0.589833) * VJ
        QC = QC + (0.004658 * J1 - 0.056094) * UU - 0.024286 * UV
        QC = Radians(QC)

        VK = 0.024039 * VJ - 0.025303 * UU + 0.006206 * UV
        VK = VK - 0.005992 * UW: QE = QC - (Radians(VK) / PL(IP, 4))

        QD = 4389 * VJ + 1129 * UV + 4262 * UU + 1089 * UW
        QD = QD * 0.0000001

        QF = 8189 * UU - 817 * VJ + 781 * UW: QF = QF * 0.000001

        VD = Sin(2 * JC): VE = Cos(2 * JC)
        VF = Sin(J8): VG = Cos(J8)
        QA = -0.009556 * Sin(JA) - 0.005178 * Sin(JB)
        QA = QA + 0.002572 * VD - 0.002972 * VE * VF - 0.002833 * VD * VG

        QG = 0.000336 * VE * VF + 0.000364 * VD * VG: QG = Radians(QG)

        QB = -40596 + 4992 * Cos(JA) + 2744 * Cos(JB)
        QB = QB + 2044 * Cos(JC) + 1051 * VE: QB = QB * 0.000001

        Return
        
5700    PlanetRVect = PVV

End Function

Function SolveCubic(W As Double) As Double

        S = W / 3#
7930    S2 = S * S: D = (S2 + 3#) * S - W

        If (Abs(D) < 0.000001) Then
            GoTo 7945
        End If
        
        S = ((2# * S * S2) + W) / (3# * (S2 + 1#)): GoTo 7930
7945    SolveCubic = S

End Function

Function PcometLong(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, TD As Double, TM As Double, TY As Double, Q As Double, I As Double, P As Double, N As Double) As Double
Dim K As Integer

        GD = LctGDay(LH, LM, LS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LH, LM, LS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LH, LM, LS, DS, ZC, DY, MN, YR)
        UT = LctUT(LH, LM, LS, DS, ZC, DY, MN, YR)
        TPE = (UT / 365.242191) + CDJD(GD, GM, GY) - CDJD(TD, TM, TY)
        LG = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR) + 180#)
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR)

        LI = 0
        For K = 1 To 2
        
        S = SolveCubic(0.0364911624 * TPE / (Q * Sqr(Q)))
        NU = 2# * Atn(S): R = Q * (1# + S * S): L = NU + Radians(P)
        S1 = Sin(L): C1 = Cos(L): I1 = Radians(I)
        S2 = S1 * Sin(I1): PS = Asin(S2): Y = S1 * Cos(I1)
        LC = Atan2(C1, Y) + Radians(N): C2 = Cos(PS)
        RD = R * C2: LL = LC - LG: C3 = Cos(LL): S3 = Sin(LL)
        RH = Sqr((RE * RE) + (R * R) - (2 * RE * RD * C3 * Cos(PS)))
        
        LI = RH * 0.005775518
        Next K

        If (RD < RE) Then
            EP = Atn((-RD * S3) / (RE - (RD * C3))) + LG + 3.141592654
        Else
            EP = Atn((RE * S3) / (RD - (RE * C3))) + LC
        End If

        EP = Unwind(EP)
        TB = (RD * S2 * Sin(EP - LC)) / (C2 * RE * S3)
        BP = Atn(TB)
        PcometLong = Degrees(EP)
        
End Function

Function PcometLat(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, TD As Double, TM As Double, TY As Double, Q As Double, I As Double, P As Double, N As Double) As Double
Dim K As Integer

        GD = LctGDay(LH, LM, LS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LH, LM, LS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LH, LM, LS, DS, ZC, DY, MN, YR)
        UT = LctUT(LH, LM, LS, DS, ZC, DY, MN, YR)
        TPE = (UT / 365.242191) + CDJD(GD, GM, GY) - CDJD(TD, TM, TY)
        LG = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR) + 180#)
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR)

        LI = 0
        For K = 1 To 2
        
        S = SolveCubic(0.0364911624 * TPE / (Q * Sqr(Q)))
        NU = 2# * Atn(S): R = Q * (1# + S * S): L = NU + Radians(P)
        S1 = Sin(L): C1 = Cos(L): I1 = Radians(I)
        S2 = S1 * Sin(I1): PS = Asin(S2): Y = S1 * Cos(I1)
        LC = Atan2(C1, Y) + Radians(N): C2 = Cos(PS)
        RD = R * C2: LL = LC - LG: C3 = Cos(LL): S3 = Sin(LL)
        RH = Sqr((RE * RE) + (R * R) - (2 * RE * RD * C3 * Cos(PS)))
        
        LI = RH * 0.005775518
        Next K

        If (RD < RE) Then
            EP = Atn((-RD * S3) / (RE - (RD * C3))) + LG + 3.141592654
        Else
            EP = Atn((RE * S3) / (RD - (RE * C3))) + LC
        End If

        EP = Unwind(EP)
        TB = (RD * S2 * Sin(EP - LC)) / (C2 * RE * S3)
        BP = Atn(TB)
        PcometLat = Degrees(BP)
        
End Function

Function PcometDist(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double, TD As Double, TM As Double, TY As Double, Q As Double, I As Double, P As Double, N As Double) As Double
Dim K As Integer

        GD = LctGDay(LH, LM, LS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LH, LM, LS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LH, LM, LS, DS, ZC, DY, MN, YR)
        UT = LctUT(LH, LM, LS, DS, ZC, DY, MN, YR)
        TPE = (UT / 365.242191) + CDJD(GD, GM, GY) - CDJD(TD, TM, TY)
        LG = Radians(SunLong(LH, LM, LS, DS, ZC, DY, MN, YR) + 180#)
        RE = SunDist(LH, LM, LS, DS, ZC, DY, MN, YR)

        LI = 0
        K = 1
        
        S = SolveCubic(0.0364911624 * TPE / (Q * Sqr(Q)))
        NU = 2# * Atn(S): R = Q * (1# + S * S): L = NU + Radians(P)
        S1 = Sin(L): C1 = Cos(L): I1 = Radians(I)
        S2 = S1 * Sin(I1): PS = Asin(S2): Y = S1 * Cos(I1)
        LC = Atan2(C1, Y) + Radians(N): C2 = Cos(PS)
        RD = R * C2: LL = LC - LG: C3 = Cos(LL): S3 = Sin(LL)
        RH = Sqr((RE * RE) + (R * R) - (2 * RE * R * Cos(PS) * Cos(L + Radians(N) - LG)))
        
        LI = RH * 0.005775518
        
        If (RD < RE) Then
            EP = Atn((-RD * S3) / (RE - (RD * C3))) + LG + 3.141592654
        Else
            EP = Atn((RE * S3) / (RD - (RE * C3))) + LC
        End If

        EP = Unwind(EP)
        TB = (RD * S2 * Sin(EP - LC)) / (C2 * RE * S3)
        BP = Atn(TB)
        PcometDist = RH
        
End Function

Function MoonLong(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        UT = LctUT(LH, LM, LS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LH, LM, LS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LH, LM, LS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LH, LM, LS, DS, ZC, DY, MN, YR)
        T = ((CDJD(GD, GM, GY) - 2415020#) / 36525#) + (UT / 876600#)
        T2 = T * T

        M1 = 27.32158213: M2 = 365.2596407: M3 = 27.55455094
        M4 = 29.53058868: M5 = 27.21222039: M6 = 6798.363307
        Q = CDJD(GD, GM, GY) - 2415020# + (UT / 24#)
        M1 = Q / M1: M2 = Q / M2: M3 = Q / M3
        M4 = Q / M4: M5 = Q / M5: M6 = Q / M6
        M1 = 360 * (M1 - Int(M1)): M2 = 360 * (M2 - Int(M2))
        M3 = 360 * (M3 - Int(M3)): M4 = 360 * (M4 - Int(M4))
        M5 = 360 * (M5 - Int(M5)): M6 = 360 * (M6 - Int(M6))

        ML = 270.434164 + M1 - (0.001133 - 0.0000019 * T) * T2
        MS = 358.475833 + M2 - (0.00015 + 0.0000033 * T) * T2
        MD = 296.104608 + M3 + (0.009192 + 0.0000144 * T) * T2
        ME1 = 350.737486 + M4 - (0.001436 - 0.0000019 * T) * T2
        MF = 11.250889 + M5 - (0.003211 + 0.0000003 * T) * T2
        NA = 259.183275 - M6 + (0.002078 + 0.0000022 * T) * T2
        A = Radians(51.2 + 20.2 * T): S1 = Sin(A): S2 = Sin(Radians(NA))
        B = 346.56 + (132.87 - 0.0091731 * T) * T
        S3 = 0.003964 * Sin(Radians(B))
        C = Radians(NA + 275.05 - 2.3 * T): S4 = Sin(C)
        ML = ML + 0.000233 * S1 + S3 + 0.001964 * S2
        MS = MS - 0.001778 * S1
        MD = MD + 0.000817 * S1 + S3 + 0.002541 * S2
        MF = MF + S3 - 0.024691 * S2 - 0.004328 * S4
        ME1 = ME1 + 0.002011 * S1 + S3 + 0.001964 * S2
        E = 1# - (0.002495 + 0.00000752 * T) * T: E2 = E * E
        ML = Radians(ML): MS = Radians(MS): NA = Radians(NA)
        ME1 = Radians(ME1): MF = Radians(MF): MD = Radians(MD)

        L = 6.28875 * Sin(MD) + 1.274018 * Sin(2 * ME1 - MD)
        L = L + 0.658309 * Sin(2 * ME1) + 0.213616 * Sin(2 * MD)
        L = L - E * 0.185596 * Sin(MS) - 0.114336 * Sin(2 * MF)
        L = L + 0.058793 * Sin(2 * (ME1 - MD))
        L = L + 0.057212 * E * Sin(2 * ME1 - MS - MD) + 0.05332 * Sin(2 * ME1 + MD)
        L = L + 0.045874 * E * Sin(2 * ME1 - MS) + 0.041024 * E * Sin(MD - MS)
        L = L - 0.034718 * Sin(ME1) - E * 0.030465 * Sin(MS + MD)
        L = L + 0.015326 * Sin(2 * (ME1 - MF)) - 0.012528 * Sin(2 * MF + MD)
        L = L - 0.01098 * Sin(2 * MF - MD) + 0.010674 * Sin(4 * ME1 - MD)
        L = L + 0.010034 * Sin(3 * MD) + 0.008548 * Sin(4 * ME1 - 2 * MD)
        L = L - E * 0.00791 * Sin(MS - MD + 2 * ME1) - E * 0.006783 * Sin(2 * ME1 + MS)
        L = L + 0.005162 * Sin(MD - ME1) + E * 0.005 * Sin(MS + ME1)
        L = L + 0.003862 * Sin(4 * ME1) + E * 0.004049 * Sin(MD - MS + 2 * ME1)
        L = L + 0.003996 * Sin(2 * (MD + ME1)) + 0.003665 * Sin(2 * ME1 - 3 * MD)
        L = L + E * 0.002695 * Sin(2 * MD - MS) + 0.002602 * Sin(MD - 2 * (MF + ME1))
        L = L + E * 0.002396 * Sin(2 * (ME1 - MD) - MS) - 0.002349 * Sin(MD + ME1)
        L = L + E2 * 0.002249 * Sin(2 * (ME1 - MS)) - E * 0.002125 * Sin(2 * MD + MS)
        L = L - E2 * 0.002079 * Sin(2 * MS) + E2 * 0.002059 * Sin(2 * (ME1 - MS) - MD)
        L = L - 0.001773 * Sin(MD + 2 * (ME1 - MF)) - 0.001595 * Sin(2 * (MF + ME1))
        L = L + E * 0.00122 * Sin(4 * ME1 - MS - MD) - 0.00111 * Sin(2 * (MD + MF))
        L = L + 0.000892 * Sin(MD - 3 * ME1) - E * 0.000811 * Sin(MS + MD + 2 * ME1)
        L = L + E * 0.000761 * Sin(4 * ME1 - MS - 2 * MD)
        L = L + E2 * 0.000704 * Sin(MD - 2 * (MS + ME1))
        L = L + E * 0.000693 * Sin(MS - 2 * (MD - ME1))
        L = L + E * 0.000598 * Sin(2 * (ME1 - MF) - MS)
        L = L + 0.00055 * Sin(MD + 4 * ME1) + 0.000538 * Sin(4 * MD)
        L = L + E * 0.000521 * Sin(4 * ME1 - MS) + 0.000486 * Sin(2 * MD - ME1)
        L = L + E2 * 0.000717 * Sin(MD - 2 * MS)
        MM = Unwind(ML + Radians(L))
        
        MoonLong = Degrees(MM)
        
End Function

Function MoonNodeLong(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        UT = LctUT(LH, LM, LS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LH, LM, LS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LH, LM, LS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LH, LM, LS, DS, ZC, DY, MN, YR)
        T = ((CDJD(GD, GM, GY) - 2415020#) / 36525#) + (UT / 876600#)
        T2 = T * T

        M1 = 27.32158213: M2 = 365.2596407: M3 = 27.55455094
        M4 = 29.53058868: M5 = 27.21222039: M6 = 6798.363307
        Q = CDJD(GD, GM, GY) - 2415020# + (UT / 24#)
        M1 = Q / M1: M2 = Q / M2: M3 = Q / M3
        M4 = Q / M4: M5 = Q / M5: M6 = Q / M6
        M1 = 360 * (M1 - Int(M1)): M2 = 360 * (M2 - Int(M2))
        M3 = 360 * (M3 - Int(M3)): M4 = 360 * (M4 - Int(M4))
        M5 = 360 * (M5 - Int(M5)): M6 = 360 * (M6 - Int(M6))

        ML = 270.434164 + M1 - (0.001133 - 0.0000019 * T) * T2
        MS = 358.475833 + M2 - (0.00015 + 0.0000033 * T) * T2
        MD = 296.104608 + M3 + (0.009192 + 0.0000144 * T) * T2
        ME1 = 350.737486 + M4 - (0.001436 - 0.0000019 * T) * T2
        MF = 11.250889 + M5 - (0.003211 + 0.0000003 * T) * T2
        NA = 259.183275 - M6 + (0.002078 + 0.0000022 * T) * T2
        
        MoonNodeLong = NA
        
End Function

Function MoonLat(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        UT = LctUT(LH, LM, LS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LH, LM, LS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LH, LM, LS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LH, LM, LS, DS, ZC, DY, MN, YR)
        T = ((CDJD(GD, GM, GY) - 2415020#) / 36525#) + (UT / 876600#)
        T2 = T * T

        M1 = 27.32158213: M2 = 365.2596407: M3 = 27.55455094
        M4 = 29.53058868: M5 = 27.21222039: M6 = 6798.363307
        Q = CDJD(GD, GM, GY) - 2415020# + (UT / 24#)
        M1 = Q / M1: M2 = Q / M2: M3 = Q / M3
        M4 = Q / M4: M5 = Q / M5: M6 = Q / M6
        M1 = 360 * (M1 - Int(M1)): M2 = 360 * (M2 - Int(M2))
        M3 = 360 * (M3 - Int(M3)): M4 = 360 * (M4 - Int(M4))
        M5 = 360 * (M5 - Int(M5)): M6 = 360 * (M6 - Int(M6))

        ML = 270.434164 + M1 - (0.001133 - 0.0000019 * T) * T2
        MS = 358.475833 + M2 - (0.00015 + 0.0000033 * T) * T2
        MD = 296.104608 + M3 + (0.009192 + 0.0000144 * T) * T2
        ME1 = 350.737486 + M4 - (0.001436 - 0.0000019 * T) * T2
        MF = 11.250889 + M5 - (0.003211 + 0.0000003 * T) * T2
        NA = 259.183275 - M6 + (0.002078 + 0.0000022 * T) * T2
        A = Radians(51.2 + 20.2 * T): S1 = Sin(A): S2 = Sin(Radians(NA))
        B = 346.56 + (132.87 - 0.0091731 * T) * T
        S3 = 0.003964 * Sin(Radians(B))
        C = Radians(NA + 275.05 - 2.3 * T): S4 = Sin(C)
        ML = ML + 0.000233 * S1 + S3 + 0.001964 * S2
        MS = MS - 0.001778 * S1
        MD = MD + 0.000817 * S1 + S3 + 0.002541 * S2
        MF = MF + S3 - 0.024691 * S2 - 0.004328 * S4
        ME1 = ME1 + 0.002011 * S1 + S3 + 0.001964 * S2
        E = 1# - (0.002495 + 0.00000752 * T) * T: E2 = E * E
        ML = Radians(ML): MS = Radians(MS): NA = Radians(NA)
        ME1 = Radians(ME1): MF = Radians(MF): MD = Radians(MD)

        G = 5.128189 * Sin(MF) + 0.280606 * Sin(MD + MF)
        G = G + 0.277693 * Sin(MD - MF) + 0.173238 * Sin(2 * ME1 - MF)
        G = G + 0.055413 * Sin(2 * ME1 + MF - MD) + 0.046272 * Sin(2 * ME1 - MF - MD)
        G = G + 0.032573 * Sin(2 * ME1 + MF) + 0.017198 * Sin(2 * MD + MF)
        G = G + 0.009267 * Sin(2 * ME1 + MD - MF) + 0.008823 * Sin(2 * MD - MF)
        G = G + E * 0.008247 * Sin(2 * ME1 - MS - MF) + 0.004323 * Sin(2 * (ME1 - MD) - MF)
        G = G + 0.0042 * Sin(2 * ME1 + MF + MD) + E * 0.003372 * Sin(MF - MS - 2 * ME1)
        G = G + E * 0.002472 * Sin(2 * ME1 + MF - MS - MD)
        G = G + E * 0.002222 * Sin(2 * ME1 + MF - MS)
        G = G + E * 0.002072 * Sin(2 * ME1 - MF - MS - MD)
        G = G + E * 0.001877 * Sin(MF - MS + MD) + 0.001828 * Sin(4 * ME1 - MF - MD)
        G = G - E * 0.001803 * Sin(MF + MS) - 0.00175 * Sin(3 * MF)
        G = G + E * 0.00157 * Sin(MD - MS - MF) - 0.001487 * Sin(MF + ME1)
        G = G - E * 0.001481 * Sin(MF + MS + MD) + E * 0.001417 * Sin(MF - MS - MD)
        G = G + E * 0.00135 * Sin(MF - MS) + 0.00133 * Sin(MF - ME1)
        G = G + 0.001106 * Sin(MF + 3 * MD) + 0.00102 * Sin(4 * ME1 - MF)
        G = G + 0.000833 * Sin(MF + 4 * ME1 - MD) + 0.000781 * Sin(MD - 3 * MF)
        G = G + 0.00067 * Sin(MF + 4 * ME1 - 2 * MD) + 0.000606 * Sin(2 * ME1 - 3 * MF)
        G = G + 0.000597 * Sin(2 * (ME1 + MD) - MF)
        G = G + E * 0.000492 * Sin(2 * ME1 + MD - MS - MF) + 0.00045 * Sin(2 * (MD - ME1) - MF)
        G = G + 0.000439 * Sin(3 * MD - MF) + 0.000423 * Sin(MF + 2 * (ME1 + MD))
        G = G + 0.000422 * Sin(2 * ME1 - MF - 3 * MD) - E * 0.000367 * Sin(MS + MF + 2 * ME1 - MD)
        G = G - E * 0.000353 * Sin(MS + MF + 2 * ME1) + 0.000331 * Sin(MF + 4 * ME1)
        G = G + E * 0.000317 * Sin(2 * ME1 + MF - MS + MD)
        G = G + E2 * 0.000306 * Sin(2 * (ME1 - MS) - MF) - 0.000283 * Sin(MD + 3 * MF)
        W1 = 0.0004664 * Cos(NA): W2 = 0.0000754 * Cos(C)
        BM = Radians(G) * (1# - W1 - W2)

        MoonLat = Degrees(BM)
        
End Function

Function MoonHP(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        UT = LctUT(LH, LM, LS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LH, LM, LS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LH, LM, LS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LH, LM, LS, DS, ZC, DY, MN, YR)
        T = ((CDJD(GD, GM, GY) - 2415020#) / 36525#) + (UT / 876600#)
        T2 = T * T

        M1 = 27.32158213: M2 = 365.2596407: M3 = 27.55455094
        M4 = 29.53058868: M5 = 27.21222039: M6 = 6798.363307
        Q = CDJD(GD, GM, GY) - 2415020# + (UT / 24#)
        M1 = Q / M1: M2 = Q / M2: M3 = Q / M3
        M4 = Q / M4: M5 = Q / M5: M6 = Q / M6
        M1 = 360 * (M1 - Int(M1)): M2 = 360 * (M2 - Int(M2))
        M3 = 360 * (M3 - Int(M3)): M4 = 360 * (M4 - Int(M4))
        M5 = 360 * (M5 - Int(M5)): M6 = 360 * (M6 - Int(M6))

        ML = 270.434164 + M1 - (0.001133 - 0.0000019 * T) * T2
        MS = 358.475833 + M2 - (0.00015 + 0.0000033 * T) * T2
        MD = 296.104608 + M3 + (0.009192 + 0.0000144 * T) * T2
        ME1 = 350.737486 + M4 - (0.001436 - 0.0000019 * T) * T2
        MF = 11.250889 + M5 - (0.003211 + 0.0000003 * T) * T2
        NA = 259.183275 - M6 + (0.002078 + 0.0000022 * T) * T2
        A = Radians(51.2 + 20.2 * T): S1 = Sin(A): S2 = Sin(Radians(NA))
        B = 346.56 + (132.87 - 0.0091731 * T) * T
        S3 = 0.003964 * Sin(Radians(B))
        C = Radians(NA + 275.05 - 2.3 * T): S4 = Sin(C)
        ML = ML + 0.000233 * S1 + S3 + 0.001964 * S2
        MS = MS - 0.001778 * S1
        MD = MD + 0.000817 * S1 + S3 + 0.002541 * S2
        MF = MF + S3 - 0.024691 * S2 - 0.004328 * S4
        ME1 = ME1 + 0.002011 * S1 + S3 + 0.001964 * S2
        E = 1# - (0.002495 + 0.00000752 * T) * T: E2 = E * E
        ML = Radians(ML): MS = Radians(MS): NA = Radians(NA)
        ME1 = Radians(ME1): MF = Radians(MF): MD = Radians(MD)

        PM = 0.950724 + 0.051818 * Cos(MD) + 0.009531 * Cos(2 * ME1 - MD)
        PM = PM + 0.007843 * Cos(2 * ME1) + 0.002824 * Cos(2 * MD)
        PM = PM + 0.000857 * Cos(2 * ME1 + MD) + E * 0.000533 * Cos(2 * ME1 - MS)
        PM = PM + E * 0.000401 * Cos(2 * ME1 - MD - MS)
        PM = PM + E * 0.00032 * Cos(MD - MS) - 0.000271 * Cos(ME1)
        PM = PM - E * 0.000264 * Cos(MS + MD) - 0.000198 * Cos(2 * MF - MD)
        PM = PM + 0.000173 * Cos(3 * MD) + 0.000167 * Cos(4 * ME1 - MD)
        PM = PM - E * 0.000111 * Cos(MS) + 0.000103 * Cos(4 * ME1 - 2 * MD)
        PM = PM - 0.000084 * Cos(2 * MD - 2 * ME1) - E * 0.000083 * Cos(2 * ME1 + MS)
        PM = PM + 0.000079 * Cos(2 * ME1 + 2 * MD) + 0.000072 * Cos(4 * ME1)
        PM = PM + E * 0.000064 * Cos(2 * ME1 - MS + MD) - E * 0.000063 * Cos(2 * ME1 + MS - MD)
        PM = PM + E * 0.000041 * Cos(MS + ME1) + E * 0.000035 * Cos(2 * MD - MS)
        PM = PM - 0.000033 * Cos(3 * MD - 2 * ME1) - 0.00003 * Cos(MD + ME1)
        PM = PM - 0.000029 * Cos(2 * (MF - ME1)) - E * 0.000029 * Cos(2 * MD + MS)
        PM = PM + E2 * 0.000026 * Cos(2 * (ME1 - MS)) - 0.000023 * Cos(2 * (MF - ME1) + MD)
        PM = PM + E * 0.000019 * Cos(4 * ME1 - MS - MD)
        
        MoonHP = PM
        
End Function

Function UnwindDeg(W As Double) As Double

        UnwindDeg = W - 360# * Int(W / 360#)

End Function

Function UnwindRad(W As Double) As Double

    UnwindRad = W - 6.283185308 * Int(W / 6.283185308)
    
End Function

Function MoonMeanAnomaly(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        UT = LctUT(LH, LM, LS, DS, ZC, DY, MN, YR)
        GD = LctGDay(LH, LM, LS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LH, LM, LS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LH, LM, LS, DS, ZC, DY, MN, YR)
        T = ((CDJD(GD, GM, GY) - 2415020#) / 36525#) + (UT / 876600#)
        T2 = T * T

        M1 = 27.32158213: M2 = 365.2596407: M3 = 27.55455094
        M4 = 29.53058868: M5 = 27.21222039: M6 = 6798.363307
        Q = CDJD(GD, GM, GY) - 2415020# + (UT / 24#)
        M1 = Q / M1: M2 = Q / M2: M3 = Q / M3
        M4 = Q / M4: M5 = Q / M5: M6 = Q / M6
        M1 = 360 * (M1 - Int(M1)): M2 = 360 * (M2 - Int(M2))
        M3 = 360 * (M3 - Int(M3)): M4 = 360 * (M4 - Int(M4))
        M5 = 360 * (M5 - Int(M5)): M6 = 360 * (M6 - Int(M6))

        ML = 270.434164 + M1 - (0.001133 - 0.0000019 * T) * T2
        MS = 358.475833 + M2 - (0.00015 + 0.0000033 * T) * T2
        MD = 296.104608 + M3 + (0.009192 + 0.0000144 * T) * T2
        ME1 = 350.737486 + M4 - (0.001436 - 0.0000019 * T) * T2
        MF = 11.250889 + M5 - (0.003211 + 0.0000003 * T) * T2
        NA = 259.183275 - M6 + (0.002078 + 0.0000022 * T) * T2
        A = Radians(51.2 + 20.2 * T): S1 = Sin(A): S2 = Sin(Radians(NA))
        B = 346.56 + (132.87 - 0.0091731 * T) * T
        S3 = 0.003964 * Sin(Radians(B))
        C = Radians(NA + 275.05 - 2.3 * T): S4 = Sin(C)
        ML = ML + 0.000233 * S1 + S3 + 0.001964 * S2
        MS = MS - 0.001778 * S1
        MD = MD + 0.000817 * S1 + S3 + 0.002541 * S2
        
        MoonMeanAnomaly = Radians(MD)
        
End Function

Function MoonPhase(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        CD = Cos(Radians(MoonLong(LH, LM, LS, DS, ZC, DY, MN, YR) - SunLong(LH, LM, LS, DS, ZC, DY, MN, YR))) * Cos(Radians(MoonLat(LH, LM, LS, DS, ZC, DY, MN, YR)))
        D = Acos(CD)
        SD = Sin(D)
        I = 0.1468 * SD * (1# - 0.0549 * Sin(MoonMeanAnomaly(LH, LM, LS, DS, ZC, DY, MN, YR)))
        I = I / (1# - 0.0167 * Sin(SunMeanAnomaly(LH, LM, LS, DS, ZC, DY, MN, YR)))
        I = 3.141592654 - D - Radians(I)
        K = (1# + Cos(I)) / 2#
        MoonPhase = Round(K, 2)
        
End Function

Function MoonPABL(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        GD = LctGDay(LH, LM, LS, DS, ZC, DY, MN, YR)
        GM = LctGMonth(LH, LM, LS, DS, ZC, DY, MN, YR)
        GY = LctGYear(LH, LM, LS, DS, ZC, DY, MN, YR)
        LLS = SunLong(LH, LM, LS, DS, ZC, DY, MN, YR)
        LLM = MoonLong(LH, LM, LS, DS, ZC, DY, MN, YR)
        BM = MoonLat(LH, LM, LS, DS, ZC, DY, MN, YR)
        RAS = Radians(ECRA(LLS, 0, 0, 0, 0, 0, GD, GM, GY))
        RAM = Radians(ECRA(LLM, 0, 0, BM, 0, 0, GD, GM, GY))
        DDS = Radians(ECDec(LLS, 0, 0, 0, 0, 0, GD, GM, GY))
        DM = Radians(ECDec(LLM, 0, 0, BM, 0, 0, GD, GM, GY))
        Y = Cos(DDS) * Sin(RAS - RAM)
        X = Cos(DM) * Sin(DDS) - Sin(DM) * Cos(DDS) * Cos(RAS - RAM)
        CHI = Atan2(X, Y)
        
        MoonPABL = Round(Degrees(CHI), 2)
        
End Function

Function MoonDist(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        HP = Radians(MoonHP(LH, LM, LS, DS, ZC, DY, MN, YR))
        R = 6378.14 / Sin(HP)
        MoonDist = R
        
End Function

Function MoonSize(LH As Double, LM As Double, LS As Double, DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        HP = Radians(MoonHP(LH, LM, LS, DS, ZC, DY, MN, YR))
        R = 6378.14 / Sin(HP)
        TH = 384401 * 0.5181 / R
        MoonSize = TH
        
End Function

Function IINT(W) As Double
        
        IINT = Sgn(W) * Int(Abs(W))
        
End Function
Function LINT(W) As Double
        
        LINT = IINT(W) + IINT(((1# * Sgn(W)) - 1#) / 2#)
        
End Function
Function FRACT(W) As Double

        FRACT = W - LINT(W)
        
End Function

Function FullMoon(DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        D0 = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        M0 = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        Y0 = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        
        If (Y0 < 0) Then
            Y0 = Y0 + 1
        End If
        
        J0 = CDJD(0, 1, Y0) - 2415020#
        DJ = CDJD(D0, M0, Y0) - 2415020#
        K = LINT(((Y0 - 1900# + ((DJ - J0) / 365#)) * 12.3685) + 0.5)
        TN = K / 1236.85: TF = (K + 0.5) / 1236.85
        T = TN: GoSub 6855: NI = A: NF = B: NB = F
        T = TF: K = K + 0.5: GoSub 6855: FI = A: FF = B: FB = F
        GoTo 6940

6855    T2 = T * T: E = 29.53 * K: C = 166.56 + (132.87 - 0.009173 * T) * T
        C = Radians(C): B = 0.00058868 * K + (0.0001178 - 0.000000155 * T) * T2
        B = B + 0.00033 * Sin(C) + 0.75933: A = K / 12.36886
        A1 = 359.2242 + 360 * FRACT(A) - (0.0000333 + 0.00000347 * T) * T2
        A2 = 306.0253 + 360 * FRACT(K / 0.9330851)
        A2 = A2 + (0.0107306 + 0.00001236 * T) * T2: A = K / 0.9214926
        F = 21.2964 + 360# * FRACT(A) - (0.0016528 + 0.00000239 * T) * T2
        A1 = UnwindDeg(A1): A2 = UnwindDeg(A2): F = UnwindDeg(F)
        A1 = Radians(A1): A2 = Radians(A2): F = Radians(F)

        DD = (0.1734 - 0.000393 * T) * Sin(A1) + 0.0021 * Sin(2 * A1)
        DD = DD - 0.4068 * Sin(A2) + 0.0161 * Sin(2 * A2) - 0.0004 * Sin(3 * A2)
        DD = DD + 0.0104 * Sin(2 * F) - 0.0051 * Sin(A1 + A2)
        DD = DD - 0.0074 * Sin(A1 - A2) + 0.0004 * Sin(2 * F + A1)
        DD = DD - 0.0004 * Sin(2 * F - A1) - 0.0006 * Sin(2 * F + A2) + 0.001 * Sin(2 * F - A2)
        DD = DD + 0.0005 * Sin(A1 + 2 * A2): E1 = Int(E): B = B + DD + (E - E1)
        B1 = Int(B): A = E1 + B1: B = B - B1
        Return

6940    FullMoon = FI + 2415020# + FF

End Function

Function Fpart(W As Double) As Double

        Fpart = W - LINT(W)
        
End Function

Function LEOccurrence(DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As String
Dim S As String

        D0 = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        M0 = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        Y0 = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        
        If (Y0 < 0) Then
            Y0 = Y0 + 1
        End If
        
        J0 = CDJD(0, 1, Y0)
        DJ = CDJD(D0, M0, Y0)
        K = ((Y0 - 1900# + ((DJ - J0) * 1# / 365#)) * 12.3685)
        K = LINT(K + 0.5)
        TN = K / 1236.85: TF = (K + 0.5) / 1236.85
        T = TN: GoSub 6855: NI = A: NF = B: NB = F
        T = TF: K = K + 0.5: GoSub 6855: FI = A: FF = B: FB = F
        GoTo 6940

6855    T2 = T * T: E = 29.53 * K: C = 166.56 + (132.87 - 0.009173 * T) * T
        C = Radians(C): B = 0.00058868 * K + (0.0001178 - 0.000000155 * T) * T2
        B = B + 0.00033 * Sin(C) + 0.75933: A = K / 12.36886
        A1 = 359.2242 + 360 * Fpart(A) - (0.0000333 + 0.00000347 * T) * T2
        A2 = 306.0253 + 360 * Fpart(K / 0.9330851)
        A2 = A2 + (0.0107306 + 0.00001236 * T) * T2: A = K / 0.9214926
        F = 21.2964 + 360# * Fpart(A) - (0.0016528 + 0.00000239 * T) * T2
        A1 = UnwindDeg(A1): A2 = UnwindDeg(A2): F = UnwindDeg(F)
        A1 = Radians(A1): A2 = Radians(A2): F = Radians(F)

        DD = (0.1734 - 0.000393 * T) * Sin(A1) + 0.0021 * Sin(2 * A1)
        DD = DD - 0.4068 * Sin(A2) + 0.0161 * Sin(2 * A2) - 0.0004 * Sin(3 * A2)
        DD = DD + 0.0104 * Sin(2 * F) - 0.0051 * Sin(A1 + A2)
        DD = DD - 0.0074 * Sin(A1 - A2) + 0.0004 * Sin(2 * F + A1)
        DD = DD - 0.0004 * Sin(2 * F - A1) - 0.0006 * Sin(2 * F + A2) + 0.001 * Sin(2 * F - A2)
        DD = DD + 0.0005 * Sin(A1 + 2 * A2): E1 = Int(E): B = B + DD + (E - E1)
        B1 = Int(B): A = E1 + B1: B = B - B1
        Return

6940    DF = Abs(FB - 3.141592654 * LINT(FB / 3.141592654))
        
        If (DF > 0.37) Then
            DF = 3.141592654 - DF
        End If
        
        S = "Lunar eclipse certain"
        If (DF >= 0.242600766) Then
            S = "Lunar eclipse possible"
            If (DF > 0.37) Then
                S = "No lunar eclipse"
            End If
        End If
        
        LEOccurrence = S

End Function

Function SEOccurrence(DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As String
Dim S As String

        D0 = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        M0 = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        Y0 = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        
        If (Y0 < 0) Then
            Y0 = Y0 + 1
        End If
        
        J0 = CDJD(0, 1, Y0)
        DJ = CDJD(D0, M0, Y0)
        K = ((Y0 - 1900# + ((DJ - J0) * 1# / 365#)) * 12.3685)
        K = LINT(K + 0.5)
        TN = K / 1236.85: TF = (K + 0.5) / 1236.85
        T = TN: GoSub 6855: NI = A: NF = B: NB = F
        T = TF: K = K + 0.5: GoSub 6855: FI = A: FF = B: FB = F
        GoTo 6940

6855    T2 = T * T: E = 29.53 * K: C = 166.56 + (132.87 - 0.009173 * T) * T
        C = Radians(C): B = 0.00058868 * K + (0.0001178 - 0.000000155 * T) * T2
        B = B + 0.00033 * Sin(C) + 0.75933: A = K / 12.36886
        A1 = 359.2242 + 360 * Fpart(A) - (0.0000333 + 0.00000347 * T) * T2
        A2 = 306.0253 + 360 * Fpart(K / 0.9330851)
        A2 = A2 + (0.0107306 + 0.00001236 * T) * T2: A = K / 0.9214926
        F = 21.2964 + 360# * Fpart(A) - (0.0016528 + 0.00000239 * T) * T2
        A1 = UnwindDeg(A1): A2 = UnwindDeg(A2): F = UnwindDeg(F)
        A1 = Radians(A1): A2 = Radians(A2): F = Radians(F)

        DD = (0.1734 - 0.000393 * T) * Sin(A1) + 0.0021 * Sin(2 * A1)
        DD = DD - 0.4068 * Sin(A2) + 0.0161 * Sin(2 * A2) - 0.0004 * Sin(3 * A2)
        DD = DD + 0.0104 * Sin(2 * F) - 0.0051 * Sin(A1 + A2)
        DD = DD - 0.0074 * Sin(A1 - A2) + 0.0004 * Sin(2 * F + A1)
        DD = DD - 0.0004 * Sin(2 * F - A1) - 0.0006 * Sin(2 * F + A2) + 0.001 * Sin(2 * F - A2)
        DD = DD + 0.0005 * Sin(A1 + 2 * A2): E1 = Int(E): B = B + DD + (E - E1)
        B1 = Int(B): A = E1 + B1: B = B - B1
        Return

6940    DF = Abs(NB - 3.141592654 * LINT(NB / 3.141592654))
        
        If (DF > 0.37) Then
            DF = 3.141592654 - DF
        End If
        
        S = "Solar eclipse certain"
        If (DF >= 0.242600766) Then
            S = "Solar eclipse possible"
            If (DF > 0.37) Then
                S = "No solar eclipse"
            End If
        End If
        
        SEOccurrence = S

End Function

Function NewMoon(DS As Double, ZC As Double, DY As Double, MN As Double, YR As Double) As Double

        D0 = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        M0 = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        Y0 = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        
        If (Y0 < 0) Then
            Y0 = Y0 + 1
        End If
        
        J0 = CDJD(0, 1, Y0) - 2415020#
        DJ = CDJD(D0, M0, Y0) - 2415020#
        K = LINT(((Y0 - 1900# + ((DJ - J0) / 365#)) * 12.3685) + 0.5)
        TN = K / 1236.85: TF = (K + 0.5) / 1236.85
        T = TN: GoSub 6855: NI = A: NF = B: NB = F
        T = TF: K = K + 0.5: GoSub 6855: FI = A: FF = B: FB = F
        GoTo 6940

6855    T2 = T * T: E = 29.53 * K: C = 166.56 + (132.87 - 0.009173 * T) * T
        C = Radians(C): B = 0.00058868 * K + (0.0001178 - 0.000000155 * T) * T2
        B = B + 0.00033 * Sin(C) + 0.75933: A = K / 12.36886
        A1 = 359.2242 + 360 * FRACT(A) - (0.0000333 + 0.00000347 * T) * T2
        A2 = 306.0253 + 360 * FRACT(K / 0.9330851)
        A2 = A2 + (0.0107306 + 0.00001236 * T) * T2: A = K / 0.9214926
        F = 21.2964 + 360# * FRACT(A) - (0.0016528 + 0.00000239 * T) * T2
        A1 = UnwindDeg(A1): A2 = UnwindDeg(A2): F = UnwindDeg(F)
        A1 = Radians(A1): A2 = Radians(A2): F = Radians(F)

        DD = (0.1734 - 0.000393 * T) * Sin(A1) + 0.0021 * Sin(2 * A1)
        DD = DD - 0.4068 * Sin(A2) + 0.0161 * Sin(2 * A2) - 0.0004 * Sin(3 * A2)
        DD = DD + 0.0104 * Sin(2 * F) - 0.0051 * Sin(A1 + A2)
        DD = DD - 0.0074 * Sin(A1 - A2) + 0.0004 * Sin(2 * F + A1)
        DD = DD - 0.0004 * Sin(2 * F - A1) - 0.0006 * Sin(2 * F + A2) + 0.001 * Sin(2 * F - A2)
        DD = DD + 0.0005 * Sin(A1 + 2 * A2): E1 = Int(E): B = B + DD + (E - E1)
        B1 = Int(B): A = E1 + B1: B = B - B1
        Return

6940    NewMoon = NI + 2415020# + NF

End Function

Function UTDayAdjust(UT As Double, G1 As Double)

        UTDayAdjust = UT
        
        If ((UT - G1) < -6#) Then
            UTDayAdjust = UT + 24#
        End If
        
        If ((UT - G1) > 6#) Then
            UTDayAdjust = UT - 24#
        End If

End Function

Function MoonRiseLCT(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
        
        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
        
        If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTR(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        
        If (eRS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat) <> "OK") Then
            LCT = -99
            GoTo 6800
        End If
        
        Return

6800    MoonRiseLCT = LCT

End Function

Function eMoonRise(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As String
Dim K As Integer
Dim S1 As String, S2 As String, S3 As String, S4 As String

        S4 = "OK"
        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        If (S1 <> "OK") Then
            S4 = S1
            GoTo 6800
        End If

        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
        S3 = eGSTUT(X, 0, 0, GDY, GMN, GYR)
        
        If (S3 <> "OK") Then
            S4 = "GST conversion: " + S3
        End If

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        If (S1 <> "OK") Then
            S4 = S1
            GoTo 6800
        End If

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
        S3 = eGSTUT(X, 0, 0, GDY, GMN, GYR)
        
        If (S3 <> "OK") Then
            S4 = "GST conversion: " + S3
        End If
        
        GoTo 6800
    
6680    If (S3 <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If

        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTR(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        S1 = eRS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    eMoonRise = S4

End Function

Function MoonRiseLcDay(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
    
        If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTR(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    MoonRiseLcDay = DY1

End Function

Function MoonRiseLcMonth(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
    
        If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTR(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    MoonRiseLcMonth = MN1

End Function

Function MoonRiseLcYear(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
    
        If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
            
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
            
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTR(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    MoonRiseLcYear = YR1

End Function

Function MoonRiseAz(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
            
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTR(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        AU = RSAZR(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    MoonRiseAz = AU

End Function

Function MoonSetLCT(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
        
        If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        
        If (eRS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat) <> "OK") Then
            LCT = -99
            GoTo 6800
        End If
        
        Return

6800    MoonSetLCT = LCT

End Function

Function eMoonSet(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As String
Dim K As Integer
Dim S1 As String, S2 As String, S3 As String, S4 As String

        S4 = "OK"
        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        If (S1 <> "OK") Then
            S4 = S1
            GoTo 6800
        End If

        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
        S3 = eGSTUT(X, 0, 0, GDY, GMN, GYR)
        
        If (S3 <> "OK") Then
            S4 = "GST conversion: " + S3
        End If

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        If (S1 <> "OK") Then
            S4 = S1
            GoTo 6800
        End If

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
        S3 = eGSTUT(X, 0, 0, GDY, GMN, GYR)
        
        If (S3 <> "OK") Then
            S4 = "GST conversion: " + S3
        End If
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        S1 = eRS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    eMoonSet = S4

End Function

Function MoonSetLcDay(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
    
        If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    MoonSetLcDay = DY1

End Function

Function MoonSetLcMonth(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
    
        If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    MoonSetLcMonth = MN1

End Function

Function MoonSetLcYear(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)
    
        If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GoTo 6800
    
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    MoonSetLcYear = YR1

End Function

Function MoonSetAz(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double
Dim K As Integer

        GDY = LctGDay(12, 0, 0, DS, ZC, DY, MN, YR)
        GMN = LctGMonth(12, 0, 0, DS, ZC, DY, MN, YR)
        GYR = LctGYear(12, 0, 0, DS, ZC, DY, MN, YR)
        LCT = 12#: DY1 = DY: MN1 = MN: YR1 = YR
        GoSub 6700: LA = LU
        
        For K = 1 To 8

        X = LSTGST(LA, 0, 0, GLong)
        UT = GSTUT(X, 0, 0, GDY, GMN, GYR)

        If (K = 1) Then
            G1 = UT
        Else
            G1 = GU
        End If
        
        GU = UT
        UT = GU: GoSub 6680: LA = LU: AA = AU

        Next K
        AU = AA
        GoTo 6800
        
6680    If (eGSTUT(X, 0, 0, GDY, GMN, GYR) <> "OK") Then
            If (Abs(G1 - UT) > 0.5) Then
                UT = UT + 23.93447
            End If
        End If
        
        UT = UTDayAdjust(UT, G1)
        LCT = UTLct(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        DY1 = UTLcDay(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        MN1 = UTLcMonth(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        YR1 = UTLcYear(UT, 0, 0, DS, ZC, GDY, GMN, GYR)
        GDY = LctGDay(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GMN = LctGMonth(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        GYR = LctGYear(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        UT = UT - 24# * Int(UT / 24#)
        
6700    MM = MoonLong(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        BM = MoonLat(LCT, 0, 0, DS, ZC, DY1, MN1, YR1)
        PM = Radians(MoonHP(LCT, 0, 0, DS, ZC, DY1, MN1, YR1))
        DP = NutatLong(GDY, GMN, GYR)
        TH = 0.27249 * Sin(PM): DI = TH + 0.0098902 - PM
        P = DDDH(ECRA(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR))
        Q = ECDec(MM + DP, 0, 0, BM, 0, 0, GDY, GMN, GYR)
        LU = RSLSTS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        AU = RSAZS(P, 0, 0, Q, 0, 0, Degrees(DI), GLat)
        Return

6800    MoonSetAz = AU

End Function

Function UTMaxLunarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (LEOccurrence(DS, ZC, DY, MN, YR) = "No lunar eclipse") Then
            Z1 = -99#
            GoTo 7500
        End If
        
        DJ = FullMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTFM = XI * 24#
        UT = UTFM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTFM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTFM
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        SR = SR + Pi - LINT((SR + Pi) / TP) * TP
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RP: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            Z1 = -99#
            GoTo 7500
        End If
        
7500    UTMaxLunarEclipse = Z1

End Function

Function UTMaxSolarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (SEOccurrence(DS, ZC, DY, MN, YR) = "No solar eclipse") Then
            Z1 = -99#
            GoTo 7500
        End If
        
        DJ = NewMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTNM = XI * 24#
        UT = UTNM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTNM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTNM: X = MY: Y = BY: TM = XH - 1#: HP = HY
        GoSub 7390: MY = P: BY = Q
        X = MZ: Y = BZ: TM = XH + 1#: HP = HZ
        GoSub 7390: MZ = P: BZ = Q
        
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        X = SR: Y = 0: TM = UT
        HP = 0.00004263452 / RR: GoSub 7390: SR = P
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RN: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            Z1 = -99#
            GoTo 7500
        End If
           
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If
        
        MG = (RM + RN - PJ) / (2# * RN)
        GoTo 7500

7390    PAA = ECRA(Degrees(X), 0, 0, Degrees(Y), 0, 0, IGDay, GMonth, GYear)
        QAA = ECDec(Degrees(X), 0, 0, Degrees(Y), 0, 0, IGDay, GMonth, GYear)
        XAA = RAHA(DDDH(PAA), 0, 0, TM, 0, 0, 0, 0, IGDay, GMonth, GYear, GLong)
        PBB = ParallaxHA(XAA, 0, 0, QAA, 0, 0, "True", GLat, 0, Degrees(HP))
        QBB = ParallaxDec(XAA, 0, 0, QAA, 0, 0, "True", GLat, 0, Degrees(HP))
        XBB = HARA(PBB, 0, 0, TM, 0, 0, 0, 0, IGDay, GMonth, GYear, GLong)
        P = Radians(EQElong(XBB, 0, 0, QBB, 0, 0, IGDay, GMonth, GYear))
        Q = Radians(EQElat(XBB, 0, 0, QBB, 0, 0, IGDay, GMonth, GYear))
        Return

7500    UTMaxSolarEclipse = Z1

End Function

Function UTFirstContactLunarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (LEOccurrence(DS, ZC, DY, MN, YR) = "No lunar eclipse") Then
            Z6 = -99#
            GoTo 7500
        End If
        
        DJ = FullMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTFM = XI * 24#
        UT = UTFM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTFM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTFM
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        SR = SR + Pi - LINT((SR + Pi) / TP) * TP
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RP: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            Z6 = -99#
            GoTo 7500
        End If
        
        ZD = Sqr(DD): Z6 = Z1 - ZD
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If

7500    UTFirstContactLunarEclipse = Z6

End Function

Function UTFirstContactSolarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (SEOccurrence(DS, ZC, DY, MN, YR) = "No solar eclipse") Then
            Z6 = -99#
            GoTo 7500
        End If
        
        DJ = NewMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTNM = XI * 24#
        UT = UTNM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTNM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTNM: X = MY: Y = BY: TM = XH - 1#: HP = HY
        GoSub 7390: MY = P: BY = Q
        X = MZ: Y = BZ: TM = XH + 1#: HP = HZ
        GoSub 7390: MZ = P: BZ = Q
        
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        X = SR: Y = 0: TM = UT
        HP = 0.00004263452 / RR: GoSub 7390: SR = P
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RN: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            Z6 = -99#
            GoTo 7500
        End If
           
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If
        
        MG = (RM + RN - PJ) / (2# * RN)
        GoTo 7500

7390    PAA = ECRA(Degrees(X), 0, 0, Degrees(Y), 0, 0, IGDay, GMonth, GYear)
        QAA = ECDec(Degrees(X), 0, 0, Degrees(Y), 0, 0, IGDay, GMonth, GYear)
        XAA = RAHA(DDDH(PAA), 0, 0, TM, 0, 0, 0, 0, IGDay, GMonth, GYear, GLong)
        PBB = ParallaxHA(XAA, 0, 0, QAA, 0, 0, "True", GLat, 0, Degrees(HP))
        QBB = ParallaxDec(XAA, 0, 0, QAA, 0, 0, "True", GLat, 0, Degrees(HP))
        XBB = HARA(PBB, 0, 0, TM, 0, 0, 0, 0, IGDay, GMonth, GYear, GLong)
        P = Radians(EQElong(XBB, 0, 0, QBB, 0, 0, IGDay, GMonth, GYear))
        Q = Radians(EQElat(XBB, 0, 0, QBB, 0, 0, IGDay, GMonth, GYear))
        Return

7500    UTFirstContactSolarEclipse = Z6

End Function

Function UTLastContactLunarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (LEOccurrence(DS, ZC, DY, MN, YR) = "No lunar eclipse") Then
            Z7 = -99#
            GoTo 7500
        End If
        
        DJ = FullMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTFM = XI * 24#
        UT = UTFM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTFM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTFM
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        SR = SR + Pi - LINT((SR + Pi) / TP) * TP
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RP: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            Z7 = -99#
            GoTo 7500
        End If
        
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
7500    UTLastContactLunarEclipse = Z7

End Function

Function UTLastContactSolarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (SEOccurrence(DS, ZC, DY, MN, YR) = "No solar eclipse") Then
            Z7 = -99#
            GoTo 7500
        End If
        
        DJ = NewMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTNM = XI * 24#
        UT = UTNM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTNM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTNM: X = MY: Y = BY: TM = XH - 1#: HP = HY
        GoSub 7390: MY = P: BY = Q
        X = MZ: Y = BZ: TM = XH + 1#: HP = HZ
        GoSub 7390: MZ = P: BZ = Q
        
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        X = SR: Y = 0: TM = UT
        HP = 0.00004263452 / RR: GoSub 7390: SR = P
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RN: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            Z7 = -99#
            GoTo 7500
        End If
           
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If
        
        MG = (RM + RN - PJ) / (2# * RN)
        GoTo 7500

7390    PAA = ECRA(Degrees(X), 0, 0, Degrees(Y), 0, 0, IGDay, GMonth, GYear)
        QAA = ECDec(Degrees(X), 0, 0, Degrees(Y), 0, 0, IGDay, GMonth, GYear)
        XAA = RAHA(DDDH(PAA), 0, 0, TM, 0, 0, 0, 0, IGDay, GMonth, GYear, GLong)
        PBB = ParallaxHA(XAA, 0, 0, QAA, 0, 0, "True", GLat, 0, Degrees(HP))
        QBB = ParallaxDec(XAA, 0, 0, QAA, 0, 0, "True", GLat, 0, Degrees(HP))
        XBB = HARA(PBB, 0, 0, TM, 0, 0, 0, 0, IGDay, GMonth, GYear, GLong)
        P = Radians(EQElong(XBB, 0, 0, QBB, 0, 0, IGDay, GMonth, GYear))
        Q = Radians(EQElat(XBB, 0, 0, QBB, 0, 0, IGDay, GMonth, GYear))
        Return

7500    UTLastContactSolarEclipse = Z7

End Function

Function UTStartUmbralLunarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (LEOccurrence(DS, ZC, DY, MN, YR) = "No lunar eclipse") Then
            Z8 = -99#
            GoTo 7500
        End If
        
        DJ = FullMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTFM = XI * 24#
        UT = UTFM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTFM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTFM
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        SR = SR + Pi - LINT((SR + Pi) / TP) * TP
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RP: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            Z8 = -99#
            GoTo 7500
        End If
        
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If
        
        R = RM + RU: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        MG = (RM + RP - PJ) / (2# * RM)
        
        If (DD < 0) Then
            Z8 = -99#
            GoTo 7500
        End If

        ZD = Sqr(DD): Z8 = Z1 - ZD
        Z9 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z8 < 0) Then
            Z8 = Z8 + 24#
        End If

7500    UTStartUmbralLunarEclipse = Z8

End Function

Function UTEndUmbralLunarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (LEOccurrence(DS, ZC, DY, MN, YR) = "No lunar eclipse") Then
            Z9 = -99#
            GoTo 7500
        End If
        
        DJ = FullMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTFM = XI * 24#
        UT = UTFM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTFM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTFM
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        SR = SR + Pi - LINT((SR + Pi) / TP) * TP
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RP: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            Z9 = -99#
            GoTo 7500
        End If
        
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If
        
        R = RM + RU: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        MG = (RM + RP - PJ) / (2# * RM)
        
        If (DD < 0) Then
            Z9 = -99#
            GoTo 7500
        End If

        ZD = Sqr(DD): Z8 = Z1 - ZD
        Z9 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
7500    UTEndUmbralLunarEclipse = Z9

End Function

Function UTStartTotalLunarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (LEOccurrence(DS, ZC, DY, MN, YR) = "No lunar eclipse") Then
            ZCC = -99#
            GoTo 7500
        End If
        
        DJ = FullMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTFM = XI * 24#
        UT = UTFM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTFM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTFM
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        SR = SR + Pi - LINT((SR + Pi) / TP) * TP
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RP: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            ZCC = -99#
            GoTo 7500
        End If
        
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If
        
        R = RM + RU: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        MG = (RM + RP - PJ) / (2# * RM)
        
        If (DD < 0) Then
            ZCC = -99#
            GoTo 7500
        End If

        ZD = Sqr(DD): Z8 = Z1 - ZD
        Z9 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z8 < 0) Then
            Z8 = Z8 + 24#
        End If
        
        R = RU - RM: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        MG = (RM + RU - PJ) / (2# * RM)
        
        If (DD < 0) Then
            ZCC = -99#
            GoTo 7500
        End If

        ZD = Sqr(DD): ZCC = Z1 - ZD
        ZB = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (ZCC < 0) Then
            ZCC = ZC + 24#
        End If

7500    UTStartTotalLunarEclipse = ZCC

End Function

Function UTEndTotalLunarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (LEOccurrence(DS, ZC, DY, MN, YR) = "No lunar eclipse") Then
            ZB = -99#
            GoTo 7500
        End If
        
        DJ = FullMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTFM = XI * 24#
        UT = UTFM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTFM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTFM
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        SR = SR + Pi - LINT((SR + Pi) / TP) * TP
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RP: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            ZB = -99#
            GoTo 7500
        End If
        
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If
        
        R = RM + RU: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        MG = (RM + RP - PJ) / (2# * RM)
        
        If (DD < 0) Then
            ZB = -99#
            GoTo 7500
        End If

        ZD = Sqr(DD): Z8 = Z1 - ZD
        Z9 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z8 < 0) Then
            Z8 = Z8 + 24#
        End If
        
        R = RU - RM: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        MG = (RM + RU - PJ) / (2# * RM)
        
        If (DD < 0) Then
            ZB = -99#
            GoTo 7500
        End If

        ZD = Sqr(DD)
        ZB = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
7500    UTEndTotalLunarEclipse = ZB

End Function

Function MagLunarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (LEOccurrence(DS, ZC, DY, MN, YR) = "No lunar eclipse") Then
            MG = -99#
            GoTo 7500
        End If
        
        DJ = FullMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTFM = XI * 24#
        UT = UTFM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTFM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTFM
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        SR = SR + Pi - LINT((SR + Pi) / TP) * TP
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RP: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            MG = -99#
            GoTo 7500
        End If
        
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If
        
        R = RM + RU: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        MG = (RM + RP - PJ) / (2# * RM)
        
        If (DD < 0) Then
            GoTo 7500
        End If

        ZD = Sqr(DD): Z8 = Z1 - ZD
        Z9 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z8 < 0) Then
            Z8 = Z8 + 24#
        End If
        
        R = RU - RM: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        MG = (RM + RU - PJ) / (2# * RM)
        
7500    MagLunarEclipse = MG

End Function

Function MagSolarEclipse(DY As Double, MN As Double, YR As Double, DS As Double, ZC As Double, GLong As Double, GLat As Double) As Double

        Pi = 3.141592654: TP = 2# * Pi
        
        If (SEOccurrence(DS, ZC, DY, MN, YR) = "No solar eclipse") Then
            MG = -99#
            GoTo 7500
        End If
        
        DJ = NewMoon(DS, ZC, DY, MN, YR): DP = 0
        GDay = JDCDay(DJ)
        GMonth = JDCMonth(DJ)
        GYear = JDCYear(DJ)
        IGDay = Int(GDay)
        XI = GDay - IGDay
        UTNM = XI * 24#
        UT = UTNM - 1#
        LY = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        MY = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BY = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HY = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        UT = UTNM + 1#
        SB = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)) - LY
        MZ = Radians(MoonLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        BZ = Radians(MoonLat(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        HZ = Radians(MoonHP(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        
        If (SB < 0) Then
            SB = SB + TP
        End If
        
        XH = UTNM: X = MY: Y = BY: TM = XH - 1#: HP = HY
        GoSub 7390: MY = P: BY = Q
        X = MZ: Y = BZ: TM = XH + 1#: HP = HZ
        GoSub 7390: MZ = P: BZ = Q
        
        X0 = XH + 1# - (2# * BZ / (BZ - BY)): DM = MZ - MY
        
        If (DM < 0) Then
            DM = DM + TP
        End If
        
        LJ = (DM - SB) / 2#: Q = 0
        MR = MY + (DM * (X0 - XH + 1#) / 2#)
        UT = X0 - 0.13851852
        RR = SunDist(UT, 0, 0, 0, 0, IGDay, GMonth, GYear)
        SR = Radians(SunLong(UT, 0, 0, 0, 0, IGDay, GMonth, GYear))
        SR = SR + Radians(NutatLong(IGDay, GMonth, GYear) - 0.00569)
        X = SR: Y = 0: TM = UT
        HP = 0.00004263452 / RR: GoSub 7390: SR = P
        BY = BY - Q: BZ = BZ - Q: P3 = 0.00004263
        ZH = (SR - MR) / LJ: TC = X0 + ZH
        SH = (((BZ - BY) * (TC - XH - 1#) / 2#) + BZ) / LJ
        S2 = SH * SH: Z2 = ZH * ZH: PS = P3 / (RR * LJ)
        Z1 = (ZH * Z2 / (Z2 + S2)) + X0
        H0 = (HY + HZ) / (2# * LJ): RM = 0.272446 * H0
        RN = 0.00465242 / (LJ * RR): HD = H0 * 0.99834
        RU = (HD - RN + PS) * 1.02: RP = (HD + RN + PS) * 1.02
        PJ = Abs(SH * ZH / Sqr(S2 + Z2))
        R = RM + RN: DD = Z1 - X0: DD = DD * DD - ((Z2 - (R * R)) * DD / ZH)
        
        If (DD < 0) Then
            MG = -99#
            GoTo 7500
        End If
           
        ZD = Sqr(DD): Z6 = Z1 - ZD
        Z7 = Z1 + ZD - LINT((Z1 + ZD) / 24#) * 24#
        
        If (Z6 < 0) Then
            Z6 = Z6 + 24#
        End If
        
        MG = (RM + RN - PJ) / (2# * RN)
        GoTo 7500

7390    PAA = ECRA(Degrees(X), 0, 0, Degrees(Y), 0, 0, IGDay, GMonth, GYear)
        QAA = ECDec(Degrees(X), 0, 0, Degrees(Y), 0, 0, IGDay, GMonth, GYear)
        XAA = RAHA(DDDH(PAA), 0, 0, TM, 0, 0, 0, 0, IGDay, GMonth, GYear, GLong)
        PBB = ParallaxHA(XAA, 0, 0, QAA, 0, 0, "True", GLat, 0, Degrees(HP))
        QBB = ParallaxDec(XAA, 0, 0, QAA, 0, 0, "True", GLat, 0, Degrees(HP))
        XBB = HARA(PBB, 0, 0, TM, 0, 0, 0, 0, IGDay, GMonth, GYear, GLong)
        P = Radians(EQElong(XBB, 0, 0, QBB, 0, 0, IGDay, GMonth, GYear))
        Q = Radians(EQElat(XBB, 0, 0, QBB, 0, 0, IGDay, GMonth, GYear))
        Return

7500    MagSolarEclipse = MG

End Function

