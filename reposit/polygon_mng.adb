package body POLYGON_MNG is

  procedure BELONG_TO_AREA (POLYGON        : in FLOAT_POINTS_ARRAY;
                            POINT_TO_CHECK : in FLOAT_POINT_REC;
                            ACCURACY       : in FLOAT;
                            RESULT         : out BELONGING_RESULTS) is
    

    ISI, ISF, ISC          : INTEGER;
    A1, A2, A3, B1, B2, B3 : FLOAT;
    P, C, E1, E2           : FLOAT;
    D, Y1                  : FLOAT;
    ACCURACY_2             : FLOAT;
    ITI                    : INTEGER;
    TEMP_RESULT            : BELONGING_RESULTS;

  begin
    -- At least three points are required to define a polygon
    if POLYGON'LENGTH < 3 then
      raise NOT_A_POLYGON;
    end if;

    -- Check to see if the point is inside or outside of the polygon
    TEMP_RESULT := OUT_OF_AREA;
    ITI := 0;
    ACCURACY_2 := ACCURACY ** 2;
    ISF := 0;
    A1 := POLYGON(POLYGON'FIRST).X - POINT_TO_CHECK.X;
    B1 := POLYGON(POLYGON'FIRST).Y - POINT_TO_CHECK.Y;
    if A1 > 0.0 then
      ISF := 1;
    elsif A1 < 0.0 then
      ISF := -1;
    end if;
    ISI := ISF;
    ISC := ISF;
    for I in POLYGON'RANGE loop
      if I /= POLYGON'LAST then
        A2 := POLYGON(I + 1).X - POINT_TO_CHECK.X;
        B2 := POLYGON(I + 1).Y - POINT_TO_CHECK.Y;
      else
        -- last segment : the second point of the segment is the first point of
        --                the polygon
        A2 := POLYGON(POLYGON'FIRST).X - POINT_TO_CHECK.X;
        B2 := POLYGON(POLYGON'FIRST).Y - POINT_TO_CHECK.Y;
      end if;
      A3 := A2 - A1;
      B3 := B2 - B1;
      P := A2 * B1 - A1 * B2;
      C := A3 ** 2 + B3 ** 2;
      E1 := A1 ** 2 + B1 ** 2;
      E2 := A2 ** 2 + B2 ** 2;
      if E1 <= ACCURACY_2 or else E2 <= ACCURACY_2 then
        TEMP_RESULT := SUMMIT;
        exit;
      end if;
      if C + E1 > E2 and then C + E2 > E1 then
        D := P ** 2 / C;
        if D <= ACCURACY_2 then
          TEMP_RESULT := BOUNDARY;
          exit;
        end if;
      end if;
      if A2 > 0.0 then
        ISF := 1;
      elsif A2 < 0.0 then
        ISF := -1;
      end if;
      --
      if ISI = 0 then
        ISI := ISF;
      end if;
      A1 := A2;
      B1 := B2;
      if ISC /= ISF then
        if ISC /= 0 then
          Y1 := P / A3;
          if Y1 > 0.0 then
            ITI := ITI + 1;
          end if;
        end if;
        ISC := ISF;
      end if;

    end loop;

    if TEMP_RESULT = OUT_OF_AREA then
      if ISF /= ISI then
        if B2 > 0.0 then
          ITI := ITI + 1;
        end if;
      end if;
      if ITI /= ITI / 2 * 2 then
        TEMP_RESULT := INSIDE_AREA;
      end if;
    end if;
    RESULT := TEMP_RESULT;

  end BELONG_TO_AREA;

  function "-" (PL, PR : FLOAT_POINT_REC) return FLOAT_POINT_REC is
  begin
    return (X => PL.X - PR.X, Y=> PL.Y - PR.Y);
  end "-";

  procedure SEGMENTS_INTERSECT (PS1, QS1, PS2, QS2 : in  FLOAT_POINT_REC;
                                INTERSECT          :    out BOOLEAN;
                                I_POINT            :    out FLOAT_POINT_REC) is

    DET                   : FLOAT;
    COEF1, COEF2          : FLOAT;
    VECT1, VECT2, VECTQ12 : FLOAT_POINT_REC;

  begin
    VECT1 := PS1 - QS1;
    VECT2 := PS2 - QS2;
    I_POINT := (X => 0.0, Y => 0.0);
    DET := VECT1.Y * VECT2.X - VECT1.X * VECT2.Y;

    -- If the determinant is null (segments parallel)
    if abs DET < 0.5 then
      INTERSECT := FALSE;
    else
      VECTQ12 := QS1 - QS2;
      COEF1 := (VECT2.Y * VECTQ12.X - VECT2.X * VECTQ12.Y) / DET;
      COEF2 := (VECT1.Y * VECTQ12.X - VECT1.X * VECTQ12.Y) / DET;
      if       COEF1 < 1.0 and then COEF1 > 0.0
      and then COEF2 < 1.0 and then COEF2 > 0.0 then
        INTERSECT := TRUE;
        I_POINT.X := VECT1.X * COEF1 + QS1.X;
        I_POINT.Y := VECT1.Y * COEF1 + QS1.Y;
      else
        INTERSECT := FALSE;
      end if;
    end if;

  end SEGMENTS_INTERSECT;

  function IS_CONNEXE (POLYGON : FLOAT_POINTS_ARRAY) return BOOLEAN is

    INTERSECT : BOOLEAN;
    I_POINT   : FLOAT_POINT_REC;

  begin
    -- At least three points are required to define a polygon
    if POLYGON'LENGTH < 3 then
      raise NOT_A_POLYGON;
    end if;

    for NEXT_POINT_NB in POLYGON'FIRST + 2 .. POLYGON'LAST - 1 loop
      for PREVIOUS_POINT_NB in POLYGON'FIRST .. NEXT_POINT_NB - 2 loop
        SEGMENTS_INTERSECT (PS1       => POLYGON(NEXT_POINT_NB),
                            QS1       => POLYGON(NEXT_POINT_NB + 1),
                            PS2       => POLYGON(PREVIOUS_POINT_NB),
                            QS2       => POLYGON(PREVIOUS_POINT_NB + 1),
                            INTERSECT => INTERSECT,
                            I_POINT   => I_POINT);
        if INTERSECT then
          return FALSE;
        end if;
      end loop;
    end loop;

    for POINT_NB in POLYGON'FIRST + 1 .. POLYGON'LAST - 2 loop
      SEGMENTS_INTERSECT (PS1       => POLYGON(POLYGON'LAST),
                          QS1       => POLYGON(POLYGON'FIRST),
                          PS2       => POLYGON(POINT_NB),
                          QS2       => POLYGON(POINT_NB + 1),
                          INTERSECT => INTERSECT,
                          I_POINT   => I_POINT);
      if INTERSECT then
        return FALSE;
      end if;
    end loop;

    return TRUE;

  end IS_CONNEXE;

  function TO_FLOAT (POINT : INT_POINT_REC) return FLOAT_POINT_REC is
  begin
    return (X => FLOAT(POINT.X), Y => FLOAT(POINT.Y));
  end TO_FLOAT;

  procedure TO_FLOAT (INT_POLYGON : in INT_POINTS_ARRAY; FLOAT_POLYGON : out FLOAT_POINTS_ARRAY) is
    J : POSITIVE;
  begin
    for I in INT_POLYGON'RANGE loop
      J := I - INT_POLYGON'FIRST + FLOAT_POLYGON'FIRST;
      FLOAT_POLYGON (J) := TO_FLOAT(INT_POLYGON(I));
    end loop;
  end TO_FLOAT;
  
  procedure BELONG_TO_AREA (POLYGON        : in INT_POINTS_ARRAY;
                            POINT_TO_CHECK : in INT_POINT_REC;
                            ACCURACY       : in FLOAT;
                            RESULT         : out BELONGING_RESULTS) is
    FLOAT_POLYGON : FLOAT_POINTS_ARRAY(1 .. POLYGON'LENGTH);
    FLOAT_POINT_TO_CHECK : FLOAT_POINT_REC;
  begin
    TO_FLOAT (POLYGON, FLOAT_POLYGON);
    FLOAT_POINT_TO_CHECK := TO_FLOAT (POINT_TO_CHECK);
    BELONG_TO_AREA (FLOAT_POLYGON, FLOAT_POINT_TO_CHECK, ACCURACY, RESULT);
  end  BELONG_TO_AREA;

  function IS_CONNEXE (POLYGON : INT_POINTS_ARRAY) return BOOLEAN is
    FLOAT_POLYGON : FLOAT_POINTS_ARRAY(1 .. POLYGON'LENGTH);
  begin
    TO_FLOAT (POLYGON, FLOAT_POLYGON);
    return IS_CONNEXE (FLOAT_POLYGON);
  end IS_CONNEXE;

end POLYGON_MNG;

