package body GRID_2 is
  use MY_MATH;

  subtype LONG_NATURAL is MY_MATH.INTE range 0 .. MY_MATH.INTE'LAST;

  DIMENSION : MY_MATH.INTE;
  FIRST_ROW : MY_MATH.INTE;
  FIRST_COL : MY_MATH.INTE;
  LAST_ROW : MY_MATH.INTE;
  LAST_COL : MY_MATH.INTE;

  procedure INITIALIZE (KEY_LENGTH, TEXT_LENGTH : in MY_MATH.INTE) is
    R : MY_MATH.REAL;

  begin
    R := MY_MATH.REAL(KEY_LENGTH) + MY_MATH.REAL(TEXT_LENGTH);
    DIMENSION := MY_MATH.INTE(TRUNC(MY_MATH.SQRT(R))) + 1;
    FIRST_ROW := KEY_LENGTH / DIMENSION + 1;
    FIRST_COL := KEY_LENGTH mod DIMENSION + 1;
    LAST_ROW :=
         KEY_LENGTH / DIMENSION
       + TEXT_LENGTH / DIMENSION
       + (KEY_LENGTH mod DIMENSION + TEXT_LENGTH mod DIMENSION) / DIMENSION
       + 1;
    LAST_COL :=
         (KEY_LENGTH mod DIMENSION + TEXT_LENGTH mod DIMENSION) mod DIMENSION
       + 1;
    -- So far , last row, col is first empty slot
    if LAST_COL /= 1 then
      LAST_COL := LAST_COL - 1;
    else
      LAST_ROW := LAST_ROW - 1;
      LAST_COL := DIMENSION;
    end if;
  end INITIALIZE;

  function INDEX (R, C : MY_MATH.INTE; ENCODE : BOOLEAN)
                 return LONG_NATURAL is
    N : LONG_NATURAL;
  begin
    if R < FIRST_ROW then
      return 0;
    elsif R = FIRST_ROW and then C < FIRST_COL then
      return 0;
    elsif R = LAST_ROW and then C > LAST_COL then
      return 0;
    elsif R > LAST_ROW then
      return 0;
    else
      -- In effective table
      if ENCODE then
        -- Substract key offset
        return (R - FIRST_ROW) * DIMENSION + C - FIRST_COL + 1;
      else
        N := 0;
        for I in 1 .. C - 1 loop
          -- General column amount
          N := N - (FIRST_ROW - 1) + LAST_ROW;
          if I < FIRST_COL then
            -- Before first col, so -1
            N := N - 1;
          end if;
          if I > LAST_COL then
            -- After last col, so -1
            N := N - 1;
          end if;
        end loop;
        -- Add key offset (raow part)
        N := N + R - FIRST_ROW;
        if C < FIRST_COL then
          -- Current col is before first col, so -1
          N := N - 1;
        end if;
        -- Always add 1
        N := N + 1;
        return N;
      end if;
    end if;
  end INDEX;


  function ENCODE (KEY : in STRING; TEXT : LONG_STRING)
           return LONG_STRING is
    STR : LONG_STRING (1 .. TEXT'LENGTH);
    I : LONG_POSITIVE;
    J : LONG_NATURAL;
  begin
    INITIALIZE (KEY'LENGTH, TEXT'LENGTH);

    I := 1;
    for C in 1 .. DIMENSION loop
      for R in 1 .. DIMENSION loop
        J := INDEX (R, C, TRUE);
        if J /= 0 then
          STR(I) := TEXT(J);
          I := I + 1;
        end if;
      end loop;
    end loop;
    return STR;
  end ENCODE;

  function DECODE (KEY : in STRING; TEXT : LONG_STRING)
           return LONG_STRING is
    STR : LONG_STRING (1 .. TEXT'LENGTH);
    I : LONG_POSITIVE;
    J : LONG_NATURAL;
  begin
    INITIALIZE (KEY'LENGTH, TEXT'LENGTH);

    I := 1;
    for R in 1 .. DIMENSION loop
      for C in 1 .. DIMENSION loop
        J := INDEX (R, C, FALSE);
        if J /= 0 then
          STR(I) := TEXT(J);
          I := I + 1;
        end if;
      end loop;
    end loop;
    return STR;
  end DECODE;

end GRID_2;
