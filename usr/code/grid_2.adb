with MATH;
package body GRID_2 is

  subtype LONG_NATURAL is LONG_INTEGER range 0 .. LONG_INTEGER'LAST;

  DIMENSION : LONG_INTEGER;
  FIRST_ROW : LONG_INTEGER;
  FIRST_COL : LONG_INTEGER;
  LAST_ROW : LONG_INTEGER;
  LAST_COL : LONG_INTEGER;

  procedure INITIALIZE (KEY_LENGTH, TEXT_LENGTH : in LONG_INTEGER) is
    R : LONG_FLOAT;

  -- Real -> integer : round or trunc
  function TRUNC (X : in LONG_FLOAT) return LONG_INTEGER is
    INT : LONG_INTEGER;
  begin
    INT := LONG_INTEGER (X);
    -- Adjust to 1
    if X > 0.0 then
      -- If x>0 error is 1 too much
      if LONG_FLOAT (INT) > X then INT := INT - 1; end if;
      return INT;
    else
      -- If x<0 error is 1 too less
      if LONG_FLOAT (INT) < X then INT := INT + 1; end if;
      return INT;
    end if;
  exception
    when others => raise CONSTRAINT_ERROR;
  end TRUNC;
  begin
    R := LONG_FLOAT(KEY_LENGTH) + LONG_FLOAT(TEXT_LENGTH);
    DIMENSION := LONG_INTEGER(TRUNC(MATH.SQRT(R))) + 1;
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

  function INDEX (R, C : LONG_INTEGER; ENCODE : BOOLEAN)
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
