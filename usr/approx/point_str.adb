with TEXT_IO;
package body POINT_STR is

  package COO_IO is new TEXT_IO.FLOAT_IO(POINTS.P_T_COORDINATE);
  package COO_INT_IO is new TEXT_IO.INTEGER_IO(INTEGER);

  function COORDINATE_IMAGE (COORDINATE : POINTS.P_T_COORDINATE)
                            return COORDINATE_STRING is
    STR : COORDINATE_STRING;
  begin
    COO_IO.PUT (STR, COORDINATE, 8, 4);
    return STR;
  end COORDINATE_IMAGE;

  -- May raise CONSTRAINT_ERROR
  function COORDINATE_VALUE (STR : STRING) return POINTS.P_T_COORDINATE is
    C : POINTS.P_T_COORDINATE;
    L : POSITIVE;
    I : INTEGER;
    STR_LEN : NATURAL;
  begin

    -- Locate last significant character of STR
    STR_LEN := 0;
    for J in reverse STR'RANGE loop
      if STR(J) /= ' ' then
        STR_LEN := J + 1 - STR'FIRST;
        exit;
      end if;
    end loop;
    if STR_LEN = 0 then
      raise CONSTRAINT_ERROR;
    end if;

    TRY_TO_CONVERT:
    begin
      -- Float format
      COO_IO.GET(STR, C, L);
    exception
      when TEXT_IO.DATA_ERROR =>
         -- Int format
        COO_INT_IO.GET(STR, I, L);
        C := POINTS.P_T_COORDINATE (I);
    end TRY_TO_CONVERT;

    if L /= STR'LAST then
      raise CONSTRAINT_ERROR;
    else
      return C;
    end if;
    exception
      when others =>
        raise CONSTRAINT_ERROR;
  end COORDINATE_VALUE;



  function ENCODE_REC (POINT : POINTS.P_T_ONE_POINT) return AFPX.LINE_REC is
    REC : AFPX.LINE_REC;
  begin
    REC.LEN := 2 * COORDINATE_STRING_LEN + 1;
    REC.STR (1 .. REC.LEN) := COORDINATE_IMAGE(POINT.X) & " " & COORDINATE_IMAGE(POINT.Y);
    return REC;
  end ENCODE_REC;

  function DECODE_REC (REC : AFPX.LINE_REC) return POINTS.P_T_ONE_POINT is
  begin
    return (X => COORDINATE_VALUE(REC.STR(1 .. COORDINATE_STRING_LEN)),
            Y => COORDINATE_VALUE(REC.STR(
          COORDINATE_STRING_LEN + 2 ..  2 * COORDINATE_STRING_LEN + 1)) );
  end DECODE_REC;


end POINT_STR;
