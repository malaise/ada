with FLO_IO, INT_IO, IO_EXCEPTIONS;
package body GET_FLOAT is

  function GET_FLOAT (STR : STRING) return FLOAT is
    INT_FLOAT : INT_FLOAT_REC;
  begin
    INT_FLOAT := GET_INT_FLOAT(STR);
    if INT_FLOAT.IS_FLOAT then
      return INT_FLOAT.FLOAT_VALUE;
    else
      return FLOAT(INT_FLOAT.INT_VALUE);
    end if;
  end GET_FLOAT;

  function GET_INT_FLOAT (STR : STRING) return INT_FLOAT_REC is
    F : FLOAT;
    L : POSITIVE;
    I : INTEGER;
    STR_LEN : NATURAL;
    GOT_A_FLOAT : BOOLEAN;
    DOT_FOUND : BOOLEAN;
  begin
    -- Locate last significant character of STR
    STR_LEN := 0;
    DOT_FOUND := FALSE;
    for J in reverse STR'RANGE loop
      if STR_LEN = 0 and then STR(J) /= ' ' then
        STR_LEN := J + 1 - STR'FIRST;
      end if;
      if STR(J) = '.' then
        DOT_FOUND := TRUE;
      end if;
    end loop;
    if STR_LEN = 0 then
      raise CONSTRAINT_ERROR;
    end if; 

    GOT_A_FLOAT := DOT_FOUND;
    if DOT_FOUND then
      -- Float format
      FLO_IO.GET(STR, F, L);
    else
      -- Int format
      INT_IO.GET(STR, I, L);
    end if;


    if L /= STR'LAST then
      raise CONSTRAINT_ERROR;
    end if;

    if GOT_A_FLOAT then
      return (IS_FLOAT => TRUE, FLOAT_VALUE => F);
    else
      return (IS_FLOAT => FALSE, INT_VALUE => I);
    end if;

  exception
    when others =>
      raise CONSTRAINT_ERROR;
  end GET_INT_FLOAT;

end GET_FLOAT;

