with TEXT_HANDLER;
with MY_IO;
with SORTS;
package body GRID_1 is


  DATA : array (ROW_COORDINATE, COL_COORDINATE) of CHARACTER
       := (others => (others => ASCII.NUL));
  INITIALIZED : BOOLEAN := FALSE;

  package CHAR_SORT is new SORTS (CHARACTER, POSITIVE, "<", STRING);

  -- Return a valid character for text
  function FILTER (C : CHARACTER) return CHARACTER is
  begin
    if (C >= ' ' and then C <= '~') or else C = ASCII.CR then
      return C;
    elsif C = ASCII.HT then
      return ' ';
    else
      return ASCII.NUL;
    end if;
  end FILTER;

  -- Init DATA
  procedure INITIALIZE (KEY : in string) is
    ROW : ROW_COORDINATE;
    COL : COL_COORDINATE;
    CHAR : CHARACTER;
    STRIPPED_KEY : TEXT_HANDLER.TEXT (80);

    -- Store a char in data, checking if it is in STRIPPED_KEY
    procedure STORE (CHAR : in CHARACTER; CHECK : in BOOLEAN) is
    begin
      if CHECK and then TEXT_HANDLER.LOCATE (STRIPPED_KEY, CHAR) /= 0 then
        return;
      end if;
      DATA (ROW, COL) := CHAR;
      if COL /= COL_COORDINATE'LAST then
        COL := COL_COORDINATE'SUCC(COL);
      else
        COL := COL_COORDINATE'FIRST;
        ROW := ROW_COORDINATE'SUCC(ROW);
      end if;
    end STORE;

  begin
    TEXT_HANDLER.SET (STRIPPED_KEY, "");
    -- Store stripped KEY
    for i in KEY'RANGE loop
      CHAR := FILTER(KEY(I));
      if CHAR /= ASCII.NUL then
        if TEXT_HANDLER.LOCATE (STRIPPED_KEY, CHAR) = 0 then
          TEXT_HANDLER.APPEND (STRIPPED_KEY, CHAR);
        end if;
      end if;
    end loop;

    -- Sort characters of Key
    declare
      SORTED_KEY : STRING (1 .. TEXT_HANDLER.LENGTH(STRIPPED_KEY))
                 := TEXT_HANDLER.VALUE(STRIPPED_KEY);
    begin
      CHAR_SORT.BUBBLE_SORT (SORTED_KEY);

      -- Store stripped key then other chars in data
      ROW := ROW_COORDINATE'FIRST;
      COL := COL_COORDINATE'FIRST;
      for I in SORTED_KEY'RANGE loop
        STORE (SORTED_KEY(I), FALSE);
      end loop;
    end;
    STORE (ASCII.CR, TRUE);
    for C in CHARACTER'(' ') .. '~' loop
      STORE (C, TRUE);
    end loop;
    INITIALIZED := TRUE;
  end INITIALIZE;


  -- C can be any char from ' ' to '~' or ASCII.CR
  function ENCODE (C : CHARACTER) return COORDINATE_REC is
    SC : constant CHARACTER := FILTER(C);
  begin
    if not INITIALIZED then
      raise GRID_NOT_INIT;
    end if;
    if SC = ASCII.NUL then
      raise INVALID_CHARACTER;
    end if;
    for ROW in ROW_COORDINATE loop
      for COL in COL_COORDINATE loop
        if DATA (ROW, COL) = SC then
          return (ROW, COL);
        end if;
      end loop;
    end loop;
    raise INVALID_CHARACTER;
  end ENCODE;

  function DECODE (COORDINATE : COORDINATE_REC) return CHARACTER is
  begin
    if not INITIALIZED then
      raise GRID_NOT_INIT;
    end if;
    return DATA (COORDINATE.ROW, COORDINATE.COL);
  end DECODE;

  procedure DUMP is
  begin
    if not INITIALIZED then
      raise GRID_NOT_INIT;
    end if;
    for R in ROW_COORDINATE loop
      for C in COL_COORDINATE loop
        if DATA(R, C) = ASCII.CR then
          MY_IO.PUT ("Ret");
        elsif DATA(R, C) /= ASCII.NUL then
          MY_IO.PUT('>' & DATA(R, C) & '<');
        elsif DATA(R, C) = ASCII.NUL then
          MY_IO.PUT ("Nul");
        else
          raise CONSTRAINT_ERROR;
        end if;
        MY_IO.PUT (' ');
      end loop;
      MY_IO.NEW_LINE;
    end loop;
  end DUMP;

end GRID_1;
