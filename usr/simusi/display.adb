with MY_IO, NORMAL, SYS_CALLS, ARG_PARSING;
package body DISPLAY is
  use COMMON;

  NOT_FOUND : exception;
  NB_COTE_LINE : constant := 12;

  OK : BOOLEAN;

  procedure PUT_VAL (VAL : in COMMON.POS_FLOAT) is
  begin
    MY_IO.PUT (VAL, FORE => 4, AFT => 3, EXP => 0);
  end PUT_VAL;

  procedure PUT_IT (IT : in COMMON.POS_FLOAT) is
  begin
    MY_IO.PUT (IT, FORE => 3, AFT => 3, EXP => 0);
  end PUT_IT;

  procedure SEARCH (KIND : in COMMON.COTE_KIND;
                    START, STOP : in DATA.EFF_LINE_RANGE;
                    COTE_NO : out DATA.EFF_COTE_RANGE;
                    PLUS : out BOOLEAN) is
  begin
    if KIND = COMMON.MANUFA then
      for I in DATA.EFF_COTE_RANGE loop
        if DATA.MANUFAS(I).START = START
        and then DATA.MANUFAS(I).STOP = STOP then
          COTE_NO := I;
          PLUS := TRUE;
          return;
        elsif DATA.MANUFAS(I).START = STOP
        and then DATA.MANUFAS(I).STOP = START then
          COTE_NO := I;
          PLUS := FALSE;
          return;
        end if;
      end loop;
      raise NOT_FOUND;
    else
      for I in DATA.EFF_COTE_RANGE loop
        if DATA.DESIGNS(I).START = START
        and then DATA.DESIGNS(I).STOP = STOP then
          COTE_NO := I;
          PLUS := TRUE;
          return;
        elsif DATA.DESIGNS(I).START = STOP
        and then DATA.DESIGNS(I).STOP = START then
          COTE_NO := I;
          PLUS := FALSE;
          return;
        end if;
      end loop;
      raise NOT_FOUND;
    end if;
  end SEARCH;

  function CHAR (KIND : in COMMON.COTE_KIND) return CHARACTER is
  begin
    if KIND = COMMON.MANUFA then
      return 'M';
    else
      return 'S';
    end if;
  end CHAR;

  procedure PRINT (KIND : in COMMON.COTE_KIND;
                   COTE : in DATA.EFF_COTE_RANGE;
                   WAY  : in WAY_VECTOR) is
    COTE_NO : DATA.EFF_COTE_RANGE;
    OTHER_KIND : COMMON.COTE_KIND;
    PLUS : BOOLEAN;
    NB_PRINTED : NATURAL := 0;
    VAL : FLOAT;
  begin
    if KIND = COMMON.MANUFA then
      OTHER_KIND := COMMON.DESIGN;
      OK := TRUE;
    else
      OTHER_KIND := COMMON.MANUFA;
    end if;
    MY_IO.PUT (CHAR(OTHER_KIND) & NORMAL(COTE, 3, TRUE, '0') & ": ");

    VAL := 0.0;
    for I in DATA.EFF_LINE_RANGE range 2 .. WAY'LAST loop
      SEARCH (KIND, WAY(I-1), WAY(I), COTE_NO, PLUS);
      if ARG_PARSING.VERBOSE then
        if NB_PRINTED >= NB_COTE_LINE
        and then (NB_PRINTED - 1) mod (NB_COTE_LINE - 1) = 0 then
          MY_IO.NEW_LINE;
          MY_IO.PUT ("      ");
        end if;
        if PLUS then
          MY_IO.PUT ("+");
        else
          MY_IO.PUT ("-");
        end if;
        MY_IO.PUT (CHAR(KIND) & NORMAL(COTE_NO, 3, TRUE, '0') & " ");
      end if;
      NB_PRINTED := NB_PRINTED + 1;
      if KIND = COMMON.MANUFA then
        -- Check INTER
        VAL := VAL + abs(DATA.MANUFAS(COTE_NO).INTER);
      else
        -- Add VALUE
        if PLUS then
          VAL := VAL + DATA.DESIGNS(COTE_NO).VALUE;
        else
          VAL := VAL - DATA.DESIGNS(COTE_NO).VALUE;
        end if;
      end if;
    end loop;
    if ARG_PARSING.VERBOSE then
      MY_IO.NEW_LINE;
    end if;

    MY_IO.PUT (" --> ");
    if KIND = COMMON.MANUFA then
      MY_IO.PUT ("IT specified: ");
      PUT_IT (DATA.DESIGNS(COTE).INTER);
      MY_IO.PUT("   IT done: ");
      PUT_IT (VAL);
      if VAL > DATA.DESIGNS(COTE).INTER then
        MY_IO.PUT_LINE (" NOT OK");
        OK := FALSE;
      else
        MY_IO.NEW_LINE;
      end if;
    else
      MY_IO.PUT ("Value : ");
      PUT_VAL (VAL); 
      MY_IO.PUT (" +/- ");
      PUT_IT (DATA.MANUFAS(COTE).INTER);
      MY_IO.NEW_LINE;
    end if;
    
  end PRINT;

  procedure PUT_NO_WAY (KIND : in COMMON.COTE_KIND;
                        COTE : in DATA.EFF_COTE_RANGE) is
  begin
    SYS_CALLS.PUT_ERROR ("No way for ");
    SYS_CALLS.PUT_LINE_ERROR (CHAR(KIND) & NORMAL(COTE, 3, TRUE, '0'));
  end PUT_NO_WAY;

  procedure PRINT_TITTLE (KIND : in COMMON.COTE_KIND) is
  begin
    if KIND = COMMON.MANUFA then
      MY_IO.PUT_LINE ("Check of feasibility");
      MY_IO.PUT_LINE ("--------------------");
    else
      MY_IO.PUT_LINE ("Resolution");
      MY_IO.PUT_LINE ("----------");
    end if;
  end PRINT_TITTLE;

  procedure PRINT_RESULT is
  begin
    if OK then
      MY_IO.PUT_LINE ("Simulation successfull.");
    else
      MY_IO.PUT_LINE ("Simulation FAILED.");
    end if;
  end PRINT_RESULT;

end DISPLAY;
