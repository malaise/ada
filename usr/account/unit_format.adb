with TEXT_IO, CALENDAR;
with MY_MATH, NORMAL, EURO_FRANC;
package body UNIT_FORMAT is

  CURRENT_UNIT : UNITS_LIST := DEFAULT_UNIT;

  package AMOUNT_IO is new TEXT_IO.FLOAT_IO(OPER_DEF.AMOUNT_RANGE);
  package REAL_IO is new TEXT_IO.FLOAT_IO(MY_MATH.REAL);
  package INTE_IO is new TEXT_IO.INTEGER_IO(MY_MATH.INTE);

  package MEF is new EURO_FRANC(OPER_DEF.AMOUNT_RANGE, OPER_DEF.AMOUNT_RANGE);

  -- Date: 25/10/2001
  function DATE_IMAGE(DATE : OPER_DEF.DATE_REC) return DATE_STR is
  begin
    return NORMAL(DATE.DAY, 2, GAP => '0') & '/'
         & NORMAL(DATE.MONTH, 2, GAP => '0') & '/'
         & NORMAL(DATE.YEAR, 4, GAP => '0');
  end DATE_IMAGE;

  function DATE_VALUE(STR : DATE_STR) return OPER_DEF.DATE_REC is
    DATE : OPER_DEF.DATE_REC;
    TIME : CALENDAR.TIME;
  begin
    DATE.DAY   := CALENDAR.DAY_NUMBER'VALUE  (STR(1 ..  2));
    DATE.MONTH := CALENDAR.MONTH_NUMBER'VALUE(STR(4 ..  5));
    DATE.YEAR  := CALENDAR.YEAR_NUMBER'VALUE (STR(7 .. 10));
    -- Check validity
    TIME := CALENDAR.TIME_OF(DATE.YEAR, DATE.MONTH, DATE.DAY, 0.0);
    return DATE;
  exception
    when others =>
      raise FORMAT_ERROR;
  end DATE_VALUE;

  -- Short date: 25/10/01
  function SHORT_DATE_IMAGE(DATE : OPER_DEF.DATE_REC) return SHORT_DATE_STR is
  begin
    return NORMAL(DATE.DAY, 2, GAP => '0') & '/'
         & NORMAL(DATE.MONTH, 2, GAP => '0') & '/'
         & NORMAL(DATE.YEAR, 4, GAP => '0')(3..4);
  end SHORT_DATE_IMAGE;

  -- Short status: Yes No Def
  function SHORT_STATUS_IMAGE (STATUS : OPER_DEF.STATUS_LIST)
           return SHORT_STATUS_STR is
  begin
    case STATUS is
      when OPER_DEF.ENTERED =>
        return "Yes";
      when OPER_DEF.NOT_ENTERED =>
        return " No";
      when OPER_DEF.DEFERED =>
        return "Def";
    end case;
  end SHORT_STATUS_IMAGE;

  -- Short kind:  Cheq Card Tran Draw
  function SHORT_KIND_IMAGE (KIND : OPER_DEF.KIND_LIST)
           return SHORT_KIND_STR is
  begin
    case KIND is
      when OPER_DEF.CHEQUE =>
        return "Cheq";
      when OPER_DEF.CREDIT =>
        return "Cred";
      when OPER_DEF.TRANSFER =>
        return "Xfer";
      when OPER_DEF.WITHDRAW =>
        return "Draw";
    end case;
  end SHORT_KIND_IMAGE;

  -- Current unit switching
  function GET_CURRENT_UNIT return UNITS_LIST is
  begin
    return CURRENT_UNIT;
  end GET_CURRENT_UNIT;

  procedure SET_UNIT_TO (UNIT : UNITS_LIST) is
  begin
    CURRENT_UNIT := UNIT;
  end SET_UNIT_TO;

  -- Amount: -12345678.12
  -- subtype AMOUNT_STR is STRING (1 .. 12);
  -- From an amount (in euros) return 'image (euros/francs)
  function IMAGE (AMOUNT_IN_EUROS : OPER_DEF.AMOUNT_RANGE)
                 return AMOUNT_STR is
    STR : AMOUNT_STR;
    AMOUNT_IN_UNIT : OPER_DEF.AMOUNT_RANGE;
  begin
    if GET_CURRENT_UNIT = EUROS then
      AMOUNT_IN_UNIT := AMOUNT_IN_EUROS;
    else
      AMOUNT_IN_UNIT := MEF.EUROS_TO_FRANCS(AMOUNT_IN_EUROS);
    end if;
    AMOUNT_IO.PUT(STR, AMOUNT_IN_UNIT, 2, 0);
    return STR;
  exception
    when others =>
      raise FORMAT_ERROR;
  end IMAGE;

  -- From a string (euros/francs) return amount in euros
  function VALUE (STR : AMOUNT_STR) return OPER_DEF.AMOUNT_RANGE is
    AMOUNT_IN_UNIT : OPER_DEF.AMOUNT_RANGE;

    function FIRST_DIG return POSITIVE is
    begin
      for I in STR'RANGE loop
        if STR(I) = ' ' then return I; end if;
      end loop;
      raise FORMAT_ERROR;
    end FIRST_DIG;

    function LAST_DIG return POSITIVE is
    begin
      for I in reverse STR'RANGE loop
        if STR(I) = ' ' then return I; end if;
      end loop;
      raise FORMAT_ERROR;
    end LAST_DIG;

    function HAS_DOT (S : STRING) return BOOLEAN is
    begin
      for I in STR'RANGE loop
        if S(I) = '.' then return TRUE; end if;
      end loop;
      return FALSE;
    end HAS_DOT;
        
  begin
    -- Get amount or int from significant characters
    declare
      -- Strip blancs
      TMP : constant STRING := STR(FIRST_DIG .. LAST_DIG);
      I : MY_MATH.INTE;
      LAST : POSITIVE;
    begin
      if HAS_DOT(TMP) then
        AMOUNT_IO.GET(TMP, AMOUNT_IN_UNIT, LAST);
      else
        INTE_IO.GET(TMP, I, LAST);
        AMOUNT_IN_UNIT := OPER_DEF.AMOUNT_RANGE(I);
      end if;
      if LAST /= TMP'LAST then
        raise FORMAT_ERROR;
      end if;
    end;
    -- Convert if needed
    if GET_CURRENT_UNIT = EUROS then
      return AMOUNT_IN_UNIT;
    else
      return MEF.FRANCS_TO_EUROS(AMOUNT_IN_UNIT);
    end if;
  exception
    when others =>
      raise FORMAT_ERROR;
  end VALUE;

  -- Amount of an operation in LIST: -12345.12
   --subtype SHORT_AMOUNT_STR is STRING (1 .. 9);
  -- From an amount (in euros) return 'image (euros/francs)
  -- Truncation rule:
  --  Sign is kept, three lower digits of unit removed,
  --  cents and dot replaced by " k "
  function SHORT_IMAGE (AMOUNT_IN_EUROS : OPER_DEF.AMOUNT_RANGE)
                       return SHORT_AMOUNT_STR is
    STR : AMOUNT_STR;
    FIRST_DIG : POSITIVE;
  begin
    -- Get full string in proper unit
    STR := IMAGE(AMOUNT_IN_EUROS);

    -- Look for first digit
    for I in STR'RANGE loop
      if STR(I) /= ' ' then
        FIRST_DIG := I;
        exit;
      end if;
    end loop;

    -- Enough space?
    if FIRST_DIG < 4 then
      -- No
      declare
        LAST : constant := STR'LAST;
        DOT : constant := LAST -2;
        R : MY_MATH.REAL;
        L : POSITIVE;
        I : MY_MATH.INTE;
        use MY_MATH;
      begin
        -- Get INT value
        REAL_IO.GET(STR, R, L);
        R := R / 1000.0;
        I := MY_MATH.ROUND(R);
        STR (4 .. DOT-1) := NORMAL(INTEGER(I), DOT-4);
    
        -- Remove digits
        STR(DOT .. LAST) := " k ";
      end;
    end if; 
  
    return STR(4 .. STR'LAST);
  exception
    when others =>
      raise FORMAT_ERROR;
  end SHORT_IMAGE;

end UNIT_FORMAT;

