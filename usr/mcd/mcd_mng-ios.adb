with TEXT_IO;
with MY_MATH; use MY_MATH;
with INTE_IO, REAL_IO, BOOL_IO;
separate (MCD_MNG)

package body IOS is 

  INTE_FORMAT_SET : BOOLEAN := FALSE;
  REAL_FORMAT_SET : BOOLEAN := FALSE;

  procedure SET_OBASE (BASE : in ITEM_REC) is
  begin
    if BASE.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
    INTE_IO.DEFAULT_BASE := TEXT_IO.NUMBER_BASE(BASE.VAL_INTE);
  exception
    when others =>
      raise INVALID_ARGUMENT;
  end SET_OBASE;

  procedure FORMAT (ITEM : in ITEM_REC) is
    R : MY_MATH.REAL;
    I : MY_MATH.INTE;
  begin
    case ITEM.KIND is
      when INTE =>
        INTE_IO.DEFAULT_WIDTH := TEXT_IO.FIELD (ITEM.VAL_INTE);
        INTE_FORMAT_SET := TRUE;
      when REAL =>
        R := MY_MATH.INT(ITEM.VAL_REAL);
        I := MY_MATH.ROUND(R);
        REAL_IO.DEFAULT_FORE := TEXT_IO.FIELD(I);
        -- AFT 0 .. 999
        R := MY_MATH.FRAC(ITEM.VAL_REAL) * 1000.0;
        I := MY_MATH.ROUND(R);
        REAL_IO.DEFAULT_AFT := TEXT_IO.FIELD(I);
        -- EXP 3
        REAL_IO.DEFAULT_EXP := 4;
        REAL_FORMAT_SET := TRUE;
      when others =>
        raise INVALID_ARGUMENT;
    end case;
  exception
    when others =>
      raise INVALID_ARGUMENT;
  end FORMAT;

  procedure CHECK_DEFAULT_FORMATS is
  begin
    if not INTE_FORMAT_SET then
      FORMAT ((KIND => INTE, VAL_INTE => 5));
    end if;
    if not REAL_FORMAT_SET then
      FORMAT ((KIND => REAL, VAL_REAL => 5.003));
    end if;
  end CHECK_DEFAULT_FORMATS;
    
    
  procedure PUT (ITEM : in ITEM_REC) is
  begin
    CHECK_DEFAULT_FORMATS;

    case ITEM.KIND is
      when INTE =>
        INTE_IO.PUT(ITEM.VAL_INTE);
      when REAL =>
        REAL_IO.PUT(ITEM.VAL_REAL);
      when BOOL  =>
        if ITEM.VAL_BOOL then
          TEXT_IO.PUT("True");
        else
          TEXT_IO.PUT("False");
        end if;
      when CHRS =>
        if ITEM.VAL_TEXT(1) = '"' then
          TEXT_IO.PUT (ITEM.VAL_TEXT(2 .. ITEM.VAL_LEN - 1));
        else
          TEXT_IO.PUT (ITEM.VAL_TEXT(1 .. ITEM.VAL_LEN));
        end if;
      when others =>
        raise INVALID_ARGUMENT;
    end case;
  end PUT;
    
  procedure PUT_LINE (ITEM : in ITEM_REC) is
  begin
    PUT(ITEM);
    NEW_LINE;
  end PUT_LINE;

  procedure NEW_LINE is
  begin
    TEXT_IO.NEW_LINE;
  end NEW_LINE;

  function STRREAL (S : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(REAL);
    LAST : POSITIVE;
  begin
    if S.KIND /= CHRS then
      raise INVALID_ARGUMENT;
    end if;
    REAL_IO.GET(S.VAL_TEXT(1 .. S.VAL_LEN), RES.VAL_REAL, LAST);
    if LAST /= S.VAL_LEN then
      raise ARGUMENT_MISMATCH;
    end if;
    return RES;
  end STRREAL;

  function STRINTE (S : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(INTE);
    LAST : POSITIVE;
  begin
    if S.KIND /= CHRS then
      raise INVALID_ARGUMENT;
    end if;
    INTE_IO.GET(S.VAL_TEXT(1 .. S.VAL_LEN), RES.VAL_INTE, LAST);
    if LAST /= S.VAL_LEN then
      raise ARGUMENT_MISMATCH;
    end if;
    return RES;
  end STRINTE;

  function STRBOOL (S : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(BOOL);
    LAST : POSITIVE;
  begin
    if S.KIND /= CHRS then
      raise INVALID_ARGUMENT;
    end if;
    BOOL_IO.GET(S.VAL_TEXT(1 .. S.VAL_LEN), RES.VAL_BOOL, LAST);
    if LAST /= S.VAL_LEN then
      raise ARGUMENT_MISMATCH;
    end if;
    return RES;
  end STRBOOL;
    
  function STROF (ITEM : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(CHRS);

    -- String is at the end of RES
    -- Move it after some spaces at the beginning
    --  so that the whoe takes SIZE characters
    procedure FIX_SIZE (SIZE : in TEXT_IO.FIELD) is
      FIRST : NATURAL := 0;
      LEN   : POSITIVE;
    begin
      for I in reverse RES.VAL_TEXT'RANGE loop
        if RES.VAL_TEXT(I) = ' ' then
          FIRST := I + 1;
          exit;
        end if;
      end loop;
      if FIRST = 0 then
        RES.VAL_LEN := 0;
        return;
      end if;
      LEN := RES.VAL_TEXT'LAST - FIRST + 1;
      if LEN >= SIZE then
        RES.VAL_LEN := LEN;
        RES.VAL_TEXT(1 .. LEN) := RES.VAL_TEXT(FIRST .. RES.VAL_TEXT'LAST);
      else
        RES.VAL_LEN := SIZE;
        RES.VAL_TEXT(SIZE - LEN + 1 .. SIZE)
            := RES.VAL_TEXT(FIRST .. RES.VAL_TEXT'LAST);
      end if;
    end FIX_SIZE;

  begin
    CHECK_DEFAULT_FORMATS;

    case ITEM.KIND is
      when INTE =>
        RES.VAL_TEXT := (others => ' ');
        INTE_IO.PUT(RES.VAL_TEXT, ITEM.VAL_INTE);
        FIX_SIZE (INTE_IO.DEFAULT_WIDTH);
      when REAL =>
        RES.VAL_TEXT := (others => ' ');
        REAL_IO.PUT(RES.VAL_TEXT, ITEM.VAL_REAL);
         FIX_SIZE (REAL_IO.DEFAULT_FORE + 1 + REAL_IO.DEFAULT_AFT
                   + REAL_IO.DEFAULT_EXP);
      when BOOL  =>
        if ITEM.VAL_BOOL then
          RES.VAL_LEN := 4;
          RES.VAL_TEXT(1 .. RES.VAL_LEN) := "True";
        else
          RES.VAL_LEN := 5;
          RES.VAL_TEXT(1 .. RES.VAL_LEN) := "False";
        end if;
      when CHRS =>
        RES := ITEM;
      when others =>
        raise INVALID_ARGUMENT;
    end case;
    return RES;
  end STROF;

end IOS;

