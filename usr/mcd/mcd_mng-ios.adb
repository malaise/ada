with TEXT_IO;
with MATH; use MATH;
with INTE_IO, REAL_IO, BOOL_IO;
separate (MCD_MNG)

package body IOS is 

  INTE_FORMAT_SET : BOOLEAN := FALSE;
  REAL_FORMAT_SET : BOOLEAN := FALSE;

  procedure FORMAT (ITEM : in ITEM_REC) is
    R : MATH.REAL;
    I : MATH.INTE;
  begin
    case ITEM.KIND is
      when INTE =>
        INTE_IO.DEFAULT_WIDTH := TEXT_IO.FIELD (ITEM.VAL_INTE);
        INTE_FORMAT_SET := TRUE;
      when REAL =>
        R := MATH.INT(ITEM.VAL_REAL);
        I := MATH.ROUND(R);
        REAL_IO.DEFAULT_FORE := TEXT_IO.FIELD(I);
        -- AFT 0 .. 999
        R := MATH.FRAC(ITEM.VAL_REAL) * 1000.0;
        I := MATH.ROUND(R);
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
          TEXT_IO.PUT(" True");
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

end IOS;

