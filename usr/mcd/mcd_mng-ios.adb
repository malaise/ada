with Text_Io;
with My_Math; use My_Math;
with Inte_Io, Real_Io, Bool_Io;
separate (Mcd_Mng)

package body Ios is 

  Inte_Format_Set : Boolean := False;
  Real_Format_Set : Boolean := False;

  procedure Set_Obase (Base : in Item_Rec) is
  begin
    if Base.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    Inte_Io.Default_Base := Text_Io.Number_Base(Base.Val_Inte);
  exception
    when others =>
      raise Invalid_Argument;
  end Set_Obase;

  procedure Format (Item : in Item_Rec) is
    R : My_Math.Real;
    I : My_Math.Inte;
  begin
    case Item.Kind is
      when Inte =>
        Inte_Io.Default_Width := Text_Io.Field (Item.Val_Inte);
        Inte_Format_Set := True;
      when Real =>
        R := My_Math.Int(Item.Val_Real);
        I := My_Math.Round(R);
        Real_Io.Default_Fore := Text_Io.Field(I);
        -- AFT 0 .. 999
        R := My_Math.Frac(Item.Val_Real) * 1000.0;
        I := My_Math.Round(R);
        Real_Io.Default_Aft := Text_Io.Field(I);
        -- EXP 3
        Real_Io.Default_Exp := 4;
        Real_Format_Set := True;
      when others =>
        raise Invalid_Argument;
    end case;
  exception
    when others =>
      raise Invalid_Argument;
  end Format;

  procedure Check_Default_Formats is
  begin
    if not Inte_Format_Set then
      Format ((Kind => Inte, Val_Inte => 5));
    end if;
    if not Real_Format_Set then
      Format ((Kind => Real, Val_Real => 5.003));
    end if;
  end Check_Default_Formats;
    
    
  procedure Put (Item : in Item_Rec) is
  begin
    Check_Default_Formats;

    case Item.Kind is
      when Inte =>
        Inte_Io.Put(Item.Val_Inte);
      when Real =>
        Real_Io.Put(Item.Val_Real);
      when Bool  =>
        if Item.Val_Bool then
          Text_Io.Put("True");
        else
          Text_Io.Put("False");
        end if;
      when Chrs =>
        if Item.Val_Text(1) = '"' then
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

