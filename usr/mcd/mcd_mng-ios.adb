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
          Text_Io.Put (Item.Val_Text(2 .. Item.Val_Len - 1));
        else
          Text_Io.Put (Item.Val_Text(1 .. Item.Val_Len));
        end if;
      when others =>
        raise Invalid_Argument;
    end case;
  end Put;
    
  procedure Put_Line (Item : in Item_Rec) is
  begin
    Put(Item);
    New_Line;
  end Put_Line;

  procedure New_Line is
  begin
    Text_Io.New_Line;
  end New_Line;

  function Strreal (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Real);
    Last : Positive;
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Real_Io.Get(S.Val_Text(1 .. S.Val_Len), Res.Val_Real, Last);
    if Last /= S.Val_Len then
      raise Argument_Mismatch;
    end if;
    return Res;
  end Strreal;

  function Strinte (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Inte);
    Last : Positive;
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Inte_Io.Get(S.Val_Text(1 .. S.Val_Len), Res.Val_Inte, Last);
    if Last /= S.Val_Len then
      raise Argument_Mismatch;
    end if;
    return Res;
  end Strinte;

  function Strbool (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Bool);
    Last : Positive;
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Bool_Io.Get(S.Val_Text(1 .. S.Val_Len), Res.Val_Bool, Last);
    if Last /= S.Val_Len then
      raise Argument_Mismatch;
    end if;
    return Res;
  end Strbool;
    
  function Strof (Item : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);

    -- String is at the end of RES
    -- Move it after some spaces at the beginning
    --  so that the whoe takes SIZE characters
    procedure Fix_Size (Size : in Text_Io.Field) is
      First : Natural := 0;
      Len   : Positive;
    begin
      for I in reverse Res.Val_Text'Range loop
        if Res.Val_Text(I) = ' ' then
          First := I + 1;
          exit;
        end if;
      end loop;
      if First = 0 then
        Res.Val_Len := 0;
        return;
      end if;
      Len := Res.Val_Text'Last - First + 1;
      if Len >= Size then
        Res.Val_Len := Len;
        Res.Val_Text(1 .. Len) := Res.Val_Text(First .. Res.Val_Text'Last);
      else
        Res.Val_Len := Size;
        Res.Val_Text(Size - Len + 1 .. Size)
            := Res.Val_Text(First .. Res.Val_Text'Last);
      end if;
    end Fix_Size;

  begin
    Check_Default_Formats;

    case Item.Kind is
      when Inte =>
        Res.Val_Text := (others => ' ');
        Inte_Io.Put(Res.Val_Text, Item.Val_Inte);
        Fix_Size (Inte_Io.Default_Width);
      when Real =>
        Res.Val_Text := (others => ' ');
        Real_Io.Put(Res.Val_Text, Item.Val_Real);
         Fix_Size (Real_Io.Default_Fore + 1 + Real_Io.Default_Aft
                   + Real_Io.Default_Exp);
      when Bool  =>
        if Item.Val_Bool then
          Res.Val_Len := 4;
          Res.Val_Text(1 .. Res.Val_Len) := "True";
        else
          Res.Val_Len := 5;
          Res.Val_Text(1 .. Res.Val_Len) := "False";
        end if;
      when Chrs =>
        Res := Item;
      when others =>
        raise Invalid_Argument;
    end case;
    return Res;
  end Strof;

end Ios;

