with Ada.Text_Io;
with Normal, Bool_Io, Mixed_Str, String_Mng;
with Inte_Io, Real_Io, Io_Flow;
separate (Mcd_Mng)

package body Ios is

  use My_Math;

  -- Max len for int/real image
  Max_Image_Len : constant := 1024;

  Inte_Format_Set : Boolean := False;
  Real_Format_Set : Boolean := False;

  procedure Set_Obase (Base : in Item_Rec) is
  begin
    if Base.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    Inte_Io.Default_Base := Ada.Text_Io.Number_Base(Base.Val_Inte);
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
        Inte_Io.Default_Width := Ada.Text_Io.Field (Item.Val_Inte);
        Inte_Format_Set := True;
      when Real =>
        R := My_Math.Int(Item.Val_Real);
        I := My_Math.Round(R);
        Real_Io.Default_Fore := Ada.Text_Io.Field(I);
        -- Aft 0 .. 999
        R := My_Math.Frac(Item.Val_Real) * 1000.0;
        I := My_Math.Round(R);
        Real_Io.Default_Aft := Ada.Text_Io.Field(I);
        -- Exp 3
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
      Format ((Kind => Real, Val_Real => 2.009));
    end if;
  end Check_Default_Formats;

  function Image (Item : in Item_Rec) return String is
    Str : Item_Rec;
  begin
    case Item.Kind is
      when Arbi =>
        return '@' & Arbitrary.Image (Item.Val_Arbi);
      when Frac =>
        return '@' & Arbitrary.Fractions.Image (Item.Val_Frac);
      when Inte | Real | Bool | Chrs =>
        Str := Strof (Item);
        return Asu_Ts (Str.Val_Text);
      when others =>
        raise Invalid_Argument;
    end case;
  end Image;

  procedure Put (Item : in Item_Rec) is
  begin
    Io_Flow.Put (Image (Item));
  end Put;

  procedure Put_Line (Item : in Item_Rec) is
  begin
    Io_Flow.Put_Line (Image (Item));
  end Put_Line;

  procedure New_Line is
  begin
    Io_Flow.New_Line;
  end New_Line;

  function Strarbi (S : Item_Rec) return Item_Rec is
    Len : Natural;
    Res : Item_Rec(Arbi);
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Len :=  Asu.Length (S.Val_Text);
    if Len < 2 or else Asu.Element (S.Val_Text, 1) /= '@' then
      raise Invalid_Argument;
    end if;
    Res.Val_Arbi := Arbitrary.Set (Asu_Ts (Asu.Tail (S.Val_Text, Len - 1)));
    return Res;
  exception
    when Invalid_Argument =>
      raise;
    when others =>
      raise Invalid_Argument;
  end Strarbi;

  function Strfrac (S : Item_Rec) return Item_Rec is
    Len : Natural;
    Sep : Positive;
    N, D : Arbitrary.Number;
    Res : Item_Rec(Frac);
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Len :=  Asu.Length (S.Val_Text);
    -- Locate @
    if Len < 2 or else Asu.Element (S.Val_Text, 1) /= '@' then
      raise Invalid_Argument;
    end if;
    -- Locate :
    Sep := String_Mng.Locate (Asu_Ts (S.Val_Text), ":");
    if Sep <= 2 then
      raise Invalid_Argument;
    end if;
    -- Parse N and D
    N := Arbitrary.Set (Asu.Slice (S.Val_Text, 2, Sep - 1));
    D := Arbitrary.Set (Asu.Slice (S.Val_Text, Sep + 1,
                                   Asu.Length(S.Val_Text)));
    -- Make fraction
    Res.Val_Frac := Arbitrary.Fractions.Set (N, D);
    return Res;
  exception
    when Invalid_Argument =>
      raise;
    when others =>
      raise Invalid_Argument;
  end Strfrac;

  function Strinte (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Inte);
    Last : Positive;
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Inte_Io.Get (Asu_Ts (S.Val_Text), Res.Val_Inte, Last);
    if Last /= Asu.Length (S.Val_Text) then
      raise Invalid_Argument;
    end if;
    return Res;
  exception
    when others =>
      raise Invalid_Argument;
  end Strinte;

  function Strreal (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Real);
    Last : Positive;
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Real_Io.Get (Asu_Ts (S.Val_Text), Res.Val_Real, Last);
    if Last /= Asu.Length (S.Val_Text) then
      raise Invalid_Argument;
    end if;
    return Res;
  exception
    when others =>
      raise Invalid_Argument;
  end Strreal;

  function Strbool (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Bool);
    Last : Positive;
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Bool_Io.Get (Asu_Ts (S.Val_Text), Res.Val_Bool, Last);
    if Last /= Asu.Length (S.Val_Text) then
      raise Invalid_Argument;
    end if;
    return Res;
  exception
    when others =>
      raise Invalid_Argument;
  end Strbool;

  function Strregi (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Regi);
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    if Asu.Length (S.Val_Text) /= 1 or
    else not Is_Register (Asu.Element(S.Val_Text, 1)) then
      raise Invalid_Argument;
    end if;
    Res.Val_Regi := Asu.Element(S.Val_Text, 1);
    return Res;
  exception
    when others =>
      raise Invalid_Argument;
  end Strregi;

  function Strprog (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Prog);
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Res.Val_Text := S.Val_Text;
    return Res;
  exception
    when others =>
      raise Invalid_Argument;
  end Strprog;

  function Strof (Item : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
    Image_Str : String (1 .. Max_Image_Len);
    Image_Len : Natural;

    -- String is at the end of Res
    -- Move it after some spaces at the beginning
    --  so that the whole takes Size characters
    procedure Fix_Size (Size : in Ada.Text_Io.Field) is
      First : Natural := 0;
      Len   : Positive;
    begin
      for I in reverse Image_Str'Range loop
        if Image_Str(I) = ' ' then
          First := I + 1;
          exit;
        end if;
      end loop;
      if First = 0 then
        Image_Len := 0;
        return;
      end if;
      Len := Image_Str'Last - First + 1;
      if Len >= Size then
        Image_Len := Len;
        Image_Str(1 .. Len) := Image_Str(First .. Image_Str'Last);
      else
        Image_Len := Size;
        Image_Str(Size - Len + 1 .. Size)
            := Image_Str(First .. Image_Str'Last);
      end if;
    end Fix_Size;

  begin
    Check_Default_Formats;

    case Item.Kind is
      when Arbi =>
        Res.Val_Text := Asu_Tus ('@' & Arbitrary.Image (Item.Val_Arbi));
      when Frac =>
        Res.Val_Text := Asu_Tus ('@' & Arbitrary.Fractions.Image (
                                             Item.Val_Frac));
      when Inte =>
        Image_Str := (others => ' ');
        Inte_Io.Put(Image_Str, Item.Val_Inte);
        Fix_Size (Inte_Io.Default_Width);
        Res.Val_Text := Asu_Tus ( Image_Str (1 .. Image_Len));
      when Real =>
        Image_Str := (others => ' ');
        Real_Io.Put(Image_Str, Item.Val_Real);
        Fix_Size (Real_Io.Default_Fore + 1 + Real_Io.Default_Aft
                  + Real_Io.Default_Exp);
        Res.Val_Text := Asu_Tus ( Image_Str (1 .. Image_Len));
      when Bool  =>
        Res.Val_Text := Asu_Tus (Mixed_Str(Item.Val_Bool'Img));
      when Chrs =>
        Res := Item;
      when Prog =>
        Res.Val_Text := Item.Val_Text;
      when Regi =>
        Res.Val_Text := Asu_Tus (Item.Val_Regi & "");
      when Oper =>
        raise Invalid_Argument;
    end case;
    return Res;
  exception
    when Constraint_Error =>
      -- Int or real format generates too long string
      raise String_Len;
  end Strof;

  function Normalof (Item      : Item_Rec;
                     Len       : Item_Rec;
                     Right_Len : Item_Rec;
                     Gap       : Item_Rec) return Item_Rec is
    Int, Frac : Integer;
    Long_Int, Tenth  : My_Math.Inte;
    Lint, Lfrac : Positive;
    Res : Item_Rec(Chrs);
  begin
    -- Check Kind is Inte or Real
    if Item.Kind /= Inte and then Item.Kind /= Real then
      raise Invalid_Argument;
    end if;
    -- Check Len is Positive, check Gap is one char
    if Len.Kind /= Inte
    or else Len.Val_Inte <= 0
    or else Gap.Kind /= Chrs
    or else Asu.Length(Gap.Val_Text) /= 1 then
      raise Invalid_Argument;
    end if;
    -- Check right is boolean for int, positive for real
    if Item.Kind = Inte then
      if Right_Len.Kind /= Bool then
        raise Invalid_Argument;
      end if;
    else
      if Right_Len.Kind /= Inte
      or else Right_Len.Val_Inte <= 0 then
        raise Invalid_Argument;
      end if;
    end if;
    if Item.Kind = Inte then
      -- Check range of ints
      begin
        Int := Integer(Item.Val_Inte);
        Lint := Positive(Len.Val_Inte);
      exception
        when Constraint_Error =>
          raise Invalid_Argument;
      end;
      -- Call Normal
      declare
        Str : constant String := Normal (Int, Lint, Right_Len.Val_Bool,
                                         Asu_Ts (Gap.Val_Text)(1));
      begin
        Res.Val_Text := Asu_Tus (Str);
      end;
    else
      -- Check range of ints
      begin
        Tenth := 10 ** Integer(Right_Len.Val_Inte);
        Long_Int := My_Math.Round(Item.Val_Real * My_Math.Real(Tenth));
        Int := Integer(Long_Int / Tenth);
        Lint := Positive(Len.Val_Inte);
        Frac := Integer(abs Long_Int mod Tenth);
        Lfrac := Positive(Right_Len.Val_Inte);
      exception
        when Constraint_Error =>
          raise Invalid_Argument;
      end;
      declare
        Str : constant String
            := Normal(Int, Lint, True, Asu_Ts (Gap.Val_Text)(1))
             & "." & Normal(Frac, Lfrac, True, '0') ;
      begin
        Res.Val_Text := Asu_Tus (Str);
      end;
    end if;
    return Res;
  end Normalof;

end Ios;

