with Ada.Text_Io;
with Normalization, Mixed_Str, Str_Util;
with Inte_Io, Real_Io, Bool_Io;
separate (Mcd_Mng)

package body Ios is

  use type My_Math.Real;

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
        return Str.Val_Text.Image;
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

  procedure Set_Echo (Item : in Item_Rec) is
  begin
    if Item.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    Io_Flow.Set_Echo (Item.Val_Bool);
  end Set_Echo;

  function Get_Key return Item_Rec is
    Res : Item_Rec(Chrs);
    Char : Character;
  begin
    Char := Io_Flow.Get_Key;
    Res.Val_Text := As.U.Tus (Char);
    return Res;
  end Get_Key;

  function Get_Key_Time (Timeout : Item_Rec) return Item_Rec is
    Delay_Ms : Integer;
    Char : Character;
    Char_Res : Item_Rec(Chrs);
    False_Res : constant Item_Rec(Bool) := (Bool, Val_Bool => False);
  begin
    -- Check in timeout
    if Timeout.Kind = Inte then
      Delay_Ms := Integer(Timeout.Val_Inte) * 1000;
    elsif Timeout.Kind = Real then
      Delay_Ms := Integer (My_Math.Round (Timeout.Val_Real * 1000.0));
    else
      raise Invalid_Argument;
    end if;
    if Delay_Ms < 0 then
      Delay_Ms := 0;
    end if;
    -- Get key
    Char := Io_Flow.Get_Key_Time (Delay_Ms);
    -- Check out timeout
    if Char = Io_Flow.Timeout_Char then
      return False_Res;
    else
      Char_Res.Val_Text.Set (Char);
      return Char_Res;
    end if;
  exception
    when Io_Flow.End_Error =>
      raise;
    when others =>
      -- E.g. too large
      raise Invalid_Argument;
  end Get_Key_Time;

  function Get_Str return Item_Rec  is
    Res : Item_Rec(Chrs);
  begin
    Res.Val_Text := As.U.Tus (Io_Flow.Get_Str);
    return Res;
  end Get_Str;

  function Is_Stdio return Item_Rec is
    Res : Item_Rec(Bool);
  begin
    Res.Val_Bool := Io_Flow.Is_Stdio;
    return Res;
  end Is_Stdio;

  function Strarbi (S : Item_Rec) return Item_Rec is
    Len : Natural;
    Res : Item_Rec(Arbi);
  begin
    if S.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Len :=  S.Val_Text.Length;
    if Len < 2 or else S.Val_Text.Element (1) /= '@' then
      raise Invalid_Argument;
    end if;
    Res.Val_Arbi := Arbitrary.Set (S.Val_Text.Tail (Len - 1).Image);
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
    Len :=  S.Val_Text.Length;
    -- Locate @
    if Len < 2 or else S.Val_Text.Element (1) /= '@' then
      raise Invalid_Argument;
    end if;
    -- Locate :
    Sep := Str_Util.Locate (S.Val_Text.Image, ":");
    if Sep <= 2 then
      raise Invalid_Argument;
    end if;
    -- Parse N and D
    N := Arbitrary.Set (S.Val_Text.Slice (2, Sep - 1));
    D := Arbitrary.Set (S.Val_Text.Slice (Sep + 1, S.Val_Text.Length));
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
    Inte_Io.Get (S.Val_Text.Image, Res.Val_Inte, Last);
    if Last /= S.Val_Text.Length then
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
    if S.Kind /= Chrs or else not Mcd_Parser.Can_Real (S).Val_Bool then
      raise Invalid_Argument;
    end if;
    Real_Io.Get (S.Val_Text.Image, Res.Val_Real, Last);
    if Last /= S.Val_Text.Length then
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
    Bool_Io.Get (S.Val_Text.Image, Res.Val_Bool, Last);
    if Last /= S.Val_Text.Length then
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
    if S.Val_Text.Length /= 1 or
    else not Is_Register (S.Val_Text.Element (1)) then
      raise Invalid_Argument;
    end if;
    Res.Val_Regi := S.Val_Text.Element (1);
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
        Res.Val_Text := As.U.Tus ('@' & Arbitrary.Image (Item.Val_Arbi));
      when Frac =>
        Res.Val_Text := As.U.Tus ('@' & Arbitrary.Fractions.Image (
                                             Item.Val_Frac));
      when Inte =>
        Image_Str := (others => ' ');
        Inte_Io.Put(Image_Str, Item.Val_Inte);
        Fix_Size (Inte_Io.Default_Width);
        Res.Val_Text := As.U.Tus ( Image_Str (1 .. Image_Len));
      when Real =>
        Image_Str := (others => ' ');
        Real_Io.Put(Image_Str, Item.Val_Real);
        Fix_Size (Real_Io.Default_Fore + 1 + Real_Io.Default_Aft
                  + Real_Io.Default_Exp);
        Res.Val_Text := As.U.Tus ( Image_Str (1 .. Image_Len));
      when Bool  =>
        Res.Val_Text := As.U.Tus (Mixed_Str(Item.Val_Bool'Img));
      when Chrs =>
        Res := Item;
      when Prog =>
        Res.Val_Text := Item.Val_Text;
      when Regi =>
        Res.Val_Text := As.U.Tus (Item.Val_Regi & "");
      when Oper =>
        raise Invalid_Argument;
    end case;
    return Res;
  exception
    when Constraint_Error =>
      -- Int or real format generates too long string
      raise String_Len;
  end Strof;

  function Normalof (Item       : Item_Rec;
                     Len        : Item_Rec;
                     Right_Fore : Item_Rec;
                     Gap        : Item_Rec) return Item_Rec is
    Plen, Pfore : Positive;
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
    or else Gap.Val_Text.Length /= 1 then
      raise Invalid_Argument;
    end if;
    -- Check right is boolean for int, positive for real
    if Item.Kind = Inte then
      if Right_Fore.Kind /= Bool then
        raise Argument_Mismatch;
      end if;
    else
      if Right_Fore.Kind /= Inte
      or else Right_Fore.Val_Inte <= 0 then
        raise Argument_Mismatch;
      end if;
    end if;
    -- Check range of ints
    begin
      Plen := Positive(Len.Val_Inte);
      if Item.Kind = Real then
        Pfore := Positive(Right_Fore.Val_Inte);
      end if;
    exception
      when Constraint_Error =>
        raise Invalid_Argument;
    end;
    declare
      Str : constant String
          := (if Item.Kind = Inte then
                -- Call Normal
                Normalization.Normal_Inte (Item.Val_Inte, Plen,
                                           Right_Fore.Val_Bool,
                                           Gap.Val_Text.Element(1))
              else
                Normalization.Normal_Fixed (Item.Val_Real, Plen,
                                            Pfore,
                                            Gap.Val_Text.Element(1))
             );
      begin
        Res.Val_Text := As.U.Tus (Str);
      end;
    return Res;
  exception
    when Invalid_Argument | Argument_Mismatch => raise;
    when others => raise Invalid_Argument;
  end Normalof;

end Ios;

