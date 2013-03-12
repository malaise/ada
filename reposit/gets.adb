with Ada.Text_Io;
package body Gets is
  package Int_Io is new Ada.Text_Io.Integer_Io (Integer);
  package Flo_Io is new Ada.Text_Io.Float_Io (Float);
  package Dur_Io is new Ada.Text_Io.Fixed_Io (Duration);

  function Get_Int (Str : String) return Integer is

    I : Integer;
    L : Positive;
    Str_Len : Natural;
  begin
    -- Locate last significant character of Str
    Str_Len := 0;
    for J in reverse Str'Range loop
      if Str_Len = 0 and then Str(J) /= ' ' then
        Str_Len := J + 1 - Str'First;
      end if;
    end loop;
    if Str_Len = 0 then
      raise Constraint_Error;
    end if;

    Int_Io.Get (Str, I, L);

    if L /= Str'Last then
      raise Constraint_Error;
    end if;
    return I;

  exception
    when others =>
      raise Constraint_Error;
  end Get_Int;

  function Get_Float (Str : String) return Float is
    F : Float;
    L : Positive;
    Str_Len : Natural;
  begin
    -- Locate last significant character of Str
    Str_Len := 0;
    for J in reverse Str'Range loop
      if Str_Len = 0 and then Str(J) /= ' ' then
        Str_Len := J + 1 - Str'First;
      end if;
    end loop;
    if Str_Len = 0 then
      raise Constraint_Error;
    end if;

    Flo_Io.Get (Str, F, L);

    if L /= Str'Last then
      raise Constraint_Error;
    end if;
    return F;

  exception
    when others =>
      raise Constraint_Error;
  end Get_Float;

  function Get_Dur (Str : String) return Duration is

    D : Duration;
    L : Positive;
    Str_Len : Natural;
  begin
    -- Locate last significant character of Str
    Str_Len := 0;
    for J in reverse Str'Range loop
      if Str_Len = 0 and then Str(J) /= ' ' then
        Str_Len := J + 1 - Str'First;
      end if;
    end loop;
    if Str_Len = 0 then
      raise Constraint_Error;
    end if;

    Dur_Io.Get (Str, D, L);

    if L /= Str'Last then
      raise Constraint_Error;
    end if;
    return D;

  exception
    when others =>
      raise Constraint_Error;
  end Get_Dur;

  function Get_Int_Float (Str : String) return Int_Float_Rec is
    Dot_Found : Boolean;
  begin
    Dot_Found := False;
    for J in Str'Range loop
      if Str(J) = '.' then
        Dot_Found := True;
        exit;
      end if;
    end loop;

    if Dot_Found then
      -- Float format
      return (Is_Float => True, Float_Value => Get_Float (Str));
    else
      -- Int format
      return (Is_Float => False, Int_Value => Get_Int (Str));
    end if;

  exception
    when others =>
      raise Constraint_Error;
  end Get_Int_Float;

  function Get_Int_Or_Float (Str : String) return Float is
    Int_Float : Int_Float_Rec;
  begin
    Int_Float := Get_Int_Float(Str);
    return (if Int_Float.Is_Float then Int_Float.Float_Value
            else Float(Int_Float.Int_Value));
  end Get_Int_Or_Float;

end Gets;

