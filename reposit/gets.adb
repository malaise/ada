with Ada.Text_Io;
package body Gets is
  package Llint_Io is new Ada.Text_Io.Integer_Io (Long_Longs.Ll_Integer);
  package Llunat_Io is new Ada.Text_Io.Modular_Io (Long_Longs.Llu_Natural);
  package Flo_Io is new Ada.Text_Io.Float_Io (Float);
  package Real_Io is new Ada.Text_Io.Float_Io (My_Math.Real);
  package Dur_Io is new Ada.Text_Io.Fixed_Io (Duration);

  function Get_Int (Str : String) return Integer is
    L : Long_Longs.Ll_Integer;
  begin
    L := Get_Llint (Str);
   return Integer (L);
  exception
    when others =>
      raise Constraint_Error;
  end Get_Int;

  function Get_Llint (Str : String) return Long_Longs.Ll_Integer is
    I : Long_Longs.Ll_Integer;
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

    Llint_Io.Get (Str, I, L);

    if L /= Str'Last then
      raise Constraint_Error;
    end if;
    return I;
  exception
    when others =>
      raise Constraint_Error;
  end Get_Llint;

  function Get_Llunat (Str : String) return Long_Longs.Llu_Natural is
    I : Long_Longs.Llu_Natural;
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

    Llunat_Io.Get (Str, I, L);

    if L /= Str'Last then
      raise Constraint_Error;
    end if;
    return I;
  exception
    when others =>
      raise Constraint_Error;
  end Get_Llunat;

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

  function Get_Int_Or_Float (Str : String) return Int_Or_Float_Rec is
    Llint_Or_Float : Llint_Or_Float_Rec;
  begin
    Llint_Or_Float := Get_Llint_Or_Float (Str);
    return (
      if Llint_Or_Float.Is_Float then
        (Is_Float => True,  Float_Value => Llint_Or_Float.Float_Value)
      else
        (Is_Float => False, Int_Value   => Integer(Llint_Or_Float.Llint_Value))
    );
  exception
    when others =>
      raise Constraint_Error;
  end Get_Int_Or_Float;

  function Get_Llint_Or_Float (Str : String) return Llint_Or_Float_Rec is
    Dot_Found : Boolean;
  begin
    Dot_Found := (for some C of Str => C = '.');

    return (
      if Dot_Found then (Is_Float => True,  Float_Value => Get_Float (Str))
                   else (Is_Float => False, Llint_Value => Get_Llint (Str)) );
  exception
    when others =>
      raise Constraint_Error;
  end Get_Llint_Or_Float;


  function Get_Llint_Float (Str : String) return Float is
    Llint_Float : Llint_Or_Float_Rec;
  begin
    Llint_Float := Get_Llint_Or_Float (Str);
    return (if Llint_Float.Is_Float then Llint_Float.Float_Value
            else Float(Llint_Float.Llint_Value));
  end Get_Llint_Float;

  function Get_Real (Str : String) return My_Math.Real is
    R : My_Math.Real;
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

    Real_Io.Get (Str, R, L);

    if L /= Str'Last then
      raise Constraint_Error;
    end if;
    return R;
  exception
    when others =>
      raise Constraint_Error;
  end Get_Real;

  function Get_Llint_Or_Real (Str : String) return Llint_Or_Real_Rec is
    Dot_Found : Boolean;
  begin
    Dot_Found := (for some C of Str => C = '.');

    return (
      if Dot_Found then (Is_Real => True,  Real_Value  => Get_Real (Str))
                   else (Is_Real => False, Llint_Value => Get_Llint (Str)) );
  exception
    when others =>
      raise Constraint_Error;
  end Get_Llint_Or_Real;


  function Get_Llint_Real (Str : String) return My_Math.Real is
    Llint_Real : Llint_Or_Real_Rec;
  begin
    Llint_Real := Get_Llint_Or_Real (Str);
    return (if Llint_Real.Is_Real then Llint_Real.Real_Value
            else My_Math.Real(Llint_Real.Llint_Value));
  end Get_Llint_Real;

end Gets;

