with Ada.Text_Io;
with Get_Int;
package body Get_Float is
  package Flo_Io is new Ada.Text_Io.Float_Io (Float);

  function Get_Float (Str : String) return Float is
    Int_Float : Int_Float_Rec;
  begin
    Int_Float := Get_Int_Float(Str);
    if Int_Float.Is_Float then
      return Int_Float.Float_Value;
    else
      return Float(Int_Float.Int_Value);
    end if;
  end Get_Float;

  function Get_Int_Float (Str : String) return Int_Float_Rec is
    F : Float;
    L : Positive;
    I : Integer;
    Str_Len : Natural;
    Dot_Found : Boolean;
  begin
    -- Locate last significant character of Str
    Str_Len := 0;
    Dot_Found := False;
    for J in reverse Str'Range loop
      if Str_Len = 0 and then Str(J) /= ' ' then
        Str_Len := J + 1 - Str'First;
      end if;
      if Str(J) = '.' then
        Dot_Found := True;
      end if;
    end loop;
    if Str_Len = 0 then
      raise Constraint_Error;
    end if;

    if Dot_Found then
      -- Float format
      Flo_Io.Get (Str, F, L);
      if L /= Str'Last then
        raise Constraint_Error;
      end if;
      return (Is_Float => True, Float_Value => F);
    else
      -- Int format
      I := Get_Int (Str);
      return (Is_Float => False, Int_Value => I);
    end if;

  exception
    when others =>
      raise Constraint_Error;
  end Get_Int_Float;

end Get_Float;

