with Flo_Io, Int_Io;
package body Get_Float is

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
    Got_A_Float : Boolean;
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

    Got_A_Float := Dot_Found;
    if Dot_Found then
      -- Float format
      Flo_Io.Get(Str, F, L);
    else
      -- Int format
      Int_Io.Get(Str, I, L);
    end if;


    if L /= Str'Last then
      raise Constraint_Error;
    end if;

    if Got_A_Float then
      return (Is_Float => True, Float_Value => F);
    else
      return (Is_Float => False, Int_Value => I);
    end if;

  exception
    when others =>
      raise Constraint_Error;
  end Get_Int_Float;

end Get_Float;

