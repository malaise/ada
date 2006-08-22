with Ada.Text_Io;
with My_Math, Round_At, Get_Float, Argument;
procedure T_Round_At is
  End_Error : exception;
  procedure Error is
  begin
    Ada.Text_Io.Put_Line ("Usage " & Argument.Get_Program_Name
                        & " <Real> <Int>");
    raise End_Error;
  end Error;

  Int_Float : Get_Float.Int_Float_Rec;
  R, Res : My_Math.Real;
  I : Integer;

begin

  if Argument.Get_Nbre_Arg /= 2 then
    Error;
  end if;
  
  -- Get R
  Int_Float := Get_Float.Get_Int_Float (
                   Argument.Get_Parameter (Occurence => 1));
  if not Int_Float.Is_Float then
    Error;
  end if;
  R := My_Math.Real(Int_Float.Float_Value);

  -- Get I
  Int_Float := Get_Float.Get_Int_Float (
                   Argument.Get_Parameter (Occurence => 2));
  if Int_Float.Is_Float then
    Error;
  end if;
  I := Int_Float.Int_Value;

  -- Round and put
  Res := Round_At (R, I);
  Ada.Text_Io.Put_Line (Res'Img);

exception
  when End_Error =>
    null;
end T_Round_At;

