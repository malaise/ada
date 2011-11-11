with Basic_Proc, My_Math, Round_At, Get_Float, Argument;
procedure T_Round_At is
  End_Error : exception;
  procedure Error is
  begin
    Basic_Proc.Put_Line_Output ("Usage " & Argument.Get_Program_Name
                        & " <Real> <Int>");
    raise End_Error;
  end Error;

  Int_Float : Get_Float.Int_Float_Rec;
  R, Res : My_Math.Real;
  I, D : Integer;

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

  -- Compute nb of digits before dot
  D := Integer (My_Math.Trunc (My_Math.Log_10 (R))) + 1;
  Basic_Proc.Put_Line_Output ("Digits: " & D'Img);

  -- Round and put
  Res := Round_At (R, I);
  Basic_Proc.Put_Line_Output ("Image -> " & Res'Img);
  Basic_Proc.Put_Output ("Real_Io -> ");
  if I >= 0 then
    My_Math.Real_Io.Put (Res, Fore => 1, Aft => D);
  else
    My_Math.Real_Io.Put (Res, Fore => 1, Aft => D - I);
  end if;
  Basic_Proc.New_Line_Output;

exception
  when End_Error =>
    null;
end T_Round_At;

