-- Put a random number:
-- By default (no arg)          0 or 1
-- If one int arg I1            0   <= N <= I1
-- If one float arg F1          0.0 <= N <  F1
-- If two int arg I1 and I2     I1  <= N <= I2
-- If two float arg F1 and F2   F1  <= N <  F2
with Argument, Rnd, Basic_Proc, Gets;
procedure Random is

  Nb_Arg : Natural;

  Int_Float_Min, Int_Float_Max : Gets.Int_Float_Rec;

  Error : exception;

begin

  Nb_Arg := Argument.Get_Nbre_Arg;

  if Nb_Arg = 0 then
    -- No arg, 0 .. 1
    Int_Float_Min := (Is_Float=> False, Int_Value => 0);
    Int_Float_Max := (Is_Float=> False, Int_Value => 1);
  elsif Nb_Arg = 1 then
    -- One arg, the max
    Int_Float_Max := Gets.Get_Int_Float (Argument.Get_Parameter(1));
    if Int_Float_Max.Is_Float then
      Int_Float_Min := (Is_Float=> True, Float_Value => 0.0);
    else
      Int_Float_Min := (Is_Float=> False, Int_Value => 0);
    end if;
  elsif Nb_Arg = 2 then
    -- Two args, the min and max
    Int_Float_Min := Gets.Get_Int_Float(Argument.Get_Parameter(1));
    Int_Float_Max := Gets.Get_Int_Float(Argument.Get_Parameter(2));
    if Int_Float_Min.Is_Float /= Int_Float_Max.Is_Float then
      raise Error;
    end if;
  else
    raise Error;
  end if;

  Rnd.Randomize;
  if Int_Float_Min.Is_Float then
    Basic_Proc.Put_Output (
     Rnd.Float_Random(Int_Float_Min.Float_Value, Int_Float_Max.Float_Value)'Img);
  else
    Basic_Proc.Put_Output (
     Rnd.Int_Random(Int_Float_Min.Int_Value, Int_Float_Max.Int_Value)'Img);
  end if;

exception
  when Error =>
    Basic_Proc.Put_Line_Error ("ERROR. Usage: " & Argument.Get_Program_Name & " [ [ <min> ] <max> ]");
    Basic_Proc.Put_Line_Error (" min and max either both integers or both floats.");
    Basic_Proc.Put_Line_Error (" default min is 0, default max is 1.");
end Random;

