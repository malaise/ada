with Ada.Calendar;
with Basic_Proc, Argument, As.U, Date_Text, Images;
procedure T_Date_Text is
  Scan_Format, Put_Format : As.U.Asu_Us;
  Date : Date_Text.Date_Rec;
  Time : Ada.Calendar.Time;
begin
  if Argument.Get_Nbre_Arg <= 1 then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " <scan_format> <put_format> { [ <string> ] }");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  Scan_Format := As.U.Tus (Argument.Get_Parameter (Occurence => 1));
  Put_Format  := As.U.Tus (Argument.Get_Parameter (Occurence => 2));

  Basic_Proc.Put_Line_Output ("Lengths:");
  begin
    Basic_Proc.Put_Line_Output (
        Natural'Image (Date_Text.Length (Scan_Format.Image)));
  exception
    when Date_Text.Unknown_Length => null;
  end;
  begin
    Basic_Proc.Put_Line_Output (
        Natural'Image (Date_Text.Length (Put_Format.Image)));
  exception
    when Date_Text.Unknown_Length => null;
  end;

  for I in 3 .. Argument.Get_Nbre_Arg loop
    Date := Date_Text.Scan (Argument.Get_Parameter (Occurence => I),
                            Scan_Format.Image);
    Time := Date_Text.Pack (Date);
    Basic_Proc.Put_Line_Output ("Input: "
        & Argument.Get_Parameter (Occurence => I)
        & "  Dump: " & Images.Date_Image (Time)
        & "  Put: " & Date_Text.Put (Date, Put_Format.Image));
  end loop;

end T_Date_Text;
