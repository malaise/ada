with Ada.Calendar;
with Basic_Proc, Argument, As.U, Date_Text, Images;
procedure T_Date_Text is
  Scan_Format, Put_Format, Arg : As.U.Asu_Us;
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
        "Scan: " & Natural'Image (Date_Text.Length (Scan_Format.Image)));
  exception
    when Date_Text.Unknown_Length =>
      null;
    when Date_Text.Invalid_Format =>
      Basic_Proc.Put_Line_Error ("ERROR: Invalid scan format: "
                               & Scan_Format.Image & ".");
      Basic_Proc.Set_Error_Exit_Code;
      return;
  end;
  begin
    Basic_Proc.Put_Line_Output (
        "Put:  " & Natural'Image (Date_Text.Length (Put_Format.Image)));
  exception
    when Date_Text.Unknown_Length =>
      null;
    when Date_Text.Invalid_Format =>
      Basic_Proc.Put_Line_Error ("ERROR: Invalid put format: "
                               & Put_Format.Image & ".");
      Basic_Proc.Set_Error_Exit_Code;
      return;
  end;

  for I in 3 .. Argument.Get_Nbre_Arg loop
    Argument.Get_Parameter (Arg, I);
    begin
      Date := Date_Text.Scan (Arg.Image, Scan_Format.Image);
    exception
      when Date_Text.Invalid_String =>
        Basic_Proc.Put_Line_Error ("ERROR: Invalid input string " & Arg.Image
                                 & ".");
        Basic_Proc.Set_Error_Exit_Code;
        return;
    end;
    Time := Date_Text.Pack (Date);
    Basic_Proc.Put_Line_Output ("Input: "
        & Argument.Get_Parameter (Occurence => I)
        & "  Dump: " & Images.Date_Image (Time)
        & "  Put: " & Date_Text.Put (Date, Put_Format.Image));
  end loop;

end T_Date_Text;

