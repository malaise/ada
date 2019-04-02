with Argument, Basic_Proc, Unicode, Byte_To_Unicode, Normal, Images;
procedure T_Byte_To_Unicode is
  Map : Byte_To_Unicode.Map;
  Uni : Unicode.Unicode_Number;
  package Image16 is new Images.Int_Image16 (Unicode.Unicode_Number);
begin


  Map.Load (Argument.Get_Parameter(1));

  for I in Byte_To_Unicode.Byte loop

    Basic_Proc.Put_Output (Normal (I, 3) & " ");
    Basic_Proc.Put_Output (Image16.Image (I));
    Basic_Proc.Put_Output (" -> ");

    Uni := Map.Convert (I);
    Basic_Proc.Put_Output (Normal (Uni, 5) & " ");
    Basic_Proc.Put_Output (Image16.Image (Uni));
    Basic_Proc.New_Line_Output;

  end loop;

exception
  when others =>
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " <Table_File>");
    raise;
end T_Byte_To_Unicode;

