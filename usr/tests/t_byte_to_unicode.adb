with Argument, Basic_Proc, Unicode, Byte_To_Unicode, Normal, Int_Io;
procedure T_Byte_To_Unicode is
  Map : Byte_To_Unicode.Map;
  Uni : Unicode.Unicode_Number;
begin

  Map.Load (Argument.Get_Parameter(1));

  for I in Byte_To_Unicode.Byte'Range loop

    Basic_Proc.Put_Output (Normal (I, 3) & " ");
    Int_Io.Put (I, Base => 16);
    Basic_Proc.Put_Output (" -> ");

    Uni := Map.Convert (I);
    Basic_Proc.Put_Output (Normal (Uni, 5) & " ");
    Int_Io.Put (Uni, Base => 16);
    Basic_Proc.New_Line_Output;

  end loop;

exception
  when others =>
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " <Table_File>");
    raise;
end T_Byte_To_Unicode;

