with Ada.Text_Io;
with Argument, Basic_Proc, Byte_To_Unicode, Normal, Int_Io;
procedure T_Byte_To_Unicode is
  Map : Byte_To_Unicode.Map;
  Unicode : Byte_To_Unicode.Unicode_Number;
begin

  Map.Load (Argument.Get_Parameter(1));

  for I in Byte_To_Unicode.Byte'Range loop

    Ada.Text_Io.Put (Normal (I, 3) & " ");
    Int_Io.Put (I, Base => 16);
    Ada.Text_Io.Put (" -> ");

    Unicode := Map.Convert (I);
    Ada.Text_Io.Put (Normal (Unicode, 5) & " ");
    Int_Io.Put (Unicode, Base => 16);
    Ada.Text_Io.New_Line;

  end loop;

exception
  when others =>
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " <Table_File>");
    raise;
end T_Byte_To_Unicode;

