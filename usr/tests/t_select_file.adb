with Text_Io;
with Text_Handler;
with Select_File;

procedure T_Select_File is

  Read : Boolean;
  File : Text_Handler.Text(500);

  procedure Init is
  begin
    null;
  end Init;

  function My_Select_File is new Select_File(Init);

begin
  Read := True;

  loop
    Text_Handler.Set (File, My_Select_File (10, Text_Handler.Value(File), Read));
    Text_Io.Put_Line (Text_Handler.Value(File));
    exit when Text_Handler.Empty(File);
    Read := not Read;
  end loop;
end T_Select_File;

