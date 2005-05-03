with Ada.Text_Io;
with Text_Handler;
with Select_File;

procedure T_Select_File is

  Read : Boolean;
  File : Text_Handler.Text(500);

  function My_Select_File is new Select_File (Read_Title  => "Test read",
                                              Write_Title => "Test write");

begin
  Read := True;

  loop
    Text_Handler.Set (File, My_Select_File (10, Text_Handler.Value(File), Read));
    Ada.Text_Io.Put_Line (Text_Handler.Value(File));
    exit when Text_Handler.Empty(File);
    Read := not Read;
  end loop;
end T_Select_File;

