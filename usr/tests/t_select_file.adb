with Ada.Text_Io;
with As.B, Select_File;

procedure T_Select_File is

  Read : Boolean;
  File : As.B.Asb_Bs(500);

  function My_Select_File is new Select_File (Read_Title  => "Test read",
                                              Write_Title => "Test write");

begin
  Read := True;

  loop
    File.Set (My_Select_File (10, File.Image, Read, not Read));
    Ada.Text_Io.Put_Line (File.Image);
    exit when File.Is_Null;
    Read := not Read;
  end loop;
end T_Select_File;

