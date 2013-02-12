with Basic_Proc, As.B, Select_File;

procedure T_Select_File is

  Read : Boolean;
  File : As.B.Asb_Bs(500);

  package My_Select_File is new Select_File (10,
                                             Read_Title  => "Test read",
                                             Write_Title => "Test write");

begin
  Read := True;

  loop
    File.Set (My_Select_File.Get_File (File.Image, Read, not Read));
    Basic_Proc.Put_Line_Output (File.Image);
    exit when File.Is_Null;
    Read := not Read;
  end loop;
exception
  when My_Select_File.Exit_Requested =>
    Basic_Proc.Put_Line_Output ("Aborted by user.");
end T_Select_File;

