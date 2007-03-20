with Sys_Calls;
separate (Xml_Parser)
package body File_Mng is

  procedure Open (File_Name : String) is
    Fd : Sys_Calls.File_Desc;
  begin
    if Text_Char.Is_Open (File) then
      raise Text_Char.Status_Error;
    end if;
    if File_Name /= "" then
      -- Open physical file for reading
      begin
        Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
      exception
        when Sys_Calls.Name_Error =>
          raise File_Error;
      end;
    else
      Fd := Sys_Calls.Stdin;
    end if;
    -- Open Text_Char file
    Text_Char.Open (File, Fd);
  end Open;

  procedure Close is
    Fd : Sys_Calls.File_Desc;
    use type Sys_Calls.File_Desc;
  begin
    -- Get fd (File must be open, otherwise Status_Error)
    Fd := Text_Char.Get_Fd (File);
    -- Close Text_Char file
    Text_Char.Close (File);
    if Fd /= Sys_Calls.Stdin then
      -- Close physical file
      Sys_Calls.Close (Fd);
    end if;
  end Close;

end File_Mng;

