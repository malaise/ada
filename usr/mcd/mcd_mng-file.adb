with Sys_Calls, Text_Line;
separate (Mcd_Mng)
package body File is

  -- Read the content of the file and return a Prog with the content
  procedure Read (File_Name : in Item_Rec; Content : out Item_Rec) is
    In_Fd : Sys_Calls.File_Desc;
    In_File : Text_Line.File_Type;
    use type As.U.Asu_Us;
  begin
    Content := (Kind => Prog, Val_Text => As.U.Asu_Null);

    -- Open File
    if File_Name.Kind /= Chrs or else File_Name.Val_Text.Is_Null then
      raise Invalid_Argument;
    end if;
    begin
      In_Fd := Sys_Calls.Open (File_Name.Val_Text.Image, Sys_Calls.In_File);
    exception
      when Sys_Calls.Name_Error | Sys_Calls.System_Error =>
        raise File_Error;
    end;
    In_File.Open (Text_Line.In_File, In_Fd);

    -- Read and append lines
    loop
      declare
        Str : constant String := In_File.Get;
      begin
        exit when Str = "";
        Content.Val_Text.Append (Str);
      end;
    end loop;

    -- Close
    In_File.Close;
    Sys_Calls.Close (In_Fd);

  end Read;
end File;

