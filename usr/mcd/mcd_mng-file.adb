with Sys_Calls, Text_Line, Many_Strings, Command;
separate (Mcd_Mng)
package body File is

  procedure Open (File_Name : in Item_Rec;
                  In_File : in out Text_Line.File_Type) is
    In_Fd : Sys_Calls.File_Desc;
  begin
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
  end Open;

  -- Close File
  procedure Close (In_File : in out Text_Line.File_Type) is
    In_Fd : constant Sys_Calls.File_Desc := In_File.Get_Fd;
  begin
    In_File.Close;
    Sys_Calls.Close (In_Fd);
  end Close;

  -- Read the content of the file and return a String with the content
  procedure Read (File_Name : in Item_Rec; Content : out Item_Rec) is
    In_File : Text_Line.File_Type;
  begin

    -- Open File
    begin
      Open (File_Name, In_File);
    exception
      when File_Error =>
        Content := (Kind => Bool, Val_Bool => False);
        return;
    end;

    -- Read all content
    In_File.Set_Line_Feed ("");
    Content := (Kind => Chrs, Val_Text => In_File.Get);

    -- Close
    Close (In_File);
  exception
    when others =>
      raise File_Error;
  end Read;

  -- Read the content of the file and append to Content
  procedure Read (File_Name : in Item_Rec;
                  Ok        : out Item_Rec;
                  Content   : out As.U.Utils.Asu_Ua.Unb_Array) is
    In_File : Text_Line.File_Type;
    Line : As.U.Asu_Us;
  begin
    Ok := (Kind => Bool, Val_Bool => False);

    -- Open File
    Content.Set_Null;
    begin
      Open (File_Name, In_File);
    exception
      when File_Error =>
        return;
     end;

    -- Read and append lines
    loop
      Line := In_File.Get;
      exit when Line.Is_Null;
      if Line.Element (Line.Length) = Text_Line.Line_Feed_Char then
        -- Remove trailing Lf
        Line.Delete (Line.Length, Line.Length);
      end if;
      Content.Append (Line);
    end loop;

    -- Close
    Close (In_File);
    Ok := (Kind => Bool, Val_Bool => True);
  exception
    when others =>
      raise File_Error;
  end Read;

  -- Execute command and arguments
  Out_Flow, Err_Flow : aliased Command.Flow_Rec (Command.Str);
  procedure Exec (Cmd    : in Item_Rec;
                  Code   : out Item_Rec;
                  Output : out Item_Rec) is
    Exit_Code : Command.Exit_Code_Range;
  begin
    if Cmd.Kind /= Chrs or else Cmd.Val_Text.Is_Null then
      raise Invalid_Argument;
    end if;
    Command.Execute (Many_Strings.Set (Cmd.Val_Text.Image), True,
        Out_Flow'Access, Err_Flow'Access, Exit_Code);
    Code := (Kind => Inte, Val_Inte => My_Math.Inte (Exit_Code));
    Output := (Kind => Chrs , Val_Text => Out_Flow.Str);
  end Exec;

end File;

