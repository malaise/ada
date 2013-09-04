separate (Replace_Pattern)

function File_Content (Path : String) return String is
  Txt, Line : As.U.Asu_Us;
  Fd : Sys_Calls.File_Desc;
  File : Text_Line.File_Type;

  procedure Close is
  begin
    if File.Is_Open then
      Sys_Calls.Close (Fd);
      File.Close;
    end if;
  end Close;

begin
  Log.Rep ("Inserting file: >" & Path & "<");

  -- Open
  begin
    Fd := Sys_Calls.Open (Path, Sys_Calls.In_File);
  exception
    when others =>
      Put_Error ("Opening file " & Path & " has failed");
      raise File_Error;
  end;
  File.Open (Text_Line.In_File, Fd);

  -- Read and concat
  loop
    Line := File.Get;
    exit when Line.Is_Null;
    Txt.Append (Line);
  end loop;

  -- Done
  Close;
  return Txt.Image;

exception
  when File_Error =>
    Close;
    raise;
  when others =>
    Put_Error ("File insertion failed");
    Close;
    raise File_Error;
end File_Content;

