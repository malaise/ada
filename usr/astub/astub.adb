with Ada.Text_Io, Ada.Exceptions;
with Argument, Sys_Calls, Mixed_Str;
with Common, Files, Parse_Context;
procedure Astub is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage : " & Argument.Get_Program_Name
                        & " <spec_file_name>");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error ("Error: " & Msg);
    Usage;
    Sys_Calls.Set_Error_Exit_Code;
  end Error;

  Generated : Boolean;
begin
  Ada.Text_Io.Put_Line ("Astubbing " & Argument.Get_Parameter);

  -- Must be one argument
  if Argument.Get_Nbre_Arg /= 1 then
    Error ("Argument expected.");
    return;
  end if;

  -- Open files
  begin
    Files.Open (Argument.Get_Parameter);
  exception
    when Files.In_Error =>
      Error ("Cannot open spec file for reading.");
      return;
    when Files.Out_Error =>
      Error ("Cannot create new body file for writing. File exists?");
      return;
  end;

  -- Parse, starting from context
  Parse_Context (Generated);

  if Generated then
    -- Done, close files
    Files.Close (Files.Keep);

    -- Output success
    Ada.Text_Io.Put_Line ("Done.");
  else
    -- Dummy body
    Ada.Text_Io.Put_Line ("Warning: " & Argument.Get_Parameter
                         & " leads to empty body. Removing it.");
    Files.Close (Files.Remove);
  end if;

exception
  when Common.Syntax_Error =>
    Files.Close (Files.Remove_If_Not_Keep);
    Error ("Syntax error while parsing.");
  when Except:others =>
    Files.Close (Files.Remove_If_Not_Keep);
    Error ("Exception " & Mixed_Str (Ada.Exceptions.Exception_Name (Except))
          & " raised while parsing.");
end Astub;

