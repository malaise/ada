with Ada.Text_Io, Ada.Exceptions;
with As.U; use As.U;
with Argument, Argument_Parser, Sys_Calls, Mixed_Str;
with Common, Files, Parse_Context;
procedure Astub is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage : " & Argument.Get_Program_Name
                   & " [ -f | --force ] [ -k | --keep ] <spec_file_name>");
    Ada.Text_Io.Put_Line (
      " -f | --force : Force generation (delete existing body)");
    Ada.Text_Io.Put_Line (
      " -k | --keep : Keep empty body");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error ("Error: " & Msg);
    Usage;
    Sys_Calls.Set_Error_Exit_Code;
  end Error;

  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => ('f', Tus ("force"), False, False),
   02 => ('k', Tus ("keep"), False, False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- The options
  Force_Opt : Boolean;
  Keep_Opt : Boolean;


  Generated : Boolean;
begin
  -- Parse keys and options
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error ("Invalid arguments: " & Arg_Dscr.Get_Error & ".");
    return;
  end if;
  -- Must be one and only one file name
  declare
     Nb_File : constant Natural
             := Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index);
  begin
    if Nb_File = 0 then
      Error ("Missing file name.");
      return;
    elsif Nb_File /= 1 then
      Error ("Too many file names.");
      return;
    end if;
  end;
  Force_Opt := Arg_Dscr.Is_Set (1);
  Keep_Opt := Arg_Dscr.Is_Set (2);

  -- Open files
  Ada.Text_Io.Put_Line ("Astubbing "
      & Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index, 1) );
  begin
    Files.Open (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index, 1),
                Force_Opt);
  exception
    when Files.In_Error =>
      Error ("Cannot open spec file for reading.");
      return;
    when Files.Out_Error =>
      if Force_Opt then
        Error ("Cannot create new body file for writing.");
      else
        Error ("Cannot create new body file for writing. File exists?");
      end if;
      return;
  end;

  -- Parse, starting from context
  Parse_Context (Generated);

  if Generated or else Keep_Opt then
    -- Done, close files
    Files.Close (Files.Keep);

    -- Output success
    if Generated then
      Ada.Text_Io.Put_Line ("Done.");
    else
      Sys_Calls.Put_Line_Error ("Warning: Body is empty but kept.");
    end if;
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

