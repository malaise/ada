-- Copy several files (arguements and/or read from stdin) into a destination
--  directory
-- Ths is not recursive (i.e. sources must be FILES)
with Ada.Exceptions;
with Basic_Proc, Argument, Argument_Parser, As.U,
      Sys_Calls.File_Access, Directory,
     Text_Line, Parser, Copy_File;
procedure Copy_Files is

  -- Help
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output (
        "Usage: " & Argument.Get_Program_Name
        & " [ <options> ] [ <files> ] <destination>");
    Basic_Proc.Put_Line_Output (
        "  <options> ::= <none> | <move>");
    Basic_Proc.Put_Line_Output (
        "  <none>    ::= -n | --none     // Just jog actions");
    Basic_Proc.Put_Line_Output (
        "  <move>    ::= -m | --move     // Move files delete after copy");
    Basic_Proc.Put_Line_Output (
        "  <files>   ::= - | { <file> } // "
        & "if ""-"" then read names from stdin");
  end Usage;

  -- Error
  Abort_Error : exception;
  procedure Error (Message : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR"
        & (if Message /= "" then ": " & Message else "")
        & ".");
    Basic_Proc.New_Line_Error;
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Abort_Error;
  end Error;

  -- Copy or move one File to Dest
  -- Don't do it (just log) if None
  procedure Copy_One (File : in String; Dest : in String;
                      Move : in Boolean; None : in Boolean) is
    Status : Sys_Calls.File_Stat_Rec;
    use type Sys_Calls.File_Desc_Kind_List;
    R, W, X : Boolean;
    Dest_Name : As.U.Asu_Us;
  begin
    -- Check read access to file
    Status := Sys_Calls.File_Stat (File);
    if Status.Kind /= Sys_Calls.File then
      Error ("Source file " & File & " is not a regular file");
    end if;
    Sys_Calls.File_Access.Has_Access (Status, R, W, X);
    if not R then
      Error ("User does not have read access to file " & File);
    end if;

    -- Build full destination name
    Dest_Name.Set (Dest & "/" & Directory.Basename (File));
    -- Check that destination does not exist
    if Sys_Calls.File_Found (Dest_Name.Image) then
       Error ("Destination " & Dest_Name.Image & " already exists");
    end if;

    -- Copy then delete, if requested
    if not None then
      if not Copy_File (File, Dest_Name.Image) then
        Error ("Cannot copy of file " & File & " to destination "
            & Dest_Name.Image);
      end if;
      -- Delete if Move
      if Move then
        if not Sys_Calls.Unlink (File) then
          Error ("Cannot remove file " & File);
        end if;
      end if;
    end if;

    -- Log
    Basic_Proc.Put_Line_Output (
      File & (if Move then " => " else " -> ") & Dest_Name.Image);
  exception
    when Sys_Calls.Name_Error | Sys_Calls.Access_Error =>
      Error ("Cannot access to file " & File);
  end Copy_One;

  -- Options: Help / None (just log) / Move
  Keys : constant Argument_Parser.The_Keys_Type := (
    01 => (False, 'h', As.U.Tus ("help"), False),
    02 => (False, 'n', As.U.Tus ("none"), False),
    03 => (False, 'm', As.U.Tus ("move"), False) );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  None, Move : Boolean;
  -- For parsing: non option key, and number of last non option
  Nki : constant Argument_Parser.The_Keys_Index := Argument_Parser.No_Key_Index;
  Nb_Args : Natural;

  -- Destination (directory)
  Dest : As.U.Asu_Us;

begin
  -- Process arguments
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error ("Invalid argument(s), " & Arg_Dscr.Get_Error);
  end if;

  -- Check help, must be alone
  if Arg_Dscr.Is_Set (01) then
    -- Help
    if Argument.Get_Nbre_Arg /= 1 then
      Error ("Invalid argument(s)");
    end if;
    Usage;
    return;
  end if;

  -- Check options
  None := Arg_Dscr.Is_Set (02);
  Move := Arg_Dscr.Is_Set (03);

  -- The options must be followed by at least 2 arguments
  -- A file or "--", and the destination
  Nb_Args := Arg_Dscr.Get_Nb_Occurences (Nki);
  if Nb_Args < 2 or else Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Error ("Invalid argument(s)");
  end if;

  -- Check destination, last argument
  Dest.Set (Arg_Dscr.Get_Option (Nki, Nb_Args));
  declare
    Status : Sys_Calls.File_Stat_Rec;
    R, W, X : Boolean;
    use type Sys_Calls.File_Desc_Kind_List;
  begin
    -- Destination must be a directory and writable
    Status := Sys_Calls.File_Stat (Dest.Image);
    if Status.Kind /= Sys_Calls.Dir then
      Error ("Destination " & Dest.Image & " is not a directory");
    end if;
    Sys_Calls.File_Access.Has_Access (Status, R, W, X);
    if not (R and W and X) then
      Error ("User does not have enough access rights to destinaton "
          & Dest.Image);
    end if;
  exception
    when Sys_Calls.Name_Error | Sys_Calls.Access_Error =>
      Error ("Cannot access to destination " & Dest.Image);
  end;

  -- One file "--" or files...
  if Nb_Args = 2 and then Arg_Dscr.Get_Option (Nki, 1) = "-" then
    -- Copy from stdin
    declare
      Stdin : Text_Line.File_Type;
      Line : As.U.Asu_Us;
      Parsed : Parser.Iterator;
    begin
      -- Open stream
      Stdin.Open_All (Text_Line.In_File);
      loop
        Line := Stdin.Get;
        exit when Line.Is_Null;
        -- Parse line
        Text_Line.Trim (Line);
        Parsed.Set (Line.Image);
        -- Process each word
        loop
           Parsed.Next_Word;
           exit when Parsed.Current_Word = "";
           Copy_One (Parsed.Current_Word, Dest.Image, Move, None);
        end loop;
      end loop;
      Stdin.Close_All;
    end;
  else
    -- Copy arguments (all but last)
    for I in 1 .. Nb_Args - 1 loop
      Copy_One (Arg_Dscr.Get_Option (Nki, I), Dest.Image, Move, None);
    end loop;
  end if;

exception
  when Abort_Error =>
     null;
  when Error:others =>
    Basic_Proc.Put_Line_Error ("ERROR: Exception "
        & Ada.Exceptions.Exception_Name (Error)
        & " raised");
    Basic_Proc.Set_Error_Exit_Code;
end Copy_Files;

