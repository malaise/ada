with Ada.Exceptions;
with As.U, Argument, Argument_Parser, Mixed_Str, Basic_Proc, Ada_Words.Keywords;
with Common, Files, Parse_Context;
procedure Astub is

  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 'f', As.U.Tus ("force"),    False),
   02 => (False, 'k', As.U.Tus ("keep"),     False),
   03 => (True,  'l', As.U.Tus ("lang"), False,
          True, As.U.Tus ("version")));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage : " & Argument.Get_Program_Name
      & " [ <force> ] [ <keep> ] [ <lang> ] <spec_file_name>");
    Basic_Proc.Put_Line_Output (
      "  <force> ::= " & Argument_Parser.Image (Keys(1))
      & " : Force generation (delete existing body)");
    Basic_Proc.Put_Line_Output (
      " <keep>   ::= " & Argument_Parser.Image (Keys(2))
      & "  : Keep empty body");
    Basic_Proc.Put_Line_Output (
      " <lang>   ::= " & Argument_Parser.Image (Keys(3))
      & "  : Ada83..Ada2012 (Ada2012)");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("Error: " & Msg);
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;


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
  begin
    if Arg_Dscr.Is_Set (3) then
      Common.Language_Version := Ada_Words.Keywords.Language_Versions'Value (
                                   Arg_Dscr.Get_Option (3));
    end if;
  exception
    when others =>
      Error ("Invalid language version");
      return;
  end;

  -- Open files
  Basic_Proc.Put_Line_Output ("Astubbing "
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
      Basic_Proc.Put_Line_Output ("Done.");
    else
      Basic_Proc.Put_Line_Error ("Warning: Body is empty but kept.");
    end if;
  else
    -- Dummy body
    Basic_Proc.Put_Line_Output ("Warning: " & Argument.Get_Parameter
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

