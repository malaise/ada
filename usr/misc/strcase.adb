-- Convert input flow to UPPER_CASE, lower_case or Mixed_Case
with Sys_Calls, Argument, Text_Line;
with Upper_Str, Lower_Str, Mixed_Str;
procedure Strcase is

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name & " [ <command> ]");
    Sys_Calls.Put_Line_Error ("<command> ::= <upper> | <lower> | <mixed> | <help>");
    Sys_Calls.Put_Line_Error ("<upper>   ::= -u | --upper");
    Sys_Calls.Put_Line_Error ("<lower>   ::= -l | --lower");
    Sys_Calls.Put_Line_Error ("<mixed>   ::= -m | --mixed");
    Sys_Calls.Put_Line_Error ("<help>    ::= -h | --help");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & " error: " & Msg & ".");
    Usage;
    Sys_Calls.Set_Error_Exit_Code;
  end Error;

  -- The command (as set by argument parsing)
  type Cmd_List is (Upper, Lower, Mixed, None);
  Command : Cmd_List;

  -- The input and output flows
  In_Flow, Out_Flow : Text_Line.File_Type;

begin

  -- Parse the optional unique argument
  if Argument.Get_Nbre_Arg = 0 then
    Command := None;
  elsif Argument.Get_Nbre_Arg = 1 then
    declare
      Arg : constant String := Lower_Str (Argument.Get_Parameter);
    begin
      if    Arg = "-u" or else Arg = "--upper" then
        Command := Upper;
      elsif Arg = "-l" or else Arg = "--lower" then
        Command := Lower;
      elsif Arg = "-m" or else Arg = "--mixed" then
        Command := Mixed;
      elsif Arg = "-h" or else Arg = "--help" then
        Usage;
        return;
      else
        Error ("Invalid command argument " & Argument.Get_Parameter);
        return;
      end if;
    end;
  else
    Error ("Invalid command arguments");
    return;
  end if;

  -- Attach stdin/stdout to Text_Line flows
  Text_Line.Open (In_Flow,  Text_Line.In_File,  Sys_Calls.Stdin);
  Text_Line.Open (Out_Flow, Text_Line.Out_File, Sys_Calls.Stdout);

  -- For each line of input flow
  loop
    declare
      -- Get the string of text
      Str : constant String := Text_Line.Get (In_Flow);
    begin
      -- Done when end of input flow
      exit when Str = "";
      -- Put text converted according to command
      case Command is
        when Upper =>
          Text_Line.Put (Out_Flow, Upper_Str (Str));
        when Lower =>
          Text_Line.Put (Out_Flow, Lower_Str (Str));
        when Mixed =>
          Text_Line.Put (Out_Flow, Mixed_Str (Str));
        when None =>
          Text_Line.Put (Out_Flow, Str);
      end case;
    end;
  end loop;

  -- Done. Close
  Text_Line.Close (Out_Flow);
  Text_Line.Close (In_Flow);

end Strcase;

