with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End;

procedure Parse_Function (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Args, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  In_Id : Boolean;
  use type Ada_Parser.Lexical_Kind_List;
begin
  Words.Add ("function");
  -- Read until function name
  loop
    Ada_Parser.Parse_Next (File, Name, Lexic, True);
    Words.Add (Name);
    exit when Lexic /= Ada_Parser.Separator;
  end loop;
  if Lexic /= Ada_Parser.Identifier
  and then Lexic /= Ada_Parser.String_Literal then
    Sys_Calls.Put_Line_Error (" -->" 
         & Asu.To_String (Name) & "<");
    raise Common.Syntax_Error;
  end if;
  -- Parse spec until next '(', beginning of arguments
  Parse_To_End (Level, "(", False, False);
  Words.Add ("(");

  -- Parse arguments, store argument formal names in Args
  --  separated by ", "
  In_Id := True;
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    Words.Add (Text);
    if In_Id and then Lexic = Ada_Parser.Identifier then
      -- Append this argument name
      if Asu.Length (Args) /= 0 then
        Asu.Append (Args, ", ");
      end if;
      Asu.Append (Args, Text);
    elsif Asu.To_String (Text) = ":" then
      -- End of argument formal names (entering in | out | inout ...)
      In_Id := False;
    elsif Asu.To_String (Text) = ";" then
      -- End of previous argument, expecting a new one
      In_Id := True;
    elsif Asu.To_String (Text) = ")" then
      exit;
    end if;
  end loop;

  -- Parse until last ";"
  Parse_To_End (Level, ";", False, False);

  -- Check that no renames nor generic instanciation
  if Words.Search ("renames") /= 0 then
    Sys_Calls.Put_Line_Error (" -->renames<");
    raise Common.Syntax_Error;
  end if;
  if Words.Search ("is") /= 0 then
    Sys_Calls.Put_Line_Error (" -->is<");
    raise Common.Syntax_Error;
  end if;

  -- Output this and " is"
  Output.Put_Line (Words.Get & " is", Level, False);
  Words.Reset;

  -- begin
  --   <name> (<args>);
  -- end <name>;
  Output.Put_Line ("begin", Level, False);
  Output.Put_Line (Asu.To_String (Name) & " (" & Asu.To_String (Args) & ");",
                   Level + 1, False);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";",
                   Level, False);
end Parse_Function;

