with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End;

procedure Parse_Procedure (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  Name, Text : Ada.Strings.Unbounded.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
begin
  Words.Add ("procedure");
  -- Read until procedure name
  loop
    Ada_Parser.Parse_Next (File, Name, Lexic, True);
    Words.Add (Name);
    exit when Lexic /= Ada_Parser.Separator;
  end loop;
  if Lexic /= Ada_Parser.Identifier
  and then Lexic /= Ada_Parser.String_Literal then
    Sys_Calls.Put_Line_Error (" -->" 
         & Ada.Strings.Unbounded.To_String (Name) & "<");
    raise Common.Syntax_Error;
  end if;
  -- Parse spec until last ';'
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
  --   null;
  -- end <name>;
  Output.Put_Line ("begin", Level, False);
  Output.Put_Line ("null;", Level + 1, False);
  Output.Put_Line ("end " & Ada.Strings.Unbounded.To_String (Name) & ";",
                   Level, False);
end Parse_Procedure;

