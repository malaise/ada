with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End,
     Parse_Procedure, Parse_Function, Parse_Task;

procedure Parse_Package (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
begin

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

  -- Parse spec until "is"
  Parse_To_End (Level, "is", False, False);

  -- Check that no renames
  if Words.Search ("renames") /= 0 then
    Sys_Calls.Put_Line_Error (" -->renames<");
    raise Common.Syntax_Error;
  end if;
  Words.Reset;

  -- Output this and " is"
  Output.Put_Line ("package body " & Asu.To_String (Name)
                 & " is", Level, False);
  Output.Put_Line ("begin", Level, False);

  -- Loop until expected word
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Text);
    begin
      if Lexic = Ada_Parser.Comment then
        Output.Put_Line (Str, Level + 1, False);
      elsif Lexic = Ada_Parser.Separator then
        -- Skip separators
        null;
      elsif Str = "end" then
        -- End of this package
        exit;
      elsif Str = "package" then
        Parse_Package (Level + 1);
      elsif Str = "procedure" then
        Parse_Procedure (Level + 1);
      elsif Str = "function" then
        Parse_Function (Level + 1);
      elsif Str = "task" then
        Parse_Task (Level + 1);
            elsif Str = Common.Line_Feed then
        -- Skip input Line_Feed
        null;
      elsif Str = "new" then
        -- This package is in fact a generic instanciation
        Sys_Calls.Put_Line_Error (" -->renames<");
        raise Common.Syntax_Error;
      else
        -- Unexpected, word. Parse to end as comment
        Words.Add (Text);
        Parse_To_End (0, ";", True, True);
      end if;
    end;
  end loop;

  -- Skip up to last ";"
  Parse_To_End (Level, ";", False, False);
  Words.Reset;

  -- end <name>;
  Output.Put_Line ("end " & Ada.Strings.Unbounded.To_String (Name) & ";",
                   Level, False);
end Parse_Package;

