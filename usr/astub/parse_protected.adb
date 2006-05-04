with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End,
     Parse_Procedure, Parse_Function, Parse_Entry;

procedure Parse_Protected (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
begin

  -- Read until protected name, skip "type"
  loop
    Ada_Parser.Parse_Next (File, Name, Lexic, True);
    exit when Lexic /= Ada_Parser.Separator;
    Words.Add (Name);
  end loop;
  if Asu.To_String (Name) = "type" then
    loop
      Ada_Parser.Parse_Next (File, Name, Lexic, True);
      Words.Add (Name);
      exit when Lexic /= Ada_Parser.Separator;
    end loop;
  end if;
 
  if Lexic /= Ada_Parser.Identifier
  and then Lexic /= Ada_Parser.String_Literal then
    Sys_Calls.Put_Line_Error (" -->" 
         & Asu.To_String (Name) & "<");
    raise Common.Syntax_Error;
  end if;

  -- Skip until "is"
  Parse_To_End ("is", False);

  -- Check that no renames
  if Words.Search ("renames") /= 0 then
    Sys_Calls.Put_Line_Error (" -->renames<");
    raise Common.Syntax_Error;
  end if;
  Words.Reset;

  -- Output this and " is"
  Output.Put_Line ("protected body " & Asu.To_String (Name)
                 & " is", Level, False);
  Output.Put_Line ("", 0, False);

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
        -- End of this protected
        exit;
      elsif Str = "procedure" then
        Parse_Procedure (Level + 1);
        Output.Put_Line ("", 0, False);
      elsif Str = "function" then
        Parse_Function (Level + 1);
        Output.Put_Line ("", 0, False);
      elsif Str = "entry" then
        Parse_Entry (Level + 1);
        Output.Put_Line ("", 0, False);
      elsif Str = "new" then
        -- This protected is in fact a generic instanciation
        Sys_Calls.Put_Line_Error (" -->new<");
        raise Common.Syntax_Error;
      elsif Str = "private" then
        -- Put "private" as a comment
        Output.Put_Line (Str, Level, True);
        Output.Put_Line ("", 0, False);
      else
        -- Unexpected, word. Parse to end as comment
        Words.Add (Text);
        Parse_To_End (";", True, Level + 1);
      end if;
    end;
  end loop;

  -- Skip up to last ";"
  Parse_To_End (";", False);
  Words.Reset;

  -- end <name>;
  Output.Put_Line ("end " & Ada.Strings.Unbounded.To_String (Name) & ";",
                   Level, False);
end Parse_Protected;

