with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End;

procedure Parse_Type (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  Text : Ada.Strings.Unbounded.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  Paren_Level : Natural := 0;
  use type Ada_Parser.Lexical_Kind_List;
begin
  Words.Add (Ada_Parser.Reserved_Word, "type");
  -- Read until "record" or ";"
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Text);
    begin
      -- In any case, save this word
      Words.Add (Lexic, Text);
      if Str= "(" then
        Paren_Level := Paren_Level + 1;
      elsif Str= ")" then
        Paren_Level := Paren_Level - 1;
      elsif Str= ";" and then Paren_Level = 0 then
        -- ";" outside () and without "record" -> end of type
        Output.Put_Line (Words.Concat, True, Level);
        Words.Reset;
        return;
      elsif Str = "access" then
        -- Access type to function or procedure, with args...
        Parse_To_End (";");
        return;
      elsif Str = "record" then
        -- Record type: special parsing follows
        exit;
      end if;
    end;
  end loop;

  -- Record type, skip ";" of fields definitions, case...
  --  up to next "(end) record;"
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Text);
    begin
      -- In any case, save this word
      Words.Add (Lexic, Text);
      if Str = "record" then
        -- This is the "record" of "end record;"
        exit;
      end if;
    end;
  end loop;
  -- Then parse up to last ";"
  Parse_To_End (";");

end Parse_Type;

