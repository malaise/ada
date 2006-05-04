with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End;

procedure Parse_Type (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  Text : Ada.Strings.Unbounded.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  First_Line_Put : Boolean := False;
  Paren_Level : Natural := 0;
  use type Ada_Parser.Lexical_Kind_List;
begin
  Words.Add ("type");
  -- Read until "record" or ";"
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (text);
    begin
      if Str = Common.Line_Feed then
        -- Flush line, basic indent
        Output.Put_Line (Words.Get, Level, True);
        First_Line_Put := True;
        Words.reset;
      elsif Str= "(" then
        Words.Add (Text);
        Paren_Level := Paren_Level + 1;
      elsif Str= ")" then
        Words.Add (Text);
        Paren_Level := Paren_Level - 1;
      elsif Str= ";" and then Paren_Level = 0 then
        -- ";" outside () and without "record" -> end of type
        Words.Add (Text);
        Output.Put_Line (Words.Get, Level, True);
        Words.reset;
        return;
      elsif Str = "access" then
        -- Access type to function or procedure, with args...
        Parse_To_End (";", True, Level);
        return;
      elsif Str = "record" then
        -- Record type: special parsing follows
        Words.Add (Text);
        exit;
      else
        -- In any case, save this word
        Words.Add (Text);
      end if;
    end;
  end loop;

  -- Record type, skip ";" of fields definitions, case...
  --  up to next "(end) record;"
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (text);
    begin
      if Str = Common.Line_Feed then
        -- Flush line, basic indent
        if First_Line_Put then
          Output.Put_Line (Words.Get, Level, True, 1);
        else
          Output.Put_Line (Words.Get, Level, True);
          First_Line_Put := True;
        end if;
        Words.reset;
      elsif Str = "record" then
        -- This is the "record" of "end record;"
        Words.Add (Text);
        exit;
      else
        -- In any case, save this word
        Words.Add (Text);
      end if;
    end;
  end loop;
  -- Then parse up to last ";"
  Parse_To_End (";", True, Level);

end Parse_Type;

