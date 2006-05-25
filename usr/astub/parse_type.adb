with Ada.Strings.Unbounded;
with Text_Char;
with Common, Files, Output, Words, Parser_Ada, Parse_To_End, Fix_Comment;

-- Parse type definition or representation clause
procedure Parse_Type (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  Word : Parser_Ada.Word_Rec;
  Paren_Level : Natural := 0;
  use type Parser_Ada.Lexical_Kind_List,
           Ada.Strings.Unbounded.Unbounded_String,
           Words.Word_Rec;

begin
  -- Handle indent on our own, keep token
  Word := Words.Get;
  Words.Reset;
  Words.Add (Word);

  -- Read until "record" or ";"
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Word.Text);
    begin
      -- In any case, save this word
      Words.Add (Word);
      if Str= "(" then
        Paren_Level := Paren_Level + 1;
      elsif Str= ")" then
        Paren_Level := Paren_Level - 1;
      elsif Str= ";" and then Paren_Level = 0 then
        -- ";" outside () and without "record" -> end of type
        -- Parse up to next significant word
        Parse_To_Ends (End_Criteria => Words.No_Word,
                       Level => Level,
                       Put_Comments => False,
                       Up_To_Next_Significant => True);
        Fix_Comment (Level);
        Output.Put_Line (Words.Concat, True, Level, True);
        Words.Reset;
        return;
      elsif Str = "access" then
        -- Access type to function or procedure, with args...
        Parse_To_End (Parser_Ada.Delimiter, ";", Level,
                      Put_Comments => False);
        Fix_Comment (Level);
        Output.Put_Line (Words.Concat, True, Level, True);
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
    Word := Parser_Ada.Multiparse.Get (True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Word.Text);
    begin
      -- In any case, save this word
      Words.Add (Word);
      if Str = "record" then
        -- This is the "record" of "end record;"
        exit;
      elsif Word.Lexic = Parser_Ada.Comment then
        -- Put comment
        Output.Put_Line (Get_Separators & Str, True, Level);
      end if;
    end;
  end loop;
  -- Then parse up to last ";" then line_feed
  Parse_To_End (Parser_Ada.Delimiter, ";", Level,
                Put_Comments => False);
  Fix_Comment (Level);
  Output.Put_Line (Words.Concat, True, Level, True);
  Words.Reset;

end Parse_Type;

