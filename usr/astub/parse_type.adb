with Ada.Strings.Unbounded;
with Text_Char;
with Common, Files, Output, Words, Parser_Ada, Parse_To_End;

-- Parse type definition or representation clause
procedure Parse_Type (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  Word : Parser_Ada.Word_Rec;
  Paren_Level : Natural := 0;
  use type Parser_Ada.Lexical_Kind_List,
           Ada.Strings.Unbounded.Unbounded_String,
           Words.Word_Rec;
  -- Because we put type text in comment (appending "--")
  -- Try to remove any "  " at beginning of lines (of Words)
  procedure Fix_Indent is
    Index : Natural;
    Word, S1, S2 : Words.Word_Rec;
  begin
    Index := 0;
    loop
      -- Locate leading "  " or Line_Feed & "  "
      if Index = 0 then
        -- Also process leading " ", as if there was a leading Line_Feed
        Word.Lexic := Parser_Ada.Separator;
        Word.Text := Common.Line_Feed;
      else
        Word := Words.Read (Index);
      end if;
      -- Exit when no more word
      exit when Ada.Strings.Unbounded.To_String (Word.Text) = "";
      if Word.Text = String'(Common.Line_Feed) then
        -- Read 2 following words (Words returns "" if no more word)
        S1 :=  Words.Read (Index + 1);
        S2 :=  Words.Read (Index + 2);
        if Ada.Strings.Unbounded.To_String (S1.Text) = " "
        and then S1 = S2 then
          -- A line feed followed by (at least) two spaces: remove 2 spaces
          Words.Del (Index + 1);
          Words.Del (Index + 1);
        end if;
      end if;
      -- In all cases, go to next word
      Index := Index + 1;
    end loop;
  end Fix_Indent;

begin

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
        Fix_Indent;
        Output.Put_Line (Words.Concat, True, Level);
        Words.Reset;
        return;
      elsif Str = "access" then
        -- Access type to function or procedure, with args...
        Parse_To_End (Parser_Ada.Delimiter, ";", Level,
                      Put_Comments => False);
        Fix_Indent;
        Output.Put_Line (Words.Concat, True, Level);
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
  Fix_Indent;
  Output.Put_Line (Words.Concat, True, Level);
  Words.Reset;

end Parse_Type;

