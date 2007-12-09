with Ada.Strings.Unbounded;
with Text_Char;
with Common, Output, Words, Parser_Ada, Parse_To_End,
     Parse_Procedure, Parse_Function, Parse_Entry;

procedure Parse_Protected (Level : in Natural) is
    package Asu renames Ada.Strings.Unbounded;
  Name : Asu.Unbounded_String;
  Word : Parser_Ada.Word_Rec;
  Dummy : Boolean := True;
  use type Parser_Ada.Lexical_Kind_List;
begin

  -- Read until protected name, skip "type"
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    declare
      Str : constant String := Asu.To_String (Word.Text);
    begin
      if Word.Lexic = Parser_Ada.Comment then
        -- Put comment or separator
        Output.Put_Line (Str, False);
      elsif Str = "type" then
        -- Skip type
        null;
      elsif Word.Lexic = Parser_Ada.Identifier then
        -- Got protected name
        Name := Word.Text;
        exit;
      elsif Word.Lexic = Parser_Ada.Separator then
        -- Skip separators
        null;
      else
        -- Unexpected word
        Common.Error (Asu.To_String (Name));
      end if;
    end;
  end loop;


  -- Output this and " is"
  Output.Put_Line (Words.Concat & "protected body " & Asu.To_String (Name)
                 & " is", False, Level);

  -- Skip until "is", put comments
  Parse_To_End (Parser_Ada.Reserved_Word, "is", Level);
  Words.Reset;

  -- Loop until expected word
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Word.Text);
    begin
      if Word.Lexic = Parser_Ada.Comment then
        Output.Put (Str, False, Level + 1);
      elsif Word.Lexic = Parser_Ada.Separator then
        -- Put separators
        Output.Put (Asu.To_String (Word.Text), False);
      elsif Str = "end" then
        -- End of this protected
        exit;
      elsif Str = "procedure" then
        Parse_Procedure (Level + 1, Dummy);
      elsif Str = "function" then
        Parse_Function (Level + 1, Dummy);
      elsif Str = "entry" then
        Parse_Entry (Level + 1);
      elsif Str = "not" then
        -- Skip "not overriding" and a separator
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Reserved_Word, "overriding", Level + 1,
                      Up_To_Next_Significant => False);
        Fix_Comment (Level + 1);
        Output.Put_Line (Words.Concat, True, Level + 1, False);
        Words.Reset;
        Output.Put ("", False, Level + 1, True);
        Word := Parser_Ada.Multiparse.Get (True);
      elsif Str = "overriding" then
        -- Skip "overriding" and a separator
        Words.Add (Word);
        Fix_Comment (Level + 1);
        Output.Put_Line (Words.Concat, True, Level + 1, False);
        Words.Reset;
        Output.Put ("", False, Level + 1, True);
        Word := Parser_Ada.Multiparse.Get (True);
      elsif Str = "private" then
        -- Put "private" as a comment
        Output.Put_Line (Str, True, Level);
      elsif Str = "new" then
        -- Parse until "with" as comment
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Reserved_Word, "with", Level + 1);
        Output.Put (Words.Concat, True, Level, True);
        Words.Reset;
      else
        -- Unexpected, word. Parse to end as comment
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Delimiter, ";", Level + 1);
        Output.Put (Words.Concat, True, Level);
        Words.Reset;
      end if;
    end;
  end loop;

  -- Skip up to last ";"
  Parse_To_End (Parser_Ada.Delimiter, ";", Level);
  Words.Reset;

  -- end <name>;
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level);
end Parse_Protected;

