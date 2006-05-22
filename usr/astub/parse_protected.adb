with Ada.Strings.Unbounded;
with Text_Char;
with Common, Files, Output, Words, Parser_Ada, Parse_To_End,
     Parse_Procedure, Parse_Function, Parse_Entry;

procedure Parse_Protected (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
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

  -- Skip until "is", put comments
  Parse_To_End (Parser_Ada.Reserved_Word, "is", Level);
  Words.Reset;

  -- Output this and " is"
  Output.Put_Line ("protected body " & Asu.To_String (Name)
                 & " is", False, Level);

  -- Loop until expected word
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Word.Text);
    begin
      if Word.Lexic = Parser_Ada.Comment then
        Output.Put (Str, False, Level + 1);
      elsif Word.Lexic = Parser_Ada.Separator then
        -- Skip separators but line_feed
        if Str = Common.Line_Feed then
          -- Put New line now
          Output.New_Line;
        end if;
      elsif Str = "end" then
        -- End of this protected
        exit;
      elsif Str = "procedure" then
        Parse_Procedure (Level + 1, Dummy);
      elsif Str = "function" then
        Parse_Function (Level + 1, Dummy);
      elsif Str = "entry" then
        Parse_Entry (Level + 1);
      elsif Str = "private" then
        -- Put "private" as a comment
        Output.Put_Line (Str, True, Level);
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

