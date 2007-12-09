with Ada.Strings.Unbounded;
with Text_Char;
with Common, Output, Words,  Parser_Ada, Parse_To_End, Fix_Comment;

procedure Parse_Task (Level : in Natural) is
    package Asu renames Ada.Strings.Unbounded;
  Word : Parser_Ada.Word_Rec;
  Name : Asu.Unbounded_String;
  use type Parser_Ada.Lexical_Kind_List;
begin
  -- Read until task name, skip "type"
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    if Word.Lexic = Parser_Ada.Reserved_Word
    and then Asu.To_String (Word.Text) = "type" then
      -- Skip type
      null;
    elsif Word.Lexic = Parser_Ada.Identifier then
      -- Identifier => task name
      Name := Word.Text;
      exit;
    elsif Word.Lexic = Parser_Ada.Comment then
      -- Put comment
      Output.Put_Line (Asu.To_String (Word.Text), False, Level);
    elsif Word.Lexic = Parser_Ada.Separator then
      -- Skip separator
      null;
    else
      Common.Error (Asu.To_String (Word.Text));
    end if;
  end loop;

  -- Skip until "is", this skips the disciminant
  Parse_To_End (Parser_Ada.Reserved_Word, "is", Level);
  Words.Reset;
  -- Prepare Output "task <name> is begin"
  Output.Put_Line ("task body " & Asu.To_String(Name) & " is",
                   False, Level, True);
  Output.Put_Line ("begin", False, Level, True);

  -- Parse until "end", display the entries as comment
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    if Word.Lexic = Parser_Ada.Comment then
      Output.Put_Line (Asu.To_String (Word.Text), False, Level + 1, True);
    elsif Word.Lexic = Parser_Ada.Separator then
      -- Skip separators
      null;
    elsif Asu.To_String (Word.Text) = "end" then
      -- End of this task
      exit;
    elsif Asu.To_String (Word.Text) = "entry" then
      -- Entry
      Words.Add (Parser_Ada.Reserved_Word, "entry");
      Parse_To_End (Parser_Ada.Delimiter, ";", Level + 1);
      Output.Put_Line (Words.Concat, True, Level + 1, True);
    elsif Asu.To_String (Word.Text) = "not" then
      -- Skip "not overriding" and a separator
      Words.Add (Word);
      Parse_To_End (Parser_Ada.Reserved_Word, "overriding", Level + 1,
                    Up_To_Next_Significant => False);
      Fix_Comment (Level + 1);
      Output.Put_Line (Words.Concat, True, Level + 1, False);
      Words.Reset;
      Output.Put ("", False, Level + 1, True);
      Word := Parser_Ada.Multiparse.Get (True);
    elsif Asu.To_String (Word.Text) = "overriding" then
      -- Skip "overriding" and a separator
      Words.Add (Word);
      Fix_Comment (Level + 1);
      Output.Put_Line (Words.Concat, True, Level + 1, False);
      Words.Reset;
      Output.Put ("", False, Level + 1, True);
      Word := Parser_Ada.Multiparse.Get (True);
    else
      Common.Error (Asu.To_String (Word.Text));
    end if;
  end loop;

  -- Skip up to end of task
  Parse_To_End (Parser_Ada.Delimiter, ";", Level);
  Words.Reset;

  -- begin
  --   null;
  -- end <name>;
  Output.Put_Line ("null;", False, Level + 1, True);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level, True);
end Parse_Task;

