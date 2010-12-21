with As.U; use As.U;
with Common, Output, Words,  Parser_Ada, Parse_To_End;

procedure Parse_Task (Level : in Natural) is
  Word : Parser_Ada.Word_Rec;
  Name : Asu_Us;
  use type Parser_Ada.Lexical_Kind_List;
begin
  -- Read until task name, skip "type"
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    if Word.Lexic = Parser_Ada.Reserved_Word
    and then Word.Text.Image = "type" then
      -- Skip type
      null;
    elsif Word.Lexic = Parser_Ada.Identifier then
      -- Identifier => task name
      Name := Word.Text;
      exit;
    elsif Word.Lexic = Parser_Ada.Comment then
      -- Put comment
      Output.Put_Line (Word.Text.Image, False, Level);
    elsif Word.Lexic = Parser_Ada.Separator then
      -- Skip separator
      null;
    else
      Common.Error (Word.Text.Image);
    end if;
  end loop;

  -- Skip until "is", this skips the disciminant
  Parse_To_End (Parser_Ada.Reserved_Word, "is", Level);
  Words.Reset;
  -- Prepare Output "task <name> is begin"
  Output.Put_Line ("task body " & Name.Image & " is",
                   False, Level, True);
  Output.Put_Line ("begin", False, Level, True);

  -- Parse until "end", display the entries as comment
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    if Word.Lexic = Parser_Ada.Comment then
      Output.Put_Line (Word.Text.Image, False, Level + 1, True);
    elsif Word.Lexic = Parser_Ada.Separator then
      -- Skip separators
      null;
    elsif Word.Text.Image = "end" then
      -- End of this task
      exit;
    elsif Word.Text.Image = "entry" then
      -- Entry
      Words.Add (Word);
      Parse_To_End (Parser_Ada.Delimiter, ";", Level + 1);
      Output.Put_Line (Words.Concat, True, Level + 1, True);
    elsif Word.Text.Image = "not" then
      -- Propagate [ not ] overriding
      Words.Add (Word);
    elsif Word.Text.Image = "overriding" then
      Words.Add (Word);
    else
      Common.Error (Word.Text.Image);
    end if;
  end loop;

  -- Skip up to end of task
  Parse_To_End (Parser_Ada.Delimiter, ";", Level);
  Words.Reset;

  -- begin
  --   null;
  -- end <name>;
  Output.Put_Line ("null;", False, Level + 1, True);
  Output.Put_Line ("end " & Name.Image & ";", False, Level, True);
end Parse_Task;

