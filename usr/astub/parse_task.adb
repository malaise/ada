with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End;

procedure Parse_Task (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
begin
  -- Read until task name, skip "type"
  loop
    Ada_Parser.Parse_Next (File, Name, Lexic, True);
    if Lexic = Ada_Parser.Reserved_Word
    and then Asu.To_String (Name) = "type" then
      -- Skip type
      null;
    elsif Lexic = Ada_Parser.Identifier
    or else Lexic /= Ada_Parser.String_Literal then
      -- Identifier => task name
      exit;
    elsif Lexic = Ada_Parser.Comment then
      -- Put comment
      Output.Put_Line (Asu.To_String (Name), False, Level);
    elsif Lexic = Ada_Parser.Separator then
      -- Skip separator
      null;
    else
      Common.Error (Asu.To_String (Name));
    end if;
  end loop;

  -- Skip until "is", this skips the disciminant
  Parse_To_End ("is", False);
  Words.Reset;
  -- Output "task <name> is begin"
  Output.Put_Line ("task body " & Asu.To_String(Name) & " is", False, Level);
  Output.Put_Line ("begin", False, Level);

  -- Parse until "end", display the entries as comment
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    if Lexic = Ada_Parser.Comment then
      Output.Put_Line (Asu.To_String (Text), False, Level + 1);
    elsif Lexic = Ada_Parser.Separator then
      -- Skip separators
      null;
    elsif Asu.To_String (Text) = "end" then
      -- End of this task
      exit;
    elsif Asu.To_String (Text) = "entry" then
      -- entry
      Parse_To_End (";");
      Output.Put_Line (Words.Concat, True, Level + 1);
    else
      Common.Error (Asu.To_String (Text));
    end if;
  end loop;

  -- Skip up to end of task
  Parse_To_End (";", False);
  Words.Reset;

  -- begin
  --   null;
  -- end <name>;
  Output.Put_Line ("null;", False, Level + 1);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level);
end Parse_Task;

