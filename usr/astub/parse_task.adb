with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
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

  -- Skip until "is", this skips the disciminant
  Parse_To_End ("is", False);
  Words.Reset;
  -- Output "task <name> is begin"
  Output.Put_Line ("task body " & Asu.To_String(Name) & " is", Level, False);
  Output.Put_Line ("begin", Level, False);

  -- Parse until "end", display the entries as comment
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    if Lexic = Ada_Parser.Comment then
      Output.Put_Line (Asu.To_String (Text), Level + 1, False);
    elsif Lexic = Ada_Parser.Separator then
      -- Skip separators
      null;
    elsif Asu.To_String (Text) = "end" then
      -- End of this task
      exit;
    else
      -- Unexpected, word (including entry).
      -- Parse to end as comment
      Words.Add (Text);
      Parse_To_End (";", True, Level + 1);
    end if;
  end loop;

  -- Skip up to end of task
  Parse_To_End (";", False);
  Words.Reset;

  -- begin
  --   null;
  -- end <name>;
  Output.Put_Line ("null;", Level + 1, False);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";",
                   Level, False);
end Parse_Task;

