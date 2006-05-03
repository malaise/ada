with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End;

procedure Parse_Task (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  In_Id : Boolean;
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

  -- Parse until "end" then ";", this skips the entries
  Parse_To_End (Level, "end", False, False);
  Parse_To_End (Level, ";", False, False);
  Words.Reset;

  -- Output "task <name> is"
  Output.Put_Line ("task " & Asu.To_String(Name) & " is", Level, False);
  -- begin
  --   null;
  -- end <name>;
  Output.Put_Line ("begin", Level, False);
  Output.Put_Line ("null", Level + 1, False);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";",
                   Level, False);
end Parse_Task;

