with Ada.Strings.Unbounded;
with Text_Char;
with Common, Files, Output, Words, Parser_Ada, Parse_To_End, Parse_Name;

procedure Parse_Procedure (Level : in Natural;
                           Generated : in out Boolean) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name : Asu.Unbounded_String;
begin

  -- Parse name
  Words.Add (Parser_Ada.Reserved_Word, "procedure");
  Parse_Name (File, Level, Name);

  -- Skip until last ';'
  Parse_To_End (Parser_Ada.Delimiter, ";", Level);

  -- If a renames or generic instanciation, put as comment
  if Words.Search (Parser_Ada.Reserved_Word, "renames") /= 0 then
    Output.Put_Line (Words.Concat, True, Level);
    Words.Reset;
    return;
  end if;
  if Words.Search (Parser_Ada.Reserved_Word, "is") /= 0 then
    Output.Put_Line (Words.Concat, True, Level);
    Words.Reset;
    return;
  end if;

  -- This is a "real" declaration: remove last ";"
  Generated := True;
  Words.Del;

  -- Output this and " is"
  Output.Put_Line (Words.Concat & " is", False, Level);
  Words.Reset;
  -- begin
  --   null;
  -- end <name>;
  Output.Put_Line ("begin", False, Level);
  Output.Put_Line ("null;", False, Level + 1);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";",
                   False, Level);
end Parse_Procedure;

