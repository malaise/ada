with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End, Parse_Name;

procedure Parse_Procedure (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Text : Asu.Unbounded_String;
begin

  -- Parse up to name and read lexic following it
  Words.Add (Ada_Parser.Reserved_Word, "procedure");
  Parse_Name (File, Level, Name);
  Text := Words.Read;

  -- Skip until last ';' (if not already got when parsing Name)
  if Asu.To_String (Text) /= ";" then
    Parse_To_End (Ada_Parser.Delimiter, ";", Level, Asu.To_String (Text) = "(");
  end if;

  -- If a renames or generic instanciation, put as comment
  if Words.Search (Ada_Parser.Reserved_Word, "renames") /= 0 then
    Output.Put_Line (Words.Concat, True, Level);
    Words.Reset;
    return;
  end if;
  if Words.Search (Ada_Parser.Reserved_Word, "is") /= 0 then
    Output.Put_Line (Words.Concat, True, Level);
    Words.Reset;
    return;
  end if;

  -- This is a "real" declaration: remove last ";"
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

