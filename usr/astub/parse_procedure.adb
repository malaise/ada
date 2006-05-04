with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End, Parse_Name;

procedure Parse_Procedure (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  Name : Ada.Strings.Unbounded.Unbounded_String;
begin

  -- Parse up to name
  Words.Add ("procedure");
  Name := Ada.Strings.Unbounded.To_Unbounded_String (Parse_Name (File));

  -- Skip until last ';'
  Parse_To_End (";", False);

  -- If a renames or generic instanciation, put as comment
  if Words.Search ("renames") /= 0 then
    Output.Put_Line (Words.Get, Level, True);
    Words.Reset;
    return;
  end if;
  if Words.Search ("is") /= 0 then
    Output.Put_Line (Words.Get, Level, True);
    Words.Reset;
    return;
  end if;

  -- This is a "real" declaration: remove las ";"
  Words.Del;

  -- Output this and " is"
  Output.Put_Line (Words.Get & " is", Level, False);
  Words.Reset;
  -- begin
  --   null;
  -- end <name>;
  Output.Put_Line ("begin", Level, False);
  Output.Put_Line ("null;", Level + 1, False);
  Output.Put_Line ("end " & Ada.Strings.Unbounded.To_String (Name) & ";",
                   Level, False);
end Parse_Procedure;

