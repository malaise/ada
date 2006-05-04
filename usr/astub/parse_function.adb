with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End, Parse_Name;

procedure Parse_Function (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Args, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  In_Parent, In_Id : Boolean;
  use type Ada_Parser.Lexical_Kind_List;
begin

  -- Parse up to name
  Words.Add ("function");
  Name := Ada.Strings.Unbounded.To_Unbounded_String (Parse_Name (File));

  -- Like Parse_To_End (";", False); but
  -- store argument formal names in Args, separated by ", "
  In_Parent := False;
  In_Id := False;
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    if Asu.To_String (Text) = Common.Line_Feed then
      Words.Add (Text);
    end if;
    if Asu.To_String (Text) = "(" then
      In_Parent := True;
      In_Id := True;
    elsif Asu.To_String (Text) = ")" then
      In_Parent := False;
    elsif In_Id and then Lexic = Ada_Parser.Identifier then
      -- Append this argument name
      if Asu.Length (Args) /= 0 then
        Asu.Append (Args, ", ");
      end if;
      Asu.Append (Args, Text);
    elsif Asu.To_String (Text) = ":" then
      -- End of argument formal names (entering in | out | inout ...)
      In_Id := False;
    elsif Asu.To_String (Text) = ";" then
      if In_Parent then
        -- End of previous argument, expecting a new one
        In_Id := True;
      else
        -- ; out of (): the end
        exit;
      end if;
    end if;
  end loop;

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
  --   return <name> (<args>);
  -- end <name>;
  Output.Put_Line ("begin", Level, False);
  Output.Put ("return " & Asu.To_String (Name), Level + 1, False);
  if Asu.Length (Args) /= 0 then
    Output.Put (" (" & Asu.To_String (Args) & ")", 0, False);
  end if;
  Output.Put_Line (";", 0, False);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";",
                   Level, False);
end Parse_Function;

