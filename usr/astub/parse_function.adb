with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End, Parse_Name;

procedure Parse_Function (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Args, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  In_Parent, In_Id : Boolean;
  use type Ada_Parser.Lexical_Kind_List;
begin

  -- Parse up to name, detect first parent
  Words.Add (Ada_Parser.Reserved_Word, "function");
  Parse_Name (File, Level, Name);
  Text := Words.Read;

  if Asu.To_String (Text) /= ";" then
    -- Like Parse_To_End (";"); but
    -- store argument formal names in Args, separated by ", "
    In_Id := False;
    loop
      Ada_Parser.Parse_Next (File, Text, Lexic, True);
      Words.Add (Lexic, Text);
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
  --   return <name> (<args>);
  -- end <name>;
  Output.Put_Line ("begin", False, Level);
  Output.Put ("return " & Asu.To_String (Name), False, Level + 1);
  if Asu.Length (Args) /= 0 then
    Output.Put (" (" & Asu.To_String (Args) & ")", False);
  end if;
  Output.Put_Line (";", False);
  Output.Put ("end " & Asu.To_String (Name) & ";", False, Level);

end Parse_Function;

