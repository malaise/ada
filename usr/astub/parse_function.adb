with Ada.Strings.Unbounded;
with Text_Char;
with Common, Files, Output, Words, Parse_To_End, Parser_Ada, Parse_Name;

procedure Parse_Function (Level : in Natural;
                          Generated : in out Boolean) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Args : Asu.Unbounded_String;
  Word : Parser_Ada.Word_Rec;
  In_Parent, In_Id : Boolean;
  use type Parser_Ada.Lexical_Kind_List;
begin

  -- Parse up to name, detect first parent
  Words.Add (Parser_Ada.Reserved_Word, "function");
  Parse_Name (File, Level, Name);

  -- Name ended either by Sep or '(' or separator
  -- read separators until '(' or "return"
  Word := Words.Read;
  while Word.Lexic = Parser_Ada.Separator loop
    Word := Parser_Ada.Multiparse.Get (True);
    Words.Add (Word);
  end loop;

  if Asu.To_String (Word.Text) = "(" then
    -- Like Parse_To_End (";"); but
    -- store argument formal names in Args, separated by ", "
    In_Id := True;
    In_Parent := True;
    loop
      Word := Parser_Ada.Multiparse.Get (True);
      Words.Add (Word);
      if Asu.To_String (Word.Text) = "(" then
        Common.Error ("(");
      elsif Asu.To_String (Word.Text) = ")" then
        if In_Id or else not In_Parent then
           Common.Error (")");
        end if;
        -- End or arguments. Now parsing return type.
        In_Parent := False;
      elsif In_Id and then Word.Lexic = Parser_Ada.Identifier then
        -- Append this argument name
        if Asu.Length (Args) /= 0 then
          Asu.Append (Args, ", ");
        end if;
        Asu.Append (Args, Word.Text);
      elsif Asu.To_String (Word.Text) = ":" then
        -- End of argument formal names (entering in | out | inout ...)
        In_Id := False;
      elsif Asu.To_String (Word.Text) = ";" then
        if In_Parent then
          -- End of previous argument, expecting a new one
          In_Id := True;
        else
          -- ; out of (): the end
          exit;
        end if;
      end if;
    end loop;
  elsif Asu.To_String (Word.Text) = "return" then
    Parse_To_End (Parser_Ada.Delimiter, ";", Level);
  else
    Common.Error (Asu.To_String (Word.Text));
  end if;

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
  --   return <name> (<args>);
  -- end <name>;
  Output.Put_Line ("begin", False, Level);
  Output.Put ("return " & Asu.To_String (Name), False, Level + 1);
  if Asu.Length (Args) /= 0 then
    Output.Put (" (" & Asu.To_String (Args) & ")", False);
  end if;
  Output.Put_Line (";", False);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level);

end Parse_Function;

