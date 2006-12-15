with Ada.Strings.Unbounded;
with Text_Char;
with Common, Files, Output, Words, Parse_To_End, Parse_To_Ends,
     Parser_Ada, Parse_Name, Fix_Comment;

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

  -- Next (significant) word is either '(' or "return". Get it
  Parse_To_Ends (
      End_Criteria => (
          (Parser_Ada.Delimiter, Asu.To_Unbounded_String("(")),
          (Parser_Ada.Reserved_Word, Asu.To_Unbounded_String("return")) ),
      Level => Level,
      Put_Comments => True,
      Up_To_Next_Significant => False);
  Word := Words.Read;

  if Asu.To_String (Word.Text) = "(" then
    -- Like Parse_To_End ("return"); but
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
      elsif Asu.To_String (Word.Text) = "return" then
        exit;
      elsif Asu.To_String (Word.Text) = ";" then
        if In_Parent then
          -- End of previous argument, expecting a new one
          In_Id := True;
        else
          -- ; out of () but before "return"
          Common.Error (Asu.To_String (Word.Text));
        end if;
      end if;
    end loop;
    -- Flush comments
    Put_Comments;
  end if;

  -- Parse return
  if Asu.To_String (Word.Text) = "return" then
    Parse_To_End (Parser_Ada.Delimiter, ";", Level, Put_Comments => False,
                  Up_To_Next_Significant => False);
  else
    Common.Error (Asu.To_String (Word.Text));
  end if;

  -- If a renames or generic instanciation, put as comment
  if Words.Search (Parser_Ada.Reserved_Word, "renames") /= 0 then
    Fix_Comment (Level);
    Output.Put_Line (Words.Concat, True, Level, True);
    Words.Reset;
    return;
  end if;
  if Words.Search (Parser_Ada.Reserved_Word, "is") /= 0 then
    Fix_Comment (Level);
    Output.Put_Line (Words.Concat, True, Level, True);
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
  Output.Put_Line ("begin", False, Level, True);
  Output.Put ("return " & Asu.To_String (Name), False, Level + 1, True);
  if Asu.Length (Args) /= 0 then
    Output.Put (" (" & Asu.To_String (Args) & ")", False);
  end if;
  Output.Put_Line (";", False);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level, True);

end Parse_Function;

