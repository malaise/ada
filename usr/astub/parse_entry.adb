with Ada.Strings.Unbounded;
with Common,  Output, Words, Parser_Ada;

procedure Parse_Entry (Level : in Natural) is
  package Asu renames Ada.Strings.Unbounded;
  Name, Family, Last_Id : Asu.Unbounded_String;
  Word : Parser_Ada.Word_Rec;
  In_Id, In_Parent : Boolean;
  use type Parser_Ada.Lexical_Kind_List;
begin
  -- Output pending words
  Output.Put (Words.Concat, False, Level);
  Words.Reset;

  -- Read until entry name
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    Words.Add (Word);
    exit when Word.Lexic /= Parser_Ada.Separator;
  end loop;
  if Word.Lexic /= Parser_Ada.Identifier
  and then Word.Lexic /= Parser_Ada.String_Literal then
    Common.Error (Asu.To_String (Word.Text));
  end if;

  -- Put "entry <name>"
  Name := Words.Get;
  Words.Reset;
  Output.Put ("entry " & Asu.To_String (Name), False, Level);

  -- Parse family and arguments, store Family
  -- Store arguments lexical elements in words
  In_Parent := False;
  In_Id := False;
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    Words.Add (Word);
    if Asu.To_String (Word.Text) = "(" then
      In_Parent := True;
      In_Id := True;
    elsif Asu.To_String (Word.Text) = ")" then
      In_Parent := False;
      if In_Id then
        -- Identifier was not followed by ':', it was the family
        Family := Last_Id;
        Words.Reset;
      end if;
    end if;
    if In_Id and then Word.Lexic = Parser_Ada.Identifier then
      -- Save this adentifier, it might be the entry family
      Last_Id := Word.Text;
    elsif Asu.To_String (Word.Text) = ":" then
      -- End of argument formal names (entering in | out | inout ...)
      In_Id := False;
    elsif Asu.To_String (Word.Text) = ";" then
      if In_Parent then
        -- End of previous argument, expecting a new one
        In_Id := True;
      else
        -- Outside (), this ends the entry definition
        exit;
      end if;
    end if;
  end loop;

  -- Remove last ";"
  Words.Del;

  -- Put Family if set
  if Asu.Length (Family) /= 0 then
    Output.Put (" (for I in " & Asu.To_String (Family) & ")", False);
  end if;
  -- Put Args if set
  if Words.Length /= 0 then
    -- Delete last saved ;
    Output.Put (Words.Concat, False);
  end if;
  Words.Reset;
  Output.Put_Line (" when True is", False);

  -- begin
  --   return <name> (<args>);
  -- end <name>;
  Output.Put_Line ("begin", False, Level, True);
  Output.Put_Line ("null;", False, Level + 1, True);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level, True);
end Parse_Entry;

