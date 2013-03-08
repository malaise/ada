with As.U;
with Common, Output, Words, Parse_To_Ends, Parse_To_End,
     Parser_Ada, Parse_Name, Fix_Comment, Put_Comments;

procedure Parse_Function (Level : in Natural;
                          Generated : in out Boolean) is
  Name, Args : As.U.Asu_Us;
  Has_Aspect : Boolean;
  Word : Parser_Ada.Word_Rec;
  In_Id : Boolean;
  Nb_Parent : Natural;
  use type Parser_Ada.Lexical_Kind_List;
begin

  -- Parse up to name, detect first parent
  Words.Add (Parser_Ada.Reserved_Word, "function");
  Parse_Name (Level, Name);

  -- Next (significant) word is either '(' or "return". Get it
  Parse_To_Ends (
      End_Criteria => (
          (Parser_Ada.Delimiter, As.U.Tus ("(")),
          (Parser_Ada.Reserved_Word, As.U.Tus ("return")) ),
      Level => Level,
      Put_Comments => True,
      Up_To_Next_Significant => False);
  Word := Words.Read;

  if Word.Text.Image = "(" then
    -- Like Parse_To_End ("return"); but
    -- store argument formal names in Args, separated by ", "
    In_Id := True;
    Nb_Parent := 1;
    loop
      Word := Parser_Ada.Multiparser.Get (True);
      Words.Add (Word);
      if Word.Text.Image = "(" then
        Nb_Parent := Nb_Parent + 1;
      elsif Word.Text.Image = ")" then
        if Nb_Parent = 0 then
           Common.Error (")");
        end if;
        -- End or arguments. Now parsing return type.
        Nb_Parent := Nb_Parent - 1;
      elsif In_Id and then Nb_Parent = 1
      and then Word.Lexic = Parser_Ada.Identifier then
        -- Append this argument name
        if Args.Length /= 0 then
          Args.Append (", ");
        end if;
        Args.Append (Word.Text);
      elsif Word.Text.Image = ":" and then Nb_Parent = 1 then
        -- End of argument formal names (entering in | out | inout | access ...)
        In_Id := False;
      elsif Word.Text.Image = "return" and then Nb_Parent = 0 then
        exit;
      elsif Word.Text.Image = ";" and then Nb_Parent = 1 then
        -- End of previous argument, expecting a new one
        In_Id := True;
      end if;
    end loop;
    -- Flush comments
    Put_Comments;
  end if;

  -- Parse return
  if Word.Text.Image = "return" then
    Parse_To_Ends (
      (1 => (Parser_Ada.Delimiter, As.U.Tus (";")),
       2 => (Parser_Ada.Reserved_Word, As.U.Tus ("with"))),
      Level, Put_Comments => False,
      Up_To_Next_Significant => False);
  else
    Common.Error (Word.Text.Image);
  end if;

  -- If a renames or generic instanciation or expresssion function
  --  then put as comment
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

  Has_Aspect := Words.Search (Parser_Ada.Reserved_Word, "with") /= 0;

  -- This is a "real" declaration: remove last ";" or " with"
  Generated := True;
  Words.Del;
  if Has_Aspect then
    Words.Del;
  end if;

  -- Output this and " is"
  Output.Put_Line (Words.Concat & " is", False, Level);
 -- Skip aspect if any
  if Has_Aspect then
    Parse_To_End (Parser_Ada.Delimiter, ";", Level);
  end if;
  Words.Reset;

  -- begin
  --   return <name> (<args>);
  -- end <name>;
  Output.Put_Line ("begin", False, Level, True);
  Output.Put ("return " & Name.Image, False, Level + 1, True);
  if Args.Length /= 0 then
    Output.Put (" (" & Args.Image & ")", False);
  end if;
  Output.Put_Line (";", False);
  Output.Put_Line ("end " & Name.Image & ";", False, Level, True);

end Parse_Function;

