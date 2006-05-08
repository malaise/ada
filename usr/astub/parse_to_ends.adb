-- Go on parsing current input file up to matching any of End_Criteria
--  that is not within parentheses.
-- For identifier/literal/comment/separator, any text will match
-- For reserved word/delimiter, text must match
with Ada.Strings.Unbounded;
with Text_Line, Text_Char, Ada_Parser;
with Files, Words;

procedure Parse_To_Ends (End_Criteria : in Words.Word_Array;
                        Already_In_Parent : Boolean := False) is
  Level : Natural := 0;
  File : constant Text_Char.File_Type := Files.In_File;
  Text : Ada.Strings.Unbounded.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List,
           Ada.Strings.Unbounded.Unbounded_String;
begin
  -- If already in parentheses, increase level
  if Already_In_Parent then
    Level := 1;
  end if;

  -- Loop until End_Char outside parnetheses (Level = 0)
  loop
    -- Read next lexical element, cannot return ""
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    -- In any case, save it
    Words.Add (Lexic, Text);

    if Ada.Strings.Unbounded.To_String (Text) = "(" then
      -- keep Level of parentheses
      Level := Level + 1;
    elsif Ada.Strings.Unbounded.To_String (Text) = ")" then
      Level := Level - 1;
    elsif Level = 0 then
      -- Not in parentheses
      for I in End_Criteria'Range loop
        if Lexic = End_Criteria(I).Lexic then
          -- Lexic match, check text if necessary
          if (Lexic /= Ada_Parser.Reserved_Word
          and then Lexic /= Ada_Parser.Delimiter)
          or else Text = End_Criteria(I).Text then
            -- Match
            return;
          end if;
        end if;
      end loop;
    end if;
  end loop;

end Parse_To_Ends;

