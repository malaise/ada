-- Go on parsing current input file up to the next End_String
--  that is not within parentheses.
-- End_String empty means anything not a separator nor a comment.
with Ada.Strings.Unbounded;
with Text_Line, Text_Char, Ada_Parser;
with Files, Words;

procedure Parse_To_End (End_String : in String;
                        Already_In_Parent : Boolean := False) is
  Level : Natural := 0;
  File : constant Text_Char.File_Type := Files.In_File;
  Text : Ada.Strings.Unbounded.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
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
      if Ada.Strings.Unbounded.To_String (Text) = End_String
      or else (End_String = "" and then Lexic /= Ada_Parser.Separator
                               and then Lexic /= Ada_Parser.Comment) then
        -- End_String at parentheses level 0
        return;
      end if;
    end if;
  end loop;

end Parse_To_End;

