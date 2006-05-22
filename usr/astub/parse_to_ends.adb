-- Go on parsing current input file up to matching any of End_Criteria
--  that is not within parentheses.
-- For identifier/literal/comment, any text will match
-- For reserved word/delimiter, text must match
-- For separator Line_fine must match line_feed
--   space or htab matches space or htab
with Ada.Strings.Unbounded;
with Text_Line, Text_Char;
with Files, Common, Words, Get_Separators, Output, Parser_Ada;

procedure Parse_To_Ends (End_Criteria : in Words.Word_Array;
                         Level : in Natural;
                         Put_Comments : in Boolean;
                         Already_In_Parent : in Boolean) is
  Parent : Natural := 0;
  File : constant Text_Char.File_Type := Files.In_File;
  Word : Parser_Ada.Word_Rec;
  use type Parser_Ada.Lexical_Kind_List,
           Ada.Strings.Unbounded.Unbounded_String;
begin
  -- If already in parentheses, increase level
  if Already_In_Parent then
    Parent := 1;
  end if;

  -- Loop until End_Char outside parnetheses (Level = 0)
  loop
    -- Read next lexical element, cannot return ""
    Word := Parser_Ada.Multiparse.Get (True);
    -- In any case, save it
    Words.Add (Word);

    -- Check if criteria match
    if Parent = 0 then
      -- Not in parentheses
      for I in End_Criteria'Range loop
        if Word.Lexic = End_Criteria(I).Lexic then
          -- Lexic match, check text if necessary
          if Word.Lexic = Parser_Ada.Reserved_Word
          or else Word.Lexic = Parser_Ada.Delimiter then
            -- Text must match for reserved word or delimiter
            if Word.Text = End_Criteria(I).Text then
              return;
            end if;
          elsif Word.Lexic = Parser_Ada.Separator then
            if Word.Text = String'(Common.Line_Feed) then
              if End_Criteria(I).Text = String'(Common.Line_Feed) then
                -- Line feed must match line_feed
                return;
              end if;
            else
              if End_Criteria(I).Text /= String'(Common.Line_Feed) then
                -- Not line feed must match not line_feed
                return;
              end if;
            end if;
          else
            -- Identifier or Literal or Comment, lexic match is enough
            return;
          end if;
        end if;
      end loop;
    end if;

    -- Handle specific words, put comments
    if Ada.Strings.Unbounded.To_String (Word.Text) = "(" then
      -- keep Level of parentheses
      Parent := Parent + 1;
    elsif Ada.Strings.Unbounded.To_String (Word.Text) = ")" then
      Parent := Parent - 1;
    elsif Word.Lexic = Parser_Ada.Comment and then Put_Comments then
      -- Put this comment with preceeding separators
      Words.Del;
      Output.Put_Line (
         Get_Separators & Ada.Strings.Unbounded.To_String (Word.Text),
         True, Level);
    end if;
  end loop;

end Parse_To_Ends;

