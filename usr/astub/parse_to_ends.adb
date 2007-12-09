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
                         Up_To_Next_Significant : in Boolean) is
  Parent : Natural := 0;
    Word : Parser_Ada.Word_Rec;
  use type Parser_Ada.Lexical_Kind_List,
           Ada.Strings.Unbounded.Unbounded_String;
begin
  -- Loop until End_Char outside parentheses (Level = 0)
  Parse_Word:
  loop
    -- Empty criteria matches anything
    exit when End_Criteria'Length = 0;

    -- Read next lexical element, cannot return ""
    Word := Parser_Ada.Multiparse.Get (True);
    -- In any case, save it
    Words.Add (Word);

    -- Check if criteria match
    if Parent = 0 then
      -- Not in parentheses
      -- Check current word vs each criteria
      Check_Criteria:
      for I in End_Criteria'Range loop
        if Word.Lexic = End_Criteria(I).Lexic then
          -- Lexic match, check text if necessary
          if Word.Lexic = Parser_Ada.Reserved_Word
          or else Word.Lexic = Parser_Ada.Delimiter then
            -- Text must match for reserved word or delimiter
            if Word.Text = End_Criteria(I).Text then
              exit Parse_Word;
            end if;
          elsif Word.Lexic = Parser_Ada.Separator then
            if Word.Text = String'(Common.Line_Feed) then
              if End_Criteria(I).Text = String'(Common.Line_Feed) then
                -- Line feed matches line_feed
                exit Parse_Word;
              end if;
            else
              if End_Criteria(I).Text /= String'(Common.Line_Feed) then
                -- Not line feed matches not line_feed
                exit Parse_Word;
              end if;
            end if;
          else
            -- Identifier or Literal or Comment, lexic match is enough
            exit Parse_Word;
          end if;
        end if;
      end loop Check_Criteria;
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
  end loop Parse_Word;

  if not Up_To_Next_Significant then
    -- Done if no need to parse up to next significant lexic
    return;
  end if;

  -- Loop until next significant word
  Parser_Ada.Multiparse.Start_Recording;
  loop
    -- Read next lexical element or end of file
    Word := Parser_Ada.Multiparse.Get (False);
    -- In any case, save it
    Words.Add (Word);
    if Word.Lexic = Parser_Ada.Comment and then Put_Comments then
      -- Put this comment with preceeding separators
      Words.Del;
      Output.Put_Line (
         Get_Separators & Ada.Strings.Unbounded.To_String (Word.Text),
         True, Level);
    end if;
    -- Exit when significant word or line_feed
    exit when Word.Lexic /= Parser_Ada.Comment
    and then Word.Lexic /= Parser_Ada.Separator;
    exit when Word.Text = String'(Common.Line_Feed);
  end loop;

  -- Unget this significant word
  Words.Del;
  Parser_Ada.Multiparse.Unget;
  Parser_Ada.Multiparse.Stop_Recording;

end Parse_To_Ends;

