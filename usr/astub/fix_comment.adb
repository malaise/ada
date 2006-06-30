-- Because we put type text, constant, pragma, renames...
--  in comment (appending an indented "--")
-- Try to remove any " " at beginning of lines (of Words)
with Ada.Strings.Unbounded;
with Common, Words, Parser_Ada, Output;
procedure Fix_Comment (Level : in Natural) is
  Index, Length : Natural;
  Word, Space : Words.Word_Rec;
  Ok : Boolean;
  use type Ada.Strings.Unbounded.Unbounded_String, Words.Word_Rec;
begin
  -- Get number of spaces to look for and exit if 0
  Length := Output.Get_Indent(Level)'Length;
  if Length = 0 then
    return;
  end if;

  Index := 0;
  loop
    -- Locate leading " " or Line_Feed & " "
    if Index = 0 then
      -- Also process leading " ", as if there was a leading Line_Feed
      Word.Lexic := Parser_Ada.Separator;
      Word.Text := Common.Line_Feed;
    else
      Word := Words.Read (Index);
    end if;
    -- Exit when no more word
    exit when Ada.Strings.Unbounded.To_String (Word.Text) = "";
    if Word.Text = String'(Common.Line_Feed) then
      -- Read Length following words (Words returns "" if no more word)
      -- Stop if not a space
      for I in 1 .. Length loop
        Space :=  Words.Read (Index + I);
        Ok := Ada.Strings.Unbounded.To_String (Space.Text) = " ";
        exit when not Ok;
      end loop;
      if Ok then
        -- A line feed followed by (at least) Length spaces: remove the spaces
        for I in 1 .. Length loop
          Words.Del (Index + 1);
        end loop;
      end if;
    end if;
    -- In all cases, go to next word
    Index := Index + 1;
  end loop;
end Fix_Comment;

