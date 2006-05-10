with Ada.Strings.Unbounded;
with Ada_Parser;
with Common, Words, Output;
-- Output comments of what has been parsed
procedure Put_Comments (Level : in Natural) is
  Index : Natural;
  Word : Words.Word_Rec;
  Prev_Lf : Boolean;
  Prev_Index : Natural;
  package Asu renames Ada.Strings.Unbounded;
  use type Ada_Parser.Lexical_Kind_List,
           Asu.Unbounded_String;
begin
  -- Put comments with line feeds
  Index := 1;
  for I in 1 .. Words.Length loop
    Word := Words.Read (Index);
    if Word.Lexic = Ada_Parser.Comment then
      -- Del and put. Next word will be at same Index
      Words.Del (Index);
      Output.Put_Line (Asu.To_String (Word.Text), False, Level);
    else
      -- Next word
      Index := Index + 1;
    end if;
  end loop;

  -- Now remove succesive line_feeds (even if separated by separators)
  Prev_Lf := False;
  Index := 1;
  for I in 1 .. Words.Length loop
    Word := Words.Read (Index);
    if Word.Lexic = Ada_Parser.Separator then
      if Word.Text = String'(Common.Line_Feed) then
        -- A line_feed
        if Prev_Lf then
          -- This one follows a line_feed (or is separated only by separators)
          -- delete all separators and this line_Feed
          for J in Prev_Index + 1 .. Index loop
            Words.Del (Prev_Index + 1);
          end loop;
          -- Next word will after Prev_Index
          Index := Prev_Index + 1;
        else
          -- First line feed: keep it
          Prev_Lf := True;
          Prev_Index := Index;
          -- Next word
          Index := Index + 1;
        end if;
      else
        -- A separator but not a line feed
        -- Leave Prev_Lf unchanged and go to next word
        Index := Index + 1;
      end if;
    else
      -- Not a line_feed
      Prev_Lf := False;
      -- Next word
      Index := Index + 1;
    end if;
  end loop;
end Put_Comments;

