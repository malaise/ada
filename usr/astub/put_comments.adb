with Ada.Strings.Unbounded;
with Ada_Parser;
with Common, Words, Output;
-- Output comments of what has been parsed 
procedure Put_Comments (Level : in Natural) is
  Index : Natural;
  Word : Words.Word_Rec;
  Prev_Lf : Boolean;
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

  -- Now remove succesive line_feeds
  Prev_Lf := False;
  Index := 1;
  for I in 1 .. Words.Length loop
    Word := Words.Read (Index);
    if Word.Lexic = Ada_Parser.Separator
    and then Word.Text = String'(Common.Line_Feed) then
      -- A line_feed
      if Prev_Lf then
        -- This one follows a line_feed: delete it
        Words.Del (Index);
        -- Next word will be at same Index
      else
        -- First line feed: keep it
        Prev_Lf := True;
        -- Next word
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

