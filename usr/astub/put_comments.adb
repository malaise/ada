with As.U; use As.U;
with Common, Words, Output, Parser_Ada;
-- Output comments and delete them and separators from words
-- Keep last indent
procedure Put_Comments is
  Index, Start : Natural;
  Word : Words.Word_Rec;
  Prev_Lf : Boolean;
  Prev_Index : Natural;
  use type Parser_Ada.Lexical_Kind_List;
  Text : Asu_Us;
begin
  -- Put comments with line feeds
  Index := 1;
  for I in 1 .. Words.Length loop
    Word := Words.Read (Index);
    if Word.Lexic = Parser_Ada.Comment then
      -- Go backwards to start of line or significant word
      Start := Index;
      loop
        Start := Start - 1;
        exit when Start = 0;
        Word := Words.Read (Start);
        exit when Word.Lexic /= Parser_Ada.Separator
        or else Word.Text = Asu_Us'(Common.Line_Feed);
      end loop;
      -- Concat indent and comment
      Start := Start + 1;
      Text := Tus ("");
      for J in Start .. Index loop
        -- Del and put.
        Word := Words.Get (Start);
        Text.Append (Word.Text);
      end loop;
      Output.Put_Line (Text.Image, False, 0);
      -- Next word will be at same Index
    else
      -- Next word
      Index := Index + 1;
    end if;
  end loop;

  -- Now remove succesive line_feeds
  -- If separated by separators, remove them as well
  Prev_Lf := False;
  Prev_Index := 0;
  Index := 1;
  for I in 1 .. Words.Length loop
    Word := Words.Read (Index);
    if Word.Lexic = Parser_Ada.Separator then
      if Word.Text = Asu_Us'(Common.Line_Feed) then
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

