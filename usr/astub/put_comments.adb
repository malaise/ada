with Ada.Strings.Unbounded;
with Ada_Parser;
with Common, Words, Output;
-- Output comments that have been parsed 
procedure Put_Comments (Level : in Natural) is
  Word : Words.Word_Rec;
  use type  Ada_Parser.Lexical_Kind_List;
  package Asu renames Ada.Strings.Unbounded;
begin
  -- Check text read. Should be only separators and comments 
  -- Put comments with line feeds
  for I in 1 .. Words.Length loop
    Word := Words.Get (1);
    if Word.Lexic /= Ada_Parser.Comment then
      Output.Put_Line (Asu.To_String (Word.Text), False, Level);
    elsif Word.Lexic /= Ada_Parser.Separator then
      Common.Error (Asu.To_String (Word.Text));
    end if;
  end loop;
end Put_Comments;

