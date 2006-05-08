with Ada.Strings.Unbounded;
with Ada_Parser;
with Common, Words, Output;
-- Output comments of what has been parsed 
procedure Put_Comments (Level : in Natural) is
  Index : Natural;
  Word : Words.Word_Rec;
  use type  Ada_Parser.Lexical_Kind_List;
  package Asu renames Ada.Strings.Unbounded;
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
end Put_Comments;

