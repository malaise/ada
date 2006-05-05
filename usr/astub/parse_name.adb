with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Words, Output, Parse_To_End, Put_Comments;

procedure Parse_Name (File : in Text_Char.File_Type;
                      Level : in Natural;
                      Name : out Ada.Strings.Unbounded.Unbounded_String) is
  package Asu renames Ada.Strings.Unbounded;
  Word, Ending : Words.Word_Rec;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
begin

  -- Read until identifier, save it in EZnding, put intermediate comments
  Parse_To_End ("");
  Ending := Words.Get;
  Put_Comments (Level);

  -- Check it is an identifier
  if Ending.Lexic /= Ada_Parser.Identifier
  and then Ending.Lexic /= Ada_Parser.String_Literal then
    Common.Error (Asu.To_String (Ending.Text));
  end if;

  -- Read while identifier or "." and concat
  Name := Ending.Text;
  loop
    Ada_Parser.Parse_Next (File, Ending.Text, Ending.Lexic, True);
    exit when Ending.Lexic /= Ada_Parser.Identifier
    and then Ending.Lexic /=  Ada_Parser.String_Literal
    and then Asu.To_String (Ending.Text) /= ".";
      Asu.Append (Name, Ending.Text);
  end loop;

  -- Store "terminating" lexic in words
  Words.Add (Ending);

end Parse_Name;

