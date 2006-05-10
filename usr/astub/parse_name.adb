with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Words, Output, Parse_To_Ends, Put_Comments;

  -- Read until identifier, save it in Name, put intermediate comments at Level
procedure Parse_Name (File : in Text_Char.File_Type;
                      Level : in Natural;
                      Name : out Ada.Strings.Unbounded.Unbounded_String) is
  package Asu renames Ada.Strings.Unbounded;
  Ending : Words.Word_Rec;
  use type Ada_Parser.Lexical_Kind_List;
begin

  -- Read until identifier or string literal, save it in Ending
  Parse_To_Ends ( ( (Ada_Parser.Identifier, Common.Null_String),
                    (Ada_Parser.String_Literal, Common.Null_String)),
                  0);
  Ending := Words.Get;

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
  Put_Comments (Level);
  Words.Add (Ada_Parser.Identifier, Name);
  Words.Add (Ending);

end Parse_Name;

