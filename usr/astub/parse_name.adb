with Ada.Strings.Unbounded;
with Text_Char;
with Common, Words, Output, Parse_To_Ends, Put_Comments, Parser_Ada;

  -- Read until identifier, save it in Name, put intermediate comments at Level
procedure Parse_Name (File : in Text_Char.File_Type;
                      Level : in Natural;
                      Name : out Ada.Strings.Unbounded.Unbounded_String) is
  package Asu renames Ada.Strings.Unbounded;
  Ending : Words.Word_Rec;
  use type Parser_Ada.Lexical_Kind_List;
begin

  -- Read until identifier or string literal, save it in Ending
  Parse_To_Ends ( ( (Parser_Ada.Identifier, Common.Null_String),
                    (Parser_Ada.String_Literal, Common.Null_String)),
                  0, True, False);
  Ending := Words.Get;

  -- Read while identifier or "." and concat
  Name := Ending.Text;
  loop
    Ending := Parser_Ada.Multiparse.Get (True);
    exit when Ending.Lexic /= Parser_Ada.Identifier
    and then Ending.Lexic /=  Parser_Ada.String_Literal
    and then Asu.To_String (Ending.Text) /= ".";
      Asu.Append (Name, Ending.Text);
  end loop;

  -- Store "terminating" lexic in words
  Put_Comments (Level);
  Words.Add (Parser_Ada.Identifier, Name);
  Words.Add (Ending);

end Parse_Name;

