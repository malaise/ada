with Ada.Strings.Unbounded;
with Common, Words, Parse_To_Ends, Put_Comments, Parser_Ada;

  -- Read until identifier, save it in Name, put intermediate comments at Level
procedure Parse_Name (Level : in Natural;
                      Name : out Ada.Strings.Unbounded.Unbounded_String) is
  package Asu renames Ada.Strings.Unbounded;
  Word : Words.Word_Rec;
  use type Parser_Ada.Lexical_Kind_List;
begin

  -- Read until identifier or string literal
  Parse_To_Ends (
      End_Criteria => ( (Parser_Ada.Identifier, Common.Null_String),
                        (Parser_Ada.String_Literal, Common.Null_String)),
      Level => Level,
      Put_Comments => False,
      Up_To_Next_Significant => False);
  Word := Words.Read;

  -- Read while identifier or "." and concat to Name
  Name := Word.Text;
  Parser_Ada.Multiparse.Start_Recording;
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    Words.Add (Word);
    exit when Word.Lexic /= Parser_Ada.Identifier
    and then  Word.Lexic /= Parser_Ada.String_Literal
    and then Asu.To_String (Word.Text) /= ".";
      Asu.Append (Name, Word.Text);
  end loop;
  Words.Del;
  Parser_Ada.Multiparse.Unget;
  Parser_Ada.Multiparse.Stop_Recording;

  -- Read until a significant character (and unget)
  Parse_To_Ends (
      End_Criteria => Words.No_Word,
      Level => 0,
      Put_Comments => False,
      Up_To_Next_Significant => True);

  -- Extract and put comments
  Put_Comments;
end Parse_Name;

