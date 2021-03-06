with As.U;
with Words, Parse_To_Ends, Put_Comments, Parser_Ada;

  -- Read until identifier, save it in Name, put intermediate comments at Level
procedure Parse_Name (Level : in Natural;
                      Name : out As.U.Asu_Us) is
  Word : Words.Word_Rec;
  use type Parser_Ada.Lexical_Kind_List;
begin

  -- Read until identifier or string literal
  Parse_To_Ends (
      End_Criteria => ( (Parser_Ada.Identifier, As.U.Asu_Null),
                        (Parser_Ada.String_Literal, As.U.Asu_Null)),
      Level => Level,
      Put_Comments => False,
      Up_To_Next_Significant => False);
  Word := Words.Read;

  -- Read while identifier or "." and concat to Name
  Name := Word.Text;
  Parser_Ada.Multiparser.Start_Recording;
  loop
    Word := Parser_Ada.Multiparser.Get (True);
    Words.Add (Word);
    exit when Word.Lexic /= Parser_Ada.Identifier
    and then  Word.Lexic /= Parser_Ada.String_Literal
    and then Word.Text.Image /= ".";
      Name.Append (Word.Text);
  end loop;
  Words.Del;
  Parser_Ada.Multiparser.Unget;
  Parser_Ada.Multiparser.Stop_Recording;

  -- Read until a significant character (and unget)
  Parse_To_Ends (
      End_Criteria => Words.No_Word,
      Level => 0,
      Put_Comments => False,
      Up_To_Next_Significant => True);

  -- Extract and put comments
  Put_Comments;
end Parse_Name;

