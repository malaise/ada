with Files;
package body Parser_Ada is

  -- Get a Word from Ada_Parser
  function Get_Word (Raise_End : in Boolean) return Word_Rec is
    Word : Word_Rec;
  begin
    Ada_Parser.Parse_Next (File => Files.In_File,
                           Text => Word.Text,
                           Lexic => Ada_Parser.Lexical_Kind_List(Word.Lexic),
                           Raise_End => Raise_End);
    return Word;
  end Get_Word;

end Parser_Ada;

