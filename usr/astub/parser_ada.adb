with Files, Common;
package body Parser_Ada is

  Context : Ada_Parser.Parsing_Context;

  -- Get a Word from Ada_Parser
  function Get_Word (Raise_End : in Boolean) return Word_Rec is
    Word : Word_Rec;
  begin
    Context.Parse_Next (File => Files.In_File,
                        Text => Word.Text,
                        Lexic => Ada_Parser.Lexical_Kind_List(Word.Lexic),
                        Raise_End => Raise_End,
                        Version => Common.Language_Version);
    return Word;
  end Get_Word;

end Parser_Ada;

