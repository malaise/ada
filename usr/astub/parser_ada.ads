with As.U; use As.U;
with Ada_Parser, Multiget;
package Parser_Ada is

  -- Rename types and exceptions from Ada_Parser
  type Lexical_Kind_List is new Ada_Parser.Lexical_Kind_List;
  Syntax_Error : exception renames Ada_Parser.Syntax_Error;
  End_Error : exception renames Ada_Parser.End_Error;

  -- A Word
  type Word_Rec is record
    Lexic : Lexical_Kind_List;
    Text  : Asu_Us;
  end record;

  -- Get a Word from Ada_Parser
  function Get_Word (Raise_End : in Boolean) return Word_Rec;

  -- The multiget ada parser
  package Multiparse is new Multiget (
    Item_Type => Word_Rec,
    User_Data_Type => Boolean,
    Get_Item => Get_Word,
    Unget_Length => 0);

end Parser_Ada;

