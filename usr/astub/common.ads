-- Common definitions
with As.U, Ada_Words.Keywords;
package Common is

  -- Current Ada langauge version
  Language_Version : Ada_Words.Keywords.Language_Versions
                   := Ada_Words.Keywords.Ada2012;

  -- Line feed string
  function Line_Feed return As.U.Asu_Us;
  function Line_Feed return String;
  function Line_Feed return Character;

  -- Syntax error detected by any parser
  Syntax_Error : exception;

  -- Put error message on stderr and raises Syntax_Error
  procedure Error (Msg : in String);

  -- Dump content of words
  procedure Dump_Words;

end Common;

