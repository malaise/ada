-- Common definitions
with As.U; use As.U;
package Common is

  -- Line feed string
  function Line_Feed return Asu_Us;
  function Line_Feed return String;
  function Line_Feed return Character;

  -- Syntax error detected by any parser
  Syntax_Error : exception;

  -- Put error message on stderr and raises Syntax_Error
  procedure Error (Msg : in String);

  -- Dump content of words
  procedure Dump_Words;

end Common;

