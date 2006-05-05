-- Common definitions
with Ada.Strings.Unbounded;
package Common is

  -- Line feed string
  function Line_Feed return Ada.Strings.Unbounded.Unbounded_String;
  function Line_Feed return String;
  function Line_Feed return Character;

  -- Syntax error detected by any parser
  Syntax_Error : exception;

  -- Put error message on stderr and raises Syntax_Error
  procedure Error (Msg : in String);

end Common;

