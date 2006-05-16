-- Common definitions
with Ada.Strings.Unbounded;
package Common is

  -- Line feed string
  function Line_Feed return Ada.Strings.Unbounded.Unbounded_String;
  function Line_Feed return String;
  function Line_Feed return Character;

  -- Null string
  Null_String : constant Ada.Strings.Unbounded.Unbounded_String
              := Ada.Strings.Unbounded.Null_Unbounded_String;

  -- Syntax error detected by any parser
  Syntax_Error : exception;

  -- Put error message on stderr and raises Syntax_Error
  procedure Error (Msg : in String);

  -- Dump content of words
  procedure Dump_Words;

end Common;

