package Input_Dispatcher is

  -- Set input flow to a new string
  --  or stdin if Str is empty
  procedure Set_Input (Str : in String);

  -- Get the ungot words of current string
  -- Program_Error is current input is stdin
  --  or if no word already got from current string
  function Get_Remaining return String;

  -- Get next word from current input
  -- Empty if end of input flow
  function Next_Word return String;

  -- String delimiter (returned by Next_Word)
  Sd : constant Character := '"';

  -- Remove first and last string delimiters
  --  and then pair of delimiters by one
  -- >"foo ""bar"" stuff"< becomes >foo "bar" stuff<
  function Parse_Substring (Str : String) return String;

  -- Error if end of string litteral not found
  -- Either in Parse_String or Next_Word
  String_Error : exception;

  -- Next string to parse, also current string
  --  if Next_Str_Word raised String_Error
  function Current_String return String;

end Input_Dispatcher;

