package Input_Dispatcher is

  -- Max line length on stdin
  -- Max string length
  Max_String_Lg : constant := 1024;

  -- Set input flow to a new string
  --  or stdin if STR is empty
  procedure Set_Input (Str : in String);

  -- Get the ungot words of current string
  -- PROGRAM_ERROR is current input is stdin
  --  or if no word already got from current string
  function Get_Remaining return String;

  -- Get next word from current input
  -- Empty if end of input flow
  function Next_Word return String;

  -- Error if end of string litteral not found
  String_Error : exception;

  -- The string on which the error occured
  function Error_String return String;

end Input_Dispatcher;

