package Async_Stdin is

  -- Maximum amount of characters stored, 0 for infinite
  subtype Max_Chars_Range is Natural;

  -- The user callback
  type User_Callback_Access is
     access function (Buffer : String) return Boolean;


  -- Set asynchronous mode for stdin
  -- User callback is called when Max_Chars characters are entered
  --  or at each control char (i.e. before space)
  -- User callback is called with empty string in case of error
  -- Set null callback to restore normal behaviour
  procedure Set_Async (User_Callback : in User_Callback_Access := null;
                       Max_Chars : in Max_Chars_Range := 1);

  -- Put on stdout when in async
  procedure Put_Out (Str : in String);
  procedure Put_Line_Out (Str : in String);
  procedure New_Line_Out;

  -- Put on stderr when in async
  procedure Put_Err (Str : in String);
  procedure Put_Line_Err (Str : in String);
  procedure New_Line_Err;

  -- Error in Set_Async
  Error : exception;

end Async_Stdin;

