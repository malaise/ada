with Sys_Calls;
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
  -- Asynchronous mode relies on a Event_Mng Fd Callback which
  --  returns the result of the User_Callback. So Event_Mng will report
  --  a Fd_Event if User_Callback returns True
  procedure Set_Async (User_Callback : in User_Callback_Access := null;
                       Max_Chars : in Max_Chars_Range := 1;
                       First_Col : in Max_Chars_Range := 1);
  function Is_Set return Boolean;

  -- Activate asynchronous data to trigger callback
  -- When not activated, input characters remain in stdin to be delivered
  --  when activating
  procedure Activate (Allow_Input : Boolean := True);
  function Is_Active return Boolean;

  -- Clear internal buffer of pending characters
  procedure Clear;

  -- By default the input is in insert mode and is reset to insert mode after
  --  each input (just before calling user callback)
  -- This operation allows setting the overwrite mode for next input
  procedure Overwrite;

  -- Set an internal callback (overwritting any Async callback set)
  --  and wait until it is called, then unset it and return the result
  function Get_Line (Max_Chars : Max_Chars_Range := 0;
                     First_Col : Max_Chars_Range := 1) return String;

  -- Strip last character if Str if it is a control char (before space)
  function Strip_Last_Control (Str : String) return String;

  -- Put on stdout when in async
  Io_Error : exception renames Sys_Calls.Io_Error;
  procedure Put_Out (Str : in String);
  procedure Put_Line_Out (Str : in String);
  procedure New_Line_Out;
  procedure Flush_Out;

  -- Put on stderr when in async
  procedure Put_Err (Str : in String);
  procedure Put_Line_Err (Str : in String);
  procedure New_Line_Err;
  procedure Flush_Err;

  -- Error in Set_Async
  Error : exception;

end Async_Stdin;

