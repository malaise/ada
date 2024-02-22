-- Handle user inputs asynchronously (call a callback with the input text)
with Basic_Proc;
package Async_Stdin is

  -- Maximum amount of characters stored, 0 for infinite
  subtype Max_Chars_Range is Natural;

  -- The user callback
  type User_Callback_Access is
     access function (Buffer : String) return Boolean;


  -- Set asynchronous mode for stdin
  -- User callback is called when Max_Chars characters are entered
  --  or at each control char (i.e. before space in the ASCII table)
  -- The following specific keys are handled:
  --  - Backspace, suppr, left and right arrows, Home and End (move cursor)
  --  - CtrlSuppr (clear line) and ShiftSuppr (clear to end of line)
  --  - Ins (toggle insertion mode)
  --  - Up and down arrows, page up and page down (move in history)
  --  - Tab (searchg in history)
  -- History size can be tuned with ENV ASYNC_STDIN_HISTORY_SIZE (default 20)
  -- If a unrecognized sequence is entered, then the user callback is called
  --  with the characters got so far and the ASCII Esc char
  -- User callback is called with empty string in case of error
  -- Set null callback to restore normal behaviour
  -- Input chars are displayed (if Echo) from Left_Col to
  --  Left_Col + Max_Chars - 1
  -- Asynchronous mode relies on a Event_Mng Fd Callback, which
  --  returns the result of the User_Callback. So Event_Mng will report
  --  a Fd_Event if User_Callback returns True
  procedure Set_Async (User_Callback : in User_Callback_Access := null;
                       Max_Chars : in Max_Chars_Range := 1;
                       Left_Col  : in Max_Chars_Range := 1;
                       Echo      : in Boolean := True);
  function Is_Set return Boolean;

  -- Activate asynchronous data to trigger callback
  -- When not activated, input characters remain in stdin to be delivered
  --  when activating
  procedure Activate (Allow_Input : Boolean := True);
  function Is_Active return Boolean;

  -- Clear internal buffer of pending characters
  procedure Clear_Pending;

  -- Clear history
  procedure Clear_History;

  -- By default the input is in insert mode and is reset to insert mode after
  --  each input (just before calling user callback)
  -- This operation allows setting the overwrite mode for next input
  --  (in user callback or before calling Get_Line)
  procedure Overwrite;

  -- Set an internal callback (overwritting any Async callback set)
  --  and wait until it is called, then unset it and return the result
  -- Raise Io_Error in case of error
  -- Return empty string in case of event (timer, signal....)
  function Get_Line (Max_Chars : Max_Chars_Range := 0;
                     First_Col : Max_Chars_Range := 1;
                     Echo      : Boolean := True) return String;

  -- Strip last character of Str if it is a control char (i.e. before space
  --  in ASCII table)
  function Strip_Last_Control (Str : String) return String;

  -- Put on stdout when in async
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

  -- Error when putting or getting
  Io_Error : exception renames Basic_Proc.Io_Error;

end Async_Stdin;

