with As.U;
package Io_Flow is

  -- Init or Next_Line can raise Comm_Error if error (re)opening communication
  --  channel
  Communication_Error : exception;

  -- Initialization, use Default if init fails, before putting init error
  -- May raise Init_Error (if invalid argument, communication error...)
  Init_Error : exception;
  procedure Init (Default : in Boolean := False);

  -- Is io flow interactive (Stdio Tty)
  function Is_Interactive return Boolean;

  -- Clear interactive input buffer (on error)
  procedure Clear_Interactive;

  -- Data to output
  procedure Put (Str : in String);
  procedure Put_Line (Str : in String);
  procedure New_Line;

  -- Input data
  procedure Next_Line (Str : out As.U.Asu_Us);

  -- Close io flows
  procedure Close;

end Io_Flow;

