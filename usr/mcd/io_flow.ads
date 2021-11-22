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

  -- Is io flow the stdio (stdin / stdout, interactive or not)
  function Is_Stdio return Boolean;

  -- Data to output
  procedure Put (Str : in String);
  procedure Put_Line (Str : in String);
  procedure New_Line;

  -- Data to input from stdin (echo or not), when stdin is not the flow
  In_Stdin : exception;
  End_Error :exception;
  procedure Set_Echo (Echo : in Boolean);
  function Get_Key return Character;
  function Get_Str return String;

  -- Input data from flow
  procedure Next_Line (Str : out As.U.Asu_Us);

  -- Close io flows
  procedure Close;

end Io_Flow;

