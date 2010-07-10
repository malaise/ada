with As.U; use As.U;
package Io_Flow is

  -- Data to output
  procedure Put (Str : in String);
  procedure Put_Line (Str : in String);
  procedure New_Line;

  -- Input data
  procedure Next_Line (Str : out Asu_Us);

  -- Close io flows
  procedure Close;

  -- May raise Fifo_Error at init (first call to Next_Line)
  Fifo_Error : exception;
end Io_Flow;

