package Io_Flow is

  procedure Put (Str : in String);
  procedure New_Line;

  procedure Next_Line (Str : in out String;
                       Len : out Natural);

  -- May raise Fifo_Error at init (first call to Next_Line)
  Fifo_Error : exception;
end Io_Flow;

