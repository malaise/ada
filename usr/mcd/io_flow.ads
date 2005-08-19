package Io_Flow is

  procedure Put (Str : in String);
  procedure New_Line;

  procedure Next_Line (Str : out String;
                       Len : out Natural);

  procedure Close;

  -- May raise Fifo_Error at init (first call to Next_Line)
  Fifo_Error : exception;
end Io_Flow;

