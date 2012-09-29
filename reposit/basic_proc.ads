package Basic_Proc is

  -- Any Put can raise (for ex on EPIPE)
  Io_Error : exception;

  -- Put line on stdout
  procedure Put_Output (Str : in String);
  procedure Put_Output (Char : in Character);
  procedure Put_Line_Output (Str : in String);
  procedure New_Line_Output;
  procedure Flush_Output;

  -- Put line on stderr
  procedure Put_Error (Str : in String);
  procedure Put_Error (Char : in Character);
  procedure Put_Line_Error (Str : in String);
  procedure New_Line_Error;
  procedure Flush_Error;

  -- Get line from stdin up to Fl or Str'Last (strip tailing Lf if any)
  End_Error : exception;
  procedure Get_Line (Item : out String;
                      Last : out Natural);
  -- Get line from stdin until Lf (strip it)
  function Get_Line return String;
  -- Skip line (until Lf)
  procedure Skip_Line;

  -- Get a character
  procedure Get (C : out Character);
  function Get return Character;

  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural);
  -- Set exit code to ok (0) or error (1)
  Exit_Code_Ok    : constant Natural := 0;
  Exit_Code_Error : constant Natural := 1;
  procedure Set_Ok_Exit_Code;
  procedure Set_Error_Exit_Code;

end Basic_Proc;

