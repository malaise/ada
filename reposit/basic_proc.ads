package Basic_Proc is

  -- Put line on stdout
  procedure Put_Output (Str : in String);
  procedure Put_Line_Output (Str : in String);
  procedure New_Line_Output;
  procedure Flush_Output;

  -- Put line on stderr
  procedure Put_Error (Str : in String);
  procedure Put_Line_Error (Str : in String);
  procedure New_Line_Error;
  procedure Flush_Error;

  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural);
  -- Set exit code to ok (0) or error (1)
  Exit_Code_Ok    : constant Natural := 0;
  Exit_Code_Error : constant Natural := 1;
  procedure Set_Ok_Exit_Code;
  procedure Set_Error_Exit_Code;

end Basic_Proc;

