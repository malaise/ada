-- Basic_Proc output on stdout or stderr, protected by a Mutex
package Protected_Put is

  procedure Put_Line_Output (Str : in String);
  procedure New_Line_Output;
  procedure Flush_Output;

  procedure Put_Line_Error (Str : in String);
  procedure New_Line_Error;
  procedure Flush_Error;

end Protected_Put;

