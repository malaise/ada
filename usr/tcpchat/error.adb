with Basic_Proc;
procedure Error (Msg : in String) is
begin
  Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
end Error;
