with Argument, Basic_Proc;
with Common;
procedure C_Magic is
  Dim : Common.Dim_Range;
begin

  -- Get dimension
  declare
    Syntax_Error : exception;
  begin
    if Argument.Get_Nbre_Arg /= 1 then
      raise Syntax_Error;
    end if;
    Dim := Common.Dim_Range'Value(Argument.Get_Parameter);
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("SYNTAX ERROR. Usage : c_magic <dim>");
      raise;
  end;

  -- Search all magic magic squares with this dimension
  Common.Search (Dim);

end C_Magic;

