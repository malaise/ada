with Ada.Text_Io;
with Argument;
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
      Ada.Text_Io.Put_Line ("SYNTAX ERROR. Usage : c_magic <dim>");
      raise;
  end;

  -- Search all magic magic squares with this dimension
  Common.Search (Dim);

end C_Magic;

