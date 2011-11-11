-- One argument : file name
-- Tests get_line
with Argument, Get_Line, Normal, Basic_Proc;
procedure T_Get_Line is

begin
  -- Check syntax
  if Argument.Get_Nbre_Arg /= 1 then
    Basic_Proc.Put_Line_Output ("ERROR. Syntax : t_get_line <file_name>");
    return;
  end if;

  declare
    package My_Get_Line is new Get_Line ("--");

  begin

    -- open file
    begin
      My_Get_Line.Open (Argument.Get_Parameter);
    exception
      when others =>
        Basic_Proc.Put_Line_Output ("ERROR opening file "
                                  & Argument.Get_Parameter & ".");
        raise;
    end;


    loop
      Basic_Proc.Put_Output (Normal (Integer (My_Get_Line.Get_Line_No), 3,
                                     Gap => '0') & " -> ");
      Basic_Proc.Put_Output (Normal (My_Get_Line.Get_Word_Number, 3) & ":");
      declare
        Line : constant My_Get_Line.Line_Array := My_Get_Line.Get_Words;
      begin
        for I in 1 .. My_Get_Line.Get_Word_Number loop
          Basic_Proc.Put_Output (">" & Line(I).Image & "<");
        end loop;
      end;
      Basic_Proc.New_Line_Output;
      My_Get_Line.Read_Next_Line;
    end loop;

  exception
    when My_Get_Line.End_Error =>
      My_Get_Line.Close;
      Basic_Proc.Put_Line_Output ("Done.");
  end;

end T_Get_Line;

