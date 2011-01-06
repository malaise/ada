-- One argument : file name
-- Tests get_line
with Ada.Text_Io;
with Argument, Get_Line, Normal;
procedure T_Get_Line is

begin
  -- Check syntax
  if Argument.Get_Nbre_Arg /= 1 then
    Ada.Text_Io.Put_Line ("ERROR. Syntax : t_get_line <file_name>");
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
        Ada.Text_Io.Put_Line ("ERROR opening file " & Argument.Get_Parameter & ".");
        raise;
    end;


    loop
      Ada.Text_Io.Put (Normal (Integer (My_Get_Line.Get_Line_No), 3, Gap => '0') & " -> ");
      Ada.Text_Io.Put (Normal (My_Get_Line.Get_Word_Number, 3) & ":");
      declare
        Line : constant My_Get_Line.Line_Array := My_Get_Line.Get_Words;
      begin
        for I in 1 .. My_Get_Line.Get_Word_Number loop
          Ada.Text_Io.Put (">" & Line(I).Image & "<");
        end loop;
      end;
      Ada.Text_Io.New_Line;
      My_Get_Line.Read_Next_Line;
    end loop;

  exception
    when My_Get_Line.End_Error =>
      My_Get_Line.Close;
      Ada.Text_Io.Put_Line ("Done.");
  end;

end T_Get_Line;

