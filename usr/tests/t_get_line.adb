
-- One argument : file name
-- Tests get_line

with Text_Io;
with Text_Handler, Argument, Get_Line, Normal;

procedure T_Get_Line is

  Max_Line_Len : constant := 1024;
  Max_Word_Nb  : constant := 500;
  Max_Word_Len : constant := 30;

begin
  -- Check syntax
  if Argument.Get_Nbre_Arg /= 1 then
    Text_Io.Put_Line ("ERROR. Syntax : t_get_line <file_name>");
    return;
  end if;

  declare
    package My_Get_Line is new Get_Line (
      Max_Word_Len => Max_Word_Len,
      Max_Word_Nb  => Max_Word_Nb,
      Max_Line_Len => Max_Line_Len,
      Comment      => '#');

    Line  : My_Get_Line.Line_Array;

  begin

    -- open file
    begin
      My_Get_Line.Open (Argument.Get_Parameter);
    exception
      when others =>
        Text_Io.Put_Line ("ERROR opening file " & Argument.Get_Parameter & ".");
        raise;
    end;


    loop
      My_Get_Line.Get_Words (Line);
      Text_Io.Put (Normal (Integer (My_Get_Line.Get_Line_No), 3, Gap => '0') & " -> ");
      Text_Io.Put (Normal (My_Get_Line.Get_Word_Number, 3) & ":");
      for I in 1 .. My_Get_Line.Get_Word_Number loop
        Text_Io.Put (">" & Text_Handler.Value (Line(I)) & "<");
      end loop;
      Text_Io.New_Line;
      My_Get_Line.Read_Next_Line;
    end loop;

  exception
    when My_Get_Line.No_More_Line =>
      Text_Io.Put_Line ("Done.");
  end;

end T_Get_Line;
