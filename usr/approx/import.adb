with Text_Io;
with Argument, Text_Handler, Get_Line;
with Points, Point_Str, File;

procedure Import is

  Point : Points.P_T_One_Point;

  package My_Get_Line is new Get_Line (
     Max_Word_Len => 40,
     Max_Word_Nb  => 3,
     Max_Line_Len => 132,
     Comment      => '#');

  Line  : My_Get_Line.Line_Array;

  procedure Error (Msg : in String)  is
  begin
    Text_Io.Put_Line ("ERROR: " & Msg);
    Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name & " <src_ascii_file> <dst_approx_file>");
  end Error;

begin

  if Argument.Get_Nbre_Arg /= 2 then
    Error ("Invalid arguments");
    return;
  end if;
  if File.F_Exists(Argument.Get_Parameter(Occurence => 2)) then
    Error ("File " & Argument.Get_Parameter(Occurence => 2) & " exists");
    return;
  end if;

  begin
    My_Get_Line.Open(Argument.Get_Parameter(Occurence => 1));
  exception
    when Text_Io.Name_Error =>
      Error ("File " & Argument.Get_Parameter(Occurence => 1) & " not found");
      return;
    when others =>
      Error ("Opening file " & Argument.Get_Parameter(Occurence => 1));
      raise;
  end;
      
  Points.P_Clear;


  loop
    My_Get_Line.Get_Words (Line);
    if My_Get_Line.Get_Word_Number /= 2 then
      Error ("At line " & Text_Io.Positive_Count'Image(My_Get_Line.Get_Line_No)
                        & " two reals expected");
      My_Get_Line.Close;
      return;
    end if;

    begin
      Point.X := Point_Str.Coordinate_Value(Text_Handler.Value(Line(1)));
      Point.Y := Point_Str.Coordinate_Value(Text_Handler.Value(Line(2)));
    exception
      when others =>
        Error ("At line " & Text_Io.Positive_Count'Image(My_Get_Line.Get_Line_No)
                          & " two reals expected");
        My_Get_Line.Close;
        return;
    end;

    Points.P_Upd_Point (Points.Add, 1, Point);

    My_Get_Line.Read_Next_Line;
  end loop;

exception
  when My_Get_Line.No_More_Line =>
    My_Get_Line.Close;
    File.F_Write(Argument.Get_Parameter(Occurence => 2),
                 Points.P_The_Points);
    Text_Io.Put_Line ("Done.");
end Import;
