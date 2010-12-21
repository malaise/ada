with As.U.Utils; use As.U, As.U.Utils;
with Ada.Text_Io;
with Argument, Get_Line;
with Points, Point_Str, File;

procedure Import is

  Point : Points.P_T_One_Point;

  package My_Get_Line is new Get_Line (Comment      => "#");

  Line  : Asu_Ua.Unbounded_Array;

  procedure Error (Msg : in String)  is
  begin
    Ada.Text_Io.Put_Line ("ERROR: " & Msg);
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name & " <src_ascii_file> <dst_approx_file>");
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
    when Ada.Text_Io.Name_Error =>
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
      Error ("At line "
          & My_Get_Line.Positive_Count'Image(My_Get_Line.Get_Line_No)
          & " two reals expected");
      My_Get_Line.Close;
      return;
    end if;

    begin
      Point.X := Point_Str.Coordinate_Value(Line.Element (1).Image);
      Point.Y := Point_Str.Coordinate_Value(Line.Element (2).Image);
    exception
      when others =>
        Error ("At line "
           & My_Get_Line.Positive_Count'Image(My_Get_Line.Get_Line_No)
           & " two reals expected");
        My_Get_Line.Close;
        return;
    end;

    Points.P_Upd_Point (Points.Add, 1, Point);

    My_Get_Line.Read_Next_Line;
  end loop;

exception
  when My_Get_Line.End_Error =>
    My_Get_Line.Close;
    File.F_Write(Argument.Get_Parameter(Occurence => 2),
                 Points.P_The_Points);
    Ada.Text_Io.Put_Line ("Done.");
end Import;

