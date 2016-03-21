with As.U.Utils, Unbounded_Arrays, Text_Line, Get_Line, Sys_Calls;
with Point_Str;
package body File is

  -- Unbounded array of points
  package Points_Array is new Unbounded_Arrays (Points.P_T_One_Point,
                                                Points.P_T_The_Points);

  -- File input
  package My_Get_Line is new Get_Line;

  -- Read a file of points
  function F_Read (Name : F_T_File_Name) return Points.P_T_The_Points is
    Line : As.U.Utils.Asu_Ua.Unbounded_Array;
    The_Points : Points_Array.Unb_Array;
    A_Point : Points.P_T_One_Point;
  begin
    -- Open the file
    begin
      My_Get_Line.Open (Name);
    exception
      when others => raise F_Access_Error;
    end;

    -- Read the points
    loop
      My_Get_Line.Get_Words (Line);
      if My_Get_Line.Get_Word_Number = 0 then
        -- Empty line
        null;
      elsif My_Get_Line.Get_Word_Number = 2 then
        -- Valid line
        A_Point.X := Point_Str.Coordinate_Value(Line.Element (1).Image);
        A_Point.Y := Point_Str.Coordinate_Value(Line.Element (2).Image);
        The_Points.Append (A_Point);
      else
        raise F_Io_Error;
      end if;
      -- Read next line or End_Error
      My_Get_Line.Read_Next_Line;
    end loop;

  exception
    when My_Get_Line.End_Error =>
      -- Done
      My_Get_Line.Close;
      Points.P_Saved;
      return The_Points.To_Array;
    when others =>
      My_Get_Line.Close;
      raise F_Io_Error;
  end F_Read;

  -- Write the points in file
  procedure F_Write (Name : in F_T_File_Name;
                     The_Points : in Points.P_T_The_Points) is
    File : Text_Line.File_Type;
  begin
    -- Create file
    begin
      File.Create_All (Name);
    exception
      when others =>
        raise F_Access_Error;
    end;

    -- Write the points
    for I in The_Points'Range loop
      File.Put_Line (
          Point_Str.Coordinate_Image(The_Points(I).X) & "  " &
          Point_Str.Coordinate_Image(The_Points(I).Y));
    end loop;

    Points.P_Saved;
    File.Close_All;
  exception
    when others =>
      File.Close_All;
      raise F_Io_Error;
  end F_Write;

  -- Check if file exists
  function F_Exists (Name : F_T_File_Name) return Boolean is
  begin
    return Sys_Calls.File_Found (Name);
  end F_Exists;

end File;

