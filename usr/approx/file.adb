with Ada.Sequential_Io;
with Directory, Text_Handler, My_Math;
package body File is

  Magic_X : constant Points.P_T_Coordinate := 21.21;

  -- Point sequential read/write
  package F_Points_Io is new Ada.Sequential_Io (Points.P_T_One_Point);
  use F_Points_Io;

  -- Read a file of points
  function F_Read (Name : F_T_File_Name) return Points.P_T_The_Points is
    Size : Natural;
    File : File_Type;
    Magic_Point : Points.P_T_One_Point;
    use My_Math;
  begin
    begin
      Open (File, In_File, Name);
    exception
      when others => raise F_Access_Error;
    end;
    Reset (File);
    
    -- Check magic x and get size
    Read (File, Magic_Point);
    if Magic_Point.X /= Magic_X then
      raise F_Io_Error;
    end if;
    Size := Natural(Magic_Point.Y);
    -- Should be int
    if Points.P_T_Coordinate(Size) /= Magic_Point.Y then
      raise F_Io_Error;
    end if;
    
    -- Read the Size points
    declare
      The_Points : Points.P_T_The_Points (1 .. Size);
    begin
      for Index in 1 .. Size loop
        Read (File, The_Points (Index));
      end loop;
      Close (File);
      Points.P_Saved;
      return (The_Points);
    end;
  exception
    when others =>
      Close (File);
      raise F_Io_Error;
  end F_Read;

  -- Write the points in file
  procedure F_Write (Name : in F_T_File_Name;
    The_Points : in Points.P_T_The_Points) is
    File : File_Type;
  begin
    begin
      Open(File, Out_File, Name);
      Delete(File);
      Create(File, Out_File, Name);
    exception
      when Name_Error =>
        -- New file
        begin
          Create (File, Out_File, Name);
        exception
          when others => raise F_Access_Error;
        end;
      when others => raise F_Access_Error;
    end;
    Write (File, (X => Magic_X, 
                  Y => Points.P_T_Coordinate(The_Points'Length)));
    begin
      for Index in The_Points'Range loop
        Write (File, The_Points(Index));
      end loop;
    exception
      when others =>
        Close (File);
        raise F_Io_Error;
    end;
    Points.P_Saved;
    Close (File);
  end F_Write;

  -- Check if file exists
  function F_Exists (Name : F_T_File_Name) return Boolean is
    File : File_Type;
  begin
    Open (File, In_File, Name);
    Close (File);
    return (True);
  exception
    when others => return False;
  end F_Exists;

end File;

