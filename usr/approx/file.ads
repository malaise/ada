with Points;
package File is

  subtype F_T_File_Name is String;

  -- Read points from file
  function F_Read (Name : F_T_File_Name) return Points.P_T_The_Points;

  -- Write points to file
  procedure F_Write (Name : in F_T_File_Name;
    The_Points : in Points.P_T_The_Points);

  -- Check if file exists
  function F_Exists (Name : F_T_File_Name) return Boolean;

  -- File not found (or protected...)
  F_Access_Error : exception;
  -- read/Write error
  F_Io_Error     : exception;

end File;
