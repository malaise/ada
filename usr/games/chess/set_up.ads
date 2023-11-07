with Space;
package Set_Up is

  -- Return color of last piece read
  --  in order to know which color plays next
  function Load (File_Name : in String) return Space.Color_List;
  Load_Error : exception;

end Set_Up;

