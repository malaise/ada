with Types;

package File is

  -- Delta above which one considers it is zero (for loss printing)
  Epsilon : constant Float := 1.0E-5;

  function Read (File_Name : String) return Types.Mattrix_Rec;

  function Get_Kind return Types.Mattrix_Kind_List;
  function Get_Note (Row, Col : Positive) return Float;

  Read_Error : exception;
  File_Not_Read : exception;
  
end File;

