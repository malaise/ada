with Oper_List_Mng;
package File_Mng is


  -- Overwrites the list from file content
  procedure Load (File_Name : in String;
                  Oper_List : in out Oper_List_Mng.List_Type;
                  Can_Write : out Boolean);

  -- Save the list in file
  procedure Save (File_Name : in String;
                  Oper_List : in Oper_List_Mng.List_Type);

  -- File not found on load, or protected on save
  F_Access_Error : exception;
  -- Read/Write error
  F_Io_Error     : exception;

end File_Mng;

