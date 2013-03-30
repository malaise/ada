with Oper_Dyn_List_Mng;
package File_Mng is

  package Oper_List_Mng renames Oper_Dyn_List_Mng.Dyn_List;

  -- Overwrites the list from file content
  function Load (File_Name : in String;
                 Oper_List : in out Oper_List_Mng.List_Type) return Boolean;

  -- Save the list in file
  procedure Save (File_Name : in String;
                  Oper_List : in Oper_List_Mng.List_Type);

  -- File not found on load, or protected on save
  F_Access_Error : exception;
  -- Read/Write error
  F_Io_Error     : exception;

end File_Mng;

