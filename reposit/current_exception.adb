with Ada.Exceptions;
with System.Tasking_Soft_Links;
package body Current_Exception is

  function Exception_Name return String is
  begin
    return Ada.Exceptions.Exception_Name(
             System.Tasking_Soft_Links.Get_Current_Excep_NT.all);
  end Exception_Name;

end Current_Exception;

