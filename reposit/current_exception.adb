with Ada.Exceptions;
pragma Warnings (Off);
with System.Soft_Links;
pragma Warnings (On);
package body Current_Exception is

  function Exception_Name return String is
  begin
    return Ada.Exceptions.Exception_Name(
             System.Soft_Links.Get_Current_Excep_Nt.all);
  end Exception_Name;

end Current_Exception;

