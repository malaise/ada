with Ada.Exceptions;
with System.Compiler_Exceptions;
package body Current_Exception is

  function Exception_Name return String is
  begin
    return Ada.Exceptions.Exception_Name(
             System.Compiler_Exceptions.Current_Exception);
  end Exception_Name;

end Current_Exception;

