with Current_Exception;
with My_Io;

procedure T_Except is

begin
  

  My_Io.Put_Line ("Before exception");
  raise Program_Error;
  My_Io.Put_Line ("After  exception");

exception
  when others =>
    My_Io.Put_Line ("In handler");
    My_Io.Put_Line ("Exception: " & Current_Exception.Exception_Name);
end T_Except;
