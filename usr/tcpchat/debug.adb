with Environ, Basic_Proc;
package body Debug is

  type Debug_Status_List is (Unknown, On, Off);
  Debug_Status : Debug_Status_List := Unknown;
  Debug_Var : constant String := "TCPCHAT_DEBUG";

  function Is_On return Boolean is
  begin
    if Debug_Status = Off then
      return False;
    end if;
    -- First call
    if Debug_Status = Unknown then
      if Environ.Is_Yes (Debug_Var) then
        Debug_Status := On;
      else
        Debug_Status := Off;
      end if;
    end if;
    return Debug_Status = On;
  end Is_On;


  procedure Log (Msg : in String; New_Line : in Boolean := True) is
  begin
    if not Is_On then
      return;
    end if;
    if New_Line then
      Basic_Proc.Put_Line_Error (Msg);
    else
      Basic_Proc.Put_Error (Msg);
    end if;
  end Log;

end Debug;

