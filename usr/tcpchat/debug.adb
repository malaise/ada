with Environ, Basic_Proc;
package body Debug is

  type Debug_Status_List is (Unknown, On, Off);
  Debug_Status : Debug_Status_List := Unknown;
  Debug_Var : constant String := "TCPCHAT_DEBUG";

  procedure Log (Msg : in String; New_Line : in Boolean := True) is
  begin
    if Debug_Status = Off then
      return;
    end if;
    if Debug_Status = Unknown then
      if Environ.Is_Set (Debug_Var) then
        Debug_Status := On;
      else
        Debug_Status := Off;
        return;
      end if;
    end if;
    if New_Line then
      Basic_Proc.Put_Line_Error (Msg);
    else
      Basic_Proc.Put_Error (Msg);
    end if;
  end Log;

end Debug;

