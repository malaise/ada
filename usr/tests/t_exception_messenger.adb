-- Test exception messenger with a short and a long mesage (560 characters),
--  fixed
with Ada.Exceptions;
with As.U, Basic_Proc, Exception_Messenger;
procedure T_Exception_Messenger is

  -- The exception that carries the message
  The_Exception : exception;

  -- Raise The_Exception with Message
  procedure Raise_Exception (Message : in String) is
  begin
    Exception_Messenger.Raise_Exception (The_Exception'Identity, Message);
  end Raise_Exception;

  -- Raise the exception and check that message is correctly retrieved
  procedure Check_Exception (Message : in String) is
  begin
    Basic_Proc.Put_Line_Output ("Checking "
      & (if Message'Length < 20 then ">" & Message & "<"
         else "a long message"));
    Raise_Exception (Message);
  exception
    when Error_Occ:The_Exception =>
      declare
         Loc_Occ : Ada.Exceptions.Exception_Occurrence;
         Msg : As.U.Asu_Us;
       begin
         Ada.Exceptions.Save_Occurrence (Loc_Occ, Error_Occ);
         Exception_Messenger.Exception_Message (Loc_Occ, Msg);
         if Msg.Image /= Message then
           -- Re raise the exception if the message does not match
           Basic_Proc.Put_Line_Output ("FAILED. Got >" & Msg.Image & "<");
           raise;
         end if;
         Basic_Proc.Put_Line_Output ("Ok");
       end;
  end Check_Exception;

  Msg : As.U.Asu_Us;

begin
  Check_Exception ("The message");
  for I in 1 .. 10 loop
    Msg.Append ("This is a very long string of more than 200 characters. ");
  end loop;
  Check_Exception (Msg.Image);
end T_Exception_Messenger;

