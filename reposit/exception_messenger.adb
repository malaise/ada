with As.U; use As.U;
with Protected_Pool;
package body Exception_Messenger is

  package Msg_Pool is new Protected_Pool (Asu_Us);
  Pool : Msg_Pool.Pool_Type;

  -- Raise an exception with a message
  procedure Raise_Exception (E       : in Ada.Exceptions.Exception_Id;
                             Message : in String := "") is
    Key : Msg_Pool.Key_Type;
  begin
    if Message'Length < Max_Message_Length then
      Ada.Exceptions.Raise_Exception (E, Message);
    else
      -- Insert in pool
      begin
        Key := Pool.Store (Asu_Tus (Message));
      exception
        when others =>
          -- Cannot insert in pool!
          Ada.Exceptions.Raise_Exception (E, Message);
      end;
      -- Raise the exception but with the key as message
      Ada.Exceptions.Raise_Exception (E,
          Key_Root & Msg_Pool.Key_Image (Key));
    end if;
  end Raise_Exception;

  -- Retrieve the message associated to an exception
  function Exception_Message (X : Ada.Exceptions.Exception_Occurrence_Access)
                             return String is
    Str : constant String := Ada.Exceptions.Exception_Message (X.all);
    Key : Msg_Pool.Key_Type;
    Res : Asu_Us;
  begin
    if Str'Length < Key_Root'Length
    or else Str (1 .. Key_Root'Length) /= Key_Root then
      -- Exception message is not a key
      return Str;
    end if;
    begin
      Key := Msg_Pool.Key_Value (Str (Key_Root'Length + 1 .. Str'Last));
    exception
      when others =>
        -- Exception message is not a key (but it starts as such!)
        return Str;
    end;

    -- Get message from pool, return "" if not found
    begin
      Res := Pool.Get (Key);
      return Asu.To_String (Res);
    exception
      when Msg_Pool.Not_Found =>
        return "";
    end;
  end Exception_Message;

end Exception_Messenger;

