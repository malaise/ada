with Protected_Pool, String_Mng;
package body Exception_Messenger is

  package Msg_Pool is new Protected_Pool (As.U.Asu_Us);
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
        Key := Pool.Store (As.U.Tus (Message));
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

  -- INTERNAL: Retrieve the message associated to an exception message
  function Get_Message (M : String) return String is
    Str : constant String (1 .. M'Length) := String_Mng.Normalize (M);
    Key : Msg_Pool.Key_Type;
    Res : As.U.Asu_Us;
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
      return Res.Image;
    exception
      when Msg_Pool.Not_Found =>
        return "";
    end;
  end Get_Message;

  -- Retrieve the message associated to an exception
  procedure Exception_Message (X : in out Ada.Exceptions.Exception_Occurrence;
                               M : out As.U.Asu_Us) is
  begin
    M := As.U.Tus (Get_Message (Ada.Exceptions.Exception_Message (X)));
  end Exception_Message;

  function Exception_Message (X : Ada.Exceptions.Exception_Occurrence_Access)
                             return String is
  begin
    return Get_Message (Ada.Exceptions.Exception_Message (X.all));
  end Exception_Message;

end Exception_Messenger;

