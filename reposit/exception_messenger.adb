with Protected_Pool, Str_Util;
package body Exception_Messenger is

  -- Name and Message with Name as key
  type Exception_Info is record
    Name, Message : As.U.Asu_Us;
  end record;
  function "=" (Current : Exception_Info; Criteria : Exception_Info)
                return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";
  function Image (Element : Exception_Info) return String is
    (Element.Name.Image);

  package Msg_Pool is new Protected_Pool (Exception_Info, "=", Image);
  Pool : Msg_Pool.Pool_Type;

  function Name (E : Ada.Exceptions.Exception_Id) return String
           renames Ada.Exceptions.Exception_Name;

  -- Raise an exception with a message
  procedure Raise_Exception (E       : in Ada.Exceptions.Exception_Id;
                             Message : in String := "") is
  begin
    if Message'Length < Max_Message_Length then
      Ada.Exceptions.Raise_Exception (E, Message);
    else
      -- Insert in pool
      begin
        Pool.Store ( (As.U.Tus (Name (E)),
                      As.U.Tus (Message)) );
      exception
        when others =>
          -- Cannot insert in pool!
          Ada.Exceptions.Raise_Exception (E, Message);
      end;
      -- Raise the exception but with the key as message
      Ada.Exceptions.Raise_Exception (E, Key_Root & Name(E));
    end if;
  end Raise_Exception;

  -- INTERNAL: Retrieve the message associated to an exception message
  function Get_Message (Msg : String) return String is
    Info : Exception_Info;
    Str : constant String := Str_Util.Normalize (Msg);
  begin
    if Str'Length < Key_Root'Length
    or else Str(1 .. Key_Root'Length) /= Key_Root then
      -- Exception message is not a key
      return Str;
    end if;
    -- Get message from pool, return Str if not found
    begin
      Info.Name := As.U.Tus (Str(Key_Root'Length + 1 .. Str'Last));
      Pool.Get (Info);
      return Info.Message.Image;
    exception
      when Msg_Pool.Not_Found =>
        return Str;
    end;
  end Get_Message;

  -- Retrieve the message associated to an exception
  procedure Exception_Message (X : in out Ada.Exceptions.Exception_Occurrence;
                               M : out As.U.Asu_Us) is
  begin
    M := As.U.Tus (Get_Message (Ada.Exceptions.Exception_Message (X)));
  end Exception_Message;

  function Exception_Message (X : in out Ada.Exceptions.Exception_Occurrence)
           return String is
    (Get_Message (Ada.Exceptions.Exception_Message (X)));

  function Exception_Message (X : Ada.Exceptions.Exception_Occurrence_Access)
                             return String is
    (Get_Message (Ada.Exceptions.Exception_Message (X.all)));

end Exception_Messenger;

