with Ada.Exceptions, Ada.Unchecked_Deallocation;
-- In Ada.Exceptions, the string returned by Exception_Message may be truncated
-- (to no less than 200 characters).
-- This package allows storing in a global pool a "long" message and
--  transfering a short access key as the exception message.

-- A message of size below Max_Message_Length will be directly attached
--  to the exception. Otherwise the message is stored on a (protected)
--  pool and the key image of it is used to build a key string that is
--  attached to the exception for later retrieval.

-- Application shall ensure that exceptions raised through Exception_Messenger
--  are caught and that Exception_Message is called on their occurence,
--  otherwise the pool will keep on growing.
with As.U;
package Exception_Messenger is

  -- The name of the key will be "Exception_Messenger_Key_xxx" where
  --  xxx is a positive number.
  -- Applications shall avoid providing this format of string as their
  --  exception messages
  Key_Root : constant String := "Exception_Messenger_Key_";

  -- Max message length
  Max_Message_Length : constant := 200;

  -- Raise an exception with a message
  procedure Raise_Exception (E       : in Ada.Exceptions.Exception_Id;
                             Message : in String := "");


  -- Retrieve the message associated to an exception.
  -- If the message attached does not match "Exception_Messenger_Key_xxx",
  --  then return this message.
  -- If it matches but is not (any more) in the pool, then return "".
  -- If it is found in the pool then suppress it from the pool and return it.
  -- The exception occurence can be passed as "in out" or by access, but in
  --  both cases it is necessary to save the original occurence first.

  -- Procedure: Occurence passed in out.
  -- To pass an exception occurrence in out the user must save it in a local
  -- copy. Example:
  -- exception
  --   when Error_Occ:Parse_Error =>
  --     declare
  --       Loc_Occ : Ada.Exceptions.Exception_Occurrence;
  --     begin
  --       Ada.Exceptions.Save_Occurrence (Loc_Occ, Error_Occ);
  --       Exception_Messenger.Exception_Message (Loc_Occ, Unb_String);
  --     end;
  procedure Exception_Message (X : in out Ada.Exceptions.Exception_Occurrence;
                               M : out As.U.Asu_Us);

  -- Function: Occurence passed by access.
  -- The only way to obtain an exception access is to use the function
  --  Ada.Exceptions.Save_Occurrence that allocates a copy of the
  --  Exception_Occurence. After passing the reference of the copy to
  --  Exception_Message the user shall deallocate this copy. Example:
  -- exception
  --   when Error_Occ:The_Exception =>
  --     declare
  --       Except_Access : Ada.Exceptions.Exception_Occurrence_Access
  --                     := Ada.Exceptions.Save_Occurrence (Error_Occ);
  --       Str : constant String
  --           := Exception_Messenger.Exception_Message(Except_Access));
  --     begin
  --       Exception_Messenger.Deallocate (Except_Access);
  --       ... put Str
  --     end;
  function Exception_Message (X : Ada.Exceptions.Exception_Occurrence_Access)
           return String;

  procedure Deallocate is new Ada.Unchecked_Deallocation (
    Ada.Exceptions.Exception_Occurrence,
    Ada.Exceptions.Exception_Occurrence_Access);

end Exception_Messenger;

