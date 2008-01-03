with Ada.Exceptions;
-- In Ada.Exceptions, the string returned by Exception_Message may be truncated
-- (to no less than 200 characters).
-- This package allows to store in a global pool a "long" message and
--  transfer a short access key as the exception message.

-- A message of size below Max_Message_Length will be directly attached
--  to the exception. Otherwise the message is stored on a (protected)
--  pool and the key image of it is used to build a key string that is
--  attached to the exception for later retrieval.

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


  -- Retrieve the message associated to an exception
  -- If the message attached does not match "Exception_Messenger_Key_xxx",
  --  then return this message.
  -- If it matches but is not (any more) in the pool, then return "".
  -- If it is found in the pool then suppress it from the pool and return it.
  function Exception_Message (X : Ada.Exceptions.Exception_Occurrence_Access)
     return String;

end Exception_Messenger;

