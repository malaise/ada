-- Allows to get/unget/re-get items (from a input flow?)
generic
  -- The type of item got
  type Item_Type is private;
  -- The user data passed through Get to Get_Item
  type User_Data_Type is private;
  -- The original function getting Items
  -- At end of input flow, it may return a specific Item or raise an exception
  with function Get_Item (User_Data : User_Data_Type) return Item_Type;
  -- The size of buffer of Items that can be ungot
  -- 0 means unlimited
  Unget_Length : Natural := 0;

package Multiget is

  -- The number of possible unget
  subtype Unget_Range is Natural range 0 .. Unget_Length;

  -- Starts recording (recording is inactive by default)
  -- Any Get performed while recording is active can be ungot.
  -- No effect if recording is already active
  procedure Start_Recording;

  -- Stops recording
  -- Clears the buffer of recorded got items
  -- No effect if recording is already stopped
  procedure Stop_Recording;

  -- Is recording active or not
  function Is_Recording return Boolean;

  -- The getting function
  -- Propagates any exception of the Get_Item (error or end of file...)
  -- If recording, copies each Item got in a buffer (for further unget).
  -- When the buffer is full, the oldest got item is overwritten by the new one
  function Get (User_Data : User_Data_Type) return Item_Type;

  -- Returns the number of Unget that can be done (0 when recording is not active)
  function Nb_Unget return Unget_Range;

  -- Ungets one or several gets (0 for all, Nb_Unget)
  -- Raises To_Many_Unget if Number > Nb_Unget (e.g. recording inactive)
  procedure Unget (Number : in Natural := 1);
  To_Many_Unget : exception;

end Multiget;

