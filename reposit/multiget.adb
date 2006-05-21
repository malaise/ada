-- Allows to get/unget/re-get items (from a input flow?)
-- generic
  -- The type of item got
  -- type Item_Type is private;
  -- The user data passed through Get to Get_Item
  -- type User_Data_Type is private;
  -- The original function getting Items
  -- At end of input flow, it may return a specific Item or raise an exception
  -- with function Get_Item (User_Data : User_Data_Type) return Item_Type;
  -- The size of buffer of Items that can be ungot
  -- 0 means unlimited
  -- Unget_Length : Natural;

with Dynamic_List;
package body Multiget is

  -- The buffer, in which current position is the "next to unget" when Offset is 0.
  -- When Offset is 1, there is no next to unget any more. This way:
  -- Get_Position (From_First) - Offset is the number of possible ungets
  -- Get_Position (From_Last)  - 1 is the number of possible re-gets
  package Item_Dyn_List_Mng is new Dynamic_List (Item_Type);
  package Item_List_Mng renames Item_Dyn_List_Mng.Dyn_List;
  Item_List : Item_List_Mng.List_Type;

  -- The virtual offset of current position vs first
  -- Because, when at pos 1 and ungetting, virtual position becomes 0
  subtype Offset_Range is Natural range 0 .. 1;
  Offset : Offset_Range := 0;

  -- Are we recording
  Recording : Boolean := False;

  -- The number of possible unget
  -- subtype Unget_Range is Natural range 0 .. Unget_Length;

  -- Starts recording
  -- Any Get performed while recording is active can be ungot.
  -- No effect if recording is already active
  procedure Start_Recording is
  begin
    Recording := True;
  end Start_Recording;

  -- Stops recording
  -- Clears the buffer of recorded got items
  -- No effect if recording is already stopped
  procedure Stop_Recording is
  begin
    -- Clear the buffer
    Item_List_Mng.Delete_List (Item_List);
    -- Init offset, by default unget is possible as long as pos > 1
    Offset := 0;
    Recording := False;
  end Stop_Recording;

  -- Is recording active or not
  function Is_Recording return Boolean is
  begin
    return Recording;
  end Is_Recording;


  -- The getting function
  -- Propagates any exception of the Get_Item (error or end of file...)
  -- If recording, copies each Item got in a buffer (for further unget).
  -- When the buffer is full, the oldest got item is overwritten by the new one
  function Get (User_Data : User_Data_Type) return Item_Type is
    Item : Item_Type;
  begin
    if not Recording then
      Item := Get_Item (User_Data);
    elsif Offset = 1 then
      -- Ungets performed up to beggining of list
      -- Getting moves to first
      Item_List_Mng.Read (Item_List, Item, Item_List_Mng.Current);
      Offset := 0;
    elsif Item_List_Mng.Check_Move (Item_List) then
      -- Current pos, "next to unget" is followed by an Item: the one to re-get
      Item_List_Mng.Read (Item_List, Item);
    else
      -- Current pos, "next to unget" is last item of list, so there is no following item
      -- to re-get => Get a new item and append it to the list
      Item := Get_Item (User_Data);
      -- Control the max length of the list
      if Unget_Length /= 0
      and then Item_List_Mng.List_Length (Item_List) = Unget_Length then
        -- Remove oldest got item
        Item_List_Mng.Rewind (Item_List);
        Item_List_Mng.Delete (Item_List);
        -- Be ready to append
        Item_List_Mng.Rewind (Item_List, Item_List_Mng.Prev);
      end if;
      Item_List_Mng.Insert (Item_List, Item);
    end if;
    return Item;
  end Get;

  -- Returns the number of Unget that can be done (0 when recording is not active)
  function Nb_Unget return Unget_Range is
  begin
    if Item_List_Mng.Is_Empty (Item_List) then
      return 0;
    end if;
    return Item_List_Mng.Get_Position (Item_List) - Offset;
  end Nb_Unget;


  -- Ungets one or several gets (0 for all, Nb_Unget)
  -- Raises To_Many_Unget if Number > Nb_Unget (e.g. recording inactive)
  procedure Unget (Number : in Natural := 1) is
  begin
    if Number = 0 then
      -- Unget all
      if not Item_List_Mng.Is_Empty (Item_List) then
        -- Move at first pos - 1
        Item_List_Mng.Rewind (Item_List);
        Offset := 1;
      end if;
      return;
    end if;
    if Number > Nb_Unget then
      raise To_Many_Unget;
    end if;
    if Item_List_Mng.Get_Position (Item_List) = 1 then
      -- Already ungot all items but first, simulate move at index -1
      Offset := 1;
    else
      -- Move "back" current pos in the history of got items
      Item_List_Mng.Move_To (Item_List, Item_List_Mng.Prev, Number, From_Current => True);
    end if;
  end Unget;

  -- To_Many_Unget : exception;

end Multiget;


