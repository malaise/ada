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

  -- The buffer, in which current position is the "next to unget"
  --  when Offset is 0.
  -- When Offset is 1, there is no next to unget any more. This way:
  -- Get_Position (From_First) - Offset is the number of possible ungets.
  -- Get_Position (From_Last)  - 1 is the number of possible re-gets.
  -- Items from current (included, except when Offset = 1) to first included
  --  are the ones that can be ungot. So ungetting consists in moving current
  --  position backwards.
  -- Items from current excluded to last included are the ones to be got.
  --  So getting consists in moving current position forward if possible, and
  --  otherwise in getting from "outside" and appending to list.
  -- When recoding is inactive there cannot be any unget (previous items are
  --  removed from buffer) and getting consists in taking current item if
  --  possible, and otherwise getting from "outside".
  package Item_Dyn_List_Mng is new Dynamic_List (Item_Type);
  package Item_List_Mng renames Item_Dyn_List_Mng.Dyn_List;
  Item_List : Item_List_Mng.List_Type;

  -- The virtual offset of current position vs first
  -- Because, when at pos 1 and ungetting, virtual position becomes 0
  -- Only significant when Item_List is not empy
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
  -- Still allows re-get to use recorded unget but new unget are impossible
  -- No effect if recording is already stopped
  procedure Stop_Recording is
    Pos : Positive;
  begin
    if not Item_List.Is_Empty then
      -- Clear the buffer up to current pos
      if Offset = 0 then
        Pos := Item_List.Get_Position;
        Item_List.Rewind;
        for I in 1 .. Pos loop
          Item_List.Delete;
        end loop;
        -- Set offset to 1
        Offset := 1;
      end if;
    end if;
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
    if Item_List.Is_Empty
    or else (Offset = 0 and then not Item_List.Check_Move) then
      -- No list or end of list of unget
      Item := Get_Item (User_Data);
      if Recording then
        -- Control the max length of the list
        if Unget_Length /= 0
        and then Item_List.List_Length = Unget_Length then
          -- Remove oldest got item
          Item_List.Rewind;
          Item_List.Delete;
          -- Be ready to append
          Item_List.Rewind (True, Item_List_Mng.Prev);
        end if;
        -- Append new got item
        Item_List.Insert (Item);
        Offset := 0;
      end if;
    elsif Offset = 1 then
      -- Ungets performed up to beggining of list
      -- Getting moves to first
      if Recording then
        Item_List.Read (Item, Item_List_Mng.Current);
        Offset := 0;
      else
        Item_List.Get (Item);
      end if;
    else
      -- Current pos, "next to unget" is followed by an Item: the one to re-get
      if Recording then
        Item_List.Read (Item);
      else
        Item_List.Get (Item);
      end if;
    end if;
    return Item;
  end Get;

  -- Returns the number of Unget that can be done (0 when recording is not active)
  function Nb_Unget return Unget_Range is
  begin
    if not Recording or else Item_List.Is_Empty then
      return 0;
    end if;
    return Item_List.Get_Position - Offset;
  end Nb_Unget;


  -- Ungets one or several gets (0 for all, Nb_Unget)
  -- Raises To_Many_Unget if Number > Nb_Unget (e.g. recording inactive)
  procedure Unget (Number : in Natural := 1) is
  begin
    if not Recording then
      raise To_Many_Unget;
    end if;
    if Number = 0 then
      -- Unget all
      if not Item_List.Is_Empty then
        -- Move at first pos - 1
        Item_List.Rewind;
        Offset := 1;
      end if;
      return;
    end if;
    if Number > Nb_Unget then
      raise To_Many_Unget;
    end if;
    if Item_List.Get_Position = 1 then
      -- Already ungot all items but first, simulate move at index -1
      Offset := 1;
    else
      -- Move "back" current pos in the history of got items
      Item_List.Move_To (Item_List_Mng.Prev, Number, From_Current => True);
    end if;
  end Unget;

  -- To_Many_Unget : exception;

end Multiget;


