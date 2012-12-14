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
with Unchecked_Deallocation;
package body Multiget is

  procedure Free is new Unchecked_Deallocation (Getter_Rec, Getter_Access);


  -- Starts recording
  -- Any Get performed while recording is active can be ungot.
  -- No effect if recording is already active
  procedure Start_Recording (Getter : in out Multigetter) is
  begin
    Getter.Acc.Recording := True;
  end Start_Recording;

  -- Stops recording
  -- Still allows re-get to use recorded unget but new unget are impossible
  -- No effect if recording is already stopped
  procedure Stop_Recording (Getter : in out Multigetter) is
    Pos : Positive;
  begin
    if not Getter.Acc.Item_List.Is_Empty then
      -- Clear the buffer up to current pos
      if Getter.Acc.Offset = 0 then
        Pos := Getter.Acc.Item_List.Get_Position;
        Getter.Acc.Item_List.Rewind;
        for I in 1 .. Pos loop
          Getter.Acc.Item_List.Delete;
        end loop;
        -- Set offset to 1
        Getter.Acc.Offset := 1;
      end if;
    end if;
    Getter.Acc.Recording := False;
  end Stop_Recording;

  -- Is recording active or not
  function Is_Recording (Getter : Multigetter) return Boolean is
  begin
    return Getter.Acc.Recording;
  end Is_Recording;


  -- The getting function
  -- Propagates any exception of the Get_Item (error or end of file...)
  -- If recording, copies each Item got in a buffer (for further unget).
  -- When the buffer is full, the oldest got item is overwritten by the new one
  function Get (Getter : Multigetter; User_Data : User_Data_Type)
               return Item_Type is
    Item : Item_Type;
  begin
    if Getter.Acc.Item_List.Is_Empty
    or else (Getter.Acc.Offset = 0
             and then not Getter.Acc.Item_List.Check_Move) then
      -- No list or end of list of unget
      Item := Get_Item (User_Data);
      if Getter.Acc.Recording then
        -- Control the max length of the list
        if Unget_Length /= 0
        and then Getter.Acc.Item_List.List_Length = Unget_Length then
          -- Remove oldest got item
          Getter.Acc.Item_List.Rewind;
          Getter.Acc.Item_List.Delete;
          -- Be ready to append
          Getter.Acc.Item_List.Rewind (True, Item_List_Mng.Prev);
        end if;
        -- Append new got item
        Getter.Acc.Item_List.Insert (Item);
        Getter.Acc.Offset := 0;
      end if;
    elsif Getter.Acc.Offset = 1 then
      -- Ungets performed up to beggining of list
      -- Getting moves to first
      if Getter.Acc.Recording then
        Getter.Acc.Item_List.Read (Item, Item_List_Mng.Current);
        Getter.Acc.Offset := 0;
      else
        Getter.Acc.Item_List.Get (Item);
      end if;
    else
      -- Current pos, "next to unget" is followed by an Item: the one to re-get
      if Getter.Acc.Recording then
        Getter.Acc.Item_List.Read (Item);
      else
        Getter.Acc.Item_List.Get (Item);
      end if;
    end if;
    return Item;
  end Get;

  -- Returns the number of Unget that can be done (0 when recording is not active)
  function Nb_Unget (Getter : Multigetter) return Unget_Range is
  begin
    if not Getter.Acc.Recording or else Getter.Acc.Item_List.Is_Empty then
      return 0;
    end if;
    return Getter.Acc.Item_List.Get_Position - Getter.Acc.Offset;
  end Nb_Unget;


  -- Ungets one or several gets (0 for all, Nb_Unget)
  -- Raises To_Many_Unget if Number > Nb_Unget (e.g. recording inactive)
  procedure Unget (Getter : in out Multigetter; Number : in Natural := 1) is
  begin
    if not Getter.Acc.Recording then
      raise To_Many_Unget;
    end if;
    if Number = 0 then
      -- Unget all
      if not Getter.Acc.Item_List.Is_Empty then
        -- Move at first pos - 1
        Getter.Acc.Item_List.Rewind;
        Getter.Acc.Offset := 1;
      end if;
      return;
    end if;
    if Number > Nb_Unget (Getter) then
      raise To_Many_Unget;
    end if;
    if Getter.Acc.Item_List.Get_Position = 1 then
      -- Already ungot all items but first, simulate move at index -1
      Getter.Acc.Offset := 1;
    else
      -- Move "back" current pos in the history of got items
      Getter.Acc.Item_List.Move_To (Item_List_Mng.Prev, Number,
                                    From_Current => True);
    end if;
  end Unget;

  -- Reset unget buffer
  procedure Reset (Getter : in out Multigetter) is
  begin
    Getter.Acc.Item_List.Delete_List (Deallocate => True);
    Getter.Acc.Offset := 0;
    Getter.Acc.Recording := False;
  end Reset;

  overriding procedure Finalize (File : in out Multigetter) is
  begin
    Free (File.Acc);
  end Finalize;

end Multiget;


