-- Allows to get/unget/re-get items (from a input flow?)
with Dynamic_List;
generic
  -- The type of item got
  type Item_Type is private;
  -- Any user data that can be passed "as is" through Get to Get_Item
  type User_Data_Type is private;
  -- The original function getting Items
  -- At end of input flow, it may return a specific Item or raise an exception
  with function Get_Item (User_Data : User_Data_Type) return Item_Type;
  -- The size of buffer of Items that can be ungot
  -- 0 means unlimited
  Unget_Length : Natural := 0;

package Multiget is

  -- The number of possible unget
  subtype Unget_Range is Natural;

  type Multigetter is tagged limited private;

  -- Starts recording (recording is inactive by default)
  -- Any Get performed while recording is active can be ungot.
  -- No effect if recording is already active
  procedure Start_Recording (Getter : in out Multigetter);

  -- Stops recording
  -- Still allows re-get to use recorded unget but new unget are impossible
  -- No effect if recording is already stopped
  procedure Stop_Recording (Getter : in out Multigetter);

  -- Is recording active or not
  function Is_Recording (Getter : Multigetter) return Boolean;

  -- The getting function
  -- Calls Get_Item (User_Data) and return its result
  -- Propagates any exception of the Get_Item (error or end of file...)
  -- If recording, copies each Item got in a buffer (for further unget).
  -- When the buffer is full, the oldest got item is overwritten by the new one
  function Get (Getter : in out Multigetter; User_Data : in User_Data_Type)
                return Item_Type;

  -- Returns the number of Unget that can be done (0 when recording is
  --  not active)
  function Nb_Unget (Getter : Multigetter) return Unget_Range;

  -- Ungets one or several gets (0 for all, Nb_Unget)
  -- Raises To_Many_Unget if Number > Nb_Unget (e.g. recording inactive)
  To_Many_Unget : exception;
  procedure Unget (Getter : in out Multigetter; Number : in Natural := 1);

  -- Reset unget buffer
  procedure Reset (Getter : in out Multigetter);


private
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

 -- The virtual offset of current position vs first
  -- Because, when at pos 1 and ungetting, virtual position becomes 0
  -- Only significant when Item_List is not empy
  subtype Offset_Range is Natural range 0 .. 1;


  -- The context of a mutiget
  type Multigetter is tagged limited record
    -- The buffer, in which current position is the "next to unget"
    --  when offset is 0
    Item_List : Item_List_Mng.List_Type;
    -- The virtual offset of current position vs first
    Offset : Offset_Range := 0;
    -- Are we recording
    Recording : Boolean := False;
  end record;

end Multiget;

