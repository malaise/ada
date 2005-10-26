with Ada.Unchecked_Deallocation;
package body Dyn_Data is

  type Cell;
  type Cell_Access is access Cell;
  type Cell is record
    Data : Data_Access_Type;
    Next : Cell_Access;
  end record;

  -- There is a linked list of data_access (pointing to free data) : Free_Data.
  -- This list needs cells.
  -- Each time a data is allocated, the cell describing it in Free_Data
  -- becomes useless and is moved in another list Free_Cell.
  --
  --                 +---------+ +---------+
  --                 |free DATA| |free DATA|
  --                 +---------+ +---------+
  --                     ^           ^
  --                     |           |
  -- First_Free_Data -> Cell ----> Cell ----> Null
  --
  --
  --                    Null        Null
  --                     ^           ^
  --                     |           |
  -- First_Free_Cell -> Cell ----> Cell ----> Null
  --


  -- On allocate, return first free data.
  -- The cell describing this first free data is moved at the
  --  beginning of the free cells list.
  -- If no free data is available, allocate (new) a new data.

  -- On free, move first free cell at the beginning of the free data list
  --  and make it point to this data.

  -- List of cells pointing to free data
  First_Free_Data : Cell_Access := null;
  -- List of free cells (not used in data list)
  First_Free_Cell : Cell_Access := null;

  function Allocate return Data_Access_Type is
    Returned_Data_Access : Data_Access_Type;
    Current_Cell_Access : Cell_Access;
  begin
    -- try to use a free data, pointed by the first cell of free_data list
    if First_Free_Data /= null then

      -- use data pointed by first free data
      Current_Cell_Access := First_Free_Data;
      Returned_Data_Access := Current_Cell_Access.Data;

      -- move this cell from free data list to the beginning of free cell list
      First_Free_Data := Current_Cell_Access.Next;
      Current_Cell_Access.Next := First_Free_Cell;
      First_Free_Cell := Current_Cell_Access;
      Current_Cell_Access.Data := null;
    else
      -- Allocate data
      Returned_Data_Access := new Data_Type;
    end if;
    return Returned_Data_Access;
  end Allocate;

  function Allocate (Data : Data_Type) return Data_Access_Type is
    Returned_Data_Access : Data_Access_Type;
  begin
    Returned_Data_Access := Allocate;
    Returned_Data_Access.all := Data;
    return Returned_Data_Access;
  end Allocate;

  procedure Free (Data_Access : in out Data_Access_Type) is
    Current_Cell_Access : Cell_Access;
  begin

    -- Check if Data_Access is not null
    if Data_Access = null then
      raise Constraint_Error;
    end if;

    -- Try to re-use a free cell
    if First_Free_Cell /= null then
      Current_Cell_Access := First_Free_Cell;
      -- Remove this cell from free_cell list
      First_Free_Cell := First_Free_Cell.Next;
    else
      Current_Cell_Access := new Cell;
    end if;

    -- Insert new cell in free data list and make it point to the free data
    Current_Cell_Access.Data := Data_Access;
    Current_Cell_Access.Next := First_Free_Data;
    First_Free_Data := Current_Cell_Access;

    -- Reset data access
    Data_Access := null;
  end Free;

  procedure Free_Data is new Ada.Unchecked_Deallocation
                            (Data_Type, Data_Access_Type);
  procedure Free_Cell is new Ada.Unchecked_Deallocation
                            (Cell, Cell_Access);

  -- Clear all the list from Cell_Acc (including data)
  procedure Clear (Cell_Acc : in out Cell_Access) is
    Next_Acc : Cell_Access;
  begin
    while Cell_Acc /= null loop
      if Cell_Acc.Data /= null then
        Free_Data (Cell_Acc.Data);
      end if;
      Next_Acc := Cell_Acc.Next;
      Free_Cell (Cell_Acc);
      Cell_Acc := Next_Acc;
    end loop;
  end Clear;

  -- Clear the free lists
  procedure Clear is
  begin
    Clear (First_Free_Data);
    Clear (First_Free_Cell);
  end Clear;

end Dyn_Data;

