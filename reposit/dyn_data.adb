package body DYN_DATA is

  type CELL;
  type CELL_ACCESS is access CELL;
  type CELL is record
    DATA : DATA_ACCESS_TYPE;
    NEXT : CELL_ACCESS;
  end record;

  -- There is a linked list of data_access (pointing to free data) : FREE_DATA.
  -- This list needs cells.
  -- Each time a data is allocated, the cell describing it in FREE_DATA
  -- becomes useless and is moved in another list FREE_CELL.
  --
  --                 +---------+ +---------+
  --                 |free DATA| |free DATA|
  --                 +---------+ +---------+
  --                     ^           ^
  --                     |           |
  -- FIRST_FREE_DATA -> Cell ----> Cell ----> Null
  --
  --
  --                    Null        Null
  --                     ^           ^
  --                     |           |
  -- FIRST_FREE_CELL -> Cell ----> Cell ----> Null
  --


  -- On allocate, return first free data.
  -- The cell describing this first free data is moved at the
  --  beginning of the free cells list.
  -- If no free data is available, allocate (new) a new data.

  -- On free, move first free cell at the beginning of the free data list
  --  and make it point to this data.

  -- List of cells pointing to free data
  FIRST_FREE_DATA : CELL_ACCESS := null;
  -- List of free cells (not used in data list)
  FIRST_FREE_CELL : CELL_ACCESS := null;

  function ALLOCATE return DATA_ACCESS_TYPE is
    RETURNED_DATA_ACCESS : DATA_ACCESS_TYPE;
    CURRENT_CELL_ACCESS : CELL_ACCESS;
  begin
    -- try to use a free data, pointed by the first cell of free_data list
    if FIRST_FREE_DATA /= null then

      -- use data pointed by first free data
      CURRENT_CELL_ACCESS := FIRST_FREE_DATA;
      RETURNED_DATA_ACCESS := CURRENT_CELL_ACCESS.DATA;

      -- move this cell from free data list to the beginning of free cell list
      FIRST_FREE_DATA := CURRENT_CELL_ACCESS.NEXT;
      CURRENT_CELL_ACCESS.NEXT := FIRST_FREE_CELL;
      FIRST_FREE_CELL := CURRENT_CELL_ACCESS;
      CURRENT_CELL_ACCESS.DATA := null;
    else
      -- Allocate data
      RETURNED_DATA_ACCESS := new DATA_TYPE;
    end if;
    return RETURNED_DATA_ACCESS;
  end ALLOCATE;

  function ALLOCATE (DATA : DATA_TYPE) return DATA_ACCESS_TYPE is
    RETURNED_DATA_ACCESS : DATA_ACCESS_TYPE;
  begin
    RETURNED_DATA_ACCESS := ALLOCATE;
    RETURNED_DATA_ACCESS.all := DATA;
    return RETURNED_DATA_ACCESS;
  end ALLOCATE;

  procedure FREE (DATA_ACCESS : in out DATA_ACCESS_TYPE) is
    CURRENT_CELL_ACCESS : CELL_ACCESS;
  begin

    -- Check if DATA_ACCESS is not null
    if DATA_ACCESS = null then
      raise CONSTRAINT_ERROR;
    end if;

    -- Try to re-use a free cell
    if FIRST_FREE_CELL /= null then
      CURRENT_CELL_ACCESS := FIRST_FREE_CELL;
      -- Remove this cell from free_cell list
      FIRST_FREE_CELL := FIRST_FREE_CELL.NEXT;
    else
      CURRENT_CELL_ACCESS := new CELL;
    end if;

    -- Insert new cell in free data list and make it point to the free data
    CURRENT_CELL_ACCESS.DATA := DATA_ACCESS;
    CURRENT_CELL_ACCESS.NEXT := FIRST_FREE_DATA;
    FIRST_FREE_DATA := CURRENT_CELL_ACCESS;

    -- Reset data access
    DATA_ACCESS := null;
  end FREE;

end DYN_DATA;

