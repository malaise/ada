-- Allocates and frees cells of data access,
--  using a free list to re-use free cells.
generic
  type Data_Type is private;
  type Data_Access_Type is access Data_Type;

package Dyn_Data is

  -- Allocates a new cell.
  -- The result is the access to a pre allocated area for DATA_TYPE.
  function Allocate return Data_Access_Type;

  -- Allocates a new cell and fills it with DATA
  -- The result is the access to a pre allocated area for DATA_TYPE,
  --  storing DATA
  function Allocate (Data : Data_Type) return Data_Access_Type;

  -- Frees a cell. DATA_ACCESS is set to null.
  procedure Free (Data_Access : in out Data_Access_Type);

end Dyn_Data;

