-- Allocates and frees cells of data access,
--  using a free list to re-use free cells.
generic
  type Data_Type is private;
  type Data_Access_Type is access Data_Type;

package Dyn_Data is

  -- Allocates a new cell.
  -- The result is the access to a pre allocated area for Data_Type.
  function Allocate return Data_Access_Type;

  -- Allocates a new cell and fills it with Data
  -- The result is the access to a pre allocated area for Data_Type,
  --  storing Data
  function Allocate (Data : Data_Type) return Data_Access_Type;

  -- Frees a cell. Data_Access is set to null.
  procedure Free (Data_Access : in out Data_Access_Type);

  -- Clear the free list
  procedure Clear;

end Dyn_Data;

