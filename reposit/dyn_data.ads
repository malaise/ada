-- Allocates and frees cells of data access,
--  using a free list to re-use free cells.
generic
  type DATA_TYPE is private;
  type DATA_ACCESS_TYPE is access DATA_TYPE;

package DYN_DATA is

  -- Allocates a new cell.
  -- The result is the access to a pre allocated area for DATA_TYPE.
  function ALLOCATE return DATA_ACCESS_TYPE;

  -- Allocates a new cell and fills it with DATA
  -- The result is the access to a pre allocated area for DATA_TYPE,
  --  storing DATA
  function ALLOCATE (DATA : DATA_TYPE) return DATA_ACCESS_TYPE;

  -- Frees a cell. DATA_ACCESS is set to null.
  procedure FREE (DATA_ACCESS : in out DATA_ACCESS_TYPE);

end DYN_DATA;

