package GRID_1 is


  subtype ROW_COORDINATE is CHARACTER range 'A' .. 'J';
  subtype COL_COORDINATE is CHARACTER range 'K' .. 'T';


  type COORDINATE_REC is record
    ROW : ROW_COORDINATE;
    COL : COL_COORDINATE;
  end record;


  procedure INITIALIZE (KEY : in string);

  -- C can be any char from ' ' to '~' or ASCII.CR
  -- Any other char is discarded
  function ENCODE (C : CHARACTER) return COORDINATE_REC;

  function DECODE (COORDINATE : COORDINATE_REC) return CHARACTER;

  GRID_NOT_INIT : exception;
  INVALID_CHARACTER : EXCEPTION;

  procedure DUMP;

end GRID_1;
