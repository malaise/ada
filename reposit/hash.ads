-- This hash package does not store the user's data but an access to it.
-- The user needs to associate a unique acess to each data
--  (index in an array, acess type...)

package HASH is

  -- Maximum size of primary hash table : 1023 (1Kb),
  -- due to the implemented hashing function
  MAX_HASH_VALUE : constant := 16#3FF#;
  type MAX_HASH_RANGE is new INTEGER range 0 .. MAX_HASH_VALUE;

  generic
    -- Size of primary hash table
    HASH_SIZE : MAX_HASH_RANGE := MAX_HASH_VALUE;
    -- acess to data, managed by client
    type DATA_ACESS is private;
    -- Procedure to dump data in DUMP
    with procedure DUMP (DATA : in DATA_ACESS);
  package HASH_MNG is

    -- Returned result of FIND_NEXT
    type FOUND_REC (FOUND : BOOLEAN := TRUE) is record
      case FOUND is
        when TRUE =>
          DATA : DATA_ACESS; -- significant only if FOUND
        when FALSE =>
          null;
      end case;
    end record;

    -- To store association KEY <-> INDEX
    -- Last found is not reset
    procedure STORE (KEY : in STRING; DATA : in DATA_ACESS);

    -- To reset finding index for matching KEY
    procedure RESET_FIND (KEY : STRING);

    -- To get first, then next INDEX matching KEY
    -- Last found is reset if not found
    function FIND_NEXT (KEY : STRING) return FOUND_REC;

    -- To remove last found association KEY <-> INDEX
    -- Last found is reset
    -- May raise NOT_FOUND if ast found is reset
    procedure REMOVE (KEY : in STRING);

    -- Dump hash value of key and lists all data found for key
    procedure DUMP (KEY : in STRING);

    -- Remove all the data stored in the hash table
    procedure CLEAR_ALL;

  end HASH_MNG;

  -- Raised on REMOVE if last found is not set
  NOT_FOUND : exception;

end HASH;
