-- This hash package does not store the user's data but an access to it.
-- The user needs to associate a unique acess to each data
--  (index in an array, acess type...)
with Crc_10;
package Hash is

  -- Maximum size of primary hash table
  -- due to the implemented hashing function
  Max_Hash_Value : constant := Crc_10.Max_Crc_Value;
  subtype Max_Hash_Range is Crc_10.Max_Crc_Range;

  generic
    -- Size of primary hash table
    Hash_Size : Max_Hash_Range := Max_Hash_Value;
    -- Acess to data, managed by client
    type Data_Acess is private;
    -- Procedure to dump data in DUMP
    with procedure Dump (Data : in Data_Acess);
  package Hash_Mng is

    -- Returned result of FIND_NEXT
    type Found_Rec (Found : Boolean := True) is record
      case Found is
        when True =>
          Data : Data_Acess; -- significant only if FOUND
        when False =>
          null;
      end case;
    end record;

    -- To store association KEY <-> INDEX
    -- Last found is not reset
    procedure Store (Key : in String; Data : in Data_Acess);

    -- To reset finding index for matching KEY
    procedure Reset_Find (Key : String);

    -- To get first, then next INDEX matching KEY
    -- Last found is reset if not found
    function Find_Next (Key : String) return Found_Rec;

    -- To remove last found association KEY <-> INDEX
    -- Last found is reset
    -- May raise NOT_FOUND if last found is reset
    procedure Remove (Key : in String);

    -- Dump hash value of key and lists all data found for key
    procedure Dump (Key : in String);

    -- Remove all the data stored in the hash table
    procedure Clear_All;

  end Hash_Mng;

  -- Raised on REMOVE if last found is not set
  Not_Found : exception;

end Hash;

