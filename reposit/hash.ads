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
    -- Procedure to dump data in Dump
    with procedure Dump (Data : in Data_Acess);
  package Hash_Mng is

    -- Returned result of Find_Next
    type Found_Rec (Found : Boolean := True) is record
      case Found is
        when True =>
          Data : Data_Acess; -- significant only if Found
        when False =>
          null;
      end case;
    end record;

    -- To store association Key <-> Index
    -- Last found is not reset
    procedure Store (Key : in String; Data : in Data_Acess);

    -- To reset finding index for matching Key
    procedure Reset_Find (Key : String);

    -- To get first, then next Index matching Key
    -- Last found is reset if not found
    function Find_Next (Key : String) return Found_Rec;

    -- To remove last found association Key <-> Index
    -- Last found is reset
    -- May raise Not_Found if last found is reset
    procedure Remove (Key : in String);

    -- Dump hash value of key and lists all data found for key
    procedure Dump (Key : in String);

    -- Remove all the data stored in the hash table
    procedure Clear_All;

  end Hash_Mng;

  -- Raised on Remove if last found is not set
  Not_Found : exception;

end Hash;

