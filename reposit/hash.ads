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

    -- One hash table
    type Hash_Table is limited private;

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
    procedure Store (Table : in out Hash_Table;
                     Key   : in String;
                     Data  : in Data_Acess);

    -- To reset finding index for matching Key
    procedure Reset_Find (Table : in out Hash_Table;
                          Key   : String);

    -- To get first, then next Index matching Key
    -- Last found is reset if not found
    procedure Find_Next (Table : in out Hash_Table;
                         Key   : in String;
                         Found  : out Found_Rec);

    -- To remove last found association Key <-> Index
    -- Last found is reset
    -- May raise Not_Found if last found is reset
    procedure Remove (Table : in out Hash_Table;
                      Key   : in String);

    -- Dump hash value of key and lists all data found for key
    procedure Dump (Table : in Hash_Table;
                    Key   : in String);

    -- Remove all the data stored in the hash table
    procedure Clear_All (Table : in out Hash_Table);

  private
    -- Data organization:
    --  An array (0 .. Hash_Size) of First_Cell_Rec
    --  Each of them pointing possibly to a Cell_Rec which itself...
    type Cell_Rec;
    type Cell_Access is access Cell_Rec;

    type Cell_Rec is record
      Data : Data_Acess;
      Next : Cell_Access := null;
    end record;


    type First_Cell_Rec is record
      First     : Cell_Access := null;
      Current   : Cell_Access := null;
    end record;

    subtype Hash_Range is Max_Hash_Range range 0 .. Hash_Size;

    -- The entries of the table
    type Hash_Table is array (Hash_Range) of First_Cell_Rec;
  end Hash_Mng;

  -- Raised on Remove if last found is not set
  Not_Found : exception;

end Hash;

