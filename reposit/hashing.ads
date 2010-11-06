-- This hash package does not store the user's data but an access to it.
-- The user needs to associate a unique acess to each data
--  (index in an array, acess type...)
with Ada.Finalization;
with Crc_10;
package Hashing is
  -- Maxmum size of the primary hash table, reasonnable
  type Max_Hash_Range is new Natural range 0 .. 16#FFFFFF#;
  Max_Hash_Value : constant Max_Hash_Range := Max_Hash_Range'Last;

  -- Raised on Remove if last found is not set
  Not_Found : exception;

  -- Default hashing function is Crc_10
  -- Default maximum size of primary hash table due to the default hashing
  --  function
  subtype Def_Max_Hash_Range is Max_Hash_Range
          range 0 .. Max_Hash_Range(Crc_10.Max_Crc_Value);

  generic
    -- Size of primary hash table
    Hash_Max : Max_Hash_Range := Def_Max_Hash_Range'Last;
  package Sized_Hash is

    -- The possible hash values
    subtype Hash_Range is Max_Hash_Range range 0 .. Hash_Max;

    -- Default function, returns default hashing truncated to Hash_Max
    -- It is adapteed when Hash_Max <= Def_Max_Hash_Range
    -- Otherwise the user shall provide its own Hash_Func (maybe based
    --  on Hash_Def_Func)
    function Hash_Def_Func (Key : String) return Hash_Range;

    -- Raised on Remove if last found is not set
    Not_Found : exception renames Hashing.Not_Found;

    generic
      -- Acess to data, managed by client
      type Data_Acess is private;
      -- Procedure to dump data in Dump
      with procedure Dump (Data : in Data_Acess);
      -- Hashing function
      with function Hash_Func (Key : String) return Hash_Range is Hash_Def_Func;
    package Hash_Mng is

      subtype Hash_Range is Sized_Hash.Hash_Range;

      -- One hash table
      type Hash_Table is tagged limited private;

      -- Returned result of Find_Next
      type Found_Rec (Found : Boolean := True) is record
        case Found is
          when True =>
            Data : Data_Acess; -- significant only if Found
          when False =>
            null;
        end case;
      end record;

      -- Raised on Remove if last found is not set
      Not_Found : exception renames Hashing.Not_Found;

      -- To store association Key <-> Index
      -- Last found is not reset
      procedure Store (Table : in out Hash_Table;
                       Key   : in String;
                       Data  : in Data_Acess);

      -- To reset finding index for matching Key
      procedure Reset_Find (Table : in out Hash_Table; Index : in Hash_Range);
      procedure Reset_Find (Table : in out Hash_Table; Key   : in String);

      -- To get first, then next Index matching Key
      -- Last found is reset if not found
      procedure Find_Next (Table : in out Hash_Table;
                           Index : in Hash_Range;
                           Found : out Found_Rec);
      procedure Find_Next (Table : in out Hash_Table;
                           Key   : in String;
                           Found : out Found_Rec);

      -- To remove last found association Key <-> Index
      -- Last found is reset
      -- Beware that this can be expensive in time/cpu if the hash tree
      --  depth is important
      -- May raise Not_Found if last found is reset
      procedure Remove (Table : in out Hash_Table;
                        Index : in Hash_Range);
      procedure Remove (Table : in out Hash_Table;
                        Key   : in String);

      -- Dump hash value of key and lists all data found for key
      procedure Dump (Table : in Hash_Table;
                      Index : in Hash_Range);
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
        Last      : Cell_Access := null;
      end record;


      -- The entries of the table
      type Hash_Arr is array (Hash_Range) of First_Cell_Rec;
      type Hash_Table is limited new Ada.Finalization.Limited_Controlled
                                 with record
        Arr : Hash_Arr;
      end record;
      overriding procedure Finalize (Table : in out Hash_Table) renames Clear_All;

    end Hash_Mng;

  end Sized_Hash;

end Hashing;

