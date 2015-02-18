-- This hash package does not store the user's data but an access to it.
-- The user needs to associate a unique access to each data
--  (index in an array, access type...)
with Ada.Finalization;
with Long_Longs, Hash_Function;
package Hashing is
  -- Maxmum size of the primary hash table (16_777_215)
  subtype Max_Hash_Range is Hash_Function.Hash_Range;
  Max_Hash_Value : constant Max_Hash_Range := Max_Hash_Range'Last;

  -- Where inserting a new data in current branch
  type Where_Insert_List is (First, Last, After_Curr, Before_Curr);

  -- In which direction searching in current branch
  type Direction_List is (Forward, Backward);

  -- Raised on Remove or on Store (After_Curr | Before_Curr)
  --  if last found is not set
  Not_Found : exception;

  -- Raised on Store if Ll_Natural'Last items already in current branch
  Too_Many : exception;

  -- Default maximum size of primary hash table, number of branches (4095)
  subtype Def_Max_Hash_Range is Max_Hash_Range range 0 .. 16#FFF#;
  Def_Max_Hash_Value : constant Def_Max_Hash_Range := Def_Max_Hash_Range'Last;
  function Def_Max_Hash_Func (Key : String) return Def_Max_Hash_Range;

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
      -- Access to data, managed by client
      type Data_Access is private;
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
            Data : Data_Access; -- significant only if Found
          when False =>
            null;
        end case;
      end record;

      -- Raised on Remove if last found is not set
      Not_Found : exception renames Hashing.Not_Found;

      -- Raised on Store if Ll_Natural'Last items already in current branch
      Too_Many : exception renames Hashing.Too_Many;

      -- The number of items on a branch
      subtype Depth_Range is Long_Longs.Ll_Mod_Natural;

      -- To store association Key/Index <-> Data
      -- Last found is not reset
      procedure Store (Table : in out Hash_Table;
                       Index : in Hash_Range;
                       Data  : in Data_Access;
                       Where : in Where_Insert_List := Last);
      procedure Store (Table : in out Hash_Table;
                       Key   : in String;
                       Data  : in Data_Access;
                       Where : in Where_Insert_List := Last);

      -- To reset finding for Index or Key
      procedure Reset_Find (Table : in out Hash_Table; Index : in Hash_Range);
      procedure Reset_Find (Table : in out Hash_Table; Key   : in String);

      -- Return the depth of the table for this Index or Key
      function Depth (Table : Hash_Table; Index : Hash_Range)
                     return Depth_Range;
      function Depth (Table : Hash_Table; Key   : String)
                     return Depth_Range;

      -- To get first, then next Data for Index or Key
      -- Last found is reset if not found
      procedure Find_Next (Table     : in out Hash_Table;
                           Index     : in Hash_Range;
                           Found     : out Found_Rec;
                           Direction : in Direction_List := Forward);
      function  Find_Next (Table     : in out Hash_Table;
                           Index     : in Hash_Range;
                           Direction : in Direction_List := Forward)
                return Found_Rec;

      procedure Find_Next (Table     : in out Hash_Table;
                           Key       : in String;
                           Found     : out Found_Rec;
                           Direction : in Direction_List := Forward);
      function  Find_Next (Table     : in out Hash_Table;
                           Key       : in String;
                           Direction : in Direction_List := Forward)
                return Found_Rec;

      -- To re-read data previously found at Index or Key
      procedure Re_Read (Table : in out Hash_Table;
                         Index : in Hash_Range;
                         Found : out Found_Rec);
      function  Re_Read (Table : in out Hash_Table;
                         Index : in Hash_Range) return Found_Rec;

      procedure Re_Read (Table : in out Hash_Table;
                         Key   : in String;
                         Found : out Found_Rec);
      function  Re_Read (Table : in out Hash_Table;
                         Key   : in String) return Found_Rec;

      -- To remove last data found at Index or Key
      -- Last found is reset
      -- May raise Not_Found if last found is not set
      procedure Remove (Table : in out Hash_Table;
                        Index : in Hash_Range);
      procedure Remove (Table : in out Hash_Table;
                        Key   : in String);

      -- Dump hash value of key and list all data found for key
      procedure Dump (Table     : in Hash_Table;
                      Index     : in Hash_Range;
                      Put       : access procedure (Data : in Data_Access);
                      Direction : in Direction_List := Forward);
      procedure Dump (Table     : in Hash_Table;
                      Key       : in String;
                      Put       : access procedure (Data : in Data_Access);
                      Direction : in Direction_List := Forward);

      -- Remove all the data stored in the hash table
      procedure Clear_All (Table : in out Hash_Table);

    private
      -- Data organization:
      --  An array (0 .. Hash_Size) of First_Cell_Rec
      --  Each of them pointing possibly to a Cell_Rec which itself...
      type Cell_Rec;
      type Cell_Access is access Cell_Rec;

      type Cell_Rec is record
        Data : Data_Access;
        Next : Cell_Access := null;
        Prev : Cell_Access := null;
      end record;


      type First_Cell_Rec is record
        First     : Cell_Access := null;
        Last      : Cell_Access := null;
        Current   : Cell_Access := null;
        Depth     : Depth_Range := 0;
      end record;


      -- The entries of the table
      type Hash_Arr is array (Hash_Range) of First_Cell_Rec;
      type Hash_Table is limited new Ada.Finalization.Limited_Controlled
                                 with record
        Arr : Hash_Arr;
      end record;
      overriding procedure Finalize (Table : in out Hash_Table)
                                    renames Clear_All;

    end Hash_Mng;

  end Sized_Hash;

end Hashing;

