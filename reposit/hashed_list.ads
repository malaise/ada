with Ada.Finalization;
with Limited_List, Hashing;
generic
  -- Type of the element of the list
  type Element_Type is limited private;
  type Element_Access is access all Element_Type;
  -- Affectation of elements
  with procedure Set (To : out Element_Type; Val : in Element_Type);
  -- Criteria of unicity (and search) of elements
  -- Must return True if and only if Current and Criteria have the same key
  with function "=" (Current : Element_Type; Criteria : Element_Type)
                return Boolean;
  -- A string representing the key (search criteria) for hashing.
  -- Must be compatible with the "=" function in the sense that
  --  Image(A) /= Image(B) implies A /= B
  -- For efficiency A /= B should better imply that Image(A) /= Image(B)
  with function Key_Image (Element : Element_Type) return String;

  -- Hashing size and function
  Hash_Max : Hashing.Max_Hash_Range := Hashing.Def_Max_Hash_Range'Last;
  with function Hash_Func (Key : String) return Hashing.Max_Hash_Range
                is Hashing.Def_Max_Hash_Func;

package Hashed_List is

  -----------------
  -- HASHED_LIST --
  -----------------
  -- A hashed list is a storage of elements that are accessed through hashing
  -- Several elements with the same value (in the sense of "=") can be stored
  --  a retrieved successively.

  type List_Type is tagged limited private;

  -- For Iterator
  type Reference is (From_First, From_Last);

  -- Where inserting a new data
  type Where_Insert_List is new Hashing.Where_Insert_List;

  -- In which direction searching
  type Direction_List is new Hashing.Direction_List;


  -- Insert/Replace_Current/Delete_Current may raise In_Callback if performed
  --  in an application callback (Iteration);

  -- Check if an element matching Crit exists in the list
  procedure Search_First (List      : in out List_Type;
                          Crit      : in Element_Type;
                          Found     : out Boolean;
                          Direction : in Direction_List:= Forward);
  -- Check if another element matching Crit exists in the list
  procedure Search_Next (List      : in out List_Type;
                         Crit      : in Element_Type;
                         Found     : out Boolean;
                         Direction : in Direction_List:= Forward);

  -- Check if an element matching Crit exists in the list
  -- May raise Not_In_List
  procedure Find_First (List      : in out List_Type;
                        Crit      : in Element_Type;
                        Direction : in Direction_List := Forward);
  -- Check if another element matching Crit exists in the list
  -- May raise Not_In_List
  procedure Find_Next (List      : in out List_Type;
                       Crit      : in Element_Type;
                       Direction : in Direction_List := Forward);

  -- Insert an item
  -- May raise Full_List (no more memory)
  procedure Insert (List  : in out List_Type;
                    Item  : in Element_Type;
                    Where : in Where_Insert_List := Last);

  -- Read the last element searched/found
  -- May raise Not_In_List
  procedure Read_Current (List : in List_Type;
                          Item : out Element_Type);

  -- Get direct access to the last element searched/found
  -- Of course, changing the key of the accessed element will break the
  --  hash table integrity
  -- May raise Not_In_List
  procedure Get_Access_Current (List : in List_Type;
                                Item_Access : out Element_Access);
  function Get_Access_Current (List : List_Type) return Element_Access;

  -- Suppress the last element searched/found (which is reset)
  -- Note that this operation leads to a sequential scan of the list
  --   of elements
  -- May raise Not_In_List
  procedure Delete_Current (List : in out List_Type);

  -- Replace the last element searched/found
  -- May raise Not_Equal if Item is not "=" to the element searched/found
  -- May raise Not_In_List
  procedure Replace_Current (List : in out List_Type;
                             Item : in Element_Type);

  -- Delete the full list
  --  deallocate or not the free list
  procedure Delete_List (List : in out List_Type;
                         Deallocate : in Boolean := True);

  -- Return without exception
  function Is_Empty (List : List_Type) return Boolean;

  -- Return the number of elements in the list (0 if empty, no exception)
  function List_Length (List : List_Type) return Natural;

  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function Is_Modified (List : List_Type) return Boolean;

  procedure Modification_Ack (List : in out List_Type);


  -- Iterating and sequential reading are compatible with search and read
  --  but not with insertion, replacement and deletion

  -- Called with each matching element, which can be updated.
  -- Processing of Iterate can be stopped by resetting Go_On to False
  --  (it is initialised to True).
  type Iteration_Access is access procedure (Current : in Element_Type;
                                             Go_On   : in out Boolean);

  -- Execute Iteration on all items
  procedure Iterate (List      : in out List_Type;
                     Iteration : access
     procedure (Current : in Element_Type;
                Go_On   : in out Boolean);
                     From    : in Reference := From_First);

  -- Rewind internal list
  -- May raise Not_In_List if list is empty
  procedure Rewind (List : in out List_Type;
                    From : in Reference := From_First);
  -- Read successive items
  -- May raise Not_In_List if list is empty
  procedure Read_Next (List      : in out List_Type;
                       Item      : out Element_Type;
                       Moved     : out Boolean;
                       Direction : in Direction_List := Forward);

  ----------------
  -- EXCEPTIONS --
  ----------------
  -- When inserting
  Full_List : exception;

  -- When deleting, reading
  Not_In_List : exception;

  -- When replacing with a "different" element
  Not_Equal : exception;

  -- When modifying List in an application callback
  In_Callback : exception;

  -- If internal inconsistency (on delete)
  Internal_Error : exception;
private
  -- The limited list of items
  package List_Mng is new Limited_List (Element_Type, Set);

  -- Element hashing
  package Sized_Hash is new Hashing.Sized_Hash (Hash_Max);

  package Hash_Mng is new Sized_Hash.Hash_Mng
                  (Element_Access, Hash_Func);

  -- A unique list
  type List_Type is limited new Ada.Finalization.Limited_Controlled with record
    List : List_Mng.List_Type;
    Table : Hash_Mng.Hash_Table;
    Current : Element_Access := null;
    Hash_Index : Hash_Mng.Hash_Range := 0;
    In_Cb : Boolean := False;
  end record;
  overriding procedure Finalize (List : in out List_Type);

  -- For children
  procedure Locate (List      : in out List_Type;
                    Crit      : in Element_Type;
                    Reset     : in Boolean;
                    Element   : out Element_Access;
                    Direction : in Direction_List := Forward);
  procedure Check_Callback (List : in out List_Type);

end Hashed_List;

