-- Protected pool of objects, each accessed by a unique key
-- Access and modification of the Pool are protected by a mutex
with Long_Longs, Long_Long_Limited_List, Mutex_Manager;
generic

  type Element_Type is private;

package Protected_Pool is

  -- A pool of elements
  type Pool_Type is tagged limited private;

  -- The key to access an element
  type Key_Type is private;

  -- Conversion from/to string
  -- Key_Value may raise Constraint_Error
  function Key_Image (Key : Key_Type) return String;
  function Key_Value (Str : String) return Key_Type;


  -- Store a new element in the pool, return the key to access it
  -- Raises Pool_Full if Positive'Last elements are already stored
  function Store (Pool : in out Pool_Type;
                  Element : in Element_Type) return Key_Type;

  -- Get/Read/Delete from the pool the element of key
  -- Raise Not_Found if no element with this key
  function Get (Pool : in out Pool_Type; Key : Key_Type) return Element_Type;
  function Read (Pool : in out Pool_Type; Key : Key_Type) return Element_Type;
  procedure Delete (Pool : in out Pool_Type; Key : in Key_Type);

  -- Delete the whole pool
  procedure Delete_Pool (Pool : in out Pool_Type);

  Pool_Full : exception;
  Not_Found : exception;
private

  -- Access key, cannot be mod because > 2**32
  type Key_Type is new Long_Longs.Ll_Natural;
  type Key_Access is access Key_Type;

  -- Data stored in list
  type Cell_Type is record
    Key : Key_Type;
    Data : Element_Type;
  end record;
  procedure Set (To : out Cell_Type; Val : in Cell_Type);

  package Elt_List_Mng is new Long_Long_Limited_List (Cell_Type, Set);
  type Pool_Type is tagged limited record
    Next_Key : Key_Type := Key_Type'First;
    Mutex : Mutex_Manager.Simple_Mutex;
    List : Elt_List_Mng.List_Type;
  end record;

end Protected_Pool;

