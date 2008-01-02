-- Protected pool of objects accessed by key
with Ada.Finalization;
with Dynamic_List, Mutex_Manager;
generic

  type Element_Type is private;

package Protected_Pool is

  -- A pool of elements
  type Pool_Type is tagged limited private;

  -- The key to access an element
  type Key_Type is private;

  -- Store a new element in the pool, return the key to access it
  -- Raises Pool_Full if Positive'Last elements are already stored
  function Store (Pool : Pool_Type; Element : Element_Type) return Key_Type;

  -- Get/Read/Delete from the pool the element of key
  -- Raise Not_Found if no element with this key
  function Get (Pool : Pool_Type; Key : Key_Type) return Element_Type;
  function Read (Pool : Pool_Type; Key : Key_Type) return Element_Type;
  procedure Delete (Pool : Pool_Type; Key : Key_Type);

  -- Delete the whole pool
  procedure Delete_Pool (Pool : Pool_Type);

  Pool_Full : exception;
  Not_Found : exception;
private

  -- Access key
  type Key_Type is mod Positive'Last;

  -- Data stored in list
  type Cell_Type is record
    Key : Key_Type;
    Data : Element_Type;
  end record;

  package Elt_Dyn_List_Mng is new Dynamic_List (Cell_Type);
  package Elt_List_Mng renames Elt_Dyn_List_Mng.Dyn_List;
  type List_Access is access Elt_List_Mng.List_Type;
  
  type Pool_Type is limited new Ada.Finalization.Limited_Controlled with record
    Next_Key : Key_Type := Key_Type'First;
    Mutex : Mutex_Manager.Mutex (Mutex_Manager.Simple); 
    List : List_Access := new Elt_List_Mng.List_Type;
  end record;

  procedure Finalize (Pool : in out Pool_Type);

end Protected_Pool;

