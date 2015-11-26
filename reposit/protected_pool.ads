-- Protected pool of objects, each accessed by a unique key
-- Access and modification of the Pool are protected by a mutex
with Hashed_List.Unique, Mutexes;
generic

  type Element_Type is private;
  with function "=" (Current : Element_Type; Criteria : Element_Type)
                return Boolean;
  with function Key_Image (Element : Element_Type) return String;

package Protected_Pool is

  -- A pool of elements
  type Pool_Type is tagged limited private;

  -- Store a new element in the pool
  -- Raises Pool_Full if Positive'Last elements are already stored
  procedure Store (Pool : in out Pool_Type;
                   Element : in Element_Type);

  -- Get/Read/Delete from the pool the element of key
  -- Raise Not_Found if no element with this key
  procedure Get (Pool : in out Pool_Type; Elt : in out Element_Type);
  procedure Read (Pool : in out Pool_Type; Elt : in out Element_Type);
  procedure Delete (Pool : in out Pool_Type; Elt : in Element_Type);

  -- Delete the whole pool
  procedure Delete_Pool (Pool : in out Pool_Type);

  Pool_Full : exception;
  Not_Found : exception;
private

  type Element_Access is access all Element_Type;
  procedure Set (To : out Element_Type; Val : in Element_Type);


  package Elt_List_Mng is new Hashed_List (Element_Type, Element_Access,
                                           Set, "=", Key_Image);
  package Elt_Uniq_Mng is new Elt_List_Mng.Unique;
  type Pool_Type is tagged limited record
    Mutex : Mutexes.Simple_Mutex;
    List : Elt_Uniq_Mng.Unique_List_Type;
  end record;

end Protected_Pool;

