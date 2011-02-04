-- Allow several object to have references to a common object and
--  automatically de-allocate the referenced object when last user releases
--  its reference
with Ada.Finalization;
generic
  type Object is limited private;
  with procedure Set (Dest : in out Object; Val : in Object);
  with procedure Finalize (Dest : in Object);
package Smart_Reference is

  type Handle is tagged limited private;

  -- Initialise a Handle to an object
  procedure Set (Reference : in out Handle; Init : in Object);

  -- Copy handle
  procedure Set (Dest : in out Handle; Val : in Handle);

  -- Release handle
  procedure Release (Reference : in out Handle);

  -- Get handled object
  -- Raises Constraint_Error is Reference is not set or released
  procedure Dereference (Reference : in out Handle; Val : in out Object);
  procedure Get (Reference : in out Handle; Val : in out Object)
            renames Dereference;

private
  type Object_Access is access Object;

  type Object_Box is record
    Obj : Object;
    Nb_Access : Natural := 0;
  end record;
  type Object_Box_Access is access Object_Box;

  type Handle is limited new Ada.Finalization.Limited_Controlled with record
    Box_Access : Object_Box_Access := null;
  end record;
  overriding procedure Initialize (Ref : in out Handle);
  overriding procedure Finalize (Ref : in out Handle);

end Smart_Reference;

