-- Allow several object to have references to a common object and
--  automatically de-allocate the referenced object when last user releases
--  its reference
with Ada.Finalization;
generic
  type Object is limited private;
  with procedure Set (Dest : in out Object; Val : in Object);
  with procedure Finalize (Dest : in Object) is null;
package Smart_Reference is

  type Handle is tagged private;

  -- Initialise a Handle to an object
  procedure Set (Reference : in out Handle; Init : in Object);

  -- Release handle (which becomes null)
  procedure Release (Reference : in out Handle);

  -- Get handled object
  -- Raises Constraint_Error is Reference is not set or released
  procedure Dereference (Reference : in Handle; Val : in out Object);
  procedure Get (Reference : in Handle; Val : in out Object)
            renames Dereference;

  -- Is a Handle set
  function Is_Set (Reference : Handle) return Boolean;

private
  type Object_Access is access Object;

  type Object_Box is record
    Obj : Object;
    Nb_Access : Natural := 0;
  end record;
  type Object_Box_Access is access Object_Box;

  type Handle is new Ada.Finalization.Controlled with record
    Box_Access : Object_Box_Access := null;
  end record;
  overriding procedure Initialize (Ref : in out Handle);
  overriding procedure Adjust     (Ref : in out Handle);
  overriding procedure Finalize   (Ref : in out Handle);

end Smart_Reference;

