with Ada.Finalization;
generic
  type Object is limited private;
  with procedure Set (Dest : in out Object; Val : in Object);
  with procedure Finalize (Dest : in Object);
package Smart_Reference is

  type Handle is limited private;

  -- Initialise a Handle to an object 
  procedure Set (Reference : in out Handle; Init : in Object);

  -- Copy handle
  procedure Set (Dest : in out Handle; Val : in Handle);

  -- Release handle
  procedure Release (Reference : in out Handle);

  -- Get handled object
  function  Dereference (Reference : Handle) return Object;
  procedure Dereference (Reference : in out Handle; Val : in out Object);

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
  procedure Initialize (Ref : in out Handle);
  procedure Finalize (Ref : in out Handle);

end Smart_Reference;

