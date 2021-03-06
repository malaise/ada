-- Allow several objects to have references to a common object, and
--  automatically de-allocate the referenced object when last user releases
--  its reference
private with Ada.Finalization;
generic
  -- Object shall NOT implement Ada.Finalization.(Limited_)Controlled
  type Object is limited private;
  with procedure Set (Dest : out Object; Val : in Object);
  with procedure Finalize (Dest : in out Object) is null;
package Smart_Reference is

  -- The handle that points to an Object
  type Handle is tagged private;
  Null_Handle : constant Handle;

  -- Initialise a Handle to an object
  procedure Init (Reference : in out Handle; Val : in Object);
  function Init (Val : Object) return Handle;

  -- Release handle (which becomes null)
  procedure Release (Reference : in out Handle);

  -- Set handled object to new value
  -- Raise Constraint_Error if Reference is not set or released
  procedure Set (Reference : in Handle; Val : in Object);

  -- Get a copy of handled object
  -- Raise Constraint_Error if Reference is not set or released
  procedure Get (Reference : in Handle; Val : out Object);
  function Get (Reference : Handle) return Object;

  -- Get a direct access to a handled object
  -- Raise Constraint_Error if Reference is not set or released
  -- CARE: Don't use an access outside the scope of the Handle
  --  that was used to get the access
  function Get_Access (Reference : Handle) return not null access Object;

  -- Is a Handle set
  function Is_Set (Reference : Handle) return Boolean;

private

  type Object_Box;
  type Object_Box_Access is access Object_Box;

  type Handle is new Ada.Finalization.Controlled with record
    Box_Access : Object_Box_Access := null;
  end record;
  overriding procedure Initialize (Ref : in out Handle);
  overriding procedure Adjust     (Ref : in out Handle);
  overriding procedure Finalize   (Ref : in out Handle);
  Null_Handle : constant Handle := (Ada.Finalization.Controlled
                                    with others => <>);

end Smart_Reference;

