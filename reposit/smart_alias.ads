-- Allow several object to have references to a common object and
--  automatically inform when last user releases its reference
with Ada.Finalization;
generic
  type Object is limited private;
  with procedure Finalize (Dest : in Object) is null;
package Smart_Alias is

  -- The handle that points to an Object
  type Handle is tagged private;
  Null_Handle : constant Handle;

  -- Initialise a Handle to an object
  procedure Init (Reference : in out Handle; Val : aliased in Object);
  function Init (Val : aliased Object) return Handle;

  -- Release handle (which becomes null)
  procedure Release (Reference : in out Handle);

  -- Get a direct access to a handled object
  -- Raise Constraint_Error if Reference is not set or released
  -- CARE: Don't use an access outside the scope of the Handle
  --  that was used to get the access
  function Get_Access (Reference : Handle) return access Object;

  -- Is a Handle set
  function Is_Set (Reference : Handle) return Boolean;

private
  type Object_Access is access all Object;

  type Handle is new Ada.Finalization.Controlled with record
    Obj_Access : Object_Access := null;
    Nb_Access : Natural := 0;
  end record;
  overriding procedure Initialize (Ref : in out Handle);
  overriding procedure Adjust     (Ref : in out Handle);
  overriding procedure Finalize   (Ref : in out Handle);
  Null_Handle : constant Handle := (Ada.Finalization.Controlled
                                    with others => <>);

end Smart_Alias;
