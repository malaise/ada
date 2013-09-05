with Ada.Unchecked_Deallocation;
with Trace, Address_Ops;
package body Smart_Reference is

  procedure Free is new Ada.Unchecked_Deallocation (Object_Box,
                                                    Object_Box_Access);

  Logger : Trace.Logger;
  procedure Trace (Ref : in Handle; Str : in String) is
  begin
    Logger.Init ("Smart_Reference");
    if Logger.Debug_On then
      Logger.Log_Debug (Str &
          (if Ref.Box_Access /= null then
            " of " & Address_Ops.Image (Ref.Box_Access.all'Address)
           else ""));
    end if;
  end Trace;

  -- Decrement reference to object and free object if no more reference
  procedure Decrement_Ref (Ref : in out Handle) is
  begin
    if Ref.Box_Access /= null then
      Ref.Box_Access.Nb_Access := Ref.Box_Access.Nb_Access - 1;
      Trace (Ref, "Decr ->" & Ref.Box_Access.Nb_Access'Img);
      if Ref.Box_Access.Nb_Access = 0 then
        Trace (Ref, "Free");
        Finalize (Ref.Box_Access.Obj);
        Free (Ref.Box_Access);
      end if;
    end if;
  end Decrement_Ref;

  -- Increment reference to object
  procedure Increment_Ref (Ref : in Handle) is
  begin
    if Ref.Box_Access /= null then
      Ref.Box_Access.Nb_Access := Ref.Box_Access.Nb_Access + 1;
      Trace (Ref, "Incr ->" & Ref.Box_Access.Nb_Access'Img);
    end if;
  end Increment_Ref;

  -- Init Nb_Access to 1
  overriding procedure Initialize (Ref : in out Handle) is
  begin
    Trace (Ref, "Initialization");
  end Initialize;

  -- Increment Nb_Access
  overriding procedure Adjust (Ref : in out Handle) is
  begin
    Trace (Ref, "Adjustment");
    Increment_Ref (Ref);
  end Adjust;

  -- Decrement Nb_Access and garbage collect when last
  overriding procedure Finalize (Ref : in out Handle) is
  begin
    Trace (Ref, "Finalization");
    Decrement_Ref(Ref);
  end Finalize;

  -- Initialize handle
  procedure Init (Reference : in out Handle; Val : in Object) is
  begin
    Decrement_Ref (Reference);
    Reference.Box_Access := new Object_Box;
    Trace (Reference, "New");
    Set (Reference.Box_Access.Obj, Val);
    Increment_Ref (Reference);
  end Init;
  function Init (Val : Object) return Handle is
  begin
    return Reference : Handle do
      Reference.Box_Access := new Object_Box;
      Trace (Reference, "New");
      Set (Reference.Box_Access.Obj, Val);
      Increment_Ref (Reference);
    end return;
  end Init;

  -- Release handle
  procedure Release (Reference : in out Handle) is
  begin
    Finalize (Reference);
    Reference.Box_Access := null;
  end Release;

  -- Set handled object to new value
  procedure Set (Reference : in Handle; Val : in Object) is
  begin
    Set (Reference.Box_Access.Obj, Val);
  end Set;

  -- Get a copy of referenced object
  procedure Get (Reference : in Handle; Val : out Object) is
  begin
    Set (Val, Reference.Box_Access.Obj);
  end Get;
  function Get (Reference : Handle) return Object is
  begin
    return Val : Object do
      Get (Reference, Val);
    end return;
  end Get;

  -- Get a direct access to a handled object
  -- Raise Constraint_Error if Reference is not set or released
  -- CARE: Don't use an access outside the scope of the Handle
  --  that was used to get the access
  function Get_Access (Reference : Handle) return access Object is
  begin
    return Reference.Box_Access.Obj'Unrestricted_Access;
  end Get_Access;

  -- Is a Handle set
  function Is_Set (Reference : Handle) return Boolean is
  begin
    return Reference.Box_Access /= null;
  end Is_Set;

end Smart_Reference;

