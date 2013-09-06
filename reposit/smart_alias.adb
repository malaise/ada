with Ada.Unchecked_Deallocation;
with Trace, Address_Ops;
package body Smart_Alias is

  procedure Free is new Ada.Unchecked_Deallocation (Object_Box,
                                                    Object_Box_Access);

  package Logger is new Trace.Basic_Logger ("Smart_Alias");

  procedure Trace (Ref : in Handle; Str : in String) is
  begin
    if Logger.Is_On (Standard.Trace.Debug) then
      Logger.Log (Standard.Trace.Debug,
          Str &
          (if Ref.Box_Access /= null then
            " of " & Address_Ops.Image (Ref.Box_Access.all'Address)
           else ""));
    end if;
  end Trace;

  -- Decrement reference to object and free object if no more reference
  procedure Decrement_Ref (Ref : in out Handle) is
    Free_Ref : Boolean;
  begin
    if Ref.Box_Access /= null then
      Ref.Box_Access.Nb_Access := Ref.Box_Access.Nb_Access - 1;
      Trace (Ref, "Decr ->" & Ref.Box_Access.Nb_Access'Img);
      -- Released may call ourself, which may already free Ref
      -- The free must be done if Nb_Access is 0 before calling Released,
      --  not after
      Free_Ref := Ref.Box_Access.Nb_Access = 0;
      Released (Ref.Box_Access.Obj, Ref.Box_Access.Nb_Access);
      if Free_Ref then
        Trace (Ref, "Free");
        Free (Ref.Box_Access);
      end if;
    end if;
  end Decrement_Ref;

  -- Increment reference to object
  procedure Increment_Ref (Ref : in out Handle) is
  begin
    if Ref.Box_Access /= null then
      Ref.Box_Access.Nb_Access := Ref.Box_Access.Nb_Access + 1;
      Trace (Ref, "Incr ->" & Ref.Box_Access.Nb_Access'Img);
    end if;
  end Increment_Ref;

  -- Init
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
    if Ref.Box_Access /= null then
      Decrement_Ref(Ref);
    end if;
  end Finalize;

  -- Initialize handle
  procedure Init (Reference : in out Handle; Val : access Object) is
  begin
    Decrement_Ref (Reference);
    Reference.Box_Access := new Object_Box;
    Trace (Reference, "New");
    Reference.Box_Access.Obj := Object_Access(Val);
    Increment_Ref (Reference);
  end Init;
  function Init (Val : access Object) return Handle is
  begin
    return Reference : Handle do
      Reference.Box_Access := new Object_Box;
      Trace (Reference, "New");
      Reference.Box_Access.Obj := Object_Access(Val);
      Increment_Ref (Reference);
    end return;
  end Init;

  -- Release handle
  procedure Release (Reference : in out Handle) is
  begin
    Finalize (Reference);
    Reference.Box_Access := null;
  end Release;

  -- Get a direct access to a handled object
  -- Raise Constraint_Error if Reference is not set or released
  -- CARE: Don't use an access outside the scope of the Handle
  --  that was used to get the access
  function Get_Access (Reference : Handle) return access Object is
  begin
    return Reference.Box_Access.Obj;
  end Get_Access;

  -- Is a Handle set
  function Is_Set (Reference : Handle) return Boolean is
  begin
    return Reference.Box_Access /= null;
  end Is_Set;

end Smart_Alias;

