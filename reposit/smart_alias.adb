with Trace, Address_Ops;
package body Smart_Alias is

  Logger : Trace.Logger;
  procedure Trace (Ref : in Handle; Str : in String) is
  begin
    Logger.Init ("Smart_Alias");
    if Logger.Debug_On and then Ref.Obj_Access /= null then
      Logger.Log_Debug ("Smart_Alias: " & Str
         & " of " & Address_Ops.Image (Ref.Obj_Access.all'Address));
    end if;
  end Trace;

  -- Decrement reference to object and free object if no more reference
  procedure Decrement_Ref (Ref : in out Handle) is
  begin
    if Ref.Obj_Access /= null then
      Ref.Nb_Access := Ref.Nb_Access - 1;
      Trace (Ref, "Decr ->" & Ref.Nb_Access'Img);
      if Ref.Nb_Access = 0 then
        Trace (Ref, "Free");
        Finalize (Ref.Obj_Access);
      end if;
    end if;
  end Decrement_Ref;

  -- Increment reference to object
  procedure Increment_Ref (Ref : in out Handle) is
  begin
    if Ref.Obj_Access /= null then
      Ref.Nb_Access := Ref.Nb_Access + 1;
      Trace (Ref, "Incr ->" & Ref.Nb_Access'Img);
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
    Decrement_Ref(Ref);
  end Finalize;

  -- Initialize handle
  procedure Init (Reference : in out Handle; Val : access Object) is
  begin
    Decrement_Ref (Reference);
    Reference.Obj_Access := Object_Access(Val);
    Trace (Reference, "New");
    Increment_Ref (Reference);
  end Init;
  function Init (Val : access Object) return Handle is
  begin
    return Reference : Handle do
      Reference.Obj_Access := Object_Access(Val);
      Trace (Reference, "New");
      Increment_Ref (Reference);
    end return;
  end Init;

  -- Release handle
  procedure Release (Reference : in out Handle) is
  begin
    Reference.Obj_Access := null;
  end Release;

  -- Get a direct access to a handled object
  -- Raise Constraint_Error if Reference is not set or released
  -- CARE: Don't use an access outside the scope of the Handle
  --  that was used to get the access
  function Get_Access (Reference : Handle) return access Object is
  begin
    return Reference.Obj_Access;
  end Get_Access;

  -- Is a Handle set
  function Is_Set (Reference : Handle) return Boolean is
  begin
    return Reference.Obj_Access /= null;
  end Is_Set;

end Smart_Alias;

