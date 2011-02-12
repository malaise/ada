with Ada.Text_Io;
with Unchecked_Deallocation;
package body Smart_Reference is

  Debug : constant Boolean := True;
  procedure Trace (Str : in String) is
  begin
    if Debug then
      Ada.Text_Io.Put_Line(Str);
    end if;
  end Trace;

  procedure Free is new Unchecked_Deallocation (Object_Box,
                                                Object_Box_Access);

  -- Decrement reference to object and free object if no more reference
  procedure Decrement_Ref (Ref : in out Handle) is
  begin
    if Ref.Box_Access /= null then
      Ref.Box_Access.Nb_Access := Ref.Box_Access.Nb_Access - 1;
      if Ref.Box_Access.Nb_Access = 0 then
        Trace("Free");
        Finalize(Ref.Box_Access.Obj);
        Free(Ref.Box_Access);
      end if;
    end if;
  end Decrement_Ref;

  -- Increment reference to object
  procedure Increment_Ref (Ref : in Handle) is
  begin
    Ref.Box_Access.Nb_Access := Ref.Box_Access.Nb_Access + 1;
  end Increment_Ref;

  -- Init Nb_Access to 1
  overriding procedure Initialize (Ref : in out Handle) is
    pragma Unreferenced (Ref);
  begin
    Trace("Initialization");
  end Initialize;

  -- Increment Nb_Access
  overriding procedure Adjust (Ref : in out Handle) is
  begin
    Trace("Adjustment");
    Increment_Ref (Ref);
  end Adjust;

  -- Decrement Nb_Access and garbage collect when last
  overriding procedure Finalize (Ref : in out Handle) is
  begin
    Trace("Finalization");
    Decrement_Ref(Ref);
  end Finalize;

  -- Initialize handle
  procedure Set (Reference : in out Handle; Init : in Object) is
  begin
    Decrement_Ref(Reference);
    Trace("New");
    Reference.Box_Access := new Object_Box;
    Set (Reference.Box_Access.Obj, Init);
    Increment_Ref(Reference);
  end Set;

  -- Release handle
  procedure Release (Reference : in out Handle) is
  begin
    Finalize (Reference);
    Reference.Box_Access := null;
  end Release;

  -- Get a copy of referenced object
  procedure Dereference (Reference : in Handle; Val : in out Object) is
  begin
    Set (Val, Reference.Box_Access.Obj);
  end Dereference;

  -- Is a Handle set
  function Is_Set (Reference : Handle) return Boolean is
  begin
    return Reference.Box_Access /= null;
  end Is_Set;

end Smart_Reference;

