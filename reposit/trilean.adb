package body Trilean is

  function Tri2Boo (Val : Trilean) return Boolean is
  begin
    case Val is
      when False =>
        return Boolean'(False);
      when True =>
        return Boolean'(True);
      when Other =>
        raise Constraint_Error;
    end case;
  end Tri2Boo;

  function Boo2Tri (Val : Boolean) return Trilean is
  begin
    return (case Val is
              when Boolean'(False) => False,
              when Boolean'(True)  => True);
  end Boo2Tri;

end Trilean;

