package body Trilean is

  function Tri2Boo (Tri : Trilean) return Boolean is
  begin
    case Tri is
      when False =>
        return Boolean'(False);
      when True =>
        return Boolean'(True);
      when Other =>
        raise Constraint_Error;
    end case;
  end Tri2Boo;

  function Boo2Tri (Boo : Boolean) return Trilean is
  begin
    return (case Boo is
              when Boolean'(False) => False,
              when Boolean'(True)  => True);
  end Boo2Tri;

end Trilean;

