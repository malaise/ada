with Mixed_Str;
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

  function Tri2Boo (Val : Trilean; Other_As : Boolean) return Boolean is
  begin
    case Val is
      when False =>
        return Boolean'(False);
      when True =>
        return Boolean'(True);
      when Other =>
        return Other_As;
    end case;
  end Tri2Boo;


  function Boo2Tri (Val : Boolean) return Trilean is
  begin
    return (case Val is
              when Boolean'(False) => False,
              when Boolean'(True)  => True);
  end Boo2Tri;

  function Image (Val : Trilean) return String is
  begin
    return Mixed_Str (Val'Img);
  end Image;

end Trilean;

