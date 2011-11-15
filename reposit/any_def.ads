with As.U, My_Math;
package Any_Def is

  -- An "any" is a multi-type container
  -- An "any" can be empty, a boolean, integer, real or string
  type Any_Kind_List is (None_Kind, Bool_Kind, Inte_Kind, Real_Kind, Str_Kind);

  -- Content of an "any"
  type Any (Kind : Any_Kind_List := None_Kind) is record
    case Kind is
      when None_Kind => null;
      when Bool_Kind => Bool : Boolean;
      when Inte_Kind => Inte : My_Math.Inte;
      when Real_Kind => Real : My_Math.Real;
      when Str_Kind  => Str  : As.U.Asu_Us;
    end case;
  end record;

  -- String representation of an "any"
  function Image (A : Any) return String;

end Any_Def;

