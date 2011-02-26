with As.U, My_Math;
package Any_Def is

  type Any_Kind_List is (None_Kind, Bool_Kind, Inte_Kind, Real_Kind,
                         Str_Kind);

  type Any (Kind : Any_Kind_List := None_Kind) is record
    case Kind is
      when None_Kind => null;
      when Bool_Kind => Bool : Boolean;
      when Inte_Kind => Inte : My_Math.Inte;
      when Real_Kind => Real : My_Math.Real;
      when Str_Kind  => Str  : As.U.Asu_Us;
    end case;
  end record;

  function Image (A : Any) return String;

end Any_Def;

