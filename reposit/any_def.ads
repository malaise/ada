with As.U; use As.U;
with My_Math;
package Any_Def is

  type Any_Kind_List is (Empty_Kind, Bool_Kind, Inte_Kind, Real_Kind,
                         Str_Kind);

  type Any (Kind : Any_Kind_List := Empty_Kind) is record
    case Kind is
      when Empty_Kind => null;
      when Bool_Kind  => Bool : Boolean;
      when Inte_Kind  => Inte : My_Math.Inte;
      when Real_Kind  => Real : My_Math.Real;
      when Str_Kind   => Str  : Asu_Us;
    end case;
  end Record;

end Any_Def;

