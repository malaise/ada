with System;
with Trilean, Long_Longs, My_Math, As.U;
package Any_Def is

  -- An "any" is a multi-type container
  -- An "any" can be empty, a boolean, trilean, integer, real, string, address
  --  or access to an "any"
  type Any_Kind_List is (None_Kind,
                         Bool_Kind, Trilean_Kind,
                         Lint_Kind, Integer_Kind,
                         Real_Kind, Float_Kind,
                         Str_Kind,
                         Address_Kind,
                         Access_Kind);

  type Any (Kind : Any_Kind_List := None_Kind);
  type Any_Access is access all Any;
  -- Content of an "any"
  type Any (Kind : Any_Kind_List := None_Kind) is record
    case Kind is
      when None_Kind    => null;
      when Bool_Kind    => Bool : Boolean;
      when Trilean_Kind => Tril : Trilean.Trilean;
      when Integer_Kind => Int  : Integer;
      when Lint_Kind    => Lint : Long_Longs.Ll_Integer;
      when Real_Kind    => Real : My_Math.Real;
      when Float_Kind   => Flo  : Float;
      when Str_Kind     => Str  : As.U.Asu_Us;
      when Address_Kind => Addr : System.Address;
      when Access_Kind  => Acc  : Any_Access;
    end case;
  end record;

  -- String representation of an "any"
  function Image (A : Any) return String;

end Any_Def;

