with System;
with As.U, My_Math;
package Any_Def is

  -- An "any" is a multi-type container
  -- An "any" can be empty, a boolean, integer, real, string, address
  --  or access to an "any"
  type Any_Kind_List is (None_Kind,
                         Bool_Kind,
                         Inte_Kind, Integer_Kind,
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
      when Integer_Kind => Int  : Integer;
      when Inte_Kind    => Inte : My_Math.Inte;
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

