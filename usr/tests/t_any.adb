with System;
with Any_Def, Trilean, As.U, Address_Ops, Basic_Proc, Mixed_Str, Arbitrary;
use Any_Def;
procedure T_Any is
  A1, A2 : Any_Def.Any;
  Addr : System.Address;
begin

  -- Prepare some specific valuies
  A2 := (Str_Kind, As.U.Tus ("Another string"));
  Addr := Address_Ops."+" (System.Null_Address, 212121);

  -- Test each kind of Any: Set a kind+value and put image
  for I in Any_Def.Any_Kind_List loop
    case I is
      when None_Kind      => A1 := (Kind => None_Kind);
      when Bool_Kind      => A1 := (Bool_Kind, True);
      when Trilean_Kind   => A1 := (Trilean_Kind, Trilean.Other);
      when Integer_Kind   => A1 := (Integer_Kind, 21);
      when Lint_Kind      => A1 := (Lint_Kind, 2121);
      when Arbitrary_Kind => A1 := (Arbitrary_Kind, Arbitrary.Set ("212121"));
      when Duration_Kind  => A1 := (Duration_Kind, 21.21);
      when Float_Kind     => A1 := (Float_Kind, 21.21);
      when Real_Kind      => A1 := (Real_Kind, 2121.2121);
      when Str_Kind       => A1 := (Str_Kind, As.U.Tus ("A string"));
      when Address_Kind   => A1 := (Address_Kind, Addr);
      when Access_Kind    => A1 := (Access_Kind, A2'Unrestricted_Access);
    end case;
    Basic_Proc.Put_Line_Output (Mixed_Str (I'Img) & ": "
                              & Any_Def.Image (A1));

  end loop;

end T_Any;

