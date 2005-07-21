package body T_Smart_Int is
  procedure Set (Dest : in out Integer; Val : in Integer) is
  begin
    Dest := Val;
  end Set;

  procedure Fin (Val : in Integer) is
  begin
    null;
  end Fin;
end T_Smart_Int;

