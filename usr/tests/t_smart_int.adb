with Basic_Proc;
package body T_Smart_Int is
  procedure Set (Dest : in out Lim; Val : in Lim) is
  begin
    Dest := Val;
  end Set;

  procedure Fin (Val : in Lim) is
  begin
    Basic_Proc.Put_Line_Output ("Finalization of" & Val'Img);
  end Fin;

  procedure Init (Dest : in out Lim; Val : in Integer) is
  begin
    Dest := Lim(Val);
  end Init;
  function Image (Val : Lim) return String is
  begin
    return Val'Img;
  end Image;
end T_Smart_Int;

