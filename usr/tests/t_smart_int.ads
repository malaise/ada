-- For T_Smart_Alias and T_Smart_Ree, a limited type (based on integer)
package T_Smart_Int is
  type Lim is limited private;

  procedure Set (Dest : out Lim; Val : in Lim);
  procedure Fin (Val : in out Lim);

  procedure Init (Dest : out Lim; Val : in Integer);
  function Image (Val : Lim) return String;
private
  type Lim is new Integer;
end T_Smart_Int;

