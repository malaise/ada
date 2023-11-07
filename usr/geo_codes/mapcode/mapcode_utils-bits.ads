-- Bit operations
package Mapcode_Utils.Bits is

  -- Bit and, bit or, bit neg, shift left and shift right

  -- Operations on integers
  function "And" (Left, Right : Integer) return Integer with Inline => True;
  function "Or"  (Left, Right : Integer) return Integer with Inline => True;
  function "Xor" (Left, Right : Integer) return Integer with Inline => True;
  function "Not" (Val : Integer) return Integer with Inline => True;
  function Shl (Val : Integer; Bits : Integer) return Integer
    with Inline => True;
  function Shr (Val : Integer; Bits : Integer) return Integer
    with Inline => True;

  -- Operations on long long integers
  subtype Ll_Integer is Long_Long_Integer;
  function "And" (Left, Right : Ll_Integer) return Ll_Integer
    with Inline => True;
  function "Or"  (Left, Right : Ll_Integer) return Ll_Integer
    with Inline => True;
  function "Xor" (Left, Right : Ll_Integer) return Ll_Integer
    with Inline => True;
  function "Not" (Val : Ll_Integer) return Long_Long_Integer
    with Inline => True;
  function Shl (Val : Ll_Integer; Bits : Integer) return Ll_Integer
    with Inline => True;
  function Shr (Val : Ll_Integer; Bits : Integer) return Ll_Integer
    with Inline => True;

end Mapcode_Utils.Bits;

