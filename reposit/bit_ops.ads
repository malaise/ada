-- Bit operations
with Long_Longs, C_Types;
package Bit_Ops is

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

  -- Operations on long integers
  function "And" (Left, Right : Long_Integer) return Long_Integer
    with Inline => True;
  function "Or"  (Left, Right : Long_Integer) return Long_Integer
    with Inline => True;
  function "Xor" (Left, Right : Long_Integer) return Long_Integer
    with Inline => True;
  function "Not" (Val : Long_Integer) return Long_Integer
    with Inline => True;
  function Shl (Val : Long_Integer; Bits : Integer) return Long_Integer
    with Inline => True;
  function Shr (Val : Long_Integer; Bits : Integer) return Long_Integer
    with Inline => True;

  -- Operations on long long integers
  use Long_Longs;
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

  -- Operations on Uint32
  use C_Types;
  function Shl (Val : Uint32; Bits : Integer) return Uint32
    with Inline => True;
  function Shr (Val : Uint32; Bits : Integer) return Uint32
    with Inline => True;

  -- Operations on Uint64
  function Shl (Val : Uint64; Bits : Integer) return Uint64
    with Inline => True;
  function Shr (Val : Uint64; Bits : Integer) return Uint64
    with Inline => True;

end Bit_Ops;

