package Bit_Ops is

  function "AND" (Left, Right : Integer) return Integer;
  function "OR"  (Left, Right : Integer) return Integer;
  function "XOR" (Left, Right : Integer) return Integer;
  function "NOT" (Val : Integer) return Integer;
  function Shl (Val : Integer; Bits : Integer) return Integer;
  function Shr (Val : Integer; Bits : Integer) return Integer;

  function "AND" (Left, Right : Long_Long_Integer) return Long_Long_Integer;
  function "OR"  (Left, Right : Long_Long_Integer) return Long_Long_Integer;
  function "XOR" (Left, Right : Long_Long_Integer) return Long_Long_Integer;
  function "NOT" (Val : Long_Long_Integer) return Long_Long_Integer;
  function Shl (Val : Long_Long_Integer; Bits : Integer) return Long_Long_Integer;
  function Shr (Val : Long_Long_Integer; Bits : Integer) return Long_Long_Integer;

end Bit_Ops;
