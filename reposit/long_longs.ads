-- Long long integers
package Long_Longs is

  -- Signed Long_Long
  subtype Ll_Integer  is Long_Long_Integer;
  subtype Ll_Natural  is Ll_Integer range 0 .. Ll_Integer'Last;
  subtype Ll_Positive is Ll_Integer range 1 .. Ll_Integer'Last;
  function Image (L : Ll_Integer) return String;

  -- Unsigned Long_Long
  type Llu_Natural is mod 2 ** 64
    with Size => Ll_Integer'Size;
  subtype Llu_Positive is Llu_Natural range 1 .. Llu_Natural'Last;
  function Image (U : Llu_Natural) return String;

end Long_Longs;

