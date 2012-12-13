with Unchecked_Conversion;

-- Suppress warning than System.Bit_Ops is an internal GNAT unit
--  thus not portable
pragma Warnings (Off, "* is an internal GNAT unit");
with System.Bit_Ops;
pragma Warnings (On,  "* is an internal GNAT unit");

with Interfaces;

package body Bit_Ops is

  function To_Unsigned_32 is new Unchecked_Conversion
    (Source => Integer, Target => Interfaces.Unsigned_32);
  function To_Integer is new Unchecked_Conversion
    (Source => Interfaces.Unsigned_32, Target => Integer);

  function To_Unsigned_64 is new Unchecked_Conversion
    (Source => Long_Integer, Target => Interfaces.Unsigned_64);
  function To_Long_Integer is new Unchecked_Conversion
    (Source => Interfaces.Unsigned_64, Target => Long_Integer);

  function To_Unsigned_64 is new Unchecked_Conversion
    (Source => Long_Long_Integer, Target => Interfaces.Unsigned_64);
  function To_Long_Long_Integer is new Unchecked_Conversion
    (Source => Interfaces.Unsigned_64, Target => Long_Long_Integer);

  -- Integer
  function "And" (Left, Right : Integer) return Integer is
    Res : Integer;
  begin
    System.Bit_Ops.Bit_And (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "And";

  function "Or"  (Left, Right : Integer) return Integer is
    Res : Integer;
  begin
    System.Bit_Ops.Bit_Or (Left'Address, Left'Size,
                           Right'Address, Right'Size,
                           Res'Address);
    return Res;
  end "Or";

  function "Xor" (Left, Right : Integer) return Integer is
    Res : Integer;
  begin
    System.Bit_Ops.Bit_Xor (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "Xor";

  function "Not" (Val : Integer) return Integer is
    Res : Integer;
  begin
    System.Bit_Ops.Bit_Not (Val'Address, Val'Size,
                            Res'Address);
    return Res;
  end "Not";

  function Shl (Val : Integer; Bits : Integer) return Integer is
  begin
    return To_Integer(Interfaces.Shift_Left(To_Unsigned_32(Val), Bits));
  end Shl;

  function Shr (Val : Integer; Bits : Integer) return Integer is
  begin
    return To_Integer(Interfaces.Shift_Right(To_Unsigned_32(Val), Bits));
  end Shr;

  -- Long_Integer
  function "And" (Left, Right : Long_Integer) return Long_Integer is
    Res : Long_Integer;
  begin
    System.Bit_Ops.Bit_And (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "And";

  function "Or"  (Left, Right : Long_Integer) return Long_Integer is
    Res : Long_Integer;
  begin
    System.Bit_Ops.Bit_Or (Left'Address, Left'Size,
                           Right'Address, Right'Size,
                           Res'Address);
    return Res;
  end "Or";

  function "Xor" (Left, Right : Long_Integer) return Long_Integer is
    Res : Long_Integer;
  begin
    System.Bit_Ops.Bit_Xor (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "Xor";

  function "Not" (Val : Long_Integer) return Long_Integer is
    Res : Long_Integer;
  begin
    System.Bit_Ops.Bit_Not (Val'Address, Val'Size,
                            Res'Address);
    return Res;
  end "Not";

  function Shl (Val : Long_Integer; Bits : Integer) return Long_Integer is
  begin
    return To_Long_Integer(
      Interfaces.Shift_Left(To_Unsigned_64(Val), Bits));
  end Shl;

  function Shr (Val : Long_Integer; Bits : Integer) return Long_Integer is
  begin
    return To_Long_Integer(
      Interfaces.Shift_Right(To_Unsigned_64(Val), Bits));
  end Shr;

  -- Long_Long_Integer
  function "And" (Left, Right : Long_Long_Integer) return Long_Long_Integer is
    Res : Long_Long_Integer;
  begin
    System.Bit_Ops.Bit_And (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "And";

  function "Or"  (Left, Right : Long_Long_Integer) return Long_Long_Integer is
    Res : Long_Long_Integer;
  begin
    System.Bit_Ops.Bit_Or (Left'Address, Left'Size,
                           Right'Address, Right'Size,
                           Res'Address);
    return Res;
  end "Or";

  function "Xor" (Left, Right : Long_Long_Integer) return Long_Long_Integer is
    Res : Long_Long_Integer;
  begin
    System.Bit_Ops.Bit_Xor (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "Xor";

  function "Not" (Val : Long_Long_Integer) return Long_Long_Integer is
    Res : Long_Long_Integer;
  begin
    System.Bit_Ops.Bit_Not (Val'Address, Val'Size,
                            Res'Address);
    return Res;
  end "Not";

  function Shl (Val : Long_Long_Integer; Bits : Integer)
               return Long_Long_Integer is
  begin
    return To_Long_Long_Integer(
      Interfaces.Shift_Left(To_Unsigned_64(Val), Bits));
  end Shl;

  function Shr (Val : Long_Long_Integer; Bits : Integer)
               return Long_Long_Integer is
  begin
    return To_Long_Long_Integer(
      Interfaces.Shift_Right(To_Unsigned_64(Val), Bits));
  end Shr;

end Bit_Ops;

