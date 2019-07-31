with Ada.Unchecked_Conversion;

-- Suppress warning than System.Bit_Ops is an internal GNAT unit
--  thus not portable
pragma Warnings (Off, "* is an internal GNAT unit");
with System.Bit_Ops;
pragma Warnings (On,  "* is an internal GNAT unit");

with Interfaces;

package body Bit_Ops is

  function To_Unsigned_32 is new Ada.Unchecked_Conversion
    (Source => Integer, Target => Interfaces.Unsigned_32);
  function To_Integer is new Ada.Unchecked_Conversion
    (Source => Interfaces.Unsigned_32, Target => Integer);

  function Shl_Long (L : C_Types.Long; Bits : C_Types.Int) return C_Types.Long
    with Import => True, Convention => C, External_Name => "shl_long";
  function Shr_Long (L : C_Types.Long; Bits : C_Types.Int) return C_Types.Long
    with Import => True, Convention => C, External_Name => "shr_long";

  function To_Unsigned_64 is new Ada.Unchecked_Conversion
    (Source => Ll_Integer, Target => Interfaces.Unsigned_64);
  function To_Ll_Integer is new Ada.Unchecked_Conversion
    (Source => Interfaces.Unsigned_64, Target => Ll_Integer);

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
    (To_Integer(Interfaces.Shift_Left(To_Unsigned_32(Val), Bits)));

  function Shr (Val : Integer; Bits : Integer) return Integer is
    (To_Integer(Interfaces.Shift_Right(To_Unsigned_32(Val), Bits)));

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
    (Shl_Long(Val, Bits));

  function Shr (Val : Long_Integer; Bits : Integer) return Long_Integer is
    (Shr_Long(Val, Bits));

  -- Long_Long_Integer
  function "And" (Left, Right : Ll_Integer) return Ll_Integer is
    Res : Ll_Integer;
  begin
    System.Bit_Ops.Bit_And (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "And";

  function "Or"  (Left, Right : Ll_Integer) return Ll_Integer is
    Res : Ll_Integer;
  begin
    System.Bit_Ops.Bit_Or (Left'Address, Left'Size,
                           Right'Address, Right'Size,
                           Res'Address);
    return Res;
  end "Or";

  function "Xor" (Left, Right : Ll_Integer) return Ll_Integer is
    Res : Ll_Integer;
  begin
    System.Bit_Ops.Bit_Xor (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "Xor";

  function "Not" (Val : Ll_Integer) return Ll_Integer is
    Res : Ll_Integer;
  begin
    System.Bit_Ops.Bit_Not (Val'Address, Val'Size, Res'Address);
    return Res;
  end "Not";

  function Shl (Val : Ll_Integer; Bits : Integer) return Ll_Integer is
    (To_Ll_Integer(Interfaces.Shift_Left (To_Unsigned_64(Val), Bits)));

  function Shr (Val : Ll_Integer; Bits : Integer) return Ll_Integer is
    (To_Ll_Integer(Interfaces.Shift_Right (To_Unsigned_64(Val), Bits)));

  -- Uint32
  function Shl (Val : Uint32; Bits : Integer) return Uint32 is
    (Uint32(Interfaces.Shift_Left (Interfaces.Unsigned_32(Val), Bits)));

  function Shr (Val : Uint32; Bits : Integer) return Uint32 is
    (Uint32(Interfaces.Shift_Right (Interfaces.Unsigned_32(Val), Bits)));


  -- Uint64
  function Shl (Val : Uint64; Bits : Integer) return Uint64 is
    (Uint64(Interfaces.Shift_Left (Interfaces.Unsigned_64(Val), Bits)));

  function Shr (Val : Uint64; Bits : Integer) return Uint64 is
    (Uint64(Interfaces.Shift_Right (Interfaces.Unsigned_64(Val), Bits)));

end Bit_Ops;

