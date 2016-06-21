with Ada.Unchecked_Conversion;

-- Suppress warning than System.Bit_Ops is an internal GNAT unit
--  thus not portable
pragma Warnings (Off, "* is an internal GNAT unit");
with System.Bit_Ops;
pragma Warnings (On,  "* is an internal GNAT unit");

with Interfaces;
with C_Types;

package body Bit_Ops is

  function To_Unsigned_32 is new Ada.Unchecked_Conversion
    (Source => Integer, Target => Interfaces.Unsigned_32);
  function To_Integer is new Ada.Unchecked_Conversion
    (Source => Interfaces.Unsigned_32, Target => Integer);

  function Shl_Long (L : C_Types.Long; Bits : C_Types.Int) return C_Types.Long;
  pragma Import (C, Shl_Long, "shl_long");
  function Shr_Long (L : C_Types.Long; Bits : C_Types.Int) return C_Types.Long;
  pragma Import (C, Shr_Long, "shr_long");

  function To_Unsigned_64 is new Ada.Unchecked_Conversion
    (Source => Ll_Integer, Target => Interfaces.Unsigned_64);
  function To_Ll_Integer is new Ada.Unchecked_Conversion
    (Source => Interfaces.Unsigned_64, Target => Ll_Integer);

  -- Integer
  function "And" (Left, Right : Integer) return Integer is
  pragma Inline ("And");
    Res : Integer;
  begin
    System.Bit_Ops.Bit_And (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "And";

  function "Or"  (Left, Right : Integer) return Integer is
  pragma Inline ("Or");
    Res : Integer;
  begin
    System.Bit_Ops.Bit_Or (Left'Address, Left'Size,
                           Right'Address, Right'Size,
                           Res'Address);
    return Res;
  end "Or";

  function "Xor" (Left, Right : Integer) return Integer is
  pragma Inline ("Xor");
    Res : Integer;
  begin
    System.Bit_Ops.Bit_Xor (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "Xor";

  function "Not" (Val : Integer) return Integer is
  pragma Inline ("Not");
    Res : Integer;
  begin
    System.Bit_Ops.Bit_Not (Val'Address, Val'Size,
                            Res'Address);
    return Res;
  end "Not";

  function Shl (Val : Integer; Bits : Integer) return Integer is
  pragma Inline (Shl);
  begin
    return To_Integer(Interfaces.Shift_Left(To_Unsigned_32(Val), Bits));
  end Shl;

  function Shr (Val : Integer; Bits : Integer) return Integer is
  pragma Inline (Shr);
  begin
    return To_Integer(Interfaces.Shift_Right(To_Unsigned_32(Val), Bits));
  end Shr;

  -- Long_Integer
  function "And" (Left, Right : Long_Integer) return Long_Integer is
  pragma Inline ("And");
    Res : Long_Integer;
  begin
    System.Bit_Ops.Bit_And (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "And";

  function "Or"  (Left, Right : Long_Integer) return Long_Integer is
  pragma Inline ("Or");
    Res : Long_Integer;
  begin
    System.Bit_Ops.Bit_Or (Left'Address, Left'Size,
                           Right'Address, Right'Size,
                           Res'Address);
    return Res;
  end "Or";

  function "Xor" (Left, Right : Long_Integer) return Long_Integer is
  pragma Inline ("Xor");
    Res : Long_Integer;
  begin
    System.Bit_Ops.Bit_Xor (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "Xor";

  function "Not" (Val : Long_Integer) return Long_Integer is
  pragma Inline ("Not");
    Res : Long_Integer;
  begin
    System.Bit_Ops.Bit_Not (Val'Address, Val'Size,
                            Res'Address);
    return Res;
  end "Not";

  function Shl (Val : Long_Integer; Bits : Integer) return Long_Integer is
  pragma Inline (Shl);
  begin
    return Shl_Long(Val, Bits);
  end Shl;

  function Shr (Val : Long_Integer; Bits : Integer) return Long_Integer is
  pragma Inline (Shr);
  begin
    return Shr_Long(Val, Bits);
  end Shr;

  -- Long_Long_Integer
  function "And" (Left, Right : Ll_Integer) return Ll_Integer is
  pragma Inline ("And");
    Res : Ll_Integer;
  begin
    System.Bit_Ops.Bit_And (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "And";

  function "Or"  (Left, Right : Ll_Integer) return Ll_Integer is
  pragma Inline ("Or");
    Res : Ll_Integer;
  begin
    System.Bit_Ops.Bit_Or (Left'Address, Left'Size,
                           Right'Address, Right'Size,
                           Res'Address);
    return Res;
  end "Or";

  function "Xor" (Left, Right : Ll_Integer) return Ll_Integer is
  pragma Inline ("Xor");
    Res : Ll_Integer;
  begin
    System.Bit_Ops.Bit_Xor (Left'Address, Left'Size,
                            Right'Address, Right'Size,
                            Res'Address);
    return Res;
  end "Xor";

  function "Not" (Val : Ll_Integer) return Ll_Integer is
  pragma Inline ("Not");
    Res : Ll_Integer;
  begin
    System.Bit_Ops.Bit_Not (Val'Address, Val'Size, Res'Address);
    return Res;
  end "Not";

  function Shl (Val : Ll_Integer; Bits : Integer) return Ll_Integer is
  pragma Inline (Shl);
  begin
    return To_Ll_Integer(Interfaces.Shift_Left (To_Unsigned_64(Val), Bits));
  end Shl;

  function Shr (Val : Ll_Integer; Bits : Integer) return Ll_Integer is
  pragma Inline (Shr);
  begin
    return To_Ll_Integer(Interfaces.Shift_Right (To_Unsigned_64(Val), Bits));
  end Shr;

end Bit_Ops;

