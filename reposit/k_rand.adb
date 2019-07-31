with Bit_Ops;
package body K_Rand is
  use type Natural_Val;

  -- Initialize the generator with given values
  procedure Start (Gen : in out Generator;
                   New_W : in Natural_Val  := Default_W;
                   New_X : in Positive_Val := Default_X;
                   New_Y : in Positive_Val := Default_Y;
                   New_Z : in Positive_Val := Default_Z) is
  begin
    Gen.W := New_W;
    Gen.X := New_X;
    Gen.Y := New_Y;
    Gen.Z := New_Z;
    Gen.Started := True;
  end Start;

  -- Get a random value
  -- First call to Next on a not-started generator starts it with
  --   the default values
  procedure Next (Gen   : in out Generator; Val : out Float) is
  begin
    Val := Next (Gen);
  end Next;

  function Next (Gen   : in out Generator) return Float is
    use Bit_Ops;
    function Ml (Value : in Natural_Val; Shift : in Natural) return Natural_Val
             with Inline => True;
    function Mr (Value : in Natural_Val; Shift : in Natural) return Natural_Val
             with Inline => True;
    function Ml (Value : in Natural_Val; Shift : in Natural)
                return Natural_Val is
    begin
      return Value xor Shl (Value, Shift);
    end Ml;

    function Mr (Value : in Natural_Val; Shift : in Natural)
                return Natural_Val is
    begin
      return Value xor Shr (Value, Shift);
    end Mr;

    F : Float;
  begin
    if not Gen.Started then
      Start (Gen);
    end if;

    Gen.W := 30903 * (Gen.W and 65535) + Shr (Gen.W, 16);
    Gen.X := 69069 * Gen.X + 1327217885;
    Gen.Y := Ml (Mr (Ml (Gen.Y, 13), 17), 5);
    Gen.Z := 18000 * (Gen.Z and 65535) + Shr (Gen.Z, 16);

    -- Build a float 0.0 <= F < 1.0
    F := Float(Gen.X + Gen.Y + Shl (Gen.Z, 16) + Gen.W)
         / Float (Natural_Val'Last);
    if F >= 1.0 then
      F := 0.0;
    end if;
    return F;
  end Next;

end K_Rand;

