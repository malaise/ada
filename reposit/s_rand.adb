with Bit_Ops;
package body S_Rand is
  use type Natural_Val;

  -- Initialize the generator with given values
  procedure Start (Gen : in out Generator;
                   New_W : in Natural_Val  := Default_W;
                   New_Z : in Natural_Val := Default_Z) is
  begin
    Gen.W := New_W;
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
    F : Float;
    use Bit_Ops;
  begin
    if not Gen.Started then
      Start (Gen);
    end if;

    Gen.Z := 36969 * (Gen.Z and 65535) + Shr (Gen.Z, 16);
    Gen.W := 18000 * (Gen.W and 65535) + Shr (Gen.W, 16);

    -- Build a float 0.0 <= F < 1.0
    F := Float (Shl (Gen.Z, 16) + Gen.W) / Float (Natural_Val'Last);
    if F >= 1.0 then
      F := 0.0;
    end if;
    return F;
  end Next;

end S_Rand;

