-- Marsaglia's simple MWC (Multiply With Carry) random number generator
with C_Types;
package S_Rand is
  subtype Natural_Val is C_Types.Uint32;

  Default_W : constant := 916_191_069;
  Default_Z : constant := 521_288_629;

  -- A random generator
  type Generator is tagged private;

  -- Initialize the generator with given values
  procedure Start (Gen : in out Generator;
                   New_W : in Natural_Val  := Default_W;
                   New_Z : in Natural_Val := Default_Z);

 -- Get a random value
 -- First call to Next on a not-started generator starts it with
 --   the default values
 procedure Next (Gen   : in out Generator; Val : out Float);
 function Next (Gen   : in out Generator) return Float;

private
  type Generator is tagged record
    Started : Boolean := False;
    W : Natural_Val;
    Z : Natural_Val;
  end record;

end S_Rand;

