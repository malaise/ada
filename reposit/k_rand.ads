-- Marsaglia's KISS Random Number Generator
-- Algorithm taken from www.fortran.com/kiss.f90
with C_Types;
package K_Rand is
  subtype Natural_Val is C_Types.Uint32;
  subtype Positive_Val is Natural_Val range 1 .. Natural_Val'Last;

  Default_W : constant := 916_191_069;
  Default_X : constant := 123_456_789;
  Default_Y : constant := 362_436_069;
  Default_Z : constant := 521_288_629;

  -- A random generator
  type Generator is tagged private;

  -- Initialize the generator with given values
  procedure Start (Gen : in out Generator;
                   New_W : in Natural_Val  := Default_W;
                   New_X : in Positive_Val := Default_X;
                   New_Y : in Positive_Val := Default_Y;
                   New_Z : in Positive_Val := Default_Z);

 -- Get a random value
 -- First call to Next on a not-started generator starts it with
 --   the default values
 procedure Next (Gen   : in out Generator; Val : out Float);
 function Next (Gen   : in out Generator) return Float;

private
  type Generator is tagged record
    Started : Boolean := False;
    W : Natural_Val;
    X : Positive_Val;
    Y : Positive_Val;
    Z : Positive_Val;
  end record;

end K_Rand;

