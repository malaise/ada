
--------------------------------------------------------------------------
-- The following is an implementation of a "universal" random number    --
-- generator algorithm developed by Dr. George Marsaglia of the         --
-- Supercomputer Computations Research Institute (SCRI) at Florida      --
-- State University. This generator has a period of ~2**144 and has     --
-- been tailored for reproducibility in all CPU's with at least         --
-- 16 bit integer arithmetic and 24 bit floating point. This algorithm  --
-- does not generate random numbers < 2**-24.
--------------------------------------------------------------------------

package U_Rand is

  M1 : constant := 179;
  M2 : constant := M1 - 10;

  subtype Seed_Range_1 is Integer range 1 .. M1 - 1;
  subtype Seed_Range_2 is Integer range 1 .. M2 - 1;

  Default_I : constant Seed_Range_1 := 12;
  Default_J : constant Seed_Range_1 := 34;
  Default_K : constant Seed_Range_1 := 56;
  Default_L : constant Seed_Range_1 := 78;

  type Generator is tagged private;

  procedure Start(Gen   : in out Generator;
                  New_I : in Seed_Range_1 := Default_I;
                  New_J : in Seed_Range_1 := Default_J;
                  New_K : in Seed_Range_1 := Default_K;
                  New_L : in Seed_Range_2 := Default_L);

  procedure Next (Gen   : in out Generator; Val : out Float);
private
  M3 : constant := 97;
  subtype Range_3 is Integer range 1 .. M3;
  type Array_3 is array(Range_3) of Float;

  type Generator is tagged record
    Started : Boolean := False;
    Ni, Nj  : Integer;
    C       : Float;
    U       : Array_3;
  end record;

end U_Rand;

