-- Generates random magic number and long magic numbers
with Long_Longs;
package Magic_Numbers is

  -- The magic number types
  subtype Extended_Magic_Int  is Natural;
  subtype Extended_Magic_Long is Long_Longs.Ll_Natural;
  -- Returned random values (1 .. Max)
  subtype Magic_Int  is Extended_Magic_Int
                          range 1 .. Extended_Magic_Int'Last;
  subtype Magic_Long is Extended_Magic_Long
                          range 1 .. Extended_Magic_Long'Last;
  -- Default values (e.g. for "not set")
  Magic_Int0  : constant Extended_Magic_Int  := 0;
  Magic_Long0 : constant Extended_Magic_Long := 0;

  -- At first call, initialize the random number generator
  -- Return a random magic number in 1 .. Max
  function Generate return Magic_Int;
  function Generate return Magic_Long;

end Magic_Numbers;

