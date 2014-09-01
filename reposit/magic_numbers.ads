-- Generates random magic number and long magic numbers
with Long_Longs;
package Magic_Numbers is

  -- The magic number types
  subtype Mn is Positive;
  subtype Lmn is  Long_Longs.Ll_Positive;

  -- At first call, initialize the random number generator
  -- Return a random magic number
  function Generate return Mn;
  function Generate return Lmn;

end Magic_Numbers;

