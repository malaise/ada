-- Generates random magic number and long magic numbers
-- with Long_Longs;
with Rnd;
package body Magic_Numbers is

  -- The generator of Lmn
  Generator : Rnd.Generator;
  function Magic_Long_Random is new Rnd.Discr_Random (Magic_Long);

  -- At first call, initialize the random number generator
  -- Return a random magic number
  function Generate return Magic_Int is
  begin
    -- Reminder of a random Long at Number'Last, + 1
    return Magic_Int(Generate rem (Magic_Long(Magic_Int'Last)) + 1);
  end Generate;

  function Generate return Magic_Long is
    L : Long_Long_Integer;
  begin
    -- Randomize only once, at first call
    if not Generator.Is_Randomized then
      Generator.Randomize;
    end if;
    L := Magic_Long_Random (Generator);

    return L;
  end Generate;

end Magic_Numbers;

