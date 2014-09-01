-- Generates random magic number and long magic numbers
-- with Long_Longs;
with Rnd;
package body Magic_Numbers is

  -- The generator of Lmn
  Generator : Rnd.Generator;
  function Lmn_Random is new Rnd.Discr_Random (Lmn);

  -- At first call, initialize the random number generator
  -- Return a random magic number
  function Generate return Mn is
  begin
    -- Reminder of a random Lmn at Mn'Last, plus one
    return Mn(Generate rem (Lmn(Mn'Last) + 1));
  end Generate;

  function Generate return Lmn is
  begin
    if not Generator.Is_Randomized then
      Generator.Randomize;
    end if;
    return Lmn_Random (Generator);
  end Generate;

end Magic_Numbers;

