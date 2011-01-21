with Types;

package Euristic is

  -- Search a solution
  -- Mattrix is set to : 1 -> affected
  --                     0 -> not affected
  -- Or Done is False
  procedure Search (Mattrix : in out Types.Mattrix_Rec;
                    Nb_Iterations : out Positive;
                    Done : out Boolean);

end Euristic;

