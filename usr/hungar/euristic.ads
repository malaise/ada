with Types;

package Euristic is

  -- Search a solution
  -- Mattrix is set to : 1 -> affected
  --                     0 -> not affected
  procedure Search (Mattrix : in out Types.Mattrix_Rec; Nb_Iterations : out Positive);

end Euristic;

