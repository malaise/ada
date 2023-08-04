with Types;

package Euristic is

  -- Search a solution
  -- Max_Iterations = 0 means unlimited
  -- Mattrix is set to : 1 -> affected
  --                     0 -> not affected
  -- Or Done is False
  procedure Search (Mattrix : in out Types.Mattrix_Rec;
                    Max_Iterations : in Natural;
                    Progress : in Boolean;
                    Nb_Iterations : out Positive;
                    Done : out Boolean);

end Euristic;

