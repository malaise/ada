with TYPES;

package EURISTIC is

  -- Search a solution
  -- Mattrix is set to : 1 -> affected
  --                     0 -> not affected
  procedure SEARCH (MATTRIX : in out TYPES.MATTRIX_REC; NB_ITERATIONS : out POSITIVE);

end EURISTIC;
