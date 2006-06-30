-- Decomposition of Arbirary precision numbers into prime factors
with Dynamic_List;
package Arbitrary.Factors is

  -- A list of arbitrary numbers (prime factors)
  package Number_List_Mng is new Dynamic_List (Number);
  package Nb_List_Mng renames Number_List_Mng.Dyn_List;


  -- Decompose N in prime factors, append them to L and rewind it
  -- If N is prime (including 1) only append it
  procedure Decompose (N : in Number; L : in out Nb_List_Mng.List_Type);

  -- Extract common numbers of L1 and L2 and move them (appending) in L
  procedure Extract_Common (L1, L2 : in out Nb_List_Mng.List_Type;
                            L      : in out Nb_List_Mng.List_Type);

  -- Multiply numbers of L from current to the last
  function Multiply (L : in Nb_List_Mng.List_Type) return Number;

end Arbitrary.Factors;

