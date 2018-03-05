-- Decomposition of Arbirary precision numbers into prime factors
with Arbitrary.Limited_List;
package Arbitrary.Factors is

  subtype Positive_Number is Number;
  procedure Set (To : out Positive_Number; Val : in Positive_Number);

  -- A list of primary factors
  package Nb_List_Mng is new Arbitrary.Limited_List (Positive_Number, Set);

  -- Decompose N in prime factors, append them to L and rewind it
  -- If N is prime (including 1) only append it
  -- Raise Constraint_Error if N <= 1
  procedure Decompose (N : in Positive_Number;
                       L : in out Nb_List_Mng.List_Type);

  -- Extract common numbers of L1 and L2 and move them (appending) in L
  procedure Extract_Common (L1, L2 : in out Nb_List_Mng.List_Type;
                            L      : in out Nb_List_Mng.List_Type);

  -- Multiply numbers of L from current to the last
  function Multiply (L : in Nb_List_Mng.List_Type) return Positive_Number;

end Arbitrary.Factors;

