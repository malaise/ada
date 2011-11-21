with My_Math;
package Normalization is

  -- Puts an integer I in a string of fixed length.
  -- If I is shorter than Len, it is aligned at Right or left and the Gap is
  --  used to fill
  -- If I is longer, it is rounded and a warning char ('!') is inserted
  function Normal_Int (I     : Integer;
                       Len   : Positive;
                       Right : Boolean := True;
                       Gap   : Character := ' ') return String;


  -- Puts a float F or a real R in a string of fixed length.
  -- S i . f {[ f ]} E S e {[ e ]}
  -- A space or '-', one digit, then a '.' and a fraction part,
  --  then a 'E' a sign then an exponent. Examples:
  -- Len=8, Exp=2 =>  x.yyE+ij,
  -- Len=7, Exp=1 => -x.yE+i which is the minimum
  -- A warning char at the end of the output signals an exponent for F larger
  --  than Exp
  -- Raises Constraint_Error if Len < Exp+6
  function Normal_Digits (F     : Float;
                          Len   : Positive;
                          Exp   : Positive) return String;
  function Normal_Digits (R     : My_Math.Real;
                          Len   : Positive;
                          Exp   : Positive) return String;

  -- Puts a float F or a real R in a string of fixed length.
  -- At least one digit then a '.' then some fraction part.
  -- S i . f
  -- Example: Dig=2 and Len=6 => " -x.yy"
  -- A warning char at the beginning of the output signals an integer part
  -- larger than Int
  -- Raises Constraint_Error if Len < Int+3
  function Normal_Fixed (F     : Float;
                         Len   : Positive;
                         Int   : Positive;
                         Gap   : Character := ' ') return String;
  function Normal_Fixed (R     : My_Math.Real;
                         Len   : Positive;
                         Int   : Positive;
                         Gap   : Character := ' ') return String;

end Normalization;

