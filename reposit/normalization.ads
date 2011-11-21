package Normalization is

  -- Puts an integer I in a string of fixed length.
  -- If I is shorter than Len, it is aligned at Right or left and the Gap is
  --  used to fill
  -- If I is longer, it is rounded and a warning char ('!') is inserted
  function Normal_Int (I     : Integer;
                       Len   : Positive;
                       Right : Boolean := True;
                       Gap   : Character := ' ') return String;


  -- Puts a float F in a string of fixed length.
  -- S i . f {[ f ]} E S e {[ e ]}
  -- A space or '-', one digit, then a '.' and a fraction part,
  --  then a 'E' a sign then an exponent. Examples:
  -- Len=8, Exp=2 =>  x.yyE+ij,
  -- Len=7, Exp=1 => -x.yE+i which is the minimum
  -- A warning char at the end of the output signal an exponent for F larger
  --  than Exp
  -- Raises Constraint_Error if Len < Exp+6
  function Normal_Digits (F     : Float;
                          Len   : Positive;
                          Exp   : Positive) return String;

  -- Puts a float F in a string of fixed length.
  -- Same as for integers but with at least one digit and possibly a '.'
  --  and some fraction part.
  -- Example: Dig=2 and Len=6 => xxx.yy
  function Normal_Fixed (F     : Float;
                         Len   : Positive;
                         Dig   : Natural;
                         Right : Boolean := True;
                         Gap   : Character := ' ') return String;

end Normalization;

