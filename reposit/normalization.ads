with My_Math;
package Normalization is

  -- Puts an int in a string of fixed length.
  -- If I is shorter than max, it is aligned at right or left
  -- If I is longer, it is rounded if possible (or truncated)

  -- I : value to put in the returned string
  -- Len : Number of characters of the returned string
  -- Right : If string is shorter than Len character, align it at Right
  --   or at left (not Right) and fill with Gap
  -- Gap : When string is shorter than len, fill empty positions with Gap
  generic
    type Int is range <>;
  function Normal_Gen (I     : Int;
                       Len   : Positive;
                       Right : Boolean := True;
                       Gap   : Character := ' ') return String;


  -- Normal_Gen for Integer, My_Math.Inte and Long_Long_Integer
  function Normal_Int (I     : Integer;
                       Len   : Positive;
                       Right : Boolean := True;
                       Gap   : Character := ' ') return String;
  function Normal_Inte (I     : My_Math.Inte;
                        Len   : Positive;
                        Right : Boolean := True;
                        Gap   : Character := ' ') return String;
  function Normal_Long (L     : Long_Long_Integer;
                        Len   : Positive;
                        Right : Boolean := True;
                        Gap   : Character := ' ') return String;



  -- Puts a float F or a real R in a string of fixed length.
  -- S i . f {[ f ]} E S e {[ e ]}
  -- A space or '-', one digit, then a '.' and a fraction part,
  --  then a 'E' a sign then an exponent.
  -- Examples:
  -- Len=9, Exp=2 =>  x.yyE+ij,
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
  -- At least space/sign, then one digit, then a '.', then some fraction part,
  --  then some Gap character if needed
  -- Fore is the length before the dot, padded with spaces if needed
  -- Example: Len=7, Fore=3, Gap='@' => " -x.yy@"
  -- A warning char at the beginning of the output signals an integer part
  -- larger than Fore
  -- Raises Constraint_Error if Fore < 2 or Len < Fore+2
  function Normal_Fixed (F     : Float;
                         Len   : Positive;
                         Fore  : Positive;
                         Gap   : Character := ' ') return String;
  function Normal_Fixed (R     : My_Math.Real;
                         Len   : Positive;
                         Fore  : Positive;
                         Gap   : Character := ' ') return String;
  generic
    type Delt is delta <>;
  function Normal_Delt (D     : Delt;
                        Len   : Positive;
                        Fore  : Positive;
                        Gap   : Character := ' ') return String;
  generic
    type Delt_Dig is delta <> digits <>;
  function Normal_Delt_Dig (D     : Delt_Dig;
                            Len   : Positive;
                            Fore  : Positive;
                            Gap   : Character := ' ') return String;

end Normalization;

