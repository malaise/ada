with Normal, Round_At, String_Mng, As.U;
package body Normalization is

  Warning_Char : constant Character := '!';

  -- Puts an integer I in a string of fixed length.
  -- If I is shorter than Len, it is aligned at Right or left and the Gap is
  --  used to fill
  -- If I is longer, it is rounded and a warning char ('!') is inserted
  function Normal_Int (I     : Integer;
                       Len   : Positive;
                       Right : Boolean := True;
                       Gap   : Character := ' ') return String
           renames Normal;

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
                          Exp   : Positive) return String is
  begin
    return Normal_Digits (My_Math.Real(F), Len, Exp);
  end Normal_Digits;

  function Normal_Digits (R     : My_Math.Real;
                          Len   : Positive;
                          Exp   : Positive) return String is
    Str : constant String := R'Img;
    Ie : constant Positive := String_Mng.Locate (Str, "E");
    Str_Len : constant Positive := Str'Length;
    Exp_Len : Positive;
    Exp_Str : String (1 .. Exp + 1) := (others => '0');
    Rea : My_Math.Real;
    Flo_Len : constant Integer:= Len - Exp - 2;
    Flo_Str : String (1 .. Flo_Len);
  begin
    if Len < Exp + 6 then
      raise Constraint_Error;
    end if;

    -- Ext_Str on sign + Exp digits
    -- See how many exp digits are needed. First non 0 after size: E-00xx
    -- Min is 1 if E+00
    Exp_Len := 1;
    for I in Ie + 2 .. Str_Len loop
      if Str(I) /= '0' then
        Exp_Len := Str_Len - I + 1;
        exit;
      end if;
    end loop;
    -- Make Exp_Str: sign and Exp digits
    Exp_Str(1) := Str(Ie + 1);
    if Exp_Len <= Exp then
      -- Copy Tail of Str at tail of Exp_Str (others are '0')
      Exp_Str(Exp_Str'Last - Exp_Len + 1 .. Exp_Str'Last)
        := Str(Str_Len - Exp_Len + 1 .. Str_Len);
    else
      -- Copy Exp-1 and '!'
      Exp_Str(Exp_Str'Last - Exp + 1 .. Exp_Str'Last)
        := Str(Str_Len - Exp + 1 .. Len - 1) & Warning_Char;
    end if;

    -- Floating part on Len - Exp - 2 digits => round at 3 less
    Rea := My_Math.Real'Value (Str(1 .. Ie - 1));
    Rea := Round_At (Rea, -(Flo_Len - 3));
    Flo_Str := Rea'Img(1 .. Flo_Len);

    -- Done
    return Flo_Str & "E" & Exp_Str;
  end Normal_Digits;


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
                         Gap   : Character := ' ') return String is
  begin
    return Normal_Fixed (My_Math.Real(F), Len, Int, Gap);
  end Normal_Fixed;

  function Normal_Fixed (R     : My_Math.Real;
                         Len   : Positive;
                         Int   : Positive;
                         Gap   : Character := ' ') return String is
  begin
    return Warning_Char & "";
  end Normal_Fixed;



end Normalization;

