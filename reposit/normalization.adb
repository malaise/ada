with Normal, Round_At, String_Mng, As.U, My_Math;
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

  -- Puts a float F in a string of fixed length.
  -- Same as for integers but with at least one digit, the letter E, a
  -- sign and exponent.  Examples:
  -- Len=8, Exp=2 => x.yyE+ij,
  -- Len=5, Exp=1 => xE+i which is the minimum
  -- raises Constraint_Error if Len < Exp+3
  function Normal_Digits (F     : Float;
                          Len   : Positive;
                          Exp   : Positive) return String is
    Str : constant String := F'Img;
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

  -- Puts a float F in a string of fixed length.
  -- Same as for integers but with at least one digit and possibly a '.'
  --  and some fraction part.
  -- Example: Dig=2 and Len=6 => xxx.yy
  function Normal_Fixed (F     : Float;
                         Len   : Positive;
                         Dig   : Natural;
                         Right : Boolean := True;
                         Gap   : Character := ' ') return String is
  begin
    return "!";
  end Normal_Fixed;


end Normalization;

