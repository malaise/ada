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
    declare
      Image : constant String := Rea'Img;
      Iie : constant Positive := String_Mng.Locate (Image, "E");
    begin
      if Iie - 1 >= Flo_Len then
        Flo_Str := Image(1 .. Flo_Len);
      else
        Flo_Str := (others => '0');
        Flo_Str(1 .. Iie - 1) := Image (1 .. Iie - 1);
      end if;
    end;

    -- Done
    return Flo_Str & "E" & Exp_Str;
  end Normal_Digits;


  -- Local: round Str (space/signXXX.YYYYY) at length Len
  procedure Round_Str (Str : in out As.U.Asu_Us; Len : in Positive) is
    Carry : Boolean;
    Char : Character;
  begin
    if Len >= Str.Length then
      -- Nothing to do
      return;
    end if;
    -- Str is longer than Len

    -- Analyse tail to see if it leads to a carry
    -- Only a '4' requires to see next char (to see if it turns to '5')
    -- By default, if end is reached without '5', then no Carry
    Carry  := False;
    for I in Len + 1 .. Str.Length loop
      Char := Str.Element (I);
      if Char >= '5' then
        Carry := True;
        exit;
      elsif Char < '4' then
        -- No carry
        exit;
      end if;
    end loop;

    -- Delete tail
    Str.Delete (Len + 1, Str.Length);
    -- Propagate Carry
    for I in reverse 2 .. Str.Length loop
      exit when not Carry;
      Char := Str.Element (I);
      if Char /= '.' then
        -- Just skip dot
        if Char /= '9' then
          Char := Character'Succ (Char);
          Carry := False;
        else
          Char := '0';
        end if;
        Str.Replace_Element (I, Char);
      end if;
    end loop;
    if Carry then
      -- Heading '9' turned into '0': make it "10"
      Str.Insert (2, "1");
    end if;

    -- Remove tailing '0's
    for I in reverse 2 .. Str.Length loop
      if Str.Element (I) /= '0' then
        if I /= Str.Length then
          Str.Delete (I + 1, Str.Length);
        end if;
        exit;
      end if;
    end loop;

    -- Done
  end Round_Str;

  -- Local: normalize Digit_Str (space/signX.YYYYY) with exp Exp
  procedure Normal_Str (Digit_Str : in out As.U.Asu_Us;
                        Len  : in Positive;
                        Fore : in Positive;
                        Gap  : in Character := ' ') is
    Ie, Idot : Positive;
    Exp  : Integer;
    Neg : constant Boolean := Digit_Str.Element (1) = '-';
    -- Len = Fore + 1 + Aft
    Aft : constant Integer := Len - Fore - 1;
    use type My_Math.Real;
  begin
    -- Compute exp of 10 and store all digits (skip E-xx)
    -- Nb of bytes for int part is Exp + 1 (10 ** Exp)
    Ie := String_Mng.Locate (Digit_Str.Image, "E");
    Exp := Natural'Value (Digit_Str.Slice (Ie + 1, Digit_Str.Length));
    -- Digit_Str contains "-x.yzt"
    Digit_Str.Delete (Ie, Digit_Str.Length);

    -- Handle the case when Exp is so high that nothing at all can be
    --  displayed: enough to shift right Digit_Str by its len -2
    -- (which leads to "      xyzt.") then right by Fore - 1
    -- => "!-000.0   " or "!000.0 ", or even "!-.0  "
    if Exp > Digit_Str.Length - 2 + Fore - 1 then
      Digit_Str := As.U."*" (Len, '0');
      Digit_Str.Replace_Element (1, Warning_Char);
      Digit_Str.Replace_Element (Fore + 1, '.');
      if Neg then
        Digit_Str.Replace_Element (2, '-');
      end if;
      for I in Fore + 3 .. Len loop
        Digit_Str.Replace_Element (I, Gap);
      end loop;
      return;
    end if;

    -- Now Exp is not so high
    -- Move dot at proper place in Digits_Str
    if Exp > 0 then
      Digit_Str.Delete (3, 3);
      if Exp + 3 <= Digit_Str.Length then
        Digit_Str.Insert (Exp + 3, ".");
      else
        Digit_Str.Append (As.U."*" (Exp  + 3 - Digit_Str.Length, '0'));
        Digit_Str.Append (".0");
      end if;
      Idot := Exp + 3;
    elsif Exp < 0 then
      -- Delete dot and sign
      Digit_Str.Delete (3, 3);
      Digit_Str.Delete (1, 1);
      Digit_Str.Prepend (As.U."*" (abs(Exp) -1, '0'));
      if Neg then
        Digit_Str.Prepend ("-0.");
      else
        Digit_Str.Prepend (" 0.");
      end if;
      Idot := 3;
    else
      Idot := 3;
    end if;

    -- Round tail so that its frac has Aft length or less
    Round_Str (Digit_Str, Idot + Aft);

    -- Complete tail with Gap so that its frac has Aft length
    if Digit_Str.Length - Idot < Aft then
      Digit_Str.Append (As.U."*" (Aft + Idot - Digit_Str.Length, Gap));
    end if;

    -- Handle case of overflow / underflow on Fore
    if Idot > Fore + 1 then
      Digit_Str.Delete (1 , Idot - Fore - 1);
      Digit_Str.Replace_Element (1, Warning_Char);
      if Neg then
        Digit_Str.Replace_Element (2, '-');
      end if;
    elsif Idot < Fore + 1 then
      Digit_Str.Prepend (As.U."*" (Fore + 1 - Idot, ' '));
    end if;

  end Normal_Str;

  -- Puts a float F or a real R in a string of fixed length.
  -- At least one digit then a '.' then some fraction part.
  -- S i . f
  -- Example: Int=2 and Len=6 => " -x.yy"
  -- A warning char at the beginning of the output signals an integer part
  -- larger than Int
  -- Raises Constraint_Error if Len < Int+3
  function Normal_Fixed (F     : Float;
                         Len   : Positive;
                         Fore  : Positive;
                         Gap   : Character := ' ') return String is
  begin
    return Normal_Fixed (My_Math.Real(F), Len, Fore, Gap);
  end Normal_Fixed;

  function Normal_Fixed (R     : My_Math.Real;
                         Len   : Positive;
                         Fore  : Positive;
                         Gap   : Character := ' ') return String is
    Digit_Str : As.U.Asu_Us;
  begin
    -- Ex with Fore=10 and Len=20
    -- <----- len -------->
    --          x.yzt
    -- <- Fore ->.<- Aft ->
    if Fore < 2 or else Len < Fore + 2 then
      raise Constraint_Error;
    end if;

    Digit_Str.Set (R'Img);
    Normal_Str (Digit_Str, Len, Fore, Gap);
    return Digit_Str.Image;

  end Normal_Fixed;


  -- Local: transform a delta image into a Digit_Str (float image)
  -- Ex: "-xyz.tu" into "-x.yztuE+2"
  procedure Make_Digit (Str : in out As.U.Asu_Us) is
    Idot : constant Positive := String_Mng.Locate (Str.Image, ".");
    Exp_Image : constant String := Integer'Image(Idot - 3);
    Exp_Str : String (1 .. Exp_Image'Length) := Exp_Image;
  begin
    if Exp_Str(1) = ' ' then
       Exp_Str(1) := '+';
    end if;
    Str.Append ("E" & Exp_Str);
    Str.Delete (Idot, Idot);
    Str.Insert (3, ".");
  end Make_Digit;

  -- generic type Delt is delta <>;
  function Normal_Delt (D     : Delt;
                        Len   : Positive;
                        Fore  : Positive;
                        Gap   : Character := ' ') return String is
    Digit_Str : As.U.Asu_Us;
  begin
    if Fore < 2 or else Len < Fore + 2 then
      raise Constraint_Error;
    end if;

    Digit_Str.Set (D'Img);
    Make_Digit (Digit_Str);
    Normal_Str (Digit_Str, Len, Fore, Gap);
    return Digit_Str.Image;
  end Normal_Delt;

  -- generic type Delt_Dig is delta <> digits <>;
  function Normal_Delt_Dig (D     : Delt_Dig;
                            Len   : Positive;
                            Fore  : Positive;
                            Gap   : Character := ' ') return String is
    Digit_Str : As.U.Asu_Us;
  begin
    if Fore < 2 or else Len < Fore + 2 then
      raise Constraint_Error;
    end if;

    Digit_Str.Set (D'Img);
    Make_Digit (Digit_Str);
    Normal_Str (Digit_Str, Len, Fore, Gap);
    return Digit_Str.Image;
  end Normal_Delt_Dig;

end Normalization;
