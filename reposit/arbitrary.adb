package body Arbitrary is

  -- Digits
  subtype C_Digit is Character range '0' .. '9';
  subtype I_Digit is Digit;


  -- Syntax checking: <sign>{<digit>}
  package Syntax is
    function Is_Digit (C : Character) return Boolean;
    function Is_Sign (C : Character) return Boolean;
    function Check (V : Number) return Boolean;
    -- Raises Constraint_Error
    procedure Check (V : Number);
  end Syntax;

  package body Syntax is
    function Is_Digit (C : Character) return Boolean is
    begin
      return C in C_Digit;
    end Is_Digit;

    function Is_Sign (C : Character) return Boolean is
    begin
      return C = '-' or else C = '+';
    end Is_Sign;

    function Check (V : Number) return Boolean is
    begin
      -- At least 2 characters
      if As.U.Asu_Us(V).Length < 2 then
        return False;
      end if;
      -- First character must be sign
      if not Is_Sign (As.U.Asu_Us(V).Element (1)) then
        return False;
      end if;
      -- All other characters must be digits
      for I in 2 .. As.U.Asu_Us(V).Length loop
        if not Is_Digit (As.U.Asu_Us(V).Element (I)) then
          return False;
        end if;
      end loop;
      return True;
    end Check;

    procedure Check (V : Number) is
    begin
      if not Check (V) then
        raise Constraint_Error;
      end if;
    end Check;
  end Syntax;

  -- Basic aritmetic
  package Basic is
    -- Normalize a number: no leading 0 except +0
    procedure Normalize (A : in out Number);

    -- Is a number positive?
    function Check_Is_Pos (A : Number) return Boolean;

    -- Char -> Digit
    function To_Digit (C : C_Digit) return I_Digit;

    -- Add/remove leading sign
    function Make (V : As.U.Asu_Us) return Number;
    function Extract (N : Number) return As.U.Asu_Us;

    -- Remove heading 0s
    procedure Trim (A : in out As.U.Asu_Us);

    -- Both must have no sign
    function Add_No_Sign (A, B : As.U.Asu_Us) return As.U.Asu_Us;
    -- Both must have no sign and B <= A
    function Sub_No_Sign (A, B : As.U.Asu_Us) return As.U.Asu_Us;
    -- Both must have no sign
    function Mul_No_Sign (A, B : As.U.Asu_Us) return As.U.Asu_Us;
    -- Both must have no sign
    procedure Div_No_Sign (A, B : in As.U.Asu_Us; Q, R : out As.U.Asu_Us);
    -- Both must have no sign
    function Les_No_Sign (A, B : As.U.Asu_Us) return Boolean;
  end Basic;

  package body Basic is
    Zero_Pos : constant Natural := Character'Pos('0');

    -- Int <-> char
    function To_Int (C : C_Digit) return I_Digit is
    begin
      return Character'Pos(C) - Zero_Pos;
    end To_Int;
    function To_Digit (C : C_Digit) return Digit renames To_Int;

    function To_Char (I : Digit) return C_Digit is
    begin
      return Character'Val(I + Zero_Pos);
    end To_Char;

    -- Normalize a number: no leading 0 except +0
    procedure Normalize (A : in out Number) is
      D : As.U.Asu_Us;
      use type As.U.Asu_Us;
    begin
      D := Basic.Extract (A);
      Basic.Trim (D);
      A := Tus (if As.U.Asu_Us(A).Element (1) = '+' or else D = "0" then "+"
                else "-") & D.Image;
    end Normalize;

    -- Is N positive
    function Check_Is_Pos (A : Number) return Boolean is
      B : Number;
    begin
      Syntax.Check(A);
      B := A;
      Normalize (B);
      return As.U.Asu_Us(B).Element (1) = '+';
    end Check_Is_Pos;

    -- Number <-> As.U.Asu_Us : add / remove leading sign
    function Make (V : As.U.Asu_Us) return Number is
      N : Number;
    begin
      N := Tus (V.Image);
      Normalize (N);
      return N;
    end Make;

    function Extract (N : Number) return As.U.Asu_Us is
    begin
      return As.U.Asu_Us(N).Tail (As.U.Asu_Us(N).Length - 1);
    end Extract;

    -- Remove heading 0s
    procedure Trim (A : in out As.U.Asu_Us) is
      Str : constant String := A.Image;
    begin
      for I in Str'Range loop
        if Str(I) /= '0' then
          A := As.U.Tus (Str(I .. Str'Last));
          return;
        end if;
      end loop;
      A := As.U.Tus ("0");
    end Trim;

    -- Char -> Char addition, substraction and multiplication
    -- Note that Carry is 'in out'
    procedure Add_Char (A, B : in Character;
                        Carry : in out Boolean; C : out Character) is
      R : Natural;
    begin
      R := To_Int(A) + To_Int(B);
      if Carry then
        R := R + 1;
      end if;
      Carry := R >= 10;
      if Carry then
        R := R - 10;
      end if;
      C := To_Char (R);
    end Add_Char;

    procedure Sub_Char (A, B : in Character;
                        Carry : in out Boolean; C : out Character) is
      R : Integer;
    begin
      R := To_Int(A) - To_Int(B);
      if Carry then
        R := R - 1;
      end if;
      Carry := R < 0;
      if Carry then
        R := R + 10;
      end if;
      C := To_Char (R);
    end Sub_Char;

    procedure Mult_Char (A, B : in Character;
                         Carry : in out Natural; C : out Character) is
      R : Natural;
    begin
      R := (To_Int(A) * To_Int(B)) + Carry;
      Carry := R / 10;
      R := R rem 10;
      C := To_Char (R);
    end Mult_Char;

    -- Division of a Char (or two) by a char, for an overestimation of the
    -- quotien.
    -- Note that quotien could be above 9. This can occur, by ex when
    --  dividing 710 by 79, which leads to divide 71 by 7. In this case
    --  9 is returned.
    --  And this is why Div_Char does not return the remaining.
    subtype Str2 is String (1 .. 2);
    procedure Div_Char (A : in Str2; B : in Character;
                        Q : out Character) is
      Ta, Tb, D : Natural;
    begin
      Ta := To_Int (A(1)) * 10 + To_Int (A(2));
      Tb := To_Int (B);
      D := Ta / Tb;
      if D in I_Digit then
        Q := To_Char (D);
      else
        -- Quotien above 9 -> 9
        Q := '9';
      end if;
    end Div_Char;

    -- Operations on data without sign
    function Add_No_Sign (A, B : As.U.Asu_Us) return As.U.Asu_Us is
      La : constant Natural := A.Length;
      Lb : constant Natural := B.Length;
      L : Natural;
      R : As.U.Asu_Us;
      Ca, Cb, Cr : Character;
      C : Boolean := False;
      use type As.U.Asu_Us;
    begin
      -- Allocate string of largest length
      if La > Lb then
        L := La;
        R := La * ' ';
      else
        L := Lb;
        R := Lb * ' ';
      end if;
      -- Add digits one by one
      for I in 1 .. L loop
        Ca := (if I <= La then A.Element (La - I + 1) else '0');
        Cb := (if I <= Lb then B.Element (Lb - I + 1) else '0');
        Add_Char (Ca, Cb, C, Cr);
        R.Replace_Element (L - I + 1, Cr);
      end loop;
      -- Add last carry and return
      if C then
        return '1' & R;
      else
        return R;
      end if;
    end Add_No_Sign;

    function Sub_No_Sign (A, B : As.U.Asu_Us) return As.U.Asu_Us is
      La : constant Natural := A.Length;
      Lb : constant Natural := B.Length;
      R : As.U.Asu_Us;
      Ca, Cb, Cr : Character;
      C : Boolean := False;
      use type As.U.Asu_Us;
    begin
      -- Allocate string of largest length
      R := La * ' ';
      -- Sub digits one by one
      for I in 1 .. La loop
        Ca := A.Element (La - I + 1);
        Cb := (if I <= Lb then B.Element (Lb - I + 1) else '0');
        Sub_Char (Ca, Cb, C, Cr);
        R.Replace_Element (La - I + 1, Cr);
      end loop;
      -- There should be no carry at the end (cause A > B)
      if C then
        raise Constraint_Error;
      end if;
      -- Remove heading '0's
      Trim (R);
      return R;
    end Sub_No_Sign;

    function Mul_No_Sign (A, B : As.U.Asu_Us) return As.U.Asu_Us is
      La : constant Natural := A.Length;
      Lb : constant Natural := B.Length;
      T, R : As.U.Asu_Us;
      Ca, Cb, Ct : Character;
      C : Natural;
      use type As.U.Asu_Us;
    begin
      R := As.U.Tus ("0");
      for I in reverse 1 .. Lb loop
        -- Multiply A by this digit of B
        T := La * ' ';
        Cb := B.Element (I);
        C := 0;
        for J in reverse 1 .. La loop
          Ca :=  A.Element (J);
          Mult_Char (Ca, Cb, C, Ct);
          T.Replace_Element (J, Ct);
        end loop;
        if C /= 0 then
          T := To_Char(C) & T;
        end if;
        -- Shift this temp result and add to final result
        T := T & ( (Lb - I) * '0');
        R := Add_No_Sign (R, T);
      end loop;
      Trim (R);
      return R;
    end Mul_No_Sign;

    -- Divide a slice by a slice, the quotient is a single digit
    -- A prerequisit is that A < 10 * B
    procedure Div_One (A, B : in As.U.Asu_Us; Q : out C_Digit; R : out As.U.Asu_Us) is
      La : constant Natural := A.Length;
      Lb : constant Natural := B.Length;
      Ca, Cb, Cq : Character;
      St : Str2;
      T : As.U.Asu_Us;
      Lt : Natural;
      use type As.U.Asu_Us;
    begin
      Ca := A.Element (1);
      Cb := B.Element (1);
      -- Find quotient
      if La < Lb then
        -- A < B
        Q := '0';
        R := A;
        return;
      elsif La = Lb then
        if A < B then
          -- A < B
          Q := '0';
          R := A;
          return;
        else
          -- Same length, so first quotient is got by first digit
          --  of A and B: Ca/Cb
          St(1) := '0';
          St(2) := Ca;
        end if;
      else
        -- A < 10 * B but has one more digit, so first quotien is got
        --  by 2 first digits of A / first digit of B
        St(1) := Ca;
        St(2) := A.Element (2);
      end if;
      Div_Char (St, Cb, Cq);
      -- Check that Q * B <= A or decrement Q until this is true
      loop
        T := As.U.Tus ("" & Cq);
        T := Mul_No_Sign (T, B);
        Lt := T.Length;
        if Lt > La or else (Lt = La and then T > A) then
          -- T > A: Decrement Q by one
          Cq := Character'Pred(Cq);
        else
          -- T <= A, done
          exit;
        end if;
      end loop;
      -- Compute R
      Q := Cq;
      R := Sub_No_Sign (A, T);
      Trim (R);
    end Div_One;

    procedure Div_No_Sign (A, B : in As.U.Asu_Us; Q, R : out As.U.Asu_Us) is
      La : constant Natural := A.Length;
      Lb : constant Natural := B.Length;
      T : As.U.Asu_Us;
      Cq : Character;
      N : Natural;
      use type As.U.Asu_Us;
    begin
      -- Check that B <= A
      if La < Lb or else (La = Lb and then A < B) then
        -- A < B. Return 0 and A
        Q := As.U.Tus ("0");
        R := A;
        return;
      end if;
      -- Divide slices of lenght <= B'Length, so quotien is always
      --  one digit
      Q.Set_Null;
      N := Lb;
      T := A.Head(N);
      loop
        -- Divide this slice and append digit to quotien
        Div_One (T, B, Cq, R);
        Q.Append (Cq);
        -- Done when last digit of A has been divided
        exit when N = La;
        -- Take next digit of A and append to previous rest
        N := N + 1;
        T := R & A.Element (N);
        -- R can be zero => T can have leading zero
        Trim (T);
      end loop;
      -- Q can have leading zeros
      Trim(Q);
    end Div_No_Sign;

    -- Both must have no sign
    function Les_No_Sign (A, B : As.U.Asu_Us) return Boolean is
      use type As.U.Asu_Us;
    begin
      return (if A.Length < B.Length then True
              elsif A.Length > B.Length then False
              else A < B);
    end Les_No_Sign;

  end Basic;

  -- Constructors
  function Set_Uncheck (V : String) return Number is
      use type As.U.Asu_Us;
  begin
    -----------------------------------------------------------------
    -- Bug in Gnat GPL 2012: A "(if" expression retuning Asu_Us fails
    --  (raises Storage_Error or returns Asu_Null)
    -- Don't use it
    -----------------------------------------------------------------
    if Syntax.Is_Sign(V(V'First)) then
      return Basic.Make (As.U.Tus (V));
    else
      return Basic.Make ("+" & As.U.Tus (V));
    end if;
  end Set_Uncheck;

  Number_Zero : constant Number := Set_Uncheck ("0");
  Number_One  : constant Number := Set_Uncheck ("1");
  Number_Two  : constant Number := Set_Uncheck ("2");

  function Set (V : String) return Number is
    N : constant Number := Set_Uncheck (V);
  begin
    Syntax.Check (N);
    return N;
  end Set;

  function Strip (V : String) return String is
  begin
    return (if V(V'First) = ' ' then V(Natural'Succ(V'First) .. V'Last)
            else V);
  end Strip;

  function Set (V : Integer) return Number is
  begin
    return Set_Uncheck (Strip (V'Img));
  end Set;

  function Set (V : Long_Integer) return Number is
  begin
    return Set_Uncheck (Strip (V'Img));
  end Set;

  function Set (V : Long_Long_Integer) return Number is
  begin
    return Set_Uncheck (Strip (V'Img) );
  end Set;

  function Is_Set (V : Number) return Boolean is
  begin
    return As.U.Asu_Us(V).Length >= 2;
  end Is_Set;

  -- "Constants"
  function Zero return Number is
  begin
    return Number_Zero;
  end Zero;

  function One  return Number is
  begin
    return Number_One;
  end One;

  function Two  return Number is
  begin
    return Number_Two;
  end Two;

  -- Image
  function Image (V : Number) return String is
  begin
    return As.U.Asu_Us(V).Image;
  end Image;

  function Length (V : Number) return Natural is
  begin
    return As.U.Asu_Us(V).Length;
  end Length;

  -- Is a Number positive (True for 0)
  function Is_Positive (V : Number) return Boolean is
  begin
    return Basic.Check_Is_Pos (V);
  end Is_Positive;

  -- Absolute and Neg
  function "abs" (A : Number) return Number is
    B : As.U.Asu_Us := As.U.Asu_Us(A);
  begin
    if B.Element(1) = '-' then
      B.Replace_Element (1, '+');
    end if;
    return Tus (B.Image);
  end "abs";

  function "-" (A : Number) return Number is
    B : As.U.Asu_Us := As.U.Asu_Us(A);
  begin
    if A = Number_Zero then
      return Number_Zero;
    end if;
      B.Replace_Element (1, (if B.Element(1) = '-' then '+' else '-'));
    return Tus (B.Image);
  end "-";

  -- Comparisons
  function "=" (A, B : Number) return Boolean is
    Ta, Tb : Number;
    use type As.U.Asu_Us;
  begin
    Ta := A;
    Basic.Normalize (Ta);
    Tb := B;
    Basic.Normalize (Tb);
    return As.U.Asu_Us(Ta) = As.U.Asu_Us(Tb);
  end "=";

  function "<" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    use type As.U.Asu_Us;
  begin
    if Pa /= Pb then
      -- If only one is positive, the other one is smaller
      return (Pb);
    end if;
    if As.U.Asu_Us(A).Length /= As.U.Asu_Us(B).Length then
      -- If one is shorter, it is the smaller in abs
      return As.U.Asu_Us(A).Length < As.U.Asu_Us(B).Length xor not Pa;
    end if;
    -- Here they have same sign and same length
    if Pa then
      return As.U.Asu_Us(A) < As.U.Asu_Us(B);
    else
      return As.U.Asu_Us(A) > As.U.Asu_Us(B);
    end if;
  end "<";

  function "<=" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    use type As.U.Asu_Us;
  begin
    if Pa /= Pb then
      -- If only one is positive, the other one is smaller
      return (Pb);
    end if;
    if As.U.Asu_Us(A).Length /= As.U.Asu_Us(B).Length then
      -- If one is shorter, it is the smaller in abs
      return As.U.Asu_Us(A).Length < As.U.Asu_Us(B).Length xor not Pa;
    end if;
    -- Here they have same sign and same length
    if Pa then
      return As.U.Asu_Us(A) <= As.U.Asu_Us(B);
    else
      return As.U.Asu_Us(A) >= As.U.Asu_Us(B);
    end if;
  end "<=";

  function ">" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    use type As.U.Asu_Us;
  begin
    if Pa /= Pb then
      -- If only one is positive, it is the bigger
      return (Pa);
    end if;
    if As.U.Asu_Us(A).Length /= As.U.Asu_Us(B).Length then
      -- If one is larger, it is the larger in abs
      return As.U.Asu_Us(A).Length > As.U.Asu_Us(B).Length xor not Pa;
    end if;
    -- Here they have same sign and same length
    if Pa then
      return As.U.Asu_Us(A) > As.U.Asu_Us(B);
    else
      return As.U.Asu_Us(A) < As.U.Asu_Us(B);
    end if;
  end ">";

  function ">=" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    use type As.U.Asu_Us;
  begin
    if Pa /= Pb then
      -- If only one is positive, it is the bigger
      return (Pa);
    end if;
    if A.Length /= B.Length then
      --  If one is larger, it is the larger in abs
      return A.Length > B.Length xor not Pa;
    end if;
    -- Here they have same sign and same length
    if Pa then
      return As.U.Asu_Us(A) >= As.U.Asu_Us(B);
    else
    return As.U.Asu_Us(A) <= As.U.Asu_Us(B);
    end if;
  end ">=";

  -- Addition
  function "+" (A, B : Number) return Number is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    Da : constant As.U.Asu_Us := Basic.Extract (A);
    Db : constant As.U.Asu_Us := Basic.Extract (B);
    Pos : Boolean;
    C : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    if Pa = Pb then
      -- Same sign: add digits
      C := Basic.Add_No_Sign (Da, Db);
      Pos := Pa;
    else
      -- Different signs: this is a substraction
      if Da = Db then
        C := As.U.Tus ("0");
        Pos := True;
      elsif Da.Length > Db.Length
      or else (Da.Length = Db.Length and then Da > Db) then
        C := Basic.Sub_No_Sign (Da, Db);
        Pos := Pa;
      else
        C := Basic.Sub_No_Sign (Db, Da);
        Pos := Pb;
      end if;
    end if;
    -- Set sign of result
    return Basic.Make ((if Pos then '+' else '-') & C);
  end "+";

  function "-" (A, B : Number) return Number is
  begin
    return A + (-B);
  end "-";

  function "*" (A, B : Number) return Number is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    Da : constant As.U.Asu_Us := Basic.Extract (A);
    Db : constant As.U.Asu_Us := Basic.Extract (B);
    C : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    C := Basic.Mul_No_Sign (Da, Db);
    -- Set sign of result
    return Basic.Make ((if Pa = Pb then '+' else '-') & C);
  end "*";

  function "/" (A, B : Number) return Number is
    Q, R : Number;
  begin
    Div (A, B, Q, R);
    return Q;
  end "/";

  function "rem" (A, B : Number) return Number is
    Q, R : Number;
  begin
    Div (A, B, Q, R);
    return R;
  end "rem";

  function "mod" (A, B : Number) return Number is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    R : Number;
  begin
    R := A rem B;
    if Pa = Pb or else R = Zero then
      return R;
    else
      return R + B;
    end if;
  end "mod";

  procedure Div (A, B : in Number; Q, R : out Number) is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    Da : constant As.U.Asu_Us := Basic.Extract (A);
    Db : constant As.U.Asu_Us := Basic.Extract (B);
    Tb : Number;
    Dq, Dr : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    Tb := B;
    Basic.Normalize (Tb);
    if B = Number_Zero then
      raise Constraint_Error;
    end if;
    Basic.Div_No_Sign (Da, Db, Dq, Dr);
    Q := Basic.Make ((if Pa = Pb then "+" else "-") & Dq);
    R := Basic.Make ((if Pa then "+" else "-") & Dr);
  end Div;

  function Roundiv (A, B : Number) return Number is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    Q, R : Number;
    Abs_R : Number;
  begin
    Div (A, B, Q, R);
    Abs_R := abs R;
    if abs B - Abs_R <= Abs_R then
      -- R >= B/2 => Q++ or Q--
      if Q = Zero then
        if Pa = Pb then
          return Q + One;
        else
          return Q - One;
        end if;
      elsif Basic.Check_Is_Pos (Q) then
        return Q + One;
      else
        return Q - One;
      end if;
    else
      return Q;
    end if;
  end Roundiv;

  function "**" (A, B : Number) return Number is
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    R : Number := One;
    I : Number := B;
  begin
    Syntax.Check (A);
    if not Pb then
      raise Constraint_Error;
    end if;
    loop
      exit when I = Number_Zero;
      R := R * A;
      I := I - Number_One;
    end loop;
    return R;
  end "**";

  procedure Sqrt (A : in Number; S, R : out Number) is
    Zerostr : constant As.U.Asu_Us := As.U.Tus ("0");
    Onestr : constant As.U.Asu_Us := As.U.Tus ("1");
    Twostr : constant As.U.Asu_Us := As.U.Tus ("2");
    Ninestr : constant As.U.Asu_Us := As.U.Tus ("9");

    -- Extract the square root of a number of 1 or 2 digits
    function Sqrt2 (N : As.U.Asu_Us) return As.U.Asu_Us is
      Prev, Curr, Val : As.U.Asu_Us;
      use type As.U.Asu_Us;
    begin
      if N.Length = 1 then
        -- 1 digit, so it can be 0, 1, 2 or 3
        -- Start trying 0
        Prev := Zerostr;
        Curr := Onestr;
        loop
          Val := Basic.Mul_No_Sign (Curr, Curr);
          if Val = N then
            return Curr;
          elsif Basic.Les_No_Sign (N, Val) then
            -- Curr * Curr > N so the result is Prev
            return Prev;
          else
            Prev := Curr;
            Curr := Basic.Add_No_Sign (Curr, Onestr);
          end if;
        end loop;
      elsif N.Length = 2 then
        -- 2 digits so it can be from 4 to 9
        -- Optim: there are more chances that it is 7 or above
        -- Start trying 9
        Curr := Ninestr;
        loop
          Val := Basic.Mul_No_Sign (Curr, Curr);
          if Val = N or else Basic.Les_No_Sign (Val, N) then
            -- Curr * Curr <= N so the result is Curr
            return Curr;
          end if;
          Curr := Basic.Sub_No_Sign (Curr, Onestr);
        end loop;
      else
        raise Constraint_Error;
      end if;
    end Sqrt2;

    -- Extract and remove a heading slice of a number
    -- The slice length is 2 if the length of the number rem 2 = 0
    --  and 1 otherwise
    procedure Get_Slice (N : in out As.U.Asu_Us; S : out As.U.Asu_Us) is
      L : Positive;
    begin
      if N.Length rem 2 = 0 then
        L := 2;
      else
        L := 1;
      end if;
      S := N.Uslice (1, L);
      N := N.Uslice (L + 1, N.Length);
    end Get_Slice;

    -- Remove last digit of a number
    function Get_Head (N : As.U.Asu_Us) return As.U.Asu_Us is
    begin
      return N.Uslice (1, N.Length - 1);
    end Get_Head;

    -- The input
    Input : As.U.Asu_Us;
    -- The current slice
    Slice : As.U.Asu_Us;
    -- The solution
    Sol : As.U.Asu_Us;
    -- The rest
    Rest : As.U.Asu_Us;
    -- The head of the rest
    Head : As.U.Asu_Us;
    -- The double of current solution
    Double : As.U.Asu_Us;
    -- Quotien and rest
    Quot, Tmp_Rest : As.U.Asu_Us;
    -- The tried digit
    Try : As.U.Asu_Us;

    use type As.U.Asu_Us;
  begin
    -- A must be positive
    if not Basic.Check_Is_Pos (A) then
      raise Constraint_Error;
    end if;
    -- Init: first digit of solution
    Input := Basic.Extract (A);
    -- Extract first slice of 1 or 2 digits
    Get_Slice (Input, Slice);
    -- Get its sqrt
    Sol := Sqrt2 (Slice);
    -- Substract to the slice
    Rest := Basic.Sub_No_Sign (Slice, Basic.Mul_No_Sign (Sol, Sol));

    -- Main loop for one slice
    One_Slice: loop
      -- Done when no more slice
      exit when Input.Length = 0;
      -- Extract next slice (of 2)
      Get_Slice (Input, Slice);
      -- Concat new slice to previous rest
      Rest := Rest & Slice;
      Basic.Trim (Rest);
      -- Discard last digit
      Head := Get_Head (Rest);
      -- Get double of current solution
      Double := Basic.Mul_No_Sign (Twostr, Sol);
      -- Head / (2 * Sol) -> Tmp_Quot
      Basic.Div_No_Sign (Head, Double, Quot, Tmp_Rest);
      -- Tmp_Quot is 9 maximum
      if Quot.Length > 1 then
        Quot := Ninestr;
      end if;

      -- Sub loop to find the correct value of Quot
      One_Try: loop
        -- Double&Tmp_Quot * Tmp_Quot
        Try := Basic.Mul_No_Sign (Double & Quot, Quot);
        -- Exit when Try <= Rest
        exit One_Try when Try = Rest or else Basic.Les_No_Sign (Try, Rest);
        -- Try too big, try with lower Tmp_Quot
        Quot := Basic.Sub_No_Sign (Quot, Onestr);
      end loop One_Try;

    -- Quot is OK
    Sol := Sol & Quot;
    Basic.Trim (Sol);
    Rest := Basic.Sub_No_Sign (Rest, Try);

    end loop One_Slice;

    -- Make numbers
    S := Basic.Make ("+" & Sol);
    R := Basic.Make ("+" & Rest);
  end Sqrt;

  function Sqrt (A : Number) return Number is
    S, R : Number;
  begin
    Sqrt (A, S, R);
    return S;
  end Sqrt;

  -- Digit extraction. Sign is not taken into account
  function Nb_Digits (A : Number) return Positive is
  begin
    Syntax.Check (A);
    -- A length is at least 2
    return As.U.Asu_Us(A).Length - 1;
  end Nb_Digits;

  function Nth_Digit (A : Number; N : Positive) return Digit is
  begin
    if N > Nb_Digits (A) then
      raise Constraint_Error;
    else
      return Basic.To_Digit (As.U.Asu_Us(A).Element (N + 1));
    end if;
  end Nth_Digit;

  function Last_Digit (A : Number) return Digit is
  begin
    Syntax.Check (A);
    return Basic.To_Digit (As.U.Asu_Us(A).Element (As.U.Asu_Us(A).Length));
  end Last_Digit;

end Arbitrary;

