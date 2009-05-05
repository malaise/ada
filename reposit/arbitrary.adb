with Ada.Strings.Maps;
package body Arbitrary is

  -- Unbounded string operations
  package Unb renames Ada.Strings.Unbounded;
  subtype Unbstr is Unb.Unbounded_String;
  function Set (Str : String) return Unbstr renames Unb.To_Unbounded_String;

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
      if Unb.Length(Unbstr(V)) < 2 then
        return False;
      end if;
      -- First character must be sign
      if not Is_Sign (Unb.Element (Unbstr(V), 1)) then
        return False;
      end if;
      -- All other characters must be digits
      for I in 2 .. Unb.Length(Unbstr(V)) loop
        if not Is_Digit (Unb.Element (Unbstr(V), I)) then
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
    function Make (V : Unbstr) return Number;
    function Extract (N : Number) return Unbstr;

    -- Remove heading 0s
    procedure Trim (A : in out Unbstr);

    -- Both must have no sign
    function Add_No_Sign (A, B : Unbstr) return Unbstr;
    -- Both must have no sign and B <= A
    function Sub_No_Sign (A, B : Unbstr) return Unbstr;
    -- Both must have no sign
    function Mul_No_Sign (A, B : Unbstr) return Unbstr;
    -- Both must have no sign
    procedure Div_No_Sign (A, B : in Unbstr; Q, R : out Unbstr);
    -- Both must have no sign
    function Les_No_Sign (A, B : Unbstr) return Boolean;
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
      D : Unbstr;
      use type Unbstr;
    begin
      D := Basic.Extract (A);
      Basic.Trim (D);
      if Unb.Element (Unbstr(A), 1) = '+' or else D = "0" then
        A := Number ("+" & D);
      else
        A := Number ("-" & D);
      end if;
    end Normalize;

    -- Is N positive
    function Check_Is_Pos (A : Number) return Boolean is
      B : Number;
    begin
      Syntax.Check(A);
      B := A;
      Normalize (B);
      return Unb.Element (Unbstr(B), 1) = '+';
    end Check_Is_Pos;

    -- Number <-> Unbstr : add / remove leading sign
    function Make (V : Unbstr) return Number is
      N : Number;
      use type Unbstr;
    begin
      N := Number(V);
      Normalize (N);
      return N;
    end Make;

    function Extract (N : Number) return Unbstr is
    begin
      return Unb.Tail (Unbstr(N), Unb.Length (Unbstr(N)) - 1);
    end Extract;

    -- Remove heading 0s
    procedure Trim (A : in out Unbstr) is
      use Unb;
    begin
      Unb.Trim (A, Ada.Strings.Maps.To_Set('0'),
                   Ada.Strings.Maps.Null_Set);
      if A = Unb.Null_Unbounded_String then
        A := Unb.To_Unbounded_String("0");
      end if;
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
      Ta := To_Int( A(1)) * 10 + To_Int( A(2));
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
    function Add_No_Sign (A, B : Unbstr) return Unbstr is
      La : constant Natural := Unb.Length(A);
      Lb : constant Natural := Unb.Length(B);
      L : Natural;
      R : Unbstr;
      Ca, Cb, Cr : Character;
      C : Boolean := False;
      use Unb;
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
        if I <= La then
          Ca := Unb.Element (A, La - I + 1);
        else
          Ca := '0';
        end if;
        if I <= Lb then
          Cb := Unb.Element (B, Lb - I + 1);
        else
          Cb := '0';
        end if;
        Add_Char (Ca, Cb, C, Cr);
        Unb.Replace_Element (R, L - I + 1, Cr);
      end loop;
      -- Add last carry and return
      declare
        use Unb;
      begin
        if C then
          return '1' & R;
        else
          return R;
        end if;
      end;
    end Add_No_Sign;

    function Sub_No_Sign (A, B : Unbstr) return Unbstr is
      La : constant Natural := Unb.Length(A);
      Lb : constant Natural := Unb.Length(B);
      R : Unbstr;
      Ca, Cb, Cr : Character;
      C : Boolean := False;
      use Unb;
    begin
      -- Allocate string of largest length
      R := La * ' ';
      -- Sub digits one by one
      for I in 1 .. La loop
        Ca := Unb.Element (A, La - I + 1);
        if I <= Lb then
          Cb := Unb.Element (B, Lb - I + 1);
        else
          Cb := '0';
        end if;
        Sub_Char (Ca, Cb, C, Cr);
        Unb.Replace_Element (R, La - I + 1, Cr);
      end loop;
      -- There should be no carry at the end (cause A > B)
      if C then
        raise Constraint_Error;
      end if;
      -- Remove tailing '0's
      Trim (R);
      return R;
    end Sub_No_Sign;

    function Mul_No_Sign (A, B : Unbstr) return Unbstr is
      La : constant Natural := Unb.Length(A);
      Lb : constant Natural := Unb.Length(B);
      T, R : Unbstr;
      Ca, Cb, Ct : Character;
      C : Natural;
      use Unb;
    begin
      R := Unb.To_Unbounded_String ("0");
      for I in reverse 1 .. Lb loop
        -- Multiply A by this digit of B
        T := La * ' ';
        Cb := Unb.Element (B, I);
        C := 0;
        for J in reverse 1 .. La loop
          Ca :=  Unb.Element (A, J);
          Mult_Char (Ca, Cb, C, Ct);
          Unb.Replace_Element (T, J, Ct);
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
    procedure Div_One (A, B : in Unbstr; Q : out C_Digit; R : out Unbstr) is
      La : constant Natural := Unb.Length(A);
      Lb : constant Natural := Unb.Length(B);
      Ca, Cb, Cq : Character;
      St : Str2;
      T : Unbstr;
      Lt : Natural;
      use type Unbstr;
    begin
      Ca := Unb.Element (A, 1);
      Cb := Unb.Element (B, 1);
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
        St(2) := Unb.Element (A, 2);
      end if;
      Div_Char (St, Cb, Cq);
      -- Check that Q * B <= A or decrement Q until this is true
      loop
        T := Unb.To_Unbounded_String ("" & Cq);
        T := Mul_No_Sign (T, B);
        Lt := Unb.Length(T);
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

    procedure Div_No_Sign (A, B : in Unbstr; Q, R : out Unbstr) is
      La : constant Natural := Unb.Length(A);
      Lb : constant Natural := Unb.Length(B);
      T : Unbstr;
      Cb : constant Character := Unb.Element (B, 1);
      Cq : Character;
      N : Natural;
      use type Unbstr;
    begin
      -- Check that B <= A
      if La < Lb or else (La = Lb and then A < B) then
        -- A < B. Return 0 and A
        Q := Unb.To_Unbounded_String("0");
        R := A;
        return;
      end if;
      -- Divide slices of lenght <= B'Length, so quotien is always
      --  one digit
      Q := Unb.Null_Unbounded_String;
      N := Lb;
      T := Unb.Head(A, N);
      loop
        -- Divide this slice and append digit to quotien
        Div_One (T, B, Cq, R);
        Unb.Append (Q, Cq);
        -- Done when last digit of A has been divided
        exit when N = La;
        -- Take next digit of A and append to previous rest
        N := N + 1;
        T := R & Unb.Element (A, N);
        -- R can be zero => T can have leading zero
        Trim (T);
      end loop;
      -- Q can have leading zeros
      Trim(Q);
    end Div_No_Sign;

    -- Both must have no sign
    function Les_No_Sign (A, B : Unbstr) return Boolean is
      use type Unbstr;
    begin
      if Unb.Length (A) < Unb.Length (B) then
        return True;
      elsif Unb.Length (A) > Unb.Length (B) then
        return False;
      else
        return A < B;
      end if;
    end Les_No_Sign;

  end Basic;

  -- Constructors
  function Set_Uncheck (V : String) return Number is
    use type Unbstr;
  begin
    if Syntax.Is_Sign(V(V'First)) then
      return Basic.Make (Unb.To_Unbounded_String (V));
    else
      return Basic.Make ("+" & Unb.To_Unbounded_String (V));
    end if;
  end Set_Uncheck;

  Number_Zero : constant Number := Set_Uncheck ("0");
  Number_One  : constant Number := Set_Uncheck ("1");

  function Set (V : String) return Number is
    N : Number := Set_Uncheck (V);
  begin
    Syntax.Check (N);
    return N;
  end Set;

  function Strip (V : String) return String is
  begin
    if V(V'First) = ' ' then
      return V(Natural'Succ(V'First) .. V'Last);
    else
      return V;
    end if;
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
    return Unb.Length (Unbstr(V)) >= 2;
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

  -- Image
  function Image (V : Number) return String is
  begin
    return Unb.To_String (Unbstr(V));
  end Image;

  function Length (V : Number) return Natural is
  begin
    return Unb.Length (Unbstr(V));
  end Length;

  -- Absolute and Neg
  function "abs" (A : Number) return Number is
    B : Unbstr := Unbstr(A);
  begin
    if Unb.Element(B, 1) = '-' then
      Unb.Replace_Element (B, 1, '+');
    end if;
    return Number(B);
  end "abs";

  function "-" (A : Number) return Number is
    B : Unbstr := Unbstr(A);
  begin
    if A = Number_Zero then
      return Number_Zero;
    end if;
    if Unb.Element(B, 1) = '-' then
      Unb.Replace_Element (B, 1, '+');
    else
      Unb.Replace_Element (B, 1, '-');
    end if;
    return Number(B);
  end "-";

  -- Comparisons
  function "=" (A, B : Number) return Boolean is
    Ta, Tb : Number;
    use type Unbstr;
  begin
    Ta := A;
    Basic.Normalize (Ta);
    Tb := B;
    Basic.Normalize (Tb);
    return Unbstr(Ta) = Unbstr(Tb);
  end "=";

  function "<" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    use type Unbstr;
  begin
    if Pa /= Pb then
      -- If only one is positive, the other one is smaller
      return (Pb);
    end if;
    if Unb.Length (Unbstr(A)) /= Unb.Length (Unbstr(B)) then
      -- If one is shorter, it is the smaller in abs
      return Unb.Length (Unbstr(A)) < Unb.Length (Unbstr(B)) xor not Pa;
    end if;
    -- Here they have same sign and same length
    if Pa then
      return Unbstr(A) < Unbstr(B);
    else
      return Unbstr(A) > Unbstr(B);
    end if;
  end "<";

  function "<=" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    use type Unbstr;
  begin
    if Pa /= Pb then
      -- If only one is positive, the other one is smaller
      return (Pb);
    end if;
    if Unb.Length (Unbstr(A)) /= Unb.Length (Unbstr(B)) then
      -- If one is shorter, it is the smaller in abs
      return Unb.Length (Unbstr(A)) < Unb.Length (Unbstr(B)) xor not Pa;
    end if;
    -- Here they have same sign and same length
    if Pa then
      return Unbstr(A) <= Unbstr(B);
    else
      return Unbstr(A) >= Unbstr(B);
    end if;
  end "<=";

  function ">" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    use type Unbstr;
  begin
    if Pa /= Pb then
      -- If only one is positive, it is the bigger
      return (Pa);
    end if;
    if Unb.Length (Unbstr(A)) /= Unb.Length (Unbstr(B)) then
      -- If one is larger, it is the larger in abs
      return Unb.Length (Unbstr(A)) > Unb.Length (Unbstr(B)) xor not Pa;
    end if;
    -- Here they have same sign and same length
    if Pa then
      return Unbstr(A) > Unbstr(B);
    else
      return Unbstr(A) < Unbstr(B);
    end if;
  end ">";

  function ">=" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    use type Unbstr;
  begin
    if Pa /= Pb then
      -- If only one is positive, it is the bigger
      return (Pa);
    end if;
    if Unb.Length (Unbstr(A)) /= Unb.Length (Unbstr(B)) then
      --  If one is larger, it is the larger in abs
      return Unb.Length (Unbstr(A)) > Unb.Length (Unbstr(B)) xor not Pa;
    end if;
    -- Here they have same sign and same length
    if Pa then
      return Unbstr(A) >= Unbstr(B);
    else
    return Unbstr(A) <= Unbstr(B);
    end if;
  end ">=";

  -- Addition
  function "+" (A, B : Number) return Number is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    Da : constant Unbstr := Basic.Extract (A);
    Db : constant Unbstr := Basic.Extract (B);
    Pos : Boolean;
    C : Unbstr;
    use type Unbstr;
  begin
    if Pa = Pb then
      -- Same sign: add digits
      C := Basic.Add_No_Sign (Da, Db);
      Pos := Pa;
    else
      -- Different signs: this is a substraction
      if Da = Db then
        C := Unb.To_Unbounded_String("0");
        Pos := True;
      elsif Unb.Length (Da) > Unb.Length (Db)
      or else (Unb.Length (Da) = Unb.Length (Db) and then Da > Db) then
        C := Basic.Sub_No_Sign (Da, Db);
        Pos := Pa;
      else
        C := Basic.Sub_No_Sign (Db, Da);
        Pos := Pb;
      end if;
    end if;
    -- Set sign of result
    if Pos then
      return Basic.Make ('+' & C);
    else
      return Basic.Make ('-' & C);
    end if;
  end "+";

  function "-" (A, B : Number) return Number is
  begin
    return A + (-B);
  end "-";

  function "*" (A, B : Number) return Number is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    Da : constant Unbstr := Basic.Extract (A);
    Db : constant Unbstr := Basic.Extract (B);
    C : Unbstr;
    use type Unbstr;
  begin
    C := Basic.Mul_No_Sign (Da, Db);
    -- Set sign of result
    if Pa = Pb then
      return Basic.Make ('+' & C);
    else
      return Basic.Make ('-' & C);
    end if;
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
    Da : constant Unbstr := Basic.Extract (A);
    Db : constant Unbstr := Basic.Extract (B);
    Tb : Number;
    Dq, Dr : Unbstr;
    use type Unbstr;
  begin
    Tb := B;
    Basic.Normalize (Tb);
    if B = Number_Zero then
      raise Constraint_Error;
    end if;
    Basic.Div_No_Sign (Da, Db, Dq, Dr);
    if Pa = Pb then
      Q := Basic.Make ("+" & Dq);
    else
      Q := Basic.Make ("-" & Dq);
    end if;
    if Pa then
      R := Basic.Make ("+" & Dr);
    else
      R := Basic.Make ("-" & Dr);
    end if;
  end Div;

  function "**" (A, B : Number) return Number is
    Pa : constant Boolean := Basic.Check_Is_Pos (A);
    Pb : constant Boolean := Basic.Check_Is_Pos (B);
    One : constant Number := Set_Uncheck ("1");
    R : Number := One;
    I : Number := B;
  begin
    if not Pb then
      raise Constraint_Error;
    end if;
    loop
      exit when I = Number_Zero;
      R := R * A;
      I := I - One;
    end loop;
    return R;
  end "**";

  procedure Sqrt (A : in Number; S, R : out Number) is
    Onestr : constant Unbstr := Set ("1");
    Twostr : constant Unbstr := Set ("2");
    Ninestr : constant Unbstr := Set ("9");

    -- Extract the square root of a number of 1 or 2 digits
    function Sqrt2 (N : Unbstr) return Unbstr is
      Prev, Curr, Val : Unbstr;
      use type Unbstr;
    begin
      if Unb.Length (N) = 1 then
        -- 1 digit, so it can be 0, 1, 2 or 3
        Prev := Set ("0");
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
      elsif Unb.Length (N) = 2 then
        -- 2 digits so it can be from 4 to 9
        -- Optim: there are more chances that it is 7 or above
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
    -- The slice length is 2 if the length of the number rem 2 = 0 and 1 otherwise
    procedure Get_Slice (N : in out Unbstr; S : out Unbstr) is
      L : Positive;
    begin
      if Unb.Length (N) rem 2 = 0 then
        L := 2;
      else
        L := 1;
      end if;
      S := Unb.To_Unbounded_String (Unb.Slice (N, 1, L));
      N := Unb.To_Unbounded_String (Unb.Slice (N, L + 1, Unb.Length (N)));
    end Get_Slice;

    -- Remove last digit of a number
    function Get_Head (N : Unbstr) return Unbstr is
    begin
      return Unb.To_Unbounded_String (Unb.Slice (N, 1, Unb.Length (N) - 1));
    end Get_Head;

    -- The input
    Input : Unbstr;
    -- The current slice
    Slice : Unbstr;
    -- The solution
    Sol : Unbstr;
    -- The rest
    Rest : Unbstr;
    -- The head of the rest
    Head : Unbstr;
    -- The double of current solution
    Double : Unbstr;
    -- Quotien and rest
    Quot, Tmp_Rest : Unbstr;
    -- The tried digit
    Try : Unbstr;

    use type Unbstr;
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
      exit when Unb.Length (Input) = 0;
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
      if Unb.Length (Quot) > 1 then
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
    return Unb.Length (Unbstr(A)) - 1;
  end Nb_Digits;

  function Nth_Digit (A : Number; N : Positive) return Digit is
  begin
    if N > Nb_Digits (A) then
      raise Constraint_Error;
    else
      return Basic.To_Digit (Unb.Element (Unbstr(A), N + 1));
    end if;
  end Nth_Digit;
    
  function Last_Digit (A : Number) return Digit is
  begin
    Syntax.Check (A);
    return Basic.To_Digit (Unb.Element (Unbstr(A), Unb.Length (Unbstr(A))));
  end Last_Digit;

end Arbitrary;

