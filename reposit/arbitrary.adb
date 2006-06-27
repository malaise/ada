with Ada.Strings.Maps;
package body Arbitrary is

  package Unb renames Ada.Strings.Unbounded;
  subtype Unbstr is Unb.Unbounded_String;

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
      return C >= '0' and then C <= '9';
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
    function Mult_No_Sign (A, B : Unbstr) return Unbstr;
    -- Both must have no sign
    procedure Div_No_Sign (A, B : in Unbstr; Q, R : out Unbstr);
  end Basic;

  package body Basic is
    Zero_Pos : constant Natural := Character'Pos('0');

    -- Int <-> char
    function To_Int (C : Character) return Natural is
    begin
      return Character'Pos(C) - Zero_Pos;
    end To_Int;

    function To_Char (I : Natural) return Character is
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

    -- Char -> Char operations
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

    subtype Str2 is String (1 .. 2);
    procedure Div_Char (A : in Str2; B : in Character;
                        Q : out Character; R : out Character) is
      Ta, Tb : Natural;
    begin
      Ta := To_Int( A(1)) * 10 + To_Int( A(2));
      Tb := To_Int (B);
      Q := To_Char (Ta / Tb);
      R := To_Char (Ta rem Tb);
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

    function Mult_No_Sign (A, B : Unbstr) return Unbstr is
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
    end Mult_No_Sign;

    procedure Div_One (A, B : in Unbstr; Q : out Character; R : out Unbstr) is
      La : constant Natural := Unb.Length(A);
      Lb : constant Natural := Unb.Length(B);
      Ca, Cb, Cq, Cr : Character;
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
          St(1) := '0';
          St(2) := Ca;
        end if;
      else
        St(1) := Ca;
        St(2) := Unb.Element (A, 2);
      end if;
      Div_Char (St, Cb, Cq, Cr);
      -- Check that Q * B < A or decrement Q until
      loop
        T := Unb.To_Unbounded_String ("" & Cq);
        T := Mult_No_Sign (T, B);
        Lt := Unb.Length(T);
        if Lt > La or else (Lt = La and then T > A) then
          -- Decrement Q by one
          Cq := Character'Pred(Cq);
        else
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
        -- A < B. Return 0, A
        Q := Unb.To_Unbounded_String("0");
        R := A;
        return;
      end if;
      Q := Unb.Null_Unbounded_String;
      N := Lb;
      T := Unb.Head(A, N);
      loop
        Div_One (T, B, Cq, R);
        Unb.Append (Q, Cq);
        exit when N = La;
        N := N + 1;
        T := R & Unb.Element (A, N);
        Trim (T);
      end loop;
      Trim(Q);
    end Div_No_Sign;
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
  Zero : constant Number := Set_Uncheck ("0");

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
    if A = Zero then
      return Zero;
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
    return Unbstr(A) < Unbstr(B) xor not Pa;
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
    return Unbstr(A) <= Unbstr(B) xor not Pa;
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
    return Unbstr(A) > Unbstr(B) xor not Pa;
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
    return Unbstr(A) >= Unbstr(B) xor not Pa;
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
    C := Basic.Mult_No_Sign (Da, Db);
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
    if B = Zero then
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

  function "**" (A, B : in Number) return Number is
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
      exit when I = Zero;
      R := R * A;
      I := I - One;
    end loop;
    return R;
  end "**";

end Arbitrary;

