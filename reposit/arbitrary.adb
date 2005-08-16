with Ada.Strings.Maps;
package body Arbitrary is

  package Unb renames Ada.Strings.Unbounded;
  subtype Unbstr is Unb.Unbounded_String;

  Zero : constant Number := Number(Unb.To_Unbounded_String ("+0"));

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
    function Is_Pos (A : Number) return Boolean;
    procedure Add_Char (A, B : in Character;
                        Carry : in out Boolean; C : out Character);
    procedure Sub_Char (A, B : in Character;
                        Carry : in out Boolean; C : out Character);
    -- Both must have no sign
    function Add_No_Sign (A, B : Unbstr) return Unbstr;
    -- Both must have no sign and B <= A
    function Sub_No_Sign (A, B : Unbstr) return Unbstr;
  end Basic;

  package body Basic is
    Zero_Pos : constant Natural := Character'Pos('0');

    function To_Int (C : Character) return Natural is
    begin
      return Character'Pos(C) - Zero_Pos;
    end To_Int;

    function To_Char (I : Natural) return Character is
    begin
      return Character'Val(I + Zero_Pos);
    end To_Char;

    function Is_Pos (A : Number) return Boolean is
    begin
      return Unb.Element (Unbstr(A), 1) = '+';
    end Is_Pos;

    procedure Add_Char (A, B : in Character; 
                        Carry : in out Boolean; C : out Character) is
      S : Natural;
    begin
      S := To_Int(A) + To_Int(B);
      if Carry then
        S := S + 1;
      end if;
      Carry := S >= 10;
      if Carry then
        S := S - 10;
      end if;
      C := To_Char (S);
    end Add_Char;

    procedure Sub_Char (A, B : in Character; 
                        Carry : in out Boolean; C : out Character) is
      S : Integer;
    begin
      S := To_Int(A) - To_Int(B);
      if Carry then
        S := S - 1;
      end if;
      Carry := S < 0;
      if Carry then
        S := S + 10;
      end if;
      C := To_Char (S);
    end Sub_Char;

    function Add_No_Sign (A, B : Unbstr) return Unbstr is
      La : constant Natural := Unb.Length(A);
      Lb : constant Natural := Unb.Length(B);
      L : Natural;
      R : Unbstr;
      Ca, Cb, Cr : Character;
      C : Boolean := False;
    begin
      -- Allocate string of largest length
      if La > Lb then
        L := La;
        R := A;
      else 
        L := Lb;
        R := B;
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
      Unb.Trim (R, Ada.Strings.Maps.To_Set('0'), Ada.Strings.Maps.Null_Set);
      return R;
    end Sub_No_Sign;

  end Basic;

  -- Constructor
  function Set (V : String) return Number is
    N : Number;
  begin
    if V = "-0" then
      N := Zero;
    elsif Syntax.Is_Sign(V(V'First)) then
      N := Number(Unb.To_Unbounded_String (V));
    else
      N := Number(Unb.To_Unbounded_String ("+" & V));
    end if;
    Syntax.Check(N);
    return N;
  end Set;

  function Set (V : Integer) return Number is
  begin
    return Set (V'Img);
  end Set;

  function Set (V : Long_Integer) return Number is
  begin
    return Set (V'Img);
  end Set;

  function Set (V : Long_Long_Integer) return Number is
  begin
    return Set (V'Img);
  end Set;

  -- Image
  function Image (V : Number) return String is
  begin
    return Unb.To_String(Unbstr(V));
  end Image;

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
    use type Unbstr;
  begin
    return Unbstr(A) = Unbstr(B);
  end "=";

  function "<" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Is_Pos (A);
    Pb : constant Boolean := Basic.Is_Pos (B);
    La : Natural;
    use type Unbstr;
  begin
    if Pa /= Pb then
      -- If only one is positive, the other one is smaller
      return (Pb);
    end if;
    La := Unb.Length (Unbstr(A));
    if La < Unb.Length (Unbstr(A)) then
      -- If one is shorter, it is the smaller in abs
      return Pa;
    end if;
    -- Here they have same sign and same length
    return Unbstr(A) < Unbstr(B) xor not Pa;
  end "<";

  function "<=" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Is_Pos (A);
    Pb : constant Boolean := Basic.Is_Pos (B);
    La : Natural;
    use type Unbstr;
  begin
    if Pa /= Pb then
      -- If only one is positive, the other one is smaller
      return (Pb);
    end if;
    La := Unb.Length (Unbstr(A));
    if La < Unb.Length (Unbstr(A)) then
      -- If one is shorter, it is the smaller in abs
      return Pa;
    end if;
    -- Here they have same sign and same length
    return Unbstr(A) <= Unbstr(B) xor not Pa;
  end "<=";

  function ">" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Is_Pos (A);
    Pb : constant Boolean := Basic.Is_Pos (B);
    La : Natural;
    use type Unbstr;
  begin
    if Pa /= Pb then
      -- If only one is positive, it is the bigger
      return (Pa);
    end if;
    La := Unb.Length (Unbstr(A));
    if La > Unb.Length (Unbstr(A)) then
      -- If one is larger, it is the larger in abs
      return Pa;
    end if;
    -- Here they have same sign and same length
    return Unbstr(A) > Unbstr(B) xor not Pa;
  end ">";

  function ">=" (A, B : Number) return Boolean is
    Pa : constant Boolean := Basic.Is_Pos (A);
    Pb : constant Boolean := Basic.Is_Pos (B);
    La : Natural;
    use type Unbstr;
  begin
    if Pa /= Pb then
      -- If only one is positive, it is the bigger
      return (Pa);
    end if;
    La := Unb.Length (Unbstr(A));
    if La > Unb.Length (Unbstr(A)) then
      --  If one is larger, it is the larger in abs
      return Pa;
    end if;
    -- Here they have same sign and same length
    return Unbstr(A) >= Unbstr(B) xor not Pa;
  end ">=";

  -- Addition
  function "+" (A, B : Number) return Number is
    Pa : constant Boolean := Basic.Is_Pos (A);
    Pb : constant Boolean := Basic.Is_Pos (B);
    Da : Unbstr := Unb.Tail (Unbstr(A), Unb.Length (Unbstr(A)) - 1);
    Db : Unbstr := Unb.Tail (Unbstr(B), Unb.Length (Unbstr(B)) - 1);
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
      return Number('+' & C);
    else
      return Number('-' & C);
    end if;
  end "+";

  function "-" (A, B : Number) return Number is
  begin
    return A + (-B);
  end "-";

end Arbitrary;

