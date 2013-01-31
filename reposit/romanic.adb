with As.U;
package body Romanic is

  Values : constant array (Digit) of Positive := (
    'I' =>     1,
    'V' =>     5,
    'X' =>    10,
    'L' =>    50,
    'C' =>   100,
    'D' =>   500,
    'M' => 1_000);

  -- Character <-> Digit conversion
  function D2C (D : Digit) return Character is
  begin
    case D is
      when 'I' => return 'I';
      when 'V' => return 'V';
      when 'X' => return 'X';
      when 'L' => return 'L';
      when 'C' => return 'C';
      when 'D' => return 'D';
      when 'M' => return 'M';
    end case;
  end D2C;
  function C2D (C : Character) return Digit is
  begin
    case C is
      when 'I' => return 'I';
      when 'V' => return 'V';
      when 'X' => return 'X';
      when 'L' => return 'L';
      when 'C' => return 'C';
      when 'D' => return 'D';
      when 'M' => return 'M';
      when others => raise Constraint_Error;
    end case;
  end C2D;

  -- Convert a romanic number into arabic
  -- May raise Invalid_Roman
  function Romanic2Arabic (R : Number) return Arabic is
    -- Number of repeated digit
    Nb_Repet : Natural;
    -- Final result
    Result : Natural;

    V_Curr, V_Next : Natural;

    -- Get current, next or next_next digit of romanic number
    None : constant Character := '-';
    Cur_Index : Positive := R'First;
    function Prev return Character is
    begin
      if Cur_Index > R'First then
        return D2C(R(Cur_Index - 1));
      else
        return None;
      end if;
    end Prev;

    function Current return Character is
    begin
      if Cur_Index <= R'Last then
        return D2C(R(Cur_Index));
      else
        return None;
      end if;
    end Current;

    function Next return Character is
    begin
      if Cur_Index + 1 <= R'Last then
        return D2C(R(Cur_Index + 1));
      else
        return None;
      end if;
    end Next;

    function Next_Next return Character is
    begin
      if Cur_Index + 2 <= R'Last then
        return D2C(R(Cur_Index + 2));
      else
        return None;
      end if;
    end Next_Next;

    function Value (C : Character) return Natural is
    begin
      if C = None then
        return 0;
      end if;
      return Values(C2D(C));
    exception
      when Constraint_Error =>
        raise Invalid_Romanic;
    end Value;

    procedure Shift is
    begin
      Cur_Index := Cur_Index + 1;
    end Shift;

  begin
    if R = "" then
      raise Invalid_Romanic;
    end if;

    -- Now the decoding
    Result := 0;
    Nb_Repet := 0;
    loop
      V_Curr := Value(Current);
      V_Next := Value(Next);
      -- Done?
      exit when Current = None;
      if Next = None then
        -- Last digit
        Result := Result + V_Curr;
      elsif Current = Next then
        -- Repeated
        if       Current /= 'I'
        and then Current /= 'X'
        and then Current /= 'C'
        and then Current /= 'M' then
          -- This digit cannot be repeated
          raise Invalid_Romanic;
        end if;
        if Nb_Repet = 3 and then Current = 'M' then
          -- This M is already repated 3 times and is also the next:
          -- 5 times the same digit
          raise Invalid_Romanic;
        elsif Nb_Repet = 2 and then Current /= 'M' then
          -- This digit is already repated twice and is also the next:
          -- 4 times the same digit
          raise Invalid_Romanic;
        else
          -- Another repeated digit
          Nb_Repet := Nb_Repet + 1;
          Result := Result + V_Curr;
        end if;
      elsif V_Curr > V_Next then
        -- Decrescent digits
        Nb_Repet := 0;
        Result := Result + V_Curr;
      else
        -- Crescent digits: Current < Next
        Nb_Repet := 0;
        if       Current /= 'I'
        and then Current /= 'X'
        and then Current /= 'C' then
          -- This digit cannot be substracted
          raise Invalid_Romanic;
        end if;
        if V_Next/V_Curr /= 5
        and then V_Next/V_Curr /= 10 then
          -- Substracting too much
          raise Invalid_Romanic;
        end if;
        if Next_Next /= None and then Value(Next_Next) >= V_Curr then
          -- Substracting then re increasing
          raise Invalid_Romanic;
        end if;
        if Prev /= None and then Value(Prev)/V_Curr < 10 then
          -- Previous must be at least 10 times the current
           raise Invalid_Romanic;
        end if;
        -- Add (next - current)
        Result := Result + V_Next - V_Curr;
        Shift;
      end if;

      -- Next digit
      Shift;
    end loop;

    return Arabic(Result);
  exception
    when others =>
      raise Invalid_Romanic;
  end Romanic2Arabic;


  -- Convert an arabic number into romanic
  function Arabic2Romanic (A : in Arabic) return Number is
    -- Remaining value
    Rest : Natural := A;
    -- Index in Type_Def_Array, init to last power of 10 (odd)
    Index : Digit := Digit'Last;
    Other : Digit;
    -- Result string
    Result : As.U.Asu_Us;
    N : Natural;
    Div : Positive;
  begin

    loop
      -- Compute number of digits(Index) from 0 to 9
      Div :=  Values(Index);
      N := Rest / Div;
      if N > 9 then
        raise Program_Error;
      elsif N = 9 then
        -- Current then Next_Next
        Other := Digit'Succ(Index);
        Other := Digit'Succ(Other);
        Result.Append (D2C (Index) & D2C (Other));
      elsif N >= 5 then
        -- Next then N times current
        Other := Digit'Succ(Index);
        Result.Append (D2C (Other));
        for I in 6 .. N loop
          Result.Append (D2C (Index));
        end loop;
      elsif N = 4 and then Index /= Digit'Last then
        -- Current then Next
        Other := Digit'Succ(Index);
        Result.Append (D2C (Index) & D2C (Other));
      else
        -- N times current
        -- Up to 3 times for all but M and up to 4 times for M
        for I in 1 .. N loop
          Result.Append (D2C (Index));
        end loop;
      end if;

      -- Compute Rest and check for exit
      Rest := Rest - N * Div;
      exit when Rest = 0;

      -- Next slice of power of 10
      Index := Digit'Pred(Index);
      Index := Digit'Pred(Index);
    end loop;

    return Value (Result.Image);
  end Arabic2Romanic;

  -- Convert Romanic from and to string
  function Image (R : Number) return String is
    Res : String (1 .. R'Length);
  begin
    if R = "" then
      raise Invalid_Romanic;
    end if;
    for I in R'Range loop
      Res (Res'First + I - R'First) := D2C (R(I));
    end loop;
    return Res;
  end Image;

  function Value (S : String) return Number is
    Res : Number (1 .. S'Length);
  begin
    if S = "" then
      raise Invalid_Romanic;
    end if;
    for I in S'Range loop
      Res (Res'First + I - S'First) := C2D (S(I));
    end loop;
    return Res;
  exception
    when others =>
      raise Invalid_Romanic;
  end Value;

end Romanic;

