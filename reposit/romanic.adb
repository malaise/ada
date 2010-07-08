with As.U; use As.U;
package body Romanic is
  -- Convert a romanic number into arabic
  -- May raise Invalid_Roman
  function Romanic2Arabic (Romanic : in String) return Arabic_Range is
    -- Is current digit valid
    Valid : Boolean;
    -- Number of repeated digit
    Nb_Repet : Natural;
    -- Final result
    Result : Natural;

    V_Curr, V_Next : Natural;

    -- Get current, next or next_next digit of romanic number
    None : constant Character := '-';
    Cur_Index : Positive := Romanic'First;
    function Prev return Character is
    begin
      if Cur_Index > Romanic'First then
        return Romanic(Cur_Index - 1);
      else
        return None;
      end if;
    end Prev;

    function Current return Character is
    begin
      if Cur_Index <= Romanic'Last then
        return Romanic(Cur_Index);
      else
        return None;
      end if;
    end Current;

    function Next return Character is
    begin
      if Cur_Index + 1 <= Romanic'Last then
        return Romanic(Cur_Index + 1);
      else
        return None;
      end if;
    end Next;

    function Next_Next return Character is
    begin
      if Cur_Index + 2 <= Romanic'Last then
        return Romanic(Cur_Index + 2);
      else
        return None;
      end if;
    end Next_Next;

    function Value (C : Character) return Natural is
    begin
      if C = None then
        return 0;
      end if;
      for I in Typo_Def_Array'Range loop
        if Typo_Def_Array(I).Typo = C then
          return Typo_Def_Array(I).Val;
        end if;
      end loop;
      raise Invalid_Romanic;
    end Value;

    procedure Shift is
    begin
      Cur_Index := Cur_Index + 1;
    end Shift;

  begin
    if Romanic = "" then
      raise Invalid_Romanic;
    end if;
    -- First check all digits are valid
    for I in Romanic'Range loop
      Valid := False;
      for J in Typo_Def_Array'Range loop
        if Typo_Def_Array(J).Typo = Romanic(I) then
          Valid := True;
          exit;
        end if;
      end loop;
      if not Valid then
        raise Invalid_Romanic;
      end if;
    end loop;

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
        if Nb_Repet = 2 then
          -- This digit is already repated twice and is also the next:
          -- Four times the same digit
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

    return Arabic_Range(Result);
  exception
    when others =>
      raise Invalid_Romanic;
  end Romanic2Arabic;


  -- Convert an arabic number into romanic
  function Arabic2Romanic (Arabic : in Arabic_Range) return String is
    -- Remaining value
    Rest : Natural := Arabic;
    -- Index in Type_Def_Array, init to last power of 10 (odd)
    Index : Digits_Range := Digits_Range'Last;
    -- Result string
    Result : Asu_Us;
    N : Natural;
    Div : Positive;
  begin

    loop
      -- Compute number of digits(Index) from 0 to 9
      Div :=  Typo_Def_Array(Index).Val;
      N := Rest / Div;
      if N > 9 then
        raise Program_Error;
      elsif N = 9 then
        -- Current then Next_Next
        Asu.Append (Result, Typo_Def_Array(Index).Typo
                          & Typo_Def_Array(Index+2).Typo);
      elsif N >= 5 then
        -- Next then N times current
        Asu.Append (Result, Typo_Def_Array(Index+1).Typo);
        for I in 6 .. N loop
          Asu.Append (Result, Typo_Def_Array(Index).Typo);
        end loop;
      elsif N = 4 then
        -- Current then Next
        Asu.Append (Result, Typo_Def_Array(Index).Typo
                          & Typo_Def_Array(Index+1).Typo);
      else
        -- N times current
        for I in 1 .. N loop
          Asu.Append (Result, Typo_Def_Array(Index).Typo);
        end loop;
      end if;

      -- Compute Rest and check for exit
      Rest := Rest - N * Div;
      exit when Rest = 0;

      -- Next slice of power of 10
      Index := Index - 2;
    end loop;

    return Asu_Ts (Result);
  end Arabic2Romanic;

end Romanic;

