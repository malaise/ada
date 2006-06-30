with Dynamic_List, My_Math;
package body Prime_List is

  use type Arbitrary.Number;

  package Prime_Dyn_List_Mng is new Dynamic_List(Prime_Positive);
  package Prime_List_Mng renames Prime_Dyn_List_Mng.Dyn_List;
  The_List : Prime_List_Mng.List_Type;
  Need_Search : Boolean := True;

  Two  : constant Prime_Positive := Arbitrary.Set (Integer'(2));

  -- Rewind the list of prime numbers found so far
  procedure Rewind is
  begin
    if not Prime_List_Mng.Is_Empty (The_List) then
      Prime_List_Mng.Rewind (The_List);
      Need_Search := False;
    else
      Need_Search := True;
    end if;
  end Rewind;

  -- Read item from list
  function Read return Prime_Positive is
    Res : Prime_Positive;
    Moved : Boolean;
  begin
    -- Read next
    Prime_List_Mng.Read (The_List, Res, Done => Moved);
    return Res;
  end Read;

  -- Append a prime number to list
  procedure Append (N : in Prime_Positive) is
  begin
    if not Prime_List_Mng.Is_Empty (The_List) then
      Prime_List_Mng.Rewind (The_List, Prime_List_Mng.Prev);
    end if;
    Prime_List_Mng.Insert (The_List, N);
  end Append;

  Zero : constant Prime_Number := Arbitrary.Zero;
  One  : constant Prime_Number := Arbitrary.One;

  Sqrt_Memory : Prime_Positive := One;
  Step : constant Prime_Positive := Arbitrary.Set (Integer'(10));
  function Sqrt (N : Prime_Positive) return Prime_Positive is
    I, J, K : Prime_Positive;
  begin
    -- Check that (Memory)2 <= N, else reset to 1
    if Sqrt_Memory * Sqrt_Memory > N then
      Sqrt_Memory := One;
    end if;
    -- J := J * 10 until (J)2 > N, I = J / 10
    I := Sqrt_Memory;
    J := I;
    while J * J <= N loop
      I := J;
      J := J * Step;
    end loop;
    -- Now (I)2 <= N < (J)2
    -- Dichotomy between I and J
    loop
      K := (I + J) / Two;
      if K * K > N then
        J := K;
      else
        I := K;
      end if;
      exit when J = I + One;
    end loop;
    return I;
  end Sqrt;

  -- Get next prime number
  function Next return Prime_Positive is
    Res, Tmp : Prime_Positive;
    Is_Prime : Boolean;
    Square : Prime_Positive;
    Moved : Boolean;
  begin
    -- Need to search?
    if not Need_Search then
      -- If read last. Next will require search
      Prime_List_Mng.Read (The_List, Res, Prime_List_Mng.Next, Moved);
      Need_Search := not Moved;
      return Res;
    end if;

    -- Empty list, add 1
    if Prime_List_Mng.Is_Empty (The_List) then
      Append (One);
      Need_Search := True;
      return One;
    end if;

    -- Need to search next, start from last found
    Prime_List_Mng.Read (The_List, Res, Prime_List_Mng.Current);
    -- Loop on Res
    Search_Loop:
    loop
      Res := Res + One;
      Is_Prime := True;
      Square := Sqrt(Res);
      Rewind;

      -- Loop on list
      Divisor_Loop:
      for I in 1 .. Prime_List_Mng.List_Length (The_List) loop
        Tmp := Read;
        if Tmp > Square then
          exit Divisor_Loop;
        end if;
        if Tmp /= One and then Res rem Tmp = Zero then
          Is_Prime := False;
          exit Divisor_Loop;
        end if;
      end loop Divisor_Loop;

      Need_Search := True;
      if Is_Prime then
        Append (Res);
        return Res;
      end if;
    end loop Search_Loop;
  end Next;

end Prime_List;

