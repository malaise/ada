with Arbitrary.Limited_List;
package body Arbitrary.Prime_List is

  procedure Set (To : out Positive_Number; Val : in Positive_Number) is
  begin
    To := Val;
  end Set;

  package Prime_List_Mng is new Arbitrary.Limited_List (Positive_Number, Set);
  The_List : Prime_List_Mng.List_Type;

  -- Rewind the list of prime numbers found so far
  procedure Rewind is
  begin
    The_List.Rewind (Check_Empty => False);
  end Rewind;

  -- Read item from list
  function Read return Positive_Number is
    Res : Positive_Number;
    Moved : Boolean;
  begin
    -- Read next
    The_List.Read (Res, Moved => Moved);
    return Res;
  end Read;

  -- Append a prime number to list
  procedure Append (N : in Positive_Number) is
  begin
    The_List.Rewind (Prime_List_Mng.Prev, False);
    The_List.Insert (N);
  end Append;

  Zero : constant Number := Arbitrary.Zero;
  One  : constant Positive_Number := Arbitrary.One;

  -- This optimisation is possible because we always get Sqrt of
  --  crescent values of N (in case of rewind the prime values are read
  --  from the list).
  -- So we just keep in mind prev Sqrt returned S and Nm=(S+1)^2
  --  as long as the arg is N < Nm then its Sqrt is S
  --  otherwise it is S+1 and we update Nm

  -- S is last Sqrt found, also store S+1 and N=(S+1)*(S+1)
  S_Memory : Positive_Number := One;
  Sp1_Memory : Positive_Number := S_Memory.Incr;
  N_Memory : Positive_Number := Sp1_Memory * Sp1_Memory;
  function Sqrt (N : Positive_Number) return Positive_Number is
  begin
    -- If N < N_Mem then Sqrt(N) = S_Mem
    -- Otherwise it is S_Mem + 1 and update S, S+1 and N
    if N >= N_Memory then
      S_Memory := Sp1_Memory;
      Sp1_Memory.Incr;
      N_Memory := Sp1_Memory * Sp1_Memory;
    end if;
    return S_Memory;
  end Sqrt;

  -- Get next prime number
  function Next return Positive_Number is
    Res, Tmp : Positive_Number;
    Is_Prime : Boolean;
    Square : Positive_Number;
  begin
    -- Empty list, add 2
    if The_List.Is_Empty then
      Append (Two);
      return Two;
    end if;

    -- Need to search next, start from last found
    The_List.Read (Res, Prime_List_Mng.Current);
    -- Loop on Res
    Search_Loop:
    loop
      -- Optim: if Res=2, then Next=Res+1 (3) and is prime
      -- otherwise Res is odd and next is odd, so at least Res+2
      if The_List.Get_Position = One then
        Res.Incr;
        Append (Res);
        return Res;
      end if;
      Res := Res + Two;
      Square := Sqrt(Res);

      Rewind;
      -- Loop on list of primes to find a divisor
      -- Because the case of Res=2 and 3 is already done, we are sure that
      --  Square > 1, so this loop always exits before exhausting the list
      --  so Is_Prime is always set
      declare
        I : Positive_Number;
      begin
        I := One;
        Divisor_Loop:
        while I <= The_List.List_Length loop
          Tmp := Read;
          if Tmp > Square then
            -- We have reached sqrt(Res), so Tmp * Tmp > Res,
            --  so Tmp cannot be a factor of Res, so Res is prime
            Is_Prime := True;
            exit Divisor_Loop;
          end if;
          if Tmp /= One and then Res rem Tmp = Zero then
            -- Res = Tmp * X, so Res is not prime
            Is_Prime := False;
            exit Divisor_Loop;
          end if;
          I.Incr;
        end loop Divisor_Loop;
      end;

      -- We always exit by appending a new prime number to the list
      if Is_Prime then
        Append (Res);
        return Res;
      end if;
    end loop Search_Loop;
  end Next;

  -- Rewind the list of prime numbers found so far
  procedure Rewind (It : in out Iterator) is
  begin
    It:= (others => <>);
  end Rewind;

  -- Get next prime number
  function Next (It : in out Iterator) return Positive_Number is
    N : Positive_Number;
  begin
    if It.Position <= The_List.List_Length then
      -- Next prime number is known, at pos It.Position
      The_List.Move_At (It.Position);
      The_List.Read (N, Prime_List_Mng.Current);
    else
      N := Next;
    end if;
    It.Position.Incr;
    return N;
  end Next;

end Arbitrary.Prime_List;

