with Dynamic_List, My_Math;
package body Arbitrary.Prime_List is


  package Prime_Dyn_List_Mng is new Dynamic_List(Positive_Number);
  package Prime_List_Mng renames Prime_Dyn_List_Mng.Dyn_List;
  The_List : Prime_List_Mng.List_Type;
  Need_Search : Boolean := True;

  Two  : constant Positive_Number := Set (Integer'(2));

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
  function Read return Positive_Number is
    Res : Positive_Number;
    Moved : Boolean;
  begin
    -- Read next
    Prime_List_Mng.Read (The_List, Res, Done => Moved);
    return Res;
  end Read;

  -- Append a prime number to list
  procedure Append (N : in Positive_Number) is
  begin
    if not Prime_List_Mng.Is_Empty (The_List) then
      Prime_List_Mng.Rewind (The_List, Prime_List_Mng.Prev);
    end if;
    Prime_List_Mng.Insert (The_List, N);
  end Append;

  Zero : constant Number := Arbitrary.Zero;
  One  : constant Positive_Number := Arbitrary.One;

  -- S is last Sqrt found, also store S+1 and N=(S+1)*(S+1)
  S_Memory : Positive_Number := One;
  Sp1_Memory : Positive_Number := S_Memory + One;
  N_Memory : Positive_Number := Sp1_Memory * Sp1_Memory;
  function Sqrt (N : Positive_Number) return Positive_Number is
  begin
    -- If N < N_Mem then Sqrt(N) = S_Mem
    -- Otherwise it is S_Mem + 1 and update S, S+1 and N
    if N >= N_Memory then
      S_Memory := Sp1_Memory;
      Sp1_Memory := Sp1_Memory + One;
      N_Memory := Sp1_Memory * Sp1_Memory;
    end if;
    return S_Memory;
  end Sqrt;

  -- Get next prime number
  function Next return Positive_Number is
    Res, Tmp : Positive_Number;
    Is_Prime : Boolean;
    Square : Positive_Number;
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

end Arbitrary.Prime_List;

