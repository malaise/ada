with Dynamic_List, My_Math;
package body Prime_List is

  package Prime_List_Mng is new Dynamic_List(Long_Long_Positive);
  The_List : Prime_List_Mng.List_Type;
  Need_Search : Boolean := True;

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
  function Read return Long_Long_Positive is
    Res : Long_Long_Positive;
    Moved : Boolean;
  begin
    -- Read next
    Prime_List_Mng.Read (The_List, Res, Done => Moved);
    return Res;
  end Read;

  -- Append a prime number to list
  procedure Append (N : in Long_Long_Positive) is
  begin
    if not Prime_List_Mng.Is_Empty (The_List) then
      Prime_List_Mng.Rewind (The_List, Prime_List_Mng.Prev);
    end if;
    Prime_List_Mng.Insert (The_List, N);
  end Append;


  -- Get next prime number
  function Next return Long_Long_Positive is
    Res, Tmp : Long_Long_Positive;
    Is_Prime : Boolean;
    Square : Long_Long_Positive;
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
      Append (1);
      Need_Search := True;
      return 1;
    end if;

    -- Need to search next, start from last found
    Prime_List_Mng.Read (The_List, Res, Prime_List_Mng.Current);
    -- Loop on Res
    Search_Loop:
    loop
      Res := Res + 1;
      Is_Prime := True;
      Square := My_Math.Round(My_Math.Sqrt(My_Math.Real(Res)));
      Rewind;

      -- Loop on list
      Divisor_Loop:
      for I in 1 .. Prime_List_Mng.List_Length (The_List) loop
        Tmp := Read;
        if Tmp > Square then
          exit Divisor_Loop;
        end if;
        if Tmp /= 1 and then Res rem Tmp = 0 then
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

