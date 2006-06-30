-- Decomposition of Arbirary precision numbers into prime factors
with Prime_List;
package body Arbitrary.Factors is

  -- Search a number
  procedure Search is new Nb_List_Mng.Search("=");

  -- Rewind a list
  procedure Rewind (L : in out Nb_List_Mng.List_Type) is
  begin
    if not Nb_List_Mng.Is_Empty (L) then
      Nb_List_Mng.Rewind (L);
    end if;
  end Rewind;

  -- Delete current
  procedure Delete (L : in out Nb_List_Mng.List_Type;
                    End_Of_List : out Boolean) is
    Done : Boolean;
  begin
    Nb_List_Mng.Delete (L, Nb_List_Mng.Next, Done);
    End_Of_List := not Done;
  end Delete;


  -- Decompose N in prime factors, append them to L and rewind it
  -- If N is prime (including 1) only append it
  procedure Decompose (N : in Number; L : in out Nb_List_Mng.List_Type) is
    C, T : Number;
  begin
    C := N;
    -- Start after 1
    if C = One then
      Nb_List_Mng.Insert (L, One);
    else
      T := Prime_List.Next;
      T := Prime_List.Next;
      loop
        if C rem T = Zero then
          -- Insert this factor and try again with it
          Nb_List_Mng.Insert (L, T);
          C := C / T;
          exit when C = One;
        else
          -- Try next factor
          T := Prime_List.Next;
        end if;
      end loop;
    end if;
    -- Rewind lists
    Rewind (L);
    Prime_List.Rewind;
  end Decompose;


  -- Extract common numbers of L1 and L2 and move them (appending) in L
  procedure Extract_Common (L1, L2 : in out Nb_List_Mng.List_Type;
                            L      : in out Nb_List_Mng.List_Type) is
    N : Number;
    Match : Boolean;
    End_Of_List : Boolean;
  begin
    Rewind (L1);
    Rewind (L2);
    loop
      -- Next factor of L1
      begin
        Nb_List_Mng.Read (L1, N, Nb_List_Mng.Current);
      exception
        when Nb_List_Mng.Empty_List =>
          exit;
      end;
      -- Find in factors of L2
      Search (L2, Match, N, From => Nb_List_Mng.Absolute);

      if Match then
        -- Found. Add it to L and remove it from L1 and L2
        Nb_List_Mng.Insert (L, N);
        Delete (L2, End_Of_List);
        Delete (L1, End_Of_List);
        -- End of L1?
        exit when End_Of_List;
      else
        -- Not found next of L1
        exit when not Nb_List_Mng.Check_Move (L1);
        Nb_List_Mng.Move_To (L1);
      end if;
    end loop;

    -- Rewind lists
    Rewind (L1);
    Rewind (L2);
    Rewind (L);
  end Extract_Common;


  -- Multiply numbers of L from current to the last
  function Multiply (L : in Nb_List_Mng.List_Type) return Number is
    S, T : Number;
    Lt : Nb_List_Mng.List_Type;
  begin
    S := One;
    Nb_List_Mng.Assign (Lt, L);
    loop
      Nb_List_Mng.Read (Lt, T, Nb_List_Mng.Current);
      S := S * T;
      exit when not Nb_List_Mng.Check_Move (Lt);
      Nb_List_Mng.Move_To (Lt);
    end loop;
    return S;
  exception
    when Nb_List_Mng.Empty_List =>
      return S;
  end Multiply;

end Arbitrary.Factors;


