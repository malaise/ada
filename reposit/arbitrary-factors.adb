-- Decomposition of Arbirary precision numbers into prime factors
with Arbitrary.Prime_List;
package body Arbitrary.Factors is

  Iterator : Arbitrary.Prime_List.Iterator;

  procedure Set (To : out Positive_Number; Val : in Positive_Number) is
  begin
    To := Val;
  end Set;

  -- Search a number
  function Search is new Nb_List_Mng.Search("=");

  -- Rewind a list
  procedure Rewind (L : in out Nb_List_Mng.List_Type) is
  begin
    L.Rewind (Check_Empty => False);
  end Rewind;

  -- Delete current
  procedure Delete (L : in out Nb_List_Mng.List_Type;
                    End_Of_List : out Boolean) is
    Done : Boolean;
  begin
    L.Delete (Nb_List_Mng.Next, Done);
    End_Of_List := not Done;
  end Delete;


  -- Decompose N in prime factors, append them to L and rewind it
  -- If N is prime only append it
  procedure Decompose (N : in Positive_Number;
                       L : in out Nb_List_Mng.List_Type) is
    C, T : Positive_Number;
  begin
    if N <= One then
      raise Constraint_Error;
    end if;
    C := N;
    -- Start at 2
    T := Iterator.Next;
    loop
      if C rem T = Zero then
        -- Insert this factor and try again with it
        L.Insert (T);
        C := C / T;
        exit when C = One;
      else
        -- Try next factor
        T := Iterator.Next;
      end if;
    end loop;
    -- Rewind lists
    Rewind (L);
    Iterator.Rewind;
  end Decompose;


  -- Extract common numbers of L1 and L2 and move them (appending) in L
  procedure Extract_Common (L1, L2 : in out Nb_List_Mng.List_Type;
                            L      : in out Nb_List_Mng.List_Type) is
    N : Positive_Number;
    End_Of_List : Boolean;
  begin
    Rewind (L1);
    Rewind (L2);
    loop
      -- Next factor of L1
      begin
        L1.Read (N, Nb_List_Mng.Current);
      exception
        when Nb_List_Mng.Empty_List =>
          exit;
      end;
      -- Find in factors of L2
      if Search (L2, N, From => Nb_List_Mng.Absolute) then
        -- Found. Add it to L and remove it from L1 and L2
        L.Insert (N);
        Delete (L2, End_Of_List);
        Delete (L1, End_Of_List);
        -- End of L1?
        exit when End_Of_List;
      else
        -- Not found next of L1
        exit when not L1.Check_Move;
        L1.Move_To;
      end if;
    end loop;

    -- Rewind lists
    Rewind (L1);
    Rewind (L2);
    Rewind (L);
  end Extract_Common;


  -- Multiply numbers of L from current to the last
  function Multiply (L : in Nb_List_Mng.List_Type) return Positive_Number is
    S, T : Positive_Number;
    Lt : Nb_List_Mng.List_Type;
  begin
    S := One;
    -- This is a temporary copy of L for scanning up to the end
    --  without modifying L.
    Lt.Unchecked_Assign (L);
    loop
      Lt.Read (T, Nb_List_Mng.Current);
      S := S * T;
      exit when not Lt.Check_Move;
      Lt.Move_To;
    end loop;
    return S;
  exception
    when Nb_List_Mng.Empty_List =>
      return S;
  end Multiply;

end Arbitrary.Factors;

