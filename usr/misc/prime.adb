-- Usage: prime -list           list prime numbers
--        prime -fact <number>  decomposition of number in prime factors
--        prime -hcd <n1> <n2>  highest common divisor
--        prime -lcm <n1> <n2>  lowest common multiple

with Ada.Text_Io;
with Argument, Dynamic_List;
with Prime_List;
procedure Prime is
  use Prime_List;

  -- Put result
  package Llp_Io is new Ada.Text_Io.Integer_Io (Long_Long_Positive);

  -- Lists of prime factors
  package Plm is new Dynamic_List(Long_Long_Positive);
  procedure Search is new Plm.Search;

  -- What should we do
  type Mode_List is (List, Factors, Hcd, Lcm);
  Mode : Mode_List;

  -- Arguments, numbers
  N1, N2 : Long_Long_Positive;

  -- Lists of prime factors
  L1, L2, Lr : Plm.List_Type;

  -- Does a factor appear in both lists
  Match : Boolean;

  -- Boolean for call to delete
  End_Of_List : Boolean;

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name & " <mode>");
    Ada.Text_Io.Put_Line ("<mode> ::= <list> | <factors> | <hcd> | <lcm>");
    Ada.Text_Io.Put_Line ("<list>      ::= -list");
    Ada.Text_Io.Put_Line ("<decompose> ::= -fact <positive>");
    Ada.Text_Io.Put_Line ("<pgcd>      ::= -hcd <positive> <positive>");
    Ada.Text_Io.Put_Line ("<ppcm>      ::= -lcm <positive> <positive>");
  end Usage;

  procedure Put_Line (P : in Long_Long_Positive) is
  begin
    llp_Io.Put (P);
    Ada.Text_Io.New_Line;
  end Put_Line;

  -- Decompose N in prime numbers.
  procedure Decompose (N : in Long_Long_Positive; L : in out Plm.List_Type) is
    C, T : Long_Long_Positive;
  begin
    C := N;
    -- Start after 1
    if C = 1 then
      Plm.Insert (L, 1);
    else
      T := Prime_List.Next;
      T := Prime_List.Next;
      loop
        if C rem T = 0 then
          Plm.Insert (L, T);
          C := C / T;
          exit when C = 1;
        else
          T := Prime_List.Next;
        end if;
      end loop;
    end if;
    Plm.Move_To (L, Plm.Next, 0, False);
    Prime_List.Rewind;
  end Decompose;

  procedure Put_List (L : in out Plm.List_type) is
    T : Long_Long_Positive;
  begin
    loop
      Plm.Read (L, T, Plm.Current);
      Put_Line (T);
      begin
        Plm.Move_To (L);
      exception
        when Plm.Not_In_List =>
          exit;
      end;
    end loop;
  end Put_List;

  procedure Delete (L : in out Plm.List_type; End_Of_List : out Boolean) is
  begin
    if Plm.Get_Position (L) /= Plm.List_Length (L) then
      Plm.Delete (L, Plm.Next);
      End_Of_List := False;
    else
      Plm.Delete (L, Plm.Prev);
      End_Of_List := True;
    end if;
  end Delete;

  procedure Rewind (L : in out Plm.List_type) is
  begin
    if not Plm.Is_Empty (L) then
      Plm.Move_To (L, Plm.Next, 0, False);
    end if;
  end Rewind;

  function Multiply (L : in Plm.List_type) return Long_Long_Integer is
    S, T : Long_Long_Integer;
    Lt : Plm.List_type;
  begin
    S := 1;
    Plm.Assign (Lt, L);
    loop
      Plm.Read (Lt, T, Plm.Current);
      S := S * T;
      Plm.Move_To (Lt);
    end loop;
    return S;
  exception
    when Plm.Not_In_List | Plm.Empty_List =>
      return S;
  end Multiply;

begin

  -- Parse arguments
  begin
    if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-list" then
      Mode := List;
    elsif Argument.Get_Nbre_Arg = 2 and then Argument.Get_Parameter = "-fact" then
      Mode := Factors;
      N1 := Long_Long_Positive'Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 3 and then Argument.Get_Parameter = "-hcd" then
      Mode := Hcd;
      N1 := Long_Long_Positive'Value (Argument.Get_Parameter(Occurence => 2));
      N2 := Long_Long_Positive'Value (Argument.Get_Parameter(Occurence => 3));
    elsif Argument.Get_Nbre_Arg = 3 and then Argument.Get_Parameter = "-lcm" then
      Mode := Lcm;
      N1 := Long_Long_Positive'Value (Argument.Get_Parameter(Occurence => 2));
      N2 := Long_Long_Positive'Value (Argument.Get_Parameter(Occurence => 3));
    else
      Usage;
      return;
    end if;

  exception
    when others =>
      Usage;
      return; 
  end;

  case Mode is
    when List =>
      -- List all prime numbers
      loop
        Put_Line (Prime_List.Next);
      end loop;
    when Factors =>
      -- Decompose N1 in prime factors
      Decompose (N1, L1);
      Put_List (L1);
    when Hcd | Lcm =>
      -- Decompose N1 and N2 in prime factors
      Decompose (N1, L1);
      Decompose (N2, L2);

      -- Add to Lr any prime factor common to L1 and L2 
      --  and remove it from from L1 and L2
      loop
        -- Next factor of N1
        begin
          Plm.Read (L1, N1, Plm.Current);
        exception
          when Plm.Empty_List | Plm.Not_In_List =>
            exit;
        end;
        -- Find in factors of N2
        begin
          Search (L2, N1, From_Current => False);
          Match := True;
        exception
          when Plm.Empty_List | Plm.Not_In_List =>
            -- Not found in L2
            Match := False;
        end;

        if Match then
          -- Found. Add it to Lr and remove it from L1 and L2
          Plm.Insert (Lr, N1);
          Delete (L2, End_Of_List);
          Delete (L1, End_Of_List);
          -- End of L1?
          exit when End_Of_List;
        else
          -- Not found next of L1
          begin
            Plm.Move_To (L1);
          exception
            when Plm.Not_In_List | Plm.Empty_List =>
              exit;
          end;
        end if;
      end loop;

      -- Rewind
      Rewind (L1);
      Rewind (L2);
      Rewind (Lr);

      if Mode = Hcd then
        -- Put multiplication of Lr
        Put_Line (Multiply (Lr));
      else
        -- Put multiplication of Lr * L1 * L2
        Put_Line (Multiply (Lr) * Multiply (L1) * Multiply (L2));
      end if;

  end case;

end Prime;

