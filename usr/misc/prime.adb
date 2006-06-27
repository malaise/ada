-- Usage: prime -list [ <number> ] list prime numbers up to a number
--        prime -is <number>       is a number prime
--        prime -next <number>     first prime greater than number
--        prime -prev <number>     last prime smaller than number
--        prime -fact <number>     decomposition of number in prime factors
--        prime -hcd <n1> <n2>     highest common divisor
--        prime -lcm <n1> <n2>     lowest common multiple

with Ada.Text_Io;
with Argument, Dynamic_List, Arbitrary;
with Prime_List;
procedure Prime is
  use type Arbitrary.Number;
  use Prime_List;

  -- Lists of prime factors
  package Pldm is new Dynamic_List(Prime_Positive);
  package Plm renames Pldm.Dyn_List;
  procedure Search is new Plm.Search("=");

  -- What should we do
  type Mode_List is (List_All, List, Is_Prime, Next, Prev, Factors, Hcd, Lcm);
  Mode : Mode_List;

  -- Arguments, numbers
  N1, N2, N3 : Prime_Positive;

  -- Lists of prime factors
  L1, L2, Lr : Plm.List_Type;

  -- Does a factor appear in both lists
  Match : Boolean;

  -- Boolean for call to delete
  End_Of_List : Boolean;

  -- Help
  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name & " <mode>");
    Ada.Text_Io.Put_Line (
  "<mode> ::= <list> | <is> | <next> | <prev> | <factors> | <hcd> | <lcm>");
    Ada.Text_Io.Put_Line (
  "<list>    ::= -list [ <positive> ]        -- list prime numbers (up to N)");
    Ada.Text_Io.Put_Line (
  "<is>      ::= -is <positive>              -- is N prime or not");
    Ada.Text_Io.Put_Line (
  "<next>    ::= -next <positive>            -- next prime number after N");
    Ada.Text_Io.Put_Line (
  "<prev>    ::= -prev <positive>            -- previous prime number before N");
    Ada.Text_Io.Put_Line (
  "<factors> ::= -fact <positive>            -- prime factors of N");
    Ada.Text_Io.Put_Line (
  "<hcd>     ::= -hcd <positive> <positive>  -- highest common denominator");
    Ada.Text_Io.Put_Line (
  "<lcm>     ::= -lcm <positive> <positive>  -- lowest common multiple");
  end Usage;

  -- Remove the leading '+'
  function Image (P : Prime_Positive) return String is
    Str : constant String := Arbitrary.Image (P);
  begin
    return Str (2 .. Str'Last);
  end Image;

  -- Put a number
  procedure Put_Line (P : in Prime_Positive) is
  begin
    Ada.Text_Io.Put (Image (P) );
    Ada.Text_Io.New_Line;
  end Put_Line;

  -- Rewind a list
  procedure Rewind (L : in out Plm.List_Type) is
  begin
    if not Plm.Is_Empty (L) then
      Plm.Rewind (L);
    end if;
  end Rewind;

  Zero : constant Prime_Number := Prime_List.Zero;
  One  : constant Prime_Positive := Prime_List.One;

  -- Set a positive number from string (for arg parsing)
  function  Prime_Positive_Value (Str : String) return Prime_Positive is
    R : Prime_Number;
  begin
    R := Arbitrary.Set (Str);
    if R <= Zero then
      raise Constraint_Error;
    end if;
    return R;
  end Prime_Positive_Value;

  -- Decompose N in prime numbers.
  procedure Decompose (N : in Prime_Positive; L : in out Plm.List_Type) is
    C, T : Prime_Positive;
  begin
    C := N;
    -- Start after 1
    if C = One then
      Plm.Insert (L, One);
    else
      T := Prime_List.Next;
      T := Prime_List.Next;
      loop
        if C rem T = Zero then
          -- Insert this factor and try again with it
          Plm.Insert (L, T);
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

  -- Put list from current
  procedure Put_List (L : in out Plm.List_Type) is
    T : Prime_Positive;
  begin
    loop
      Plm.Read (L, T, Plm.Current);
      Put_Line (T);
      exit when not Plm.Check_Move (L);
      Plm.Move_To (L);
    end loop;
  end Put_List;

  -- Delete current
  procedure Delete (L : in out Plm.List_Type; End_Of_List : out Boolean) is
    Done : Boolean;
  begin
    Plm.Delete (L, Plm.Next, Done);
    End_Of_List := not Done;
  end Delete;

  -- Mustiply numbers of list from current
  function Multiply (L : in Plm.List_Type) return Prime_Number is
    S, T : Prime_Number;
    Lt : Plm.List_Type;
  begin
    S := One;
    Plm.Assign (Lt, L);
    loop
      Plm.Read (Lt, T, Plm.Current);
      S := S * T;
      exit when not Plm.Check_Move (Lt);
      Plm.Move_To (Lt);
    end loop;
    return S;
  exception
    when Plm.Empty_List =>
      return S;
  end Multiply;

begin

  -- Parse arguments
  begin
    if Argument.Get_Nbre_Arg = 1
    and then Argument.Get_Parameter = "-list" then
      Mode := List_All;
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-list" then
      Mode := List;
      N1 := Prime_Positive_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-is" then
      Mode := Is_Prime;
      N1 := Prime_Positive_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-next" then
      Mode := Next;
      N1 := Prime_Positive_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-prev" then
      Mode := Prev;
      N1 := Prime_Positive_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-fact" then
      Mode := Factors;
      N1 := Prime_Positive_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 3
    and then Argument.Get_Parameter = "-hcd" then
      Mode := Hcd;
      N1 := Prime_Positive_Value (Argument.Get_Parameter(Occurence => 2));
      N2 := Prime_Positive_Value (Argument.Get_Parameter(Occurence => 3));
    elsif Argument.Get_Nbre_Arg = 3
    and then Argument.Get_Parameter = "-lcm" then
      Mode := Lcm;
      N1 := Prime_Positive_Value (Argument.Get_Parameter(Occurence => 2));
      N2 := Prime_Positive_Value (Argument.Get_Parameter(Occurence => 3));
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
    when List_All =>
      -- List all prime numbers
      loop
        Put_Line (Prime_List.Next);
      end loop;
    when List =>
      -- List prime numbers up to N1
      loop
        N2 := Prime_List.Next;
        exit when N2 > N1;
        Put_Line (N2);
      end loop;

    when Is_Prime =>
      N2 := Prime_List.Next;
      if N1 /= One then
        loop
          N2 := Prime_List.Next;
          exit when N1 rem N2 = Zero;
        end loop;
      end if;
      Ada.Text_Io.Put (Image (N1));
      if N1 = N2 then
        Ada.Text_Io.Put_Line (" is prime.");
      else
        Ada.Text_Io.Put_Line (" is not prime.");
      end if;

    when Next =>
      loop
        N2 := Prime_List.Next;
        exit when N2 > N1;
      end loop;
      Ada.Text_Io.Put_Line (Image (N2));
    when Prev =>
      N3 := One;
      loop
        N2 := Prime_List.Next;
        exit when N2 >= N1;
        N3 := N2;
      end loop;
      Ada.Text_Io.Put_Line (Image (N3));

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
          when Plm.Empty_List =>
            exit;
        end;
        -- Find in factors of N2
        Search (L2, Match, N1, From => Plm.Absolute);

        if Match then
          -- Found. Add it to Lr and remove it from L1 and L2
          Plm.Insert (Lr, N1);
          Delete (L2, End_Of_List);
          Delete (L1, End_Of_List);
          -- End of L1?
          exit when End_Of_List;
        else
          -- Not found next of L1
          exit when not Plm.Check_Move (L1);
          Plm.Move_To (L1);
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

