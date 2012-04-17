-- Usage: prime -list [ <number> ] list prime numbers up to number
--        prime -from <number>     list prime numbers greater than number
--        prime -is <number>       is a number prime
--        prime -next <number>     first prime greater than number
--        prime -prev <number>     last prime smaller than number
--        prime -fact <number>     decomposition of number in prime factors
--        prime -hcd <n1> <n2>     highest common divisor
--        prime -lcm <n1> <n2>     lowest common multiple

with Argument, Arbitrary.Factors, Arbitrary.Prime_List, Basic_Proc;
procedure Prime is
  use type Arbitrary.Number;
  subtype Positive_Number is Arbitrary.Prime_List.Positive_Number;

  -- Lists of prime factors
  package Plm renames Arbitrary.Factors.Nb_List_Mng;

  -- What should we do
  type Mode_List is (List_All, List, From, Is_Prime, Next, Prev, Factors, Hcd, Lcm);
  Mode : Mode_List;

  -- Arguments, numbers
  N1, N2, N3 : Positive_Number;

  -- Lists of prime factors
  L1, L2, Lr : Plm.List_Type;

  -- Help
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output (
  "Usage: " & Argument.Get_Program_Name & " <mode>");
    Basic_Proc.Put_Line_Output (
  "<mode> ::= <list> | <from> | <is> | <next> | <prev> | <factors> | <hcd> | <lcm>");
    Basic_Proc.Put_Line_Output (
  "<list>    ::= -list [ <positive> ]        -- list prime numbers (up to N)");
    Basic_Proc.Put_Line_Output (
  "<from>    ::= -from [ <positive> ]        -- list prime numbers above N");
    Basic_Proc.Put_Line_Output (
  "<is>      ::= -is <positive>              -- is N prime or not");
    Basic_Proc.Put_Line_Output (
  "<next>    ::= -next <positive>            -- next prime number after N");
    Basic_Proc.Put_Line_Output (
  "<prev>    ::= -prev <positive>            -- previous prime number before N");
    Basic_Proc.Put_Line_Output (
  "<factors> ::= -fact <positive>            -- prime factors of N");
    Basic_Proc.Put_Line_Output (
  "<hcd>     ::= -hcd <positive> <positive>  -- highest common denominator");
    Basic_Proc.Put_Line_Output (
  "<lcm>     ::= -lcm <positive> <positive>  -- lowest common multiple");
  end Usage;

  -- Remove the leading '+'
  function Image (P : Positive_Number) return String is
    Str : constant String := Arbitrary.Image (P);
  begin
    return Str (2 .. Str'Last);
  end Image;

  -- Put a number
  procedure Put_Line (P : in Positive_Number) is
  begin
    Basic_Proc.Put_Line_Output (Image (P) );
  end Put_Line;

  Zero : constant Arbitrary.Number := Arbitrary.Zero;
  One  : constant Positive_Number := Arbitrary.One;

  -- Set a positive number from string (for arg parsing)
  function  Positive_Number_Value (Str : String) return Positive_Number is
    R : Positive_Number;
  begin
    R := Arbitrary.Set (Str);
    if R <= Zero then
      raise Constraint_Error;
    end if;
    return R;
  end Positive_Number_Value;

  -- Put list from current
  procedure Put_List (L : in out Plm.List_Type) is
    T : Positive_Number;
  begin
    loop
      Plm.Read (L, T, Plm.Current);
      Put_Line (T);
      exit when not Plm.Check_Move (L);
      Plm.Move_To (L);
    end loop;
  end Put_List;

begin

  -- Parse arguments
  begin
    if Argument.Get_Nbre_Arg = 1
    and then Argument.Get_Parameter = "-list" then
      Mode := List_All;
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-list" then
      Mode := List;
      N1 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-from" then
      Mode := From;
      N1 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-is" then
      Mode := Is_Prime;
      N1 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-next" then
      Mode := Next;
      N1 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-prev" then
      Mode := Prev;
      N1 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 2
    and then Argument.Get_Parameter = "-fact" then
      Mode := Factors;
      N1 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 2));
    elsif Argument.Get_Nbre_Arg = 3
    and then Argument.Get_Parameter = "-hcd" then
      Mode := Hcd;
      N1 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 2));
      N2 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 3));
    elsif Argument.Get_Nbre_Arg = 3
    and then Argument.Get_Parameter = "-lcm" then
      Mode := Lcm;
      N1 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 2));
      N2 := Positive_Number_Value (Argument.Get_Parameter(Occurence => 3));
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
        Put_Line (Arbitrary.Prime_List.Next);
      end loop;
    when List =>
      -- List prime numbers up to N1
      loop
        N2 := Arbitrary.Prime_List.Next;
        exit when N2 > N1;
        Put_Line (N2);
      end loop;

    when From =>
      -- List prime numbers from N1 included
      loop
        N2 := Arbitrary.Prime_List.Next;
        if N2 >= N1 then
          Put_Line (N2);
        end if;
      end loop;

    when Is_Prime =>
      -- Check if N1 is prime
      N2 := Arbitrary.Prime_List.Next;
      if N1 /= One then
        loop
          N2 := Arbitrary.Prime_List.Next;
          exit when N1 rem N2 = Zero;
        end loop;
      end if;
      Basic_Proc.Put_Output (Image (N1));
      if N1 = N2 then
        Basic_Proc.Put_Line_Output (" is prime.");
      else
        Basic_Proc.Put_Line_Output (" is not prime.");
      end if;

    when Next =>
      -- Look for first prime number > N1
      loop
        N2 := Arbitrary.Prime_List.Next;
        exit when N2 > N1;
      end loop;
      Basic_Proc.Put_Line_Output (Image (N2));
    when Prev =>
      -- Look for last prime number < N1
      N3 := One;
      loop
        N2 := Arbitrary.Prime_List.Next;
        exit when N2 >= N1;
        N3 := N2;
      end loop;
      Basic_Proc.Put_Line_Output (Image (N3));

    when Factors =>
      -- Decompose N1 in prime factors
      Arbitrary.Factors.Decompose (N1, L1);
      Put_List (L1);
    when Hcd | Lcm =>
      -- Highest common denominator and lowest common multiplicator
      -- Decompose N1 and N2 in prime factors
      Arbitrary.Factors.Decompose (N1, L1);
      Arbitrary.Factors.Decompose (N2, L2);

      -- Add to Lr any prime factor common to L1 and L2
      --  and remove it from from L1 and L2
      Arbitrary.Factors.Extract_Common (L1, L2, Lr);

      if Mode = Hcd then
        -- Highest common denominator
        -- Put multiplication of Lr
        Put_Line (Arbitrary.Factors.Multiply (Lr));
      else
        -- Lowest common multiplicator
        -- Put multiplication of Lr * L1 * L2
        Put_Line (Arbitrary.Factors.Multiply (Lr)
                * Arbitrary.Factors.Multiply (L1)
                * Arbitrary.Factors.Multiply (L2));
      end if;

  end case;

end Prime;

