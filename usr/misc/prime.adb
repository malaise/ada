-- Usage: prime -list [ <number> ] list prime numbers up to a number
--        prime -is <number>       is a number prime
--        prime -next <number>     first prime greater than number
--        prime -prev <number>     last prime smaller than number
--        prime -fact <number>     decomposition of number in prime factors
--        prime -hcd <n1> <n2>     highest common divisor
--        prime -lcm <n1> <n2>     lowest common multiple

with Ada.Text_Io;
with Argument, Arbitrary.Factors, Arbitrary.Prime_List;
procedure Prime is
  use type Arbitrary.Number;
  subtype Positive_Number is Arbitrary.Prime_List.Positive_Number;

  -- Lists of prime factors
  package Plm renames Arbitrary.Factors.Nb_List_Mng;

  -- What should we do
  type Mode_List is (List_All, List, Is_Prime, Next, Prev, Factors, Hcd, Lcm);
  Mode : Mode_List;

  -- Arguments, numbers
  N1, N2, N3 : Positive_Number;

  -- Lists of prime factors
  L1, L2, Lr : Plm.List_Type;

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
  function Image (P : Positive_Number) return String is
    Str : constant String := Arbitrary.Image (P);
  begin
    return Str (2 .. Str'Last);
  end Image;

  -- Put a number
  procedure Put_Line (P : in Positive_Number) is
  begin
    Ada.Text_Io.Put (Image (P) );
    Ada.Text_Io.New_Line;
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

    when Is_Prime =>
      -- Check if N1 is prime
      N2 := Arbitrary.Prime_List.Next;
      if N1 /= One then
        loop
          N2 := Arbitrary.Prime_List.Next;
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
      -- Look for first prime number > N1
      loop
        N2 := Arbitrary.Prime_List.Next;
        exit when N2 > N1;
      end loop;
      Ada.Text_Io.Put_Line (Image (N2));
    when Prev =>
      -- Look for last prime number < N1
      N3 := One;
      loop
        N2 := Arbitrary.Prime_List.Next;
        exit when N2 >= N1;
        N3 := N2;
      end loop;
      Ada.Text_Io.Put_Line (Image (N3));

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

