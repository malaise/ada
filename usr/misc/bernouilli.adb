with Long_Longs, Arbitrary.Fractions,
     Basic_Proc, Normalization, Str_Util;
procedure Bernouilli is
  use type Arbitrary.Fractions.Fraction;
  subtype Fraction is Arbitrary.Fractions.Fraction;

  function Compute (N : Long_Longs.Ll_Positive) return Fraction is
    type Result_Array is array (Long_Longs.Ll_Natural range <>) of Fraction;
    Results: Result_Array (0 .. N);
  begin

    for M in 0 .. N loop
      Results(M).Set (Arbitrary.One, Arbitrary.Set (M + 1));

      for J in reverse 1 .. M loop
        Results(J-1) := Results(J-1) - Results(J);
        Results(J-1) := Results(J-1)
                      * Arbitrary.Fractions.Set (Arbitrary.Set(J));
      end loop;
    end loop;

    return Results(0);
  end Compute;

  B : Long_Longs.Ll_Positive;

begin
  Basic_Proc.Put_Line_Output ("Calculating Bernoulli numbers...");
  B := 1;
  loop
    Basic_Proc.Put_Output (Normalization.Normal_Long_Long (B, 3, Gap => '0'));
    Basic_Proc.Put_Output  (" : ");
    declare
      Str : constant String := Compute (B).Image;
    begin
      Basic_Proc.Put_Line_Output  (Str_Util.Substit (Str, ":", "/"));
    end;
    B := B + 1;
    exit when B > 60;
  end loop;

end Bernouilli;

