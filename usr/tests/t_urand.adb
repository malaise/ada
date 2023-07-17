-- Test U_Rand (fixed set of 6 expected values)
with U_Rand, Basic_Proc, Normalization;
procedure T_Urand is
    subtype Result is String(1..12);
    type Results is array(1..6) of Result;
    Tmp_Result : Result;
    Results_Ok : Boolean := True;
    Expected_Results : constant Results
                     := (1=>"  6533892.00",
                         2=>" 14220222.00",
                         3=>"  7275067.00",
                         4=>"  6172232.00",
                         5=>"  8354498.00",
                         6=>" 10633180.00");
    Gen : U_Rand.Generator;
    Rnum : Float;
begin
    for I in 1..20000 loop
      U_Rand.Next (Gen, Rnum);
    end loop;

    Basic_Proc.Put_Output ("Expected Results    ");
    Basic_Proc.Put_Line_Output ("Actual Results");

    for I in Results'Range loop
      U_Rand.Next (Gen, Rnum);
      Basic_Proc.Put_Output (Expected_Results(I));
      Basic_Proc.Put_Output ("         ");
      Tmp_Result := Normalization.Normal_Fixed (2.0 ** 24 * Rnum,
                                                Tmp_Result'Length, 9, '0');
      Basic_Proc.Put_Line_Output (Tmp_Result);
      if Tmp_Result  /= Expected_Results(I) then
        Results_Ok := False;
      end if;
    end loop;

    Basic_Proc.New_Line_Output;
    Basic_Proc.New_Line_Output;
    if not Results_Ok then
      Basic_Proc.Put_Line_Output (
          "!! CAUTION!! Random Number Generator inconsistent on this inplementation");
    else
      Basic_Proc.Put_Line_Output ("Random Number Generator Consistent");
    end if;
end T_Urand;

