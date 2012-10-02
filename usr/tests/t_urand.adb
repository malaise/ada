with U_Rand, Basic_Proc, Normalization, Text_Line;
procedure T_Urand is
    Test_File : Text_Line.File_Type;
    File_Name : constant String := "T_URAND.DAT";
    subtype A_Result is String(1..12);
    type Results_Type is array(1..6) of A_Result;
    Actual_Results : Results_Type;
    Results_Ok : Boolean := True;
    Expected_Results : constant Results_Type
                     := (1=>"  6533892.00",
                         2=>" 14220222.00",
                         3=>"  7275067.00",
                         4=>"  6172232.00",
                         5=>"  8354498.00",
                         6=>" 10633180.00");

    Rnum : Float;
    pragma Unreferenced (Rnum);
begin
    for I in 1..20000 loop
        Rnum := U_Rand.Next;
    end loop;

    Test_File.Create_All (File_Name);
    for I in 1..6 loop
        Test_File.Put(Normalization.Normal_Fixed ((2.0**24)*U_Rand.Next,
                                                  A_Result'Length, 9, '0'));
        Test_File.New_Line;
    end loop;

    Test_File.Close_All;
    Test_File.Open_All (Text_Line.In_File, File_Name);
    Basic_Proc.Put_Output ("Expected Results    ");
    Basic_Proc.Put_Line_Output ("Actual Results");

    for I in 1..6 loop
        Actual_Results(I) := Text_Line.Trim(Test_File.Get);
        Basic_Proc.Put_Output (Expected_Results(I));
        Basic_Proc.Put_Output ("         ");
        Basic_Proc.Put_Line_Output (Actual_Results(I));
        if Actual_Results(I) /= Expected_Results(I) then
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

