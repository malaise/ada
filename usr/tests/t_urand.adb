with Ada.Text_Io; use Ada.Text_Io;
with U_Rand;

procedure T_Urand is
    package Float_Io is new Ada.Text_Io.Float_Io(Float);
    use Float_Io;
    Test_File : Ada.Text_Io.File_Type;
    File_Name : constant String := "T_URAND.DAT";
    subtype A_Result is String(1..11);
    type Results_Type is array(1..6) of A_Result;
    Actual_Results : Results_Type;
    Eos : Natural;
    Results_Ok : Boolean := True;
    Expected_Results : Results_Type := (1=>" 6533892.00",
                                        2=>"14220222.00",
                                        3=>" 7275067.00",
                                        4=>" 6172232.00",
                                        5=>" 8354498.00",
                                        6=>"10633180.00");

    Rnum : Float;
begin
    for I in 1..20000 loop
        Rnum := U_Rand.Next;
    end loop;

    Ada.Text_Io.Create(File=>Test_File, Name=>File_Name);
    for I in 1..6 loop
        Put(File=>Test_File,
            Item=>(2.0**24)*U_Rand.Next,
            Fore=>8, Aft=>2, Exp=>0);
        New_Line(Test_File);
    end loop;

    Ada.Text_Io.Close(File=>Test_File);
    Ada.Text_Io.Open(File=>Test_File, Mode=>In_File, Name=>File_Name);
    Put("Expected Results    "); Put_Line("Actual Results");

    for I in 1..6 loop
        Get_Line(File=>Test_File, Item=>Actual_Results(I), Last=>Eos);
        Skip_Line(Test_File);
        Put(Expected_Results(I)); Put("         ");
        Put_Line(Actual_Results(I));
        if Actual_Results(I) /= Expected_Results(I) then
            Results_Ok := False;
        end if;
    end loop;

    New_Line(2);
    if not Results_Ok then
        Put_Line("!! CAUTION!! Random Number Generator inconsistent on this inplementation");
    else
        Put_Line("Random Number Generator Consistent");
    end if;
end T_Urand;

