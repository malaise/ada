with TEXT_IO; use TEXT_IO;
with U_RAND;

procedure T_URAND is
    package FLOAT_IO is new TEXT_IO.FLOAT_IO(FLOAT);
    use FLOAT_IO;
    TEST_FILE : TEXT_IO.FILE_TYPE;
    FILE_NAME : constant STRING := "T_URAND.DAT";
    subtype A_RESULT is STRING(1..11);
    type RESULTS_TYPE is array(1..6) of A_RESULT;
    ACTUAL_RESULTS : RESULTS_TYPE;
    EOS : NATURAL;
    RESULTS_OK : BOOLEAN := TRUE;
    EXPECTED_RESULTS : RESULTS_TYPE := (1=>" 6533892.00",
                                        2=>"14220222.00",
                                        3=>" 7275067.00",
                                        4=>" 6172232.00",
                                        5=>" 8354498.00",
                                        6=>"10633180.00");

    RNUM : FLOAT;
begin
    for I in 1..20000 loop
        RNUM := U_RAND.NEXT;
    end loop;

    TEXT_IO.CREATE(FILE=>TEST_FILE, NAME=>FILE_NAME);
    for I in 1..6 loop
        PUT(FILE=>TEST_FILE,
            ITEM=>(2.0**24)*U_RAND.NEXT,
            FORE=>8, AFT=>2, EXP=>0);
        NEW_LINE(TEST_FILE);
    end loop;

    TEXT_IO.CLOSE(FILE=>TEST_FILE);
    TEXT_IO.OPEN(FILE=>TEST_FILE, MODE=>IN_FILE, NAME=>File_NAME);
    PUT("Expected Results    "); PUT_LINE("Actual Results");

    for I in 1..6 loop
        GET_LINE(FILE=>TEST_FILE, ITEM=>ACTUAL_RESULTS(I), LAST=>EOS);
        SKIP_LINE(TEST_FILE);
        PUT(EXPECTED_RESULTS(I)); PUT("         ");
        PUT_LINE(ACTUAL_RESULTS(I));
        if ACTUAL_RESULTS(I) /= EXPECTED_RESULTS(I) then
            RESULTS_OK := FALSE;
        end if;
    end loop;

    NEW_LINE(2);
    if not RESULTS_OK then
        PUT_LINE("!! CAUTION!! Random Number Generator inconsistent on this inplementation");
    else
        PUT_lINE("Random Number Generator Consistent");
    end if;
end T_URAND;

