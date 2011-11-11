with Basic_Proc, Assertion;
procedure T_Assert is

begin
  Basic_Proc.Put_Line_Output ("This program is normally silent");
  Basic_Proc.Put_Line_Output (
    "Set ENV variable ASSERT_ACTION to TRACE to see the 3 traces");
  Basic_Proc.Put_Line_Output (" or to RAISE to raise first exception.");
  Assertion.Assert (False, "Assert test 1");
  Assertion.Assert (True, "Should not appear");
  Assertion.Assert (False, "Assert test 2");
  Assertion.Assert (True, "Should not appear");
  Assertion.Assert (False);
end T_Assert;

