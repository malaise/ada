with Ada.Text_Io;
with Assertion;
procedure T_Assert is

begin
  Assertion.Assert (False, "Assert test 1");
  Assertion.Assert (True, "Should not appear");
  Assertion.Assert (False, "Assert test 2");
  Assertion.Assert (True, "Should not appear");
  Assertion.Assert (False);
end T_Assert;

