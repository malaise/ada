with Ada.Text_Io;
with Assertion;
procedure T_Assert is

begin
  Ada.Text_Io.Put_Line ("This program is normally silent");
  Ada.Text_Io.Put_Line ("Set ENV variable ASSERT_ACTION to TRACE to see the 3 traces");
   Ada.Text_Io.Put_Line (" or to RAISE to raise first exception.");
  Assertion.Assert (False, "Assert test 1");
  Assertion.Assert (True, "Should not appear");
  Assertion.Assert (False, "Assert test 2");
  Assertion.Assert (True, "Should not appear");
  Assertion.Assert (False);
end T_Assert;

