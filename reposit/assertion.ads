package Assertion is

  -- Do nothing if What is True,
  -- else check environment variable ASSERT_ACTION
  --  If set to TRACE, trace assertion error
  --  If set to RAISE, trace and raise Assert_Error
  --  Else do nothing
  procedure Assert (What : in Boolean; Trace : in String := "");

  Assert_Error : exception;

end Assertion;

