package Assertion is

  -- Action can be Ignore (do nothing), Put_Trace (trace assertion error)
  --  or Raise_Exception (trace and raise Assert_Error)
  type Action_List is (Ignore, Put_Trace, Raise_Exception);
  Default_Action : constant Action_List := Ignore;

  Assert_Error : exception;

  -- Action can be set by call to Set_Action, 
  --  or environment variable ASSERT_ACTION:
  --    If set to IGNORE, Ignore
  --    If set to TRACE, Put_Trace
  --    If set to RAISE, Raise_Exception

  -- Set/Change action (preempts env variable)
  procedure Set (Action : in Action_List);

  -- Get action value
  function Get return Action_List;

  -- Do nothing if What is True, else do action
  procedure Assert (What : in Boolean; Trace : in String := "");

end Assertion;

