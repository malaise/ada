package Assertion is

  -- Action can be Ignore (do nothing), Put_Trace (trace assertion error)
  --  or Raise_Exception (trace and raise Assert_Error)
  type Assert_Action_List is (Default, Ignore, Put_Trace, Raise_Exception);
  type Action_List is new Assert_Action_List range Ignore .. Raise_Exception;
  Default_Action : constant Action_List := Ignore;

  Assert_Error : exception;

  -- Action can be initialized by environment variable ASSERT_ACTION:
  --    If set to IGNORE, Ignore
  --    If set to TRACE, Put_Trace
  --    If set to RAISE, Raise_Exception
  --  and can be set or modified by call to Set (Action),

  -- Set/Change action (preempts env variable)
  procedure Set (Action : in Action_List);

  -- Get action value
  function Get return Action_List;

  -- Do nothing if What is True, else do Action
  -- If Action is Default, do the Action Set or defined in ENV
  procedure Assert (What : in Boolean; Trace : in String := "";
                    Action : in Assert_Action_List := Default);

end Assertion;

