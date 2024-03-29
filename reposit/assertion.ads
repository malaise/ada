-- Assert validity of prerequists
-- Note that the overall Assertion behavior can be de-activated at compile time
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
  --  casing is not significant for the value
  -- Action can be set or modified by call to Set (Action)

  -- Set/Change action (preempts env variable)
  procedure Set (Action : in Action_List);

  -- Get the current value set for Action
  function Get return Action_List;

  -- Do nothing if What is True, else do Action
  -- If Action is Default, then do the Action that was Set or defined in ENV
  procedure Assert (What : in Boolean; Trace : in String := "";
                    Action : in Assert_Action_List := Default);

end Assertion;

