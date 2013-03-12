with Ada.Calendar;
with Argument, Images, Basic_Proc, Environ, Upper_Str;
package body Assertion is

  -- Init (getenv) done?
  Init_Done : Boolean := False;

  Action_Name : constant String := "ASSERT_ACTION";
  Action : Action_List := Default_Action;

  procedure Init is
    Action_Str : String (1 .. 6);
    Len : Natural;
  begin
    if Init_Done then
      return;
    end if;
    Init_Done := True;
    -- Set Action according to env variable Action_Name
    Action_Str := (others => '-');
    Len := Action_Str'Last;
    Environ.Get_Str (Action_Name, Action_Str, Len);
    Action_Str := Upper_Str (Action_Str);
    Action := (if    Action_Str (1 .. Len) = "IGNORE" then Ignore
               elsif Action_Str (1 .. Len) = "TRACE"  then Put_Trace
               elsif Action_Str (1 .. Len) = "RAISE"  then Raise_Exception
               else Default_Action);
  end Init;

  -- Set/Change action (preempts env variable)
  procedure Set (Action : in Action_List) is
  begin
    Assertion.Action := Set.Action;
    Init_Done := True;
  end Set;

  -- Get action value
  function Get return Action_List is
  begin
    Init;
    return Action;
  end Get;

  -- Do nothing if What is True,
  -- else check environment variable ASSERT_ACTION
  --  If set to TRACE, trace assertion error
  --  If set to RAISE, trace and raise Assert_Error
  --  Else do nothing
  procedure Assert (What : in Boolean; Trace : in String := "";
                    Action : in Assert_Action_List := Default) is
    To_Do : Action_List;
  begin
    if What then
      -- Assertion is True
      return;
    end if;
    -- Optim, avoid function call if init already done
    if not Init_Done then
      -- Init is necessary
      Init;
    end if;
    To_Do := (if Action = Default then Assertion.Action
              else Action_List(Action));
    if To_Do = Ignore then
      -- Action is to ignore False assertion
      return;
    end if;

    -- Trace
    Basic_Proc.Put_Error (Images.Date_Image(Ada.Calendar.Clock)
                       & " - " & Argument.Get_Program_Name
                       & ", Assertion failed");
    if Trace /= "" then
      Basic_Proc.Put_Error (": " & Trace );
    end if;
    Basic_Proc.Put_Line_Error (".");

    if To_Do = Put_Trace then
      -- Action is only to log False assertion
      return;
    end if;

    -- Action is to raise exception
    raise Assert_Error;
  end Assert;

end Assertion;

