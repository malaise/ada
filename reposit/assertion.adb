with Ada.Calendar;
with Argument, Date_Image, Sys_Calls, Environ, Upper_Str;
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
    if Action_Str (1 .. Len) = "IGNORE" then
      Action := Ignore;
    elsif Action_Str (1 .. Len) = "TRACE" then
      Action := Put_Trace;
    elsif Action_Str (1 .. Len) = "RAISE" then
      Action := Raise_Exception;
    else
      Action := Default_Action;
    end if;
  end Init;

    
  procedure Set (Action : in Action_List) is
  begin
    if not Init_Done then
      -- Init is necessary
      Init;
    end if;
    Assertion.Action := Set.Action;
  end Set;

  -- Do nothing if What is True,
  -- else check environment variable ASSERT_ACTION
  --  If set to TRACE, trace assertion error
  --  If set to RAISE, trace and raise Assert_Error
  --  Else do nothing
  procedure Assert (What : in Boolean; Trace : in String := "") is
  begin
    if What then
      -- Assertion is True
      return;
    end if;
    if not Init_Done then
      -- Init is necessary
      Init;
    end if;
    if Action = Ignore then
      -- Action is to ignore False assertion
      return;
    end if;

    -- Trace
    Sys_Calls.Put_Error (Date_Image(Ada.Calendar.Clock)
                       & " - " & Argument.Get_Program_Name
                       & ", Assertion failed");
    if Trace /= "" then
      Sys_Calls.Put_Error (": " & Trace );
    end if;
    Sys_Calls.Put_Line_Error (".");

    if Action = Put_Trace then
      -- Action is only to log False assertion
      return;
    end if;

    -- Action is to raise exception
    raise Assert_Error;
  end Assert;

end Assertion;

