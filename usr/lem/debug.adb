with Environ;
package body Debug is

  Debug_Is_Checked : Boolean := False;
  Debug_Is_Set_Lem : Boolean;
  Debug_Is_Set_Flight : Boolean;

  procedure Check is
  begin
    if Debug_Is_Checked then
       return;
    end if;
    Debug_Is_Set_Lem := Environ.Is_Yes ("LEM_DEBUG_LEM");
    Debug_Is_Set_Flight := Environ.Is_Yes ("LEM_DEBUG_FLIGHT");
    if Environ.Is_Yes ("LEM_DEBUG_ALL") then
      Debug_Is_Set_Lem := True;
      Debug_Is_Set_Flight := True;
    end if;
    Debug_Is_Checked := True;
  end Check;

  function Set_Lem return Boolean is
  begin
    Check;
    return Debug_Is_Set_Lem;
  end Set_Lem;

  function Set_Flight return Boolean is
  begin
    Check;
    return Debug_Is_Set_Flight;
  end Set_Flight;

end Debug;

