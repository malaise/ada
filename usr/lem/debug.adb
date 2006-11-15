with Environ;
package body Debug is

  Debug_Is_Checked : Boolean := False;
  Debug_Is_Set : Boolean;

  function Set return Boolean is
  begin
    if not Debug_Is_Checked then
      Debug_Is_Set := Environ.Is_Yes ("LEM_DEBUG");
      Debug_Is_Checked := True;
    end if;
    return Debug_Is_Set;
  end Set;

end Debug;

