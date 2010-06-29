with Environ;
package body Debug is

  Var_Name : constant String := "LSADEPS_DEBUG";
  Checked : Boolean := False;
  Set : Boolean := False;

  function Is_Set return Boolean is
  begin
    if not Checked then
      Set := Environ.Is_Yes (Var_Name);
      Checked := True;
    end if;
    return Set;
  end Is_Set;

end Debug;

