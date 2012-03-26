with Environ;
package body Debug is

  Init : Boolean := False;
  Set : Boolean := False;
  function Is_Set return Boolean is
  begin
    if not Init then
      Set := Environ.Is_Yes ("TCPIPE_DEBUG");
      Init := True;
    end if;
    return Set;
  end Is_Set;

end Debug;

