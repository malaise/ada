with Environ;
package body Debug is

  Known : Boolean := False;
  Set : Boolean := False;

  Debug_Var : constant String := "HUNGAR_DEBUG";

  function On return Boolean is
  begin
    if not Known then
      Set := Environ.Is_Yes (Debug_Var);
      Known := True;
    end if;
    return Set;
  end On;
end Debug;

