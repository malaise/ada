pragma Ada_2012;
procedure Conflict is

  function F (I : in out Integer) return String is
  begin
    return "";
  end F;

  generic
    type T is range <>;
  procedure P;

  procedure P is
    I : Integer := 0;
    -- Call to F here is rejected by the compiler
    S : constant String := F (I);
  begin
    null;
  end P;

begin
  null;
end Conflict;

