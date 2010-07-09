package body As.U is
  use Asu;

  function Asu_Is_Null (Str : Asu_Us) return Boolean is
  begin
    return Str = Asu_Null;
  end Asu_Is_Null;

  function Asu_Tus (Char : Character) return Asu_Us is
  begin
    return Asu_Tus ("" & Char);
  end Asu_Tus;

  -- For Unique_List
  procedure Set (To : out Asu_Us; Val : in Asu_Us) is
  begin
    To := Val;
  end Set;

  function Image (Element : Asu_Us) return String is
  begin
    return Asu_Ts (Element);
  end Image;

end As.U;

