package body As.U.Utils is

  -- For Unique_List
  procedure Set (To : out Asu_Us; Val : in Asu_Us) is
  begin
    To := Val;
  end Set;

  function Image (Element : Asu_Us) return String is
  begin
    return Element.Image;
  end Image;

end As.U.Utils;

