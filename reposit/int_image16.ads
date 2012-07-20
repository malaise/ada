-- Image of an integer in base 16 (without leading space)
-- "16#XYZ#"
generic
  type Int is range <>;
function Int_Image16 (I : Int) return String;

