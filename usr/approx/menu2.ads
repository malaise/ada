package Menu2 is

  procedure Main_Screen (Data_Changed : in Boolean);

  -- Is the curve stopped (can we exit)
  function Curved_Stopped return Boolean;
end Menu2;

