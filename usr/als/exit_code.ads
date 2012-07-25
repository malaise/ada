package Exit_Code is

  -- Exit codes
  Found : constant Natural := 0;
  Empty : constant Natural := 1;
  Error : constant Natural := 2;
  procedure Update (New_Code : in Natural);

end Exit_Code;

