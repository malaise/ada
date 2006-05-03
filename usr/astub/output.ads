package Output is

  -- Put Str at correct indentation Level, optionaly in comment
  procedure Put_Line (Str : in String; 
                      Level : in Natural;
                      Comment : in Boolean);

end Output;

