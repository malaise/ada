package Output is

  -- Put Str at correct indentation Level, optionaly in comment
  --  optionnaly indent after comment "--" string
  procedure Put_Line (Str : in String; 
                      Level : in Natural;
                      Comment : in Boolean;
                      Comment_Level : in Natural := 0);

  procedure Put (Str : in String; 
                 Level : in Natural;
                 Comment : in Boolean;
                 Comment_Level : in Natural := 0);

end Output;

